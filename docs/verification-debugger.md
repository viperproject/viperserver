# The verification debugger

The verification debugger lets a user interrogate a failed Viper verification: it shows the symbolic state,
the path conditions and the assumptions at the point where verification failed, and lets the user modify them
and re-run the prover. It only works with the Silicon backend and it is **experimental**.

This document describes how the feature is put together across the three repositories, so that it can be
extended without having to re-discover the architecture.

## Overview

```
viper-ide (TypeScript)                   ViperServer (Scala)                     Silicon (Scala)
─────────────────────                    ───────────────────                     ───────────────
ViperDebugAdapterDescriptorFactory ─LSP─▶ Receiver.onStartDebugAdapter
                                           DebugAdapterServer (opens a socket)
VS Code's debugger UI  ◀──────── DAP ───▶ ViperDebugAdapter
DebugCommands (menus) ── custom DAP ──▶     DebugSessionRegistry.start
DebugCodeActionProvider                       ViperServerService.startDebugVerification
                                                (re-verifies with --enableDebugging)
                                            ServerDebugSession            ───────▶ SiliconDebugSession
                                            DapModel                      ◀─────── ObligationModel
```

The debugger is a regular VS Code debug session of type `viper`. Its debug adapter runs inside ViperServer and
speaks the [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/) (DAP) on a socket of its
own; the only LSP message involved is the one that opens that socket.

`SiliconDebuggerCli` is the second client of the same API and provides Silicon's `--enableDebugging` REPL.

## Silicon

Debugging is enabled with `--enableDebugging`, which makes Silicon record a `DebugExp` for every assumption
and keep the expression form of the store, the heap and the path conditions. When a verification fails, a
`SiliconDebuggingFailureContext` (see `interfaces/Verification.scala`) is attached to the failure; it holds the
live `State` and `Verifier` of the failing branch.

At the end of `DefaultMainVerifier.verify`, a `SiliconDebugSession` is created from those failure contexts and
published via `Silicon.debugSession`. It is *not* started interactively there — the CLI picks it up in
`SiliconRunnerInstance.runMain`, and ViperServer picks it up from the `Silicon` instance.

| File | Purpose |
|---|---|
| `debugger/SiliconDebugSession.scala` | The headless API: open an obligation, add/remove assumptions, prove, change prover/timeout/print configuration. Not thread-safe. |
| `debugger/ProofObligation.scala` | All state of one obligation being debugged. |
| `debugger/DebugModel.scala` | The serializable model of an obligation (`ObligationModel`, `DebugNode`, `CounterexampleModel`, ...). |
| `debugger/CounterexampleModelBuilder.scala` | Turns a mapped counterexample into the same node model. |
| `debugger/ObligationModelBuilder.scala` | Builds that model from a `ProofObligation`. |
| `debugger/DebugRenderer.scala` | Renders the model as the text the CLI prints. |
| `debugger/SiliconDebuggerCli.scala` | The REPL, a thin client of the API. |
| `debugger/DebugExp.scala` | The recorded assumptions, and their conversion to model nodes. |
| `debugger/DebugParser.scala`, `debugger/DebugTypechecker.scala` | Parsing/typechecking of expressions the user enters, including versioned identifiers (`x@3`) and `old[debug@label](e)`. |

Adding a new debugger operation means adding a method to `SiliconDebugSession` that returns a
`DebugCommandResult`, and then exposing it in both clients.

## ViperServer

A debug session is started **on demand** for a single verification error: normal verifications are not slowed
down by the debug instrumentation. `ViperServerService.startDebugVerification` builds the AST, then re-verifies
with `--enableDebugging --disableCaching` (and `--select <member>`, falling back to the whole file if that does
not reproduce the error), keeping the verifier alive afterwards.

Keeping the verifier alive is what `ViperBackend`'s and `VerificationWorker`'s `keepVerifierAlive` flag is for:
normally `ViperBackend.execute` stops the verifier in a `finally`. The `onFinished` callback of
`VerificationWorker` hands the resulting `Silicon` instance to `ViperCoreServer.verifyForDebugging`.

`ServerDebugSession` owns the Silicon instance and serializes all commands onto one chain of futures running on
the `VerificationExecutionContext` — a session owns a prover process and mutable symbolic state, and the LSP
handler threads must not block on prover round-trips.

`DebugSessionRegistry` (one per `ClientCoordinator`, but with a server-wide single-session rule) starts, finds
and closes sessions. A session is closed when the debugged file is edited or closed, when a new session is
started, when the client disconnects, and on shutdown. While a session is open, verifications are refused
(`Receiver.onVerify` answers with `VerificationNotStarted`); the user has to stop the debugger first. Whoever
shows a session registers with `ServerDebugSession.onClose` to learn that, and why, it was closed.

The message stream of a debug run goes to `DebugDrainActor` rather than the file's `RelayActor`: relaying it
normally would overwrite the user's diagnostics and emit spurious state-change notifications. Its progress
messages are shown in the Debug Console instead.

## Protocol

### Starting the debug adapter (LSP)

The `StartDebugAdapter` request (`frontends/lsp/CommandProtocol.scala`, payload in `frontends/lsp/DebugProtocol.scala`,
mirrored in viper-ide's `client/src/ViperProtocol.ts`) makes `DebugAdapterServer` open a socket and returns its
port. The socket accepts a single connection, within 30 seconds, and listens on all interfaces like the LSP
socket does, so that an IDE attached to a remote ViperServer can reach it. The IDE's
`ViperDebugAdapterDescriptorFactory` returns that port to VS Code, which then talks DAP to it directly.

### The debug adapter (DAP)

`frontends/lsp/debug/dap/ViperDebugAdapter.scala` implements LSP4J's `IDebugProtocolServer`:

| DAP | The verification debugger |
|---|---|
| `launch` | Takes `uri` (or `program`), `backend`, `customArgs`, `failure` (`line`, `column`, both 0-based, and `message`), `restrictToMember` and `counterexample`. The IDE's `ViperDebugConfigurationProvider` fills these in from its settings. |
| `configurationDone` | Starts the debug run via `DebugSessionRegistry.start`, with a `DebugRunController` (see "Breakpoints" below). If there are breakpoints, the whole file is verified, even if `restrictToMember` is set. |
| `setBreakpoints` | Line breakpoints, handed to the controller. They are reported as unverified until the verification has started; then `breakpoint` events report where each one stops. A breakpoint may have a condition, a Viper expression: it stops unless the condition is provably false in the paused state, since a symbolic state stands for many concrete states, in some of which the condition may hold. A condition that cannot be checked (e.g. because it does not typecheck) stops, with a message in the Debug Console. |
| `setExceptionBreakpoints` | The filter `failures` ("Verification failures", off by default) makes the verification pause at every final verification failure, with the failure's proof obligation (including its assertion and counterexample). |
| threads | While the debug run is verified: thread 1, "Verification", stopped (reason `breakpoint`, `step`, `pause` or `exception`) whenever the verification is paused. Afterwards: one thread per verification error, thread `i + 2` for failure `i`, all stopped (reason `exception`) at their error, the one matching `failure` first. |
| `stackTrace` | One frame per thread, at the paused statement or the error; the frame id is the thread id. |
| `scopes` | Assertion, Assumptions, Store, the heap(s), Branch conditions, the counterexample (if computed), axioms and declarations (if shown), and the prover settings. A paused verification has no assertion and no counterexample. Silicon's session has one current obligation, so opening a frame of another error's thread opens that thread's obligation and discards the changes made to the previous one. |
| `variables` | The `DebugNode`s of a scope, see `DapModel`. Assumptions are named `[id]`, the id the user refers to them by; nodes of the form `name -> value` are split. Children beyond what the model contains are requested with `SiliconDebugSession.expand`. References are dropped whenever the obligation changes. |
| `evaluate` (Debug Console, watch expressions) | The commands of `ConsoleCommand`: `:prove`, `:assert`, `:assume`, `:remove`, `:reset`, `:solver`, `:timeout`, `:set`, `:text`, `:counterexample`, `:help`. Anything else is a Viper expression, which is checked in the current state (`SiliconDebugSession.assertText`): the result is `holds` or `does not hold`. After a command that changes the obligation, the adapter sends `invalidated` (or `stopped` again, for clients that do not support `invalidated`). |
| `evaluate`: `:fold`, `:unfold`, `:case`, `:recheck` | Fold or unfold a predicate instance (e.g. `:unfold list(x@1)`) in the current proof obligation, as if the statement was executed at that point; this happens on the obligation's own prover (`SiliconDebugSession.foldText`). Unfolding a predicate with a conditional body produces one case per feasible branch; `:case n` switches between them. A fold that fails reports why and leaves the obligation as it was. Since the heap changed but the obligation's assertion is the one evaluated before, the Assertion scope is then labelled "(before fold/unfold)", and `:recheck` checks the failed assertion (the node the error points to) again in the current state. The IDE offers "Unfold this predicate" on the predicate chunks of the Heap scope (custom request `viperFold`), using VS Code's `__vscodeVariableMenuContext` (see `DapModel.MenuContextVariable`). |
| `continue` | Lets a paused verification go on; once it has finished, goes to the next verification error. |
| `next`, `stepIn`, `stepOut` | Let a paused verification go on to the next statement the same verifier executes: on the same path, or on the next path once this one is explored. `stepIn` stops at the next statement; `next` skips the statements of `package` proof scripts that start after the current statement; `stepOut` also skips the rest of the current proof script, and continues outside of one. Once the verification has finished, there is nothing to step. |
| `pause` | Pauses the running verification before the next statement it executes. |
| `disconnect`, `terminate` | Close the session. If the server closes it (e.g. because the file was edited), the adapter explains why in the Debug Console and sends `terminated`. |

The IDE offers the console commands from menus as well, using custom requests: `viperCommand` (any console
command), `viperRemoveAssumptions`, `viperGetPrintConfig`, `viperSetPrintConfig` and `viperGetText` (the obligation
as the CLI prints it). See `client/src/debug/DebugCommands.ts`.

### Breakpoints

Silicon's executor calls a `DebugHook` (`silicon/src/main/scala/debugger/DebugHook.scala`) before every statement
it executes. ViperServer installs one in the `Silicon` instance of a debug run: `DebugSessionRegistry.start`
passes it to `ViperServerService.startDebugVerification`, which hands it via `ViperCoreServer.verifyForDebugging`,
`VerificationWorker` and `ViperBackend` to `Silicon.debugHook`.

The hook is Silicon's `DebugRunController`. When the verification starts, it resolves the breakpoints against the
program as it is executed (`BreakpointResolver`): a breakpoint stops at the first executed statement on or after
its line within the same method, since conditionals and loops are not executed as statements. When a verifier is
about to execute such a statement, the controller opens a `SiliconDebugSession` on the current state
(`openPausedState`), tells the adapter (`DebugRunListener.paused`) and blocks the verifier until `resume` or
`abort`. The session has a prover of its own, so whatever the user does with the paused state has no effect on
the verification; it is closed when the verification continues.

Silicon explores the paths of a method one after another, so a breakpoint is hit once for every feasible path
that reaches it, and a loop body once per path rather than once per iteration. The debug run uses a single
verifier, no parallel branches and no timeout (see `ViperServerService.debugArgs`), so that only one path is
paused at a time. Closing the session, e.g. because the file was edited, aborts a paused or running debug run
via `DebugRunController.abort`.

Steps are counted in statements Silicon executes, and nesting only exists inside `package` proof scripts:
conditionals and loops become control flow, and method calls are verified modularly, so there is nothing to step
into. `BreakpointResolver.depth` gives the number of proof scripts a statement is nested in.

`SymbolicExecutionRules.createFailure` reports failures to the hook (`DebugHook.failed`), but only final ones:
Silicon verifies some assertions twice, first without and then with additional heap consolidation, and a failure
of the first attempt (`!State.isLastRetry`) is not an error.

Note that LSP4J finds the methods of a service by their annotations, including those on the forwarders Scala
generates for inherited default methods. Since DAP methods, unlike LSP methods, have no segment that tells such
copies apart, ViperServer is compiled with `-Xmixin-force-forwarders:false` (see `build.sbt`).

## Versioning

ViperServer's version is bumped to `3.3.0` for the debug adapter (`3.2.0` had an earlier, LSP-based version of
the debugger). The IDE's `MIN_SERVER_VERSION` is deliberately *not* raised, so the extension keeps working with
the older ViperServer release it pins in `client/viperserver-version`; instead the client refuses to start a
debug session against a server older than `MIN_SERVER_VERSION_FOR_DEBUGGER`. Once a release containing the
debugger exists, bump `client/viperserver-version` to it.

## Counterexamples

A counterexample is a model of the assumptions in which the assertion does not hold — that is, a model of
exactly the proof obligation the debugger is looking at. It is therefore not a separate feature but a second
view of the same session, and it follows the session: when the user adds or removes assumptions, the next
counterexample satisfies the new ones.

How it is obtained:

- **For the original failure**, the debug run itself computes one. `SymbolicExecutionRules.createFailure`
  attaches it to `SiliconDebuggingFailureContext.counterExample`, and `openObligation` picks it up.
- **For the current state**, `SiliconDebugSession.prove` extracts one from the prover: `assertUsingPushPop`
  already stores the model of a failed assertion (`retrieveAndSaveModel`), so after a failed proof attempt the
  session builds a `SiliconMappedCounterexample` from the obligation's symbolic state and that model —
  the same construction Silicon uses when it reports a verification error.

Any change to the assumptions marks the counterexample `stale` rather than dropping it, so the IDE can keep
showing the old values while making clear that they no longer match.

`CounterexampleModelBuilder` turns the `ExtractedModel` of a mapped counterexample into the same `DebugNode`
trees the obligation uses, grouped into sections: the values on return, the values at each labelled (old)
state, the domains and the functions of the model. Only `--counterexample=mapped` has that structure; other
kinds fall back to their textual representation.

The debug run therefore adds `--counterexample mapped --exhaleMode 1` (controlled by the
`viper.debugger.counterexample` setting). The more complete exhale mode keeps permissions in the heap, which
is what makes the heap part of a counterexample informative.

## A Silicon bug this uncovered

`SymbolicExecutionRules.withExp` used to be a `lazy val` reading `Verifier.config.enableDebugging()`. The rules
are singleton objects, so the value was fixed by the *first* verification in the JVM. That is harmless for the
command line (one verification per process) but meant that in a server, `--enableDebugging` had no effect
as soon as any verification had run without it: the symbolic execution then produced states whose expressions
were missing and crashed with `NoSuchElementException: None.get`. It is now a `def`. If you add further
`enableDebugging` checks, make sure they are not cached across verifications.

## Known limitations

1. **One session at a time, for the whole server.** Silicon keeps the configuration of the most recently
   created verifier in the global `Verifier.config`, and the frontend state the debugger needs for typechecking
   user input in the global `FrontendStateCache`. A concurrent verification would silently change the meaning
   of a live session, so running verifications are stopped before a session starts, only one session may exist
   at a time, and no verification is accepted while a session is being started or is open.
2. **The `FrontendStateCache` race is only mitigated, not fixed.** While a session is being started, the
   registry refuses to start verifications and parse/typecheck runs; but nothing prevents a second client from
   parsing a file in that window. Fixing this properly means snapshotting the frontend state after AST
   construction and handing it to the `Silicon` instance instead of reading the global.
3. **The debug run is a second verification.** Its failures can differ from those of the run whose diagnostics
   the user clicked on (branch parallelisation, prover randomness, timeouts), which is why failures are matched
   by position and message rather than by index.
4. **`--select` changes the program.** Verifying only the failing member is much faster, but the chopped
   program is not the one the diagnostics came from. If the error is not reproduced, the whole file is verified
   instead.
5. **Memory.** Each debuggable failure retains a live `State` (store, heap, all old heaps) and a `Verifier`,
   and the obligation retains the whole assumption graph. Closing a session is what releases this.
6. **Carbon is not supported.**
7. **Evaluating user input is best-effort.** Expressions are evaluated with `evaluator.eval3` against a state
   reconstructed from a finished run; heap-dependent functions, magic wands and quantified permissions may fail
   to evaluate or give misleading results.

## Testing

- `src/test/scala/viper/server/core/DebugSessionSpec.scala` drives a session through `DebugSessionRegistry`:
  starting a session for an error, proving, adding assumptions, expanding nodes, changing the print
  configuration, and verifying normally afterwards.
- `src/test/scala/viper/server/core/DebugAdapterSpec.scala` connects a DAP client to `DebugAdapterServer` and
  goes through DAP's initialization sequence like VS Code does, then inspects scopes and variables and runs
  Debug Console commands. `ConsoleCommandSpec` tests the parsing of those commands.
- The Silicon CLI can be exercised directly:
  `java -Xss128m -cp <silicon classpath> viper.silicon.SiliconRunner --enableDebugging <file.vpr>`
- viper-ide has `client/src/test/3_debugger.test.ts` for the IDE side.

Note that the interesting failure modes only appear once *another* verification has run in the same server,
so a test that debugs the very first verification of a JVM proves little. `DebugSessionSpec` and the IDE tests
both cover that case.

To try the whole stack locally, build `viperserver.jar` with `sbt assembly` and point VS Code at it:

```jsonc
"viper.buildVersion": "External",
"viper.viperServer.serverJars": ["/path/to/viperserver/target/scala-2.13/viperserver.jar"]
```
