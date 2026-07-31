# The verification debugger

The verification debugger lets a user interrogate a failed Viper verification: it shows the symbolic state,
the path conditions and the assumptions at the point where verification failed, and lets the user modify them
and re-run the prover. It only works with the Silicon backend and it is **experimental**.

This document describes how the feature is put together across the three repositories, so that it can be
extended without having to re-discover the architecture.

## Overview

```
viper-ide (TypeScript)            ViperServer (Scala)                    Silicon (Scala)
─────────────────────             ───────────────────                    ───────────────
DebugCodeActionProvider  ──LSP──▶ Receiver.onStartDebugSession
DebugController                   DebugSessionRegistry.start
DebugTreeProvider                   ViperServerService.startDebugVerification
                                      (re-verifies with --enableDebugging)
                                    ServerDebugSession           ───────▶ SiliconDebugSession
                                    DebugProtocolConverter       ◀─────── ObligationModel
```

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
and closes sessions. A session is closed when a verification is started, when the debugged file is edited or
closed, when the client disconnects, and on shutdown.

The message stream of a debug run goes to `DebugDrainActor` rather than the file's `RelayActor`: relaying it
normally would overwrite the user's diagnostics and emit spurious state-change notifications.

## Protocol

`frontends/lsp/CommandProtocol.scala` declares the method names and `frontends/lsp/DebugProtocol.scala` the
payloads; `client/src/ViperProtocol.ts` in viper-ide mirrors both and must be kept in sync. Since LSP4J
serializes with plain Gson, the payloads use `Array`, `null` and sentinel numbers rather than Scala's `Seq` and
`Option`.

Positions are converted from Viper's 1-based to LSP's 0-based coordinates in `DebugProtocolConverter`.

## Versioning

ViperServer's version is bumped to `3.2.0` for the debugger messages. The IDE's `MIN_SERVER_VERSION` is
deliberately *not* raised, so the extension keeps working with the older ViperServer release it pins in
`client/viperserver-version`; instead the client refuses to start a debug session against a server older than
`MIN_SERVER_VERSION_FOR_DEBUGGER`. Once a release containing the debugger exists, bump
`client/viperserver-version` to it.

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
   of a live session, so sessions are closed and running verifications are stopped before a session starts,
   only one session may exist at a time, and no verification is accepted while one is being started.
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

- `src/test/scala/viper/server/core/DebugSessionSpec.scala` drives a session end to end through the same code
  paths the IDE uses (no VS Code needed): starting a session for an error, proving, adding assumptions,
  expanding nodes, changing the print configuration, and verifying normally afterwards.
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
