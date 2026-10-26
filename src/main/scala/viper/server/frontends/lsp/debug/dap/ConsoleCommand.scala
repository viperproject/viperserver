// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.frontends.lsp.debug.dap

/**
  * A command entered in the Debug Console. Commands start with a colon, so that a plain Viper expression can
  * later be given a meaning of its own (checking whether it holds in the current state).
  */
sealed trait ConsoleCommand

object ConsoleCommand {
  case object Help extends ConsoleCommand
  case object Prove extends ConsoleCommand
  case object Reset extends ConsoleCommand
  case object Text extends ConsoleCommand
  case object Counterexample extends ConsoleCommand
  /** Adds `expression` to the assumptions; unless `free`, it has to be provable first. */
  case class Assume(expression: String, free: Boolean) extends ConsoleCommand
  case class Remove(ids: Seq[Int]) extends ConsoleCommand
  case class Solver(name: String, args: Option[String]) extends ConsoleCommand
  /** A timeout of `None` means no timeout. */
  case class Timeout(ms: Option[Int]) extends ConsoleCommand
  case class Set(setting: PrintSetting, value: Either[Boolean, Int]) extends ConsoleCommand
  /** Input that does not start with a colon. */
  case class Expression(text: String) extends ConsoleCommand
  case class Invalid(message: String) extends ConsoleCommand

  /** The print settings that `:set` can change, by the name the user types. */
  sealed abstract class PrintSetting(val name: String, val isBoolean: Boolean)
  object PrintSetting {
    case object Internal extends PrintSetting("internal", true)
    case object Axioms extends PrintSetting("axioms", true)
    case object Terms extends PrintSetting("terms", true)
    case object OldHeaps extends PrintSetting("oldHeaps", true)
    case object Depth extends PrintSetting("depth", false)
    case object Children extends PrintSetting("children", false)
    val all: Seq[PrintSetting] = Seq(Internal, Axioms, Terms, OldHeaps, Depth, Children)
  }

  val help: String =
    """Commands of the Viper verification debugger:
      |  :prove                 try to prove the proof obligation again
      |  :assert <expression>   prove <expression> and add it to the assumptions
      |  :assume <expression>   add <expression> to the assumptions without proving it
      |  :remove <id>, ...      remove the assumptions with the given ids (shown as [id] in the Variables view)
      |  :reset                 discard all changes made to the proof obligation
      |  :solver <name> [args]  use another SMT solver (z3 or cvc5), optionally with command line arguments
      |  :timeout <ms>          set the prover timeout; 0 means no timeout
      |  :set <setting> <value> change what is shown: internal, axioms, terms, oldHeaps (on/off),
      |                         depth, children (numbers)
      |  :text                  print the proof obligation as text
      |  :counterexample        print the counterexample as text
      |  :help                  show this help
      |Continue goes to the next verification error.""".stripMargin

  def parse(input: String): ConsoleCommand = {
    val trimmed = input.trim
    if (!trimmed.startsWith(":")) return Expression(trimmed)

    val (name, rest) = trimmed.drop(1).span(c => !c.isWhitespace) match {
      case (n, r) => (n.toLowerCase, r.trim)
    }
    name match {
      case "help" | "h" | "?" => Help
      case "prove" | "p" => Prove
      case "reset" | "r" => Reset
      case "text" => Text
      case "counterexample" | "ce" => Counterexample
      case "assert" | "ap" => nonEmpty(rest, ":assert")(Assume(_, free = false))
      case "assume" | "af" => nonEmpty(rest, ":assume")(Assume(_, free = true))
      case "remove" | "ra" =>
        val parts = rest.split("[,\\s]+").filter(_.nonEmpty).toSeq
        val ids = parts.map(p => p.stripPrefix("[").stripSuffix("]").toIntOption)
        if (ids.isEmpty) Invalid("Usage: :remove <id>, ...")
        else if (ids.exists(_.isEmpty)) Invalid(s"Not an assumption id: ${parts.zip(ids).collectFirst { case (p, None) => p }.get}")
        else Remove(ids.flatten)
      case "solver" | "s" =>
        val (solver, args) = rest.span(c => !c.isWhitespace)
        if (solver.isEmpty) Invalid("Usage: :solver <name> [args]")
        else Solver(solver, Some(args.trim).filter(_.nonEmpty))
      case "timeout" | "t" =>
        rest.toIntOption match {
          case Some(ms) if ms >= 0 => Timeout(Some(ms).filter(_ > 0))
          case _ => Invalid("Usage: :timeout <ms>, where 0 means no timeout")
        }
      case "set" =>
        val (key, value) = rest.span(c => !c.isWhitespace)
        PrintSetting.all.find(_.name.equalsIgnoreCase(key)) match {
          case None =>
            Invalid(s"Unknown setting '$key'. Settings are ${PrintSetting.all.map(_.name).mkString(", ")}.")
          case Some(setting) if setting.isBoolean =>
            parseBoolean(value.trim) match {
              case Some(b) => Set(setting, Left(b))
              case None => Invalid(s"Usage: :set ${setting.name} on|off")
            }
          case Some(setting) =>
            value.trim.toIntOption match {
              case Some(n) if n >= 0 => Set(setting, Right(n))
              case _ => Invalid(s"Usage: :set ${setting.name} <number>")
            }
        }
      case other => Invalid(s"Unknown command ':$other'. Type :help for a list of commands.")
    }
  }

  private def nonEmpty(arg: String, command: String)(f: String => ConsoleCommand): ConsoleCommand =
    if (arg.isEmpty) Invalid(s"Usage: $command <expression>") else f(arg)

  private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match {
    case "on" | "true" | "1" | "yes" => Some(true)
    case "off" | "false" | "0" | "no" => Some(false)
    case _ => None
  }
}
