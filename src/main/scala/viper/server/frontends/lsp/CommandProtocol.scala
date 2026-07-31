// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

/** This file contains custom LSP commands.
  *
  * There exists a similar file in the Viper IDE client, called 'ViperProtocol.ts', that should
  * contain the same set of commands. The set of commands in both files should be kept in sync.
  * */

object C2S_Commands {
  final val GetVersion = "GetVersion"
  final val Verify = "Verify"
  final val StopVerification = "StopVerification"
  final val GetLanguageServerUrl = "GetLanguageServerUrl"
  final val RemoveDiagnostics = "RemoveDiagnostics"
  final val GetViperFileEndings = "GetViperFileEndings"
  final val SetupProject = "SetupProject"
  final val FlushCache = "FlushCache"
  final val GetIdentifier = "GetIdentifier"
  final val GetRange = "GetRange"

  /* The verification debugger (Silicon only). */
  final val StartDebugSession = "StartDebugSession"
  final val StopDebugSession = "StopDebugSession"
  final val DebugSelectFailure = "DebugSelectFailure"
  final val DebugExpandNode = "DebugExpandNode"
  final val DebugAddAssumption = "DebugAddAssumption"
  final val DebugRemoveAssumptions = "DebugRemoveAssumptions"
  final val DebugProve = "DebugProve"
  final val DebugReset = "DebugReset"
  final val DebugSetPrintConfig = "DebugSetPrintConfig"
  final val DebugSetProver = "DebugSetProver"
  final val DebugSetTimeout = "DebugSetTimeout"
}

object S2C_Commands {
  final val StateChange = "StateChange"
  final val Log = "Log"
  final val Hint = "Hint"
  final val VerificationNotStarted = "VerificationNotStarted"
  final val UnhandledViperServerMessageType = "UnhandledViperServerMessageType"

  /* The verification debugger (Silicon only). */
  final val DebugSessionState = "DebugSessionState"
  final val DebugSessionClosed = "DebugSessionClosed"
}
