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
  final val RequestBackendNames = "RequestBackendNames"
  final val Verify = "Verify"
  final val StopVerification = "StopVerification"
  // final val ShowHeap = "ShowHeap"
  final val StartBackend = "StartBackend"
  final val StopBackend = "StopBackend"
  final val SwapBackend = "SwapBackend"
  // final val GetExecutionTrace = "GetExecutionTrace"
  final val RemoveDiagnostics = "RemoveDiagnostics"
  final val GetViperFileEndings = "GetViperFileEndings"
  final val FlushCache = "FlushCache"
  final val GetIdentifier = "GetIdentifier"
}

object S2C_Commands {
  final val BackendChange = "BackendChange"
  final val CheckIfSettingsVersionsSpecified = "CheckIfSettingsVersionsSpecified"
  final val StateChange = "StateChange"
  final val Log = "Log"
  final val Hint = "Hint"
  final val Progress = "Progress"
  final val FileOpened = "FileOpened"
  final val FileClosed = "FileClosed"
  final val VerificationNotStarted = "VerificationNotStarted"
  // final val StopDebugging = "StopDebugging"
  final val BackendReady = "BackendReady"
  // final val StepsAsDecorationOptions = "StepsAsDecorationOptions"
  // final val HeapGraph = "HeapGraph"
  final val UnhandledViperServerMessageType = "UnhandledViperServerMessageType"
  final val BackendStarted = "BackendStarted"
}