// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.frontends.lsp

/**
  * The only LSP message of the verification debugger. The debugger itself speaks the Debug Adapter Protocol
  * (see [[viper.server.frontends.lsp.debug.dap.ViperDebugAdapter]]) on a socket of its own; this request merely
  * opens that socket. There is a corresponding interface in the Viper IDE client, in 'ViperProtocol.ts', which
  * has to be kept in sync with this file.
  *
  * @param port  the port on which the debug adapter waits for a single connection, or -1 if `error` is set.
  * @param error null, or why no debug adapter could be started.
  */
case class StartDebugAdapterResponse(port: Int, error: String = null)
