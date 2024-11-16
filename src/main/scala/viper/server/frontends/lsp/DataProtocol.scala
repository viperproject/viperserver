// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import org.eclipse.lsp4j.{Diagnostic, Position, Range}
import viper.silver.reporter.SymbolKind

object VerificationSuccess extends Enumeration {
  type VerificationSuccess = Value

  val NA, Success, ParsingFailed, TypecheckingFailed = Value
  val VerificationFailed: VerificationSuccess = Value  // Manually aborted verification
  val Aborted: VerificationSuccess = Value             // Caused by internal error
  val Error: VerificationSuccess = Value               // Caused by verification taking too long
  val Timeout: VerificationSuccess = Value
}
import viper.server.frontends.lsp.VerificationSuccess._

object VerificationState extends Enumeration {
  type VerificationState = Value

  val Stopped, Starting = Value
  val VerificationRunning, VerificationPrintingHelp, VerificationReporting = Value
  val PostProcessing, Ready, Stopping, Stage = Value
}

object SettingsErrorType extends Enumeration {
  type SettingsErrorType = Value

  val Error, Warning = Value
}
import viper.server.frontends.lsp.SettingsErrorType._

object LogLevel extends Enumeration {
  type LogLevel = Value

  val None: LogLevel = Value // No output
  val Default: LogLevel = Value // Only verification specific output
  val Info: LogLevel = Value // Some info about internal state, critical errors
  val Verbose: LogLevel = Value // More info about internal state
  val Debug: LogLevel = Value // Detailed information about internal state, non critical errors
  val LowLevelDebug: LogLevel = Value // all output of used tools is written to logFile, some of it also to the console
}

object BackendOutputType {
  val Start = "Start"
  val End = "End"
  val VerificationStart = "VerificationStart"
  val MethodVerified = "MethodVerified"
  val FunctionVerified = "FunctionVerified"
  val PredicateVerified = "PredicateVerified"
  val Error = "Error"
  val Outline = "Outline"
  val Definitions = "Definitions"
  val Success = "Success"
  val Stopped = "Stopped"
}

case class ProgressParams(data: Progress, logLevel: Int)

case class Progress (
              domain: String,
              current: Double,
              total: Double,
              progress: Double,
              postfix: Double)

case class BackendProperties(
              name: String,
              backend_type: String,
              paths: Array[String] = null,
              engine: String = "Viper",
              timeout: Int = 5000,
              stages: Array[Stage] = null,
              stoppingTimeout: Int = 5000,
              version: String = null)

case class VerifyParams (
              uri: String,                  // file to verify
              manuallyTriggered: Boolean,   // was the verification triggered manually
              workspace: String,            // the path to the open workspace folder
              backend: String,
              customArgs: String)            // contains the path of the file that should be verified

case class ReformatParams (
                          uri: String)

case class SettingsError (errorType: SettingsErrorType, msg: String)

case class Stage(
              name: String,      //The per backend unique name of this stage
              isVerification: Boolean,       //Enable if this stage is describing a verification
              mainMethod: String,      //The method to invoke when staring the stage
              customArguments: String,       //the commandline arguments for the java engine
              onParsingError: String,      //The name of the stage to start in case of a parsing error
              onTypeCheckingError: String,       //The name of the stage to start in case of a type checking error
              onVerificationError: String,       //The name of the stage to start in case of a verification error
              onSuccess: String)       //The name of the stage to start in case of a success

case class PlatformDependentPath (
              windows: Option[String],
              mac: Option[String],
              linux: Option[String])

case class PlatformDependentURL (
              windows: Option[String],
              mac: Option[String],
              linux: Option[String])

// scope == null means global scope
case class Definition(definition_type: SymbolKind, name: String, code_location: Position, scope: Range)

case class BackendOutput(
              typ: String,
              name: String = null,
              backendType: String = null,
              nofMethods: Int = -1,
              nofPredicates: Int = -1,
              nofFunctions: Int = -1,  //for End
              time: Long = -1,  //for Error
              file: String = null,
              errors: Array[Error] = null,  //for Outline
              definitions: Array[Definition] = null)

////////////////////////////////////////////////////////////////////////////////////////////////////
//////              SETTINGS                                                                 ///////
////////////////////////////////////////////////////////////////////////////////////////////////////

case class GetVersionRequest(clientVersion: String)

case class GetVersionResponse(
                               serverVersion: String,
                               error: String = null // error message if client is not supported by server, null otherwise
                             )

case class LogParams(data: String, logLevel: Int)

case class HintMessage(message: String, showSettingsButton: Boolean, showViperToolsUpdateButton: Boolean)

case class BackendReadyParams(
              name: String,  // name of the backend ready to use
              restarted: Boolean, // should the open file be reverified
              isViperServer: Boolean)

case class BackendStartedParams(
              name: String,
              forceRestart: Boolean = false,
              isViperServer: Boolean = true)

case class StateChangeParams(
              newState: Int,
              progress: Double = -1,
              success: Int = NA.id,
              verificationCompleted: Double = -1,
              manuallyTriggered: Double = -1,
              filename: String = null,
              backendName: String = null,
              time: Double = -1, /** [sec] */
              verificationNeeded: Double = -1,
              uri: String = null,
              stage: String = null,
              error: String = null,
              diagnostics: Array[Diagnostic] = null)

case class UnhandledViperServerMessageTypeParams(msgType: String, msg: String, logLevel: Int)

/**
  *
  * @param uri nullable (null indicates that the cache for all files should be flushed)
  * @param backend non-null
  */
case class FlushCacheParams(uri: String, backend: String)

case class StopVerificationRequest(uri: String)

case class StopVerificationResponse(success: Boolean)

case class GetLanguageServerUrlResponse(url: String)

case class RemoveDiagnosticsRequest(uri: String)

case class RemoveDiagnosticsResponse(success: Boolean)

case class GetViperFileEndingsResponse(fileEndings: Array[String])

case class GetIdentifierResponse(identifier: String)

case class VerificationNotStartedParams(uri: String)

////////////////////////////////////////////////////////////////////////////////////////////////////
//////              SETTINGS                                                                 ///////
////////////////////////////////////////////////////////////////////////////////////////////////////

case class ViperServerSettings(
                                serverJars: Array[String],     // Locator to the ViperServer jars
                                customArguments: String,       // custom commandLine arguments
                                backendSpecificCache: Boolean, // it set to false, cached errors are reused across backends
                                disableCaching: Boolean,    // disable the caching mechanism
                                timeout: Int,               // After timeout ms the startup of the viperServer is expected to have failed and thus aborted
                                viperServerPolicy: String,  // Specifies whether ViperServer should be started by the IDE or whether the IDE should attach to an existing instance of ViperServer. Possible values: "attach", "create".
                                viperServerAddress: String, // Specifies the address part of the URL that ViperServer is running on.
                                viperServerPort: Int,       // Specifies the port part of the URL that ViperServer is running on. Only needed if viperServerPolicy is set to 'attach'.
                                version: String ) extends VersionedSettings

case class Versions(
                     viperServerSettingsVersion: String,
                     backendSettingsVersion: String,
                     pathSettingsVersion: String,
                     userPreferencesVersion: String,
                     javaSettingsVersion: String,
                     advancedFeaturesVersion: String,
                     defaultSettings: AnyRef,
                     extensionVersion: String)

case class ViperSettings(
                          viperServerSettings: ViperServerSettings, // All viperServer related settings
                          verificationBackends: Array[BackendProperties], // Description of backends
                          paths: PathSettings, // Used paths
                          preferences: UserPreferences, // General user preferences
                          javaSettings: JavaSettings, // Java settings
                          advancedFeatures: AdvancedFeatureSettings) // Settings for AdvancedFeatures

trait VersionedSettings {
  val version: String
}

case class PathSettings(
                         viperToolsPath: Either[String, PlatformDependentPath],    // Path to the folder containing all the ViperTools
                         z3Executable: Either[String, PlatformDependentPath],      // The path to the z3 executable
                         boogieExecutable: Either[String, PlatformDependentPath],  // The path to the boogie executable
                         version: String) extends VersionedSettings

case class UserPreferences (
                             autoSave: Boolean,        //Enable automatically saving modified viper files
                             logLevel: Int,            //Verbosity of the output, all output is written to the logFile, regardless of the logLevel
                             autoVerifyAfterBackendChange: Boolean,      //Reverify the open viper file upon backend change.
                             showProgress: Boolean,   //Display the verification progress in the status bar. Only useful if the backend supports progress reporting.
                             viperToolsProvider: Either[String, PlatformDependentURL], //The URL for downloading the ViperTools from
                             version: String) extends VersionedSettings

//The arguments used for all java invocations
case class JavaSettings(customArguments: String, version: String) extends VersionedSettings

case class AdvancedFeatureSettings(
                                    enabled: Boolean,         //Enable heap visualization, stepwise debugging and execution path visualization
                                    showSymbolicState: Boolean,     //Show the symbolic values in the heap visualization. If disabled, the symbolic values are only shown in the error states.
                                    darkGraphs: Boolean,      //To get the best visual heap representation, this setting should match with the active theme.
                                    simpleMode: Boolean,      //Useful for verifying programs. Disable when developing the backend
                                    showOldState: Boolean,    //Visualize also the oldHeap in the heap preview
                                    showPartialExecutionTree: Boolean,    //Show the part of the execution tree around the current state in the state visualization
                                    verificationBufferSize: Int,          //Maximal buffer size for verification in KB
                                    compareStates: Boolean,
                                    version: String) extends VersionedSettings
