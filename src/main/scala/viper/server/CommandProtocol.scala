package viper.server

object C2S_Commands {
  final val RequestBackendNames = "RequestBackendNames" //void
  final val Dispose = "Dispose" //void
  final val Verify = "Verify" //VerifyParams
  final val StopVerification = "StopVerification"//filePath:String
  final val ShowHeap = "ShowHeap"//ShowHeapParams
  final val StartBackend = "StartBackend"//backendName:String
  final val StopBackend = "StopBackend"//void
  final val SwapBackend = "SwapBackend"//backendName:String
  final val GetExecutionTrace = "GetExecutionTrace"//GetExecutionTraceParams -> trace:ExecutionTrace[]
  final val RemoveDiagnostics = "RemoveDiagnostics"
  final val UpdateViperTools = "UpdateViperTools"
  final val GetViperFileEndings = "GetViperFileEndings"
  final val ViperUpdateComplete = "ViperUpdateComplete"
  final val FlushCache = "FlushCache"
  final val GetIdentifier = "GetIdentifier"
}

object S2C_Commands {
  final val BackendChange = "BackendChange"
  final val CheckIfSettingsVersionsSpecified = "CheckIfSettingsVersionsSpecified"
  final val SettingsChecked = "SettingsChecked"   //SettingsCheckedParams
  final val RequestRequiredVersion = "RequestRequiredVersion"   //void -> requiredVersions: Versions
  final val StateChange = "StateChange"   //StateChangeParams
  final val Log = "Log"   //LogParams
  final val Error = "Error"   //LogParams
  final val ToLogFile = "ToLogFile"   //LogParams
  final val Hint = "Hint"   //message: String
  final val Progress = "Progress"   //message: {domain:String, curr:number, total:number}
  final val FileOpened = "FileOpened"   //uri: String
  final val FileClosed = "FileClosed"   //uri: String
  final val VerificationNotStarted = "VerificationNotStarted"   //uri: String
  final val StopDebugging = "StopDebugging"   //void
  final val BackendReady = "BackendReady"   //BackendReadyParams
  final val StepsAsDecorationOptions = "StepsAsDecorationOptions"   //StepsAsDecorationOptionsResult
  final val HeapGraph = "HeapGraph"   //HeapGraph
  final val UnhandledViperServerMessageType = "UnhandledViperServerMessageType"
}