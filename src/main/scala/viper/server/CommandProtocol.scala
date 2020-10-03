package viper.server

object C2S_Commands {
  val RequestBackendNames = "RequestBackendNames" //void
  val Dispose = "Dispose" //void
  val Verify = "Verify" //VerifyParams
  val StopVerification = "StopVerification"//filePath:String
  val ShowHeap = "ShowHeap"//ShowHeapParams
  val StartBackend = "StartBackend"//backendName:String
  val StopBackend = "StopBackend"//void
  val SwapBackend = "SwapBackend"//backendName:String
  val GetExecutionTrace = "GetExecutionTrace"//GetExecutionTraceParams -> trace:ExecutionTrace[]
  val RemoveDiagnostics = "RemoveDiagnostics"
  val UpdateViperTools = "UpdateViperTools"
  val GetViperFileEndings = "GetViperFileEndings"
  val ViperUpdateComplete = "ViperUpdateComplete"
  val FlushCache = "FlushCache"
  val GetIdentifier = "GetIdentifier"
}

object S2C_Commands {
  val BackendChange = "BackendChange"
  val CheckIfSettingsVersionsSpecified = "CheckIfSettingsVersionsSpecified"
  val SettingsChecked = "SettingsChecked"   //SettingsCheckedParams
  val RequestRequiredVersion = "RequestRequiredVersion"   //void -> requiredVersions: Versions
  val StateChange = "StateChange"   //StateChangeParams
  val Log = "Log"   //LogParams
  val Error = "Error"   //LogParams
  val ToLogFile = "ToLogFile"   //LogParams
  val Hint = "Hint"   //message: String
  val Progress = "Progress"   //message: {domain:String, curr:number, total:number}
  val FileOpened = "FileOpened"   //uri: String
  val FileClosed = "FileClosed"   //uri: String
  val VerificationNotStarted = "VerificationNotStarted"   //uri: String
  val StopDebugging = "StopDebugging"   //void
  val BackendReady = "BackendReady"   //BackendReadyParams
  val StepsAsDecorationOptions = "StepsAsDecorationOptions"   //StepsAsDecorationOptionsResult
  val HeapGraph = "HeapGraph"   //HeapGraph
  val UnhandledViperServerMessageType = "UnhandledViperServerMessageType"
}