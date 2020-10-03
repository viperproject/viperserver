package viper.server

import java.util.concurrent.CompletableFuture

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

case class ResolvedPath (path: String, exists: Boolean, error: Option[String])

object Settings {
  implicit val ex = ExecutionContext.global
  var settings: ViperSettings
  var isWin = System.getProperties.get("os.name")
  var isLinux = /^linux/.test(process.platform)
  var isMac = /^darwin/.test(process.platform)
  var workspace: String = _
  var VERIFY = "verify"
  var selectedBackend: String

  private var firstSettingsCheck = true

  private var _valid: Boolean = false
  private var _errors: Array[SettingsError] = Array()
  private var _upToDate: Boolean = false

  private var home = os.homedir();

  def getStage(backend: Backend, name: Option[String]): Option[Stage] = {
    for {
      n <- name
      res <- backend.stages.find(_.name == n)
    } yield res
  }

  def getStageFromSuccess(backend: Backend, stage: Stage, success: Success): Stage = {
    switch (success) {
    case VerificationSuccess.ParsingFailed:
    return this.getStage(backend, stage.onParsingError);
    case VerificationSuccess.VerificationFailed:
    return this.getStage(backend, stage.onVerificationError);
    case VerificationSuccess.TypecheckingFailed:
    return this.getStage(backend, stage.onTypeCheckingError);
    case VerificationSuccess.Success:
    return this.getStage(backend, stage.onSuccess);
  }
    return null;
  }

  def backendEquals(a: Backend, b: Backend): Boolean = {
    var areEqual: Boolean = a.stages.length == b.stages.length
    areEqual = areEqual && a.name == b.name
    areEqual = areEqual && a.backend_type == b.backend_type
    areEqual = areEqual && a.timeout == b.timeout
    areEqual = areEqual && this.resolveEngine(a.engine) == this.resolveEngine(b.engine);
    a.stages.forEach((element, i) => {
      areEqual = areEqual && this.stageEquals(element, b.stages[i]);
    });
    areEqual = areEqual && a.paths.length == b.paths.length;
    for (var i = 0; i < a.paths.length; i++) {
      areEqual = areEqual && a.paths[i] == b.paths[i];
    }
    return areEqual;
  }

  def backendEquals(one: Option[Backend], other: Option[Backend]): Option[Boolean] = {
    for {
      a <- one
      b <- other
    } yield backendEquals(a, b)
  }

  private def resolveEngine(engine: String) {
    if (engine && (engine.toLowerCase() == "viperserver")) {
      return engine;
    } else {
      return "none";
    }
  }

  def useViperServer(backend: Backend) {
    if (!backend || !backend.engine) return false;
    return backend.engine.toLowerCase() == "viperserver";
  }

  private def stageEquals(a: Stage, b: Stage): Boolean {
    var same = a.customArguments == b.customArguments;
    same = same && a.mainMethod == b.mainMethod;
    same = same && a.name == b.name;
    same = same && a.isVerification == b.isVerification;
    same = same && a.onParsingError == b.onParsingError;
    same = same && a.onTypeCheckingError == b.onTypeCheckingError;
    same = same && a.onVerificationError == b.onVerificationError;
    same = same && a.onSuccess == b.onSuccess;
    return same;
  }

  def expandCustomArguments(args: String, stage: Stage, fileToVerify: String, backend: Backend): String {
    //Log.log("Command before expanding: " + args,LogLevel.LowLevelDebug);
    args = args.replace(/\s+/g, ' '); //remove multiple spaces
    args = args.replace(/\$z3Exe\$/g, '"' + this.settings.paths.z3Executable + '"');
    args = args.replace(/\$boogieExe\$/g, '"' + this.settings.paths.boogieExecutable + '"');
    args = args.replace(/\$mainMethod\$/g, stage.mainMethod);
    args = args.replace(/\$backendPaths\$/g, Settings.backendJars(backend));
    args = args.replace(/\$disableCaching\$/g, (Settings.settings.viperServerSettings.disableCaching === true ? "--disableCaching" : ""));
    args = args.replace(/\$fileToVerify\$/g, '"' + fileToVerify + '"');
    args = args.replace(/\s+/g, ' '); //remove multiple spaces
    //Log.log("Command after expanding: " + args.trim(),LogLevel.LowLevelDebug);

    return args.trim();
  }

  def expandViperToolsPath(path: String): String = {
    if (!path) return path
    if (typeof Settings.settings.paths.viperToolsPath !== "String") {
      return path
    }
//    path = path.replace(/\$viperTools\$/g, [String>Settings.settings.paths.viperToolsPath)
    return path
  }

  def selectBackend(settings: ViperSettings, selectedBackend: String): Backend = {
    if (selectedBackend !=  null) {
      Settings.selectedBackend = selectedBackend;
    }
    if (!settings || !settings.verificationBackends || settings.verificationBackends.length == 0) {
      this.selectedBackend = null;
      return null;
    }
    if (this.selectedBackend) {
//      for (var i = 0; i < settings.verificationBackends.length; i++) {
//        var backend = settings.verificationBackends[i];
//        if (backend.name === this.selectedBackend) {
//          return backend;
//        }
//      }
    }
    this.selectedBackend = settings.verificationBackends[0].name;
    return settings.verificationBackends[0];
  }

    def getBackendNames(settings: ViperSettings): Array[String] {
    var backendNames = [];
    settings.verificationBackends.forEach((backend) => {
    backendNames.push(backend.name);
  })
    return backendNames;
    }

    def getBackend(backendName: String): Backend {
    return Settings.settings.verificationBackends.find(b => { return b.name == backendName });
    }

    def valid(): Boolean {
    LanguageServerState.sendSettingsCheckedNotification({ ok: this._valid, errors: this._errors, settings: this.settings });
    return this._valid;
    }

    def upToDate(): Boolean {
    return this._upToDate;
    }

    private def viperServerRelatedSettingsChanged(oldSettings: ViperSettings) {
    if (!oldSettings) return true;
    if ((Array[[String]]oldSettings.viperServerSettings.serverJars).length != (Array[[String]]this.settings.viperServerSettings.serverJars).length)
    return true;
    (Array[[String]]oldSettings.viperServerSettings.serverJars).forEach((path, index) => {
    if (path != (Array[[String]]this.settings.viperServerSettings.serverJars)[index]) {
    return true;
  }
  })
    if (oldSettings.viperServerSettings.backendSpecificCache != this.settings.viperServerSettings.backendSpecificCache
    || oldSettings.viperServerSettings.customArguments != this.settings.viperServerSettings.customArguments
    //|| oldSettings.viperServerSettings.disableCaching != this.settings.viperServerSettings.disableCaching //no need to restart the ViperServer if only that changes
    || oldSettings.viperServerSettings.timeout != this.settings.viperServerSettings.timeout
    ) {
    return true;
  }
    Log.log("ViperServer settings did not change", LogLevel.LowLevelDebug);
    return false;
    }

    //tries to restart backend,
    def initiateBackendRestartIfNeeded(oldSettings?: ViperSettings, selectedBackend?: String, viperToolsUpdated: Boolean = false) {
    Settings.checkSettings(viperToolsUpdated).then(() => {
    if (Settings.valid()) {
    var newBackend = Settings.selectBackend(Settings.settings, selectedBackend);

    if (newBackend) {
    //only restart the backend after settings changed if the active backend was affected

    Log.log("check if restart needed", LogLevel.LowLevelDebug);
    var backendChanged = !Settings.backendEquals(Coordinator.backend, newBackend) //change in backend
    var mustRestartBackend = !Coordinator.backendService.isReady() //backend is not ready -> restart
    || viperToolsUpdated //Viper Tools Update might have modified the binaries
    || (Coordinator.backendService.isViperServerService != this.useViperServer(newBackend)) //the new backend requires another engine type
    || (Settings.useViperServer(newBackend) && this.viperServerRelatedSettingsChanged(oldSettings)) // the viperServerSettings changed
    if (mustRestartBackend || backendChanged) {
    Log.log(`Change Backend: from ${LanguageServerState.backend ? LanguageServerState.backend.name : "No Backend"} to ${newBackend ? newBackend.name : "No Backend"}`, LogLevel.Info);
      Coordinator.backend = newBackend;
      Coordinator.verificationTasks.forEach(task => task.resetLastSuccess());
      Coordinator.sendStartBackendMessage(Coordinator.backend.name, mustRestartBackend, Settings.useViperServer(newBackend));
  } else {
    Log.log("No need to restart backend. It is still the same", LogLevel.Debug)
      Coordinator.backend = newBackend;
      Coordinator.sendBackendReadyNotification({
    name: Coordinator.backend.name,
    restarted: false,
    isViperServer: Settings.useViperServer(newBackend)
  });
  }
  } else {
    Log.debug("No backend, even though the setting check succeeded.");
  }
  } else {
      Coordinator.backendService.stop();
  }
  });
    }

    private def addError(msg: String) {
    this._errors.push({ type: SettingsErrorType.Error, msg: msg });
    }
    private def addErrors(errors: SettingsError[]) {
    this._errors = this._errors.concat(errors);
    }
    private def addWarning(msg: String) {
    this._errors.push({ type: SettingsErrorType.Warning, msg: msg });
    }

//  private def checkSettingsVersion(settings: AnyRef, requiredVersions: AnyRef): Array[String] = {
//    var oldSettings = Array()
//    //check the settings versions
//    if (!requiredVersions) {
//      Log.error("Getting required version failed.");
//    } else {
//      if (Version.createFromVersion(requiredVersions.advancedFeaturesVersion).compare(Version.createFromHash(settings.advancedFeatures.v)) > 0) {
//      oldSettings.push("advancedFeatures");
//    }
//      if (Version.createFromVersion(requiredVersions.javaSettingsVersion).compare(Version.createFromHash(settings.javaSettings.v)) > 0) {
//      oldSettings.push("javaSettings");
//    }
//      if (Version.createFromVersion(requiredVersions.viperServerSettingsVersion).compare(Version.createFromHash(settings.viperServerSettings.v)) > 0) {
//      oldSettings.push("viperServerSettings");
//    }
//      if (Version.createFromVersion(requiredVersions.pathSettingsVersion).compare(Version.createFromHash(settings.paths.v)) > 0) {
//      oldSettings.push("paths");
//    }
//      if (Version.createFromVersion(requiredVersions.userPreferencesVersion).compare(Version.createFromHash(settings.preferences.v)) > 0) {
//      oldSettings.push("preferences");
//    }
//      var requiredBackendVersion = Version.createFromVersion(requiredVersions.backendSettingsVersion);
//      settings.verificationBackends.forEach(backend => {
//      if (requiredBackendVersion.compare(Version.createFromHash(backend.v)) > 0) {
//      oldSettings.push("backend " + backend.name);
//    }
//    });
//    }
//    return oldSettings;
//  }



//  def checkSettings(viperToolsUpdated: Boolean): Future[Boolean] = {
//    try {
//      _valid = false;
//      _errors = Array();
//      _upToDate = false;
//
//      def getSettingErrorFuture: Future[Array[SettingsError]] = Future {
//        val cf = LanguageServerState.client.checkIfSettingsVersionsSpecified()
//        val res = cf.join()
//        res
//      }
//
//      def hasErrors(errors: Array[SettingsError]): Boolean = {
//        if (errors.nonEmpty) {
//          errors.foreach(addErrors)
//          true
//        } else {
//          false
//        }
//      }
//
//      def getVersionsFuture(errors: Array[SettingsError]): Future[Option[Versions]] = {
//        if(hasErrors(errors)) {
//            Future {
//              None
//            }
//        } else {
//          Future {
//            val cf = LanguageServerState.client.requestRequiredVersion()
//            val res = cf.join()
//            Some(res)
//          }
//        }
//      }
//
//      def useVersions(versions: Option[Versions]): Future[Boolean] = {
//        Future {
//          case None =>
//            false
//          case Some(v) =>
//            if (firstSettingsCheck) {
//              Log.log("Extension Version: " + v.extensionVersion + " - " + Version.hash(requiredVersions.extensionVersion), LogLevel.LowLevelDebug)
//              firstSettingsCheck = false;
//            }
//            var settings = Settings.settings;
//            var oldSettings: Array[String] = this.checkSettingsVersion(settings, v);
//            var defaultSettings = v.defaultSettings;
//
//            if (oldSettings.nonEmpty) {
//              var affectedSettings = oldSettings.length < 10 ? "(" + oldSettings.join(", ") + ")": "(" + oldSettings.length + ")";
//              addError("Old viper settings detected: " + affectedSettings + " please replace the old settings with the new default settings.");
//              false
//            } else {
//              _upToDate = true;
//              //Check viperToolsProvider
//              settings.preferences.viperToolsProvider = this.checkPlatformDependentUrl(settings.preferences.viperToolsProvider);
//
//              //Check Paths
//              //check viperToolsPath
//              var resolvedPath: ResolvedPath = this.checkPath(settings.paths.viperToolsPath, "Path to Viper Tools:", false, true, true);
//              settings.paths.viperToolsPath = resolvedPath.path;
//                if (!resolvedPath.exists) {
//                  if (!viperToolsUpdated) {
//                    //Automatically install the Viper tools
//                    LanguageServerState.updateViperTools(true);
//                    reject(); // in this case we do not want to continue restarting the backend,
//                    //the backend will be restarted after the update
//                  } else {
//                    resolve(false);
//                  }
//                  return;
//                }
//
//                //check z3 Executable
//                settings.paths.z3Executable = this.checkPath(settings.paths.z3Executable, "z3 Executable:", true, true, true).path;
//                //check boogie executable
//                settings.paths.boogieExecutable = this.checkPath(settings.paths.boogieExecutable, `Boogie Executable: (If you don't need boogie, set it to "")`, true, true, true).path;
//
//                //check backends
//                if (!settings.verificationBackends || settings.verificationBackends.length == 0) {
//                  settings.verificationBackends = defaultSettings["viperSettings.verificationBackends"].default;
//                } else {
//                  defaultSettings["viperSettings.verificationBackends"].default.forEach(defaultBackend => {
//                    var customBackend = settings.verificationBackends.filter(backend => backend.name == defaultBackend.name)[0];
//                    if (customBackend) {
//                      //Merge the backend with the default backend
//                      this.mergeBackend(customBackend, defaultBackend);
//                    } else {
//                      //Add the default backend if there is none with the same name
//                      settings.verificationBackends.push(defaultBackend);
//                    }
//                  })
//                }
//                Settings.checkBackends(settings.verificationBackends);
//
//                //check ViperServer related settings
//                var viperServerRequired = settings.verificationBackends.some(elem => this.useViperServer(elem));
//                if (viperServerRequired) {
//                  //check viperServer path
//                  settings.viperServerSettings.serverJars = this.checkPaths(settings.viperServerSettings.serverJars, "viperServerPath:");
//                  if (this.viperServerJars().trim().length == 0) {
//                    this.addError("Missing viper server jars at paths: " + JSON.Stringify(settings.viperServerSettings.serverJars))
//                  }
//                  //check viperServerTimeout
//                  settings.viperServerSettings.timeout = this.checkTimeout(settings.viperServerSettings.timeout, "viperServerSettings:");
//                  //check the customArguments
//                }
//
//                //no need to check preferences
//                //check java settings
//                if (!settings.javaSettings.customArguments) {
//                  this.addError("The customArguments are missing in the java settings");
//                }
//
//                //checks done
//                this._valid = !this._errors.some(error => error.
//                type ==
//                SettingsErrorType.Error
//                ); //if there is no error -> valid
//                if (this._valid) {
//                  Log.log("The settings are ok", LogLevel.Info);
//                  resolve(true);
//                } else {
//                  resolve(false);
//                }
//              }
//            }
//          }
//        }
//
//      for {
//        errors <- getSettingErrorFuture
//        versions <- getVersionsFuture(errors)
//        result <- useVersions(versions)
//      } yield result
//
//
//
//    }
//
//    })
//    } catch {
//      Log.error("Error checking settings: " + e)
//      resolve(false)
//    }
//    })
//  }

//  private def mergeBackend(custom: Backend, def: Backend) = {
//    if (!custom || !def || custom.name != def.name) return;
//    if (!custom.paths) custom.paths = def.paths;
//    if (!custom.stages) custom.stages = def.stages
//    else this.mergeStages(custom.stages, def.stages);
//    if (!custom.timeout) custom.timeout = def.timeout;
//    if (!custom.engine || custom.engine.length == 0) custom.engine = def.engine;
//    if (!custom.type || custom.type.length == 0) custom.type = def.type;
//    }
//
//  private def mergeStages(custom: Stage[], defaultStages: Stage[]) = {
//    defaultStages.forEach(def => {
//    var cus = custom.filter(stage => stage.name == def.name)[0];
//    if (cus) {
//    //merge
//    if (cus.customArguments === undefined) cus.customArguments = def.customArguments;
//    if (!cus.mainMethod) cus.mainMethod = def.mainMethod;
//    if (cus.isVerification === undefined) cus.isVerification = def.isVerification;
//  } else {
//    custom.push(def);
//  }
//  });
//    }
//
//  private def checkPlatformDependentUrl(url: String | PlatformDependentURL): String = {
//    var StringURL = null;
//    if (url) {
//    if (typeof url === "String") {
//    StringURL = url;
//  } else {
//    if (Settings.isLinux) {
//    StringURL = url.linux;
//  } else if (Settings.isMac) {
//    StringURL = url.mac;
//  } else if (Settings.isWin) {
//    StringURL = url.windows;
//  } else {
//    Log.error("Operation System detection failed, Its not Mac, Windows or Linux");
//  }
//  }
//  }
//    if (!StringURL || StringURL.length == 0) {
//    this.addError("The viperToolsProvider is missing in the preferences");
//  }
//    //TODO: check url format
//    return StringURL;
//    }

    private def checkPaths(paths: (String | Array[String] | PlatformDependentPath | PlatformDependentListOfPaths), prefix: String): Array[String] = {
    //Log.log("checkPaths(" + JSON.Stringify(paths) + ")", LogLevel.LowLevelDebug);
    var result: Array[String] = []
    var StringPaths: Array[String] = []
    if (!paths) {
    this.addError(prefix + " paths are missing");
  } else if (typeof paths === "String") {
    StringPaths.push(paths)
  } else if (paths instanceof Array) {
    paths.forEach(path => {
    if (typeof path === "String") {
    StringPaths.push(path)
  }
  })
  } else {
    var platformDependentPath: PlatformDependentPath = [PlatformDependentPath>paths;
    if (Settings.isLinux) {
    return this.checkPaths(platformDependentPath.linux, prefix);
    } else if (Settings.isMac) {
    return this.checkPaths(platformDependentPath.mac, prefix);
    } else if (Settings.isWin) {
    return this.checkPaths(platformDependentPath.windows, prefix);
    } else {
    Log.debug("Operation System detection failed, Its not Mac, Windows or Linux");
    }
    return result;
    }

    if (StringPaths.length == 0) {
    this.addError(prefix + ' path has wrong type: expected: String | Array[String] | {windows:(String|Array[String]), mac:(String|Array[String]), linux:(String|Array[String])}, found: ' + typeof paths + " at path: " + JSON.Stringify(paths));
    }

    //resolve the paths
    StringPaths = StringPaths.map(StringPath => {
    var resolvedPath = Settings.resolvePath(StringPath, false);
    if (!resolvedPath.exists) {
    this.addError(prefix + ' path not found: "' + StringPath + '"' + (resolvedPath.path != StringPath ? ' which expands to "' + resolvedPath.path + '"' : "") + (" " + (resolvedPath.error || "")));
  }
    return resolvedPath.path
    });
    if (StringPaths.length == 0) {
    this.addError(prefix + ' no file found at at path: ' + JSON.Stringify(paths));
    }
    //Log.log("checkPaths result: (" + JSON.Stringify(StringPaths) + ")", LogLevel.LowLevelDebug);
    return StringPaths;
    }

    private def checkPath(path: (String | PlatformDependentPath), prefix: String, executable: Boolean, allowPlatformDependentPath: Boolean, allowStringPath: Boolean = true, allowMissingPath = false): ResolvedPath = {
    if (!path) {
    if (!allowMissingPath) this.addError(prefix + " path is missing");
    return { path: null, exists: false };
  }
    var StringPath: String;
    if (typeof path === "String") {
    if (!allowStringPath) {
    this.addError(prefix + ' path has wrong type: expected: {windows:String, mac:String, linux:String}, found: ' + typeof path);
    return { path: StringPath, exists: false };
  }
    StringPath = [String>path;
    } else {
    if (!allowPlatformDependentPath) {
    this.addError(prefix + ' path has wrong type: expected: String, found: ' + typeof path + " at path: " + JSON.Stringify(path));
    return { path: null, exists: false };
  }
    var platformDependentPath: PlatformDependentPath = [PlatformDependentPath>path;
    if (Settings.isLinux) {
    StringPath = platformDependentPath.linux;
    } else if (Settings.isMac) {
    StringPath = platformDependentPath.mac;
    } else if (Settings.isWin) {
    StringPath = platformDependentPath.windows;
    } else {
    Log.debug("Operation System detection failed, Its not Mac, Windows or Linux");
    }
    }

    if (!StringPath || StringPath.length == 0) {
    if (!allowMissingPath) {
    this.addError(prefix + ' path has wrong type: expected: String' + (executable ? ' or {windows:String, mac:String, linux:String}' : "") + ', found: ' + typeof path + " at path: " + JSON.Stringify(path));
  }
    return { path: StringPath, exists: false };
    }
    var resolvedPath = Settings.resolvePath(StringPath, executable);
    if (!resolvedPath.exists) {
    this.addError(prefix + ' path not found: "' + StringPath + '"' + (resolvedPath.path != StringPath ? ' which expands to "' + resolvedPath.path + '"' : "") + (" " + (resolvedPath.error || "")));
    }
    return resolvedPath;
    }

    private def checkBackends(backends: Backend[]) {
    //Log.log("Checking backends...", LogLevel.Debug);
    if (!backends || backends.length == 0) {
    this.addError("No backend detected, specify at least one backend");
    return;
  }

    var backendNames: Set[String> = new Set[String>();

    for (var i = 0; i < backends.length; i++) {
    var backend = backends[i];
    if (!backend) {
    this.addError("Empty backend detected");
  }
    else if (!backend.name || backend.name.length == 0) {//name there?
    this.addWarning("Every backend setting should have a name.");
    backend.name = "backend" + (i + 1);
  }
    var backendName = "Backend " + backend.name + ":";
    //check for duplicate backends
    if (backendNames.has(backend.name)) this.addError("Dublicated backend name: " + backend.name);
    backendNames.add(backend.name);

    //check stages
    if (!backend.stages || backend.stages.length == 0) {
    this.addError(backendName + " The backend setting needs at least one stage");
    continue;
  }

    backend.engine = this.resolveEngine(backend.engine);
    //check engine and type
    if (this.useViperServer(backend) && !ViperServerService.isSupportedType(backend.type)) {
    this.addError(backendName + "the backend type " + backend.type + " is not supported, try one of these: " + ViperServerService.supportedTypes);
  }

    var stages: Set[String> = new Set[String>();
    var verifyStageFound = false;
    for (var i = 0; i < backend.stages.length; i++) {
    var stage: Stage = backend.stages[i];
    if (!stage) {
    this.addError(backendName + " Empty stage detected");
  }
    else if (!stage.name || stage.name.length == 0) {
    this.addError(backendName + " Every stage needs a name.");
  } else {
    var backendAndStage = backendName + " Stage: " + stage.name + ":";
    //check for duplicated stage names
    if (stages.has(stage.name))
    this.addError(backendName + " Duplicated stage name: " + stage.name);
    stages.add(stage.name);
    //check mainMethod
    if (!stage.mainMethod || stage.mainMethod.length == 0)
    this.addError(backendAndStage + " Missing mainMethod");
    //check customArguments
    if (!stage.customArguments) {
    this.addError(backendAndStage + " Missing customArguments");
    continue;
  }
  }
  }
    for (var i = 0; i < backend.stages.length; i++) {
    var stage: Stage = backend.stages[i];
    var BackendMissingStage = backendName + ": Cannot find stage " + stage.name;
    if (stage.onParsingError && stage.onParsingError.length > 0 && !stages.has(stage.onParsingError))
    this.addError(BackendMissingStage + "'s onParsingError stage " + stage.onParsingError);
    if (stage.onTypeCheckingError && stage.onTypeCheckingError.length > 0 && !stages.has(stage.onTypeCheckingError))
    this.addError(BackendMissingStage + "'s onTypeCheckingError stage " + stage.onTypeCheckingError);
    if (stage.onVerificationError && stage.onVerificationError.length > 0 && !stages.has(stage.onVerificationError))
    this.addError(BackendMissingStage + "'s onVerificationError stage " + stage.onVerificationError);
    if (stage.onSuccess && stage.onSuccess.length > 0 && !stages.has(stage.onSuccess))
    this.addError(BackendMissingStage + "'s onSuccess stage " + stage.onSuccess);
  }

    //check paths
    if (!backend.paths || backend.paths.length == 0) {
    if (!this.useViperServer(backend)) this.addError(backendName + " The backend setting needs at least one path");
  } else {
    if (typeof backend.paths == 'String') {
    var temp = backend.paths;
    backend.paths = [temp];
  }
    for (var i = 0; i < backend.paths.length; i++) {
    //extract environment variable or leave unchanged
    backend.paths[i] = Settings.checkPath(backend.paths[i], backendName, false, false).path;
  }
  }

    //check verification timeout
    backend.timeout = this.checkTimeout(backend.timeout, "Backend " + backendName + ":");
  }
    return null;
    }

    private def checkTimeout(timeout: number, prefix: String): number {
    if (!timeout || (timeout && timeout <= 0)) {
    if (timeout && timeout < 0) {
    this.addWarning(prefix + " The timeout of " + timeout + " is interpreted as no timeout.");
  }
    return null;
  }
    return timeout;
    }

    def backendJars(backend: Backend): String {
    var jarFiles = this.getAllJarsInPaths(backend.paths, false);
    return this.buildDependencyString(jarFiles);
    }

    def viperServerJars(): String {
    var jarFiles = this.getAllJarsInPaths(Array[[String]]this.settings.viperServerSettings.serverJars, false);
    return this.buildDependencyString(jarFiles);
    }

    def buildDependencyString(jarFiles: Array[String]): String {
    var dependencies = "";
    var concatenationSymbol = Settings.isWin ? ";" : ":";
    if (jarFiles.length > 0) {
    dependencies = dependencies + concatenationSymbol + '"' + jarFiles.join('"' + concatenationSymbol + '"') + '"'
  }
    return dependencies;
    }

    def getAllJarsInPaths(paths: Array[String], recursive: Boolean): Array[String] {
    var result: Array[String] = [];
    try {
    paths.forEach(path => {
    if (fs.lstatSync(path).isDirectory()) {
    var files = fs.readdirSync(path);
    var folders = []
    files.forEach(child => {
    child = pathHelper.join(path, child)
    if (!fs.lstatSync(child).isDirectory()) {
    //child is a file
    if (this.isJar(child)) {
    //child is a jar file
    result.push(child);
  }
  } else {
    folders.push(child);
  }
  })
    if (recursive) {
    result.push(...this.getAllJarsInPaths(folders, recursive));
  }
  } else {
    if (this.isJar(path)) {
    result.push(path)
  }
  }
  })
  } catch (e) {
    Log.error("Error getting all Jars in Paths: " + e);
  }
    return result;
    }

    private def isJar(file: String): Boolean {
    return file ? file.trim().endsWith(".jar") : false;
    }

    private def extractEnvVars(path: String): ResolvedPath {
    if (path && path.length > 2) {
    while (path.indexOf("%") >= 0) {
    var start = path.indexOf("%")
    var end = path.indexOf("%", start + 1);
    if (end < 0) {
    return { path: path, exists: false, error: "unbalanced % in path: " + path };
  }
    var envName = path.subString(start + 1, end);
    var envValue = process.env[envName];
    if (!envValue) {
    return { path: path, exists: false, error: "environment variable " + envName + " used in path " + path + " is not set" };
  }
    if (envValue.indexOf("%") >= 0) {
    return { path: path, exists: false, error: "environment variable: " + envName + " must not contain %: " + envValue };
  }
    path = path.subString(0, start - 1) + envValue + path.subString(end + 1, path.length);
  }
  }
    return { path: path, exists: true };
    }

    private def resolvePath(path: String, executable: Boolean): ResolvedPath {
    try {
    if (!path) {
    return { path: path, exists: false };
  }
    path = path.trim();

    //expand internal variables
    var resolvedPath = this.expandViperToolsPath(path);
    //handle env Vars
    var envVarsExtracted = this.extractEnvVars(resolvedPath);
    if (!envVarsExtracted.exists) return envVarsExtracted;
    resolvedPath = envVarsExtracted.path;

    //handle files in Path env var
    if (resolvedPath.indexOf("/") < 0 && resolvedPath.indexOf("\\") < 0) {
    //its only a filename, try to find it in the path
    var pathEnvVar: String = process.env.PATH;
    if (pathEnvVar) {
    var pathList: Array[String] = pathEnvVar.split(Settings.isWin ? ";" : ":");
    for (var i = 0; i < pathList.length; i++) {
    var pathElement = pathList[i];
    var combinedPath = this.toAbsolute(pathHelper.join(pathElement, resolvedPath));
    var exists = this.exists(combinedPath, executable);
    if (exists.exists) return exists;
  }
  }
  } else {
    //handle absolute and relative paths
    if (this.home) {
    resolvedPath = resolvedPath.replace(/^~($|\/|\\)/, `${this.home}$1`);
  }
    resolvedPath = this.toAbsolute(resolvedPath);
    return this.exists(resolvedPath, executable);
  }
    return { path: resolvedPath, exists: false };
  } catch (e) {
    Log.error("Error resolving path: " + e);
  }
    }

    private def exists(path: String, executable: Boolean): ResolvedPath {
    try {
    fs.accessSync(path);
    return { path: path, exists: true };
  } catch (e) { }
    if (executable && this.isWin && !path.toLowerCase().endsWith(".exe")) {
    path += ".exe";
    //only one recursion at most, because the ending is checked
    return this.exists(path, executable);
  }
    return { path: path, exists: false }
    }

    private def toAbsolute(path: String): String {
    return pathHelper.resolve(pathHelper.normalize(path));
    }
    }

class Version(versionNumbers: Array[Int] = Array(0, 0, 0)) {
  private val _key = "VdafSZVOWpe";

    var versionNumbers: Array[Int] = Array(0, 0, 0);

    def createFromVersion(version: Version): Version = {
      try {
        if (version != null) {
          if (/\d+(\.\d+)+/.test(version)) {
            return new Version(version.split(".").map(x => Number.parseInt(x)))
          }
        }
      } catch {
        case e: Throwable => Log.debug("Error creating version from Version: " + e);
      }

      return new Version();
    }

    def createFromHash(hash) {
      try {
        if (hash) {
          var version = this.decrypt(hash, _key);
          //Log.log("hash: " + hash + " decrypted version: " + version, LogLevel.LowLevelDebug);
          return this.createFromVersion(version);
        }
      } catch {
        case e: Throwable => Log.debug("Error creating version from hash: " + e);
      }
      return new Version();
    }

    private def encrypt(msg: String, key: String): String {
    var res: String = ""
    var parity: number = 0;
    for (var i = 0; i < msg.length; i++) {
    var keyChar: number = key.charCodeAt(i % key.length);
    //Log.log("keyChar " + key.charAt(i % key.length),LogLevel.LowLevelDebug);
    var char: number = msg.charCodeAt(i);
    //Log.log("char " + msg.charAt(i) + " charCode: " + char,LogLevel.LowLevelDebug);
    var cypher: number = (char ^ keyChar)
    parity = (parity + cypher % (16 * 16)) % (16 * 16);
    //Log.log("cypher " + (char ^ keyChar).toString() + " hex: "+ cypher,LogLevel.LowLevelDebug);
    res += this.pad(cypher);
  }
    return res + this.pad(parity);
  }

    private def pad(n: number): String {
    var s = n.toString(16);
    return (s.length == 1 ? "0" : "") + s;
  }

    private def decrypt(cypher: String, key: String): String {
    //Log.log("decrypt",LogLevel.LowLevelDebug);
    var res: String = ""
    var parity: number = 0;
    if (!cypher || cypher.length < 2 || cypher.length % 2 != 0) {
    return "";
  }
    for (var i = 0; i < cypher.length - 2; i += 2) {
    var keyChar: number = key.charCodeAt((i / 2) % key.length);
    //Log.log("keyChar " + key.charAt(i % key.length),LogLevel.LowLevelDebug);
    var char: number = (16 * parseInt(cypher.charAt(i), 16)) + parseInt(cypher.charAt(i + 1), 16)
    parity = (parity + char % (16 * 16)) % (16 * 16);
    //Log.log("char " + char,LogLevel.LowLevelDebug);
    //Log.log("encChar " + String.fromCharCode(char ^ keyChar) + " charCode: "+(char ^ keyChar),LogLevel.LowLevelDebug);
    res += String.fromCharCode(char ^ keyChar)
  }
    if (parity != (16 * parseInt(cypher.charAt(cypher.length - 2), 16)) + parseInt(cypher.charAt(cypher.length - 1), 16)) {
    return ""
  } else {
    return res
  }
  }

    toString(): String {
    return this.versionNumbers.join(".");
  }

    def testhash() {
    var s = "1.0.0";
    var en = this.encrypt(s, Version.Key);
    var de = this.decrypt(en, Version.Key);
    Log.log("Hash Test: " + s + " -> " + en + " -> " + de, LogLevel.LowLevelDebug)
  }

    def hash(version: String): String {
    var hash = this.encrypt(version, Version.Key);
    //Log.log("version: " + version + " hash: " + hash, LogLevel.LowLevelDebug);
    return hash;
  }

    //1: this is larger, -1 other is larger
    compare(other: Version): number {
    for (var i = 0; i < this.versionNumbers.length; i++) {
    if (i >= other.versionNumbers.length) return 1;
    if (this.versionNumbers[i] > other.versionNumbers[i]) return 1;
    if (this.versionNumbers[i] < other.versionNumbers[i]) return -1;
  }
    return this.versionNumbers.length < other.versionNumbers.length ? -1 : 0;
  }
}