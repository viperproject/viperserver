![Scala CI](https://github.com/viperproject/viperserver/workflows/Scala%20CI/badge.svg?branch=master)

# README #

This is ViperServer, an HTTP server that manages verification requests to different tools from the Viper tool stack.

The main two tools currently are:

- [Carbon](https://bitbucket.org/viperproject/carbon), a verification condition generation (VCG) backend for the Viper language.
- [Silicon](https://bitbucket.org/viperproject/silicon), a symbolic execution verification backend.


### The Purpose of ViperServer ###

1. Viper IDE: integration of Viper into Visual Studio Code (VS Code). Viper IDE provides the best user experience for Viper.
   More details here: http://viper.ethz.ch/downloads/
2. Avoid 1-3 second delays caused by JVM startup time. ViperServer offers a robust alternative to, e.g.,
   [Nailgun](https://github.com/facebook/nailgun).
3. Develop Viper encodings more efficiently with caching.
4. Interact with Viper tools programmatically using the HTTP API. A reference client implementation (in Python) is
   available via [viper_client](https://bitbucket.org/viperproject/viper_client).

For more details, please visit: http://viper.ethz.ch/downloads/


### Installation Instructions ###

* Clone [silver](https://bitbucket.org/viperproject/silver/), [silicon](https://bitbucket.org/viperproject/silicon/) and [carbon](https://bitbucket.org/viperproject/carbon/) repositories in your computer, in separate directories.
* Clone **viperserver** (this repository) in your computer, in another directory.
* From within the directory where you installed viperserver, create a symbolic links to the directories where you installed silver, silicon and carbon.
* On Linux/Mac OS X:  
```
ln -s <relative path to diretory where you installed silver> silver  
ln -s <relative path to diretory where you installed silicon> silicon  
ln -s <relative path to diretory where you installed carbon> carbon  
```
* On Windows:  
```
mklink /D silver <relative path to diretory where you installed silver>  
mklink /D silicon <relative path to diretory where you installed silicon>  
mklink /D carbon <relative path to diretory where you installed carbon>  
```
* Compile by typing: ```sbt compile```

* Other supported SBT commands are: ```sbt stage``` (produces fine-grained jar files), ```sbt assembly``` (produces a single fat jar file).

### Running Tests ###

* Set the environment variable ```Z3_EXE``` to an executable of a recent version of [Z3](https://github.com/Z3Prover/z3).

* Run the following command: ```sbt test```.


### Who do I talk to? ###

* This repository is maintained by [Arshavir Ter-Gabrielyan](mailto:ter-gabrielyan@inf.ethz.ch).
