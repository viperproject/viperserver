# README #

Viper Server

### What is this repository for? ###

* Quick summary
* Version
* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### How do I get set up? ###

* Summary of set up
* Configuration
* Dependencies
* Database configuration
* How to run tests
* Deployment instructions

### Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin
* Other community or team contact

### Installation Instructions:

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