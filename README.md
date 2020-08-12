![Devnull](https://i.imgur.com/vbtoZX7.png)


# DEVNULL Org

Please take a look at our [wiki](https://github.com/v0d1ch/devnull/wiki/DEVNULL-Org) page for details.

 - writing production level code
 - code optimization
 - debugging and fixing problems with execution speed and/or large memory consumption

 Additionally we work on tools that should solve some of the pain points various companies experience in their daily operations.


### Products


### Fakie - The ultimate API glue!
------------

#### Merge multiple API endpoints into results you control

Fakie is the tool that is capable of calling multiple api endpoints and merging
the received data into results you control.

### Getting started

## Running Fakie

Fakie tool comes in a form of binary file that you should run.
There are couple of command line options so let's go through them now:

If you just call the binary without any arguments, you should see something like this:

```
./fakie
Missing: (-p|--port ARG)

Usage: fakie [-c|--configuration-file ARG] [-o|--output-file ARG]
             (-p|--port ARG)
  Fakie - merge multiple API endpoints into results you control.

```

When the app is started it spawns the web server on the port you need to provide
using `-p` or `--port` flag.

```
./fakie -p 5588

```

Fakie can't do anything meaninful if there is no configuration file that is used
to specify all sorts of options you can use to call external api services and
further control the results you get. By default Fakie looks at current directory for a file
named `.fakie.json` and if that file does not exist you get the following output:

```
/fakie -p 5588
[Info] No configuration file specified. Defaulting to .fakie.json
[Info] Fakie Api
reading configuration...
[Error] Configuration error!
Fakie config could not be obtained.
.fakie.json: openBinaryFile: does not exist (No such file or directory)
fakie: FakieException "Configuration file failure. Please double check the file path."
```



When you run the tool it spawns the configurable server that awaits requests and

Well, it all starts with a configuration file. All you need to know in order to control the fakie api server is some JSON.
Configuration file determines what api endpoints you want to call and how to you want to map the results you get to the results
you actually want.

When you call some external json api endpoint the result is obviously or hopefully json. You might want to specify your own
field names instead of ones you get externally because it is convenient to have this kind of control.
You can also choose which field you would like to keep and which ones to omit. Also you could convert types of fields i.e. Array to Object or
vice versa as well as group the results into your own endpoint that serves the data in the way it is convenient to you.

So if you need a flexible tool for working with and merging multiple api calls into a single or multiple responses Fakie is the thing to reach for.
