# Compilers NTUA 2019-20 8th semester
A complete compiler for the [Tony](https://courses.softlab.ntua.gr/compilers/2020a/tony2020.pdf) language for the course of Compilers in ECE, NTUA.

## Authors
[Ιάσων Λαζαρίδης](https://github.com/jasonlazar)
[Θανάσης Κουτρούμπας](https://github.com/thanoskoutr)


## Technologies Used
- flex &#8594; For Lexical Analysis
- bison &#8594; For Syntax/Semantic Analysis
- [LLVM](https://llvm.org/) &#8594; For IR Code/Code Generation/Optimizations
- [edsger-lib](https://github.com/abenetopoulos/edsger_lib) &#8594; A x86 assembly runtime libraby for Linux
- [bdwgc](https://www.hboehm.info/gc/) &#8594; Boehm's Garbage Collector


## Dependecies
- flex (v2.6.4)
- bison (v3.0.4)
- llmv-9
- libgc-dev

### LLVM-9
- For a Ubuntu/Debian system download the Automatic Installation Script and install with version 9:
```
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh 9
```
- For other systems build version 9 from source from here https://releases.llvm.org/download.html

### libgc-dev
- For a Ubuntu/Debian system install header files for Boehm's Garbage Collector from apt package:
```
sudo apt install libgc-dev
```
- For other systems find here https://www.hboehm.info/gc/ or find appropriate packages for your distribution.

## Building
To build everything do:
```
make
```

## Usage
To run the compiler:
```
./tony [flags] [FILE]
```
The supported flags are:
- -O: Enable optimizations
- -i: The input must be given on stdin and the intermediate code will be printed on stdout
- -f: The input must be given on stdin and the final code will be printed on stdout

If no -i or -f flags are given the input file must be given as a command line parameter.
