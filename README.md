Binôme 1: Matthias FORNARO
Binôme 2: Samuel CLEREMBAUX

Travail réalisé:

-un module tools a été implémenté avec les fonctions requises ainsi qu'avec d'autres fonctions utiles pour notre implémentation de l'algorithme de Ford-Ferguson
-la fonction export a été implémentée dans gfile
-l'agorithme de Ford-Ferguson a été implémenté dans le module algo
-le fichier Makefile a été modifié pour permettre l'affichage du fichier outImage.svg lorsque on lance make demo
-le projet medium a été implémenté, nous avans choisi le problème de cricket élimination, le projet consiste en:
    -le module cricket contenant les fonctions permettant de résoudre le problème
    -le module cfile permettant, à l'image de gfile, de traduir le contenu d'un fichier en données utiles à la résolution du problème
    -le fichier cricket.txt, contenant les données visées à être traduites par les fonctions de cfile
-le module ftest a été modifié afin de pouvoir tester le projet minimal et medium




Base project for Ocaml project on Ford-Fulkerson. This project contains some simple configuration files to facilitate editing Ocaml in VSCode.

To use, you should install the *OCaml Platform* extension in VSCode.
Then open VSCode in the root directory of this repository (command line: `code path/to/ocaml-maxflow-project`).

Features :
 - full compilation as VSCode build task (Ctrl+Shift+b)
 - highlights of compilation errors as you type
 - code completion
 - view of variable types


A [`Makefile`](Makefile) provides some useful commands:

 - `make build` to compile. This creates an `ftest.exe` executable
 - `make demo` to run the `ftest` program with some arguments
 - `make format` to indent the entire project
 - `make edit` to open the project in VSCode
 - `make clean` to remove build artifacts

In case of trouble with the VSCode extension (e.g. the project does not build, there are strange mistakes), a common workaround is to (1) close vscode, (2) `make clean`, (3) `make build` and (4) reopen vscode (`make edit`).

