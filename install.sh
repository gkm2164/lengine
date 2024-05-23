#!/bin/sh

sbt assembly
cp ./lengine-repl/target/scala-2.13/lengine.jar .
cp ./lengine-compiler/target/scala-2.13/lenginec.jar .
cp ./lengine-runtime/target/scala-2.13/lengine-runtime.jar .

# Building some standard library
./lenginec lengine-code/stdlib.lg
