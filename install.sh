#!/bin/sh

sbt assembly
cp ./lengine-repl/target/scala-2.12/lengine.jar .
cp ./lengine-compiler/target/scala-2.12/lenginec.jar .