#!/bin/sh

sbt assembly
cp ./lengine-exec/target/scala-2.12/lengine.jar .