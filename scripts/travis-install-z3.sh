#!/bin/sh

case "$(uname)" in
  Darwin)
    Z3_FILENAME=z3-4.8.4.d6df51951f4c-x64-osx-10.14.1
    ;;
  Linux)
    # Assume Debian
    Z3_FILENAME=z3-4.8.4.d6df51951f4c-x64-debian-8.11
    ;;
esac
curl -o z3.zip -L https://github.com/Z3Prover/z3/releases/download/z3-4.8.4/${Z3_FILENAME}.zip
unzip z3.zip
rm z3.zip
mv ${Z3_FILENAME} z3

