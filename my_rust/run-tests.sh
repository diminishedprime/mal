#!/bin/bash

set -ex

cargo build --release

cd ..

make "test^my_rust^step0"
make "test^my_rust^step1"
make "test^my_rust^step2"
make "test^my_rust^step3"
make "test^my_rust^step4"
