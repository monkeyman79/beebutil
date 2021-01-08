#! /bin/sh

# Remove comments to fit the program into what little memory we have
set -e

[ $# -eq 2 ] || (echo "Invalid parameters"; exit 1)

sed 's/:: REM .*$//
s/ REM .*$//
s/\\.*$//' < $1 > $2
