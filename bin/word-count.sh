#!/bin/bash

regex='.*\('

for arg in "$@"
do
    regex+="$arg"'\|'
done
regex="${regex%'\|'}"
regex+='\)'

find . -type f -regex "$regex" -print0 | wc -l --files0-from=-
