#!/bin/bash

if [ -z "$1" ]; then
    echo "Please provide the name of the file to convert" >2
    exit 1
fi

hp2pretty $1
