#!/usr/bin/env bash

if [ $# -eq 0 ]; then
    echo "Calling docker image clas6"
    IM_NAME="clas6"
else
    IM_NAME=$1
fi


docker build --rm -t $IM_NAME .
