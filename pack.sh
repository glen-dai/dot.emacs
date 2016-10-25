#!/bin/bash

output=e.tar.gz

if [ "$#" = "1" ];then
    output=$1
fi

git archive --format tar.gz -o ${output} HEAD
