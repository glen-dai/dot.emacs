#!/bin/bash

output=e.tar.gz

if [ "$#" = "1" ];then
    output=$1
fi

rm -rf ${output}
git archive --format tar.gz -o ${output} HEAD
echo pkg ${output} is ready
