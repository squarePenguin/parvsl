#!/bin/env bash

declare -a chapters=("introduction" "preparation" "implementation" "evaluation" "conclusion")
total=0

for chapter in "${chapters[@]}"; do
    path="./chapters/${chapter}.tex"
    wordcount=$(detex ${path} | tr -cd "0-9A-Za-z \n" | wc -w)
    echo "${chapter}: ${wordcount}"
    total=$((total + wordcount))
done

echo "total: ${total}"