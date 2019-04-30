#!/bin/env bash

declare -a chapters=("introduction" "preparation" "implementation" "evaluation" "conclusion")
total=0

alias detex="detex -e array,eqnarray,equation,figure,mathmatica,picture,table,verbatim,code"

for chapter in "${chapters[@]}"; do
    path="./chapters/${chapter}.tex"
    wordcount=$(detex -l ${path} | tr -cd "0-9A-Za-z \n" | wc -w)
    echo "${chapter}: ${wordcount}"
    total=$((total + wordcount))
done

echo "total chapters (bad): ${total}"

chapter_correct=$(detex diss.tex | tr -cd "0-9A-Za-z \n" | sed -n '/Introduction/,$p' | sed -n '/Bibliography/q;p' | wc -w)
all=$(detex diss.tex | tr -cd "0-9A-Za-z \n" | wc -w)

# detex diss.tex | tr -cd "0-9A-Za-z \n" | sed -e/Introduction/\{ -e:1 -en\;b1 -e\} -ed | sed -n '/Bibliography/q;p' | less
# detex diss.tex | tr -cd "0-9A-Za-z \n" | sed -n '/Introduction/,$p' | sed -n '/Bibliography/q;p' | wc

echo "total chapters: ${chapter_correct}"
echo "total: ${all}"