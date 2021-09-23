#!/bin/bash

echo "fjerner .DS_Store, .stack.yaml.lock, boa.yamel filterne"
find . -name ".DS_Store" -print -delete
find . -name "stack.yaml.lock" -print -delete
find . -name "boa.cabal" -print -delete

echo "fjerner alle .stack-work/"
find .stack-work -mindepth 1 -print -depth -exec rm -rf {} ';'



echo "fjerner den gammel code.zip"
rm code.zip 

echo "zipper den nye"
zip -r code.zip code/

echo "curl'er den til find.incorrectness.dk - ass3 ... outcomment when ass3 is up and running"
# curl -F handin=@code.zip https://find.incorrectness.dk/grade/ass3