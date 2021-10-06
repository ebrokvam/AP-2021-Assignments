#!/bin/bash

echo "fjerner .DS_Store filerne of *.beam filer"
find . -name ".DS_Store" -print -delete
#find . -name "*.beam" -print -delete
#find . -name "stack.yaml.lock" -print -delete
#find . -name "boa.cabal" -print -delete

#echo "fjerner alle .stack-work/nmapperne"
#find .stack-work -mindepth 1 -print -depth -exec rm -rf {} ';'

echo "omdøber den gamle code.zip of flytter den i mappen gammel_kode/"
mv code.zip code_$(date +%d-%m-%Y).zip 
if [ ! -d gammel_kode ]; then
  mkdir gammel_kode
fi
mv code_$(date +%d-%m-%Y).zip gammel_kode/

echo "zipper den nye"
zip -r code.zip code/

echo "curl'er den til find.incorrectness.dk - ass4"
curl -F handin=@code.zip https://find.incorrectness.dk/grade/ass4
