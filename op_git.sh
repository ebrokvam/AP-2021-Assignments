#!/bin/bash

#cd "$(2021/AP-2021-Assignment-1/ "$0")"

echo "hiver git'eren ned"
git pull

echo "what do you wanna add? choose . if you don't care no more"
read input
git add $input


echo "now for the exciting part! what'll you comment be? rmmbr it has to be serious"
read comment
git commit -m $comment 

echo "do you wanna commit now? Enter either JA or NEJ"

read choice

if $choice == JA then 
  git push
fi

if $choice == NEJ then 
  echo "you choose wisely"
fi



