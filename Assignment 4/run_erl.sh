#!/bin/bash

echo "vær rar at vælge en .erl fil"
cd code/part2/tests && ls -a
read erfil

echo "oversætter og køre dialyzer på ${erlfil}"
erlc +debug_info $erfil

beam=$(echo $erfil | cut -d. -f1)
beam+='.beam'
echo $beam

dialyzer -Wunderspecs $beam
