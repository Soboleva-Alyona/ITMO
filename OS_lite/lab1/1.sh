#!/bin/bash

if [[ $(bc<<<"$1 >= $2") && $(bc<<<"$1 >= $3") ]] 
then 
echo $1
else 
if [[ $(bc<<<"$2 >= $1") && $(bc<<<"$2 >= $3") ]]
then 
echo $2
else 
echo $3
fi
fi
