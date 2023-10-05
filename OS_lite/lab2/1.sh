#!/bin/bash

ps -fU dead -o pid,command > res.txt
wc -l res.txt  > 1.txt
awk '{printf $1; printf ": "; printf $2; printf "\n" }' res.txt >> 1.txt

rm  res.txt
