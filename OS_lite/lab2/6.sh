#!/bin/bash

answ_process=""
max=0

for pid in $(ps -e -o pid --no-header) 
do
file=/proc/$pid/status
if [[ -f "$file" ]] 
	then
	cur=$(grep "^VmRSS" $file | grep -o -E '[0-9]+')
	if [[ $cur -gt $max ]] 
	then 
		max=$cur
		answ_process=$pid
	fi
fi
done

echo $answ_process $max

#top -o RES
