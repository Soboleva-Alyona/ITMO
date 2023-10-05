#!/bin/bash

SECONDS=0

while [[ $SECONDS -lt 60 ]] 
do
	for pid in $(ps --deselect --pid $$ -o pid --no-header) 
	do
	file=/proc/$pid/io
		if [[ -z "${start[$pid]}" ]] 
		then
			if [[ -r /proc/$pid/io ]]
			then
			    start[$pid]="$(grep "rchar" $file | awk '{ print $2 }')"
			    delta[$pid]="0"
			    command[$pid]="$(cat /proc/$pid/cmdline | tr -d '\0')"
			fi
		else
		    let delta[$pid]="$(grep "rchar" $file | awk '{ print $2 }')"-${start[$pid]}
		fi
		
		
	done	
	echo -n -e "\r$SECONDS\033[36m"
done

echo "$(
	for pid in "${!delta[@]}"
	do
    		echo "$pid ${delta[$pid]} ${command[$pid]}"
	done)" | sort -k2 -nr | head -n3 | awk '{ print $1":"$2":"$3 }'



















