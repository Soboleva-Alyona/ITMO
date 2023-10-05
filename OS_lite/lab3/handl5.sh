#!/bin/bash

res=1
cur_op="+"

(tail -f pipe) |
while true
do
	read LINE;
	if [[ $LINE == "+" || $LINE == "*" ]] 
	then  
		cur_op="$LINE"
	fi
	
	if [[ "$LINE" =~ -?[0-9]+ ]] 
	then
		case $cur_op in
			"+")
				res=$(echo $res $LINE | awk '{print $1 + $2}')
				
				;;
			"*")
				res=$(echo $res $LINE | awk '{print $1 + $2}')
				;;
		esac 
	fi
	
	if [[ "$LINE" == "QUIT" ]]
	then
 			echo $res
			echo "Exit.."
			killall tail
			exit 0
	fi
	if [[ "$LINE" != "+" && "$LINE" != "*" && ! "$LINE" =~ -?[0-9]+ ]] 
	then 
		echo "Incorrect input"
		exit 1
	fi
done
