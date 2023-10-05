#!/bin/bash

while true
do
	read LINE
	echo $LINE > pipe
	if [[ $LINE == "QUIT" ]] 
	then 
		echo "Exit.."
		exit 0
	fi
	
	if [[ "$LINE" != "+" && "$LINE" != "*" && ! "$LINE" =~ -?[0-9]+ ]] 
	then 
		echo "Incorrect input"
		exit 1
	fi
	
done			
			
			
