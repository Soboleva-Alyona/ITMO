#!/bin/bash

answ=''
while true
	do
	read str
	if [[ $str == q ]]
		then 
			break
	fi
	answ=$answ$str
done
echo $answ

