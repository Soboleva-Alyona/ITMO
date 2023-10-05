#!/bin/bash
#> report2.log
let cnt=0
declare -a M


echo $$ > pid

while [[ true ]] 
do
	for i in 1 2 3 4 5 6 7 8 9 10
	do
		M+=($i)
	done
	let cnt=$cnt+1
	#echo $cnt
	if [[ cnt -eq 100000 ]]
	then
		echo ${#M[@]} >> report.log
		let cnt=0
	fi

done
