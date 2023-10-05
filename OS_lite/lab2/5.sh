#!/bin/bash


rm 5.txt
touch 5.txt

prev_ppid=$(head -n 1 4.txt | awk '{print $5}')
c=0
sum_art=0
M=0

while read string
do
	cur_ppid=$(echo $string | awk '{print $5}')
	art=$(echo $string | awk '{print $8}')
	sum_art=$(echo $sum_art $art | awk '{print $1+$2}')
	

	if [[ $cur_ppid != $prev_ppid ]]
	then 		
		M=$(echo $sum_art $c | awk '{print $1/$2}')
		echo 'Average_Running_Children_of_ParentID=' $prev_ppid ' is ' $M >> 5.txt
		sum_art=$art
		c=0
		prev_ppid=$cur_ppid
	fi
	
	c=$(echo $count | awk '{print $1+1}')
done < 4.txt





