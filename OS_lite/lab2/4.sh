#!/bin/bash

touch temp.txt


for pid in $(ps -e -o pid --no-header) 
do
ppid_file=/proc/$pid/status
art_file=/proc/$pid/sched

if [[ -f "$ppid_file" ]] 
	then
	ppid=$(grep PPid $ppid_file | grep -o -E '[0-9]+')
	n1=$(grep se.sum_exec_runtime $art_file | awk '{print $3}')
	n2=$(grep nr_switches $art_file | awk '{print $3}')
	art=$(echo $n1 $n2 | awk '{print $1/$2}')
	
	if [[ -f "$ppid_file" ]]
	then 
		echo 'ProcessID=' $pid ':' 'Parent_ProcessID=' $ppid ':' 'Average_Running_Time=' $art >> temp.txt
	fi
fi
done

touch 4.txt

sort -nk 5 temp.txt > 4.txt 

rm temp.txt


