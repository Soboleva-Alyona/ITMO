#!/bin/bash

for pid in $(ps --deselect --pid $$ --ppid $$ -o pid --no-header) 
do
	cur=$(sudo pwdx $pid)
	echo $cur | awk '{if ($2 ~ "^/sbin/") print $1}'
done
