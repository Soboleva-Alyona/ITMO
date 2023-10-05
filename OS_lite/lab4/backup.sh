#!/bin/bash

dateNow=$(date +%F)

datePrev=$(find ~/Backup-* -maxdepth 0 2> /dev/null \
| sort -n | tail -n 1 | awk -F '/' '{print $4}' |\
	awk -F '-' '{print $2 "-" $3 "-" $4}')
	
if [[ -z $datePrev || $datePrev == "--" ]]; 
then
	day_diff=8
else
	sec_diff=$(($(date -d $dateNow +%s) - $(date -d $datePrev +%s)))
	day_diff=$(($sec_diff/60/60/24))
fi

if [[ $day_diff -ge 7 ]] 
then
	cd ~
	
	mkdir "Backup-"$dateNow
	cd ~/source 
	ArrayListFilesInSource=$(ls)
	for currentFile in $ArrayListFilesInSource
	do
		cp -R $currentFile ~/Backup-$dateNow		
	done
	cd ~
	echo "Directory Backup-$dateNow was created."
	echo "Files: $ArrayListFilesInSource" >> backup-report	
else
	cd ~/source 
	ArrayListFilesInSource=$(ls)
	cd ~
	echo "Directory Backup-$datePrev was created."
	echo "Files: $ArrayListFilesInSource" >> backup-report
	
	cd ~/"Backup-"$datePrev
	for currentFile in $ArrayListFilesInSource
	do
		if [[ -f $currentFile ]]
		then
			sizeSourceFile=$(ls -l ~/source/$currentFile | awk '{print$5}')
			sizeBackupFile=$(ls -l $currentFile | awk '{print$5}')
			if [[ $sizeSourceFile -ne $sizeBackupFile ]]
			then 
				mv $currentFile $currentFile-$dateNow
				cp -R ~/source/$currentFile ~/Backup-$datePrev
				echo "rename $currentFile to $currentFile-$dateNow" >> ~/backup-report
			fi
		else
			cp -R ~/source/$i ~/Backup-$dateFiles
		fi		
	done
fi
