#!/bin/bash

cur_dir=$PWD
source=$1
source=$cur_dir/$source

if [[ -e $source ]] 
then

	trash_dir=".trash"
	cd ~
	if [[ -d "$trash_dir" ]]
	then
		echo "trash_dir exists!"
	else
		mkdir $trash_dir
		echo "dir is made!"
	fi

	cd $trash_dir

	trash_log=".trash.log"

	num=$(ls | wc -w)
	num=$(($num + 1)) 

	ln $source $num && rm $source && 
	{
	cd ..
	echo -n "link: " >> $trash_log
	echo -n $num >> $trash_log 
	echo -n " file: " >> $trash_log
	echo -n $source >> $trash_log
	echo >> $trash_log
	}
else 
	echo "source file doesn't exist ^~^"
	exit 1
fi


