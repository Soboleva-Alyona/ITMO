#!/bin/bash

source=$1
if [[ -f $source ]] 
then 
	echo "file already exists"
	exit 0
fi
cd ~
trash_log=".trash.log"
cur_link=0
copy_num=0
inp=$(cat $trash_log | grep "$source$")
for cur in $inp
do
	
	if [[ $cur =~ ^[0-9]+ ]]
	then
		cur_link=$cur
		continue
	fi
	
	
		
	if [[ "$cur" =~ $source ]]
	then
		echo $cur
		read -p "Are you sure? " -n 1 -r
		
		echo   # (optional) move to a new line
		if [[ ! $REPLY =~ ^[Yy]$ ]]
		then
		    continue
		fi
		
		# иначе удаляем
		final_dir=$(echo ${cur//$source/})
		source_link=".trash/$cur_link"
		
		if [[ -f $source_link ]]
		then
			if [[ -d $final_dir ]] 
			then		
				echo $final_dir	
				while [[ -f $final_dir$source ]] #уже восстановили файл с таким именем, восстановим копию
				do
					copy_num=$(($copy_num + 1)) 
					source=$source"copy"
				done	
				
				if [[ $copy_num == 0 ]]
				then	
					ln $source_link $final_dir$source && rm $source_link
				else
					ln $source_link $final_dir$source && rm $source_link
				fi
				
			else
				echo "directory doesnt exist"
				ln $source_link $source && rm $source_link
			fi
		else 
			echo "link does not exist"
		fi
		
	fi
	
		
done 






