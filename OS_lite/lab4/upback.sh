#!/bin/bash


cd ~

if ! [[ -d restore ]]
then 
	mkdir restore
fi

cd ~/$(ls | grep '^Backup-' | sort -n | tail -n -1)

for cur in $(ls)
do 
if [ -z "`echo $cur | grep -E "*.[0-9]{4}-[0-9]{2}-[0-9]{2}$"`" ]
then
	cp -R $cur ~/restore
fi
done		
