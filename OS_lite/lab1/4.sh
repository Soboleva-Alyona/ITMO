#!/bin/bash

if [[ $PWD == $HOME ]]; then
	echo $HOME
	exit 0
else 
	echo "wrong dir" 
	exit 1
fi
