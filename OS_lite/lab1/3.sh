#!/bin/bash

echo "1 - nano"
echo "2 - vi"
echo "3 - chrome"
echo "4 - exit"

read command
if [[ $command == 1 ]]; then
	nano 
fi
if [[ $command == 2 ]]; then
	vi
fi
if [[ $command == 3 ]]; then
	google-chrome
fi
if [[ $command == 4 ]]; then
	exit
fi

