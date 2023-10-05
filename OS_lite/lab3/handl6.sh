#!/bin/bash

echo $$ > .pid
A=1
MODE="no operation"
usr1()
{
	MODE="+"
}
usr2()
{
	MODE="*"
}

trap 'usr1' USR1
trap 'usr2' USR2

while true; do
	case $MODE in
	"+")
		let A=$A+2
		echo $A
		;;
	"*")
		let A=$A\*2
		echo $A
		;;
	"no operation")
		echo "Waiting for input operation"
		;;
	esac
	sleep 1
done
