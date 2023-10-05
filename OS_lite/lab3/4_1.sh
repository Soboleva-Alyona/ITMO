#!/bin/bash

pid1=$$
echo $pid1 > pid1

let x=2
while true  
do
	let x=$x+2
done
