#!/bin/bash

ps -A -o pid,command | awk '{if ($2 ~ "^/sbin/") print $1}' > 2.txt 
