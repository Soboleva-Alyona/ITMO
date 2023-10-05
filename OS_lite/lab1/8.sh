#!/bin/bash

sort -t : -k3 -n /etc/passwd | awk -F ":" '{print $1}' 

