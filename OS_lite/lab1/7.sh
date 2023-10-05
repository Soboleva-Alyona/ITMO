#!/bin/bash

touch emails.lst
grep -r -h -s -o '[[:alnum:]][[:alnum:]+\.\_\-]*@[[:alnum:]+\_\-]*\.[a-z]*' /etc | awk '{printf $1; printf ", "}' > emails.lst 
