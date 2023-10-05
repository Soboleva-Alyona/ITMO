#!/bin/bash

touch info.log
awk '$2 == "INFO"' /var/log/anaconda/syslog > info.log

