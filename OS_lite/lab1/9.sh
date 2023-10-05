#!/bin/bash

sudo find /var/log -name "*.log" | sudo xargs wc -l
