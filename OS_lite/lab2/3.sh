#!/bin/bash

ps --deselect --pid $$ --ppid $$ --no-header --sort=-start_time | head -n 1 | awk '{print $1}'
