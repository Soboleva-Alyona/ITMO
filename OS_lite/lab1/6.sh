#!/bin/bash


touch full.log
awk '{if ($3 == "(WW)") print}' /var/log/anaconda/X.log | awk '{if ($3 == "(WW)") {$3="Warning:"; print}}' >> full.log
awk '{if ($3 == "(II)") print}' /var/log/anaconda/X.log | awk '{if ($3 == "(II)") {$3="Information:"; print}}' >> full.log

