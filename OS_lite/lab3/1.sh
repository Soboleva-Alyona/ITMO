#!/bin/bash
answ=$( date -u )

cd ~ && mkdir test && 
{ 
	echo "catalog test was created successfully" > report
	touch test/"$answ"
}

ping www.net_nikogo.ru || echo "$(date -u) error" >> report
