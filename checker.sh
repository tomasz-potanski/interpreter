#!/bin/bash

# Text color variables
txtund=$(tput sgr 0 1)          # Underline
txtbld=$(tput bold)             # Bold
bldred=${txtbld}$(tput setaf 1) #  red
bldblu=${txtbld}$(tput setaf 4) #  blue
green=$(tput setaf 2) 		#  green
bldgreen=${txtbld}$(tput setaf 2) 		#  green
bldwht=${txtbld}$(tput setaf 7) #  white
reset=$(tput sgr0)             # Reset
info=${bldwht}*${txtrst}        # Feedback
pass=${bldblu}*${txtrst}
warn=${bldred}*${txtrst}
ques=${bldblu}?${txtrst}

errors=0

clear
echo "\n"
echo "|||--------------------------------"
echo "||| Those files should be vaild:"
for file in ./good/*pas
do

#echo "$file"
	echo "||| $file:"
	if [ `./verifier $file 2>&1 | grep "Parse Successful!" | wc -l` = 1 ]
	then
		echo "|||    $green \bparser: OK$reset"
	else 
		echo "|||    $bldred \bparser: Error$(tput sgr0)" 
		echo "  "
		./verifier $file
		echo "  "
		errors=$((errors + 1))
	fi

	if [ `./interpreter $file 2>&1 | grep -E "Error|Non-exhaustive pat" | wc -l` = 0 ]
	then
		echo "|||    $green \binterpreter: OK$reset"
	else 
		echo "|||    $bldred \binterpreter: Error$(tput sgr0)" 
		echo "  "
		./interpreter $file
		echo "  "
		errors=$((errors + 1))
	fi

done

echo "|||"


echo "|||--------------------------------"
echo "|||"
echo "||| Those files should NOT be vaild:"
for file in ./bad/*pas
do

#echo "$file"
	echo "||| $file:"
	if [ `./verifier $file | grep "Parse Successful!" | wc -l` = 1 ]
	then
		echo "|||    $bldred \bparser: OK$reset"
		errors=$((errors + 1))
	else 
		echo "|||    $green \bparser: Error$(tput sgr0)"
	fi

	if [ `./interpreter 2>&1 $file | grep "Error" | wc -l` = 0 ]
	then
		echo "|||    $bldred \binterpreter: OK$reset"
		errors=$((errors + 1))
	else 
		echo "|||    $green \binterpreter: Error$(tput sgr0)"
	fi

done

echo "|||--------------------------------"


if [ $errors -gt 0 ]
then
	echo "$bldred"
	echo ">>> Ilosc bledow: $errors <<<$reset"
else 
	echo "$bldgreen"
	echo ">>> Testy OK <<<$reset"
fi