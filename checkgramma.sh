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

clear
echo "$bldred \r>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>$reset"
bnfc -m -haskell actualgrammar.cf | ack-grep --passthru --color -i "warning|error"
echo "$bldred \r--------------------------------------$reset"

sed -i /latex/d  Makefile
sed -i '4s/-o .*$/-o verifier/' Makefile
make 2>&1 | ack-grep --passthru --color -i "unused|conflicts|shift|reduce"
echo "$bldred \r<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<$reset"
ghc --make interpreter.hs
echo "$bldred \r<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<$reset"

echo "  "

git add --ignore-removal 'good/*.pas'
git add --ignore-removal 'bad/*.pas'
git update-index -q --refresh
if ! git diff-index --quiet HEAD --
then
	DATA=`date`
	echo "I am updating repo..."
	git commit -a -m "$1 ($(date))"
	git push origin master --quiet &
fi


echo "Do you want to run tests? [Y/n]"
read yn
    case "$yn" in
        "n") exit;;
	*) clear; sh checker.sh; break;;
    esac
