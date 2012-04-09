#!/bin/sh

terminate="\033[0m"
begin=
end=

if [ $# -ge 1 ]; then
    case $1 in
    "black")       begin="\033[0;30m"; end="$terminate";;
    "red")         begin="\033[0;31m"; end="$terminate";;
    "green")       begin="\033[0;32m"; end="$terminate";;
    "brown")       begin="\033[0;33m"; end="$terminate";;
    "blue")        begin="\033[0;34m"; end="$terminate";;
    "purple")      begin="\033[0;35m"; end="$terminate";;
    "cyan")        begin="\033[0;36m"; end="$terminate";;
    "lightgray")   begin="\033[0;37m"; end="$terminate";;
    "darkgray")    begin="\033[1;30m"; end="$terminate";;
    "boldred")     begin="\033[1;31m"; end="$terminate";;
    "boldgreen")   begin="\033[1;32m"; end="$terminate";;
    "boldbrown")   begin="\033[1;33m"; end="$terminate";;
    "boldblue")    begin="\033[1;34m"; end="$terminate";;
    "boldpurple")  begin="\033[1;35m"; end="$terminate";;
    "boldcyan")    begin="\033[1;36m"; end="$terminate";;
    "white")       begin="\033[1;37m"; end="$terminate";;
    esac
fi

while read line; do
    printf "${begin}%s${end}\n" "$line"
done
