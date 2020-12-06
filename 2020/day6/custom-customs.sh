#!/bin/bash -e

groups=$(cat $1 | tr "\n" " " | sed -e 's/  /\n/g' | sed -e 's/ //g')
for g in $groups; do
    v=$(echo $g | fold -1 | sort -u | tr -d "\n" | wc -c)
    ((sum += v))
done

echo "Answers From Anyone:" $sum
