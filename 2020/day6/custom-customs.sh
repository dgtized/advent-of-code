#!/bin/bash -e

groups=$(cat $1 | tr "\n" " " | sed -e 's/  /\n/g' | sed -e 's/ //g')
for g in $groups; do
    v=$(echo $g | fold -1 | sort -u | tr -d "\n" | wc -c)
    ((sum += v))
done

echo "Answers From Anyone:" $sum

for g in $(awk NF=NF RS="" FS="\n" OFS=":" $1); do
    echo $g | cut -d: -f1 | fold -1 | sort > group_answers
    for i in $(echo $g | sed -e 's/:/\n/g'); do
        echo $i | fold -1 | sort | join group_answers - > join_group
        mv join_group group_answers
    done
    v=$(cat group_answers | tr -d "\n" | wc -c)
    ((everyone += v))
done

echo "Answers From Everyone:" $everyone
