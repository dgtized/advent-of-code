#!/bin/bash -e

# separate each group into a single line and iterate over them
groups=$(cat $1 | tr "\n" " " | sed -e 's/  /\n/g' | sed -e 's/ //g')
for g in $groups; do
    # take all the votes in the group as line, change them to a column with
    # fold, sort with unique and then flatten again with tr so as to count the
    # total uniq for that line.
    v=$(echo $g | fold -1 | sort -u | tr -d "\n" | wc -c)
    ((sum += v))
done

echo "Answers From Anyone:" $sum

# separate each group into a line, with each individuals votes separated by :
for g in $(awk NF=NF RS="" FS="\n" OFS=":" $1); do
    # grab the first individuals votes and create a list of answers
    echo $g | cut -d: -f1 | fold -1 | sort > group_answers

    # for all the individuals in the group
    for i in $(echo $g | sed -e 's/:/\n/g'); do
        # fold the votes into a column sort and then join with the previous set to create a new set
        echo $i | fold -1 | sort | join group_answers - > join_group
        # move the remainder from the join as the current group answer set
        mv join_group group_answers
    done
    # flatten the groups answers into a line and count characters
    v=$(cat group_answers | tr -d "\n" | wc -c)
    # sum results
    ((everyone += v))
done

echo "Answers From Everyone:" $everyone
