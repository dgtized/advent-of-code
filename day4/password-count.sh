#!/bin/bash -ex

# grep sequence for pairs, and then remove any where a number decreased from
# proceeding answer
seq 254032 789860 |
    grep -e '00\|11\|22\|33\|44\|55\|66\|77\|88\|99' |
    grep -ve '[1-9]0\|[2-9][01]\|[3-9][0-2]\|[4-9][0-3]\|[5-9][0-4]\|[6-9][0-5]\|[789][0-6]\|[89][0-7]\|9[0-8]' > matches

wc -l matches

# globally replace any sequence of 3+ identical numbers with x, and then look
# for any remaining sequences that still have a pair.
sed -e 's/0\{3,\}\|1\{3,\}\|2\{3,\}\|3\{3,\}\|4\{3,\}\|5\{3,\}\|6\{3,\}\|7\{3,\}\|8\{3,\}\|9\{3,\}/x/g' matches |
    grep -e '00\|11\|22\|33\|44\|55\|66\|77\|88\|99' > refined

wc -l refined
