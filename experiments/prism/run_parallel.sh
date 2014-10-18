#!/bin/bash

NumExamples=(100 300 500 700 900 1100 1300 1500 1700 1900 2100);
NumReps=10;
NumPred=(1 2 3 4);
SaveDir="data/overnight"
args=()

for nEx in ${NumExamples[*]}
do
    for rep in $(eval echo {1..$NumReps})
    do
        for nPred in ${NumPred[*]}
        do
            args+=("$nEx")
            args+=("$rep")
            args+=("$nPred")
        done
    done
done

CMD="parallel -n 3 --delay 2 upprism srs {} $SaveDir &> logs/overnight/log_{1}_{2}_{3}.log ::: ${args[*]}"
echo $CMD
`$CMD`





