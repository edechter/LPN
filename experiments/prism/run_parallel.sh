#!/bin/bash

NumExamples=(700 900 1100 1300 1500 1700 1900 2100);
NumReps=10;
NumPred=(4);
SaveDir="data/saturday_"
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

CMD="parallel -n 3 --noswap --resume-failed --joblog ./parallel_log --delay 2 upprism srs {} $SaveDir &> logs/overnight/log_{1}_{2}_{3}.log ::: ${args[*]}"
echo $CMD
`$CMD`





