#!/bin/bash

NumExamples=(100 200) # 200 300 400 500 600 700 800 900 1000);
NumReps=2;
NumPred=(1 2 3 4);
SaveDir="data/"
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

CMD="parallel -n 3 --delay 2 upprism srs {} $SaveDir &> logs/log_{1}_{2}_{3}.log ::: ${args[*]}"
echo $CMD
`$CMD`





