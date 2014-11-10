#!/bin/bash
# extract number words from child directed speech corpus 
# CHILDES treebank for CDS taken from http://www.socsci.uci.edu/~lpearl/CoLaLab/CHILDESTreebank/childestreebank.html
# extract number words (category CD) using tregex

TREGEX=~/Downloads/stanford-tregex-2014-08-27/tregex.sh
CORPRA_DIR="/Users/edechter/Downloads/CHILDESTreebank-curr/"
PATTERN="CD=number"

PARSED_CORPRA=`ls ${CORPRA_DIR}/*.parsed`
echo $PARSED_CORPRA

OUT=""
for f in $PARSED_CORPRA
do
    CMD="${TREGEX} $PATTERN $f"
    echo -e "Processing $f: \n$CMD"
    OUT="$OUT `$CMD`"
    echo $OUT
    echo -e "Done.\n"
done

