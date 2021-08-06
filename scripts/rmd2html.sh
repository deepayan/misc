#!/usr/bin/env bash

INFILE=$1
MDFILE=${INFILE/.rmd/}.md
OUTFILE=${INFILE/.rmd/}.html

echo "$INFILE ==> $OUTFILE"

Rscript -e "library(knitr); knit('$INFILE', output='$MDFILE')"

pandoc -s -o $OUTFILE $MDFILE

## FIXME: need to do more to include CSS etc. Probably easiest to use --template=URL





