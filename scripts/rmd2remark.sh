#!/usr/bin/env bash

INFILE=$1
OUTFILE=${INFILE/.rmd/}.html

echo "$INFILE ==> $OUTFILE"

Rscript -e "require(knitr); knit('$INFILE', output='$OUTFILE')"



