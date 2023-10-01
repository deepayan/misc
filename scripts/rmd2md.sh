#!/usr/bin/env bash

INFILE=$1
MDFILE=${INFILE/.rmd/}.md

echo "$INFILE ==> $MDFILE"
Rscript -e "require(knitr); knit('$INFILE', output='$MDFILE')"



