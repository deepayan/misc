#!/usr/bin/env bash

INFILE=$1
MDFILE=${INFILE/.rmd/}.md
PDFFILE=${INFILE/.rmd/}.pdf

echo "$INFILE ==> $MDFILE"
Rscript -e "require(knitr); knit('$INFILE', output='$MDFILE')"

echo "$MDFILE ==> $PDFFILE"
pandoc -o "$PDFFILE" -f markdown "$MDFILE"



