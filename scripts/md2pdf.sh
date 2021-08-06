#!/usr/bin/env bash

INFILE=$1
OUTFILE=${INFILE/.md/}.pdf

echo "$INFILE ==> $OUTFILE"

pandoc -V geometry="margin=1in" --number-sections -o $OUTFILE $INFILE

