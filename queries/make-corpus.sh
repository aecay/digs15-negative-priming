#!/bin/sh

# Edit this command to point to CorpusSearch on your machine
CS_COMMAND="java -classpath /home/aecay/corpora/CS_2.003.04.jar csearch/CorpusSearch"

# Revision queries
$CS_COMMAND revision.q ppcme2.out
$CS_COMMAND revision2.q ppcme2.out.out
$CS_COMMAND revision3.q ppcme2.out.out.out

mv ppcme2.out.out.out.out all-revisions.out

# Filter query
$CS_COMMAND filter.q all-revisions.out

# Coding query

$CS_COMMAND coding.c filter.out

# Data file

$CS_COMMAND only_coding.q coding.cod

sed -i -e "s/@/:/" coding.cod.ooo
