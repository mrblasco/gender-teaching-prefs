#!/usr/bin/env bash

# Set date automatically (YYYY-MM-DD)
DATE=$(date +%Y-%m-%d)
OUTDIR=peer_review/submissions/${DATE}-Nexus

mkdir -p $OUTDIR

old="peer_review/submissions/2025-06-07-PNAS/Manuscript/main_20250607_5c14834.tex"
new="docs/report.tex"

# Generate diff
latexdiff \
    --exclude-safecmd="printbibliography" \
    --exclude-textcmd="input" \
    --config="PICTUREENV=(?:picture|DIFnomarkup|tabu)[\w\d*@]*" "$old" "$new" > diff.tex

# Compile
xelatex diff.tex
#pdflatex diff.tex

# Copy PDF to submission directory with date tag
cp diff.pdf "$OUTDIR/"
cp docs/report.pdf "$OUTDIR/"
cp peer_review/docs/responses.pdf "$OUTDIR/"