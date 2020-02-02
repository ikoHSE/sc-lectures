#!/bin/sh

for file in *.md
do
  base=$(basename $file .md)
  pandoc -i $file -t revealjs -s --highlight-style=breezeDark --include-in-header=style.html >"$base.html"
done
