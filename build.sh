#!/bin/sh

for file in *.md
do
  base=$(basename $file .md)
  if [ $base != "README" ]
  then
    pandoc -i $file -t revealjs -s --highlight-style=breezeDark --include-in-header=style.html --highlight-style=highlight.theme >"$base.html"
  fi
done
