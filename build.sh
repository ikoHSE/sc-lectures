#!/bin/sh

for file in *.md
do
  base=$(basename $file .md)
  if [ $base != "README" ]
  then
    pandoc -i $file -t revealjs -s --highlight-style=breezeDark --include-in-header=style.html >"$base.html"
  fi
done
