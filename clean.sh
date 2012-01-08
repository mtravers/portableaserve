#!/bin/sh

for i in `cat acl-compat/.cvsignore` ;
  do find . -name $i -type f -delete
done
