#!/bin/sh


# what we'd like this script to do is the following:
# 1. first test for git.  If it isn't there, we can't do much. 
# 2. specify a directory to work in (default = current working directory)
# 3. see if any repos exist (?)
# 4. grab the missing repositories -- ?submodules implementation?
# 5. update/pull to update any present repositories.

github = "http://github.com/blindglobe"
git clone $(github)/CommonLispStat.git
git clone $(github)/lisp-matrix.git
git clone $(github)/fnv.git
git clone $(github)/cl-blapack.git
git clone $(github)/ffa.git
git clone $(github)/lift.git

# use just one directory for the ASDF files, and we'll link to link. 
mkdir ASDF
ln -s *.git/*.asd ASDF

# setup a file to configure the directories... this isn't quite right.
cat "(push \"ASDF\"  *asdf-load-dirs*)" > asdf-load.lisp
