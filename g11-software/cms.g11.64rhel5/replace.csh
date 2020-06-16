#! /bin/sh -f
# replace all occurences of a string by another one in a third one.
# exemple : source replace.csh to ti rototo
# (result will be 'rotiti' on standart output)
# or : source replace.sh $OLDROOTSYS/lib $NEWROOTSYS/lib $LD_LIBRARY_PATH
# or : setenv $LD_LIBRARY_PATH `source replace.sh $ROOTSYS/lib "" $LD_LIBRARY_PATH`

if (-f /tmp/tmp.sed.script.$USER.$$) then
  rm /tmp/tmp.sed.script.$USER.$$
endif

setenv Replace `echo "$1" | sed -e 's/\//\\\//g'`
setenv With `echo "$2" | sed -e 's/\//\\\//g'`
echo "s/$Replace/$With/g" > /tmp/tmp.sed.script.$USER.$$
unsetenv Replace
unsetenv With
echo "$3" | sed -f /tmp/tmp.sed.script.$USER.$$
rm /tmp/tmp.sed.script.$USER.$$
