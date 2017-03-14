#!/bin/sh

cd $CLAS_PACK
cd $CLAS_PACK/scripts
for i in `ls *.pl *.perl`
do
toipnperl.sh $i
chmod u+x $i
done

cd $CLAS_PACK/cms
for i in `ls *.pl`
do
toipnperl.sh $i
chmod u+x $i
done
