#!/bin/sh
cp $1 $1.orig
tempfich=/tmp/${USER}_sed_usrlocal_$$
cat $1 | sed 's/^#\!\/usr\/local/#\!\/import\/divers/' > $tempfich
/bin/mv $tempfich $1


