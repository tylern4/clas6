#!/bin/tcsh
if ($#argv != 2 || $1 == "-h") goto print_help
set this=$1
set that=$2
echo cd $CLAS_ROOT/builds/$this/packages
cd $CLAS_ROOT/builds/$this/packages
rm -f /tmp/{$this}_changes.txt /tmp/{$this}_new.txt /tmp/cvs_error.tmp
(cvs_rel_diff.perl $that > /tmp/{$this}_changes.txt) >& /tmp/cvs_error.tmp
cvs_rel_diff_err.perl > /tmp/{$this}_new.txt
exit
print_help:
echo Example:
echo "" 
echo " " diff_release.csh release-2-0-1 release-2-0
echo ""
echo will compare the release found in $CLAS_ROOT/builds/release-2-0-1 with
echo the cvs tag release-2-0. Changes will be written to
echo /tmp/release-2-0-1_changes.txt and new files will be written to
echo /tmp/release-2-0-1_new.txt.

