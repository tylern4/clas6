#! /bin/sh
#
# Create the three release reports:
# 1. Differences from original checkout
# 2. Differences from reference release
# 3. New files with respect to reference release
#
# $Id: release_docs.sh,v 1.3 2002/11/19 18:13:27 claslib Exp $
#
if [ $# -ne 3 ] ; then
    echo 'Usage: release_docs.sh <checkout-date> <release-tag> <reference-release-tag>'
    exit 1
fi
checkout_date=$1
release_this=$2
release_that=$3
echo checkout_date=$checkout_date
echo release_this=$release_this
echo release_that=$release_that
starting_dir=`pwd`
echo starting in directory $starting_dir
cd $CLAS_ROOT/builds/$release_this/packages
if [ $? -ne 0 ] ; then
    echo script exiting
    exit 2
fi
PATH=./scripts:$PATH
export PATH
rm -f $starting_dir/release_report_*.txt
echo creating date difference report
cvs_date_diff.perl $checkout_date > $starting_dir/release_report_date.txt
echo creating release difference report
cvs_rel_diff.perl $release_that 1> $starting_dir/release_report_diff.txt \
    2> /tmp/cvs_error.tmp
echo creating release new file report
cvs_rel_diff_err.perl > $starting_dir/release_report_new.txt
exit
