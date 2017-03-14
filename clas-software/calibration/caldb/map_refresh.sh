#! /bin/sh
export CLAS_ROOT=$1
hostname=$2
min=$3
max=$4
export CLAS_PARMS=$CLAS_ROOT/parms
#echo CLAS_PARMS = $CLAS_PARMS
TEMPDIR=$CLAS_ROOT/db2map_temp
#echo TEMPDIR=$TEMPDIR
MAPDIR_NEW=$TEMPDIR/Maps_$min-$max
#echo MAPDIR_NEW = $MAPDIR_NEW
export CLAS_BIN=/group/clas/builds/release-2-5/bin/LinuxRH6
#echo CLAS_BIN=$CLAS_BIN
export CLAS_TOOLS=$CLAS_ROOT/tools
#echo CLAS_TOOLS=$CLAS_TOOLS
#
#echo make $TEMPDIR and cd there
test -d $TEMPDIR && echo temp. dir exists, exiting && exit 1
mkdir $TEMPDIR
cd $TEMPDIR
#
#echo /apps/bin/perl $CLAS_TOOLS/caldb/db2map.pl runMin=$min runMax=$max hostname=$hostname skip_run_control=1 quiet=1
/apps/bin/perl $CLAS_TOOLS/caldb/db2map.pl runMin=$min runMax=$max hostname=$hostname skip_run_control=1 quiet=1
status_db2map=$?
echo exit status from db2map.pl = $status_db2map
if [ $status_db2map -ne 0 ]
    then
    echo exiting due to error in db2map.pl
    exit 1
fi
chmod g+w $MAPDIR_NEW/*.map # allow write access to all maps
cp $CLAS_PARMS/Maps/RUN_CONTROL.map $MAPDIR_NEW # get the old RUN_CONTROL.map
chmod g+w $MAPDIR_NEW/RUN_CONTROL.map # allow write to RUN_CONTROL
#
rm -rf $CLAS_PARMS/Maps_old
mv $CLAS_PARMS/Maps $CLAS_PARMS/Maps_old
mv $MAPDIR_NEW $CLAS_PARMS/Maps
#
echo removing $TEMPDIR
rm -rf $TEMPDIR
#
exit 0
