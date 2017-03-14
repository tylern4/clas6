#!/bin/sh -f
runno=0
TCIin=tci.dat
TCIout=tci_new.dat
if [ $# -lt 1 -o "X$1" = "X-h" ]; then
 echo "tci.sh <tcioffset> [<TCI_input_file> | -r <runno>] [<TCI_output_file>]"
 echo "       tcioffset = overall offset (from RFused-Tpho =tagraw hist. rftpho)"
 echo "                 (or NEGATIVE of anamon histogram seb/3102)"
 echo "       TCI_input_file  (optional, default=${TCIin})  or alternatively:"
 echo "       -r run_number = reads file from CALDB for RunIndex=${RUN_INDEX}" 
 echo "       TCI_output_file (optional, default=${TCIout})" 
 exit
fi
tcioff=$1
if [ $# -gt 1 ]; then
 if [ "X$2" = "X-r" -a $# -gt 2 ]; then
  runno=$3
  [ $# -gt 3 ] && TCIout=$4
 else
  TCIin=$2
  [ $# -gt 2 ] && TCIout=$3
 fi
fi
echo "got add.offset=${tcioff} TCIin=${TCIin} TCIout=${TCIout} runno=${runno}"
if [ $runno -gt 0 ]; then
 [ -r $TCIin ] && /bin/rm -f $TCIin
 $CLAS_TOOLS/caldb/caldb_show_constants_run.pl s=TAG_CALIB ss=tag_t i=ci r=${runno} it=${RUN_INDEX} > $TCIin
fi
[ -r $TCIout ] && /bin/rm -f $TCIout
nw=`gawk 'NR==1 { print NF }' ${TCIin}`
[ $nw -gt 1 ] && i=3 || i=1
imax=$[i+121]
gawk 'NR=='$i' { print $1+'$tcioff' }' $TCIin > $TCIout
i=$[i+1]
while [ $i -lt $imax ]; do
#adjust this in case that the tci value is not the first token
#tci=`gawk 'NR=='$i' { print $3 }' ${TCIin}`
  gawk 'NR=='$i' { print $1+'$tcioff' }' $TCIin >> $TCIout
 i=$[i+1]
done
