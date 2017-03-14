#!/bin/tcsh
############################
#
# buildBatchJobFileForCooking.sh
# 
# This is a shell script which builds all 
# the necessary file for cooking a job for
# the DC alignment procedure, and submits
# them to the JLab farm
#
# Matt Bellis
# 12/14/05
#
############################

###############
#
# These variables may need to be changed from
# run to run, or based on the user's 
# preferences
#
###############
set prlinkfile = prlink_dvcs_torus_solenoidnegative.bos
set poltargcurrent = "-534000"
set targposition = "-58.0"
set poltargetfieldfilename = bgrid_s.fpk
set bfieldfilename = bgrid_T67to33.fpk
set useralignlocation = $CLAS_BIN
###############

#
# Test to see if some environment variables
# are set
#
set test = `printenv | grep DCALIGN`
if( $test == "" ) then 
    echo
    echo Environment variable DCALIGN is not defined
    echo
    exit(-1)
endif

set test = `printenv | grep EMAIL`
if( $test == "" ) then 
  echo
    echo Environment variable EMAIL is not defined
    echo
    exit(-1)
endif


#
# Make sure we can overwrite files
#
unset noclobber

#
# Write out the usage
#
if($# < 7) then
  echo
	echo "Wrong number of arguments"
  echo
  echo "Usage: buildBatchJobFileForCooking.sh <pass num> <sigma1> ... <sigma6> [max events=25000] [maxfiles=20]"
  echo
  exit(-1)
endif

#
# Set up our local variables from the command line arguments
#
set maxevents = "25000"
@ maxfiles = 20

set num = $1 # Pass number

set SIGMA1 = $2 # Sigmas for each superlayer
set SIGMA2 = $3
set SIGMA3 = $4
set SIGMA4 = $5
set SIGMA5 = $6
set SIGMA6 = $7

if($8 != "")then # Number of events in each file to cook
  set maxevents = $8
endif

if($9 != "")then # Number of files to cook
  @ maxfiles = $9
endif

set passdir = $DCALIGN/pass$num

#
# Make whatever directories are needed
#
if(! -e $passdir)then
  echo
  echo "Making directory " $passdir
  echo
  mkdir $passdir
endif

if(! -e ./tclFiles)then
  echo
  echo "Making directory tclFiles"
  echo
  mkdir ./tclFiles
endif

if(! -e ./jobFiles)then
  echo
  echo "Making directory jobFiles"
  echo
  mkdir ./jobFiles
endif

if(! -e ./commandFiles)then
  echo
  echo "Making directory commandFiles"
  echo
  mkdir ./commandFiles
endif

#
# Read back our sigmas and save them in a file
#
echo "sigma(1)\t" $SIGMA1 
echo "sigma(2)\t" $SIGMA2 
echo "sigma(3)\t" $SIGMA3 
echo "sigma(4)\t" $SIGMA4 
echo "sigma(5)\t" $SIGMA5 
echo "sigma(6)\t" $SIGMA6 

echo "sigma(1)\t" $SIGMA1 > $passdir/sigmas.txt
echo "sigma(2)\t" $SIGMA2 >> $passdir/sigmas.txt
echo "sigma(3)\t" $SIGMA3 >> $passdir/sigmas.txt
echo "sigma(4)\t" $SIGMA4 >> $passdir/sigmas.txt
echo "sigma(5)\t" $SIGMA5 >> $passdir/sigmas.txt
echo "sigma(6)\t" $SIGMA6 >> $passdir/sigmas.txt

#
# Save the alignment constants with which we are cooking
#
printDCgeomCommand.sh > $passdir/offsetsUsedInCookingTheseFiles.txt

#
# For each file to cook, create it's own tcl file and batch farm file 
# and then submit them and move them to the appropriate directories
#
@ i = 0

while ($i < $maxfiles )
  set tag = `printf %02d $i`

  set inputfile = alignfile.$tag
  set myhistfile = alignfile.$tag.hbook
  set outfile =  alignfile.$tag.out
  set logfile =  alignfile.$tag.log

  if(-e $DCALIGN/splitBosFiles/$inputfile) then

    echo Building and submitting files for $inputfile

    # 
    # Make the .tcl file
    #
    set tclfile = `pwd`/tclFiles/user_align.pass$num.$tag.tcl
    echo Creating $tclfile

    echo source /u/group/clas/builds/PRODUCTION/packages/tcl/recsis_proc.tcl\; > $tclfile
    echo puts '"'This is your TCL file \!\!\!'"'\; >> $tclfile
    echo turnoff ALL\; >> $tclfile
    echo global_section off\; >> $tclfile
    echo turnon seb cc trk tof egn user pid\; >> $tclfile
    echo >> $tclfile
    echo inputfile           $inputfile >> $tclfile
    echo setc chist_filename $myhistfile >> $tclfile
    echo outputfile $outfile >> $tclfile
    echo setc log_file_name $logfile >> $tclfile
    echo >> $tclfile
    echo setc outbanknames"("1")" '"'HEADDC0 DC1 TBLAHBLAHDPLTDPLEC  SC  CC  HEVTEVNTDCPBCCPBSCPBECPBLCPB    CALLTGBITRKSTBERTBTRSCRCCL01PART    TBIDBMPREPICECPIECHB'"'\; >> $tclfile
    echo >> $tclfile
    echo set runfile 1 >> $tclfile
    echo >> $tclfile
    echo go\;  >> $tclfile
    echo >> $tclfile
    echo set torus_current 0\; >> $tclfile
    echo "#**************to insert the solenoid field******************" >> $tclfile
    echo set poltarget_current $poltargcurrent\; >> $tclfile
    echo set mini_torus_current 0\; >> $tclfile
    echo set TargetPos"("3")" $targposition\; >> $tclfile
    echo set BeamPos"("3")" $targposition\; >> $tclfile
    echo set TargetMagPos"("3")" $targposition\; >> $tclfile
    echo setc poltarget_field_file_name '"'$poltargetfieldfilename'"' >> $tclfile
    echo setc bfield_file_name '"'$bfieldfilename'"' >> $tclfile
    echo setc prlink_file_name '"'$prlinkfile'"' >> $tclfile
    echo "#***********************************************************" >> $tclfile
    echo >> $tclfile
    echo set lseb_nt_do   -1\; >> $tclfile
    echo set lall_nt_do   -1\; >> $tclfile
    echo set lgraw_nt_do   0\; >> $tclfile
    echo  >> $tclfile
    echo "#***************** For alignment procedure ********************" >> $tclfile
    echo set dc_mult_Sigma"("1")" $SIGMA1"." >> $tclfile
    echo set dc_mult_Sigma"("2")" $SIGMA2"." >> $tclfile
    echo set dc_mult_Sigma"("3")" $SIGMA3"." >> $tclfile
    echo set dc_mult_Sigma"("4")" $SIGMA4"." >> $tclfile
    echo set dc_mult_Sigma"("5")" $SIGMA5"." >> $tclfile
    echo set dc_mult_Sigma"("6")" $SIGMA6"." >> $tclfile
    echo "#***********************************************************" >> $tclfile
    echo  >> $tclfile
    echo "#**************** Added for alignment procedure ************" >> $tclfile
    echo set trk_minhits"("1")" 2\; >> $tclfile
    echo set lst_do      0\; >> $tclfile
    echo set ltime_do    0\; >> $tclfile
    echo set lpid_make_trks   0\; >> $tclfile
    echo set photon_trig_type 0\; >> $tclfile
    echo set trk_magtype 5\; >> $tclfile
    echo set trk_print"("1")" 1\; >> $tclfile
    echo set trigger_particle -211\; >> $tclfile
    echo set trk_maxiter       8\; >> $tclfile
    echo set trk_minhits"("1")"    2\; >> $tclfile
    echo set trk_lrambfit_chi2 50.\; >> $tclfile
    echo set trk_tbtfit_chi2   70.\; >> $tclfile
    echo set trk_prfit_chi2    70.\; >> $tclfile
    echo set trk_minlramb 4\; >> $tclfile
    echo "#***********************************************************" >> $tclfile
    echo  >> $tclfile
    echo "# tell FPACK not to stop if it thinks you are running out of time" >> $tclfile
    echo fpack '"'timestop -9999999999'"' >> $tclfile
    echo "# do not send events to event display" >> $tclfile
    echo set lscat \$false\; >> $tclfile
    echo set ldisplay_all \$false\; >> $tclfile
    echo "#set nevt_to_skip  500000; # how many events to SKIP set to .le. 0 to NOT SKIP" >> $tclfile
    echo "# tell recsis to pause or go" >> $tclfile
    echo setc rec_prompt '"'exec whoami]_recsis\> '"' >> $tclfile
    echo go $maxevents >> $tclfile
    echo exit_pend\; >> $tclfile

    #
    # Make the command file and submit it. 
    #
    set commandfile = `pwd`/commandFiles/command.pass$num.$tag
    echo Creating $commandfile

    echo cp $DCALIGN/splitBosFiles/$inputfile . > $commandfile
    echo cp $useralignlocation/user_align . >> $commandfile
    echo cp $tclfile . >> $commandfile
    echo ./user_align -t $tclfile >> $commandfile
    echo mv $outfile $passdir/. >> $commandfile
    echo mv $myhistfile $passdir/. >> $commandfile
    echo rm $inputfile $logfile user_align >> $commandfile

    chmod +x $commandfile

    #
    # Make the batch job file and submit it. 
    #
    set jobfile = user_alignJob.pass$num.$tag
    echo Creating $jobfile

    echo PROJECT: clas > $jobfile
    echo COMMAND: $commandfile >> $jobfile
    echo OPTIONS: >> $jobfile
    echo JOBNAME: user_align$num"."$tag >> $jobfile
    echo MAIL: $EMAIL >> $jobfile
    echo OS: linux >> $jobfile

    #######
    #
    # Comment this line out if you just want to build the files
    # without submitting the job to the farm.
    #
    #######
    jsub user_alignJob.pass$num"."$tag
    #######

    mv user_alignJob.pass$num"."$tag jobFiles/

    endif

  @ i++

end

