#!/usr/local/bin/perl -w

use File::Path;

# ------------------------------------------
#        To be modified by the user
# ------------------------------------------

$root_dir     = "/w/work601/guillo/eccalib";
$originalfiles_dir = "$root_dir/Skimmed";
$bin_dir = "/home/guillo/bin/Linux";
$NumberOfIterations = 6;                       # read manual to understant this

#-------  function for calibration ---------
#  user may add some flags to override some default behavior

$computeCalibConsts   = "$bin_dir/computeCalibConsts -i";
$fitTimeDifference = "$bin_dir/fitTimeDifference -i";
$rejectGammas = "$bin_dir/rejectGammas -i";
$getCalibConsts = "$bin_dir/getCalibConsts";


#-------------------------------------------------------
# User should not normally modify code below this part
#-------------------------------------------------------


# --------------------------------------------------
# creates different directories if they do not exist
# --------------------------------------------------

$log_dir = "$root_dir/Log";  # stdout and stderr of all programms
$calibconsts_dir = "$root_dir/CalibConsts"; # 
$ROOT_dir = "$root_dir/ROOT"; # directory to contain ROOT files

rmtree ($log_dir) if (-e $log_dir);  # delete directory (and sub-dirs) if it already exist
mkdir ($log_dir, 0755) || die "Cannot create LOG directory $log_dir $!\n";

rmtree ($calibconsts_dir) if (-e $calibconsts_dir);  # delete directory (and sub-dirs) if it already exist
mkdir ($calibconsts_dir, 0755) || die "Cannot create Calibration Constants directory $calibconsts_dir $!\n";

rmtree ($ROOT_dir) if (-e $ROOT_dir);
mkdir ($ROOT_dir, 0755) || die "Cannot create ROOT directory $ROOT_dir $!\n";

# ---------------------------------------
# files for starting calibrating (pass 1)
# ---------------------------------------
$finalcalibfile = "$calibconsts_dir/calibconsts.dat";
$start_dir = "$root_dir/pass1";
rmtree ($start_dir) if (-e $start_dir);
mkdir ($start_dir, 0755);
system ("ln -s $originalfiles_dir/* $start_dir/.");

# -----------------------------------------------
# creates stub file from files in pass1 directory
#------------------------------------------------

$stubfile = "$root_dir/files.dat";
$stubfile_orig = "$root_dir/files_orig.dat";

opendir(DIR, "$root_dir/pass1") || die "Couldn't read directory $root_dir $!\n";   # directory containing original skimmed files
open(STUBFILE, "> $stubfile") || die "Couldn't create file $stubfile $! \n";
open(STUBFILE_ORIG, "> $stubfile_orig") || die "Couldn't create file $stubfile_orig $! \n";

while (defined ($file = readdir(DIR))) {
    next if $file =~ /^\.\.?$/; # skip . and ..
    print STUBFILE "$root_dir/pass1/$file\n";
    print STUBFILE_ORIG "$originalfiles_dir/$file\n";
}
close(STUBFILE_ORIG);
close(STUBFILE);
closedir(DIR);

# -----------------------------------
#        start calibration
# -----------------------------------
for ($nLoop = 1; $nLoop <= $NumberOfIterations; $nLoop++) {

    $CurrentDir = "$root_dir/pass".$nLoop;
    $NextDir    = "$root_dir/pass".($nLoop+1);
    rmtree ($NextDir) if (-e $NextDir); # remove directory if it exists already
    mkdir ($NextDir, 0755); # create directory

    #
    # first step: finding calibration constants
    #
    $command = "$computeCalibConsts -f$stubfile -c$calibconsts_dir/calibconsts${nLoop}.dat >& ${log_dir}/computeCalibConsts${nLoop}.log";
    system("$command");

    #
    # second step: fitting histogramms Texpected - Tfit
    #
    $command = "$fitTimeDifference -f$stubfile -c$calibconsts_dir/calibconsts${nLoop}.dat -m$calibconsts_dir/fitconsts${nLoop}.dat -r$ROOT_dir/calib${nLoop}.root >& ${log_dir}/fitTimeDifference${nLoop}.Log";
    #print "%%%% Doing $command\n";
    system("$command");

    #
    # third step: rejection mode
    #

    $command = "$rejectGammas -f$stubfile -c$calibconsts_dir/calibconsts${nLoop}.dat  -m$calibconsts_dir/fitconsts${nLoop}.dat -d$NextDir >& ${log_dir}/rejectGammas${nLoop}.Log";
    system("$command");
        
    #
    # last step: change stub file and delete old files
    #
    open(OLDSTUB, "$stubfile") || die "Couldn't read stub file $stubfile $!\n";
    open(NEWSTUB, "> $root_dir/files_new.dat") || die "Couldn't create temporary stub file $root_dir/files_new.dat $!\n ";
    $oldpattern = "pass". $nLoop;
    $newpattern = "pass".($nLoop + 1);
    while (<OLDSTUB>) {
	s/$oldpattern/$newpattern/;
	print NEWSTUB $_;
    }
    close(NEWSTUB);
    close(OLDSTUB);
    unlink($stubfile);
    rename("$root_dir/files_new.dat", $stubfile);
    rmtree ($CurrentDir); # delete previous directory
}

#
# compute the last calibration constants 
#

$command = "$computeCalibConsts -f$stubfile -c$calibconsts_dir/calibconsts${nLoop}.dat >& ${log_dir}/computeCalibConsts${nLoop}.log";
system($command);

#
# find best calibration constants from all previous calculations in the loop

$command = "$getCalibConsts -o$finalcalibfile $calibconsts_dir/calibconsts*.dat >& $log_dir/getCalibConsts.log";
system($command);

#
# test calibration on all data using latest calibration constants
#

$command = "$fitTimeDifference -f$stubfile_orig -c$calibconsts_dir/calibconsts.dat -m$calibconsts_dir/fitconsts${nLoop}.dat -r$ROOT_dir/calib${nLoop}.root >& ${log_dir}/fitTimeDifference${nLoop}.Log";
system($command);









