#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunPhysfilter;
}

sub RunPhysfilter {

# Runs the monitoring program physfilter. It parses the 
# output for important database information.
#
# Usage: RunPhysfilter($files, $Outpath, $db_file, $basename)
#
# Inputs: $files          = List of files to be processed
#         $Outpath        = Path where the ouput file should be copied
#         $db_file        = Database file including path
#         $basename       = Basename for output files
 
    require "ctime.pl";
    $perl_lib = "$ENV{'CLAS_SCRIPTS'}";
    require "$perl_lib/MoveFile.pl";
    
    local($t) = time();
    local($start_time) = &ctime($t);
    local($end_time);

    local($files)   = $_[0];
    local($OutPath) = $_[1];
    local($db_file) = $_[2];
    local($basename) = $_[3];

    local($os) = $ENV{"OSNAME"};
    local($clas_bin) = $ENV{"CLAS_BIN"};

    if ($clas_bin eq "") {
	$clas_bin = "/group/clasdev/clas/src/prod-1-5/bin/$os/";
    }

    $physfilt_cmd = "$clas_bin/physfilter -T4 -b -ophys$basename.filt $files";

    print "\n$0 EXECUTE: $physfilt_cmd at $start_time\n";

    dbmopen(%RUN,$db_file, 0644) || print "$0: Could not open $db_file for offline db\n";

    $RUN{"physfilter command"} = $physfilt_cmd;

    open(PHYS, "$physfilt_cmd 2>&1 |") || warn "DB: Could not open $phys_cmd.\n";

    while (<PHYS>) {
       print $_;
       if ($_ =~ /\# of events written:\s+(\d+)/) {
	   $RUN{"physfilter filtered events"} = $1;
       }
   }
    
    close(PHYS);

    dbmclose(%RUN);

    MoveFile("phys*filt", $OutPath, "physfilter file", $db_file);

    $t = time();
    $end_time = &ctime($t);

    print "\n$0 FINISHED: $physfilt_cmd at $end_time\n";

    print"\n\n";

}

1;







