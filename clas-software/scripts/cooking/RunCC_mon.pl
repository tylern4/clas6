#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunCC_mon;
}

sub RunCC_mon {

# Runs the monitoring program cc_mon. It parses the 
# output for important database information.
#
# Usage: RunCC_mon($files, $Outpath, $db_file, $basename, $db_mysql)
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
    local($db_mysql) = $_[4];

    local($os) = $ENV{"OSNAME"};
    local($clas_bin) = $ENV{"CLAS_BIN"};
    
    if ($clas_bin eq "") {
	$clas_bin = "/group/clasdev/clas/src/prod-1-5/bin/$os/";
    }
    
    $ccmon_cmd = $clas_bin."/cc_mon -P -g1 -i -occm$basename $files";
    
    print  "\n$0 EXECUTE: $ccmon_cmd at $start_time\n";

    if (!$db_mysql) {
      dbmopen(%RUN,$db_file, 0644) || print  "$0: Could not open $db_file for offline db\n";

      $RUN{"cc_mon command"} = $ccmon_cmd;

      dbmclose(%RUN);    
    }

    open(CCMON, "$ccmon_cmd 2>&1 |") || warn "$0: Could not open $ccmon_cmd\n";
    
    while (<CCMON>) {
	print $_;
    }

    close(CCMON);

    MoveFile("ccm*hb4", "$OutPath", "cc_mon histo file", $db_file);
    MoveFile("ccm*txt", "$OutPath", "cc_mon text file", $db_file);

    $t = time();
    $end_time = &ctime($t);

    print "\n$0 FINISHED: $ccmon_cmd at $end_time\n";

    print"\n\n";

}

1;
