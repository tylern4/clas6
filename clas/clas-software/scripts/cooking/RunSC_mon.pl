#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunSC_mon;
}

sub RunSC_mon {

# Runs the monitoring program sc_mon. It parses the 
# output for important database information.
#
# Usage: RunSC_mon($files, $Outpath, $db_file, $basename, $db_mysql)
#
# Inputs: $files          = List of files to be processed
#         $Outpath        = Path where the ouput file should be copied
#         $db_file        = Database file including path
#         $basename       = Basename for output files
#         $db_mysql      = Flag to use the MYSQL db instead of the perl db.
 
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
    
    $scmon_cmd = $clas_bin."/sc_mon -R -b -oscm$basename.hbook $files";
    
    print  "\n$0 EXECUTE: $scmon_cmd at $start_time\n";

    if (!$db_mysql) {
      dbmopen(%RUN,$db_file, 0644) || print  "$0: Could not open $db_file for offline db\n";

      $RUN{"sc_mon command"} = $scmon_cmd;

      dbmclose(%RUN);
    }    

    open(SCMON, "$scmon_cmd 2>&1 |") || warn "$0: Could not open $scmon_cmd\n";
    
    while (<SCMON>) {
	print $_;
    }

    close(SCMON);

    MoveFile("scm*hbook", $OutPath, "sc_mon histo file", $db_file);

    $t = time();
    $end_time = &ctime($t);

    print "\n$0 FINISHED: $scmon_cmd at $end_time\n";

    print"\n\n";

}

1;
