#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 19, 1999

CONFIG:{
    package RunEC_ntupilizer;
}

sub RunEC_ntupilizer {

# Runs the monitoring  program filter_calib. 
#
# Usage: RunElastic_mon($files, $Outpath, $db_file, $basename, $db_mysql)
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

    open(L, ">> $progress_file") || warn "$0: Could not open log file: $progress_file.\n";

    $ntupl_cmd = "$clas_bin/nt10maker -M10000000 -t1 -orun$basename.rzn $files";

    print "\n$0 EXECUTE: $ntupl_cmd at $start_time.\n";

    if (!$db_mysql) {
      dbmopen(%RUN,$db_file, 0644) || print "$0: Could not open $db_file for offline db\n";
    
      $RUN{"EC ntupilizer command"} = $ntupl_cmd;

      dbmclose(%RUN);
    }

    open(NTUPL, "$ntupl_cmd 2>&1 |") || warn "\n$0: Could not open $ntupl_cmd\n";

    while (<NTUPL>) {
# Ignore annoying ntuple fill errors
      if ($_ =~ /ERROR in HFNT/) {
	next;
      }
      print $_;
    }

    close (NTUPL);
    
    $t = time();
    $end_time = &ctime($t);

    print "\n$0 FINISHED: $ntupl_cmd at $end_time\n";

    $lscmd = "ls -l run*rzn | ";

    open(LS, $lscmd);
    while(<LS>) {
      if ($_ =~ /(\S+)\s+(\d+)\s+(\S+)\s+(\S+)\s+(\d+)/) {
	$fsize = $5;
	if ($fsize > 65528) {
    	  MoveFile("run*rzn", $OutPath, "EC ntuple file", $db_file);
	} else {
	  print "\n#######################################################\n";
	  print "\n Did not move ntuple to silo due to empty ntuple\n"; 
	}
      }
    }

    print"\n\n";

  }

1;
