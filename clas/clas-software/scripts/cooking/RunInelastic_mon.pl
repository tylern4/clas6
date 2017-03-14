#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunInelastic_mon;
}

sub RunInelastic_mon {

# Runs the monitoring program inelastic_mon. It parses the 
# output for important database information.
#
# Usage: RunInelastic_mon($files, $Outpath, $db_file, $basename, $db_mysql)
#
# Inputs: $files          = List of files to be processed
#         $Outpath        = Path where the ouput file should be copied
#         $db_file        = Database file including path
#         $basename       = Basename for output files
#         $db_mysql       = Flag to use the MYSQL db instead of the perl db.
 
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

  $inemon_cmd = "$clas_bin/inelastic_mon -b -oinemon$basename.hbook $files";
    
  print  "\n$0 EXECUTE: $inemon_cmd at $start_time\n";
  
  if (!$db_mysql) {
    dbmopen(%RUN,$db_file, 0644) || print  "$0: Could not open $db_file for offline db\n";

    $RUN{"inelastic_mon command"} = $inemon_cmd;
  }

  open(INEMON, "$inemon_cmd 2>&1 |") || warn "$0: Could not open $emon_cmd\n";
  
  while (<INEMON>) {
    print $_;
    chop;
    if ($_ =~ /^  Nevt   Nelec/) {
      $stats = 1;
      next;
    }
    if ($stats) {
      $_ =~ s/^\s+//;
      @line = split(/\s+/,$_); 
      if (!$db_mysql) {
	$RUN{"ine ep"} = $line[2];
	$RUN{"ine epi+"} = $line[3];
	$RUN{"ine epi+pi-p"} = $line[4];
	$RUN{"ine epi+p"} = $line[5];
	$RUN{"ine epi+pi-"} = $line[6];
	$RUN{"ine epi-p"} = $line[7];
	$RUN{"ine eK+"} = $line[8];
	$RUN{"ine eK+p"} = $line[9];
      }
      $stats = 0;
    }
  }
  
  close(INEMON);
  
  if (!$db_mysql) {
    dbmclose(%RUN);
  }

  MoveFile("inemon*hbook", $OutPath, "inelastic_mon file", $db_file);
  
  $t = time();
  $end_time = &ctime($t);
  
  print "\n$0 FINISHED: $inemon_cmd at $end_time\n";
  
  print"\n\n";
  
}

1;
