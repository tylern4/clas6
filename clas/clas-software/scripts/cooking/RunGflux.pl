#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunGflux;
}

sub RunGflux {

# Runs the monitoring program scaler_mon. It parses the output
# for problems. It then parses the output text file for 
# important database information.
#
# Usage: RunScaler_mon($files, $Outpath, $db_file, $basename, $run_type, $db_mysql)
#
# Inputs: $files          = List of files to be processed
#         $Outpath        = Path where the ouput file should be copied
#         $db_file        = Database file including path
#         $basename       = Basename for output files
#         $run_type       = electron or photon
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
  local($run_type) = $_[4];
  local($db_mysql) = $_[5];
  
  local($os) = $ENV{"OSNAME"};
  local($clas_bin) = $ENV{"CLAS_BIN"};
  
  if ($clas_bin eq "") {
    $clas_bin = "/group/clasdev/clas/src/prod-1-5/bin/$os/";
  }
  
  $fnum = $basename;
  $fnum =~  s/\d+\WA*//;
  
  $tripdir = "/work/clas/production/g8a/pass1/monitor/sync";
  
  $scaler_mon_cmd = "$clas_bin/gflux -c -F100000 -b -T -f$fnum -t$tripdir/clas_0$basename.trip -y$tripdir/clas_0$basename.sync -ogflux$basename.hbook $files";
  
  print  "\n$0 EXECUTE: $scaler_mon_cmd at $start_time\n";
  
  system($scaler_mon_cmd);
  
  if (!$db_mysql) {
    dbmopen(%RUN,$db_file, 0644) || print  "$0: Could not open $db_file for offline db\n";
    
    $RUN{"scaler_mon cmd"} = $scaler_mon_cmd;
    
#    if (-r "scaler$basename.txt") { 
#	open(SCAL_TXT, "scaler$basename.txt") || print  "$0: Could not open scaler$basename.txt\n";
#    }
    
#    while (<SCAL_TXT>) {
#	if ($run_type eq "electron") {
#	    if (/^Trigger Rate :\s*(\d+)\.(\d+)/) {
#		$RUN{"Trigger Rate"} = $1.".".$2." Hz";
#	    }
#	    if (/^Live time percentage by clock:\s*(\d+)\.(\d+)/) {
#		$RUN{"Clock Live time %"} = $1.".".$2;
#	    }
#	    if (/^Live time percentage by Fcup:\s*(\d+)\.(\d+)/) {
#		$RUN{"FC Live time %"} = $1.".".$2;
#	    }
#	    if (/^Ungated Faraday cup:\s*(\d+)/){
#		$RUN{"Ungated FC counts"} = $1;
#		$line = <SCAL_TXT>;
#		if ($line =~ /^Ungated Faraday cup:\s*(\d+)\.(\d+)e-(\d+)/) {
#		    $RUN{"Ungated FC Coulombs"} = $1.".".$2."e-".$3;
#		}
#	    }
#	}
#	if ($run_type eq "photon") {
#	}
#    }
#
#    close(SCAL_TXT);

    dbmclose(%RUN);
  }
#    MoveFile("gflux*txt", $OutPath, "gflux text", $db_file);
  MoveFile("gflux*hbook", $OutPath, "gflux hbook", $db_file);

  MoveFile("gflux*dat", $OutPath, "gflux data", $db_file);

  
  $t = time();
  $end_time = &ctime($t);
  
  print "\n$0 FINISHED: $scaler_mon_cmd at $end_time\n";
  
  print "\n\n";
    
}

1;
