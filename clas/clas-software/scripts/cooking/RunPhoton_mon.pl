#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunPhoton_mon;
}

sub RunPhoton_mon {

# Runs the monitoring program photon_mon. It parses the output
# text file for important database information.
#
# Usage: RunPhoton_mon($files, $Outpath, $db_file, $basename, $db_mysql)
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
  local($dispatcher) = $_[4];
  local($db_mysql) = $_[5];
  
  local($os) = $ENV{"OSNAME"};
  local($clas_bin) = $ENV{"CLAS_BIN"};
  
  if ($clas_bin eq "") {
    $clas_bin = "/group/clasdev/clas/src/prod-1-5/bin/$os/";
  }
  
  if ($dispatcher){
    $photon_cmd = "$clas_bin/photon_mon -R -D -P -i ";
  } else{
    $photon_cmd = "$clas_bin/photon_mon -R -P -i ";
  }
  
  # Monitor info is stored directly into MYSQL db with the -z flag
  if ($db_mysql) {
    $photon_cmd .= " -z ";
  }
  
  $photon_cmd .= " -ophmon$basename.hbook $files > phmon$basename.txt";
  
  print  "\n$0 EXECUTE: $photon_cmd at $start_time\n";
  
  system($photon_cmd);
  
  if (!$db_mysql) {
    dbmopen(%RUN,$db_file, 0644) || print  "$0: Could not open $db_file for offline db\n";
    
    $RUN{"photon_mon_cmd"} = $photon_cmd;
    
    if (-r "phmon$basename.txt") { 
      open(PHM_TXT, "phmon$basename.txt") || print  "$0: Could not open phmon$basename.txt\n";
    }
    
    while (<PHM>) {
      if ($evt_flag){
	if(@values = /\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)/){
	  $i = 0;
	  foreach $f (@names){
	    $RUN{$f} = @values[$i];
	    $i++;
	  }
	}
      }
      if (@values = /pi0 p\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)/){
	$i = 1;
	foreach $f (@values){
	  $key = "pi0p_s" . $i;  
	  print $key . " " . $f . "\n";
	  $RUN{$key} = $f;
	  $i++;
	}
      }
      if (@values = /pi0 p \(f\)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)/){
	$i = 1;
	foreach $f (@values){
	  $key = "pi0p(f)_s" . $i; 
	  print $key . " " . $f . "\n";
	  $RUN{$key} = $f;
	  $i++;
	}
      }
      if (@values = /pi\+ n\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)/){
	$i = 1;
	foreach $f (@values){
	  $key = "pi+n_s" . $i;  
	  print $key . " " . $f . "\n";
	  $RUN{$key} = $f;
	  $i++;
	}
      }
      if (@values = /pi\+ n \(f\)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)/){
	$i = 1;
	foreach $f (@values){
	  $key = "pi+n(f)_s" . $i;  
	  print $key . " " . $f . "\n";
	  $RUN{$key} = $f;
	  $i++;
	}
      }
      if (@names = /\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)/){
	if (@names[0] =~ /Nevt/) {
	  $evt_flag = 1;
	}
      }
    }
    
    close(PHM);
    
    dbmclose(%RUN);
  }
  
  MoveFile("phm*txt", $OutPath, "photon_mon text file", $db_file);
  MoveFile("phm*hbook", $OutPath, "photon_mon histo file", $db_file);
  
  $t = time();
  $end_time = &ctime($t);
  
  print "\n$0 FINISHED: $photon_cmd at $end_time\n";
  
  print "\n\n";
  
}

1;
