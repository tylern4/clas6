#!/apps/bin/perl

use lib ("$ENV{CLAS_TOOLS}/perl");
use Csql;

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunA1c;
}

sub RunA1c {

# Runs the cooking program a1c. It parses the output for the 
# number of events cooked and the total number of events.
#
# Usage: RunA1c($args, $TapePath, $db_file, $db_mysql)
# 
# Inputs: $args          = List of arguments to be passed to a1c.
#         $TapePath      = Path to where the output files should 
#                          be copied.
#         $db_file       = Database file including path.
#         $db_mysql      = Flag to use the MYSQL db instead of the perl db.

  require "ctime.pl";

  local($args) = $_[0];
  local($TapePath) = $_[1];
  local($db_file) = $_[2];
  local($db_mysql) = $_[3];
  
  local($host) = $ENV{"HOSTNAME"};
  local($os) = $ENV{"OSNAME"};
  local($clas_bin) = $ENV{"CLAS_BIN"};
  local($csql_table) = $ENV{"CSQL_TABLE"};
  
# Set default CLAS_BIN
  if ($clas_bin eq "") {

#   $clas_bin = "/group/clasdev/clas/src/prod-1-5/bin/$os/";
    $clas_bin = "/work/clas/pdisk4/clarisse/pass0/v1/bin/$os/";
  }
  
  $a1c_cmd = $clas_bin."/a1c_debug ".$args;
  
  $t = time();
  $start_time = &ctime($t);
  
  print  "\n$0 Execute: Process $a1c_cmd on $host $run_file at ".$start_time."\n";
    
  chop($start_time);
    
  if ($os eq "Linux") {
    ($doweek, $month, $day, $time, $year) = split(/\s+/, $start_time);
    if ($year =~ /US/) {
      ($doweek,$month,$day,$time,$zone,$year) = split(/\s+/, $start_time);
    }
  }
  else {
    ($doweek, $month, $day, $time, $zone, $year) = split(/\s+/, $start_time);
  }

#MYSQL DATABASE INSERTION

  if ($db_mysql) {

    $a1c_st_time = "NOW()";
    
    @row = (
	  {
	    name => "a1cStartTime",
	    value => $a1c_st_time
	   }
          );
    UpdateRow(@row);
  }

# PERL DATABASE INSERTION

  else{


    dbmopen(%RUN, $db_file, 0644) || print "$0: Could not open $db_file for offline db.\n";
    
    $RUN{"Host Node"} = $host;
    $RUN{"a1c Command Line"} = $a1c_cmd;
    $RUN{"Cooking Start Time"} = $time;
    $RUN{"Cooking Start Date"} = "$month $day, $year";
  }    
  print "\n$0 Execute: $a1c_cmd\n";
  
# Run and parse a1c ouput

  open(A1C, "$a1c_cmd 2>&1 |");
    
  while (<A1C>) {
    if ($_ =~ /Wrong number of/) {
      next;
    }
    if ($_ =~ /^# of events:\s*(\d+)/) {

#MYSQL DATABASE INSERTION

	if ($db_mysql) {
	   
           @row = (
		       {
			   name => "eventsReadByA1c",
			   value => $1
			   }
		   );
           UpdateRow(@row);
        }

#PERL DATABASE INSERTION

	else {
          $RUN{"# of events"} = $1;
        }
      } 
      if ($_ =~ /^# of events written:\s*(\d+)/) {
	
#MYSQL DATABASE INSERTION
  
            if ($db_mysql) {
	    
               @row = (
		       {
			   name => "eventsWrittenByA1c",
			   value => $1
			   }
		       );
               UpdateRow(@row);
	  }

#PERL DATABASE INSERTION

	  else {
	    $RUN{"# of events written"} = $1;
	  }
	  if ($1 <= 100) {
	    print "\n******************************************\n";
	    print "\nRunA1c produced a cooked file that contained\n";
	    print "less then 100 events. As a result I will exit.\n";
	    print "Please check email for possible problems. \n";
	    exit;
	  }
	}    
	if ($_ =~ /^cget: I\/O error/) {
	  print "\nGot a cget error. Going to exit job now!\n";
	  exit;
	}
    if ($_ =~ /Program ABORT\./) {
      print "\nGot a program abort message. Exiting now!\n";
      exit;
    }
    print $_; 
  }
  
  close(A1C);

  $t = time();
  $end_time = &ctime($t);
  print  "\nFinished cooking on $host $run_file at ".$end_time."\n";
  
  chop($end_time);
  if ($os eq "Linux") {
    ($doweek, $month, $day, $time, $year) = split(/\s+/, $end_time);
# Added in hopes of removing problem with US/EASTERN years
    if ($year =~ /US/) {
      ($doweek,$month,$day,$time,$zone,$year) = split(/\s+/, $end_time);
    }
  }
  else {
    ($doweek, $month, $day, $time, $zone, $year) = split(/\s+/, $end_time);
  }

#MYSQL DATABASE INSERTION
    
  if ($db_mysql) {

    $a1c_end_time = "NOW()";
    
    @row = (
	  {
	    name => "a1cEndTime",
	    value => $a1c_end_time
	   }
          );

    UpdateRow(@row);
  }

#PERL DATABASE INSERTION

  else{
    $RUN{"Cooking End Time"} = $end_time;
    $RUN{"Cooking End Date"} = "$month $day, $year";
    
    dbmclose(%RUN);
  }
    
# Move the cooked files to the tape silo

  print "Getting a list of cooked files on farm:\n";
  
  $lscmd = "ls -1 run* | ";

  open(LS, $lscmd) || warn "$0: Could not open $lscmd.\n";
  
  if (!$db_mysql) {
    dbmopen(%RUN, $db_file, 0644) || print "$0: Could not open $db_file for offline db.\n";
  }

  while (<LS>) {
    print $_;
    chop;
    if (!$db_mysql) {
      $RUN{"Output Files"} .= "$TapePath/$_ ";
    }
  }
  
  close(LS);
    
  if (!$db_mysql) {
    dbmclose(%RUN);
  }

  if ($TapePath =~ /^\/mss\//) {
      $copy = "jputremote";
    } else {
      $copy = "cp";
    }

  $mvcmd = "$copy run* $TapePath/ | ";
  
  print "\n$0 EXECUTE: $mvcmd\n";

  open(MV, $mvcmd) || warn "$0  Could not open $mvcmd.\n";
  while (<MV>) {
    print $_;
  }
  close(MV);
  
  print "\n\n";
    
}

1;


