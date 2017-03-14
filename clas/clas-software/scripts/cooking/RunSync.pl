#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunSync;
}

sub RunSync {

# Runs the monitoring program sync which looks for sync
# events in a raw data file. It parses the output for 
# important database information.
#
# Usage: RunSync($files, $Outpath, $db_file, $basename, $db_mysql)
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
  local($csql_table) = $ENV{"CSQL_TABLE"};
    
  if ($clas_bin eq "") {
    $clas_bin = "/group/clasdev/clas/src/prod-1-5/bin/$os/";
  }
  
  $sync_cmd = $clas_bin."/sync -s10 $files";
  
  print  "\n$0 EXECUTE: $sync_cmd at $start_time\n";

  if ($db_mysql) {
    use DBI;
    $sql = "select last_insert_id()";
    DO_IT();
    @row = $sth->fetchrow_array;
    $read_id = $row[0];
  }
  else{
    dbmopen(%RUN,$db_file, 0644) || print  "$0: Could not open $db_file for offline db\n";
    
    $RUN{"sync command"} = $sync_cmd;
  }
  
  open(SYNC, "$sync_cmd 2>&1 |") || warn "$0: Could not open $sync_cmd\n";
  
  while (<SYNC>) {
    print $_;
    if (/^There were\s+(\d+)\s+bad syncs out of\s+(\d+)/) {
      if ($db_mysql) {
	$sql = "update $csql_table set bad_sync=($1),total_sync=($2) where id=$read_id";
	DO_IT();
      }
      else{
	$RUN{"# of Bad Sync Events"} = $1;
	$RUN{"Total Sync Events"} = $2;
      }
      if ($2 > 0) {
	if ($db_mysql) {
	  $sql = "update $csql_table set frac_bad_sync=($1/$2) where id=$read_id";
	  DO_IT();
	}
	else{
	  $RUN{"Fraction Bad Sync Events"} = $1 / $2 ;
	}
      }
    }
  }
  
  close(SYNC);
  
  if (!$db_mysql) {
    dbmclose(%RUN);
  }

  MoveFile("*sync", $OutPath, "sync file", $db_file);
  
  $t = time();
  $end_time = &ctime($t);
  
  print  "\n$0 FINISHED: $sync_cmd at $end_time\n";
  
  print"\n\n";
  
}

1;

sub DO_IT {

  $sth = $dbh->prepare($sql)
    or die "Can't prepare $sql: $dbh->errstr\n";
  
  $rv = $sth->execute
    or die "Can't execute the query $sql\n error: $sth->errstr\n";
  
  return 0;
}
