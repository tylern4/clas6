#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunG_filter;
}

sub RunG_filter {

# Runs the filtering program g_filter. It parses the
# output for important database information.
#
# Usage: RunG_filter($files, $Outpath, $db_file, $basename, $db_mysql)
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
  
  $gfilter_cmd = "$clas_bin/g_filter -i -R1 -orun$basename.kaon -m0.0625 -M0.64 $files";

  print "\n$0 EXECUTE: $gfilter_cmd at $start_time\n";

  
  if ($db_mysql) {
    use DBI;
    $sql = "select last_insert_id()";
    DO_IT();
    @row = $sth->fetchrow_array;
    $read_id = $row[0];
  }
  else{
    dbmopen(%RUN,$db_file, 0644) || print "$0: Could not open $db_file for offline db\n";
    
    $RUN{"g_filter command"} = $gfilter_cmd;
  }

  open(FILT, "$gfilter_cmd 2>&1 |") || warn "DB: Could not open $gfilter_cmd.\n";

  while (<FILT>) {
    print $_;
    if ($_ =~ /\# of events written:\s+(\d+)/) {
      if ($db_mysql) {
	$sql = "update $csql_table set gfilt_K=($1) where id=$read_id";
	DO_IT();
      }
      else{
	$RUN{"kaon filtered events"} = $1;
      }
    }
    
    if ($_ =~ /hadron mass squared cut\(s\)/) {
      $line = <FILT>;
      print $line;
      if ($line =~ /^Lower/) {
	$line =~ /\s+.*:\s+(\d+.\d+)/;
	if ($db_mysql) {
	  $sql = "update $csql_table set gfilt_MM2min_cut=($1) where id=$read_id";
	  DO_IT();
	}
	else{
	  $RUN{"gfilter hadron mass min cut"} = $1;
	}
      }
      $line = <FILT>;
      print $line;
      if ($line =~ /^Upper:/) {
	$line =~ /\s+.*:\s+(\d+.\d+)/;
	if ($db_mysql) {
	  $sql = "update $csql_table set gfilt_MM2max_cut=($1) where id=$read_id";
	  DO_IT();
	}
	else{
	  $RUN{"gfilter hadron mass max cut"} = $1;
	}
      }
    }
  }
    
  close(FILT);

  if (!$db_mysql) {
    dbmclose(%RUN);
  }

  MoveFile("run*kaon", $OutPath, "gfiltered file", $db_file);
  
  $t = time();
  $end_time = &ctime($t);
  
  print "\n$0 FINISHED: $gfilter_cmd at $end_time\n";

  print "\n\n";

}

1;


sub DO_IT {
  
  $sth = $dbh->prepare($sql)
    or die "Can't prepare $sql: $dbh->errstr\n";
  
  $rv = $sth->execute
    or die "Can't execute the query $sql\n error: $sth->errstr\n";

  return 0;
}
