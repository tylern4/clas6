#!/usr/bin/env perl

# this script is known to work on Linux and SunOS CUE.  Computer Center
# staff is working on getting perl up to date on AIX and HP's

use DBI;

if ($ARGV[0] =~ /^.h/ || $ARGV[0] =~ /^.H/) {
    &PrintUsage;
    exit(0);
}

# Set default variables
$run_min = 1;
$run_max = 999999;

# connect to MYSQL database on claspc10

foreach (@ARGV) {
  if (/^-R(\d+)/) {
    $run_max = $1;
  } elsif (/^-r(\d+)/) {
    $run_min = $1;
  } elsif (/^-d(.*)/) {
    $dir = $1;
  } elsif (/^-b(.*)/) {
    $base = $1;
  } elsif (/^-l(.*)/) {
    $dest = $1;
  }
}

if ($dir =~ /\/$/) {
  chop($dir);
}

if (($dir eq "") || ($base eq "") || ($dest eq "")) {
  die  "\n$0: Directory, basename, or destination not specified. Please try again\n";
}

print STDERR "\n$0: Using lower run limit $run_min.\n";
print STDERR "$0: Using upper run limit $run_max. \n";
print STDERR "$0: Locating files in MSS directory $dir.\n";
print STDERR "$0: Searching for file names with the base $base.\n";
print STDERR "$0: Placing retrieved files in directory $dest.\n\n";

# Adjust maximum run number for conditional

$run_max++;

$database = 'Store';
$user = "farmuser";
$password = "farmuser";
$hostname = "mssdb1";

print STDERR "$0: MSQL connction stuff: \n";
print STDERR "---------------------- \n";

$dbh = DBI->connect("DBI:mysql:database=$database;host=$hostname",
                    $user, $password);

if (defined $dbh) {
    print STDERR "Connection successful: handle: $dbh\n";
} else {
    die "Could not make database connect...yuz got problems...\n";
}

# construct the SQL query

$sql  = "SELECT ";
$sql .= "  CONCAT(full_path, '/', stub_filename), vol_name, file_position ";
$sql .= "FROM ";
$sql .= "  stub, dir, vol, bf ";
$sql .= "WHERE ";
$sql .= "  stub.dir_index = dir.dir_index AND stub.bitfile_index = bf.bitfile_index AND bf.vol_index = vol.vol_index AND";
$sql .= "  full_path =  '$dir'  AND";
$sql .= "  stub_filename >=  '$base"."$run_min%'  AND";
$sql .= "  stub_filename <=   '$base"."$run_max%' ";
$sql .= " order by vol_name, file_position";



print STDERR "\n$0: Using sql command:\n";
print STDERR "--------------------\n";
print STDERR $sql."\n\n";

# prepare and execute the query 

$sth = $dbh->prepare($sql)
    or die "Can't prepare $sql: $dbh->errstr\n";

$rv = $sth->execute
    or die "Can't execute the query $sql\n error: $sth->errstr\n";

# retrieve the rows of data and sum up the number of events

$first =1; 
$old_volName = "";

while (($stubPath, $volName) = $sth->fetchrow_array) {
    if ($first) {
      print "volName:$volName\n";
      print " jget \\\n";
      $old_volName = $volName;
      $first = 0;
    }
    if (($volName ne $old_volName)) {
      print "  $dest\n";
      print "\nvolName:$volName\n";
      print "jget \\\n";
      $old_volName = $volName;
    }
    print "  $stubPath \\\n";
}
print "  $dest\n";

print "\n";

sub PrintUsage {

  print "\nUsage: $0 -h\n";
  print "\t-h \tPrint this help message.\n";
  print "\t-r# \tLower limit for run number range.\n";
  print "\t-R# \tUpper limit for run number range.\n";
  print "\t-d<string> \tMSS directory where files reside.\n";
  print "\t-l<string> \tFinal location for files once found.\n";
  print "\t-b<string> \tInitial basename of file up to the run number.\n";
  print "\n";
  print "Example: $0 -r16833 -R16835 -d/mss/clas/e1b/production/pass2.x/prod19/cooked/ -brun\n";
  print "\t-l/work/clas/disk2/mcaleer\n";
  print "\n";
}







