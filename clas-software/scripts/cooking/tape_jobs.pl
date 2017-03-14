#!/apps/bin/perl

# $Id: tape_jobs.pl,v 1.10 2004/06/09 20:02:11 clarisse Exp $
use DBI; # define the database calls

#########################################################################
# Modified version of Mark Ito's tape_jobs.pl script found under CVS in #
# tools/misc. This version uses most of the old setup for the original  #
# cooking_scripts. The major changes that will occur are that the you   #
# almost all of the arguments get passed to RunJob automatically.       #
# Second the whole setup goes from being run based to being file based. #
#                                                                       #
# Modified by McAleer on July 12, 2000                                  #
#########################################################################

#
# parse the command line arguments
#
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book
if (!$file_spec) {
    print "ERROR: file_spec not defined\n";
    &print_usage();
    exit 1;
}
if (!$script) {print "ERROR: script not defined\n"; exit 1;}
if (!$run_list) {print "INFO: run_list not specified,"
		     . " no run selection will be done\n";}
if (!$volume_list) {print "INFO: volume_list not specified,"
			. " no volume selection will be done\n";}
if (!$run_list && !$volume_list) { print "I can't run without either a run_list or volume set\n"; exit 1}

if(!$env) {
  print "You probably will not want to run the cooking scripts without an environment file\n";
  exit 1;
}

if (!$options) {print "Without a set of options the RunJob script is going to FAIL\n"; exit 1}

print "Options to be used: $options\n";

if(!$ext) {print "INFO: No limits put on file extensions\n";}

#########################################################
# Begin some of special cooking changes                 #
#########################################################

print "\n$0: Going to use file $env  to set environment variables.\n\n";

open(ENV, "$env") || die "\n$0: Could not open environment variable text template.\n";

while(<ENV>) {
    if ($_ =~ /^#/) {
	next;
    } elsif ($_ =~ /\s*(\S+)\s+(\S+)/) {
	$ENV{$1} = $2;
    }
}

close(ENV);

#######################################################
# Get rid of possible extraneous ending / marks on environment variables
#######################################################

@env_vars = ("CLAS_BIN","CLAS_PARMS","CLAS_SCRIPTS","CLAS_DB", "CLAS_OUT_PATH","CLAS_TAPE_PATH","CLAS_DATA");

foreach (@env_vars) {
  if ($ENV{"$_"} =~ /\/$/) {
    $ENV{"$_"} =~ s /\/$//;
  }
}

$dbdir = $ENV{"CLAS_DB"};

if ($dbdir eq "") { 
    print "\n$0: Can not run without a database directory identified.\n";
    print "\nPlease specify a database directory using \n\n";
    die "CLAS_DB in the ENV_SRC_FILE\n\n";
}

require "$ENV{'CLAS_SCRIPTS'}/CheckDir.pl";

CheckDir($dbdir);
CheckDir($ENV{"CLAS_OUT_PATH"});
if (!($ENV{"CLAS_TAPE_PATH"} =~ /^\/mss/)) {
  CheckDir($ENV{"CLAS_TAPE_PATH"});
}

#########################################################
# End some of special cooking changes                                                                     #
#########################################################


#
# connect to the tape server database
#
$database = "Store"; 
$user = "farmuser"; 
$password ="farmuser"; 
$hostname = "mssdb1"; 
$dbh = DBI->connect("DBI:mysql:database=$database;host=$hostname",  
		    $user, $password);
#
# start constructing the query
#
$sql_cmd = "select CONCAT(full_path, '/', stub_filename),vol_name,file_position from stub, dir, vol, bf where";
$sql_cmd .= " stub.dir_index = dir.dir_index AND stub.bitfile_index = bf.bitfile_index AND bf.vol_index = vol.vol_index AND";
$sql_cmd .= " CONCAT(full_path, '/', stub_filename) like \"$file_spec\"";
#
# if run_list is defined, require that the file name contain one of the runs
#
if ($run_list) {
    @run = split(/,/, $run_list);
    for ($i = 0; $i <= $#run; $i++) {
	if ($i == 0) {$sql_cmd .= " and ( ";}
	else {$sql_cmd .= " or ";}
	$sql_cmd .= "stub_filename like \"%$run[$i]%\"";
    }
    $sql_cmd .= " )";
}
#
# if volume_list is defined, require that the file comes from one of
# the specified volumes
#
#Rafael
if ($volume_list) {
#    @volume = split(/,/, $volume_list);
    @volName = split(/,/, $volume_list);
#    for ($i = 0; $i <= $#volume; $i++) {
    for ($i = 0; $i <= $#volName; $i++) {
	if ($i == 0) {$sql_cmd .= " and ( ";}
	else {$sql_cmd .= " or ";}
#	$sql_cmd .= "vol_name = $volume[$i]";
	$sql_cmd .= "vol_name = $volName[$i]";
    }
    $sql_cmd .= " )";
}
#
# if extension list is defined, require that the file contains one of the 
# specified file extensions
#
if ($ext) {
  @extensions = split(/,/, $ext);
  for ($i = 0; $i <= $#extensions; $i++) {
	if ($i == 0) {$sql_cmd .= " and ( ";}
	else {$sql_cmd .= " or ";}
	$sql_cmd .= "stub_filename like \"%$extensions[$i]%\"";
    }
    $sql_cmd .= " )";
}
#
# finish construction the query
#
$sql_cmd .= " order by vol_name,file_position";
print "sql_cmd = $sql_cmd\n";
#
# prepare and execute
#
$sth = $dbh -> prepare($sql_cmd) 
    or die "Can't prepare sql:$sql_cmd\nerror:$dbh->errstr\n"; 
$rv = $sth-> execute 
    or die "Can't execute the query sql:$sql_cmd\nerror: $sth-> errstr\n";
#
# get the results and construct the jcache command
#
print "Selected files:\n";
$jget_command = "jcache \\\n";
$nfile = 0;
while (@row_ary = $sth->fetchrow_array) {
    $silo_file = $row_ary[0];
#    $volume = $row_ary[1];
    $volName = $row_ary[1];
    $location = $row_ary[2];
    
    $run_file = $silo_file;
    $path = $file_spec;
    $path =~ s/[a-zA-Z0-9_\.%]*$//;
    $run_file =~ s/$path(\/*)//;
    if (&TestFile($run_file)) {
      $jget_command .= "  $silo_file \\\n";
      $silo_file =~ s/\/mss/\/cache/;
      $cache_file[$nfile] = $silo_file;
#      print "$cache_file[$nfile] from tape $volume, file # $location\n";
      print "$cache_file[$nfile] from tape $volName, file # $location\n";
      $nfile++;
    } else {
      print "$run_file was already submitted. Submit again?(yes/no)\n";
      $response = <STDIN>;
      chop $response;
      if ($response eq "yes" || $response eq "y") {
	$jget_command .= "  $silo_file \\\n";
	$silo_file =~ s/\/mss/\/cache/;
	$cache_file[$nfile] = $silo_file;
#	print "$cache_file[$nfile] from tape $volume, file # $location\n";
	print "$cache_file[$nfile] from tape $volName, file # $location\n";
	$nfile++;
      } else {
	print "Going on to the next file then.\n";
      }
    }
  }
$jget_command .= "\n";
print "number of files to process = $nfile\n";
print STDERR $jget_command;
#
# ask if jcache is to be submitted and submit if answer is yes
#
print "Do you want to submit the tape request? (yes or no): ";
$response = <STDIN>;
chop $response;
if ($response eq "yes" || $response eq "y") {
  system($jget_command);
  print "Updating the cache database\n";
  for ($i = 0; $i < $nfile; $i++) {
    $dbfile = $cache_file[$i];
    $dbfile =~ s/\/cache/\/mss/;
    $dbfile =~ s/$path//;
    dbmopen(%DB, "$dbdir/Data_Files.db", 0644) || print  "$0: Could not open $dbdir/Data_Files.db for caching\n";
    $DB{"$dbfile"} = "Caching";
    dbmclose(%DB);
  }
}

print "Do you want to start the jobs? (yes or no): ";
$response = <STDIN>;
chop $response;
if ($response eq "yes" || $response eq "y") {
  
  open(CRON, ">$dbdir/submit_cron") || die "Could not open cron job\n";
  
  $path =~ s/mss/cache\/mss/;
  $path =~ s#//#/#;
  print CRON "0,10,20,30,40,50 * * * * $ENV{'CLAS_SCRIPTS'}/submit_cached_files.pl script=$script path=$path env=$env options='$options'\n";

  close(CRON);

  print "Submitting crontab $dbdir/submit_cron\n";
  system("crontab $dbdir/submit_cron");
}

exit 0;

sub print_usage {
    print <<EOM;

tape_jobs.pl: Selects a set of silo files by filename pattern, tape
              volume and run number, executes a jcache for those
              files, and starts cron job for  a user-supplied script.

Usage: tape_jobs.pl \\
           file_spec=<stub file spec> \\
           script=<user-supplied script> \\
           env=<ENV_SRC_FILE> \\
           [run_list=<list of runs>] \\
           [volume_list=<list of volumes>] \\
           [ext=<list of file extensions>] \\
           [options='<user-supplied script options>']

Notes: 1. Use the percent sign (%) as a wildcard in file_spec.
       2. run_list, ext, volume_list are comma (,) delimited lists, no spaces

Example:

tape_jobs.pl \\
    file_spec=/mss/clas/e1c/data/clas_0%.A% \\
    script=e1_pass1_job.pl \\
    env=ENV_SRC_FILE \\
    run_list=17135,17136,17137 \\
    volume_list=002279,002032 \\
    ext=A00,A01
    options='+base prod19 +P0x7'

EOM
    return;
}

sub TestFile{
  
  local($file) = $_[0];
  local($return_val) = 1;
  
  if (-e "$dbdir/done" && -r "$dbdir/done") {
    open(D,"$dbdir/done") || die "\n$0: Could not read $dbdir/done list.\n";
    while (<D>) {
      chop;
      $pfile = $_;
      if ("$file" eq "$pfile") {
	print STDERR "File $file has already been submitted\n";
	$return_val  = 0;
	close(D);
      } 
    }
    close (D);
  } else {
    print "\n I will submit the run but I never found a done list.\n";
  }

  return $return_val;
  
}

