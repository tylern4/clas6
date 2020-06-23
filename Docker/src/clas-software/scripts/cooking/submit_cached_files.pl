#!/apps/bin/perl

# This script takes as an argument a set of parameters 
# for jobs to be run on the farm. It then checks the database 
# for files that appear on the cache disk. If the file status is 
# cached that file is then submitted to the farm. If the status is
# anything else the file is ignored.

# Parse the command line arguments
print "In submit_cached files!\n";
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

if (!$script) {
  print "ERROR: script not defined\n"; 
  &print_usage;
  exit 1;
}
  
if (!$path) {print "ERROR: Don't know where the files are located\n"; exit 1;}

if (!$options) {print "Without a set of options the RunJob script is going to FAIL\n"; exit 1}

if(!$env) {
  print "You probably will not want to run the cooking scripts without an environment file\n";
  exit 1;
}

if ($script =~ /RunJob/) {
  $options .= " +env $env";
}

# End of parsing command line
print "End of parsing command line.\n";

# Begin parsing through environment template file

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

print "Close env\n";
#######################################################
# Get rid of possible extraneous ending / marks on environment variables
#######################################################

@env_vars = ("CLAS_BIN","CLAS_PARMS","CLAS_SCRIPTS","CLAS_DB", "CLAS_OUT_PATH","CLAS_TAPE_PATH","CLAS_DATA");

foreach (@env_vars) {
  if ($ENV{"$_"} =~ /\/$/) {
    $ENV{"$_"} =~ s /\/$//;
  }
}

print "End cosmetics\n";
#######################################################
# Let's get down to business 
#######################################################

$dbdir = $ENV{"CLAS_DB"};

$user = $ENV{"LOGNAME"};
$name = $ENV{"CLAS_MAIL"};
print "Before dbmopen $dbdir \n";
dbmopen(%DB, "$dbdir/Data_Files.db",  0644) || print  "$0: Could not open $dbdir/Data_files for submitting files.\n";

$stop = 1;

foreach $file (keys %DB) {
  $status = $DB{"$file"};
  print "$file has value of $status\n";
  $cache_file = "$path$file";
  if ($status eq "Caching" && -e "$cache_file") {
    $DB{"$file"} = "Submitted";
    open(DONE, ">>$dbdir/done");
    print DONE "$file\n";
    close(DONE);
    $run = $file;
    $run =~ s/clas_0//;
    $run =~ s/\.A\d\d//;
    $fn  = $file;
    $fn  =~ s/clas_\d+\.A//;
	
    $submit_file = "submit$run.$fn";
    print " submit $run\n";
    open(SUBFILE, ">$dbdir/$submit_file") || die "Could not open submit file\n";
    print SUBFILE "PROJECT: clasG7\n";
    print SUBFILE "JOBNAME: Run$run.$fn\n";
    print SUBFILE  "COMMAND: $script \n";
    print SUBFILE  "OS: Linux\n";
    print SUBFILE  "MAIL: $name \n";
    print SUBFILE  "OPTIONS: $run $options \n";
    print SUBFILE  "INPUT_FILES: $cache_file \n";
    close(SUBFILE);
    print "PROJECT: clasG7\n";
    print "JOBNAME: Run$run.$fn\n";
    print "COMMAND: $script \n";
    print "OS: Linux(RH6)\n";
    print "MAIL: $name \n";
    print "OPTIONS: $run $options \n";
    print "INPUT_FILES: $cache_file \n";
    print "\njsub $dbdir/$submit_file\n";
    $jsub_cmd = "/site/bin/jsub $dbdir/$submit_file |";
    open(JSUB, "$jsub_cmd") || print "Could not open jsub command\n";
    while (<JSUB>) {
      print $_;
    }
    close(JSUB);
  }
  
  # Flag to continue crontab if files are still caching

  $status = $DB{"$file"};
  if ($status eq "Caching") {
    $stop = 0;
  }

}

dbmclose(%DB);

if ($stop) {
  system("crontab -r");
}

sub print_usage {
  print <<EOM;

submit_cached_files.pl: Examines the PERL database files for the cached  
        data files. If the file exists on the cache disk a job is submitted 
        to the batch farm. 

Usage: submit_cached_files.pl \\
           path=<location of files> \\
           script=<user-supplied script> \\
           env=<ENV_SRC_FILE> \\
           [options=<user-supplied script options>]

Example:

submit_cached_files.pl \\
    path=/cache/clas/e1c/data/
    script=RunJob \\
    env=ENV_SRC_FILE \\
    options=+base prod19 +P0x1

EOM
    return;
}

