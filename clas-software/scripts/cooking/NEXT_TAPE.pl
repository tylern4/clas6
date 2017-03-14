#!/apps/bin/perl

#default values for pass0_v1 of g7 running
$prefix="clas_0";
$postfix="";
$run_min=34630;
$run_max=34900;
$^I=".old";
$pattern="A00";

#default value for pass0_v1 of g7 running.  
$env_src_file="/work/clas/pdisk4/clarisse/pass0/v1/scripts/ENV_SRC_FILE";

# process arguments
for ($i = 0; $i <= $#ARGV; $i++) {
  $arg = $ARGV[$i];
  if ($arg =~ /^-.*/) {
    push(@opts, $arg);
  }
  if ($arg eq "+h" || $arg eq "-h") {
    &PrintUsage;
    exit(0);
  }
  elsif ($arg =~ /^\+(\w*)/) {
    if ($1 =~ /^env/) {
      $i++;
      $env_src_file = $ARGV[$i];
    }
    if ($1 =~ /^pre/) {
      $i++;
      $prefix = $ARGV[$i];
    }
    if ($1 =~ /^post/) {
      $i++;
      $postfix = $ARGV[$i];
    }
    if ($1 =~ /^match/) {
      $i++;
      $pattern = $ARGV[$i];
    }
    if ($1 =~ /^r/) {
      $i++;
      $run_min = $ARGV[$i];
    }
    if ($1 =~ /^R/) {
      $i++;
      $run_max = $ARGV[$i];
    }
    else {
      # @arr not used yet.
      push(@arr, $arg);
    }
  }
}

if ($env_src_file eq ""){
  &PrintUsage;
  exit(0);
}

if ($os eq "") { $os = $ENV{'OSCLAS'} }

# Parse through environmental variable template file
print "\n$0: Going to use file $env_src_file to set environment variables.\n\n";
open(ENV, "$env_src_file") || die "\n$0: Could not open environment variable text template.\n";
while(<ENV>) {
  if ($_ =~ /^\#/) {
    next;
  } elsif ($_ =~ /\s*(\S+)\s+(\S+)/) {
    $ENV{$1} = $2;
  }
}
# Get some environmental variables
$host = $ENV{"HOSTNAME"};
$home = $ENV{"HOME"};
$os  = $ENV{"OSNAME"};
# Get rid of possible extraneous ending / marks
@env_vars = ("CLAS_BIN","CLAS_PARMS","CLAS_SCRIPTS","CLAS_DB", "CLAS_OUT_PATH","CLAS_TAPE_PATH");
foreach (@env_vars) {
  if ($ENV{"$_"} =~ /\/$/) {
    $ENV{"$_"} =~ s /\/$//;
  }
}
# Finished removing extraneous ending / marks
$clas_bin = $ENV{"CLAS_BIN"};
$clas_parms = $ENV{"CLAS_PARMS"};
$perl_lib = $ENV{"CLAS_SCRIPTS"};
$clas_db = $ENV{"CLAS_DB"};
$OutPath = $ENV{"CLAS_OUT_PATH"};
$TapePath = $ENV{"CLAS_TAPE_PATH"};
$SourcePath = $ENV{"CLAS_DATA"};
$ENV{"CLAS_BIN"} .= "/$os";
#####################################################

$script_dir="$perl_lib";
#$data_dir="/mss/clas/g1c/data";
$data_dir="$SourcePath/";
#$data_dir=~s/cache/mss/;

# $out_dir is never really used except as a dummy for get_tape_info. 
$out_dir="/work/clas/pdisk4/clarisse/pass0/v1/database/";

$database_done="$clas_db/done_tape";
$database_list="$clas_db/list";
$tape_script="$script_dir/tape_jobs.pl";
$file_spec="$data_dir$prefix%$pattern%";
$script="$script_dir/RunJob";
$get_tape_info_cmd="$script_dir/get_tape_vol.pl -d$data_dir -b$prefix";
$get_tape_info_cmd="$get_tape_info_cmd -l$out_dir -r$run_min -R$run_max";
#open list of already completed tape volumes
open(DONE_LIST, "<$database_done")||print "creating new $database_done\n";
while (<DONE_LIST>){
  chomp;
  $_=~s/\s//;
  push(@donelist,$_);
}
close(DONE_LIST);
#open list of run numbers to be analyzed
open(RUN_LIST, "<$database_list")||print "creating new $database_list\n";
while (<RUN_LIST>){
  chomp;
  $_=~s/\s//;
  push(@good_run_list,$_);
}
close(RUN_LIST);

print "gtic: $get_tape_info_cmd\n";

# fill array with file extension patterns
@patt=split(/:/,$pattern);

# run get_tape_vol.pl script to list files on mss with tape volume number
open(JGET_LIST,  "$get_tape_info_cmd |") || die "couldn't get jget list";
$newtape=0;  # new tape volume flag
$first=0;    # flag to check if tape is done
$nfiles=0;   # file counter
$notfound=0; # flag if tape is to be search for files
$moretapes=1; # flag to stop looking for more tapes
while(<JGET_LIST>){    
  print $_;
#deal with jget lines and find tape volume numbers
  if($_ =~ /$outdir/ ){  # reinitial the flags
    $newtape=0;
    $first=0;
    $notfound=0;
  }
  if($_ =~ /volName/){  # set flags if volName found
    @line=split(/:/,$_);
    $volName=$line[1];
    chomp($volName);
    $newtape=1;
    $first=1;
  }
  if($newtape){    # enter if tape exists
    if($first){    # check once if tape is on the done_list 
      if($moretapes){ # if more tapes to check, continue with loop
	if($#donelist>=0){ # check for completed tape volume numbers 
	  foreach $done (@donelist){ # loop over completed tape numbers
	    if($done =~ /$volName/){
	      $notfound=0;
	      last;
	    }
	    else{
	      $notfound=1;
	    }
	  }
	}
	else{ # if donelist is empty
	  $notfound=1;
	}
      }
      else{ # if moretapes=0, ie tape has been found,stop searching
	$notfound=0;
      }
      $first=0;
    } 
    if($notfound){   # if tape is not on done_list, check for files
      foreach $filepatt (@patt){
	if($_=~ /$filepatt/){    # look for file pattern (A,A##,etc)
	  #     if($_=~ /$pattern/){    # look for file pattern (A,A##,etc)
	  $_=~ s/\\//;
	  chomp;
	  # remove path info and file extention to get run number
	  $_=~ s/$data_dir//;
	  $_=~ s/$prefix//;
	  $_=~ s/\s//g;	    
	  $_=~ s/\.//g;
	  $_=~ s/$filepatt.*//g; # remove everything after pattern
	  $run=$_;    # set run number
	  foreach $good_run (@good_run_list){
	    if($good_run =~ /$run/){
	      push(@run_list,$run);
	      $good_volume=$volName; # set good tape volume number
	      $moretapes=0; # stop looking for new tapes
	      $nfiles++; # number of files found
	    }
	  }
	  if($run_list[$#run_list]!=$run){
	    print STDERR "$run not on list\n";
	  }
	}
      }
    }
  }
}

$tape_cmd="$tape_script \\\n  file_spec=$file_spec \\\n  script=$script \\\n  env=$env_src_file \\\n";
$options="\'+d +sp +mysql\'";

$tape_cmd="$tape_cmd  run_list=";
foreach $run(@run_list){
  $tape_cmd="$tape_cmd$run,";
}
chop($tape_cmd);
$tape_cmd="$tape_cmd \\\n";

$tape_cmd="$tape_cmd  volume_list=$good_volume \\\n";
$tape_cmd="$tape_cmd  ext=$pattern \\\n  options=$options";

print "Number of files found : $nfiles\n";
print "The run numbers are @run_list\n";

if($nfiles && $#run_list>=0){
  print "TAPE_CMD $tape_cmd\n";
  print STDERR "execute command?\n";
  if(<STDIN>=~/^[yY]/)
    {
      print "executing...\n";
      system($tape_cmd);
      open(DONE_LIST, ">>$database_done") || die "couldn't open tape done list:$database_done\n";	
      print DONE_LIST "$good_volume\n";
    } 
  else {
  }
}
else{
  print "next tape contained no $pattern files\n";
  print "mark tape volume as done?\n";
  if(<STDIN>=~/^[yY]/){
    open(DONE_LIST, ">>$database_done") || die "couldn't open tape done list:$database_done\n";	
    print DONE_LIST "$good_volume\n";
  }
}

sub PrintUsage {
  print "\nUsage:\n    $0 [+h] +env <env_src_file> [+pre <prefix>][+post <postfix>][+r <run_min>][+R <run_max>] [+match <pattern>]\n\n";
  print "\t+env <env_src_file> \t File name(w/path) of environment variable text file.\n";
  print "\t+pre <prefix> \t prefix of file_names in data directory.\n";
  print "\t+post <postfix> \t postfix added to output files.\n";
  print "\t+match <pattern> \t require <pattern> to be in file name. default is A01. \n";
  print "\t+r <run_min> \t set minimum run number to <run_min>.\n";
  print "\t+R <run_max> \t set maximum run number to <run_max>.\n";
  return;
}
