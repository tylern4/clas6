#!/apps/bin/perl
#
#$Id: file_monitor.pl,v 1.1 2000/08/04 16:56:53 clasg1 Exp $
#
use DBI; # define the database calls
#
# evaluate and check the arguments
#
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book
#
if (!$work_dir) {$work_dir = $wd;}
if (!$work_dir) {
    print "ERROR: work_dir or wd not defined\n";
    &print_usage();
    exit 1;
}
if (!$mss_dir) {$mss_dir = $md}
if (!$mss_dir) {
    print "ERROR: mss_dir or md not defined\n";
    &print_usage();
    exit 1;
}
if (!$size_trigger_copy) {$size_trigger_copy = $st}
if (!$size_trigger_copy) {
    print "ERROR: size_trigger_copy or st not defined\n";
    &print_usage();
    exit 1;
}
if (!$time_since_mod_cut) {$time_since_mod_cut = $tm}
if (!$time_since_mod_cut) {
    print "ERROR: time_since_mod_cut or tm not defined\n";
    &print_usage();
    exit 1;
}
if (!$time_check_period) {$time_check_period = $tc}
if (!$time_check_period) {
    print "ERROR: time_check_period or tc not defined\n";
    &print_usage();
    exit 1;
}
#
# connect to the database
#
$database = "TapeServer"; 
$user = "farmuser"; 
$password =""; 
$hostname = "mss2"; 
$dbh = DBI->connect("DBI:mysql:database=$database;host=$hostname",  
                    $user, $password);
#
# start looping
#
while () {
    system(date);
#
# reset script start time to the current time
#
    $^T = time;
#
# get size of directory
#
    open (FIND, "find $work_dir -type f |");
    $size_total = 0;
    $ifile = 0;
    while ($file = <FIND>) {
	chop $file;
	$size = -s $file;
	$time_since_mod = -M $file; # hours
	$time_since_mod = 24*(3600*$time_since_mod); # seconds
	print "$file, size = $size, time_since_mod = $time_since_mod\n";
	if ($time_since_mod > $time_since_mod_cut) {
	    if ($size > 0) {
		$size_total += $size;
		$file_old[$ifile] = $file;
		$ifile++;
	    } else {
		print "$file has size = 0, ignoring it.\n";
	    }
	}
    }
    $nfile = $ifile;
    print "total size of files = $size_total\n";
    if ($size_total > $size_trigger_copy) {
#
# make list of files
#
	print "check file status for jputting, jput if not ok\n";
	$ncopy = 0;
	$filelist = "";
	for ($ifile = 0; $ifile < $nfile; $ifile++) {
	    $file = $file_old[$ifile];
	    unless (&FILE_OK($mss_dir, $file, "copy")) {
		$ncopy++;
		$filelist .= "  $file \\\n";
	    }
	}
#
# jput 'em
#
	if ($ncopy > 0) {
	    print "jputting\n$filelist";
	    $jput_status = system("jput $filelist $mss_dir");
#######	    $jput_status = system("cp $filelist $mss_dir"); $jput_status = 0; # for testing only
	    print "jput status = $jput_status\n";
	} else {
	    print "no files to jput\n";
	}
#
# check for success then delete 'em
#
	print "check file status for deletion, delete if ok\n";
	for ($ifile = 0; $ifile < $nfile; $ifile++) {
	    $file = $file_old[$ifile];
	    if (&FILE_OK($mss_dir, $file, "delete")) {
		print "deleting $file\n";
		system("rm $file");
	    }
	}
    } else {
	print "not enough bytes to trigger copy...go to sleep...\n";
	system("sleep $time_check_period");
    }
}

exit;

sub FILE_OK {
    local($mss_dir, $file, $copy_or_delete) = @_;
    @field = split(/\//, $file);
    $filename = $field[$#field];
#               fq means fully qualified
    $filefq = "$mss_dir" . "/" . "$filename";
    print "$filefq: ";
    $sql_cmd = "select state,request_id from jput where dest=\'$filefq\' ";
    $sql_cmd .= "order by request_id desc";
    $sth = $dbh -> prepare($sql_cmd) 
	or die "Can't prepare sql:$sql_cmd\nerror:$dbh->errstr\n"; 

    $rv = $sth-> execute;
    if (!$rv) { # try again
	print "Can't execute: $sql_cmd\nerror: $sth-> errstr\nTrying again\n";
	$rv = $sth-> execute
	    or die "Can't execute the query sql:$sql_cmd\nerror: $sth-> errstr\nExiting\n";
    }
    $ok = 0; # assume failure
#           get first row
    @row_ary = $sth->fetchrow_array;
    ($state, $request_id) = @row_ary;
    #print "state = $state, request_id = $request_id\n";
    if ($state eq "Done") {
#       it may be done, but let's check for the stub file to make sure
	if (-e $filefq) {
	    $ok = 1;
	} else {
	    print "done with no stub. ";
	}
    } elsif ($state eq "Staged" || $state eq "Running") {
	# in progress
	$ok = 1;
    } elsif ($state eq "Failed") {
	while (@row_ary = $sth->fetchrow_array) {
	    ($state, $request_id) = @row_ary;
	    #print "state = $state, request_id = $request_id\n";
	    if ($state eq "Done") {$ok = 1;}
	}
    } elsif ($state eq "Pending") {
	if ($copy_or_delete eq "copy") {
	    #print "file is pending, do not copy\n";
	    $ok = 1;
	} elsif ($copy_or_delete eq "delete") {
	    #print "file is pending, do not delete\n";
	} else {
	    die "bad copy_or_delete argument = $copy_or_delete\n";
	}
    } elsif ($state eq "Staging") {
	die "$file staging after put. This cannot happen.\n";
    }
    if (!$state) {$state = "NULL"}
    if (!$request_id) {$request_id = "NULL"}
    print "state = $state, request_id = $request_id, ok = $ok\n";
    $ok_return = $ok;
}

sub print_usage {
    print <<EOM;
    
file_monitor.pl: Checks a directory for recently-unmodified files. If the total
              size of such files is greater than a user-specified amount, the
              files will be jput to the tape silo. Files which are
              successfully jput are deleted. After a user-specified time, this
              process is repeated.

Usage: file_monitor.pl OPTIONS

Options: All options must be specified. Form of specification is
         "option=value".

    work_dir or wd            disk directory to look for files
    mss_dir or md             destination directory in mss stub file tree
    size_trigger_copy or st   total size of files need to trigger a jput, in
	                      bytes
    time_since_mod_cut or tm  time that a file must go unmodified before it
                              will be considered for copying, in seconds
                              (intended to prevent copying a file before it is
                              complete)
    time_check_period or tc   time to wait before checking the directory
                              again, in seconds

Example:
    
file_monitor.pl \\
    work_dir=/work/clas/production/e1d/pass1 \\
    mss_dir=/mss/clas/e1d/production/pass1/data \\
    size_trigger_copy=10.0e9 \\
    time_since_mod_cut=600 \\
    time_check_period=3600

Jputs and then deletes files from /work/clas/production/e1d/pass1 to
/mss/clas/e1d/production/pass1/data when the amount of data exceeds
10 GB, only for files that have been unmodified for 10 minutes. Does this
once an hour.

EOM
    return;
}
