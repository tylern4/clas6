#!/usr/local/bin/perl

# Simeon McAleer 08 - Jan - 1999 Florida State University

# This script will open a file containing a list of run numbers.
# It will then open all of the database files corresponding it those
# runs in the database directory specified and calculate a sum over
# the specified entry. 
# 
# Usage : totaldb.pl -i input file -d database_dir -n "category name"
# 

if (@ARGV[0] =~ /-h/i) {
    &print_help && exit;
}

for ($i=0; $i < $#ARGV; $i++) {
    if ($ARGV[$i] =~ /-i/i) {
	$input_file = $ARGV[$i+1];
	splice(@ARGV, $i, 2);
	last;
	}
}

for ($i=0; $i < $#ARGV; $i++) {
    if ($ARGV[$i] =~ /-d/i) {
	$database_dir = $ARGV[$i+1];
	splice(@ARGV, $i, 2);
	last;
	}
}

for ($i=0; $i < $#ARGV; $i++) {
    if ($ARGV[$i] =~ /-n/i) {
	$entry = $ARGV[$i+1];
	splice(@ARGV, $i, 2);
	last;
	}
}

print "\nThe database is $database_dir\n";
print "\nThe file is $input_file\n";
print "\nThe category is $entry\n";

$total = 0;

open(IN, "$input_file") || die "\ntotaldb.pl: Could not open $input_file\n";

while (<IN>) {
    chop;
    $run = $_;
    print "\nWorking on  run number $run\n";
    if (!($run =~ /(\d)+/)) {
	next;
    }

    $ls_cmd = "ls -1 $database_dir/*$run* |";
    open(LS, "$ls_cmd") || die "\ntotaldb.pl: Could not perform $ls_cmd\n";
    while (<LS>) {
	chop;
	$db_file = $_;
	$db_file =~ s/\.db//;
	dbmopen(%DB, "$db_file", 0644) || warn "\ntotaldb.pl: Could not open $db_file\n";
	$total += $DB{"$entry"};
	dbmclose(%DB);
    }
}

print "The total sum for $entry for the runs in $input_file is $total.\n";



#for ($i = 0; $i <= $#ARGV; $i++) {
#    print $ARGV[$i];
#    $file = $ARGV[$i];
#    $file =~ s/\.db//;
#    print $file."\n";
#    dbmopen(%RUN, "$file", 0644);
#    $events = $events + $RUN{"ine epi+pi-p"};
#    print "Num events is now ".$events."\n";
#    dbmclose(%RUN);
#}



sub print_help {

    print "\nUsage:totaldb.pl -i input file -d database_dir ";
    print "-n 'category name'"; 

}
