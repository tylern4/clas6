#!/usr/local/bin/perl
# $Id : $
#        R A Thompson
#        University of Pittsburgh
#        August 5, 1998
#
# this script takes a list of run numbers from stdin and prints to stdout the list of cooked data
# files corresponding to those run numbers

# -------------- defaults --------------
$Pass = 1;

for (@ARGV) {
    if(/^-h/)      {
	&PrintUsage;
        exit;
    }
    if(/^-P(.+)/) { $Pass = $1;}            # analysis pass
}

$StubPath = "/mss/clas/e1a/production/v1.$Pass";
if ($Pass == 3){
    $StubPath = $StubPath."/cooked";
}

while (<STDIN>) {
    chop;
    $RunNo = $_;
    $cmd = "ls -1 $StubPath/*$RunNo* |";
    open(F,$cmd) || die "Could not execute\n";
    while (<F>) {
	print"$_";
    }
}
	
sub PrintUsage {
    print "input (stdin) : a list of run numbers
output (stdout) : a list of cooked data files corresponding to those run numbers
options:
    -h     print this Help
    -P#    analysis pass\n"
    }
