#!/usr/bin/env perl
#
# $Id: caldb_drop_table.pl,v 2.1 2001/11/15 21:18:15 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

print "INFO: Dropping table $table from the calib_user database on"
    . " $hostname.\n";
print "Is this OK? (yes/no): ";
$answer = <>;
chomp $answer;
if ($answer ne 'yes') {
    print "INFO: exitting at user request.\n";
    exit 0;
}

$sql = "DROP TABLE $table";

$sth = $dbh->prepare($sql)
        or die "Can't prepare $sql: $dbh->errstr\n";
$rv = $sth->execute
        or die "Can't execute the query $sql\n error: $sth->errstr\n";

print "INFO: Table dropped.\n";

exit 0;

sub ProcessCommandLine {

    if ($help) {
	PrintUsage();
	exit 0;
    }
    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
    $user = 'clasuser';
    $password = '';
    $database = 'calib_user';
    $verbose = 0;
    if (!$table) {
	print "ERROR: no table name defined\n";
	PrintUsage();
    }
}

sub PrintUsage {
    print <<EOM;
Purpose:
    Drops a table from the calib_user database.

usage:
    caldb_drop_table.pl table=<table name> \\
        [hostname=<hostname of db server>] [help=<non-zero for usage message>]

example:

caldb_drop_table.pl table=RunIndexJunk

EOM
    return;
}

# end of perl script
