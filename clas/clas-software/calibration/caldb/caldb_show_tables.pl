#!/usr/bin/env perl
#
# $Id: caldb_show_tables.pl,v 2.3 2001/05/22 21:08:16 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

if ($verbose) {
    print "database = $database\n";
    print "Tables are:\n"
}

$sql = "SHOW TABLES";

$sth = $dbh->prepare($sql)
        or die "Can't prepare $sql: $dbh->errstr\n";
$rv = $sth->execute
        or die "Can't execute the query $sql\n error: $sth->errstr\n";

# display the results

while (@row = $sth->fetchrow_array) {
    print "$row[0]\n";
}

exit 0;

sub ProcessCommandLine {

    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
    if (!$user){
	$user = "clasuser";
    }
    if (!$password) {
	$password = "";
    }
    if (!$verbose) {
	$verbose = 0;
    }
    if ($help) {
	PrintUsage();
	exit 0;
    }
    if (!$database) {
	print "ERROR: no database name entered\n";
	PrintUsage();
	exit 1;
    }
}

sub PrintUsage {
    print <<EOM;

Shows all tables in the given database. User can choose db server, db
username and password. Default is to connect to clasdb.jlab.org as
clasuser with no password.

usage:

caldb_show_tables.pl database=<database name> \
    [hostname=<hostname of db server>] [user=<db username>] \
    [password=<db password>] [verbose=<non-zero for verbose output>] \
    [help=<non-zero for usage message>]

example:

caldb_show_tables.pl database=calib_user

EOM
    return;
}

# end of perl script
