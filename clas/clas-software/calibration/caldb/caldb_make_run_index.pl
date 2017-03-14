#! /usr/bin/env perl
##########
# Creates a copy of the RunIndex table in a user-specified database
# with a user-specified name.
# 
# To Do:
#   man page
#   comments
#
# $Id: caldb_make_run_index.pl,v 2.5 2013/08/12 14:58:44 marki Exp $
##########
$big_run = 1000000;
#
# parse the command line arguments
#
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book
$args_ok = 1;
if ($help) {
    &print_usage();
    exit 0;
}
if (!$db) {
    print "INFO: no database name supplied, will use \"calib_user\"\n";
    $db = "calib_user";
}
if (!$table) {
    print "ERROR: table name must be supplied\n";
    $args_ok = 0;
}
if (!$min) {$min = -$big_run}
if (!$max) {$max = $big_run}
if (!$hostname) {
    $hostname = 'clasdb.jlab.org';
    print "INFO: no hostname supplied, will use \"clasdb.jlab.org\"\n";
}
if (!$user) {
    $user = 'clasuser';
    print "INFO: no user supplied, will use \"clasuser\" for write"
	. " connection\n";
}
if ($user ne 'clasuser' && !$password) {
    print "INFO: no password supplied, will not use one for write"
	. " connection\n";
}
if (!$args_ok) {&print_usage(); die "nothing done, invalid arguments\n";}

$mysqldump_command = "mysqldump -h $hostname -u clasuser "
    . "\"--where=minRun <= $max and maxRun >= $min\" calib RunIndex";
$perl_filter = "perl -e \'"
    . "while (<>) {"
    . "    if (/CREATE TABLE/ || /INSERT INTO/ || /LOCK TABLES/ || /DROP TABLE/ || /ALTER TABLE/) {s/RunIndex/$table/;} "
    . "    print;"
    . "}"
    . "\'";

$mysql_command = "mysql -h $hostname -u $user $db";
print "INFO: dump command -> $mysqldump_command\n";
print "INFO: execute command -> $mysql_command\n";
system("$mysqldump_command | $perl_filter | $mysql_command");

exit;

sub print_usage {
    print <<EOM;
usage:
caldb_make_run_index.pl table=<run index table name> [db=<database name>] \\
    [min=<min. run no.>] [max=<max. run no.>] \\
    [hostname=<MySQL server hostname>] [user=<MySQL username>] \\
    [help=<non-zero>]
EOM
    return;
}

# end of perl script
