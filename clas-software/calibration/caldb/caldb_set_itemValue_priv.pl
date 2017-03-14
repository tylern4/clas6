#!/usr/bin/env perl
# Just prints out the commands, does not execute them.
#
# $Id: caldb_set_itemValue_priv.pl,v 2.4 2002/11/12 21:45:57 marki Exp $
#
use DBI;
#
# set connection parameters
#
$database = "mysql";
$user = "dbmanager";
$hostname = "localhost";
#
# get the password
#
print "password: ";
system("stty -echo");
chop($password=<STDIN>);
print "\n";
system("stty echo");
#
# connect
#
$dbh = DBI->connect("DBI:mysql:$database:$hostname", $user, $password);

if (defined $dbh) {
    print "Connection successful: handle: $dbh\n";
} else {
    die "Could not connect to $database at $hostname as $user\n";
}
#
# get all items
#
$sql  = "SELECT systemName, subsystemName, itemName";
$sql .= " FROM calib.System LEFT JOIN calib.Subsystem USING (systemId)";
$sql .= " LEFT JOIN calib.Item USING (subsystemId)";
DoIt();

while ($row = $sth -> fetchrow_hashref) {
    $table_name = $$row{systemName} . '_' . $$row{subsystemName};
    $table_name .= '_' . $$row{itemName};
    $sql = "INSERT INTO tables_priv (Host, Db, User, Table_name, Table_priv)";
    $sql .= " VALUES ('%', 'calib', 'clasuser',";
    $sql .= " '$table_name', 'Select,Insert');";
    print "$sql\n";
}

exit 0;

#############################################

sub DoIt {
# prepare and execute the query 

$sth = $dbh->prepare($sql)
    or die "Can't prepare $sql: $dbh->errstr\n";

$rv = $sth->execute
    or die "Can't execute the query $sql\n error: $sth->errstr\n";
}

# end of perl script
