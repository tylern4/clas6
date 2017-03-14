#!/usr/bin/env perl
#
#   access_priv.pl: Create the Access Privileges tables of "calib" database
#   Created    : 25-MAY-2000 by Riad Suleiman
#

use DBI;

# connect to MySQL database on localhost

$database = "mysql";
$user = "dbmanager";
$hostname = "localhost";

print "password: ";
$password = <>;
chop $password;


$dbh = DBI->connect("DBI:mysql:$database:$hostname", $user, $password);

if (defined $dbh) {
    print "Connection successful: handle: $dbh\n";
} else {
    die "Could not make database connect...yuz got problems...\n";
}


#
##  Fill user table
#

# construct the SQL query

$sql  = " INSERT INTO user (Host, User, Password) VALUES('localhost', 'dbmanager', PASSWORD('e1tocome')) ";

# prepare and execute the query 

$sth = $dbh->prepare($sql)
    or die "Can't prepare $sql: $dbh->errstr\n";

$rv = $sth->execute
    or die "Can't execute the query $sql\n error: $sth->errstr\n";

##

$sql  = " INSERT INTO user (Host, User, Password) VALUES('%', 'clasuser', '') ";

# prepare and execute the query 

$sth = $dbh->prepare($sql)
    or die "Can't prepare $sql: $dbh->errstr\n";

$rv = $sth->execute
    or die "Can't execute the query $sql\n error: $sth->errstr\n";


#
##  Fill db table
#

# construct the SQL query

$sql  = " INSERT INTO db VALUES('%', 'calib', 'dbmanager', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y',' Y', 'Y') ";

# prepare and execute the query 

$sth = $dbh->prepare($sql)
    or die "Can't prepare $sql: $dbh->errstr\n";

$rv = $sth->execute
    or die "Can't execute the query $sql\n error: $sth->errstr\n";

##

$sql  = " INSERT INTO db (Host, Db, User, Select_priv) VALUES('%', 'calib', 'clasuser', 'Y') ";

# prepare and execute the query 

$sth = $dbh->prepare($sql)
    or die "Can't prepare $sql: $dbh->errstr\n";

$rv = $sth->execute
    or die "Can't execute the query $sql\n error: $sth->errstr\n";

#
##  Fill tables_priv table
#

# construct the SQL query

$sql  = " INSERT INTO tables_priv (Host, Db, User, Table_name, Table_priv) VALUES('%', 'calib', 'clasuser', 'RunIndexUsersuleiman', 'Select,Insert,Update,Delete') ";

# prepare and execute the query 

$sth = $dbh->prepare($sql)
    or die "Can't prepare $sql: $dbh->errstr\n";

$rv = $sth->execute
    or die "Can't execute the query $sql\n error: $sth->errstr\n";

##
# construct the SQL query

$sql  = " INSERT INTO tables_priv (Host, Db, User, Table_name, Table_priv) VALUES('%', 'calib', 'clasuser', 'RunIndexUsermarki', 'Select,Insert,Update,Delete') ";

# prepare and execute the query 

$sth = $dbh->prepare($sql)
    or die "Can't prepare $sql: $dbh->errstr\n";

$rv = $sth->execute
    or die "Can't execute the query $sql\n error: $sth->errstr\n";

##
# construct the SQL query

$sql  = " INSERT INTO tables_priv (Host, Db, User, Table_name, Table_priv) VALUES('%', 'calib', 'clasuser', 'RunIndexUserdavidl', 'Select,Insert,Update,Delete') ";

# prepare and execute the query 

$sth = $dbh->prepare($sql)
    or die "Can't prepare $sql: $dbh->errstr\n";

$rv = $sth->execute
    or die "Can't execute the query $sql\n error: $sth->errstr\n";

##
# construct the SQL query

$sql  = " INSERT INTO tables_priv (Host, Db, User, Table_name, Table_priv) VALUES('%', 'calib', 'clasuser', 'RunIndexUsere5a', 'Select,Insert,Update,Delete') ";

# prepare and execute the query 

$sth = $dbh->prepare($sql)
    or die "Can't prepare $sql: $dbh->errstr\n";

$rv = $sth->execute
    or die "Can't execute the query $sql\n error: $sth->errstr\n";

##


##

open (INPUT,"<Item.txt") ||
    die "Can't read input file";
while ( <INPUT> ) {
    $line = $_;
    chop $line;
    @field = split(/\t/,$line);
    $item = $field[0];
    $subsystem = $field[1];
    $system = $field[2];
    $length = $field[3];
    $type = $field[4];

####

    $itemtableValue = "$system\_$subsystem\_$item";


    $sql  = " INSERT INTO tables_priv (Host, Db, User, Table_name, Table_priv) VALUES('%', 'calib', 'clasuser', ' $itemtableValue', 'Select,Insert') ";
    
# prepare and execute the query 
    
    $sth = $dbh->prepare($sql)
	or die "Can't prepare $sql: $dbh->errstr\n";
    
    $rv = $sth->execute
	or die "Can't execute the query $sql\n error: $sth->errstr\n";
}
##

exit
