#!/usr/bin/env perl
#
# $Id: caldb_add_officer.pl,v 2.9 2007/08/15 15:57:29 marki Exp $
########################################################################

use DBI;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

# get the password

print "password: ";
system("stty -echo");
chop($password=<STDIN>);
print "\n";
system("stty echo");

$dbh = DBI->connect("DBI:mysql:$database:$hostname", $user, $password);

$sql = "INSERT INTO user VALUES ('%','$officer','$new_password','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','Y','Y','N','N','N','','','','',0,0,0)";

&DO_IT;

$sql = "INSERT INTO db "
    . "VALUES ('%.jlab.org', 'calib%', '$officer', "
    . "'Y','Y','Y','Y','Y','Y','Y','Y','Y','Y','N','N')";


&DO_IT;

print "Updates made. 'mysqladmin -u username -p reload' to reload privilege"
    . " tables.\n";

exit 0;

sub ProcessCommandLine {

    $user = 'dbmanager';
    $database = 'mysql';
    $hostname = "localhost";
    $new_password = '';
    if (!$officer) {
	&PrintUsage();
	die "error: officer not defined\n";
    }
}

sub DO_IT {    

    $sth = $dbh->prepare($sql)
	or die "Can't prepare $sql: $dbh->errstr\n";
    
    $rv = $sth->execute
	or die "Can't execute the query $sql\n error: $sth->errstr\n";
    
    return 0;

}

sub PrintUsage {
    print <<EOM;
usage:
caldb_add_officer.pl officer=<new user>

example:
caldb_add_officer.pl officer=foo
EOM
    return;
}
