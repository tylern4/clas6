#! /usr/bin/env perl

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

ProcessCommandLine();

$database = "calib";

# get the password

print "password: ";
system("stty -echo");
chop($password=<STDIN>);
print "\n";
system("stty echo");

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
                          \$dbh);

$system = '';
$subsystem = '';
$item='';
ShowSSI($dbh, $system, $subsystem, $item, \@ssi_ray);

for $i (0..$#ssi_ray) {
    $table = $ssi_ray[$i][0] . '_' .  $ssi_ray[$i][1] . '_' .$ssi_ray[$i][2];
    $sql = "SELECT count(*) FROM mysql.tables_priv WHERE Table_name = '$table'";
    DO_IT();
    @row = $sth->fetchrow_array;
    $count = $row[0];
    if ($count != 1) {
	print "$table: $count\n";
	if ($fix && $count == 0) {
	    FixIt($table);
	}
    }
}

exit 0;

sub ProcessCommandLine {

    if ($help) {
        &PrintUsage();  
        exit 0;
    }
    if (!$user) {
	$user = "dbreader";
    }
    if (!$hostname){
        $hostname = "clasdb.jlab.org";
    }
}

sub PrintUsage {
    print <<EOM;
purpose: check that each item has an entry in the tables_priv table to allow
         write access for clasuser to the constant set table
usage:
caldb_check_tables_priv.pl [fix=<non-zero>] [user=<user name>] \\
    [hostname=<hostname of db server>] [help=<non-zero>]

example:
caldb_check_tables_priv.pl
EOM
    return;
}

sub DO_IT {
    $sth = $dbh->prepare($sql)
	or die "Can't prepare $sql: $dbh->errstr\n";
    
    $rv = $sth->execute
	or die "Can't execute the query $sql\n error: $sth->errstr\n";
    
    return 0;
}

sub FixIt {
    my ($table) = @_;
    $sql = "GRANT INSERT ON $table TO clasuser\@'%.jlab.org'";
    print "$sql\n";
    DO_IT();
    return 0;
}

# end of perl script


