#!/apps/perl/5.6.0/bin/perl
#
# $Id: online_translate.pl,v 2.1 2003/08/26 20:19:44 clascron Exp $
#

use DBI;

$infinity = 1000000;

# connect to the INGRES DB on db5

$database = "clasprod";
$user = "clascron";
$password = "";
$hostname = "db5";

$dbh_ingres = DBI->connect("DBI:Ingres:$hostname\::$database",$user, undef);

if (not defined $dbh_ingres) {
    die "Failed to connect to Ingres database\n";
} else {
    print "The dbh handle is: $dbh_ingres\n";
}
    
$dbh_ingres->do('set lockmode session where readlock=nolock')
    or  die "Cannot set readlock=nolock: \n $dbh_ingres->errstr\n";

# connect to the MySQL database

$hostname = "clasdb.jlab.org";
$user = "clascron";
$password = "";
$database = "test";
$verbose = 1;
$dbh_mysql = DBI->connect("DBI:mysql:$database:$hostname", $user, $password);
if (not defined $dbh_mysql) {
    die "Failed to connect to MySQL database\n";
} else {
    print "The dbh handle is: $dbh_mysql\n";
}

$nswcs = 60*$ARGV[0];
$time_check = &GET_DATE_TIME($nswcs);
print "time_check: $time_check\n";

print "begin_run table\n";
&UpdateBOR;

print "end_run table\n";
&UpdateEOR;

$dbh_ingres->disconnect;

exit;

##########

sub UpdateBOR {

    $sql_cmd = "SELECT run, beam_energy, torus_current, mini_current,";
    $sql_cmd .= " tagger_current";
    $sql_cmd .= " FROM ingres.run_log_begin";
    $sql_cmd .= " WHERE start_date > \'$time_check\'";
    $sql_cmd .= " ORDER BY run";
    &DoIt($dbh_ingres, \$sth_ingres);

# Loop through all the rows and check if stuff is there. If not insert into
# Caldb.

    while (@row = $sth_ingres -> fetchrow_array) {
	print "@row\n";
	($run, $beam_energy, $torus_current, $mini_current, $tagger_current)
	    = @row;
    }
    return;
}

##########

sub UpdateEOR {

    $sql_cmd = "SELECT run, fcup, fcup_active, fcup_live, nevent";
    $sql_cmd .= " FROM ingres.run_log_end WHERE end_date > \'$time_check\'";
    $sql_cmd .= " ORDER BY run";
    &DoIt($dbh_ingres, \$sth_ingres);

# Loop through all the rows and check if stuff is there. If not insert into
# Caldb.

    while (@row = $sth_ingres -> fetchrow_array) {
	print "@row\n";
	($run, $fcup, $fcup_active, $fcup_live, $nevent) = @row;
    }

    return;
}

##########

sub DoIt {
    my($dbh, $sthref) = @_;
    $$sthref = $dbh -> prepare($sql_cmd)
	or die "Can't prepare $sql_cmd: $dbh-errstr\n";

    $rv = $$sthref -> execute
	or die "Can't execute the query: $sth-> errstr\n";
    return;
}

##########

sub GET_DATE_TIME {
    my($delta_secs) = @_;
    my @month_name = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
    $time_check = time() - $delta_secs;
    my @time_array = localtime($time_check);
    my $year = 1900 + $time_array[5];

    $month = $month_name[$time_array[4]];

    for ($i = 0; $i <5; $i++) {
	if ($time_array[$i] < 10) {
	    $time_array[$i] = '0'.$time_array[$i];
	}
    }
    $day = $time_array[3];

    my $date_time
	= "$day-$month-$year $time_array[2]:$time_array[1]:$time_array[0]";
    
    return($date_time);
}

# end of perl script
