#!/apps/perl/5.6.0/bin/perl
#
# $Id: online_update.pl,v 1.10 2004/01/14 15:13:08 clascron Exp $
#

use lib ("$ENV{CLAS_TOOLS}/caldb");
use DBI;
use CaldbUtil;

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
$database = "calib";
$verbose = 1;
ConnectToServer ($hostname, $user, $password, $database, $verbose, \$dbh_mysql);

$nswcs = 60*$ARGV[0];
$time_check = &GET_DATE_TIME($nswcs);
print "time_check: $time_check\n";

# find out if we are doing electron running or photon running

# assume everything is an electron run, mini = 0 used to indicate that
# the minitorus ps was being used for the old downstream pair
# spectrometer

$mini = 1;
print "mini = $mini\n";

&UpdateBOR;

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
	print "@row ";
	($run, $beam_energy, $torus_current, $mini_current, $tagger_current)
	    = @row;
	if ($mini) {
	    $pair_spec_current = 0;
	} else {
	    $pair_spec_current = $mini_current;
	    $mini_current = 0;
	}
	print "mini_current = $mini_current, pair_spec_current = $pair_spec_current\n";
	$sql_cmd = "SELECT * FROM System, Subsystem, Item, RunIndex";
	$sql_cmd .= " WHERE Subsystem.systemId = System.systemId";
	$sql_cmd .= " AND Item.subsystemId = Subsystem.subsystemId";
	$sql_cmd .= " AND RunIndex.itemId = Item.itemId";
	$sql_cmd .= " AND systemName = 'RUN_CONTROL'";
	$sql_cmd .= " AND subsystemName = 'beam'";
	$sql_cmd .= " AND itemName = 'energy'";
	$sql_cmd .= " AND minRun = $run";
	&DoIt($dbh_mysql, \$sth_mysql);
	if ($sth_mysql -> fetchrow_array) {
	    print "bor found\n";
	} else {
	    print "bor missing\n";
	    &InsertBOR;
	}
    }
    return;
}

##########

sub InsertBOR {
    print "Inserting entries for beginning-of-run, run $run.\n";
    WriteAndLinkConstantSet($dbh_mysql, 'RUN_CONTROL', 'beam',
			    'energy', 'RunIndex', $run, $infinity, 'from online',
			    $run, $run, 'from online', [ $beam_energy ],
			    \$runIndexId);
    WriteAndLinkConstantSet($dbh_mysql, 'RUN_CONTROL', 'currents',
			    'torus', 'RunIndex', $run, $infinity, 'from online',
			    $run, $run, 'from online', [ $torus_current ],
			    \$runIndexId);
    WriteAndLinkConstantSet($dbh_mysql, 'RUN_CONTROL', 'currents',
			    'minitorus', 'RunIndex', $run, $infinity, 'from online',
			    $run, $run, 'from online', [ $mini_current ],
			    \$runIndexId);
    WriteAndLinkConstantSet($dbh_mysql, 'RUN_CONTROL', 'currents',
			    'pair_spec', 'RunIndex', $run, $infinity, 'from online',
			    $run, $run, 'from online', [ $pair_spec_current ],
			    \$runIndexId);
    WriteAndLinkConstantSet($dbh_mysql, 'RUN_CONTROL', 'currents',
			    'tagger', 'RunIndex', $run, $infinity, 'from online',
			    $run, $run, 'from online', [ $tagger_current ],
			    \$runIndexId);
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
	print "@row ";
	($run, $fcup, $fcup_active, $fcup_live, $nevent) = @row;
	$sql_cmd = "SELECT * FROM System, Subsystem, Item, RunIndex";
	$sql_cmd .= " WHERE Subsystem.systemId = System.systemId";
	$sql_cmd .= " AND Item.subsystemId = Subsystem.subsystemId";
	$sql_cmd .= " AND RunIndex.itemId = Item.itemId";
	$sql_cmd .= " AND systemName = 'RUN_CONTROL'";
	$sql_cmd .= " AND subsystemName = 'Faraday_cup'";
	$sql_cmd .= " AND itemName = 'Q_all'";
	$sql_cmd .= " AND minRun = $run";
	&DoIt($dbh_mysql, \$sth_mysql);
	if ($sth_mysql -> fetchrow_array) {
	    print "eor found\n";
	} else {
	    print "eor missing\n";
	    &InsertEOR;
	}
    }

    return;
}

##########

sub InsertEOR {
    print "Inserting entries for end-of-run, run $run.\n";
    WriteAndLinkConstantSet($dbh_mysql, 'RUN_CONTROL', 'Faraday_cup',
			    'Q_all', 'RunIndex', $run, $run, 'from online',
			    $run, $run, 'from online', [ $fcup ],
			    \$runIndexId);
    WriteAndLinkConstantSet($dbh_mysql, 'RUN_CONTROL', 'Faraday_cup',
			    'Q_active', 'RunIndex', $run, $run, 'from online',
			    $run, $run, 'from online', [ $fcup_active ],
			    \$runIndexId);
    WriteAndLinkConstantSet($dbh_mysql, 'RUN_CONTROL', 'Faraday_cup',
			    'Q_live', 'RunIndex', $run, $run, 'from online',
			    $run, $run, 'from online', [ $fcup_live ],
			    \$runIndexId);
    WriteAndLinkConstantSet($dbh_mysql, 'RUN_CONTROL', 'events',
			    'raw', 'RunIndex', $run, $run, 'from online',
			    $run, $run, 'from online', [ $nevent ],
			    \$runIndexId);
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
