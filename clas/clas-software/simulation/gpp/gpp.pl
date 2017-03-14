#!/apps/perl/perl5.005_03/bin/perl
#
$pgm = 'gpp';
$runno = 8903;
$nevent = 20000;
$workdir = '/work/clas/disk1/kjoo/gpp_study';
$silodir = '/work/clas/gsim/gsim_unprocessed/kjoo/aps-99/hf1g';
#$silodir = '/work/clas/disk2/kjoo/gpp_study';
$OS      = $ENV{"OSNAME"};
$TOP_DIR = $ENV{"TOP_DIR"};
#
chdir("$workdir");
$infile = "$silodir/hf1g-ppi-01.gsim";
$outfile = "hf1g-ppi-01.gpp";
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#
use DBI;

@drivers = DBI->available_drivers;

print "drivers available are: @drivers\n";

# connect to MYSQL database on claspc10

$database = 'clas_online';
$user = "clas_offline";
$password = "";
$hostname = "claspc10.cebaf.gov";
$dbh = DBI->connect("DBI:mysql:$database:$hostname", $user, $password);
$sql_cmd  = "SELECT ";
$sql_cmd .= "ec_inner_lo, ec_inner_hi, ec_outer_lo, ec_outer_hi, ";
$sql_cmd .= "ec_total_lo, ec_total_hi, cc_lo, cc_hi, beam_energy, ";
$sql_cmd .= "run_log_files.run, location ";
$sql_cmd .= "FROM ";
$sql_cmd .= "run_log_begin, run_log_files ";
$sql_cmd .= "WHERE ";
$sql_cmd .= "run_log_files.run=run_log_begin.run and ";
$sql_cmd .= "run_log_files.run=$runno";
#
$sth = $dbh -> prepare($sql_cmd)
    or die "Can't prepare $sql_cmd: $dbh-errstr\n";
#
$rv = $sth-> execute
    or die "Can't execute the query $sql_cmd\n error: $sth-> errstr\n";
#
while (($ec_inner_lo, $ec_inner_hi, $ec_outer_lo, $ec_outer_hi,$ec_total_lo, 
        $ec_total_hi, $cc_lo, $cc_hi, $beam_energy, $run, $location) = 
        $sth->fetchrow_array) {
	$ec_inner = $ec_inner_lo;
	$ec_total = $ec_total_lo;
    $cc       = $cc_hi;
}
#------------------------------------------------------------------------
#------------------------------------------------------------------------
$cmd = "$TOP_DIR/bin/$OS/$pgm -M$nevent -R$runno -P0x1f -T0x7 -t -E$ec_total -C$cc -I$ec_inner -f2.0 -a2.0 -b2.5 -c3.0 -o$outfile $infile";
system $cmd;


