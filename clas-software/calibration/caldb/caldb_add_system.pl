#!/usr/bin/env perl
#
# caldb_add_system.pl: add a system to the System table of calibration
# database (calib).
# H.Avakian 08/01/01
# $Id: caldb_add_system.pl,v 2.5 2001/10/31 15:33:35 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");

use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();


$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$sql="SELECT systemName FROM System WHERE systemName=\'$system\'";

# check if we have already that system
if($dbh->do($sql) eq 1 ) {  die "error: system $system already exists.\n";}

# insert new system
$sql="INSERT System SET  systemName= \'$system \', description=\'$comment\'";
$dbh->do($sql);

# notify
$systemId=$dbh->{'mysql_insertid'};
if($systemId >0){ print  "info: new system $system created on $hostname server with systemId $systemId.\n";}

exit 0;

sub ProcessCommandLine {

    $database = "calib";
    $user=$ENV{USER} ;
    $password="";

    if ($help) {
	&PrintUsage();
	exit 0;
    }
    if (!$system) {
        if ($s) {
            $system = $s;
        } else {
            &PrintUsage();
            die "error: system not defined\n";
        }
    }
    if (!$comment) {
        if ($c) {
            $comment = $c;
        } else {
            &PrintUsage();
            die "error: comment not defined\n";
        }
    }

    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }

}


sub PrintUsage {
    print <<EOM;
function: This script adds a system to CalDB.

usage:
caldb_add_system.pl s=<system> c=<description> \\
      [hostname=<hostname of db server>]

alternate flag names:
  s or system for system name
  c or comment for system description

example:
caldb_add_system.pl  s=newGreatSystem c='important system'
EOM
    return;
}

# end of perl script
