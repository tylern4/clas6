#!/usr/bin/env perl
#
# caldb_add_subsystem.pl: add a subsystem to the Subsystem table of 
# calibration database given the existing system name
#
# H.Avakian 08/01/01
# $Id: caldb_add_subsystem.pl,v 2.5 2001/10/31 15:33:35 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");

use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();


$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);
# check the $system
$sql="SELECT systemId FROM System WHERE systemName=\'$system\'";
$sth= $dbh->prepare($sql); $sth->execute;
$systemid =  $sth->fetchrow_array;
if($systemid < 1 ) {die "error: system $system unknown\n";}

# check the subsystem

$where="systemName=\'$system\' && subsystemName=\'$subsystem\' && System.systemId=Subsystem.systemId";
$sql="SELECT subsystemId FROM System,Subsystem WHERE $where "; 
if($dbh->do($sql) eq 1 ) {  
    die "error: subsystem $subsystem for sysytem=$system already exist\n";
}

# add a new system
$sql="INSERT Subsystem SET subsystemName= \'$subsystem \', systemId=$systemid, description=\'$comment\'";
$dbh->do($sql);
# get the subsystemId  
$subsystemId=$dbh->{'mysql_insertid'};

# notify
if($subsystemId >0 ) {print "info: new subsystem $subsystem created for $system on $hostname with subsystemId $subsystemId.\n";}

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

     if (!$subsystem) {
        if ($ss) {
            $subsystem = $ss;
        } else {
            &PrintUsage();
            die "error: subsystem not defined\n";
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
function: This script adds a subsystem to the CalDB. An existing system must
          be specified.

usage:
caldb_add_subsystem.pl s=<system> ss=<subsystem> c=<description> \\
   [hostname=<hostname of db server>]

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  c or comment for subsystem description

example:
caldb_add_subsystem.pl  s=newGreatSystem ss=somesubs  c='important subsystem' 
EOM
    return;
}

# end of perl script
