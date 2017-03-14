#!/usr/bin/env perl
#
# Delete the specified items. If item is not specified, all items in
# the subsystem will be deleted. If subsystem is not specified, all
# subsystems will be deleted. Corresponding constant set tables and
# main run index entries will be deleted as well.
#
# $Id: caldb_delete_items.pl,v 2.5 2002/11/13 19:31:46 marki Exp $
##################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");

use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();
#
# get the password
#
print "password: ";
system("stty -echo");
chop($password=<STDIN>);
print "\n";
system("stty echo");

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

print "info: working on system $system\n";

&DoSel($dbh, "SELECT systemId FROM System WHERE systemName=\'$system\'",
       \$sth);
$system_id = $sth->fetchrow_array;
if (!$system_id) {die "error: system $system not found\n";}

@subsystem_array = ();
if ($subsystem) {
    $subsystem_array[0] = $subsystem;
} else {
    $status = ShowSubsystems($dbh, $system, $item, \@subsystem_array);
}
for ($iss = 0; $iss <= $#subsystem_array; $iss++) {
    $subsystem_this = $subsystem_array[$iss];
    print "info:   working on subsystem $subsystem_this\n";
    @item_array = ();
    if ($item) {
	$item_array[0] = $item;
    } else {
	$status = ShowItems($dbh, $system, $subsystem_this, \@item_array);
    }
    for ($ii = 0; $ii <= $#item_array; $ii++) {
	$item_this = $item_array[$ii];
	print "info:     working on item $item_this\n";
	$status = ShowItemInfo($dbh, $system, $subsystem_this, $item_this,
			       \%itemInfo);
	$item_id = $itemInfo{itemId};
	$ssi = $system."_".$subsystem_this."_".$item_this;
	print "info:     deleting item $item_this\n";

# delete valueID table
	$sql="DROP TABLE IF EXISTS $ssi";
	$status=$dbh->do($sql)
	    or die "error: can't execute $sql: $dbh->errstr\n";

# delete entry in privilege table
	$sql = "DELETE from mysql.tables_priv WHERE Table_name = '$ssi'";
	$status=$dbh->do($sql)
	    or die "error: can't execute $sql: $dbh->errstr\n";
    
# delete RunIndex entries       
	$sql="DELETE FROM RunIndex WHERE itemId = $item_id";
	$status=$dbh->do($sql)
	    or die "error: can't execute $sql: $dbh->errstr\n";
    
# delete item entry
	$sql="DELETE FROM Item WHERE itemName=\'$item\' && itemId=$item_id";
	$status=$dbh->do($sql)
	    or die "error: can't execute $sql: $dbh->errstr\n";
    }
    if (!$item) {
	print "info:   deleting subsystem $subsystem_this\n";
	$sql = "DELETE FROM Subsystem WHERE subsystemName = '$subsystem_this'"
	    . " AND systemId = $system_id";
	$status=$dbh->do($sql)
	    or die "error: can't execute $sql: $dbh->errstr\n";
    }
}

# delete the system if subsystem not defined
if (!$subsystem) {
    print "info: deleting system $system\n";
    $sql="DELETE FROM System WHERE systemName = '$system'";
    $status=$dbh->do($sql)
	or die "error: can't execute $sql: $dbh->errstr\n";
}

exit 0;

sub ProcessCommandLine {

    $database = "calib";
    $user="dbmanager";
    $hostname="localhost";

    if ($help) {
	PrintUsage();
	exit 0;
    }

    if (!$system) {
        if ($s) {
            $system = $s;
        } else {
            PrintUsage();
            die "error: system not defined\n";
        }
    }

    if (!$subsystem) {
	if ($ss) {
	    $subsystem = $ss;
	} else {
	    print "info: subsystem not defined, all subsystems will be deleted\n";
	}
    }
    if (!$item) {
	if ($i) {
	    $item = $i;
	} else {
	    print "info: item not defined, all items will be deleted\n";
	}
    }
}


sub PrintUsage {
    print <<EOM;
function: deletes systems, subsystems and items from the CalDB. Must be run by
          dbmanager on localhost.

usage:
caldb_delete_items.pl s=<system> [ss=<subsystem>] [i=<item>]

alternate flag names:
  s  or system for system name
  ss or subsystem for subsystem name
  i  or item for item name

example:
caldb_delete_items.pl s=newGreatSystem ss=badsubs i=item9
EOM
    return;
}

sub DoSel {
    my($dbh,$sql_cmd,$sthref) = @_;
    $$sthref = $dbh -> prepare($sql_cmd)
	or die "Can't prepare $sql_cmd: $dbh-errstr\n";
    $rv = $$sthref -> execute
	or die "Can't execute the query: $sth-> errstr\n";
}

# end of perl script
