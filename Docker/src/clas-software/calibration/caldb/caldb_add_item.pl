#!/usr/bin/env perl
#
# caldb_add_item.pl: add an item to Item table in calib database
# given a name of existing system and subsystem
# 
# H.Avakian 8/02
# $Id: caldb_add_item.pl,v 2.11 2003/03/11 20:10:21 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");

use CaldbUtil;

$mysql_column_max = 3398;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);
# check the $system and $subystem
$sql  ="SELECT subsystemId FROM System,Subsystem ";
$sql .=" WHERE systemName=\'$system\'";
$sql .=" && subsystemName=\'$subsystem\'";
$sql .=" && System.systemId=Subsystem.systemId";
$sth= $dbh->prepare($sql); $sth->execute;
$subsystemid =  $sth->fetchrow_array;
if($subsystemid < 1 ) {  die "error: subsystem $subsystem unknown.\n";}

# check the item

$where  ="systemName=\'$system\' && subsystemName=\'$subsystem\' ";
$where .=" && itemName=\'$item\' ";
$where .=" && System.systemId=Subsystem.systemId";
$where .=" && Subsystem.subsystemId=Item.subsystemId";
$sql="SELECT itemId FROM Item,System,Subsystem WHERE $where "; 
if($dbh->do($sql) eq 1 ) {  
    die "error: item $item for system $system, subsystem $subsystem already exists.\n";
}

# check for long arrays

if ($length > $mysql_column_max) {
    if ($type eq 'int') {
	$type = 'intblob';
    } elsif ($type eq 'float') {
	$type = 'floatblo';
    }
}

# insert the item

$sql ="INSERT Item SET  itemName= \'$item \',subsystemId=$subsystemid,";
$sql .=" type=\'$type\',";
$sql .="length=$length,";
$sql .="description = \'$comment\'";

DO_IT();

# get the itemId
$itemId=$sth->{mysql_insertid};

if($itemId > 0) {
# create the itemValue table if there is an item entry
    $table=$system.'_'.$subsystem.'_'.$item;
    $sql  ="CREATE TABLE $table (";
    $sql .=" itemValueId int(11) NOT NULL auto_increment,author varchar(31), ";
    $sql .=" time timestamp(14),minRunSource int(11),maxRunSource int(11),";
    $sql .=" comment text";
    if ($type eq 'char') {
	$sql  .= ", v_0001 text";
    } elsif ($type eq 'intblob' || $type eq 'floatblo') {
	$sql .=  ", v_0001 blob";
    } else {
	if ($type eq 'int') {
	    $typedb = " int(11) default NULL ";
	} elsif ($type eq 'float') {
	    $typedb = " float default NULL ";
	} else {
	    die "error: type not float or int. This should never happen.";
	}
	for ($ix=1 ; $ix<=$length; $ix++){
	    $inum = sprintf("%04d", $ix);
	    $sql  .= ", v_$inum $typedb ";
	}
    }
    $sql .=" ,PRIMARY KEY (itemValueId))";
    
# Notify
    if($dbh->do($sql)) {
	print  "info: new item $item (itemId=$itemId) created for system $system, subsystem $subsystem on $hostname.\n";
	Grant_Privileges_For_Clasuser($table);
    } else { 
# delete the itemId we don't need it
        print "Error in: $sql $dbh->errstr \n";
        print "Trying to delete the corresponding entry in Item table: $itemId \n";
        $sql ="DELETE from Item where  itemId=$itemId";
        $rv=$dbh->do($sql);
        print " rv=$rv \n";
    }
}

exit 0;


sub ProcessCommandLine {

    $database = "calib";
    $user=$ENV{USER};
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
    if (!$item) {
        if ($i) {
            $item = $i;
        } else {
            &PrintUsage();
            die "error: item not defined\n";
        }
    }

    if (!$type) {
	&PrintUsage();
	die "error: item type not defined\n";
    } else {
	if( "float" ne $type && "int" ne $type && "char" ne $type) {
	    die "error: type $type not supported\n";
	}
    }
    
    
    if (!$length) {
	die "error: item length not defined\n";
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
function: This script adds an item to the CalDB and creates the corresponding
          constant set table

usage:
caldb_add_item.pl s=<system>  ss=<subsystem>  i=<item> c=<description> \\
    type=[float|int|char|intblob] length=<length of constant array>  \\
    [hostname=<hostname of db server>]

alternate flag names:
  s  or system for system name
  ss or subsystem for subsystem name
  i  or item for item name
  c or comment for system description

example:
caldb_add_item.pl s=newGreatSystem c="int_item8" ss=somesubs i=item8 \\
    type=int length=10
EOM
    return;
}

sub Grant_Privileges_For_Clasuser {
    my ($tablename) = @_;
    $sql = "GRANT SELECT, INSERT ON $tablename TO 'clasuser'";
    DO_IT();
}

sub DO_IT {
    $sth = $dbh->prepare($sql)
        or die "Can't prepare $sql: $dbh->errstr\n";
    
    $rv = $sth->execute
        or die "Can't execute the query $sql\n error: $sth->errstr\n";
    
    return 0;
}

# end of perl script
