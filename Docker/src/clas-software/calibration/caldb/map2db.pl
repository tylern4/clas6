#!/usr/bin/env perl 
#
#   map2db.pl: Fills the tables with constants from map files
#   Created  : 25-MAY-2000 by Riad Suleiman
#
# $Id: map2db.pl,v 1.32 2001/03/22 16:36:55 marki Exp $
#####################################################################

use DBI;

$nquery = 0;

# connect to MySQL database on localhost

$database = "calib";
$user = "dbmanager";
$hostname = "localhost";
$debug = 0;
if ($debug) {$query_prescale = 1;} else {$query_prescale = 500;}
$run_trans_low = -1000000;
$run_trans_high = 1000000;
print "run_trans_low = $run_trans_low, run_trans_high = $run_trans_high\n";

print "password: ";
system("stty -echo");
chop($password=<STDIN>);
print "\n";
system("stty echo");
$dbh = DBI->connect("DBI:mysql:$database:$hostname", $user, $password);

if (defined $dbh) {
    print "Connection successful: handle: $dbh\n";
} else {
    die "Could not make database connect...yuz got problems...\n";
}

# create the system table

$sql  = "CREATE TABLE System (systemId int(11) not NULL auto_increment, ";
$sql .= " systemName varchar(64) not NULL, description text,";
$sql .= " PRIMARY KEY (systemId))";
&DO_IT;

# create the subsystem table

$sql  = "CREATE TABLE Subsystem (subsystemId int(11) not NULL auto_increment,";
$sql .= " subsystemName varchar(64) not NULL, systemId int(11) not NULL,";
$sql .= " description text, PRIMARY KEY (subsystemId))";
&DO_IT;

# create the item table

$sql  = "CREATE TABLE Item (itemId int(11) not NULL auto_increment,";
$sql .= " itemName varchar(64) not NULL, subsystemId int(11) not NULL,";
$sql .= " length int(11) unsigned, type char(8), description text,";
$sql .= " PRIMARY KEY (itemId))";
&DO_IT;

# create the run index table

$sql  = "CREATE TABLE RunIndex";
$sql .= " (RunIndexId int(11) not NULL auto_increment,";
$sql .= " minRun int(11), maxRun int(11), itemId int(11) not NULL,"; 
$sql .= " itemValueId int(11) not NULL, officer VARCHAR(31),";
$sql .= " time TIMESTAMP(14), comment TEXT,";
$sql .= " PRIMARY KEY (RunIndexId),";
$sql .= " INDEX ItemMinMax (itemId, minRun, maxRun))";
&DO_IT;

# Scan the map files
 
$author  = 'dbmanager';
$comment = 'copied from Map';
$officer = 'dbmanager';

&READ_IN_EXCLUDED_LIST;

open (LS, "ls $ENV{CLAS_PARMS}/Maps/*.map |");

while ( $mapfile = <LS> ) {

# get the system name

    chop $mapfile;
    print "Working on map file $mapfile\n";
    @field = split(/\//, $mapfile);
    @subfield = split(/\.map/, $field[$#field]);
    $system = $subfield[0];

    $ok = &CHECK_AGAINST_EXCLUDED_LIST;

    if ($ok) {

# insert the new system into the System table

	$sql  = "INSERT INTO System VALUES (NULL, '$system', 'from the Map')";
	&DO_IT;
	$systemId = $sth->{mysql_insertid};
	print "system = $system, systemId = $systemId\n";

# scan the Map file
    
	open (SCAN, "$ENV{'CLAS_BIN'}/scan_map -t $mapfile |");
	$line = <SCAN>;
	$line = <SCAN>;
	while ($line = <SCAN>) {

# get the subsystem information

	    chop $line;
	    @field = split(/\s+/, $line);
	    $keyword = $field[1];
	    if ($keyword eq "Subsystem:") {
		$subsystem = $field[2];
		$subsystem =~ s/,//;
		$nitems = $field[4];

# insert the new subsystem into the Subsystem table

		$sql = "INSERT INTO Subsystem VALUES (NULL, '$subsystem',"
		    . " $systemId, 'from the $system Map') ";
		&DO_IT;
		$subsystemId = $sth->{mysql_insertid};
		print "  subsystem = $subsystem, subsystemId = $subsystemId,"
		    . " nitems = $nitems\n";
	    
# loop over all items for this subsystem

		for ($i = 0; $i < $nitems; $i++) {
		    &PROCESS_ITEM();
		}

	    } else {
		print "bad line:\n";
		print $line;
		exit(1);
	    }
	}
    }
}

exit;

sub PROCESS_ITEM {

# get the item information

    $line = <SCAN>;
    chop $line;
    @field = split(/:/, $line);
    @field1 = split(/,/, $field[1]);
    $item = $field1[0];
    $item =~ s/^\s+//;
    @field1 = split(/,/, $field[2]);
    $length_map = $field1[0];
    $length_map =~ s/^\s+//;
    @field1 = split(/,/, $field[3]);
    $type = $field1[0];
    $type =~ s/^\s+//;
    @field1 = split(/,/, $field[4]);
    $nsets = $field1[0];
    $nsets =~ s/^\s+//;
    print "    Item: $item, Length: $length_map, Type: $type, Sets: $nsets\n";
    $line = <SCAN>;
    chop $line;
    @run = split(/\s+<--\s+/, $line);
    if ($debug) {print "\@run = @run\n";}

    $length_db = $length_map;
    if ($length_map == 6912) {
	$type_db = $type . 'blob';
    } else {
	$type_db = $type;
    }

# insert the new item into the Item table
    
    $sql = "INSERT INTO Item VALUES (NULL, '$item', $subsystemId,"
	. " $length_map, '$type_db',"
	    . " 'from the $system Map, subsystem $subsystem')";
    &DO_IT;
    $itemId = $sth->{mysql_insertid};
    if ($debug) {print "itemId = $itemId\n";}

    &CREATE_ITEM_VALUE_TABLE;
    
    for ($s = 1; $s <= $nsets; $s++) {
	$maxRun = $run[$s-1]-1;
	$minRun = $run[$s];
	if ($s == 1){$maxRun = 1000000;}
	if ($minRun == 'UNDEF') {$minRun = 0;}
	if ($s%1000 == 0) {print "    set=$s minRun=$minRun maxRun=$maxRun\n";}
	if ($maxRun >= $run_trans_low && $minRun <= $run_trans_high ) {
	    &PROCESS_SET();
	}
    }
}

sub CREATE_ITEM_VALUE_TABLE {

    $itemtableValue = "$system\_$subsystem\_$item";
    print "    creating table $itemtableValue\n";

    if ($type eq 'float'){
        $byte=10;
        $size = "$type($byte)";
    } elsif ($type eq 'int') {
        $byte=11;
        $size = "$type($byte)";
    } elsif ($type eq 'char') {
        $byte=1024;
        $size = "text";
        $length_db = 1;
    }
#
    if ($length_map == 6912){
        $length_db = 1;
        $size = "BLOB";
    }
#
    $sql = "CREATE TABLE $itemtableValue"
        . " (itemValueId INT(11) NOT NULL AUTO_INCREMENT PRIMARY KEY,"
        . " author VARCHAR(31), time TIMESTAMP(14),"
        . " minRunSource INT(11), maxRunSource INT(11), comment TEXT";
    foreach $i (1 .. $length_db) {
        $ipadded = sprintf("%04d", $i);
        $sql .= ", v_$ipadded $size";
    }
    $sql .= ')';
    &DO_IT;
}

sub PROCESS_SET {
    if ($debug) {print "set $s:\n";}
    $index = 0;
    if ($type eq "float") {
	$command = "$ENV{'CLAS_BIN'}/get_map_float -m$mapfile -s$subsystem -i$item -t$run[$s] -l$length_map";
	if ($debug) {print "command = $command\n";}
	open (GET, "$command |");
	while ($line = <GET>) { 
	    chop $line;
	    $value[$index] = $line;
	    if ($debug) {print "value[$index] = $value[$index]\n";}
	    $index++;
	} 
	
    } elsif ($type eq "int") {
	$command = "$ENV{'CLAS_BIN'}/get_map_int -m$mapfile -s$subsystem -i$item -t$run[$s] -l$length_map";
	if ($debug) {print "command = $command\n";}
	open (GET, "$command |");
	while ($line = <GET>) {
	    chop $line;
	    $value[$index] = $line;
	    if ($debug) {print "value[index] = $value[$index]\n";}
	    $index++;
	} 
	
    }  elsif ($type eq "char") {
	$command = "$ENV{'CLAS_BIN'}/get_map_char -m$mapfile -s$subsystem -i$item -t$run[$s] -l$length_map";
	if ($debug) {print "command = $command\n";}
	open (GET, "$command |");
	while ($line = <GET>) {
	    $value[$index] = $line;
	    if ($debug) {print "value[index] = $value[$index]\n";}
	    $index++;
	}
	chop $value[$index - 1]; # chop extra newline that get_map_char
                                 # "tosses in"
	
    }  else {
	print "Unknown Type";
	exit(1);
    }
    if ($debug) {print "index = $index\n";}
    
    for ($j = 0; $j < $index - 1; $j++) {
	if($value[$j] =~ /a/) {
	    if($type ne "char")
	    { 
		$value[$j] = '-9999';
	    }
	}
	$data[$j] = "$value[$j], ";
    }
    
    $data[$j] = "$value[$j] ";

    if ($debug) {print "\@data = @data[0 .. $index - 1]\n";}
    
    $itemtableValue = "$system\_$subsystem\_$item";
    
    $minRunSource = $minRun;
    $maxRunSource = $maxRun;
    
    if ($type eq "char"){ 
	$sql  = " INSERT INTO ";
	$sql .= " $itemtableValue VALUES ";
	$sql .= " (NULL, '$author', NULL, $minRunSource, $maxRunSource, '$comment', \"@value[0..$index-1]\")";
    } elsif ($length_map == 6912){
	@data = @value;
	$sql  = " INSERT INTO ";
	$sql .= " $itemtableValue VALUES ";
	$sql .= " (NULL, '$author', NULL, $minRunSource, $maxRunSource, '$comment', '@value[0..$index-1]')";  
    } else {
	$sql  = " INSERT INTO ";
	$sql .= " $itemtableValue VALUES ";
	$sql .= " (NULL, '$author', NULL, $minRunSource, $maxRunSource, '$comment', @data[0..$index-1])";
    }
    
    &DO_IT();
    
    $itemValueId = $sth->{mysql_insertid};
    if ($debug) {print "itemValueId = $itemValueId \n";}
    
    $itemtable = "RunIndex"; 
    $sql  = " INSERT INTO ";
    $sql .= " $itemtable VALUES ";
    $sql .= " (NULL, $minRun, $maxRun, $itemId,";
    $sql .= " $itemValueId, '$officer',";
    $sql .= " NULL, '$comment') ";
    &DO_IT();
}

sub DO_IT {

    $nquery++;
    if ($nquery%$query_prescale == 0) {
	$sql_frag = substr($sql, 0, 133); 
	print "nquery = $nquery, query = $sql_frag\n";
    }
                
    $sth = $dbh->prepare($sql)
        or die "Can't prepare $sql: $dbh->errstr\n";
                    
    $rv = $sth->execute
        or die "Can't execute the query $sql\n error: $sth->errstr\n";
                    
    return 0;
}

sub READ_IN_EXCLUDED_LIST {
    open (LIST, "systems_excluded.txt");
    $ix = 0;
    while (<LIST>) {
	chop;
	$system_excluded[$ix++] = $_;
    }
}

sub CHECK_AGAINST_EXCLUDED_LIST {
    $ok_local = 1;
    foreach $ix (0 .. $#system_excluded) {
	if ($system eq $system_excluded[$ix]) {$ok_local = 0;}
    }
    return $ok_local;
}
# end of perl script
