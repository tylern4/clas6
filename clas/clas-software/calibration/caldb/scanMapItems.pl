#!/usr/bin/env perl 
#
# $Id: scanMapItems.pl,v 1.3 2001/03/22 16:36:55 marki Exp $
#####################################################################

open (LS, "ls $ENV{CLAS_PARMS}/Maps/*.map |");

while ( $mapfile = <LS> ) {

# get the system name

    chop $mapfile;
    print "Working on map file $mapfile\n";
    @field = split(/\//, $mapfile);
    @subfield = split(/\.map/, $field[$#field]);
    $system = $subfield[0];

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
            print "subsystem = $subsystem, subsystemId = $subsystemId,"
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
    $length = $field1[0];
    $length =~ s/^\s+//;
    @field1 = split(/,/, $field[3]);
    $type = $field1[0];
    $type =~ s/^\s+//;
    @field1 = split(/,/, $field[4]);
    $nsets = $field1[0];
    $nsets =~ s/^\s+//;
    print "Item: $item, Length: $length, Type: $type, Sets: $nsets\n";
    $line = <SCAN>;
    chop $line;
    @run = split(/\s+<--\s+/, $line);
    if ($debug) {print "\@run = @run\n";}
}

# end of perl script
