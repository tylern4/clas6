#!/usr/local/bin/perl -w
# dump_map_values.pl creates a text-dump of the contents of a given
# map file, in a format that can be easily diff'ed with the a dump
# of perhaps a different version of the same map-file.


# Set default variables
$run = 1;
$dir=$ENV{'CLAS_PARMS'};
$dir="$dir/Maps";
$mapfile= "RF_OFFSETS.map";
$dofirst = 0;

$run_entry = " ";
if ($ARGV[0] eq "-h") {
    &PrintUsage;
    exit(0);
}

# Parse through command line
foreach (@ARGV) {
    if (/^-R/) {
	$run = $_;
	$run =~ s/^-R//;
    } elsif (/^-m/) {
	$mapfile = $_;
	$mapfile =~ s/^-m//;
    } elsif (/^-d/) {
	$dir = $_;
	$dir =~ s/^-d//;
    } elsif (/^-f/) {
	$dofirst = 1;
    } else {
	&PrintUsage;
	exit(0);
    }
}
$fullmapfile = "$dir/$mapfile";
print "#$0: map used is $fullmapfile for $run\n";

$sm_cmd = "scan_map $fullmapfile";
#print "\n sm_cmd = $sm_cmd\n";

foreach(`$sm_cmd`){
    $line=$_;
    if ($line =~ /Subsystem:/){
#	($dummy,$dummy,$subsystem,$dummy,$nitem) = split(/[\s,]+/,$line);
	($dummy,$dummy,$subsystem,$dummy) = split(/[\s,]+/,$line);
#	print "$line becomes\n";
#	print "Found $subsystem\n";
    }
    if ($line =~ /Item:/) {
	($dummy,$dummy,$item,$dummy,$length,$dummy,$type) = split(/[\s,]+/,$line);
	push(@itemlist,"$subsystem $item $length $type");
#	print "$line becomes\n";
#	print "$subsystem $item $length $type\n";
    }
}

foreach(@itemlist) {
    ($subsystem,$item,$length,$type) = split(/\s+/,$_);

#    print "$subsystem $item $length $type\n";
    $gmf_cmd = "get_map_$type -t$run -s$subsystem -i$item -m$fullmapfile";
    
#    print " ** Doing: $gmf_cmd\n";
    @values = (`$gmf_cmd`);
    if ($dofirst) {
	$first_cmd = "get_first -m $fullmapfile -s $subsystem -i $item -r $run";
	@firstret = (`$first_cmd 2>&1`);
	$run_entry = $firstret[0];
	chomp($run_entry);
    }
    for ($i=0; $i < $length; $i++) {
	$thisval = $values[$i];
	chomp($thisval);
	write;
#	print "$mapfile $subsystem $item $i :\t $thisval";
#	if ($dofirst) {
#	    print "\t $run_entry";
#	}
#	print "\n";
    }
#    exit;
}

format =
@<<<<<<<<<<<<<<<<<<<< @||||||||||| @|||||||||||| @>>>> : @>>>>>>>   @>>>>>>>>
$mapfile,             $subsystem,  $item,       $i,     $thisval,  $run_entry
.

sub PrintUsage {
    print "$0:\n Creates a text-dump of the contents of a given\n";
    print "map file, in a format that can be easily diff'ed with\n";
    print "the a dump of perhaps a different version of the same\n";
    print "map-file.\n";
    print "\nUsage: $0 [-h] -R<run#> -m<map> -d<dir>\n";
    print "\t-h \t\tPrint this help message.\n";
    print "\t-R<run#> \tRun number.(default: $run)\n";
    print "\t-m<mapfile> \tMap file.(default: $mapfile)\n";
    print "\t-d<dir> \tDirectory for mapfile.(default: $dir)\n";
    print "\t-f      \tPrint out the run number for the map-entry.\n";
}

