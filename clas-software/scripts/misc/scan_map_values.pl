#!/usr/local/bin/perl -w
# plot_map.pl a program to plot the map variables by entry

if ($ARGV[0] eq "-h") {
    &PrintUsage;
    exit(0);
}

# Set default variables
$run_min = 1;
$run_max = 999999;
$dir=$ENV{'CLAS_PARMS'};
$mapfile= "$dir/Maps/TAG_CALIB.map";
$column=0;

# Parse through command line
foreach (@ARGV) {
    if (/^-R/) {
	$run_max = $_;
	$run_max =~ s/^-R//;
    } elsif (/^-r/) {
	$run_min = $_;
	$run_min =~ s/^-r//;
    } elsif (/^-m/) {
	$mapfile = $_;
	$mapfile =~ s/^-m//;
    } elsif (/^-s/) {
	$subsystem = $_;
	$subsystem =~ s/^-s//;
    } elsif (/^-i/) {
	$item = $_;
	$item =~ s/^-i//;
    } elsif (/^-c/) {
	$column = $_;
	$column =~ s/^-c//;
    } else {
	&PrintUsage;
	exit(0);
    }
}
print "#$0: map used is $mapfile.\n";
print "#$0: Minimum run number: $run_min.\n";
print "#$0: Maximum run number: $run_max.\n";

$sm_cmd = "scan_map -t $mapfile";

$found_s=0;
$found_i=0;
foreach(`$sm_cmd`){
    $line=$_;
    if ($line =~ /Subsystem:/){
	if ($line =~ /$subsystem/){
	    $found_s=1;
	}
	else {
	    $found_s=0;
	}
    }
    if (1==$found_s){
	if ($line =~ /Item:/){
	    if ($line =~ /$item/){
		$found_i=1;
	    }
	    else {
		$found_i=0;
	    }
	}
	elsif(1==$found_i){
	    @runlist=split(/\s+<--\s+/,$line);
	}
    }
}

#remove INF and UNDEF
pop(@runlist);
@runlist=reverse(@runlist);
pop(@runlist);

foreach $run (@runlist){
    if ($run>$run_min && $run <$run_max){ 
	$gmf_cmd = "get_map_float -t$run -s$subsystem -i$item -m$mapfile";
	$value = (`$gmf_cmd`)[$column];
	chomp($value);
	$entry{"$run"}=$value;
    }
}

foreach $key (sort{$a<=>$b}(keys(%entry))){
    print "$key\t$entry{$key}\n";
}

sub PrintUsage {
    print "\nUsage: $0 [-h][-R<run#>][-r<run#>][-l] -m<map> -s<sub> -i<item> -t<run> -c<column>\n";
    print "\t-h \t\tPrint this help message.\n";
    print "\t-R<run#> \tMaximum run number to include in list.\n";
    print "\t-r<run#> \tMinimum run number to include in list.\n";
    print "\t-m<mapfile> \t\tMap file\n";
    print "\t-s<subsystem> \t\tSubsystem to get array of floats from\n";
    print "\t-i<item> \t\tItem to get\n";
    print "\t-c<column> \t\tcolumn of array to get (default=0)\n";
}

