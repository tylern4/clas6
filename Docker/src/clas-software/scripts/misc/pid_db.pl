#! /usr/local/bin/perl

@arr = ();
for (@ARGV) {

    if(/^-d(.+)/)      { $dbfile = $1;}

    else {
        push(@arr,($_));
    }
}

# undef - remember
dbmopen(%RUN, $dbfile, 0644) || print L "Could not open database for offline\n";

for (@arr){
    if (-r $_){
	open(INFILE, $_);
	print "opening file: " . $_ . "\n";
	while (<INFILE>){
	    sectorfield($_, "HB pos");
	    sectorfield($_, "HB neg");
	    sectorfield($_, "TB neg");
	    sectorfield($_, "TB pos");
	    sectorfield($_, "Chi2 pos");
	    sectorfield($_, "Chi2 neg");
	    sectorfield($_, "Unknown");
	    sectorfield($_, "Gamma");
	    sectorfield($_, "Electron");
	    sectorfield($_, "PiPlus");
	    sectorfield($_, "PiMinus");
	    sectorfield($_, "Proton");
	    sectorfield($_, "KPlus");
	    sectorfield($_, "KMinus");
	    sectorfield($_, "Deuteron");
	    
	    partfield($_, "Unknown", 0);
	    partfield($_, "Gamma", 1);
	    partfield($_, "Electron", 3);
	    partfield($_, "PiPlus", 8);
	    partfield($_, "PiMinus", 9);
	    partfield($_, "Proton", 14);
	    partfield($_, "KPlus", 11);
	    partfield($_, "KMinus", 12);
	    partfield($_, "Deuteron", 45);
	}
    }
}

dbmclose(%RUN);


# what a $!^#@^ @* language

sub sectorfield{
    local($check) = $_[0];
    local($string) = $_[1];
    $newstring = "\\s*". $string . ":\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)";
    if (@fields = ($check =~ /$newstring/)){
	$i = 0;
	foreach $f (@fields){
	    $i++;
	    $key = $string . "_s" . $i;
	    $RUN{$key} = $f;
	}
    }
}

sub partfield{
    local($check) = $_[0];
    local($string) = $_[1];
    local($number) = $_[2];
    $newstring = "\\s*" . $string . "\\s*" . $number . "\\s*(\\w+)\\s*(\\w+.\\w+)";
    if ($check =~ /$newstring/){
	$key = "n_" . $string;
	$RUN{$key} = $1;
	$key = "qtrk_" . $string;
	$RUN{$key} = $2;
	print $key  . " " . $2 . "\n";
    }
}
