#!/usr/local/bin/perl

local($clas_bin) = $ENV{"CLAS_BIN"};
local($clas_parms) = $ENV{"CLAS_PARMS"};
local($home) = $ENV{"HOME"};
$sc_map = "SC_CALIBRATIONS.map";
@t0 = (-10.0, 10.0);
@t1 = (0.045, 0.055);
$t2 = 1.0E-5;
@ped = (10.0, 800.0);
@side = ("left", "right");
@list = ("T0_TDC", "T1", "T2", "pedestals");


print "\nThis script will check out the problems in the SC Map:\n";
print "      $clas_parms/Maps/$sc_map \n";
print "for a given Run and output the summary on the screen and\n";
print "in the text file:\n";
print "      sc<run>.trouble\n\n"; 
print "Used good limits for the constants:\n\n";
print "      $list[0]: from $t0[0] to $t0[1],\n";
print "      $list[1]: from $t1[0] to $t1[1],\n";
print "      $list[2]: less than $t2,\n";
print "      $list[3]: from $ped[0] to $ped[1],\n\n";



print "So, what is the Run number ? \n";
$runno = <STDIN>;
chomp($runno); 

print "\n";

open(OUT,">sc$runno.trouble") || die "Can't open the output !";
  
print OUT "\n   The Map: $clas_parms/Maps/$sc_map \n\n";
print OUT "    Used good limits for the constants:\n\n";
print OUT "      $list[0]: from $t0[0] to $t0[1],\n";
print OUT "      $list[1]: from $t1[0] to $t1[1],\n";
print OUT "      $list[2]: less than $t2,\n";
print OUT "      $list[3]: from $ped[0] to $ped[1],\n\n";
print OUT "\n       TROUBLES for the Run $runno : \n\n";

$i = 0;
until($i == 4){

$constant = $list[$i];

$j = 0;
until($j == 2){

$count = 0;
$item = $side[$j];

print "\n Checking the constants: $constant, $item \n\n";
print OUT "\n Checking the constants: $constant, $item \n\n";

open(CMD,"$clas_bin/get_map_float -m$clas_parms/Maps/$sc_map -s$constant -i$item -t$runno |") 
     || die "Can't read the Map !";

while (<CMD>){

$check = $_;
chomp($check);
$count++; 

    if(($constant eq "T0_TDC") && ($check < $t0[0] || $check > $t0[1])){
	print " * Problem in channel $count (run $runno): $constant = $check \n";
        print OUT " * Problem in channel $count (run $runno): $constant = $check\n";
    }elsif(($constant eq "T1") && ($check < $t1[0] || $check > $t1[1] )){
	print " * Problem in channel $count (run $runno): $constant = $check \n";
	print OUT " * Problem in channel $count (run $runno): $constant = $check\n";
    }elsif(($constant eq "T2") && ($check > $t2)){
	print " * Problem in channel $count (run $runno): $constant = $check \n";
	print OUT " * Problem in channel $count (run $runno): $constant = $check\n";
    }elsif(($constant eq "pedestals") && ($check < $ped[0] || $check > $ped[1])){
	print " * Problem in channel $count (run $runno): $constant = $check \n";
	print OUT " * Problem in channel $count (run $runno): $constant = $check\n";
    }

}

close(CMD);
print "\n";

$j++;
}

$i++;
}

close(OUT);









