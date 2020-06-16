#!/apps/bin/perl

# plot_strip_chart takes as arguments two run numbers as limits.
#      If you don't specify a run number you need to specify the
#      command line arguments for beam energy and main torus
#      current. The result of this code is a postscript file produced
#      by gnuplot.

######## Must be run on linux machine ###############

# Usage: plot_strip_chart.pl [-h][-R<run#>][-r<run#>][-f<file>][-d<dbdir>][-b<beam.t>][-t<torus_current>][-nf][-ne][-A][-S] ARGS
#        -h              Print this help message.
#        -R<run#>        Maximum run number to include in plot.
#        -r<run#>        Minimum run number to include in plot.
#        -f<file>        Base file name for output (Default: psc_out).
#        -d<dbdir>       Path to database directory.
#        -b<beam.t>      Plot only runs with this beam energy in MeV.
#        -t<torus>       Plot only runs with this torus current.
#        -nf             Normalize results to Faraday cup.
#        -ne             Normalize results to # of events written.
#        -A              Average over all file extensions.
#        -S              Plot variables on same page.
#        ARGS            List of variables to plot on same graph.
#        -y<ymin>        Set min Y range     
#        -Y<ymax>        Set max Y range     
#        -l              use logarithmic scale

if ($ARGV[0] eq "-h") {
    &PrintUsage;
    exit(0);
}

# Set default variables
@run_list = ();
@var_list = ();
$dbdir = "/work/clas/pdisk4/clarisse/pass0/v1/database/";
$beam = 0;
$torus = 0;
$run_min = 1;
$run_max = 999999;
$file = "psc_out";
$avg = 0;
$same = 1;
$ymin=0;
$ymax=$ymin;
$logscale=0;

# Parse through command line
foreach (@ARGV) {
    if (/^-R/) {
	$run_max = $_;
	$run_max =~ s/^-R//;
    } elsif (/^-l/) {
	$logscale=1;
    } elsif (/^-y/) {
	$ymin = $_;
	$ymin =~ s/^-y//;
    } elsif (/^-Y/) {
	$ymax = $_;
	$ymax =~ s/^-Y//;
    } elsif (/^-r/) {
	$run_min = $_;
	$run_min =~ s/^-r//;
    } elsif (/^-d/) {
	$dbdir =$_;
	$dbdir =~ s/^-d//;
    } elsif (/^-b/) {
	$beam = 1;
	$beam_E = $_;
	$beam_E =~ s/^-b//;
    } elsif (/^-t/) {
	$torus = 1;
	$torus_A = $_;
	$torus_A =~ s/^-t//;
    } elsif (/^-f/) {
	$file = $_;
	$file =~ s/^-f//;
    } elsif (/^-nf/) {
	$norm_farad = 1;
    } elsif (/^-ne/) {
	$norm_events = 1;
    } elsif (/^-A/) {
	$avg = 1;
    } elsif (/^-S/) {
	$same = 0;
    } else {
	push(@var_list, $_);
    }
}

print "\n$0: Output file names $file.txt and $file.ps.\n";
print "$0: Database directory to be used is $dbdir.\n";
print "$0: Minimum run number: $run_min.\n";
print "$0: Maximum run number: $run_max.\n";
if ($beam) { print "$0: Beam energy for search: $beam_E.\n" }
if ($torus) { print "$0: Main torus current for search: $torus_A.\n\n" }

print "$0: Variables to plot against run number: \n";

foreach (@var_list) {
    print "\t\t$_\n";
}


$lscmd = "ls -1 $dbdir/*.db |";

open(LS, $lscmd) || die "\n$0: $!\n";

while (<LS>) {
    $run = $_;
    chop($run);
    $run =~ s/\.db$//;
    if ($beam || $torus) {
	dbmopen(%RUN, "$run", 0644) || warn "$0: Couldn't open $run\n";
	if ($beam && !(($RUN{"beam energy"} + 40 >= $beam_E) && ($RUN{"beam energy"} - 40 <= $beam_E))) { 
	    dbmclose(%RUN);
	    next;
	}
	if ($torus && !(($RUN{"itorus"} + 100 >= $torus_A) && ($RUN{"itorus"} - 100 <= $torus_A))) { 
	    dbmclose(%RUN);
	    next;
	}
	dbmclose(%RUN);
    }
    $run =~ s/$dbdir\/+clas_0*//;
    $run =~ s/\.A\d\d.*$//;
    if ($run >= $run_min && $run <= $run_max) {
	if ($run != $run_list[$#run_list]) {
	    push(@run_list, $run);
	}
    }
}

close(LS);

# Test for existence of files. If they already exist delete them

if (-e "$file.txt") {
  $rmcmd = "rm $file.txt | ";
  open(RM, $rmcmd) || die "\n$0: Could not remove existing $file.txt.\n Please delete and run $0 again.\n";
  while(<RM>) {
    print $_;
  }
}

if (-e "$file.gnu") {
  $rmcmd = "rm $file.gnu | ";
  open(RM, $rmcmd) || die "\n$0: Could not remove existing $file.gnu.\n Please delete and run $0 again.\n";
  while(<RM>) {
    print $_;
  }
}

if (-e "$file.ps") {
  $rmcmd = "rm $file.ps | ";
  open(RM, $rmcmd) || die "\n$0: Could not remove existing $file.ps.\n Please delete and run $0 again.\n";
  while(<RM>) {
    print $_;
  }
}

# Print file header

open(OUT, ">>$file.txt") || die "\n$0: Couldn't open $file.\n";

print OUT "# Run Number ";
foreach $var (@var_list) {
    print OUT "\t$var ";
}
print OUT "\n";

close(OUT);

# Get and print info for each run
    
foreach $run (@run_list) {
    $num_files{$run} = 0;
# Initialize hash
    foreach $var (@var_list) {
      $entry{"$var"} = 0;
    }
    $entry{"GFC"} = 0;
    $entry{"NEW"} = 0;

    print $run."\n";
    $lscmd = "ls -1 $dbdir/*$run*db |";
    open(LS, $lscmd) || die "\$0: $!\n";

    while (<LS>) {
      $num_files{$run} += 1;
      chop;
      $dbfile = $_;
      $dbfile =~ s/\.db//;
      
      dbmopen(%RUN, "$dbfile", 0644) || warn "\n$0: $!\n";
      
      $first = 0;
      
      foreach $var (@var_list) {
	$temp = $RUN{$var};
	if ($norm_farad && !$first) {
	  $un_farad_coul = $RUN{"Ungated FC Coulombs"};
	  $un_farad_coul =~ /(\d+\.\d+)e-(\d+)/;
	  $un_farad_coul = $1 * (10**(-$2));
	  $gate_farad_coul = $un_farad_coul * ($RUN{"Clock Live time %"}/100);
	  if ($gate_farad_coul == 0) {
	    next;
	  } else {
	    $entry{"GFC"} +=  $gate_farad_coul;
	  }
	} elsif ($norm_events && !$first) {
	  if  ($RUN{"# of events written"} == 0) {
	    next;
	  } else {
	    $entry{"NEW"} += $RUN{"# of events written"};
	  }
	}
	$entry{$var} += $temp;
	$first = 1;
      }
      dbmclose(%RUN);
    }
    close(LS);
    
    open(OUT, ">>$file.txt") || die "\n$0: Couldn't open $file.\n";
    
    print OUT "$run ";
    foreach $var (@var_list) {
     if ($norm_farad) {
       $entry{$var} = $entry{$var} / $entry{"GFC"};
     }
     if ($norm_events) {
       $entry{$var} = $entry{$var} / $entry{"NEW"};
     }
      if ($avg) {
	$entry{$var} = $entry{$var} / $num_files{$run};
      }
      print OUT "\t$entry{$var} ";
    }
    print OUT "\n";
    close(OUT);
}

# Create GNUPLOT command file

open(GNUOUT, ">$file.gnu") || warn "$0: Couldn't open $file.gnu\n";

print GNUOUT "set term postscript\n";
if ($ymin < $ymax){
    print GNUOUT "set yrange [$ymin:$ymax];\n";
}
if ($logscale==1){
    print "using log scale \n";
    print GNUOUT "set logscale y \n";
}
print GNUOUT "set output \"$file.ps\"\n\n";


$first = 1;
$count = 2;
foreach $var (@var_list) {
    if ($first || $same) {
	print GNUOUT "plot \"$file.txt\" using 1:$count title '$var' with points\n";
	$count += 1;
	$first = 0;
    } else {
	print GNUOUT "replot \"$file.txt\" using 1:$count title '$var' with points\n";
	$count += 1;
    }
}
print GNUOUT "exit\n";

close(GNUOUT);

# Plot them using gnuplot

system("gnuplot $file.gnu");


sub PrintUsage {

    print "\nUsage: $0 [-h][-R<run#>][-r<run#>][-f<file>][-d<dbdir>][-b<beam.t>][-t<torus_current>][-nf][-ne][-A][-S][-y<ymin> -Y<ymax>][-l] ARGS\n";
    print "\t-h \t\tPrint this help message.\n";
    print "\t-R<run#> \tMaximum run number to include in plot.\n";
    print "\t-r<run#> \tMinimum run number to include in plot.\n";
    print "\t-f<file> \tBase file name for output.\n";
    print "\t-d<dbdir> \tPath to database directory.\n";
    print "\t-b<beam.t> \tPlot only runs with this beam energy in MeV.\n";
    print "\t-t<torus> \tPlot only runs with this torus current.\n";
    print "\t-nf \t\tNormalize results to Faraday cup.\n";
    print "\t-ne \t\tNormalize results to # of events written.\n";
    print "\t-A \t\tAverage over all of the file extensions.\n";
    print "\t-S \t\tPlot variables on same page.\n";
    print "\t-y<ymin> \t\tset yrange minimum to ymin if ymin<ymax, default 0.0 \n";
    print "\t-Y<ymax> \t\tset yrange maximum to ymax, default 0.0 \n";
    print "\t-l \t\tset yrange to logscale\n";
    print "\tARGS \t\tList of variables to plot on same graph.\n\n";
}




