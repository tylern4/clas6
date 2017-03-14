#!/usr/local/bin/perl

# Generates a list of all the files to be cooked to help with bookkeeping

if ($ARGV[0] eq "-h" || $ARGV[0] eq "") {
    &PrintUsage;
    exit(0);
}

foreach (@ARGV) {
  if (/^-r/) {
    $min_run = $_;
    $min_run =~ s/^-r//;
  } elsif (/^-R/) {
    $max_run = $_;
    $max_run =~ s/^-R//;
  } elsif (/^-x/) {
    $exclude = $_;
    $exclude =~ s/^-x//;
    $runs_to_exclude{"$exclude"} = 1;
  } elsif (/^-o/) {
    $file = $_;
    $file =~ s/^-o//;
  } elsif (/^-d/) {
    $data_path = $_;
    $data_path =~ s/^-d//;
  }
}

print STDERR "$0: Minimum run to use is $min_run\n";
print STDERR "$0: Maximum run to use is $max_run\n";
print STDERR "$0: Results written to $file\n";
print STDERR "$0: Searching in directory $data_path\n";

open(LSCMD, "ls -1 $data_path/* |");
open(OUT, ">$file"); 

$first = 1;

while(<LSCMD>) {
  chop;
  $data = $_;
  $data =~ s/$data_path\///;
  $run = $data;
  if (!($run =~ /^clas_0+.*\.A.*/)) {
    next;
  }
  $run =~ s/^clas_0+//;
  $run =~ s/\.A.*//;
  if (($run <= $max_run) && ($run >= $min_run) && (!($runs_to_exclude{"$run"} == 1))) { 
    if ($first) {
      $old_run = $run;
      $first = 0;
    }
    if ($data =~ /^clas_0+/) {
      if ($run == $old_run) {
	print OUT "$data\n";
      } else {
	print OUT "\n$data\n";
	$old_run = $run;
      }
    }
  }
}

close(LSCMD);
close(OUT);


sub PrintUsage {
  print <<EOM;
  
  Usage: get_file_list -r# -R# -x# -x# ... -ofile

    -r#                            Minumum number to include in list
    -R#                            Maximum number to include in list
    -x#                            Run number to exclude from list
    -ofile                         File name to contain run list
    -dpath                         Path to data files

EOM
    return;
}

