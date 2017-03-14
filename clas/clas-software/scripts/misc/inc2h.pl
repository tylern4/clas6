#!/usr/local/bin/perl5.002

#
# $Id : $
#        S. McAleer
#        Florida State University
#        July 9, 1998
#

# This script converts fortran header files into c header files for 
# common blocks.

# Usage: inc2h.pl -i input_file [-h]

if (@ARGV[0] =~ /-i/i) {
    if ($ARGV[1] eq "") {
        print " Please enter an input file : \n";
        chop($input_file = <STDIN>);
    } else {
        $input_file = $ARGV[1];
    }
} else {
    &print_help && exit;
}

if (-e $input_file && -r $input_file) {
    open(INPUT, $input_file);
} else {
    die " ERR: The .inc file doesn't exist or isn't readable.\n\n";
}

# Extract block and output file name frominput file name

$block = $input_file;
$block =~ s/.inc//;
$output_file = $input_file;
$output_file =~ s/.inc/.h/;

open(OUT, ">$output_file") || die " Couldn't open $output_file";

&print_header;

# Initialize counts for different types

$num{"int"} = 0;
$num{"float"} = 0;
$num{"char"} = 0;
$num{"double"} = 0;

# Loop through input file

while (<INPUT>) {
    chop;
#If comment line ignore
    if (/^c/i) { next };
# Get rid of spaces padding front of line
    $_ =~ s/^\s+//;
    if (/^PARAMETER/i) {
# Get rid of parenthesis and PARAMETER and all spaces
	$_ =~ s/(\(|\))//g;
        $_ =~ s/^PARAMETER//i;
	$_ =~ s/\s+//g;
	@line = split(/=/,$_);
# Convert all letters to uppercase
	$line[0] =~ s/(\w)/\U$1/g;
        $def{$line[0]} = $line[1];
    } 
    if (/^(LOGICAL|REAL|CHARACTER|INTEGER)/i) {
#	$_ =~ s/(\(|\))//g;
      TYPE: {
	  if (/^LOGICAL/i) {
	      $type = "int";
	      last TYPE;
	  }
	  if (/^INTEGER/i) {
	      $type = "int";
	      last TYPE;
	  }
	  if (/^REAL/i) {
	      $type = "float";
	      last TYPE;
	  }
	  if (/^CHARACTER/i) {
	      $type = "char";
	      last TYPE;
	  }
      }
	$num = $num{$type};
	$_ =~ s/^(LOGICAL|REAL|CHARACTER|INTEGER)//i;
# Grab the array size from the definition
	($tsize) = /^\*\s*\(?\s*(\d+)/;
# Convert real*8 to double
	if (($tsize == 8) && ($type =~ "float")) {
	    $type = "double";
	    $tsize = "";
        }
# Get rid of size of array and extra spaces. Convert , to spaces.
	$_ =~ s/^(\*\s*\(?\s*\d+\s*\)?)//;
	$_ =~ s/\s+//g;
        $_ =~ s/,/ /g;
	@line = split(/\s+/,$_);
# Loop through the line and convert to lowercase. Create string with form
# variable_name:size_of_array/ ... Increment type count.
        for ($i = 0 ; $i <= @line - 1; $i++) {
	    $line[$i] =~ s/(\w)/\L$1/g;
	    if ($line[$i] =~ /\w+\(\d+\)/) {
		($size) = /\((\d+)\)/;
		$line[$i] =~ s/\(\d+\)//;
	    } else {
		$size = $tsize;
	    }
	    $vars{$type} = $vars{$type}."$line[$i]:$size/";
	    $num++;
	}
	$num{$type} = $num;
    } elsif (/^\d/)  {
# Repeat same as above for lines continuations that start with a number
	$num = $num{$type};
	$_ =~ s/^\d//;
	$_ =~ s/\s+//g;
	$_ =~ s/,/ /g;
	@line = split(/\s+/,$_);
        for ($i = 0 ; $i <= @line - 1; $i++) {
	    $line[$i] =~ s/(\w)/\L$1/g;
	    if ($line[$i] =~ /\w+\(\d+\)/) {
		($size) = /\((\d+)\)/;
		$line[$i] =~ s/\(\d+\)//;
	    } else {
		$size = $tsize;
	    }
	    $vars{$type} = $vars{$type}."$line[$i]:$size/";
	    $num++;
	}
	$num{$type} = $num;
    } elsif (/^COMMON/) { last }
}       

#
# The $_ line now contains the string that starts with COMMON/BLOCK
#

&print_defs;

print OUT "\ntypedef struct {\n";

$block =~ s/(\w)/\U$1/g;
$_ =~ s/^COMMON\/$block\///;
$_ =~ s/,/ /g;
@common = split(/\s+/,$_);

while (<INPUT>) {
    if (/^c/i) { next }
    chop;
# Get rid of numbers or ampersand starting a line
    $_ =~ s/^\s+(\d|\&)\s+//;
    $_ =~ s/,/ /g;
    @line = split(/\s+/,$_);
    for ($i = 0 ; $i <= @line - 1; $i++) {
	$common[@common] = $line[$i];
    }
}

print OUT "\n";

# Loop through common block array and look for corresponding type and size

COMM: for ($i = 0 ; $i <= @common - 1 ; $i++) {
    $cvar = $common[$i];
# Remove anything inside of and including parenthesis
    $cvar =~ s/(\(.*\))//;
# If entry was NULL next
    if (length($common[$i]) < 1) { 
	next COMM;
    }
# Loop through type strings with variables' names
    foreach $type (keys(%vars)) {
	$_ = $vars{$type};
	if (/$cvar:/i) {
	    $var = $common[$i];
# Grab size from variable type string
	    ($size) = /\/$cvar:(\d*)/i;
# Convert parenthesis to brackets
   	    $var =~ s/\(/\[/;
	    $var =~ s/\)/\]/;
# Convert to lowercase
	    $var =~ s/(\w)/\L$1/g;
	    $arr_siz = $var;
# Put variable name up to [ into arr_size
	    $arr_siz =~ s/(\w+\[)//;
# If the variable name and array size are the same array size is NULL
	    if ($var =~ /\b$arr_siz\b/) {
		$arr_siz = "";
	    } else {
# If var and arr_siz are different, then $arr_siz contains array_size]
		$arr_siz =~ s/(\w)/\U$1/g;
		$arr_siz = "[$arr_siz";
	    }
# Put into $var everything before the first bracket
	    $var =~ s/(\[.*)//;
	    if ($size == "") {
		print OUT "   $type $var$arr_siz;\n";
	    } else {
		print OUT "   $type $var"."[$size]$arr_siz;\n";
	    }
	    next COMM;
	}
    }	
    
}

$block =~ s/(\w)/\L$1/g;
print OUT "\n\} $block"."_t;\n"; 
print OUT "\nextern $block" . "_t"." $block" . "_;\n\n";
close(INPUT);
close(OUT);

sub print_help {

    print " \n Usage: inc2h.pl -i include_file \n\n";

}

sub print_header {

    chop($date = `date`);
    print OUT "/* This file generated by inc2h.pl on $date */\n\n";
    print OUT "/* C representation of $block common block */\n\n";
    print OUT "/* MUST BE KEPT IN SYNCH WITH $input_file */\n\n";

}   

sub print_defs {

    foreach $key (keys (%def)) {
	print OUT "#define $key $def{$key}\n";
    }
}
