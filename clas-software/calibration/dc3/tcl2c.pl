#!/usr/bin/perl

if(@ARGV==0){
	print "You must provide the name of a tcl input file.\n";
	exit -1;
}

foreach $tcl_fname (@ARGV)
{
	$base_tcl_fname=`basename $tcl_fname`;
	$base_tcl_fname =~ ".tcl";
#	$c_fname = "$`_tcl.cc";
	$c_fname = "$`_tcl.c";
	$proc_name = "$`_tcl_proc";

	open(INFILE,$tcl_fname);
	@lines=<INFILE>;
	close(INFILE);
	
	if(@lines == 0){
		print "$tcl_fname : File emtpy or doesn't exist. Skipping.\n";
	} else {
		print "$tcl_fname -> $c_fname\n";
		open(OUTFILE,">$c_fname");

		# Insert header
		print OUTFILE "\n\nchar $proc_name\[\]=\n";
		
		for(@lines){
			# Insert backslash infront of all initial backslashes
			s/\\\n/\\\\/g;
			# Insert backslash infront of all double quotes
			s/\"/\\\"/g;
			chomp;
			print OUTFILE "\"$_\\n\"\n";
		}

		# Trailer
		print OUTFILE "\n;\n\n";
	
		close(OUTFILE);


		# Regenerate header file with this declaration
		$header_file= 'tcl_procs.h';
		`touch $header_file`; # make sure it exists
		@header=`grep -v $proc_name $header_file`;
		open(OUTFILE,">$header_file");
		print OUTFILE @header;
		print OUTFILE "extern char $proc_name\[\];\n";
		close(OUTFILE);
		
	}
	
}

exit 0;


