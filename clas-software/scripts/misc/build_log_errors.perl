#!/usr/bin/env perl
$platform = $ARGV[0];
$logfile = $ARGV[1];
$nerror_grand = $ARGV[2];
open (ERRORS, ">>build_log_errors.log");
print ERRORS "\n------------------platform $platform-------------------\n\n";
open(LOGFILE, $logfile);
$no_of_errors = &ERROR_SEARCH($platform);
print ERRORS "Number of errors for $platform: $no_of_errors\n";
$nerror_grand += $no_of_errors;
close(LOGFILE);
print ERRORS "\nGrand total number of errors: $nerror_grand\n";
close (ERRORS);
exit $nerror_grand;

sub ERROR_SEARCH {
    local($platform_name) = $_[0];
    $#section = 0; # zero the length of the section
    $check = 1; # check for errors
    $nline_section = 0; # zero the line counter
    $error_section = 0; # no errors at start
    $nerror_section = 0; # counter of sections with errors
    while($line = <LOGFILE>) {
	$dir = &DIRECTORY_FOUND($line);
	if ($dir) {$dir_last = $dir;}
	$key = &KEYWORD_FOUND($line);
	if ($key) {
	    #print "keyword = $key\n";
	    $dir_current = $dir_last;
	    #print "dir_current = $dir_current\n";
#	    print out the last section
	    if ($nline_section && $error_section) {
		$nerror_section++;
		$iret = PRINT_SECTION($platform_name, $nerror_section,
				      *section);
		$iret = SUMMARIZE_SECTION($platform_name, $nerror_section,
					  $key_old, $dir_old, *section);
	    }
	    $key_old = $key;
	    $dir_old = $dir_current;
#	    start a new section
	    $check = 1; # check for errors
	    $#section = 0; # zero the length of the section
	    $nline_section = 0; # zero the line counter
	    $error_section = 0; # no errors at start
	    $add_lines = 1; # go ahead and add lines to the current section
	}
	if ($line =~ /not remade/) {$check = 0;} # turn off checking for errors
	# for this section if the "not remade" message is found
	if ($check) {
	    if (&ERROR_FOUND($line)) {
	    #print "error found\n$line";
		$error_section = 1;
	    }
	}
	if ($add_lines) {
#	    put the current line in the current section
	    $section[$nline_section] = $line;
#	    increment the section line counter
	    $nline_section++;
	    if ($line =~ /\*\*\*/) {$add_lines = 0;} # stop putting lines in
                                                  # section, error message
                                                  # found
	}
    }
    if ($nline_section && $error_section) {
	$nerror_section++;
	$iret = PRINT_SECTION($platform_name, $nerror_section,
			      *section);
	$iret = SUMMARIZE_SECTION($platform_name, $nerror_section,
				  $key_old, $dir_old, *section);
    }
    return $nerror_section;
}

sub KEYWORD_FOUND {
    local($string) = @_;
    $string =~ s/^\s+//; # strip leading white space
    #print "string = /$string/\n";
    if ($string =~ /^fort77 / || $string =~ /^f77 / || $string =~ /^g77 / ||
        $string =~ /^cc / || $string =~ /^gcc / ||
	$string =~ /^g\+\+ / ||
	$string =~ /^\/bin\/sh/ ||
#	$string =~ /^cd / ||
	$string =~ /^make / ||
	$string =~ /^CC / ||
	$string =~ /^CC_r4 / ||
	$string =~ /^c\+\+ / ||
#	$string =~ /^test / ||
	$string =~ /^echo / ||
	$string =~ /^egcs / ||
	$string =~ /^Making / ||
	$string =~ /^Makefile: archiving files / ||
	$string =~ /^mv / ) {
	@field = split(/\s+/,$string);
	return $field[0];
    } else {
	return 0;
    }
}

sub DIRECTORY_FOUND {
    local($string) = @_;
    if ($string =~ /Entering directory/) {
	@field = split(/`/,$string);
        @field2 = split(/'/, $field[1]);
        $rawdir = $field2[0];
        if ($rawdir =~ m|^/tmp_mnt|) {
            @field3 = split("/tmp_mnt", $rawdir);
            $dir = $field3[1];
        } else {
           $dir = $rawdir;
        }
	return $dir;
    } else {
	return 0;
    }
}

sub ERROR_FOUND {
    local($string) = @_;
    if ($string =~ /No rule to make target/
	|| $string =~ /error/i
	&& $string !~ /eserror/
	&& $string !~ /map_syserror/ && $string !~ /map_error/
	&& $string !~ /logerror/ && $string !~ /rerror/
	&& $string !~ /not remade/ && $string !~ /error_output/
	&& $string !~ /(ignored)/
	&& $string !~ /"error"/ # this is error inside double quotes
	&& $string !~ /make lockfile_error/
	&& $string !~ /int errorLine/
	&& $string !~ /f90comp:/ # exclude error summary line from sun compiler
	&& $string !~ /Scat_ErrorMsg/
	) {
	return 1;
    } else {
	return 0;
    }
}

sub PRINT_SECTION {
    local ($platform_name, $nsection, *section) = @_;
    print ERRORS "$platform_name number $nsection\n";
    print ERRORS "----------log text begins------------\n";
    foreach $print_line (@section) {
	print ERRORS $print_line;
    }
    print ERRORS "----------log text ends------------\n\n";
}

sub SUMMARIZE_SECTION {
    local ($platform_name, $nsection, $key, $directory, *section) = @_;
    #print "$platform_name number $nsection\n";
    #print "/$key/\n";
    #print "/$directory/\n";
    $action = "unknown";
    foreach $line (@section) {
	@field = split(/\s+/, $line);
	$itoken = 0;
	foreach $token (@field) {
	    if ($token eq "-o") {
		$output = $field[$itoken + 1];
		#print "output = /$output/\n";
		if ($output =~ /\.o$/) {
		    $action = "compile";
		} else {
		    $action = "link";
		}
	    }
	    $itoken++;
	}
	if ($line =~ /No rule to make target/) {
	    $action = "norule";
	    $line_norule = $line;
	    $line_norule =~ s/make\[\d+\]: \*\*\* //;
	}
    }
    if ($key eq 'Making') {
	$action = 'Making';
    }
    #print "action = $action\n";
    $date = "no date";
    $author = "no author";
    if ($action eq "link") {
	$iret = CVS_QUERY($directory, Makefile, $date, $author);
	print "$output does not link on $platform_name.\n";
    } elsif ($action eq "compile") {
	#print "look for $output\n";
	$filename = LOOK_FOR_FILENAME($output, *section);
	if ($filename eq "filename not found") {
	    print "no file name found in command, exiting. output=$output\n";
	    exit 1;
	}
	if ($author eq "no author") {
	    print "$directory/$filename does not compile on $platform_name.\n";
	} else {
	    $iret = CVS_QUERY($directory, $filename, $date, $author);
	    print "$directory/$filename does not compile on $platform_name. Last modified by $author on $date.\n";
	}
    } elsif ($action eq "norule") {
	print "$line_norule";
    } elsif ($action eq 'Making') {
	$filename = LOOK_FOR_FILENAME($output, *section);
	print "Error making $filename on $platform_name.\n";
    } elsif ($action eq 'unknown') {
#	print "Error on miscellaeous command\n";
    } else {
	die "error: action not even unknown, shutting down";
    }
}

sub LOOK_FOR_FILENAME {
    local ($output, *section) = @_;
    $filename = "filename not found";
    @field = split(/\//, $output);
    #print "$#field\n";
    @field3 = split(/\.o/, $field[$#field]);
    $filename_no_ext = @field3[0];
    $filename_no_ext =~ s/_debug$//; # strip off trailing "_debug"
#    if ($filename_no_ext eq 'rootMap_TROOTMap_sh') {
#	$filename_no_ext = 'TROOTMap';
#    }
    #print "filename_no_ext=/$filename_no_ext/\n";
    foreach $line (@section) {
	@field2 = split(/\s+/, $line);
	foreach $token (@field2) {
	    if (($token =~ /^$filename_no_ext/ # filename w/o extension
		|| $token =~ /^.\/ROOTCintDerived\/$filename_no_ext/)
		&& $token =~ /\./             # must have a .
		&& $token !~ /:$/) {          # no trailing :
		$filename = $token;
	    }
	    #print "token=/$token/, filename=/$filename/\n";
	}
    }
    #print "filename=/$filename/\n";
    return $filename;
}

sub CVS_QUERY {
    local ($directory, $filename, $date, $author) = @_;
    #print "doing cvs query on directory=/$directory/, filename=/$filename/\n";
    open (CVSLOG, "cd $directory; cvs log $filename |");
    $date_found = 0;
    while ($cvsline = <CVSLOG>) {
	if ($cvsline =~ /^date: /) {
	    if (!$date_found) {
		#print $cvsline;
		@field = split(/\s+/, $cvsline);
		$date = @field[1];
		$author = @field[4];
		#print "$date $author\n";
		$_[2] = $date;
		$_[3] = $author;
	    }
	    $date_found = 1;
	}
    }
    return $date_found;
}
