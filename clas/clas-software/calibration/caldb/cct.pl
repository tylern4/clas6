#!/apps/bin/perl

use CGI;
use DBI;

my $ishtml = defined $ENV{HTTP_HOST};
#my $ishtml = 1;

# shell script mode: get command line parameters
if ($ishtml == 0) {
    scalar @ARGV or $help = 1; 
    eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book
}

my $cgi   = new CGI;
my $myurl = $cgi->self_url;
#--------------------------------------------------

$hostname_a = $cgi->param('hostnameaa')  unless defined $hostname_a;
$hostname_b = $cgi->param('hostnamebb')  unless defined $hostname_b;
$it_a       = $cgi->param('runindexaa')  unless defined $it_a;
$it_b       = $cgi->param('runindexbb')  unless defined $it_b;
$r_a        = $cgi->param('runaa')       unless defined $r_a;
$r_b        = $cgi->param('runbb')       unless defined $r_b;
$mapdir     = $cgi->param('mapdir')      unless defined $mapdir;
$mapget     = $cgi->param('mapget')      unless defined $mapget;

$hostname_a = "clasdb.jlab.org"  unless defined $hostname_a;
$hostname_b = "clasdb.jlab.org"  unless defined $hostname_b;
$it_a       = "RunIndex"         unless defined $it_a;
$it_b       = "RunIndex"         unless defined $it_b;
$r_a        = 45942              unless defined $r_a;
$r_b        = $r_a               unless defined $r_b;
$r_b        = $r_a               if  $r_b =~ /same/;
$mapdir     = ""                 unless defined $mapdir;
$mapget     = ""                 unless defined $mapget;
$compare    = 'compare'          unless defined $compare;
$compare    = 'compare'          if $compare < 0;
#--------------------------------------------------
# global variables
my %aa = ( hostname => $hostname_a,
	   runindex => $it_a,
	   run      => $r_a );
my %bb = ( hostname => $hostname_b,
	   runindex => $it_b,
	   run      => $r_b );
my $isaa  = 1;
	   
my $mdir  = $mapdir;
my $staa;
my $stbb;
my $query;
my $rv;
my $type;
my $database = "calib";
my $username = "clasuser";
my $password = "";
my $verbose  = 0;
my $dbaa     = 0;
my $dbbb     = 0;
my $status;
my $samedb   = 0;
my $title = "Compare Calibration Tables";
my $comptitle;
my $method = "GET";
my $mapreader = "get_map_";
my $crlf = $ishtml == 1 ? "<p> \n" : "\n";
my $tr   = $ishtml == 1 ? "<tr>" : "";
my $tr_  = $ishtml == 1 ? "</tr>" : "";
my @privatindexlistaa;
my @privatindexlistbb;
my @ssi = ();

#--------------------------------------------------
# possible table types
my %vtype = (
	     "int"      => 1,
	     "float"    => 2,
	     "char"     => 3,
	     "intblob"  => 4,
	     "floatblo" => 5,
	     );

my @ctype = ( "???", "int", "float", "char", "int", "float" );
my @cctype = ("??", "I ", "F ", "C ", "I_", "F_" );

#--------------------------------------------------
my %col;
if ($ishtml == 1) {    
    %col = ( blue  => '<font color=blue>' , 
	     green => '<font color=green>'  , 
	     red   => '<font color=red>', 
	     bold  => '<b>',
	     text  => '</font>' );
}
else {
    %col = ( blue  => "\e[46m" , 
	     green => "\e[42m" , 
	     red   => "\e[41m\e[37m", 
	     bold  => "\e[1m",
	     text  => "\e[0m" );
}

my @negresult = ("???", 
	       $col{blue}."[no index in A]".$col{text},
	       $col{blue}."[err reading A]".$col{text},
	       $col{blue}."[no index in B]".$col{text},
	       $col{blue}."[no index in B]".$col{text},
	       $col{blue}."[err reading B]".$col{text},
	       $col{blue}."[  not in map ]".$col{text},
	       $col{blue}."[no index in A]".$col{text},
	       $col{green}."[ same values ]".$col{text},
	       $col{green}."[ same table  ]".$col{text});
#--------------------------------------------------


sub cry {
    die $_[0] unless $ishtml == 1;
    
    print $cgi->h1 ("cgi-error: $_[0]"), $cgi->end_html, "\n";
    exit();
}


sub ttable {
    $ishtml || return;
    print  ($_[0] eq end ? "</table>\n" : "<table cellspacing=6>\n");
}

sub tdata {
    my $s = "<td> " . $_[0] . "</td> \n";
    if ($ishtml == 0) { $s = sprintf ("%16s", "$_[0]"); }
    $s;
}

sub trowx {
    my $c0 = $_[1] == $_[2] ? $col{green} : $col{red};
    my $c1 = $col{text};
    my $c0h = $ishtml == 1 ? $c0 : '';
    my $c1h = $ishtml == 1 ? $c1 : '';
    my $s = $c0.$tr;
    foreach my $td ( @_ ) {
	$s .= tdata ($c0h.$td.$c1h);
    }
    $s . $tr_ . $c1 . "\n";
}
    

sub trow {
    my $s = $tr;
    foreach my $td ( @_ ) {
	$s .= tdata ($td);
    }
    $s . $tr_ . "\n";
}

sub htmlform {
    print $cgi->header, $cgi->start_html($title), $cgi->h1($title), 
    $cgi->startform(-method=>$method);

    ttable('start');
    print trow ("&nbsp", "Compare clas database", "...with database"),
    trow ("Hostname",  
	  $cgi->textfield(-name=>'hostnameaa', -size=>25, -value=>$aa{hostname}),
	  $cgi->textfield(-name=>'hostnamebb', -size=>25, -value=>$bb{hostname})),
    trow ("RunIndex", $cgi->popup_menu(-name=>'runindexaa', 
				       -values=>[ $aa{runindex}, @privatindexlistaa]).
	  $cgi->submit(-name=>'listaa', 
		       -value=>'Get list from host'),
	  $cgi->popup_menu(-name=>'runindexbb', 
			   -values=>[ $bb{runindex}, @privatindexlistbb]).
	  $cgi->submit( -name=>'listbb', -value=>'Get list from host') ),
    trow ("Run No.", $cgi->textfield(-name=>'runaa', -size=>25, -value=>$aa{run}),
	  $cgi->textfield(-name=>'runbb', -size=>25, 
			  -value=> ($bb{run} == 0 || $bb{run} == $aa{run} ? 'same as in left column' : 0) ) ),
    trow ("&nbsp; <br> &nbsp; <br> ".
	  $cgi->submit(-name=>'submit', -value=>'compare'), 
	  "&nbsp; <br> &nbsp; <br> ".
	  $cgi->submit(-name=>'help', -value=>'help'),
	  "&nbsp; <br>... or with maps in directory <br>" .
	  $cgi->textfield(-name=>'mapdir', -size=>25, -value=>$mapdir) . 
	  "<br> <br> path to get_map_float (if not in \$PATH) <br>" .
	  $cgi->textfield(-name=>'mapget', -size=>25, -value=>$mapget));
    ttable('end');

    print $cgi->endform,
	  $cgi->hr, "\n";
}

sub htmlhelp {
    print $cgi->h3("Entries in form above"),
    $cgi->ul(
      $cgi->li("<b>Left column:</b> location of calibration constants"
	       . " A to compare," 
	       . " must be a caldb database."), 
      $cgi->li("<b>Right column:</b> location of calibration constants"
	       . "B to compare,"
	       . " either a caldb database or a map directory."),
      $cgi->li("<b>Hostname:</b> the host of the caldb database,"
	       . " could be clasdb.jlab.org, any mirror (like"
	       . " solomon.physics.sc.edu) or any other mysql host."
	       . " The user clasuser must have read access granted to the"
	       . " calib database."),
      $cgi->li("<b>RunIndex:</b> the run index table to be used."
	       . " The click on the \"Get"
	       . " list from host\" button yields a list of all tables in"
	       . " calib_user to select between besides the default"
	       . " RunIndex."),
      $cgi->li("<b>Run No.:</b> run: the run number to be used."
	       . " Beside a valid integer"
	       . " number a string containing the bare word \"same\" is"
	       . " allowed in the right column."),
      $cgi->li("<b>map directory:</b> if the second target for comparison"
	       . " are map files, enter the directory where these files"
	       . " can be found (like /group/clas/parms/Maps). Leave this"
               . " entry empty if the second target is a mysql database."),
      $cgi->li("<b>path to get_map_float:</b> if you want to deal with"
	       . " maps the tools get_map_float,"
	       . " get_map_int, and get_map_char should be available in"
	       . " the search path. If you have neither permission to"
	       . " change the path nor to copy the tools in a common"
	       . " directory like /usr/local/bin you can provide the"
	       . " path for these tools in this text entry field."
	       . " It is save to leave it empty to start with, if the"
	       . " tools are not available the script will tell you...")),
	       $cgi->p, "\n";

    print $cgi->h3("Results"),
    $cgi->ul(
      $cgi->li("<b>Col 1: Name of the table.</b> Click on the name (link)"
	       . " to display the contents for the individual item and the"
	       . " differences.<br><b>Hint:</b> Going back to the overview"
               . " using the navigator buttons of your browser is much"
	       . " faster than repeating the query."),
      $cgi->li("<b>Col 2: Type of table.</b> C=character, I=int, F=float."
	       . " A trailing underscore indicates, that the values are stored"
	       . " as a binary large object in the database."),
      $cgi->li("<b>Col 3: [Number of entries].</b> If the table is of type"
	       . " character, it is the maximum length of the string."),
      $cgi->li("<b>Col 4: [Results of comparison].</b> If both"
	       . " tables exist and differences have been found, the"
	       . " $col{red} [number of different values] $col{text} "
	       . " are highlighted. Other possible"
               . " results: <br>"
	       . "$negresult[1]   no index for the given RunIndex and for"
	       . " the given Run (left column) have been "
	       . " found for this item.<br>"
	       . "$negresult[2]   the query for the values using a valid"
	       . " index wasn't successful, maybe a corrupt database?!<br>"
	       . "$negresult[3]   no index for the given RunIndex and for"
	       . " the given Run (right column) have been "
	       . " found for this item.<br>"
	       . "$negresult[5]   the query for the values using a valid"
	       . " index wasn't successful, maybe a corrupt database?!<br>"
	       . "$negresult[6]   comparing with a map, the map request"
	       . " using a get_map_xxx tool didn't return useful values.<br>"
	       . "$negresult[8]   both set of constants exist and contain the"
	       . " same values.<br>"
	       . "$negresult[9]   the RunIndex in the left and right "
	       . " column are pointing to the identical set of constants.<br>"
	       )),
	       $cgi->p, "\n";

}

sub texthelp {
    print ("$col{bold} usage:$col{text} \n",
	   "cct.pl [hostname_a=<hostname db server A>] [hostname_b=<... server B>] \\\n",
	   "       [it_a=<run index table for A>] [it_b=<... for B>] \\\n",
	   "       [r_a=<run A>] [r_b=<run B>] [mapdir=<map directory>] \\\n",
	   "       [mapget=<directory get_map_tools>] [compare=<ItemId>] [help=<non-zero>] \n\n");

    print ("$col{bold} defaults:$col{text} \n",
	   "       hostname_a, hostname_b = \"clasdb.jlab.org\"\n",
	   "       it_a, it_b             = \"RunIndex\"\n",
	   "       r_a                    = 45942\n",
	   "       r_b                    = \"same\"  [bare word same or number]\n",
	   "       mapdir                 = \"\"      [database rather than map]\n",
	   "       mapget                 = \"\"      [search maptools in \$PATH]\n",
	   "       compare                undef     [compare all cldb items A]\n",
	   "       help                   undef     [help invoked if no param]\n\n");
    print ("$col{bold} example:$col{text} \n",
	   "cct.pl r_a=30222 hostname_b=localhost it_b=calib_user.RunIndexe1_6\n\n"); 

    print ("$col{bold} results:$col{text} \n");
    print (
	   "$col{bold} Col 1: Name of the table.$col{text} \n",
	   "$col{bold} Col 2: ItemId.$col{text} Use this number as a value for the compare option\n",
	   "        to browse the difference for an individual item.\n",
	   "$col{bold} Col 3: Type of table.$col{text} C=character, I=int, F=float. A trailing _ indicates\n",
	   "        that the values are stored as a blob in the database.\n",
	   "$col{bold} Col 4: [Number of entries]$col{text} If the table is of type",
	   " character, it is the\n",
	   "        maximum length of the string.\n",
	   "$col{bold} Col 5: [Results of comparison]$col{text} If both tables exist and differences\n",
           "        have been found, the $col{red} [number of different values]$col{text} are highlighted.\n", ,
	   "Other possible results: \n",
	   "$negresult[1] no index RunIndex,run (left column) found for this item.\n",
	   "$negresult[2] unsuccessful query for the index given by RunIndex.\n",
	   "$negresult[3] same as above for right column.\n",
	   "$negresult[5] same as above for right column.\n",
	   "$negresult[6] map request using get_map_xxx didn't return useful values.\n",
	   "$negresult[8] both set of constants exist and contain the same values.\n",
	   "$negresult[9] indices are pointing to the identical set of constants.\n");
    exit(0);
}

sub ConnectToServer {

    my ($hostname, $user, $password, $database, $verbose, $dbhref) =  @_;

# connect to MySQL database on localhost

    if ($verbose) {print "hostname=$hostname, user=$user\n";}

    my $dbh = DBI->connect("DBI:mysql:$database:$hostname", $user, $password);

    if (defined $dbh) {
	if ($verbose) {print "Connection successful: handle: $dbh\n";}
    } else {
	cry ("Could not connect to the database server $hostname");
    }

# return the connection handle

    $$dbhref = $dbh;

}

sub list_runindex {
    my ($host, $ptr) = @_;
    my $dbh;
    @$ptr = ();
    ConnectToServer ($host, 'clasuser', '', 'calib_user', 0, \$dbh);
    my $stinx = $dbh->prepare("show tables") 
	or cry ("Can't prepare query: show tables\n");
    my $rvinx = $stinx->execute
	or cry ("Can't execute show tables \n error: $stinx->errstr\n");

    my $i = 0;
    $$ptr[$i++] = 'RunIndex';
    while ( my @resultinx = $stinx->fetchrow_array ) {
	$$ptr[$i++] = "calib_user." . $resultinx[0];
    }
}

list_runindex ($cgi->param('hostnameaa'), \@privatindexlistaa )
    if ( $cgi->param('listaa') eq 'Get list from host');

list_runindex ($cgi->param('hostnamebb'), \@privatindexlistbb )
    if ( $cgi->param('listbb') eq 'Get list from host');



$ishtml && htmlform();

htmlhelp() 
    if ($cgi->param('help') eq 'help');

$help && texthelp();

#--------------------------------------------------

sub DO_IT {    

    my($dbh) = @_;
    if ($isaa == 1) {
	$staa = $dbh->prepare($query)
	    or cry ("Can't prepare $query: $dbh->errstr\n");
	$rv = $staa->execute
	    or cry ("Can't execute the query $query\n error: $staa->errstr\n");
    
    }
    else {
	$stbb = $dbh->prepare($query)
	    or cry ("Can't prepare $query: $dbh->errstr\n");
	$rv = $stbb->execute
	    or cry ("Can't execute the query $query\n error: $stbb->errstr\n");
    
    }

    return 0;

}



{package SSI;
 sub new {
     my $class = shift;
     my $self  = {
	 s    => $_[0],
	 ss   => $_[1],
	 i    => $_[2],
	 id   => $_[3],
	 len  => $_[4],
	 type => $vtype{$_[5]},
	 aa   => -1,
	 bb   => -1,
	 whoa => "",
	 whob => "",
	 comp => 0,
	 
     };
     if ($self->{type} == 0) {
	 cry ("unknown date type: $_[5]");
     }
     return bless $self, $class;
 }
 sub getName {
     my $c = shift;
     "$c->{s}_$c->{ss}_$c->{i}";
 }
 sub getLength {
     my $c = shift;
     $c->{len};
 }
 sub getSystem {
     my $c = shift;
     $c->{s};
 }
 sub getSubsystem {
     my $c = shift;
     $c->{ss};
 }
 sub getItem {
     my $c = shift;
     $c->{i};
 }
 sub getType {
     my $c = shift;
     $c->{type};
 }
 sub getCompare {
     my $c = shift;
     $c->{comp};
 }
 sub getId {
     my $c = shift;
     $c->{id};
 }
 sub setIndex {
     my $c = shift;
     my $inx = $_[0];
     if ($isaa == 1) {
	 $c->{aa} = $inx;
     }
     else {
	 $c->{bb} = $inx;
     }
 }
 sub getIndex {
     my $c = shift;
     $_[0] == 0 ? $c->{aa} : $c->{bb} 
 }

 sub parseValues {
     my $c = shift;
     my ($ptr, $array) = @_;

#character type--------------------
     if ($c->{type} == 3) {
	 $$ptr[0] = $$array[0];
     }
     elsif ($c->{type} < 3) {
#int or float
	 @{$ptr} = @{$array};
     }
#blobs
     else {
	 my $blobaa = $$array[0];
	 $blobaa =~ s/^\s+//; # strip off leading white space
	 @{$ptr} = split(/\s+/, $blobaa);
     }
 }

 sub compareValues {
     my $c = shift;
     my ($ptraa, $ptrbb, $call) = @_;

#same index -> identical table
     if ($samedb == 1 && $call eq 'compare' && 
	 $c->{aa} > 0 && $c->{aa} == $c->{bb}) {
	$c->{comp} = -9;
	return;
     }

#no index entry in A
     if ($c->{aa} < 0) {
	 $c->{comp} = -1;
	 return;
     } 

     my @resultaa;
     my @resultbb;

     my $querycore = "select author";

     # check for blobs
     my $qlen = $c->{type} > 2 ? 1 : $c->{len};
     for (my $i = 0; $i < $qlen; $i++) {
	 $querycore .= sprintf (",v_%04d", $i+1);
     } 
     $querycore .= " from ". $c->getName() ;

     $isaa = 1;
    
     $query = "$querycore where itemValueId = $c->{aa}";
     $staa = $dbaa->prepare($query)
	 or cry ("Can't prepare $query: $dbaa->errstr\n");
     $rv = $staa->execute
	 or cry ("Can't execute the query $query\n error: $staa->errstr\n");

#problems to get any values upon the query
     unless (@resultaa = $staa->fetchrow_array) {
	 $c->{comp} = -2;
	 return;
     }

     $c->{whoa} = shift @resultaa;

     $c->parseValues($ptraa, \@resultaa);
#------------------------------------

#identical as above, but we want to get the values for display
     if ($samedb == 1 && $c->{aa} > 0 && $c->{aa} == $c->{bb}) {
	$c->{comp} = -9;
	return;
     }

     $isaa = 0;

#read second database
     if ($mdir eq "") {
	 if ($c->{bb} < 0) {
	     $c->{comp} = -2 + $c->{bb};
	     return;
	 } 

	 $query = "$querycore where itemValueId = $c->{bb}";
	 $stbb = $dbbb->prepare($query)
	     or cry ("Can't prepare $query: $dbbb->errstr\n");
	 $rv = $stbb->execute
	     or cry ("Can't execute the query $query\n error: $stbb->errstr\n");

	 unless (@resultbb = $stbb->fetchrow_array) {
	     $c->{comp} = -5;
	     return;
	 }

	 $c->{whob} = shift @resultbb;

	 $c->parseValues($ptrbb, \@resultbb);
     }
#it is a map
     else {
	 my $command = $mapreader . $ctype[$c->{type}] 
	     . " -m" . $mdir . "/" . $c->{s} . ".map "
	     . " -s" . $c->{ss}
	     . " -i" . $c->{i}
	     . " -l" . $c->{len}
	     . " -t" . $bb{run} . " 2> /dev/null";
	 
#	 print $cgi->h3($command);
	 chop (@resultbb = `$command`);

#	 print ('(', join(') (', @resultbb), ") <p>\n");

	 my $explen = $c->{type} == 3 ? 1 : $c->{len};
	 unless (scalar @resultbb == $explen) {
	     $c->{comp} = -6;
	     return;
	 }

	 @{$ptrbb} = @resultbb;
     }

     $isaa = 1;

#at this points: results stored in ptraa and ptrbb
	 

#character type, use eq rather than ==
     if ($c->{type} == 3) {
	 $c->{comp} = $ptraa[0] eq $ptrbb[0] ? -8 : 1;
     }
     else {
	 my $ndiff = 0;

	 for (my $i = 0; $i < $c->{len}; $i++) {
	     if ($$ptraa[$i] != $$ptrbb[$i]) {
		 $ndiff++;
	     }
	 }
	 $c->{comp} = $ndiff == 0 ? -8 : $ndiff;
     } 
 } 

};

sub get_single_item {
    my ($dbh, $itemId) = @_;

    $query  = "select systemName,subsystemName,itemName,itemId,length,type";
    $query .= " from System left join Subsystem using (systemId)";
    $query .= " left join Item using (subsystemId)";
    $query .= " where itemId = $itemId";

# prepare and execute the query 

    &DO_IT($dbh);

    
    if (my @result = $staa->fetchrow_array) {
	if (scalar @result != 6 ) {
	    cry ("get_all_items: got invalid row");
	} 
	
	push @ssi, SSI->new(@result);
    }
    else {
	cry ("itemId $itemId not found in Item table");
    }
    return 0;
}

sub get_all_items {
    my($dbh) = @_;

    $query  = "select systemName,subsystemName,itemName,itemId,length,type";
    $query .= " from System left join Subsystem using (systemId)";
    $query .= " left join Item using (subsystemId)";
    $query .= " order by systemName,subsystemName,itemName";

# prepare and execute the query 

    &DO_IT($dbh);

    
    while (my @result = $staa->fetchrow_array) {
	if (scalar @result != 6 ) {
	    cry ("get_all_items: got invalid row");
	} 
	
	push @ssi, SSI->new(@result);
    }

    return 0;
}

sub get_itemvalueid {
    my($dbh, $run, $runIndexTable, $itemId) = @_;

    $query  = "select itemValueId from $runIndexTable";
    $query .= " where itemId = $itemId and minRun <= $run and maxRun >= $run";
    $query .= " order by time desc limit 1";

    &DO_IT($dbh);

    if ($isaa == 1) {
	if (my @result = $staa->fetchrow_array) {
	    return $result[0];
	}
    }
    else {
	if (my @result = $stbb->fetchrow_array) {
	    return $result[0];
	}
    }
    return -1;
}

sub connect_both {
    $status = ConnectToServer($aa{hostname}, $username, $password, 
			  $database, $verbose, \$dbaa);
    printf ("connected to %s: runindex %s, run %d %s", 
	    $aa{hostname}, $aa{runindex}, $aa{run}, $crlf);
#compare with database on same host
    if ($mdir eq "") {
	$samedb =  $aa{hostname} eq $bb{hostname}  ? 1 : 0 ;
    }

    if ($samedb == 1) {
	printf ("same host: runindex %s, run %d %s",
		$bb{runindex}, $bb{run}, $crlf);
	$dbbb = $dbaa; return;
    }

#compare with database on different host
    if ($mdir eq "") {
	$status = ConnectToServer($bb{hostname}, $username, $password, 
			      $database, $verbose, \$dbbb);   
	printf ("connected to %s: runindex %s, run %d %s", 
		$bb{hostname}, $bb{runindex}, $bb{run}, $crlf);
    }
#compare with map files
    else {
	if ($mapget eq "") {
	    !system "which get_map_float > /dev/null 2> /dev/null" 
		or cry ("Can't find or access get_map_float to read the maps");
	}
	else {
	    -x "$mapget/get_map_float" 
		or cry ("Can't find or execute $mapget/get_map_float");
	    $mapreader = "$mapget/get_map_";
	}
	    
	print "maps in directory $mdir $crlf";
	my @maplist = `ls $mdir/*.map` =~ m/([^\/]*)\.map/g ;
	print join (", ", @maplist), $crlf;
    }
}

sub compare_database {
    my $itemId = @_[0];
    my @values_aa;
    my @values_bb;

    connect_both();

    if ($itemId eq 'compare') {
	get_all_items ($dbaa);
	$comptitle = "Compare all tables";
    }
    else {
	get_single_item ($dbaa, $itemId);
	$comptitle = "Compare [" . $ssi[0]->getName . "] [" . 
	    $ssi[0]->getSubsystem . "] [" . $ssi[0]->getItem . "]";
    }

    print $cgi->hr, $cgi->h3($comptitle) if ($ishtml == 1);
    ttable('start');

#    print $cgi->h3($cgi->url(-full=>1, -query=>1));
#    print $cgi->h3($myurl) ,"\n";
   

    foreach my $z (@ssi) {
	$isaa = 1;
	$z->setIndex(get_itemvalueid($dbaa, $aa{run}, 
				     $aa{runindex}, $z->getId()) );

	$isaa = 0;
	if ($mdir eq "") {
	    $z->setIndex(get_itemvalueid($dbbb, $bb{run}, 
					 $bb{runindex}, $z->getId()) );
	}

	$z->compareValues(\@values_aa, \@values_bb, $itemId);

	my $xresult =  $z->getCompare() < 0 ? $negresult[-$z->getCompare()] :
	    sprintf ($col{red}."[differs: %4d]".$col{text}, $z->getCompare());
	    
	if ($ishtml == 1) {
	    my $iid = $z->getId();
	    $myurl =~ s/submit=\w*/submit=$iid/;
	    print trow ( "<a href=\"$myurl\"> " . $z->getName() . " </a>  " , 
		    $cctype[$z->getType()]."[".$z->getLength()."]  ",  $xresult );
	}
	else {
	    printf ("%-42s %3d  %s[%4d]   %s\n", $z->getName() , $z->getId(),
		    $cctype[$z->getType()], $z->getLength(),  $xresult );

	}
    }

    print ("\n");

    ttable('end');

    if ($itemId ne 'compare') {
#	print join ("," , @values_aa), " <p>\n";
	ttable('start');
	foreach my $i (0..$#values_aa) {
	    if ($ssi[0]->getCompare() > 0) {
		print trowx ( "[$i]", $values_aa[$i], $values_bb[$i]) ;
	    }
	    else {
		print trow ( "  [$i]   ", "  ". $values_aa[$i]."  ");
	    }
	}
	ttable('end');	
    } 
}

if (defined  $cgi->param('submit')) {
    compare_database ($cgi->param('submit'));
}

if ($ishtml == 1) {
    print   "<address><a href=\"mailto:langhei\@physics.sc.edu\">",
    "<font size=-2><b>bugs, problems, feedback</b></font></a></address>",
    $cgi->end_html, "\n";
}
else {
    compare_database ($compare);
}
    
