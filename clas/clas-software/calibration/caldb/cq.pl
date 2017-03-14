#! /apps/bin/perl
# $Id: cq.pl,v 1.31 2001/12/13 22:36:23 marki Exp $

use CGI;                             # load CGI routines

use CaldbUtil;

$ndata_per_row = 10;

$user = "clasuser";
$password = "";
$database = "calib";

$method = "GET";

if (!$hostname){
    $hostname = "clasdb.jlab.org";
}

$q = new CGI;                        # create new CGI object

&GetInputParameters;

&GetSSILists;

if (!$button || $button eq 'Limit Choices') {
    &MakeMainPage;
} elsif ($button eq 'Reset Parameters') {
    $q->delete_all();
    &GetSSILists;
    &MakeMainPage;
} elsif ($button eq 'Sets by Run') {
    &MakeValidItemsPage;
} elsif ($button eq 'Sets by Item') {
    &MakeItemValuesByItemPage;
} elsif ($button eq 'Values by Run') {
    &MakeItemValuePage;
} elsif ($button eq 'Values by ID') {
    &MakeItemValueByIndexPage;
} elsif ($button eq 'Run Ranges') {
    &MakeRunRangesPage;
} elsif ($button eq 'Run History') {
    &MakeRunHistoryPage;
} else {
    &MakeExcusesPage;
}

exit 0;

#################################################################

sub GetSSILists {

    if ($q->param('system') eq 'none') {
	$system = '';
    } else {
	$system = $q->param('system');
    }
    if ($q->param('subsystem') eq 'none') {
	$subsystem = '';
    } else {
	$subsystem = $q->param('subsystem');
    }
    if ($q->param('item') eq 'none') {
	$item = '';
    } else {
	$item = $q->param('item');
    }
    $status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			      \$dbh);
    $status = ShowSystems($dbh, $subsystem, $item, \@systemArray);
    $status = ShowSubsystems($dbh, $system, $item, \@subsystemArray);
    $status = ShowItems($dbh, $system, $subsystem, \@itemArray);

}

#################################################################

sub MakeMainPage {

    $title = "Calibration Database Query Page";
    $td_required = "<td align=center bgcolor=lightgreen>required";
    $td_optional = "<td align=center bgcolor=lightyellow>optional";
    $td_ignored = "<td align=center bgcolor=lightpink>ignored";
    print $q->header,
    $q->start_html($title), # start the HTML
    $q->h1($title),         # level 1 header
    $q->p,
    "Column headings are queries. Row headings are input parameters.",
    " Table cells indicate whether the input parameter is required, ignored",
    " or optional for the query.",
    " The \"Limit Choices\" buttons limit pull-down menus to self-consistent",
    " choices for system, subsystem and item.",
    " Explanations of queries and input parameters follow the table.",
    $q->startform(-method=>$method),
    "<table border>\n<tr>\n<th>columns: Queries-><br>",
    $q->submit(-name=>'button', -value=>'Reset Parameters'),
    "<br>rows: Parameters<br>|<br>V<br>",
    "\n<td>",
    $q->submit(-name=>'button', -value=>'Sets by Run'),
    "\n<td>",
    $q->submit(-name=>'button', -value=>'Sets by Item'),
    "\n<td>",
    $q->submit(-name=>'button', -value=>'Values by Run'),
    "\n<td>",
    $q->submit(-name=>'button', -value=>'Values by ID'),
    "\n<td>",
    $q->submit(-name=>'button', -value=>'Run Ranges'),
    "<br>",
    "min run (optional)",
    $q->textfield(-name=>'runMin', -size=>8),
    "<br>",
    "max run (optional)",
    $q->textfield(-name=>'runMax', -size=>8),
    "\n<td>",
    $q->submit(-name=>'button', -value=>'Run History'),
    "\n<tr><th>system:",
    $q->popup_menu(-name=>'system', -values=>[ "none", @systemArray ]),
    $q->submit(-name=>'button', -value=>'Limit Choices'),
    "$td_ignored $td_required $td_required $td_required $td_required $td_required\n",
    "<tr><th>subsystem:",
    $q->popup_menu(-name=>'subsystem', -values=>[ "none", @subsystemArray ] ),
    $q->submit(-name=>'button', -value=>'Limit Choices'),
    "$td_ignored $td_required $td_required $td_required $td_required $td_required\n",
    "<tr><th>item:",
    $q->popup_menu(-name=>'item', -values=>[ "none", @itemArray ] ),
    $q->submit(-name=>'button', -value=>'Limit Choices'),
    "$td_ignored $td_required $td_required $td_required $td_required $td_required\n",
    "<tr><th>run: ",
    $q->textfield(-name=>'run'),
    "$td_required $td_ignored $td_required $td_ignored $td_ignored $td_required\n",
    "<tr><th>const. set ID: ",
    $q->textfield(-name=>'itemValueId'),
    "$td_ignored $td_ignored $td_ignored $td_required $td_ignored $td_ignored\n",
    "<tr><th>date: ",
    $q->textfield(-name=>'date'),
    "$td_optional $td_ignored $td_optional $td_ignored $td_optional $td_optional\n",
    "<tr><th>run index table: ",
    $q->textfield(-name=>'runIndexTable'),
    "$td_optional $td_ignored $td_optional $td_ignored $td_optional $td_optional\n",
    "<tr><th>hostname: ",
    $q->textfield(-name=>'hostname'),
    "$td_optional $td_optional $td_optional $td_optional $td_optional $td_optional\n",
    "</table>\n",
    $q->p,
    "The queries are:",
    $q->ol(["<b>Sets by Run:</b> For a given run, list all system/subsystem/items that have valid constant sets for that run.",
	    "<b>Sets by Item:</b> For a given system/subsystem/item, list all constant sets in the database, including those not linked to run ranges.",
	    "<b>Values by Run:</b> Display the calibration constant values for the given system/subsystem/item and run.",
	    "<b>Values ID:</b> Display the calibration constant values for the given system/subsystem/item and constant set ID.",
	    "<b>Run Ranges:</b> Shows all run ranges defined for the specified system/subsystem/item. Optionally a run range restriction can be specified.",
	    "<b>Run History:</b> Shows the history of changes of the constants for the specified system/subsystem/item and run."]),
    "The optional input parameters are:",
    $q->ol(["<b>date:</b> The query will reflect the status of the database as of this date. The format is \"YYYY-MM-DD\". The default is \"2037-01-01\" which is equivalent to asking for the current time.", 
	    "<b>run index table:</b> The query will use this table to make the correlation between run number and calibration constants. The default is \"calib.RunIndex\".",
	    "<b>hostname:</b> The query will use this host as the database server. The default is \"clasdb.jlab.org\""]);
    
    print $q->endform;
    
    print $q->end_html;                  # end the HTML
    
    print "\n";

}

#################################################################

sub MakeItemValuePage {

    $title = "Calibration Database Item Value";
    print $q->header,
    $q->start_html($title), # start the HTML
    $q->h1($title),         # level 1 header
    $q->h3('input arguments:'),
    "<table border>\n",
    "<tr><th>hostname<th>system<th>subsystem<th>item<th>run<th>runIndexTable<th>date\n",
    "<tr><td>$hostname<td>$system<td>$subsystem<td>$item<td>$run<td>$runIndexTable<td>$date\n",
    "</table>\n";

    $status = ReadConstants($dbh, $system, $subsystem, $item, $run,
			    $runIndexTable, $date, \@itemvalue);

# display the results
    
    $ncol = $#itemvalue + 1; # total number of columns

    $index = $itemvalue[0];
    $author = $itemvalue[1];
    $time = $itemvalue[2];
    $source_min = $itemvalue[3];
    $source_max = $itemvalue[4];
    $comment = $itemvalue[5];

    $time_formatted = FormatTimestamp($dbh, $time);

    print $q->h3('query results:'),
    "\n<table border>\n",
    "<tr><th>itemValueId<th>author<th>time<th>source_min<th>source_max\n",
    "<tr><td>$index <td>$author <td>$time_formatted <td>$source_min <td>$source_max\n",
    "</table>",
    "\n<table border><tr><th>comment<td>$comment</table>\n";

    PrintValueTable();

    $q->end_html;                  # end the HTML

}

#################################################################

sub MakeItemValueByIndexPage {

    $title = "Calibration Database Item Value by Index";
    print $q->header,
    $q->start_html($title), # start the HTML
    $q->h1($title),         # level 1 header
    $q->h3('input arguments:'),
    "<table border>\n",
    "<tr><th>hostname<th>system<th>subsystem<th>item<th>itemValueId\n",
    "<tr><td>$hostname<td>$system<td>$subsystem<td>$item<td>$itemValueId\n",
    "</table>\n";

    $status = ReadConstantsByIndex($dbh, $system, $subsystem, $item,
				   $itemValueId, \@itemvalue);

# display the results
    
    $ncol = $#itemvalue + 1; # total number of columns

    $itemValueIdRead = $itemvalue[0];
    $author = $itemvalue[1];
    $time = $itemvalue[2];
    $source_min = $itemvalue[3];
    $source_max = $itemvalue[4];
    $comment = $itemvalue[5];

    $time_formatted = FormatTimestamp($dbh, $time);

    print $q->h3('query results:'),
    "\n<table border>\n",
    "<tr><th>author<th>time<th>source_min<th>source_max\n",
    "<tr><td>$author <td>$time_formatted <td>$source_min <td>$source_max\n",
    "</table>",
    "\n<table border><tr><th>comment<td>$comment</table>\n";

    PrintValueTable();

    $q->end_html;                  # end the HTML

}

#################################################################

sub PrintValueTable {

    print "<table border>\n";
    $nval = $ncol - 6;
    $nrow_full = int($nval/$ndata_per_row);
    foreach $ir (1 .. $nrow_full) {
	$i_pad_begin = sprintf("%04d", ($ir - 1)*$ndata_per_row + 1);
	$i_pad_end = sprintf("%04d", $ir*$ndata_per_row);
	print "<tr><th>v_$i_pad_begin-$i_pad_end";
	foreach $ic (1 .. $ndata_per_row) {
	    $column_index = ($ir - 1)*$ndata_per_row + $ic + 5;
	    print "<td>$itemvalue[$column_index]";
	}
	print "\n";
    }
    $ndata_last = $nval - $nrow_full*$ndata_per_row;
    if ($ndata_last) {
	$i_pad_begin = sprintf("%04d", $nrow_full*$ndata_per_row + 1);
	$i_pad_end = sprintf("%04d", $nval);
	print "<tr><th>v_$i_pad_begin-$i_pad_end";
	foreach $ic (1 .. $ndata_last) {
	    $column_index = $nrow_full*$ndata_per_row + $ic + 5;
	    print "<td>$itemvalue[$column_index]";
	}
    }
    print "\n";
    print "</table>\n";

}

#################################################################

sub GetInputParameters {
    @names = $q->param;
    #print "<p> names\n";
    foreach $in (0 .. $#names) {
	#print "<p>$in $names[$in]\n"; 
	eval "\$$names[$in] = \$q->param('$names[$in]')";
    }
    if (!$hostname) {$hostname = "clasdb.jlab.org";}
    if (!$runIndexTable) {$runIndexTable = "calib.RunIndex";}
    if (!$date) {$date = "2037-1-1";}
    if (!$runMin) {$runMin=-1000000;}
    if (!$runMax) {$runMax=1000000;}
    #print "<p> $system $subsystem $item $run $button $hostname $runIndexTable $date $minRun $maxRun\n";
}

#################################################################

sub MakeExcusesPage {
    $title = "Calibration Database Regrets";
    print $q->header,
    $q->start_html($title), # start the HTML
    $q->h1($title),         # level 1 header
    "function not implemented",
    $q->end_html;                  # end the HTML
    
    print "\n";
}

#################################################################

sub MakeValidItemsPage {

    $title = "Calibration Database Valid Items";
    print $q->header,
    $q->start_html($title), # start the HTML
    $q->h1($title);         # level 1 header

    $ncol = 9; # number of columns returned from query
    $verbose = 0;

    print $q->h3('input arguments:'),
    "\n<table border>\n",
    "<tr><th>hostname<th>run<th>runIndexTable<th>date\n",
    "<tr><td>$hostname<td>$run<td>$runIndexTable<td>$date\n",
    "</table>\n";

$status = ShowSetsRun($dbh, $run,
			$runIndexTable, $date, \@itemInfo);

# display the results

    print $q->h3('query results:'),
    "\n<table border>\n",
    "<tr><th>system<th>subsystem<th>item<th>min run<th>max run<th>itemValueId",
    "<th>officer<th>time<th>comment\n";

    for ($iitem = 0; $iitem <= $#itemInfo; $iitem++) {
	print "<tr>";
	foreach $icol (0 .. 4) {
	    print "<td>$itemInfo[$iitem][$icol]";
	}
	$system = $itemInfo[$iitem][0];
	$subsystem = $itemInfo[$iitem][1];
	$item = $itemInfo[$iitem][2];
	$itemValueIdRead = $itemInfo[$iitem][5];
	$href = HrefConstsByID($itemValueIdRead);
	print "<td><a href=$href>$itemValueIdRead</a>";
	foreach $icol (6 .. $ncol - 1) {
	    print "<td>$itemInfo[$iitem][$icol]";
	}
	print "\n";
    }
    print "</table>\n";

    $q->end_html;                  # end the HTML
}

#################################################################

sub MakeItemValuesByItemPage {

    $title = "Calibration Database Item Values by Item";
    print $q->header,
    $q->start_html($title), # start the HTML
    $q->h1($title);         # level 1 header

    print $q->h3('input arguments:'),
    "\n<table border>\n",
    "<tr><th>system<th>subsystem<th>item<th>hostname\n",
    "<tr><td>$system<td>$subsystem<td>$item<td>$hostname\n",
    "</table>\n";

    $status = ShowSetsItem($dbh, $system, $subsystem, $item, \@itemValue);

# display the results

    print $q->h3('query results:'),
    "\n<table border>\n",
    "<tr><th>itemValueId<th>author<th>time<th>minRunSource<th>maxRunSource",
    "<th>comment\n";

    foreach $iiv (0 .. $#itemValue) {
	print "<tr>";
	%iv = %{$itemValue[$iiv]};
	$ivid = $iv{itemValueId};
	$href = HrefConstsByID($ivid);
	print "<td><a href=$href>$ivid</a><td>$iv{author}<td>$iv{time}",
	"<td>$iv{minRunSource}<td>$iv{maxRunSource}<td>$iv{comment}\n";
    }
    print "</table>\n";

    $q->end_html;                  # end the HTML
}

#################################################################

sub MakeRunRangesPage {

    $ncol = 6;

    $title = "Calibration Database Run Ranges";
    print $q->header,
    $q->start_html($title), # start the HTML
    $q->h1($title);         # level 1 header

    print $q->h3('input arguments:'),
    "\n<table border>\n",
    "<tr><th>hostname<th>system<th>subsystem<th>item<th>runIndexTable<th>date\n",
    "<tr><td>$hostname<td>$system<td>$subsystem<td>$item<td>$runIndexTable<td>$date\n",
    "</table>\n";
#
    $status = ShowRunRanges($dbh, $system, $subsystem, $item, $runIndexTable,
			    $date, $runMin, $runMax, \@runRangeInfo);

# display the results

    print $q->h3('query results:'),
    "\n<table border>\n",
    "<tr><th>run min<th>run max<th>itemValueId<th>first constant<th>officer<th>time<th>comment\n";
    
    foreach $ir (0 .. $#runRangeInfo) {
	print "<tr>";
	foreach $ic (0 .. 1) {
	    print "<td>$runRangeInfo[$ir][$ic]";
	}
	$itemValueIdRead = $runRangeInfo[$ir][2];
	$href = HrefConstsByID($itemValueIdRead);
	print "<td><a href=$href>$itemValueIdRead</a>";
	$run = $runRangeInfo[$ir][0];
	$status = ReadConstants($dbh, $system, $subsystem, $item, $run,
				$runIndexTable, $time, \@constants);
	$constant_first = $constants[6];
	print "<td>$constant_first";
	foreach $ic (3 .. $ncol - 1) {
	    print "<td>$runRangeInfo[$ir][$ic]";
	}
	print "\n";
    }
    print "</table>\n";

    $q->end_html;                  # end the HTML

}

#################################################################

sub HrefConstsByID {
    my($itemValueId) = @_;
    local($href);
    $href = "cq.pl?button=Values+by+ID&system=$system";
	$href .= "&subsystem=$subsystem&item=$item";
	$href .= "&itemValueId=$itemValueId";
	$href .= "&hostname=$hostname";
    return $href;
}

#################################################################

sub MakeRunHistoryPage {

    $ncol = 6;

    $title = "Calibration Database Run History";
    print $q->header,
    $q->start_html($title), # start the HTML
    $q->h1($title);         # level 1 header

    print $q->h3('input arguments:'),
    "\n<table border>\n",
    "<tr><th>hostname<th>system<th>subsystem<th>item<th>run<th>runIndexTable<th>date\n",
    "<tr><td>$hostname<td>$system<td>$subsystem<td>$item<td>$run<td>$runIndexTable<td>$date\n",
    "</table>\n";

$status = ShowHistory($dbh, $system, $subsystem, $item, $run,
			$runIndexTable, \@historyInfo);

# display the results

    print $q->h3('query results:'),
    "\n<table border>\n",
    "<tr><th>time<th>run min<th>run max<th>itemValueId<th>officer<th>comment\n";

    foreach $ir (0 .. $#historyInfo) {
	print "<tr>";
	foreach $ic (0 .. 2) {
	    print "<td>$historyInfo[$ir][$ic]";
	}
	$itemValueIdRead = $historyInfo[$ir][3];
	$href = HrefConstsByID($itemValueIdRead);
	print "<td><a href=$href>$itemValueIdRead</a>";
	foreach $ic (4 .. $ncol - 1) {
	    print "<td>$historyInfo[$ir][$ic]";
	}
	print "\n";
}
    print "</table>\n";

    $q->end_html;                  # end the HTML

}

#################################################################

#end of perl script
