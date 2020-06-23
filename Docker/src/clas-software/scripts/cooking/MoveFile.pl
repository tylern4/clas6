#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        December 17, 1998

CONFIG:{
    package MoveFile;
}

sub MoveFile{

# Looks for the occurences of a resulting file and then moves them to 
# a certain location which is recorded in the specified database
# Inputs
#    MoveFile(output, res_loc, monitor, db_file)
#                     output  = string to be searched for using an ls
#                     res_loc  = the location the file is to be copied to
#                     monitor = the monitoring program that this file is from
#                     db_file  = database file including path


    local($output) = $_[0];
    local($res_loc) = $_[1];
    local($monitor) = $_[2];
    local($db_file) = $_[3];
    local($ls_command);

    $ls_command = "ls -1 $output | ";

    print $ls_command."\n";

    open(LS, $ls_command) || warn "$0: Could not open $ls_command.\n";

    dbmopen(%RUN, "$db_file", 0644) || print  "$0: Could not open $db_dile for offline database.\n";

    while (<LS>) {
	print $_;
	chop;
	if ($res_loc =~ /^\/mss\//) {
	    $copy = "jputremote";
	} else {
	    $copy = "cp";
	}
	$mvcmd = "$copy $_ $res_loc/$_ | ";
	print "\n$0 EXECUTE: $mvcmd\n";
	$RUN{"$monitor"} .= "$res_loc/$_ ";
	open(MV, $mvcmd) || warn "$0  Could not open $mvcmd.\n";
	while (<MV>) {
	    print $_;
	}
	close(MV);
    }

    close(LS);
    
    dbmclose(%RUN);

}

1;
