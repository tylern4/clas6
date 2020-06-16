#!/usr/local/bin/perl

# This is a perl library to be used for the CLAS cooking scripts

# By: Simeon McAleer
#         Florida State University
#         December 17, 1998

CONFIG:{
    package GetMapFloat;
}

sub GetMapFloat {

# Queries the map in the PARMS area for the available information
# The order of the passed parameters are the subsystem of the map, 
# the item about which information is wanted from that subsystem,
# the run number, the map location and the executable path. The 
# value from the map is the returned.

    local($sub_system) = $_[0];
    local($item) = $_[1];
    local($run_num) = $_[2];
    local($map) = $_[3];
    local($length) = $_[4];
    local($cmd);

    local($os) = $ENV{"OSNAME"};
    local($clas_bin) = $ENV{"CLAS_BIN"};

# Set default CLAS_BIN and CLAS_DB
    if ($clas_bin eq "") {
	$clas_bin = "/group/clasdev/clas/src/prod-1-5/bin/$os/";
    }

    $cmd = "$clas_bin/get_map_float -m$map -s$sub_system -i$item -t$run_num  -l$length";

    open(CMD, "$cmd 2>&1 |") || warn "\nCouldn't execute $cmd.\n";

    while (<CMD>) {
        if (/MAP_SYSTEM_ERROR_OPEN/) {
            print "\n**** Error with $cmd.\n";
            while (<CMD>) {
                print $_;
            }
        } else {
            chop;
# Determine if the value is a float and if so return that number
            if (/^\d+.\d+\s+$/) {
	      return  $_ ;
            }
        }
    }

    close(CMD);
    
}
1;
