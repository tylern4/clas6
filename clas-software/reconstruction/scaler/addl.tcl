#!/usr/bin/tclsh 

proc error_message {} {
puts {
Use: scaler.tcl output_file_from_scaler_mon
Options are: 
        [-t<mapfile>]     Time to place array at    
        [-d]              run in debug mode

Note:
The script scaler.tcl can be run on: AIX; SunOS, or HP-UX
The script scalerl.tcl is to be run on Linux machines
It is assumed that $CLAS_BIN is in your path

}
}
set val_m -1;set val_t -1;set val_b 1;set val_n -1;set val_d -1;
set input_list $argv
set one_time -1
set file_list {}
set option_list {}
if {$argv == {}} {error_message;exit}

foreach element $input_list {
    set element_list  [split $element {}]
    set a [lindex $element_list 0]
    if {$a == "-"} {
	set option_list [linsert $option_list 0 $element]
    }
    if {$a != "-"} {
	set file_list [linsert $file_list 0  $element]
    }	
}
foreach element $option_list {
    set element_list  [split $element {}]
    set a [lindex $element_list 1]
    set arg [join [lrange $element_list 2 end] {}]
    switch -exact -- $a {
	t {if {$val_t == 1} {error_message;exit}
	set T $arg; set val_t 1}
	d {if {$val_d == 1} {error_message;exit}
	set val_d 1;set val_b -1;}
	default  {}
    }
}
if {$val_t !=1} {error_message;exit}
set number_of_files [llength $file_list]
if {$val_b != 1} {
    puts "input list = $input_list"
    puts "file list = $file_list"
    puts "option T = $T"
    puts "number of files = $number_of_files"
}
####################################################
# Finished getting input into lists 
####################################################
set list  {}
set get_line -1
set number_of_lines 0
set files_read 0
foreach element $file_list {
    set list_temp  {}
    set fid [open $element r]
    while {[gets $fid data] >= 0} {
        if {$data != 0} {
            set get_line 0
        }
        if {$get_line >= 0} {
	    incr get_line
        }
        if {$data == {}} {
            set get_line -1
        }
        if {$get_line >= 1} {
	    for {set i 0} {$i < [llength $data]} {incr i +1} {
            set thing   [lindex $data $i]
	    set list_temp  [linsert $list_temp  end $thing ]
	    }
        }
	if {$val_b != 1} {
	    puts "$data"
	}
    }
    close $fid
    incr files_read +1
    if {$files_read == 1} then {
        set list $list_temp
    } else {
        set a_plus_b 0
        set i 0
        foreach a $list_temp {
            set a_plus_b [expr $a + [lindex $list $i]]
            set list [lreplace $list $i $i $a_plus_b]
            incr i 
        }
    }
}
set fid2 [open "datafile" w]
puts -nonewline $fid2 $list
close $fid2
set mapfile $env(CLAS_PARMS)/Maps/E_T_MATRIX.map
exec put_map_int -m$mapfile -smatrix -icontents -t$T < datafile &

