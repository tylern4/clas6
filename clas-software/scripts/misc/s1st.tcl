#!/apps/tcl/bin/tclsh 

proc error_message {} {
puts {
Use: s1st.tcl output_file_from_scaler_mon
Options are: 
        [-t<mapfile>]     Time to place array at    

Note:
The script s1st.tcl can be run on: AIX; SunOS, or HP-UX
The script s1stl.tcl is to be run on Linux machines
It is assumed that $CLAS_BIN is in your path

}
}
set val_m -1;set val_t -1;set val_b 1;set val_n -1;set val_d -1;
set input_list $argv
set one_time -1
set file_list {}
set option_list {}
#if {$argv == {}} {error_message;exit}

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
	set val_d 1;set val_b -1}
	n {if {$val_n == 1} {error_message;exit}
	set val_n 1}
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
    set latch_bit1_count 0
    set latch_bit2_count 0
    set latch_bit3_count 0
    set latch_bit4_count 0
    set latch_bit5_count 0
    set latch_bit6_count 0
    set latch_bit7_count 0
    set latch_bit8_count 0
    set latch_bit9_count 0
    set latch_bit10_count 0
    set latch_bit11_count 0
    set latch_bit12_count 0
    set event_count 0
    set latch_zero_count 0
    set latch_empty_count 0
    set latch_not_empty_count 0

set get_line -1
set number_of_lines 0
set files_read 0

foreach element $file_list {
    set latch_bit1_count_tmp 0
    set latch_bit2_count_tmp 0
    set latch_bit3_count_tmp 0
    set latch_bit4_count_tmp 0
    set latch_bit5_count_tmp 0
    set latch_bit6_count_tmp 0
    set latch_bit7_count_tmp 0
    set latch_bit8_count_tmp 0
    set latch_bit9_count_tmp 0
    set latch_bit10_count_tmp 0
    set latch_bit11_count_tmp 0
    set latch_bit12_count_tmp 0
    set event_count_tmp 0
    set latch_zero_count_tmp 0
    set latch_empty_count_tmp 0
    set latch_not_empty_count_tmp 0

    set fid [open $element r]
    while {[gets $fid data] >= 0} {
	
        if {[lindex $data 0] == "latch_bit1_count"} {
	    set  latch_bit1_count_tmp  [lindex $data 2]
#	    puts "latch_bit1_count_tmp = $latch_bit1_count_tmp"
        }
	if {[lindex $data 0] == "latch_bit2_count"} {
	    set  latch_bit2_count_tmp  [lindex $data 2]
#	    puts "latch_bit2_count_tmp = $latch_bit2_count_tmp"
        }
	if {[lindex $data 0] == "latch_bit3_count"} {
	    set  latch_bit3_count_tmp  [lindex $data 2]
#	    puts "latch_bit3_count_tmp = $latch_bit3_count_tmp"
        }
	if {[lindex $data 0] == "latch_bit4_count"} {
	    set  latch_bit4_count_tmp  [lindex $data 2]
#	    puts "latch_bit4_count_tmp = $latch_bit4_count_tmp"
        }
	if {[lindex $data 0] == "latch_bit5_count"} {
	    set  latch_bit5_count_tmp  [lindex $data 2]
#	    puts "latch_bit5_count_tmp = $latch_bit5_count_tmp"
        }
	if {[lindex $data 0] == "latch_bit6_count"} {
	    set  latch_bit6_count_tmp  [lindex $data 2]
#	    puts "latch_bit6_count_tmp = $latch_bit6_count_tmp"
        }
	if {[lindex $data 0] == "latch_bit7_count"} {
	    set  latch_bit7_count_tmp  [lindex $data 2]
#	    puts "latch_bit7_count_tmp = $latch_bit7_count_tmp"
        }
	if {[lindex $data 0] == "latch_bit8_count"} {
	    set  latch_bit8_count_tmp  [lindex $data 2]
#	    puts "latch_bit8_count_tmp = $latch_bit8_count_tmp"
        }
	if {[lindex $data 0] == "latch_bit9_count"} {
	    set  latch_bit9_count_tmp  [lindex $data 2]
#	    puts "latch_bit9_count_tmp = $latch_bit9_count_tmp"
        }
	if {[lindex $data 0] == "latch_bit10_count"} {
	    set  latch_bit10_count_tmp  [lindex $data 2]
#	    puts "latch_bit10_count_tmp = $latch_bit10_count_tmp"
        }
	if {[lindex $data 0] == "latch_bit11_count"} {
	    set  latch_bit11_count_tmp  [lindex $data 2]
#	    puts "latch_bit11_count_tmp = $latch_bit11_count_tmp"
        }
	if {[lindex $data 0] == "latch_bit12_count"} {
	    set  latch_bit12_count_tmp  [lindex $data 2]
#	    puts "latch_bit12_count_tmp = $latch_bit12_count_tmp"
        }
	if {[lindex $data 0] == "event_count"} {
	    set  event_count_tmp  [lindex $data 2]
#	    puts "event_count_tmp = $event_count_tmp"
        }
	if {[lindex $data 0] == "latch_zero_count"} {
	    set  latch_zero_count_tmp  [lindex $data 2]
#	    puts "latch_zero_count_tmp = $latch_zero_count_tmp"
        }
	if {[lindex $data 0] == "latch_empty_count"} {
	    set  latch_empty_count_tmp  [lindex $data 2]
#	    puts "latch_empty_count_tmp = $latch_empty_count_tmp"
        }
	if {[lindex $data 0] == "latch_not_empty_count"} {
	    set  latch_not_empty_count_tmp  [lindex $data 2]
#	    puts "latch_not_empty_count_tmp = $latch_not_empty_count_tmp"
        }
    }   

    close $fid
    incr files_read +1
    set latch_bit1_count  [expr $latch_bit1_count      + $latch_bit1_count_tmp]
    set latch_bit2_count  [expr $latch_bit2_count      + $latch_bit2_count_tmp]
    set latch_bit3_count  [expr $latch_bit3_count      + $latch_bit3_count_tmp]
    set latch_bit4_count  [expr $latch_bit4_count      + $latch_bit4_count_tmp]
    set latch_bit5_count  [expr $latch_bit5_count      + $latch_bit5_count_tmp]
    set latch_bit6_count  [expr $latch_bit6_count      + $latch_bit6_count_tmp]
    set latch_bit7_count  [expr $latch_bit7_count      + $latch_bit7_count_tmp]
    set latch_bit8_count  [expr $latch_bit8_count      + $latch_bit8_count_tmp]
    set latch_bit9_count  [expr $latch_bit9_count      + $latch_bit9_count_tmp]
    set latch_bit10_count  [expr $latch_bit10_count     + $latch_bit10_count_tmp]
    set latch_bit11_count  [expr $latch_bit11_count     + $latch_bit11_count_tmp]
    set latch_bit12_count  [expr $latch_bit12_count     + $latch_bit12_count_tmp]
    set event_count       [expr $event_count           + $event_count_tmp]
    set latch_zero_count  [expr $latch_zero_count      + $latch_zero_count_tmp]
    set latch_empty_count [expr $latch_empty_count     + $latch_empty_count_tmp]
    set latch_not_empty_count [expr $latch_not_empty_count + $latch_not_empty_count_tmp]
}
#puts "event_count = $event_count"

set fid2 [open ".event_count" w]
puts -nonewline $fid2 $event_count
close $fid2  
set fid2 [open ".latch_bit1_count" w]
puts -nonewline $fid2 $latch_bit1_count
close $fid2 
set fid2 [open ".latch_bit2_count" w]
puts -nonewline $fid2 $latch_bit2_count
close $fid2 
set fid2 [open ".latch_bit3_count" w]
puts -nonewline $fid2 $latch_bit3_count
close $fid2 
set fid2 [open ".latch_bit4_count" w]
puts -nonewline $fid2 $latch_bit4_count
close $fid2 
set fid2 [open ".latch_bit5_count" w]
puts -nonewline $fid2 $latch_bit5_count
close $fid2 
set fid2 [open ".latch_bit6_count" w]
puts -nonewline $fid2 $latch_bit6_count
close $fid2 
set fid2 [open ".latch_bit7_count" w]
puts -nonewline $fid2 $latch_bit7_count
close $fid2 
set fid2 [open ".latch_bit8_count" w]
puts -nonewline $fid2 $latch_bit8_count
close $fid2 
set fid2 [open ".latch_bit9_count" w]
puts -nonewline $fid2 $latch_bit9_count
close $fid2 
set fid2 [open ".latch_bit10_count" w]
puts -nonewline $fid2 $latch_bit10_count
close $fid2 
set fid2 [open ".latch_bit11_count" w]
puts -nonewline $fid2 $latch_bit11_count
close $fid2 
set fid2 [open ".latch_bit12_count" w]
puts -nonewline $fid2 $latch_bit12_count
close $fid2 
set fid2 [open ".latch_zero_count" w]
puts -nonewline $fid2 $latch_zero_count
close $fid2 
set fid2 [open ".latch_empty_count" w]
puts -nonewline $fid2 $latch_empty_count
close $fid2 
set fid2 [open ".latch_not_empty_count" w]
puts -nonewline $fid2 $latch_not_empty_count
close $fid2 

#set mapfile $env(CLAS_PARMS)/Maps/S1ST.map
set mapfile S1ST.map

exec put_map_int -m$mapfile -sS1ST -t$T -ievent_count     < .event_count           &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_bit1      < .latch_bit1_count      &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_bit2      < .latch_bit2_count      &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_bit3      < .latch_bit3_count      &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_bit4      < .latch_bit4_count      &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_bit5      < .latch_bit5_count      &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_bit6      < .latch_bit6_count      &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_bit7      < .latch_bit7_count      &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_bit8      < .latch_bit8_count      &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_bit9      < .latch_bit9_count      &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_bit10     < .latch_bit10_count     &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_bit11     < .latch_bit11_count     &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_bit12     < .latch_bit12_count     &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_zero      < .latch_zero_count      &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_empty     < .latch_empty_count     &
exec put_map_int -m$mapfile -sS1ST -t$T -ilatch_not_empty < .latch_not_empty_count &





