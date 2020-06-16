#!/usr/bin/tclsh 

proc error_message {} {
puts {
Use: scaler.tcl output_file_from_scaler_mon
Options are: 
        [-t<mapfile>]     Time to place array at    
        [-d]              run in debug mode
        [-n]              normalization run

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
set tgs_list  {}
set trgs_list {}
set rtsl_list {}
set g1sl_list {}
set g2sl_list {}
set g3sl_list {}
set g4sl_list {}
set pc_list  {}
set ps_list  {}
set tag_list {}
set get_line -1
set get_line2 -1
set get_line3 -1
set get_line4 -1
set number_of_lines 0
set files_read 0
foreach element $file_list {
    set tgs_list_temp  {}
    set trgs_list_temp {}
    set rtsl_list_temp {}
    set g1sl_list_temp {}
    set g2sl_list_temp {}
    set g3sl_list_temp {}
    set g4sl_list_temp {}
    set pc_list_temp   {}
    set ps_list_temp   {}
    set tag_list_temp  {}
    set fid [open $element r]
    while {[gets $fid data] >= 0} {
#getting TGS
        if {([lindex $data 0] == "Group:")&&([lindex $data 1] == "TGS")} {
            set get_line 0
        }
        if {$get_line >= 0} {
	    incr get_line
        }
        if {$data == {}} {
            set get_line -1
        }
        if {$get_line >= 2} {
	    for {set i 1} {$i <= [llength $data]} {incr i +2} {
            set tgs   [lindex $data $i]
	    set tgs_list_temp  [linsert $tgs_list_temp  end $tgs ]
	    }
        }
	if {$val_b != 1} {
	    puts "$data"
	}
#getting TRGS
        if {([lindex $data 0] == "Group:")&&([lindex $data 1] == "TRGS")} {
            set get_line2 0
        }
        if {$get_line2 >= 0} {
	    incr get_line2
        }
        if {$data == {}} {
            set get_line2 -1
        }
        if {$get_line2 >= 2} {
	    for {set i 1} {$i <= [llength $data]} {incr i +2} {
            set trgs   [lindex $data $i]
	    set trgs_list_temp  [linsert $trgs_list_temp  end $trgs ]
	    }
	}
	if {$val_b != 1} {
	    puts "$data"
	}
#getting RTSL & G1SL & G2SL & G3SL & G4SL
        if {([lindex $data 0] == "Tag")&&([lindex $data 3] == "(counts):")} {
            set get_line3 0
        }
        if {$get_line3 >= 0} {
	    incr get_line3
        }
        if {$data == {}} {
            set get_line3 -1
        }
        if {$get_line3 >= 3} {
            set rtsl   [lindex $data 1]
	    set rtsl_list_temp  [linsert $rtsl_list_temp  end $rtsl ]
	    set g1sl   [lindex $data 2]
	    set g1sl_list_temp  [linsert $g1sl_list_temp  end $g1sl ]
	    set g2sl   [lindex $data 3]
	    set g2sl_list_temp  [linsert $g2sl_list_temp  end $g2sl ]
	    set g3sl   [lindex $data 4]
	    set g3sl_list_temp  [linsert $g3sl_list_temp  end $g3sl ]
	    set g4sl   [lindex $data 5]
	    set g4sl_list_temp  [linsert $g4sl_list_temp  end $g4sl ]
	}
        if {([lindex $data 0] == "T-counter")&&([lindex $data 1] == "PC")&&([lindex $data 2] == "Eff.")} {
            set get_line4 0
        }
        if {$get_line4 >= 0} {
	    incr get_line4
        }
        if {$data == {}} {
            set get_line4 -1
        }
        if {$get_line4 >= 2} {
            set pc_eff   [lindex $data 1]
            set ps_eff   [lindex $data 2]
            set tag_eff  [lindex $data 3]
	    set pc_list_temp  [linsert $pc_list_temp  end $pc_eff ]
	    set ps_list_temp  [linsert $ps_list_temp  end $ps_eff ]
	    set tag_list_temp [linsert $tag_list_temp end $tag_eff] 
	    
        }
	if {$val_b != 1} {
	    puts "$data"
	}
    }
    close $fid
    incr files_read +1
    if {$files_read == 1} then {
        set tgs_list   $tgs_list_temp
	set trgs_list  $trgs_list_temp
	set rtsl_list  $rtsl_list_temp
	set g1sl_list  $g1sl_list_temp
	set g2sl_list  $g2sl_list_temp
	set g3sl_list  $g3sl_list_temp
	set g4sl_list  $g4sl_list_temp
        set pc_list    $pc_list_temp
	set ps_list    $ps_list_temp
	set tag_list   $tag_list_temp
    } else {
        set a_plus_b 0
        set i 0
        foreach a $tgs_list_temp {
            set a_plus_b [expr $a + [lindex $tgs_list $i]]
            set tgs_list [lreplace $tgs_list $i $i $a_plus_b]
            incr i 
        }
        set a_plus_b 0
        set i 0
        foreach a $trgs_list_temp {
            set a_plus_b [expr $a + [lindex $trgs_list $i]]
            set trgs_list [lreplace $trgs_list $i $i $a_plus_b]
            incr i 
        }
        set a_plus_b 0
        set i 0
        foreach a $rtsl_list_temp {
            set a_plus_b [expr $a + [lindex $rtsl_list $i]]
            set rtsl_list [lreplace $rtsl_list $i $i $a_plus_b]
            incr i 
        }
        set a_plus_b 0
        set i 0
        foreach a $g1sl_list_temp {
            set a_plus_b [expr $a + [lindex $g1sl_list $i]]
            set g1sl_list [lreplace $g1sl_list $i $i $a_plus_b]
            incr i 
        }
        set a_plus_b 0
        set i 0
        foreach a $g2sl_list_temp {
            set a_plus_b [expr $a + [lindex $g2sl_list $i]]
            set g2sl_list [lreplace $g2sl_list $i $i $a_plus_b]
            incr i 
        }
        set a_plus_b 0
        set i 0
        foreach a $g3sl_list_temp {
            set a_plus_b [expr $a + [lindex $g3sl_list $i]]
            set g3sl_list [lreplace $g3sl_list $i $i $a_plus_b]
            incr i 
        }
        set a_plus_b 0
        set i 0
        foreach a $g4sl_list_temp {
            set a_plus_b [expr $a + [lindex $g4sl_list $i]]
            set g4sl_list [lreplace $g4sl_list $i $i $a_plus_b]
            incr i 
        }
        set a_plus_b 0
        set i 0
        foreach a $pc_list_temp {
            set a_plus_b [expr $a + [lindex $pc_list $i]]
            set pc_list [lreplace $pc_list $i $i $a_plus_b]
            incr i 
        }
        set a_plus_b 0
        set i 0
        foreach a $ps_list_temp {
            set a_plus_b [expr $a + [lindex $ps_list $i]]
            set ps_list [lreplace $ps_list $i $i $a_plus_b]
            incr i 
        }
        set a_plus_b 0
        set i 0
        foreach a $tag_list_temp {
            set a_plus_b [expr $a + [lindex $tag_list $i]]
            set tag_list [lreplace $tag_list $i $i $a_plus_b]
            incr i 
        }
    }
}
for {set i 61} {$i <= 63} {incr i} {
    set rtsl_list [linsert $rtsl_list end   0]
    set g1sl_list [linsert $g1sl_list end   0]
    set g2sl_list [linsert $g2sl_list end   0]
    set g3sl_list [linsert $g3sl_list end   0]
    set g4sl_list [linsert $g4sl_list end   0]
    set pc_list   [linsert $pc_list   end 0.0]
    set ps_list   [linsert $ps_list   end 0.0]
    set tag_list  [linsert $tag_list  end 0.0]
}
set fid2 [open "tgs_datafile" w]
puts -nonewline $fid2 $tgs_list
close $fid2
set fid2 [open "trgs_datafile" w]
puts -nonewline $fid2 $trgs_list
close $fid2
if {$val_n == 1} {
    set fid2 [open "rtsl_datafile" w]
    puts -nonewline $fid2 $rtsl_list
    close $fid2
    set fid2 [open "g1sl_datafile" w]
    puts -nonewline $fid2 $g1sl_list
    close $fid2
    set fid2 [open "g2sl_datafile" w]
    puts -nonewline $fid2 $g2sl_list
    close $fid2
    set fid2 [open "g3sl_datafile" w]
    puts -nonewline $fid2 $g3sl_list
    close $fid2
    set fid2 [open "g4sl_datafile" w]
    puts -nonewline $fid2 $g4sl_list
    close $fid2
    set fid2 [open "pc_eff_datafile" w]
    puts -nonewline $fid2 $pc_list
    close $fid2
    set fid2 [open "ps_eff_datafile" w]
    puts -nonewline $fid2 $ps_list
    close $fid2
    set fid2 [open "tag_eff_datafile" w]
    puts -nonewline $fid2 $tag_list
    close $fid2
}
set mapfile $env(CLAS_PARMS)/Maps/SCALERS.map
exec put_map_int -m$mapfile -sTGS -ivalues -t$T < tgs_datafile &
exec put_map_int -m$mapfile -sTRGS -ivalues -t$T < trgs_datafile &

if {$val_n == 1} {
    set mapfile $env(CLAS_PARMS)/Maps/TAG_SCALER.map
    exec put_map_int -m$mapfile -sRTSL -ivalue -t$T < rtsl_datafile &
    exec put_map_int -m$mapfile -sG1SL -ivalue -t$T < g1sl_datafile &
    exec put_map_int -m$mapfile -sG2SL -ivalue -t$T < g2sl_datafile &
    exec put_map_int -m$mapfile -sG3SL -ivalue -t$T < g3sl_datafile &
    exec put_map_int -m$mapfile -sG4SL -ivalue -t$T < g4sl_datafile &
    exec put_map_float -m$mapfile -sPCEff -ivalue -t$T < pc_eff_datafile &
    exec put_map_float -m$mapfile -sPSEff -ivalue -t$T < ps_eff_datafile &
    exec put_map_float -m$mapfile -sTagEff -ivalue -t$T < tag_eff_datafile &
}
