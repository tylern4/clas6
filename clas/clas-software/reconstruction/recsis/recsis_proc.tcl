global env
set on -1
set off 0
set true -1
set false 0
set ifpack 0
#
#  write out some info
puts "recsis_proc.tcl  this Tcl script defines some tcl procedures that";
puts "define your interactive command set\n";
set nhist_tcl 0;
set nbook_stack 0;
set nfill_stack 0;
setc hdir "//PAWC";
set tdir "//PAWC"

puts "defining the <cdir> proceedure, that defines the PAWC directory";
puts "usage: cdir //PAWC/NEWDIR \n";

proc term { } { 
    global ncol_screen;
    set f [exec stty -a];
#    puts $f
    set f [split $f \;\n];
    set col_def [lsearch -glob $f *columns*];
#    puts $col_def
    set f [lindex $f $col_def];
#    puts $f
    set blank " "
#    puts "here is a :$blank: hahha\n";
#    set f [split $f s\n];
#    set ncol_screen [lindex $f 1];
    set flen [string length $f]
    set fbeg [expr $flen-3]
    set ncol_screen [string range $f $fbeg $flen]
#    puts "Screen width = $ncol_screen\n";
}
#proc term { } { 
#    global ncol_screen;
#    set f [exec stty -a];
#    set f [split $f \;\n];
#    set col_def [lsearch -glob $f *columns*];
#    set f [lindex $f $col_def];
#    set f [split $f \=];
#    set ncol_screen [lindex $f 1];
#    puts "Screen width = $ncol_screen\n";
#}

proc cdir {{ctmp "//PAWC"}} {
    global rchar_control tdir hdir;
    set tdir $ctmp;
    setc hdir $tdir;
    puts "Changing PAW directory to $tdir";
}

puts "defining the <global_section> procedure, this will turn on/off";
puts "PAW shared memory for those machines that support it";
puts "usage: global_section off  default is off\n";

proc global_section {{onoff off}} {
    global rchar_control
    if {$onoff == "off"} {
	setc cshared_name "    "
    } 
}

puts "defining the <book> procedure, that books new histograms";
puts "usage: book id title nx xlow xmax \[ny ylow ymax PAWDIR\] \n";

proc book {id ctit nx xlow xhgh {ny 0} {ylow 0} {yhgh 0} {cdir 0}} {
    global rchar_control lhbook hist_dir hist_id hist_tit \
        book    nxchan xmin xmax nychan ymin ymax nhist nhist_tcl \
	   hist_table hdir nbook_stack lntup tdir;
    incr nbook_stack
    if {$cdir != 0} {
	cdir $cdir
    } 
    setc hist_dir($nbook_stack) $tdir
    set hist_id($nbook_stack) $id
    if ![info exists hist_table($tdir,$id)]  {
	incr nhist_tcl;
	set hist_table($tdir,$id) $nhist_tcl
    } 
    set nhist($nbook_stack) $hist_table($tdir,$id)

    setc hist_tit($nbook_stack) $ctit
    set nxchan($nbook_stack) $nx
    set xmin($nbook_stack) $xlow
    set xmax($nbook_stack) $xhgh
    set nychan($nbook_stack) $ny
    set ymin($nbook_stack) $ylow
    set ymax($nbook_stack) $yhgh
    set lntup($nbook_stack) 0
    set lhbook -1
}

puts "defining the <bookn> procedure, that books new ntuples";
puts "usage: bookn id title \[c1 c2 c3 ... c25\] \n";

proc bookn {id ctit  args} {
    global rchar_control lhbook hist_dir hist_id hist_tit \
           nxchan nhist nhist_tcl cntup\
	   hist_table hdir  tdir nbook_stack lntup;
    
    incr nbook_stack

    set i 0
    foreach c $args {
        incr i
        setc cntup($nbook_stack,$i) $c
    }
    setc hdir $tdir
    setc hist_dir($nbook_stack) $tdir
    set hist_id($nbook_stack) $id
    if ![info exists hist_table($tdir,$id)]  {
	incr nhist_tcl;
	set hist_table($tdir,$id) $nhist_tcl
    } 
    set nhist($nbook_stack) $hist_table($tdir,$id)

    setc hist_tit($nbook_stack) $ctit
    set nxchan($nbook_stack) $i
    set lntup($nbook_stack) -1;
    set lhbook -1
}

puts "defining the <fill> procedure for histograms \
	defined via <book> procedure";
puts "usage: fill id BANK.SEC.VAR \[ BANK.SEC.VAR \] \n";

proc fill {id cx {cy 0}  {cdir 0}} {
    global rchar_control lsetvar nhist hist_x hist_y  \
            hist_table nfill_stack hdir tdir;
    incr nfill_stack;
    if {$cdir != 0} {
	cdir $cdir
    }
    if ![info exists hist_table($tdir,$id)]  {
	incr nhist_tcl;
	set hist_table($tdir,$id) $nhist_tcl
    } 
    set nhist($nfill_stack) $hist_table($tdir,$id);
    set hist_id($nfill_stack) $id
    setc hist_dir($nfill_stack) $tdir
    setc hist_x($nfill_stack) $cx
    setc hist_y($nfill_stack) $cy
    set ndim($nfill_stack) 0
    set lsetvar -1
}    
puts "defining the <filln> procedure for ntuples \
	defined via <bookn> procedure";
puts "usage: filln id BANK.SEC.VAR \[ BANK.SEC.VAR .... BANK.SEC.VAR \] \n";
proc filln {id args} {
    global rchar_control lsetvar nhist   \
            hist_table nfill_stack hdir tdir ctclvar ndim;
    incr nfill_stack;

    if ![info exists hist_table($tdir,$id)]  {
	incr nhist_tcl;
	set hist_table($tdir,$id) $nhist_tcl
    } 
    set nhist($nfill_stack) $hist_table($tdir,$id);
    set hist_id($nfill_stack) $id
    setc hist_dir($nfill_stack) $tdir
    set i 0
    foreach c $args {
        incr i
        setc ctclvar($nfill_stack,$i) $c
    }
    set ndim($nfill_stack) $i
    set lsetvar -1
}    
puts "defining the <dump> procedure, which dumps histograms to the terminal"; 
puts "usage: dump \[id //PAWC/SC \] \n";

proc dump {{id 0} {cdir 0}} {
    global rchar_control lhist hid hdir tdir;

    if {$cdir != 0} {
	cdir $cdir
    }
    set hid $id;
    set lhist -1;
}


puts "defining the <bdump> procedure, which dumps a BOS bank to the terminal"; 
puts "usage: bdump BANKNAME \[ jw \] : iw/jw -> BOS iw/jw array \n";

proc bdump {ctmp {bos_array iw}} {
 global cbname lbnkdmp rchar_control ibos_array;
 term
 setc cbname $ctmp;
 if {$bos_array == "jw"} {
     set ibos_array 2
 } else {
     set ibos_array 1
 };
 set lbnkdmp -1
}

puts "defining the <ndump> procedure, which dumps all BOS\
	bank names in memory "; 
puts "usage: ndump \[ jw \] : iw/jw -> BOS iw/jw array \n";

proc ndump {{bos_array iw}} {
 global lnamdmp ibos_array;
 if {$bos_array == "jw"} {
     set ibos_array 2;
 } else {
     set ibos_array 1;
 };
 set lnamdmp -1
}

puts "defining the <edump> procedure, which dumps ALL BOS banks in memory"; 
puts "usage: edump \[ jw \] : iw/jw -> BOS iw/jw array \n";

proc edump {{bos_array iw}} {
 global levtdmp ibos_array;
 term
 if {$bos_array == "jw"} {
     set ibos_array 2;
 } else {
     set ibos_array 1;
 };
 set levtdmp -1
}

puts "defining the <bedit> procedure, for bank editting"; 
puts "usage: bedit BANKNAME row col sector newvalue \[ jw \] :\
	iw/jw -> BOS iw/jw array \n";

proc bedit {ctmp row column sector value {bos_array iw}} {
 global cbname  nrow_tcl      ncol_tcl    nrec_tcl rnew_value \
        lbnkedt rchar_control ibos_array;

 setc cbname $ctmp;
 set nrow_tcl $row; 
 if {$bos_array == "jw"} {
     set ibos_array 2;
 } else {
     set ibos_array 1;
 };
 set ncol_tcl $column;
 set nrec_tcl $sector;
 set rnew_value $value;
 set lbnkedt -1
}

puts "defining the <par> procedure, which pause execution at the END of event"; 
puts "usage:  par\n";

proc par {} {
    global lpar;
    set lpar -1;
}

puts "defining the <pause> procedure, which pause execution at the \
	BEGINNING of event"; 
puts "usage:  pause\n";

proc pause {} {
    global lpause;
    set lpause -1;
}

puts "defining the <go> procedure, which resumes execution"; 
puts "usage:  go \[number of events to process\]\n";

proc go {{ntmp 0}} {
    global lpause nevt_to_process;
    set nevt_to_process  $ntmp
    set lpause 0;
}

puts "defining the <fpack> procedure, for FPACK commands\r";
puts "files under FPACK control\r";
puts "usage: fpack \"fpack command string\" \n";

proc fpack {{ctmp}} {
    global rchar_control ifpack fpk_cmd
    incr ifpack;
    puts "recsis_proc I: sending FPACK CMD: $ctmp\n";
    setc fpk_cmd($ifpack) "$ctmp"
}

puts "defining the <closefile> procedure, which will close input or output\r";
puts "files under FPACK control\r";
puts "usage: closefile \[CLASINPUT\] \n";

proc closefile {{ctmp CLASINPUT}} {
    global tcl_rec_input rchar_control;
#    setc rec_input " "
    fpack "CLOSE $ctmp"
}
puts "defining the <inputfile> procedure, which defines the input file";
puts "usage:  inputfile filename \n";

proc inputfile {{ctmp} {fpack_link CLASINPUT}} {
    global tcl_rec_input rchar_control 

#    setc rec_input " ";

    fpack "OPEN $fpack_link UNIT=01 FILE=\"$ctmp\" READ RECL=32768 STATUS=OLD"

}

puts "defining the <outputfile> procedure, which defines the output file";
puts "usage:  outputfile filename \n";

proc outputfile {{ctmp  junk.evt} {fpack_link CLASOUTPUT} {split 0}} {
    global rec_output rchar_control new_unit
    setc rec_output " ";

    if {[info exists new_unit] != 1 || $new_unit <= 1} {
	set new_unit 2
    }
    set split_cmd " ";
    if {$split != 0} then {
	set split_cmd  "SPLITMB=$split";
    }

    fpack "OPEN $fpack_link UNIT=$new_unit FILE=\"$ctmp\" WRITE RECL=32768 $split_cmd STATUS=NEW"

    incr new_unit 1

}

puts "Defining  <doevent> procedure ref page 16b in FPACK manual \n";
puts "usage: doevent \"1000 2000 3000 4000:\" "
proc doevent {ievent} {

    fpack "select CLASINPUT NUMRB= $ievent"

} 
puts "Defining  <dorun> procedure ref page 16b in FPACK manual \n";
puts "usage: dorun \"1000 2000 3000 4000:\" "
proc dorun {ievent} {
    global rchar_control fpk_cmd
    fpack "select CLASINPUT NUMRA= $ievent"
} 

puts "defining the <quit> procedure, which causes immediate job termination"; 
puts "usage:  quit\n";

proc quit {} {
    global lquit;
    set lquit -1;
}

puts "defining the <exit> procedure, which causes an orderly job termination"; 
puts "usage:  exit\n";

proc exit {} {
    global lexit;
    set lexit -1;
}

puts "defining the <exit_pend> procedure, which causes an orderly job termination, pending the end of the event loop"; 
puts "usage:  exit_pend\n";

proc exit_pend {} {
    global lexit_pend;
    set lexit_pend -1;
}

puts "defining the <status> procedure, which dumps jobs \
	status to the terminal"; 
puts "usage:  status\n";

proc status {} {
    global lstat;
    set lstat -1;
}


puts "defining the <turnon> procedure for turning on \
	reconstruction packages";
puts "usage: turnon \[ seb trk cc tof egn lac trig tagger user start ALL \] \n";
proc turnon {args} {
    global lseb_do ltrk_do  lcc_do ltof_do legn_do lst_do ltime_do ltagger_do \
	    ltrig_do lsc_do lec_do lusr0_do lusr1_do llac_do ldc_do lec1_do lhbid_do ltbid_do;
    foreach c $args {
     if [string match ALL $c] {
	 set lseb_do -1;
	 set ltrk_do -1;
	 set lcc_do  -1;
	 set ltof_do -1;
	 set legn_do -1;
	 set lst_do  -1;
	 set ltime_do -1;
	 set ltrig_do -1;
	 set lsc_do   -1;
	 set lec_do   -1;
     set lhbid_do -1;    
     set ltbid_do -1;    
	 set lusr0_do -1;
	 set lusr1_do -1;
	 set lusr2_do -1;
     }
     if [string match seb $c] {
	 set lseb_do -1;
     }
     if [string match trk $c] {
	 set ltrk_do -1;
	 set ldc_do -1;
     }
     if [string match cc $c] {
	 set lcc_do -1;
     }
     if [string match tof $c] {
	 set ltof_do -1;
	 set lsc_do -1;
     }
     if [string match egn $c] {
	 set legn_do -1;
	 set lec_do -1;
     }
     if [string match lac $c] {
	 set lec1_do -1;
     }
     if [string match tagger $c] {
	 set ltagger_do -1;
     }
     if [string match trig $c] {
	 set ltrig_do -1;
     }
     if [string match pid $c] {
     set lhbid_do -1;    
     set ltbid_do -1;    
     }
     if [string match start $c] {
	 set lsc_do -1;    
     }
     if {[string match user $c] || \
	     [string match cc $c] || \
	     [string match trk $c]} {
	 set lusr0_do -1;
	 set lusr1_do -1;
	 set lusr2_do -1;
     }
 }
}
puts "defining the <turnoff> procedure for turning off \
	reconstruction packages";
puts "usage: turnoff \[ seb trk cc tof egn lac trig tagger user start ALL \] \n";
proc turnoff {args} {
    global lseb_do ltrk_do  lcc_do ltof_do legn_do lst_do ltime_do ltagger_do \
	    ltrig_do lsc_do lec_do lusr0_do lusr1_do llac_do ldc_do lec1_do \
	    lrf_do lcall_do lhbid_do ltbid_do;
    foreach c $args {
     if [string match ALL $c] {
         set lrf_do   0
	 set lcall_do 0
	 set lseb_do  0
	 set ltrk_do  0
	 set lcc_do   0
	 set ltof_do  0
	 set legn_do  0
	 set lst_do   0
	 set ltime_do 0
	 set ltrig_do 0
	 set lsc_do   0
	 set lec_do   0
	 set lec1_do  0
     set lhbid_do 0
     set ltbid_do 0
	 set lusr0_do 0
	 set lusr1_do 0
	 set lusr2_do 0
	 set lac_do   0
     }
     if [string match seb $c] {
	 set lseb_do 0;
     }
     if [string match rf $c] {
	 set lrf_do 0;
     }
     if [string match trk $c] {
	 set ltrk_do 0;
	 set ldc_do 0;
     }
     if [string match cc $c] {
	 set lcc_do 0;
     }
     if [string match tof $c] {
	 set ltof_do 0;
	 set lsc_do 0;
     }
     if [string match egn $c] {
	 set legn_do 0;
	 set lec_do 0;
     }
     if [string match lac $c] {
	 set lec1_do 0;
     }
     if [string match pid $c] {
       set lhbid_do 0;    
       set ltbid_do 0;    
     }
     if [string match tagger $c] {
	 set ltagger_do 0;
     }
     if [string match trig $c] {
	 set ltrig_do 0;
     }
     if [string match start $c] {
	 set lsc_do 0;
     }
     if [string match user $c] {
	 set lusr0_do 0;
	 set lusr1_do 0;
	 set lusr2_do 0;
     }
 }
}

puts "Defining to <paw> procedure, to launch a PAW session type: paw\n";

proc paw {} {
# first determine if this is a shared memory job and if so create/mod
# .pawlogon.kumac so that the shared memory
# is attached at startup
    global iseq env
    set imem 0
# first remove old .clas_shared.kumac files

    if [file exists .clas_shared.kumac] {exec rm .clas_shared.kumac}

    if {$iseq >= 0} {
        if {$iseq < 10} {
	    set memory_name HB0$iseq
	} else {
	    set memory_name HB$iseq
	}

# check to see that the users .pawlogon has "exec .clas_shared.kumac" command

	if {[file exists $env(HOME)/.pawlogon.kumac]} {
	    set pawfile [open $env(HOME)/.pawlogon.kumac r]
	    set pat "^exec .clas_shared.kumac$"
	    set iok 0
	    while {[gets $pawfile line] >= 0} {
		if [regexp $pat $line] {set iok 1; set imem 1}
	    }
	    close $pawfile

	    if {$iok == 0} {
		puts "You need to insert:     exec .clas_shared.kumac       into your"
		puts -nonewline ".pawlogon.kumac file.  \
			Do you want me to do it for you? (yes/no)\a"
		gets stdin  answer 

		if {$answer == "yes" } {
		    puts "My you are a trusting individual......."
		    puts "Moving .pawlogon.kumac to .pawlogon.kumac.old"
		    exec mv $env(HOME)/.pawlogon.kumac $env(HOME)/.pawlogon.kumac.old

		    set pawfile [open $env(HOME)/.pawlogon.kumac.old r]
		    set kumac [open $env(HOME)/.pawlogon.kumac w+]
		    puts $kumac "| kumac for automated RECSIS-paw linking"
		    puts $kumac "|"
		    puts $kumac "exec .clas_shared.kumac" 
		    puts $kumac "|" 
		    puts $kumac "| your pawlogon commands follow-------" 
		    while {[gets $pawfile line] >= 0} {puts $kumac $line}
		    close $pawfile
		    close $kumac
		    set imem 1
		} else { puts "What you don't trust me \???" }
	    } 
	} else {
	    set kumac [open $env(HOME)/.pawlogon.kumac w+]
	    puts $kumac "| kumac for automated RECSIS-paw linking"
	    puts $kumac "| place  exec .clas_shared.kumac in your"
	    puts $kumac "| .pawlogon file please"
	    puts $kumac "exec $env(PWD)/.clas_shared.kumac" 
	    puts $kumac "|" 
	    close $kumac
	    set imem  1

	}
	
	set kumac [open $env(HOME)/.clas_shared.kumac w+]
	puts $kumac "| kumac for automated RECSIS-paw linking"
	puts $kumac "| place  exec .clas_shared.kumac in your"
	puts $kumac "| .pawlogon file please"
	puts $kumac "global_sect $memory_name" 
	puts $kumac "|" 

	close $kumac;
    }
    if {$imem == 0} {
	puts "Launching PAW, shared memory not enabled!\n";
	exec xterm -cr blue -title "recsis+paw-NOSHARED" \
		-font fixed -ls -ms red +s -sb -n paw -e paw &
    } else {
	puts "Launching PAW, will try to automatically attach to \
		shared memory $memory_name\r";
	exec xterm -cr blue -title "recsis+paw-$memory_name" \
		-font fixed -ls -ms red +s -sb -n paw -e paw &
    }
}

puts "Defining to <ced> procedure, to launch a ced session type: ced\n";
proc ced {} {
    exec ced >> /dev/null &
}


  


