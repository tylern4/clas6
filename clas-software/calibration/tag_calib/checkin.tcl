#!/bin/sh
#
#$Id: checkin.tcl,v 1.3 2008/09/25 19:23:05 fklein Exp $
#
# script: checkin.tcl <source_runno> <data_dir> [<item_new>]
#    default to check in: subsystem=tag_t item=dt_left/right,slope_left/right
#    optional <item_new>: item_new="edt" -> subsystem=tag_e item=dt
#                         item_new="tci" -> subsystem=tag_t item=ci
#
# the next line restarts using wish \
exec wish "$0" "$@"

set dummyrc  ".dummy.rc"
set dummylog ".dummy.log"

set cenv(host) "CLAS_CALDB_HOST"
set cenv(user) "CLAS_CALDB_USER"
set cenv(pass) "CLAS_CALDB_PASS"
set cenv(base) "CLAS_CALDB_DBNAME"
set cenv(indx) "RUN_INDEX"

set caldb(host) ""
set caldb(user) ""
set caldb(pass) ""
set caldb(base) ""
set caldb(indx) ""
set caldb(runm) [lindex $argv 0]
set caldb(rmax) [lindex $argv 0]
set caldb(rsrc) [lindex $argv 0]
set caldb(comi) ""
set caldb(comc) ""

set cdefault(host) "clasdb.jlab.org"
set cdefault(user) "clasuser"
set cdefault(pass) ""
set cdefault(base) "calib"
set cdefault(indx) "RunIndex"

set clabel(host) "Caldb server hostname" 
set clabel(user) "Caldb user"
set clabel(pass) "Caldb password"
set clabel(base) "Database"
set clabel(indx) "RunIndex table"
set clabel(runm) "Valid for run"
set clabel(rmax) "-"
set clabel(rsrc) "Source from run"
set clabel(comi) "Comment for RunIndex"
set clabel(comc) "Comment for constants"

set perl5lib "$env(CLAS_TOOLS)/caldb"
#set perl5lib "/group/clas/tools/caldb"
#catch {set perl5lib    $env(PERL5LIB)}
set perlprog "caldb_write_and_link.pl"

catch {set caldb(user) $env(USER)}
set isSync 1

set constPath "/tmp"
if {[llength $argv] > 1} {
    set constPath "[lindex $argv 1]/"
}

set itemnew "-"
if {[llength $argv] > 2} {
    set itemnew "[lindex $argv 2]"
}

proc checkIn {} {
    global caldb  perl5lib perlprog dummyrc dummylog constPath itemnew
    if {! [string is integer $caldb(runm)] || ! [string is integer $caldb(rmax)] } {
	tk_messageBox -type ok -icon error -message \
	    "Non integer entry for run number"
	return
    }

    if {! [string is integer $caldb(rsrc)] } {
	tk_messageBox -type ok -icon error -message \
	    "Non integer entry for source run number"
	return
    }

    if {! $caldb(runm) || ! $caldb(rmax) } {
	tk_messageBox -type ok -icon error -message \
	    "Missing valid range of run numbers this calibration spans"
	return
    }

    if {$caldb(runm) > $caldb(rmax) } {
	tk_messageBox -type ok -icon error -message \
	    "First run number must be less or equal last one"
	return
    }

    if {"caldb(comi)"=="" ||  "$caldb(comc)"=="" } {
	tk_messageBox -type ok -icon error -message \
	    "Please enter a comment for the RunIndex"
	return
    }

    if {$caldb(rsrc) < $caldb(runm) || $caldb(rsrc) > $caldb(rmax)} {
	if {[tk_messageBox -type yesno -icon warning -message \
		 "Source run number not in validity range, continue anyhow?"]=="no"} {
	    return
	}
    }
    
    set passwd ""
    if {"$caldb(pass)"!=""} {set passwd "password=$caldb(pass)"}
		
    set ff [open $dummyrc w]
    puts $ff "#!/bin/sh"
    puts $ff "export PERL5LIB=$perl5lib"

    if {"$itemnew"=="-"} {

	foreach tx {dt slope} {
	    foreach lr {left right} {
		set item "${tx}_$lr"
		set ss   "tag_t"
		set fname "$constPath/$item.$caldb(rsrc).dat"
	    
		puts $ff "$perl5lib/$perlprog hostname=$caldb(host) user=$caldb(user) \
                          $passwd s=TAG_CALIB ss=$ss i=$item min=$caldb(runm)   \
                          max=$caldb(rmax) srmin=$caldb(rsrc) srmax=$caldb(rsrc)      \
                          ci=\"$caldb(comi)\" cc=\"$caldb(comc)\" f=$fname            \
                          it=$caldb(indx)"
	    }
	}
    } else {
	if {"$itemnew"=="edt"} {
	    set ss   "tag_e"
	    set item "dt"
	    set fname "$constPath/tage_dt.$caldb(rsrc).dat"
	}
	if {"$itemnew"=="tci"} {
	    set ss   "tag_t"
	    set item "ci"
	    set fname "$constPath/tagt_ci.$caldb(rsrc).dat"
	}

	puts $ff "$perl5lib/$perlprog hostname=$caldb(host) user=$caldb(user) \
                          $passwd s=TAG_CALIB ss=$ss i=$item min=$caldb(runm)   \
                          max=$caldb(rmax) srmin=$caldb(rsrc) srmax=$caldb(rsrc)      \
                          ci=\"$caldb(comi)\" cc=\"$caldb(comc)\" f=$fname            \
                          it=$caldb(indx)"
    }
    close $ff
    exec chmod 755 $dummyrc

    set perlOutput "*** can't read the error output of the perl script ***"
    if [catch {set perlOutput [exec $dummyrc 2> $dummylog]}] {
	tk_messageBox -type ok -icon error -message "error in perlscript, check output!"
	catch { set perlOutput [exec cat $dummylog] }
    }

    toplevel .output
    wm title .output "Output from perl script"

    text .output.text -relief sunken -borderwidth 3
    .output.text insert 0.0 $perlOutput

    button .output.but -text OK -width 9 -command exit
    pack .output.text .output.but -padx 2m -pady 2m
}

proc updateComc par {
    global caldb isSync

    set ch [lindex $par 0]
    set ky [lindex $par 1]
    set newSync 0

    if {"$caldb(comc)"=="$caldb(comi)"} {
	if {[string is control $ch] || [string length $ch]==2} {
	    set newSync 1
	    switch -exact -- $ky {
		"BackSpace"     {if $isSync {set caldb(comc) [string range $caldb(comc) 0 end-1]}}
		"KP_Enter"      {}
		"Return"        {}
		"Up"            {}
		"Down"          {}
		"Shift_L"       {}
		"Shift_R"       {}
		default  {set newSync 0}
	    }
	} else {	
	    if $isSync { set caldb(comc) "$caldb(comc)$ch" }
	    set newSync 1
	}
    }
    set isSync $newSync
}

proc showForms {} {
    global caldb cdefault clabel cenv env itemnew

    foreach var {host user pass base indx} {
	catch { set caldb($var) $env($cenv($var)) }
	if {"$caldb($var)"==""} {set caldb($var) $cdefault($var) }
    }

    frame .info -borderwidth 3 -relief groove
    if { "$itemnew"=="-" } {
	label .info.l -text "Constants for TAG_CALIB.tag_t: dt_left&right slope_left&right"
    }
    if {"$itemnew"=="edt"} { 
	label .info.l -anchor w -text "Constants for TAG_CALIB.tag_e: dt"
    }
    if {"$itemnew"=="tci"} { 
	label .info.l -anchor w -text "Constants for TAG_CALIB.tag_t: ci"
    }
    pack .info.l -side left -padx 2m -pady 2m

    foreach var {host user pass base indx rsrc comi comc} {
	frame .$var   -relief groove -borderwidth 3
	label .$var.l -anchor w -width 20 -text $clabel($var) 
	entry .$var.e -width 30 -textvariable caldb($var)
	pack  .$var.l .$var.e -side left -padx 2m -pady 2m
    }

    .pass.e config -show *
    frame .runm -relief groove -borderwidth 3
    label .runm.l -anchor w -width 20 -text "Valid for run"
    entry .runm.e -width 10 -textvariable caldb(runm)
    label .runm.s  -text "-"
    entry .runm.x -width 10 -textvariable caldb(rmax) 
    pack .runm.l .runm.e .runm.s .runm.x -side left -padx 2m -pady 2m
    
    frame .butt
    button .butt.ok  -width 9 -command checkIn -text "OK"
    frame  .butt.f   
    button .butt.esc -width 9 -command exit  -text "cancel"


    pack .butt.ok  -side left -pady 2m
    pack .butt.f   -side left  -fill x -expand yes
    pack .butt.esc -side left -pady 2m
    
    pack .info .host .user .pass .indx .runm .rsrc .comi .comc .butt -padx 2m -pady 2m -fill x

    bind .comi.e <Key> {updateComc {"%A" "%K"}}
}

proc interactiveSearch {} {
    global perl5lib perlprog
    set title "Searching for $perlprog..."

    set filename [tk_getOpenFile -title $title -parent .]

    catch {set perl5lib [file dirname $filename] }

    if {"$filename" !="" && [file exists "$perl5lib/$perlprog" ]} {
	showForms
    } else {
	if {[tk_messageBox -type ok -icon error -message \
		 "perl script $perlprog not found"]!=""} exit
    }
}

proc searchPerlscript {} {
    global perl5lib perlprog

    if [file exists "$perl5lib/$perlprog" ] {
	showForms
    } elseif [file exists "/group/clas/tools/caldb/$perlprog" ] {
	set perl5lib "/group/clas/tools/caldb"
	showForms
    } elseif [file exists "[pwd]/$perlprog" ] {
	set perl5lib [pwd]
	showForms
    } else {
	interactiveSearch
    }
}

searchPerlscript
