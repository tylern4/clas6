#!/usr/bin/wish -f


set usercomment ""

# These defaults are overwritten from C in LinkTclToC()
set caldb_host      "clasdb.jlab.org"
set caldb_runindex  "calib_user.RunIndexDavidL"
set caldb_user      "davidl"
set caldb_password  ""

set minrun 0
set maxrun 0

set boldfont    "-*-times-bold-o-*-*-10-*-*-*-*-*-*-*"

proc caldb_init {path} {

   global caldb_path
   global caldb_host caldb_runindex caldb_user caldb_password
   global runnumber minrun maxrun
   global boldfont

   set caldb_path $path
   
   ######## checkbuttons #######
   set win $caldb_path.buttons
   frame $win -relief groove -borderwidth 3
   pack $win -side top -fill both

   ######## Write buttons ######
   set win $caldb_path.controls
   frame $win
   pack $win -side bottom -fill both -pady 3
   button $win.map  -text "Write to Database" -command {CaldbWrite}
   pack $win.map

   ####### Frame for caldb info and run numbers #######
   set win $caldb_path.ri
   frame $win -relief groove -borderwidth 2
   pack $win -side top -fill both -pady 10
   
   ######### RunNumbers ###########
   set win $caldb_path.ri.run
   frame $win -relief groove -borderwidth 2
   pack $win -side left -fill both -ipady 3

   frame $win.run
   label $win.run.lab -text "Run Number:"
   label $win.run.num -textvariable runnumber
   pack $win.run.lab $win.run.num -side left -fill x

   frame $win.minrun
   label $win.minrun.lab -text "Min. Run Num.:"
   entry $win.minrun.en -width 10 -textvariable minrun
   pack $win.minrun.lab $win.minrun.en -side left -fill x

   frame $win.maxrun
   label $win.maxrun.lab -text "Max. Run Num.:"
   entry $win.maxrun.en -width 10 -textvariable maxrun
   pack $win.maxrun.lab $win.maxrun.en -side left -fill x

   pack $win.run $win.minrun $win.maxrun -side top 

   ######### Database info ###########
   set win $caldb_path.ri.dbi
   frame $win -relief groove -borderwidth 2
   pack $win -side right -fill x -ipady 3

   label $win.title -text "Database Info."
   pack $win.title -side top -fill x

	frame $win.info
   pack $win.info -side top -fill x
   frame $win.info.labs
   frame $win.info.vals
   pack $win.info.labs $win.info.vals -side left
   
   label $win.info.labs.host		-anchor e -text "HOST:"
   label $win.info.vals.host		-anchor w -textvariable caldb_host
   
   label $win.info.labs.runindex	-anchor e -text "RUNINDEX:"
   label $win.info.vals.runindex	-anchor w -textvariable caldb_runindex
   
   label $win.info.labs.user		-anchor e -text "USER:"
   label $win.info.vals.user		-anchor w -textvariable caldb_user
   
   label $win.info.labs.password	-anchor e -text "PASSWORD:"
   label $win.info.vals.password	-anchor w -text ""
#   label $win.info.vals.password	-anchor w -textvariable caldb_password

	pack $win.info.labs.host $win.info.labs.runindex $win.info.labs.user $win.info.labs.password -side top -fill x
	pack $win.info.vals.host $win.info.vals.runindex $win.info.vals.user $win.info.vals.password -side top -fill x

   button $win.mod -text "Modify" -width 10 -command {ModifyDBInfo}
   pack $win.mod -side bottom

   ####### Frame for defaults and misc #######
   set win $caldb_path.misc
   frame $win -relief groove -borderwidth 2
   pack $win -side top -fill both -pady 10

   ####### Defaults #########
   set win $caldb_path.misc.def
   frame $win -relief groove -borderwidth 2
   pack $win -side left -fill both -ipady 3
	button $win.settodef -text "Set to Defaults" -command {SetDefaults 1}
	pack $win.settodef -side top -fill x
	button $win.setalltodef -text "Set All to Defaults" -command {SetAllDefaults}
	pack $win.setalltodef -side top -fill x
}

proc UpdateMinMaxRun {var val} {
	global caldb_path minrun maxrun

	$caldb_path.ri.run.$var.en delete 0 end
	$caldb_path.ri.run.$var.en insert 0 $val
}

proc ModifyDBInfo {} {
	global changedbinfowin
	global mdbi_result
   global caldb_host caldb_runindex caldb_user caldb_password
   
   set changedbinfowin .changedbinfo
   toplevel $changedbinfowin

	### Title ###
	set win $changedbinfowin
	label $win.title -text "Modify DB Access Info."
	pack $win.title -side top -fill x
	wm title $changedbinfowin "dc3: Modifying Database Access Information"

	### Information ###
	frame $win.info -relief groove -borderwidth 3
   pack $win.info -side top -fill x
   frame $win.info.labs
   frame $win.info.vals
   pack $win.info.labs $win.info.vals -side left
   
   label $win.info.labs.host		-anchor e -text "HOST:"
   entry $win.info.vals.host		-width 40
   $win.info.vals.host insert 0 $caldb_host
   
   label $win.info.labs.runindex	-anchor e -text "RUNINDEX:"
   entry $win.info.vals.runindex	-width 40
   $win.info.vals.runindex insert 0 $caldb_runindex
   
   label $win.info.labs.user		-anchor e -text "USER:"
   entry $win.info.vals.user		-width 40
   $win.info.vals.user insert 0 $caldb_user
   
   label $win.info.labs.password	-anchor e -text "PASSWORD:"
   entry $win.info.vals.password	-width 40 -show *
   $win.info.vals.password insert 0 $caldb_password

	pack $win.info.labs.host $win.info.labs.runindex $win.info.labs.user $win.info.labs.password -side top -fill x
	pack $win.info.vals.host $win.info.vals.runindex $win.info.vals.user $win.info.vals.password -side top -fill x

	### OK and Cancel buttons ###
	frame $win.buttons
	pack $win.buttons -side bottom -fill x
	button $win.buttons.ok -text "OK" -command {set mdbi_result 1}
	pack $win.buttons.ok -side right
	button $win.buttons.cancel -text "Cancel" -command {set mdbi_result 0}
	pack $win.buttons.cancel -side left

   set mdbi_result 0
   CenterWindow $changedbinfowin
   tkwait variable mdbi_result

   if [expr $mdbi_result==1] {
		set caldb_host			[$win.info.vals.host get		]
		set caldb_runindex	[$win.info.vals.runindex get	]
		set caldb_user			[$win.info.vals.user get		]
		set caldb_password	[$win.info.vals.password get	]
   }

   destroy $changedbinfowin
}

proc NewMapFile {} {

   global mapfile

   set types {
      {{Map files} {.map} }
      {{All files} *      }
   }
   
   set fname [tk_getOpenFile -filetypes $types -title "Select new DC_DOCA.map file"]
   
   if {$fname!=""} {
      set mapfile $fname
   }
}

proc GetPassword {} {
   
   global passwin passwd

   set passwd ""

   set passwin .passwin
   toplevel $passwin
   wm title $passwin "Expert Password"
   
   label $passwin.mess1 -text "Some values were obtained while in"
   label $passwin.mess2 -text "expert mode. Please enter the expert"
   label $passwin.mess3 -text "mode password."
   pack $passwin.mess1 $passwin.mess2 $passwin.mess3 -side top -fill both
   
   set win $passwin.pass
   frame $win -relief groove -borderwidth 3
   pack $win -side top -fill x
   label $win.lab -text "Password:"
   entry $win.en -width 20 -show *
   bind $win.en <Return> {$passwin.buttons.ok invoke}
   pack $win.lab $win.en -side left -fill x
   
   set win $passwin.buttons
   frame $win -relief groove -borderwidth 3
   pack $win -side bottom -fill x
   button $win.ok -text "OK" -command {\
      set passwd [$passwin.pass.en get]; \
      destroy $passwin;\
   }
   button $win.cancel -text "Cancel" -command {destroy $passwin}
   pack $win.ok -side left
   pack $win.cancel -side right
   CenterWindow $passwin
   tkwait window $passwin
   
   return $passwd
}


proc AdditionalComments {} {

   global usercomment
   global commentswin
   global ac_result
   
   set commentswin .comments
   toplevel $commentswin

   set win $commentswin
   label $win.lab -text "Please enter any additional comments"
   pack $win.lab -side top
   
   text $win.text -height 5 -width 40
   $win.text insert end $usercomment
   pack $win.text -side top -fill both
   focus $win.text

   set win $commentswin.buttons
   frame $win
   pack $win -side bottom -fill both
   button $win.ok -text "OK" -command {set ac_result "OK"}
   button $win.cancel -text "Cancel" -command {set ac_result "Cancel"}
   pack $win.ok -side left
   pack $win.cancel -side right

   set ac_result "None"
   CenterWindow $commentswin
   tkwait variable ac_result
   
   set usercomment [$commentswin.text get 1.0 end]
   destroy $commentswin
   
   return $ac_result
}

#
# OK, we need to be careful here. There is a C routine linked to a Tcl
# routine called "SetParametersToDefault" that we will call from here.
# That routine sets parameters in the "Additional Expert Controls" 
# window to the defaults for whatever function type is currently
# specified.
#
# 	This routine will make sure the "Additional Expert Controls"
# window is open, select the "Power" X vs. T function type and "Mac"
# timewalk types, invoke the "Defaults" buttons (which, in turn, will
# call "SetParametersToDefault"). After that, it will invoke the 
# "ApplyExpertParameters" buttons to copy the values from the "Additional
# Expert Controls" window into the appropriate C data structures.
# Whew! 
proc SetDefaults {close_aec} {
	global options_path
	global expert_controls

	# Open "Additional Expert Controls" window
	invokeButton $options_path.aec

	# Select "Mac" timewalk type and "Power" X vs. T type
	.expert_controls.timewalk.tf.twmenu.m	invoke 3 ; SetTWType 0 0 0
	.expert_controls.xvst.tf.xvstmenu.m		invoke 3 ; SetXvsTType 0 0 0

	#### Here, we Set and apply the default parameters. It would be nice
	#### if we did this via inoking the buttons, but it takes significantly
	#### longer to do it that way. We also use the "bypass_warning" and
	#### "bypass_histo_refill" flags here to help speed things up.

	# Set values in A.E.C. window to defaults.
	.expert_controls.timewalk.but.defaults flash
	SetParametersToDefault timewalk xvst bypass_warning
	.expert_controls.xvst.but.defaults flash
	
	# Copy parameters from A.E.C window to C data structure
	.expert_controls.timewalk.but.apply flash
	ApplyExpertParameters timewalk xvst bypass_histo_refill
	.expert_controls.xvst.but.apply flash
	
	# Optionally close "Additional Expert Controls" window
	if {[expr $close_aec!=0]} {.expert_controls.dismiss invoke}
}


#
# Call the above routine for all sup/sec
#
proc SetAllDefaults {} {
	global sec sup
   
   set save_sec $sec
   set save_sup $sup
      
   for {set sup 1} {[expr $sup<7]} {incr sup} {
      for {set sec 1} {[expr $sec<7]} {incr sec} {
         update
         SetDefaults 0
      }
      set sec 6
   }
   
   set sup $save_sup
   set sec $save_sec
}




