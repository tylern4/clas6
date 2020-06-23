#!/usr/bin/wish -f

global copy_parms copy_parms_help
set copy_parms 0
set copy_parms_help 0
set copy_selected_parms 1

global copy_from_sec copy_to_sec
global copy_from_sup copy_to_sup

proc CopyParmsDialog {} {

	global copy_parms
   global sec sup
	global copy_from_sec copy_to_sec
	global copy_from_sup copy_to_sup
	global copy_selected_parms

	# Make sure window is not already open
	if {[expr $copy_parms!=0]} then {return}
	set copy_parms 1

	set path .copy_parms
	toplevel $path

	wm title $path "DC3 Copy Paramters"
	wm transient $path .

	# Initialize globals to current setting in control window
	set copy_from_sec $sec
	set copy_from_sup $sup
	set copy_to_sec $sec
	set copy_to_sup $sup

	################## Create some frames ##################
	frame $path.top
	frame $path.bot -relief groove -borderwidth 2
	pack $path.top $path.bot -side top -fill both

	################## Create "Dismiss" button ##############
	button $path.bot.dismiss -text "Dismiss" -command {destroy .copy_parms;set copy_parms 0}
	pack $path.bot.dismiss

	
   ############ Create "From" Superlayer/Sector buttons ##########
   frame $path.top.from -borderwidth 3 -relief groove
   pack $path.top.from -side left
   label $path.top.from.lab -text "Copy From"
   pack $path.top.from.lab -side top
   set w $path.top.from.secsup
   frame $w 
   frame $w.sec -borderwidth 3 -relief groove
   frame $w.sup -borderwidth 3 -relief groove
   label $w.sec.lab -text "Sector"
   label $w.sup.lab -text "Superlayer"
   pack $w.sec.lab -side top
   pack $w.sup.lab -side top
   for {set i 6} {[expr $i>=0]} {incr i -1} {
      radiobutton $w.sec.sec$i -text Sec$i -value $i -variable copy_from_sec -anchor w
      radiobutton $w.sup.sup$i -text SL$i -value $i -variable copy_from_sup -anchor w
      pack $w.sec.sec$i -side bottom -fill x
      pack $w.sup.sup$i -side bottom -fill x
   }
   $w.sec.sec0 configure -text "All"
   $w.sup.sup0 configure -text "All" -state disabled
   pack $w.sup $w.sec -side left
   pack $w -side top -fill x 

	################# "-->" arrow ###################
	label $path.top.arrow -text "-->"
	pack $path.top.arrow -side left

   ############ Create "To" Superlayer/Sector buttons ##########
   frame $path.top.to -borderwidth 3 -relief groove
   pack $path.top.to -side left
   label $path.top.to.lab -text "Copy To"
   pack $path.top.to.lab -side top
   set w $path.top.to.secsup
   frame $w
   frame $w.sec -borderwidth 3 -relief groove
   frame $w.sup -borderwidth 3 -relief groove
   label $w.sec.lab -text "Sector"
   label $w.sup.lab -text "Superlayer"
   pack $w.sec.lab -side top
   pack $w.sup.lab -side top
   for {set i 6} {[expr $i>=0]} {incr i -1} {
      radiobutton $w.sec.sec$i -text Sec$i -value $i -variable copy_to_sec -anchor w
      radiobutton $w.sup.sup$i -text SL$i -value $i -variable copy_to_sup -anchor w
      pack $w.sec.sec$i -side bottom -fill x
      pack $w.sup.sup$i -side bottom -fill x
   }
   $w.sec.sec0 configure -text "All"
   $w.sup.sup0 configure -text "All" -state disabled
   pack $w.sup $w.sec -side left
   pack $w -side top -fill x 

	################### Copy buttons ################
	set w $path.top.cb
	frame $w
	pack $w -side right -fill y
	button $w.copy -text "Copy" -command {CopyUserParms $copy_from_sec $copy_to_sec $copy_from_sup $copy_to_sup $copy_selected_parms}
	pack $w.copy -side top -fill both
	button $w.copysector -text "Copy to all\nSectors" -command {CopyParmsSector}
	pack $w.copysector -side top -fill both
	checkbutton $w.copyselected -text "Copy selected\nparameters" -variable copy_selected_parms
	pack $w.copyselected -side top -fill x -ipady 10
	button $w.help -text "help" -command {CopyParmsHelp}
	pack $w.help -side bottom -fill both

	CenterWindow $path
}

proc CopyParmsHelp {} {

	global copy_parms copy_parms_help
	
	# Make sure copy_parms window is open
	if {[expr $copy_parms==0]} then {return}

	# Make sure window is not already open
	if {[expr $copy_parms_help!=0]} then {return}
	set copy_parms_help 1

	# Create help message in string
	set mess ""
	append mess "This window allows you to copy the parms used in one superlayer to another. "
	append mess "This uses only the \\"user\\" parameters.\\n\\n"
	append mess "Use \\"Copy\\" to copy a single superlayer/sector.\\n\\n"
	append mess "Use \\"Copy to all Sectors\\" to copy a single superlayer/sector to all sectors "
	append mess "of the same superlayer."
	append mess "\\n\\n"
	append mess "If the 'Copy selected parameters' button is checked, then only the following "
	append mess "parameters are copied: ff, p1-p4, xvst function type, and all timewalk "
	append mess "parameters.\\n\\n"
	append mess "If it is not checked, then the remaining parameters, including tzero and tmax "
	append mess "are copied in addition to those listed above."

	# Open dialog window
	tk_dialog .copy_parms_help "DC3: Copy Parms Help" $mess info 0 "OK"

	set copy_parms_help 0
}

proc CopyParmsSector {} {

	global copy_from_sec copy_to_sec
	global copy_from_sup copy_to_sup

	set save_copy_to_sec $copy_to_sec

	for {set i 0} {[expr $i<=6]} {incr i} {
		set copy_to_sec $i
		update
		update
		after 100
		.copy_parms.top.cb.copy invoke
	}

	set copy_to_sec $save_copy_to_sec
}







