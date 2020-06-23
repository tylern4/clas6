#!/usr/bin/wish -f


global notebook_page
global sec sup
global filename
global expert


set tlim(0) 1800
set tlim(1) 220
set tlim(2) 220
set tlim(3) 700
set tlim(4) 800
set tlim(5) 1300
set tlim(6) 1500
set tlim_low -20

set tlimt(0) [expr $tlim(0)*1.4]
set tlimt(1) [expr $tlim(1)*1.4]
set tlimt(2) [expr $tlim(2)*1.4]
set tlimt(3) [expr $tlim(3)*1.4]
set tlimt(4) [expr $tlim(4)*1.4]
set tlimt(5) [expr $tlim(5)*1.4]
set tlimt(6) [expr $tlim(6)*1.4]

set smallfont  "-adobe-times-*-r-*-*-12-*-*-*-*-*-*-*"
set mediumfont "-adobe-times-*-r-*-*-14-*-*-*-*-*-*-*"
set largefont  "-adobe-times-*-r-*-*-16-*-*-*-*-*-*-*"
set hugefont   "-adobe-times-*-r-*-*-18-*-*-*-*-*-*-*"


proc MainProc {} {

   global sec sup
   global filename
   global expert

   set sec 1
   set sup 1

   ############################################################
   #  This creates the main window and the menu bar.          #
   ############################################################

   ########## Create frames  ###############
   frame .top
   frame .middle
   frame .bottom

   ########## Create Menu Buttons #############################
   button .top.file -text "Open File" -command {OpenFile}
   button .top.go -text "Go" -command {go}
   canvas .top.progress -width 250 -height 20 
   button .top.help -text "Help" -state disabled
   button .top.quit -text "Quit" -command {destroy .}
   pack .top.file .top.go .top.progress -side left
   pack .top.quit .top.help -side right
   pack .top -side top -fill both
   
   ######### Make file label ###############################
   frame .curfile
   label .curfile.lab -text "Current File: "
   set filename "none"
   label .curfile.nam -textvariable filename
   pack .curfile.lab .curfile.nam -side left -anchor w
   pack .curfile -side top -fill x

   ############# Create notebook widget #################
   set item1 "T0"
   set item2 "Tmax"
   set item3 "Fit"
   set item4 "Calib. Quality"
   set item5 "CalDB"
   set item6 "Map"
   #set item1 "Calib. Quality"
   #set item2 "T0"
   #set item3 "Tmax"
   #set item4 "Fit"
   #set item5 "CalDB"
   #set item6 "Map"
   #Notebook:create .middle.n -pages [list $item1 $item2 $item3 $item4 $item5 $item6] -pad 20 
   Notebook:create .middle.n -pages [list $item4 $item1 $item2 $item3 $item5] -pad 20 
   pack .middle.n -side right -fill both -expand 1
   calibration_quality [Notebook:frame .middle.n $item4]   
   find_tzero [Notebook:frame .middle.n $item1]
   find_tmax [Notebook:frame .middle.n $item2]
   fit_init [Notebook:frame .middle.n $item3]
   caldb_init [Notebook:frame .middle.n $item5]
   #map_init [Notebook:frame .middle.n $item6]
   
   ############ Create Superlayer/Sector buttons ##########
   frame .middle.left
   set w .middle.left.secsup
   frame $w -borderwidth 3 -relief groove
   frame $w.sec -borderwidth 3 -relief groove
   frame $w.sup -borderwidth 3 -relief groove
   label $w.sec.lab -text "Sector"
   label $w.sup.lab -text "Superlayer"
   pack $w.sec.lab -side top
   pack $w.sup.lab -side top
   for {set i 6} {[expr $i>=0]} {incr i -1} {
      radiobutton $w.sec.sec$i -text Sec$i -value $i -variable sec -anchor w
      radiobutton $w.sup.sup$i -text SL$i -value $i -variable sup -anchor w
      pack $w.sec.sec$i -side bottom -fill x
      pack $w.sup.sup$i -side bottom -fill x
   }
   $w.sec.sec0 configure -text "All"
   $w.sup.sup0 configure -text "All" -state disabled
   pack $w.sup $w.sec -side left
   pack $w -side top -fill x 

   ######### Frame under sector superlayer buttons #########
   ######### (Possibly for future expansions)      #########
   frame .middle.left.bottom -borderwidth 3 -relief groove
   InitOptions .middle.left.bottom
   pack .middle.left.bottom -side top -fill both
   pack .middle.left -side left -fill both

   ################ Pack #######################
   pack .middle -side top -fill both -expand 1

}

# This is an alternative to using a real thread to handle the plotting
proc PlotThread {} {
	global replot

	if {[expr $replot!=0]} {PlotThreadTcl}

	after 500 after idle PlotThread
}




# withdraw the main window until its contents are filled
wm withdraw .

# If splash screen was shown, wait for it to go away so dc3 controls
# don't obscure it.
global splash_win
if {[string compare $splash_win "0"]} then {tkwait window $splash_win }

# Fill the controls window
MainProc

# Now map the main window
wm deiconify .
tkwait visibility .
wm title . "DC Calibration check 3"
wm resizable . 0 0


