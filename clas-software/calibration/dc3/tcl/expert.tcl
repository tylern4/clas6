#!/usr/bin/wish -f


# The routines in here unpack/pack and disable/enable
# widgets to bring them in and out of "expert" mode

global expert



# Trace the "expert" variable that is changed whenever
# the "Expert Mode" checkbutton is toggled
trace variable expert w {ExpertToggle}

# The routine called when "expert" is changed. This just
# calls either "ExpertOn" or "ExpertOff" depending on the
# value of "expert".
proc ExpertToggle {name1 name2 ops} {
   global expert
   
   if {[expr $expert == 0]} then {ExpertOff} else {ExpertOn}
}

# Pack and enable all "expert" features
proc ExpertOn {} {
   
   global find_tmax_win options_path fit_path

   # Warn user about expert mode
   ExpertWarn
   
   set state normal
   
   # Main window
   #pack .middle.left.secsup.sec.sec0 .middle.left.secsup.sup.sup0 -side top -fill x
   pack $options_path.plottype -side top -fill both
   pack $options_path.recalc -side top -fill x
   pack $options_path.recalcmethod -side top -fill x
   pack $options_path.corrt -side top -fill x
	pack $options_path.aec -side top

   # Tmax/Tzero
   pack $find_tmax_win.tzero -fill x -side top

   # Fit
   pack $fit_path.bottom.expert

}

# Unpack and disable all "expert features"
proc ExpertOff {} {

   global sec sup
   global find_tmax_win options_path fit_path show_residual

   set state disabled

   # Main window
	#pack forget .middle.left.secsup.sec.sec0 .middle.left.secsup.sup.sup0
   if {[expr $sec==0]} then {set sec 1}
   if {[expr $sup==0]} then {set sup 1}
   if {[expr $show_residual==0]} then {set show_residual 1}
   pack forget $options_path.plottype
   pack forget $options_path.recalc
   pack forget $options_path.recalcmethod
   pack forget $options_path.corrt -side top -fill x
	pack forget $options_path.aec -side top

   # Tmax/Tzero
   pack forget $find_tmax_win.tzero

   # Fit
   pack forget $fit_path.bottom.expert

	# Expert Controls Window
	CloseExpertControls
}

proc ExpertWarn {} {

   tk_dialog .expertwarn "Expert Mode" \
"Warning: Any changes made while in expert mode will
require a password before allowing you to write
to the Map." \
   warning 0 OK 

}


# Disable all expert features at startup
ExpertToggle "" "" ""




