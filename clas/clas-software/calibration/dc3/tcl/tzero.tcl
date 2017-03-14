#!/usr/bin/wish -f

set tzero_method 1
set tzero_plot_min 1
set tzero_plot_max 500
set tzero_fit_min 10
set tzero_fit_max 30

proc find_tzero {win} {
   global sec sup
   global tlimt tlim_low
   global cancel_op
   global tzero
   global find_tzero_win
   global normalfont boldfont bigboldfont bigfont smallfont hugeboldfont hugefont
   global tzero_method
	global tzero_plot_min tzero_plot_max
	global tzero_fit_min tzero_fit_max
   
   set find_tzero_win $win

	### Tzero Method #############
	set w $win.method
	frame $w
	pack $w -side top -fill x
	label $w.lab -text "Method:"
	radiobutton $w.mac -text "Mac" -variable tzero_method -value 1
	radiobutton $w.dave -text "Dave" -variable tzero_method -value 2 -state disabled
	pack $w.lab $w.mac $w.dave -side left -fill both

	set w $win.limits
	frame $w -borderwidth 2 -relief groove
	pack $w -side top -fill both
	label $w.unitslab -text "(units are histogram bins 1 to 500)"
	pack $w.unitslab -side top -fill x
	set w $win.limits.f
	frame $w
	pack $w -side bottom -fill both

	## Plot limits ############
	set w2 $w.plotlimits
	frame $w2 -borderwidth 2 -relief groove
	pack $w2 -side left -fill both
	label $w2.lab -text "Plot Limits"
	pack $w2.lab -side top -fill x
	frame $w2.max -borderwidth 1 -relief groove
	button $w2.max.minusten -text "-100" -padx 0 -command {ShiftTzeroLimit plotlimits.max -100}
	button $w2.max.minus -text "-10" -width 4 -padx 0 -command {ShiftTzeroLimit plotlimits.max -10}
	entry $w2.max.en -width 4
	button $w2.max.plus -text "+10" -width 4 -padx 0  -command {ShiftTzeroLimit plotlimits.max +10}
	button $w2.max.plusten -text "+100" -padx 0  -command {ShiftTzeroLimit plotlimits.max +100}
	label $w2.max.lab -text "max"
	pack $w2.max.lab -side left
	pack $w2.max.plusten $w2.max.plus $w2.max.en $w2.max.minus $w2.max.minusten  -side right
	frame $w2.min -borderwidth 1 -relief groove
	button $w2.min.minusten -text "-100" -padx 0 -command {ShiftTzeroLimit plotlimits.min -100}
	button $w2.min.minus -text "-10" -width 4 -padx 0 -command {ShiftTzeroLimit plotlimits.min -10}
	entry $w2.min.en -width 4
	button $w2.min.plus -text "+10" -width 4 -padx 0 -command {ShiftTzeroLimit plotlimits.min +10}
	button $w2.min.plusten -text "+100" -padx 0  -command {ShiftTzeroLimit plotlimits.min +100}
	label $w2.min.lab -text "min"
	pack $w2.min.lab -side left
	pack $w2.min.plusten $w2.min.plus $w2.min.en $w2.min.minus $w2.min.minusten  -side right
	pack $w2.max $w2.min -side top -fill both

	$w2.min.en insert 0 $tzero_plot_min
	bind $w2.max.en <Return> NewTzeroPlotLimits
	$w2.max.en insert 0 $tzero_plot_max
	bind $w2.min.en <Return> NewTzeroPlotLimits

	## Fit limits ##############
	set w2 $w.fitlimits
	frame $w2 -borderwidth 2 -relief groove
	pack $w2 -side right -fill both
	label $w2.lab -text "Fit Limits"
	pack $w2.lab -side top -fill x
	frame $w2.max -borderwidth 1 -relief groove
	button $w2.max.minusten -text "-10" -padx 0 -command {ShiftTzeroLimit fitlimits.max -10}
	button $w2.max.minus -text "-1" -width 3 -padx 0 -command {ShiftTzeroLimit fitlimits.max -1}
	entry $w2.max.en -width 4
	button $w2.max.plus -text "+1" -width 3 -padx 0  -command {ShiftTzeroLimit fitlimits.max +1}
	button $w2.max.plusten -text "+10" -padx 0  -command {ShiftTzeroLimit fitlimits.max +10}
	label $w2.max.lab -text "max"
	pack $w2.max.lab -side left
	pack $w2.max.plusten $w2.max.plus $w2.max.en $w2.max.minus $w2.max.minusten  -side right
	frame $w2.min -borderwidth 1 -relief groove
	button $w2.min.minusten -text "-10" -padx 0 -command {ShiftTzeroLimit fitlimits.min -10}
	button $w2.min.minus -text "-1" -width 3 -padx 0 -command {ShiftTzeroLimit fitlimits.min -1}
	entry $w2.min.en -width 4
	button $w2.min.plus -text "+1" -width 3 -padx 0 -command {ShiftTzeroLimit fitlimits.min +1}
	button $w2.min.plusten -text "+10" -padx 0  -command {ShiftTzeroLimit fitlimits.min +10}
	label $w2.min.lab -text "min"
	pack $w2.min.lab -side left
	pack $w2.min.plusten $w2.min.plus $w2.min.en $w2.min.minus $w2.min.minusten  -side right
	pack $w2.max $w2.min -side top -fill both

	$w2.min.en insert 0 $tzero_fit_min
	bind $w2.max.en <Return> NewTzeroFitLimits
	$w2.max.en insert 0 $tzero_fit_max
	bind $w2.min.en <Return> NewTzeroFitLimits

   ################# Tzero  ####################
   set w $win.tzero
   frame $w -borderwidth 3 -relief groove
   pack $w -side top -fill x

   frame $win.tzero.ctls
   frame $win.tzero.find
   frame $win.tzero.lab
   pack $win.tzero.ctls -side right -fill both
   pack $win.tzero.lab $win.tzero.find -side left

	label $win.tzero.lab.lab -text "Set Tzero:"
	pack $win.tzero.lab.lab -side top -fill y
   
   set w $win.tzero.ctls.sbf
   frame $w
   scrollbar $w.sb -orient horizontal\
      -command {scrollvalue $tlim_low $tlimt($sup) $find_tzero_win.tzero.ctls.sbf}
   entry $w.en -width 10 -textvariable tzero
   scrollvalue $tlim_low $tlimt($sup) $find_tzero_win.tzero.ctls.sbf moveto 0
   bind $w.sb <ButtonRelease> {set sec $sec}
   bind $w.en <KeyPress-KP_Enter> \
      {entryvalue $tlim_low $tlimt($sup) $find_tzero_win.tzero.ctls.sbf}
   bind $w.en <Return> \
      {entryvalue $tlim_low $tlimt($sup) $find_tzero_win.tzero.ctls.sbf}
   pack $w.sb -side left -fill x -expand 1
   pack $w.en -side left
   pack $w -side top -fill both

   set w $win.tzero.ctls.delta
   frame $w
   button $w.hunplus -text "+100" -font $smallfont -command {DeltaTime 100 $find_tzero_win.tzero.ctls.sbf}
   button $w.tenplus -text "+10" -font $smallfont -command {DeltaTime 10 $find_tzero_win.tzero.ctls.sbf}
   button $w.oneplus -text "+1" -font $smallfont -command {DeltaTime 1 $find_tzero_win.tzero.ctls.sbf}
   pack $w.hunplus $w.tenplus $w.oneplus -side right
   button $w.hunminus -text "-100" -font $smallfont -command {DeltaTime -100 $find_tzero_win.tzero.ctls.sbf}
   button $w.tenminus -text "-10" -font $smallfont -command {DeltaTime -10 $find_tzero_win.tzero.ctls.sbf}
   button $w.oneminus -text "-1" -font $smallfont -command {DeltaTime -1 $find_tzero_win.tzero.ctls.sbf}
   pack $w.hunminus $w.tenminus $w.oneminus -side left
   pack $w -side bottom -fill both

	## Middle frame for fit buttons and Info ##############
	set w $win.mf
	frame $w -borderwidth 2 -relief groove
	pack $w -side top -fill both

	## Fit buttons ###################
	set w $win.mf.fit
	frame $w
	pack $w -side left -fill x -ipadx 10
	button $w.fitone -width 10 -text "Fit One" -command {SetTzeroFromLinearFit "one"}
	button $w.fitsup -width 10 -text "Fit Sup" -command {SetTzeroFromLinearFit "sup"}
	button $w.fitall -width 10 -text "Fit All" -command {SetTzeroFromLinearFit "all"}
	#pack 	$w.fitone $w.fitsup $w.fitall -side top
	pack 	$w.fitone -side top

	## Information labels ############
	set w $win.mf.info
	frame $w 
	pack $w -side top -fill both
	label $w.integralfraction -text "Integral Fraction : " -anchor w
	label $w.tzero -text "Tzero : "  -anchor w
	label $w.initialtzero -text "Initial Tzero : "  -anchor w
	pack $w.integralfraction $w.tzero $w.initialtzero -side top -fill x

	## Buttons #################
	set w $win.buttons
	frame $w
	pack $w -side bottom -fill x
	button $w.reset -text "Reset" -state disabled
	button $w.plotdoca -text "Plot DOCA" -command {PlotCalcDOCA}
        pack $w.plotdoca -side left -fill x
	pack $w.reset -side right

        ## Misc Buttons #################
        ##  Added by Philip Coltharp, coltharp@jlab.org
	set w $win.misc
	frame $w
        button $w.tzerotable -text "Tzero Table" -command {TzeroTable}
        pack $w.tzerotable -side left 
        pack $w -side bottom -fill x

		
}

proc NewTzeroPlotLimits {} {

	global find_tzero_win
	global tzero_plot_min tzero_plot_max
	global replot

	set w2 $find_tzero_win.limits.f.plotlimits
	set tzero_plot_min [$w2.min.en get]
	set tzero_plot_max [$w2.max.en get]

	set replot 1
}

proc NewTzeroFitLimits {} {

	global find_tzero_win
	global tzero_fit_min tzero_fit_max
	global replot

	set w2 $find_tzero_win.limits.f.fitlimits
	set tzero_fit_min [$w2.min.en get]
	set tzero_fit_max [$w2.max.en get]

	set replot 1
}

proc ShiftTzeroLimit {who delta} {

	global find_tzero_win
	set w $find_tzero_win.limits.f.$who
	set val [$w.en get]
	$w.en delete 0 end
	$w.en insert 0 [expr $val+$delta]

	NewTzeroFitLimits
	NewTzeroPlotLimits
}





