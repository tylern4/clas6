#!/usr/bin/wish -f

global expert_controls
set expert_controls 0

global tw_type xvst_type

set tw_type 0
set xvst_type 0

proc OpenExpertControls {} {

	global expert_controls
	global tw_type xvst_type
	global sec sup

	if {[expr $expert_controls!=0]} then {return}
	set expert_controls 1

	set path .expert_controls
	toplevel $path

	wm title $path "DC3 expert Controls"
	wm transient $path .


	button $path.dismiss -text "Dismiss" -command {CloseExpertControls}
	pack $path.dismiss -side bottom

	
   ########## Timewalk function ######################################
	set win $path.timewalk
	frame $win -relief groove -borderwidth 4
	pack $win -side top -fill both -pady 5

	set w $win.tf
	frame $w
	pack $w -side top -fill y

	label $win.tf.lab -text "Timewalk Function type:"
	pack $win.tf.lab -side left
	set w $win.tf.twmenu
   menubutton $w -relief raised -text "Timewalk Function" -width 20 -menu $w.m
   pack $w -side left -fill both
   menu $w.m
      $w.m add radiobutton -label "Classic(Liming)" -variable tw_type -value 0
      $w.m add radiobutton -label "Vipuli"          -variable tw_type -value 1
      $w.m add radiobutton -label "Mac"             -variable tw_type -value 2
      $w.m add radiobutton -label "None"            -variable tw_type -value 3
	trace variable tw_type w {SetTWType}
	SetTWType 0 0 0

	########### Timewalk parameter entries #################
	set w $win.bot
	frame $w
	pack $w -side top -fill both

	for {set region 1} {[expr $region<=3]} {incr region} {

		set rw $w.f$region
		frame $rw -relief groove -borderwidth 2
		pack $rw -side left -fill both

		set pc $rw.l
		frame $pc
		pack $pc -side left -fill both		
		for {set p 1} {[expr $p<=5]} {incr p} {
			set pf $pc.p$p
			frame $pf
			pack $pf -side top -fill both

			set str [format "R%d %d" $region $p]
			label $pf.lab -text $str
			entry $pf.en -width 8
			pack $pf.lab $pf.en -side left
		}

		set pf $pc.fac
		frame $pf
		pack $pf -side top -fill both
		label $pf.lab -text "fac"
		entry $pf.en -width 8
		pack $pf.lab $pf.en -side left

		set pf $pc.tau
		frame $pf
		pack $pf -side top -fill both
		label $pf.lab -text "tau" 
		entry $pf.en -width 8
		pack $pf.lab $pf.en -side left


		set pc $rw.r
		frame $pc
		pack $pc -side left -fill both		
		for {set p 6} {[expr $p<=10]} {incr p} {
			set pf $pc.p$p
			frame $pf
			pack $pf -side top -fill both

			set str [format "R%d %d" $region $p]
			label $pf.lab -text $str
			entry $pf.en -width 8
			pack $pf.lab $pf.en -side left
		}

		set inner "axial" ; set outer "stereo"
		if {[expr $region==1]} then {set inner "stereo" ; set outer "axial"}

		set pf $pc.betaslope_inner
		frame $pf
		pack $pf -side top -fill both
		label $pf.lab -text [format "betaslope\n(%s)" $inner] 
		entry $pf.en -width 8
		pack $pf.lab $pf.en -side left

		set pf $pc.betaslope_outer
		frame $pf
		pack $pf -side top -fill both
		label $pf.lab -text [format "betaslope\n(%s)" $outer]  
		entry $pf.en -width 8
		pack $pf.lab $pf.en -side left
		
	}

	set w $win.but
	frame $w
	pack $w -side top -fill both
	button $w.apply -text "Apply" -command {ApplyExpertParameters timewalk}
	button $w.reread -text "Reset" -command {ResetUserParms}
	pack $w.apply $w.reread -side right
	button $w.defaults -text "Defaults" -command {SetParametersToDefault timewalk}
	pack $w.defaults -side left
	
	
   ########## XvsT function ######################################
	set win $path.xvst
	frame $win -relief groove -borderwidth 4
	pack $win -side top -fill both -pady 5

	set w $win.tf
	frame $w
	pack $w -side top -fill y

	label $win.tf.lab -text "X vs. T Function type:"
	pack $win.tf.lab -side left
	set w $win.tf.xvstmenu
   menubutton $w -relief raised -text "X vs. T Function" -width 20 -menu $w.m
   pack $w -side left -fill both
   menu $w.m
      $w.m add radiobutton -label "Classic(Liming)" -variable xvst_type -value 0
      $w.m add radiobutton -label "Polynomial"      -variable xvst_type -value 1
      $w.m add radiobutton -label "Power"           -variable xvst_type -value 2
      $w.m add radiobutton -label "Bessel"          -variable xvst_type -value 3
      $w.m add radiobutton -label "Legendre"        -variable xvst_type -value 4
	trace variable xvst_type w {SetXvsTType}
	SetXvsTType 0 0 0

	########### X vs. T parameter entries #################
	set w $win.bot
	frame $w
	pack $w -side top -fill both

	set p 1
	for {set section 1} {[expr $section<=6]} {incr section} {

		set pc $w.f$section
		frame $pc
		pack $pc -side left -fill both		
		for {set i 1} {[expr $i<=4]} {incr i;incr p} {
			set pf $pc.p$p
			frame $pf
			pack $pf -side top -fill both

			set str [format "par %2d" $p]
			label $pf.lab -text $str
			entry $pf.en -width 8
			pack $pf.lab $pf.en -side left
		}
		
	}

	set w $win.but
	frame $w
	pack $w -side top -fill both
	button $w.apply -text "Apply" -command {ApplyExpertParameters xvst}
	button $w.reread -text "Reset" -command {ResetUserParms}
	pack $w.apply $w.reread -side right
	button $w.defaults -text "Defaults"  -command {SetParametersToDefault xvst}
	pack $w.defaults -side left

	tkwait visibility $path

	set sec $sec
}


proc CloseExpertControls {} {

	global expert_controls

	if {[expr $expert_controls==0]} then {return}
	set expert_controls 0
	destroy .expert_controls
	
}


proc SetTWType {name1 name2 ops} {

	global expert_controls
	global tw_type

	if {[expr $expert_controls==0]} then {return}

	set str "Unknown"
	
	switch $tw_type {
		0 {set str "Classic(Liming)" }
		1 {set str "Vipuli" }
		2 {set str "Mac" }
		3 {set str "None" }
	}

	.expert_controls.timewalk.tf.twmenu configure -text $str
}

proc SetXvsTType {name1 name2 ops} {

	global expert_controls
	global xvst_type

	if {[expr $expert_controls==0]} then {return}

	set str "Unknown"
	
	switch $xvst_type {
		0 {set str "Classic(Liming)" }
		1 {set str "Polynomial" }
		2 {set str "Power" }
		3 {set str "Bessel" }
		4 {set str "Legendre" }
	}

	.expert_controls.xvst.tf.xvstmenu configure -text $str
}






