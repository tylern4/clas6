#!/usr/bin/wish -f


####################################################
# Note: This dialog makes use of the "scrollvalue" #
# and "entryvalue" routines in xvst_plot.tcl       #
####################################################

# These will be set by C to the appropriate limits
set cut_high  1.0
set cut_low   0.0
set cut_left  0.5
set cut_right 0.5

set cut_cancel 0
set cutbutton 0

set tlothi 0


#
# This does the initial setup of the filter box in the
# "Fit" tab. The setup must be completed from C when
# the names of the Ntuple fields are determined.
#
proc init_filter {path} {

   global filter_path
   global cutbutton
   global tlothi
   global smallfont mediumfont largefont hugefont
   
   set filter_path $path
   
   set w $filter_path
   label $w.lab -text "Filter" -font $mediumfont
   frame $w.lbf
   listbox $w.lbf.lb -width 13 -height 7 -font $smallfont \
      -yscrollcommand {$filter_path.lbf.sb set}
   # (listbox elements filled from C)
   scrollbar $w.lbf.sb -command {$filter_path.lbf.lb yview}
   bind $w.lbf.lb <ButtonRelease> {SetCutButton}
   pack $w.lbf.lb $w.lbf.sb -side left -fill both
   checkbutton $w.cut -text "Cut" -font $smallfont -variable cutbutton
   bind $w.cut <ButtonRelease> {SetCuts}
   pack $w.lab $w.lbf $w.cut -side top -fill both

   checkbutton $w.tlothi -text "Std. time cut" -font $smallfont -variable std_tcut -width 15
   bind $w.tlothi <Double-Button> {Edit_t_limits}
   pack $w.tlothi -side top -fill both

}

proc SetCutDialog {name upper lower max min} {
   global cut_upper cut_lower cut_max cut_min cut_cancel
   
   set cut_upper $upper
   set cut_lower $lower
   set cut_max   $max
   set cut_min   $min

   toplevel .c
   wm transient .c .
   wm title .c "Set Filter Cut"

   # this is really used to set the scrollbar width
   set label_width 40

   # main label
   label .c.lab -text [format "Set cut limits for %s" $name] \
      -width $label_width -relief groove -borderwidth 2
   pack .c.lab -side top -ipady 5 -pady 5 -padx 3

   # OK and Cancel buttons
   frame .c.but
   button .c.but.ok -text "OK" -command {SetCutDialogEnd 0}
   pack .c.but.ok -side right -fill x
   button .c.but.cancel -text "Cancel" -command {SetCutDialogEnd 1}
   pack .c.but.cancel -side left -fill x
   pack .c.but -side bottom -fill x

   # upper
   frame .c.r
   label .c.r.lab -text "upper" -width $label_width
   frame .c.r.sbf
   scrollbar .c.r.sbf.sb -orient horizontal \
      -command {scrollvalue $cut_min $cut_max .c.r.sbf}
   entry .c.r.sbf.en -width 8
   .c.r.sbf.en insert 0 $cut_upper
   bind .c.r.sbf.en <KeyPress-KP_Enter> {entryvalue $cut_min $cut_max .c.r.sbf}
   bind .c.r.sbf.en <Return>            {entryvalue $cut_min $cut_max .c.r.sbf}
   pack .c.r.sbf.en -side right
   pack .c.r.sbf.sb -side right -fill x -expand 1
   pack .c.r.lab .c.r.sbf -side top -fill both
   pack .c.r -side bottom -fill both
   entryvalue $cut_min $cut_max .c.r.sbf

   # lower
   frame .c.l
   label .c.l.lab -text "lower" -width $label_width
   frame .c.l.sbf
   scrollbar .c.l.sbf.sb -orient horizontal \
      -command {scrollvalue $cut_min $cut_max .c.l.sbf}
   entry .c.l.sbf.en -width 8
   .c.l.sbf.en insert 0 $cut_lower
   bind .c.l.sbf.en <KeyPress-KP_Enter> {entryvalue $cut_min $cut_max .c.l.sbf}
   bind .c.l.sbf.en <Return>            {entryvalue $cut_min $cut_max .c.l.sbf}
   pack .c.l.sbf.en -side right
   pack .c.l.sbf.sb -side right -fill x -expand 1
   pack .c.l.lab .c.l.sbf -side top -fill both
   pack .c.l -side bottom -fill both
   entryvalue $cut_min $cut_max .c.l.sbf
   
	CenterWindow .c
}

proc SetCutDialogEnd {cancelme} {
   global cut_upper cut_lower cut_cancel
   
   set cut_cancel $cancelme
   
   set cut_upper [.c.r.sbf.en get]
   set cut_lower [.c.l.sbf.en get]
   
   if {[expr $cut_cancel==0]} {
      if {[expr $cut_upper<$cut_lower]} {
         set msg ""
         set msg [concat $msg "Your upper cut is less than your lower cut."]
         set msg [concat $msg "The values for the two will be switched."]
         tk_dialog .c.mess "Cut values reversed" $msg "" 0 "OK"

         set tmp $cut_upper
         set $cut_upper $cut_lower
         set $cut_lower $tmp
      }
   }
   
   destroy .c
   
}




proc Edit_t_limits {} {	
   global t_lo1 t_lo2 t_lo3 t_lo4 t_lo5 t_lo6
   global t_hi1 t_hi2 t_hi3 t_hi4 t_hi5 t_hi6
   global autocut_t autocut_t_frac
   
   set win .edit_t_limit_win
   toplevel $win
   tkwait visibility $win
   wm title $win "Edit default limits on t"
   
   ####### OK, Cancel buttons #########
   frame $win.but
   button $win.but.ok -text "OK" -width 6 -command { \
      for {set super 1} {[expr $super<=6]} {incr super} {\
         set win2 .edit_t_limit_win.sl$super ;\
         set t_lo$super [$win2.lo get] ;\
         set t_hi$super [$win2.hi get] ;\
      } ;\
      if {[expr $autocut_t!=0]} then {\
         set autocut_t_frac [.edit_t_limit_win.autofind.en get] \
      };\
      destroy .edit_t_limit_win ;\
   }
   pack $win.but.ok -side right
   button $win.but.cancel -text "Cancel" -width 6 \
   	-command {destroy .edit_t_limit_win}
   pack $win.but.cancel -side left
   pack $win.but -side bottom -fill x
   
   ####### autofind #######
   set win2 $win.autofind
   frame $win2
   checkbutton $win2.autofind -variable autocut_t -text "autofind" \
      -command {Set_t_limit_state}
   entry $win2.en -width 6
   $win2.en insert 0 $autocut_t_frac

   pack $win2.autofind $win2.en -side left
   pack $win2 -side bottom

   ####### limit entries ######
   frame $win.lab
   label $win.lab.lab1 -text "Layer" -width 5
   label $win.lab.lab2 -text "t_lo" -width 10 -borderwidth 2 -relief groove
   label $win.lab.lab3 -text "t_hi" -width 10 -borderwidth 2 -relief groove
   pack $win.lab.lab1 $win.lab.lab2 $win.lab.lab3 -side left
   pack $win.lab -side top
   
   for {set super 1} {[expr $super<=6]} {incr super} {
      
      set win2 $win.sl$super
      frame $win2
      label $win2.lab -text SL$super -width 5
      entry $win2.lo -width 10
      entry $win2.hi -width 10
      pack $win2.lab $win2.lo $win2.hi -side left
      pack $win2 -side top
      
      $win2.lo insert 0 [set t_lo$super]
      $win2.hi insert 0 [set t_hi$super]
   }
      
   Set_t_limit_state
   CenterWindow .edit_t_limit_win
   tkwait window .edit_t_limit_win
}

proc Set_t_limit_state {} {	
   global autocut_t autocut_t_frac

   set win .edit_t_limit_win
   set win2 $win.autofind
   if {[expr $autocut_t==0]} then {
      $win2.en configure -state disabled -fg "#777"
      set color "#000"
      set en_state normal
   } else {
      $win2.en configure -state normal -fg "#000"
      focus $win2.en
      set color "#777"
      set en_state disabled
   }
   
   $win.lab.lab1 configure -foreground $color
   $win.lab.lab2 configure -foreground $color
   $win.lab.lab3 configure -foreground $color
   for {set super 1} {[expr $super<=6]} {incr super} { 
      $win.sl$super.lab configure -foreground $color 
      $win.sl$super.lo configure -state $en_state -fg $color
      $win.sl$super.hi configure -state $en_state -fg $color
   }

}












