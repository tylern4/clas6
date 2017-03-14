#!/usr/bin/wish -f


set hold_par1  0
set hold_par2  0
set hold_par3  0
set hold_par4  0
set hold_tmax  1
set hold_tzero 1
set hold_ff    1


proc fit_init {path} {

   global fit_path
   global hold_par1 hold_par2 hold_par3 hold_par4
   global hold_tmax hold_tzero hold_ff
   global seek_first
   global cancel
   global smallfont mediumfont largefont hugefont
   
   set fit_path $path
  
   ######## frame to hold all info and basic controls #########
   set win $fit_path.novice
   frame $win -relief raised -borderwidth 3
   pack $win -side top -fill both
   
   # Fit buttons
   set w $win.fit
   frame $w -relief groove -borderwidth 2
   pack $w -side left -fill y
   button $w.fitone -text "Fit One" -width 7 -font $largefont -command {FitOne}
   button $w.fitsup -text "Fit Sup" -width 7 -font $largefont -command {FitSuperlayer}
   button $w.fitall -text "Fit All" -width 7 -font $largefont -command {FitAll}
   checkbutton $w.seek -text "Seek\nFirst" -font $smallfont -variable seek_first
   button $w.cancel -text "Cancel" -width 7 -font $mediumfont -command {set cancel 1}
   button $w.reset -text "Reset" -width 7 -font $mediumfont -command {ResetUserParms}
   pack $w.fitone $w.fitsup $w.fitall $w.seek -side top
   pack $w.cancel $w.reset -side bottom
   
   # starting values
   set w $win.sv
   frame $w -relief groove -borderwidth 2
   pack $w -side left
   label $w.lab -text "Starting Values" -anchor center -width 15 -font $mediumfont
   label $w.p1    -text "par 1:" -width 15 -anchor w -font $smallfont
   label $w.p2    -text "par 2:" -width 15 -anchor w -font $smallfont
   label $w.p3    -text "par 3:" -width 15 -anchor w -font $smallfont
   label $w.p4    -text "par 4:" -width 15 -anchor w -font $smallfont
   label $w.tmax  -text "tmax :" -width 15 -anchor w -font $smallfont
   label $w.tzero -text "tzero:" -width 15 -anchor w -font $smallfont
   label $w.ff    -text "ff   :" -width 15 -anchor w -font $smallfont
   label $w.chisq -text "chisq:" -width 15 -anchor w -font $smallfont
   pack $w.lab $w.p1 $w.p2 $w.p3 $w.p4 $w.tmax $w.tzero $w.ff $w.chisq -side top

   # final values
   set w $win.fv
   frame $w -relief groove -borderwidth 2
   pack $w -side left
   label $w.lab -text "Final Values" -anchor center -width 15 -font $mediumfont
   label $w.p1    -text "par 1:" -width 15 -anchor w -font $smallfont
   label $w.p2    -text "par 2:" -width 15 -anchor w -font $smallfont
   label $w.p3    -text "par 3:" -width 15 -anchor w -font $smallfont
   label $w.p4    -text "par 4:" -width 15 -anchor w -font $smallfont
   label $w.tmax  -text "tmax :" -width 15 -anchor w -font $smallfont
   label $w.tzero -text "tzero:" -width 15 -anchor w -font $smallfont
   label $w.ff    -text "ff   :" -width 15 -anchor w -font $smallfont
   label $w.chisq -text "chisq:" -width 15 -anchor w -font $smallfont
   pack $w.lab $w.p1 $w.p2 $w.p3 $w.p4 $w.tmax $w.tzero $w.ff $w.chisq -side top
   
   # hit filter
   set w $win.filter
   frame $w -relief groove -borderwidth 2
   pack $w -side right
   init_filter $w

	################# More Novice controls #####################
	set win $fit_path.bottom
   frame $win -relief groove -borderwidth 3
   pack $win -side bottom -fill both

	set win $fit_path.bottom.novice
   frame $win
   pack $win -side left -fill both

   set w $win.chisqb
   frame $w -relief groove -borderwidth 3
   pack $w -side left -fill both

	button $w.chisqvec -text "Chisq Vector" -font $mediumfont -command {ChisqVector}
	button $w.chisqtab -text "Chisq Table" -font $mediumfont -command {ChisqTable}
	pack $w.chisqvec $w.chisqtab -side top -fill both

	button $w.copyparms -text "Copy Parameters" -font $mediumfont -command {CopyParmsDialog}
	pack $w.copyparms -side bottom -fill x
   
   #################### expert controls etc. ##################
   set win $fit_path.bottom.expert
   frame $win -relief groove -borderwidth 3
   pack $win -side bottom -fill both

   #################### even more novice controls etc. ##################
   set win $fit_path.bottom.hold
   frame $win -relief groove -borderwidth 3
   pack $win -side bottom -fill both

   # Hold buttons
   set w $win.hb
   frame $w -relief groove -borderwidth 2
   pack $w -side left -fill both
   label $w.lab -text "Hold"
   pack $w.lab -side top
   frame $w.f
   pack $w.f -side top -fill both
   frame $w.f.l
   pack $w.f.l -side left -fill both
   checkbutton $w.f.l.p1 -text "par 1" -width 8 -anchor w -variable hold_par1
   checkbutton $w.f.l.p2 -text "par 2" -width 8 -anchor w -variable hold_par2
   checkbutton $w.f.l.p3 -text "par 3" -width 8 -anchor w -variable hold_par3
   checkbutton $w.f.l.p4 -text "par 4" -width 8 -anchor w -variable hold_par4
   pack $w.f.l.p1 $w.f.l.p2 $w.f.l.p3 $w.f.l.p4 -side top
   frame $w.f.r
   pack $w.f.r -side left -fill both
   checkbutton $w.f.r.tmax -text "tmax" -width 8 -anchor w -variable hold_tmax
   checkbutton $w.f.r.tzero -text "tzero" -width 8 -anchor w -variable hold_tzero
   checkbutton $w.f.r.ff -text "ff" -width 8 -anchor w -variable hold_ff
   pack $w.f.r.tmax $w.f.r.tzero $w.f.r.ff -side top

}





