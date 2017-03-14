#!/usr/bin/wish -f


global find_tmax_win
global tzero tmax
global tmax_frac tzero_frac
global tzero_autofrac tmax_region1_autofrac tmax_region2_autofrac tmax_region3_autofrac


set tmax 100.0
set tzero 0.0
set tmax_frac 0.97
set tzero_frac 0.03
set tmax_autofrac 0.0

set smallfont  "-*-helvetica-medium-o-*-*-9-*-*-*-*-*-*-*"
set normalfont  "-*-times-medium-r-*-*-14-*-*-*-*-*-*-*"
set boldfont    "-*-times-bold-o-*-*-10-*-*-*-*-*-*-*"
set bigfont     "-*-helvetica-medium-o-*-*-14-*-*-*-*-*-*-*"
set bigboldfont "-*-times-bold-r-*-*-14-*-*-*-*-*-*-*"
set hugefont     "-*-helvetica-medium-o-*-*-20-*-*-*-*-*-*-*"
set hugeboldfont "-*-times-bold-r-*-*-20-*-*-*-*-*-*-*"
set fixedfont    "-*-fixed-bold-*-*-*-12-*-*-*-*-*-*-*"

proc find_tmax {win} {
   global sec sup
   global find_tmax_win tmax_frac tzero_frac tmax_region1_autofrac tmax_region2_autofrac tmax_region3_autofrac
   global tlimt tlim_low
   global cancel_op
   global tzero tmax
   global normalfont boldfont bigboldfont bigfont smallfont hugeboldfont hugefont
   global tmax_autofrac
	
   set find_tmax_win $win

   ################# Tzero  ####################
   set w $win.tzero
   frame $w -borderwidth 3 -relief groove
   pack $w -side top -fill both -pady 30

   set w $win.tzero.top
   frame $w
   label $w.lab -text "Tzero" -borderwidth 2 -relief groove -padx 40 -pady 5
   pack $w.lab -side left -fill both
   frame $w.f
   frame $w.f.int
   label $w.f.int.ifraclab -text "Integral fraction:"
   label $w.f.int.ifrac -textvariable tzero_frac -width 6 -anchor w
   pack $w.f.int.ifrac $w.f.int.ifraclab -side right
   pack $w.f.int -side top -fill both
   frame $w.f.auto
   entry $w.f.auto.en -width 6 -textvariable tzero_autofrac
   bind $w.f.auto.en <Return> {$find_tmax_win.tzero.find.find invoke}
   label $w.f.auto.enlab -text "Autofind fraction:"
   pack $w.f.auto.en $w.f.auto.enlab -side right
   pack $w.f.auto -side top -fill both
   pack $w.f -side right -fill both
   pack $w -side top -fill both

   frame $win.tzero.ctls
   frame $win.tzero.find
   pack $win.tzero.ctls -side right -fill both
   pack $win.tzero.find -side left
   
   button $win.tzero.find.find -text "Find" -width 10 -command {FindTFraction $tzero_autofrac tzero}
   button $win.tzero.find.findall -text "Find All" -width 10 -command {FindTAll $tmax_autofrac tzero}
   pack $win.tzero.find.find $win.tzero.find.findall -side top -fill x
   
   set w $win.tzero.ctls.sbf
   frame $w
   scrollbar $w.sb -orient horizontal\
      -command {scrollvalue $tlim_low $tlimt($sup) $find_tmax_win.tzero.ctls.sbf}
   entry $w.en -width 10 -textvariable tzero
   scrollvalue $tlim_low $tlimt($sup) $find_tmax_win.tzero.ctls.sbf moveto 0
   bind $w.sb <ButtonRelease> {set sec $sec}
   bind $w.en <KeyPress-KP_Enter> \
      {entryvalue $tlim_low $tlimt($sup) $find_tmax_win.tzero.ctls.sbf}
   bind $w.en <Return> \
      {entryvalue $tlim_low $tlimt($sup) $find_tmax_win.tzero.ctls.sbf}
   pack $w.sb -side left -fill x -expand 1
   pack $w.en -side left
   pack $w -side top -fill both

   set w $win.tzero.ctls.delta
   frame $w
   button $w.hunplus -text "+100" -font $smallfont -command {DeltaTime 100 $find_tmax_win.tzero.ctls.sbf}
   button $w.tenplus -text "+10" -font $smallfont -command {DeltaTime 10 $find_tmax_win.tzero.ctls.sbf}
   button $w.oneplus -text "+1" -font $smallfont -command {DeltaTime 1 $find_tmax_win.tzero.ctls.sbf}
   pack $w.hunplus $w.tenplus $w.oneplus -side right
   button $w.hunminus -text "-100" -font $smallfont -command {DeltaTime -100 $find_tmax_win.tzero.ctls.sbf}
   button $w.tenminus -text "-10" -font $smallfont -command {DeltaTime -10 $find_tmax_win.tzero.ctls.sbf}
   button $w.oneminus -text "-1" -font $smallfont -command {DeltaTime -1 $find_tmax_win.tzero.ctls.sbf}
   pack $w.hunminus $w.tenminus $w.oneminus -side left
   pack $w -side bottom -fill both
   

   ################# Tmax  ####################
   set w $win.tmax
   frame $w -borderwidth 3 -relief groove
   pack $w -side top -fill both -pady 30

   set w $win.tmax.top
   frame $w
   label $w.lab -text "Tmax" -borderwidth 2 -relief groove -padx 40 -pady 5
   pack $w.lab -side left -fill both
   frame $w.f
   frame $w.f.int
   label $w.f.int.ifraclab -text "Integral fraction:"
   label $w.f.int.ifrac -textvariable tmax_frac -width 6 -anchor w
   pack $w.f.int.ifrac $w.f.int.ifraclab -side right
   pack $w.f.int -side top -fill both
   frame $w.f.autoreg1
   entry $w.f.autoreg1.en -width 6 -textvariable tmax_region1_autofrac
   bind $w.f.autoreg1.en <Return> {$find_tmax_win.tmax.find.find invoke}
   label $w.f.autoreg1.enlab -text "Autofind fraction (Region1):"
   pack $w.f.autoreg1.en $w.f.autoreg1.enlab -side right
   pack $w.f.autoreg1 -side top -fill both
   frame $w.f.autoreg2
   entry $w.f.autoreg2.en -width 6 -textvariable tmax_region2_autofrac
   bind $w.f.autoreg2.en <Return> {$find_tmax_win.tmax.find.find invoke}
   label $w.f.autoreg2.enlab -text "Autofind fraction (Region2):"
   pack $w.f.autoreg2.en $w.f.autoreg2.enlab -side right
   pack $w.f.autoreg2 -side top -fill both
   frame $w.f.autoreg3
   entry $w.f.autoreg3.en -width 6 -textvariable tmax_region3_autofrac
   bind $w.f.autoreg3.en <Return> {$find_tmax_win.tmax.find.find invoke}
   label $w.f.autoreg3.enlab -text "Autofind fraction (Region3):"
   pack $w.f.autoreg3.en $w.f.autoreg3.enlab -side right
   pack $w.f.autoreg3 -side top -fill both
   frame $w.f.auto
   pack $w.f -side right -fill both
   pack $w -side top -fill both

   frame $win.tmax.ctls
   frame $win.tmax.find
   pack $win.tmax.ctls -side right -fill both
   pack $win.tmax.find -side left
   
   button $win.tmax.find.find -text "Find" -width 9 -command \
   {FindTFraction [expr $sup>2 ? ($sup>4 ? $tmax_region3_autofrac:$tmax_region2_autofrac):$tmax_region1_autofrac] tmax}
   button $win.tmax.find.findall -text "Find All" -width 9 -command {FindTAll $tmax_autofrac tmax}
   pack $win.tmax.find.find $win.tmax.find.findall -side top -fill x
   
   set w $win.tmax.ctls.sbf
   frame $w
   scrollbar $w.sb -orient horizontal\
      -command {scrollvalue $tlim_low $tlimt($sup) $find_tmax_win.tmax.ctls.sbf}
   entry $w.en -width 10 -textvariable tmax
   scrollvalue $tlim_low $tlimt($sup) $find_tmax_win.tmax.ctls.sbf moveto 0
   bind $w.sb <ButtonRelease> {set sec $sec}
   bind $w.en <KeyPress-KP_Enter> \
      {entryvalue $tlim_low $tlimt($sup) $find_tmax_win.tmax.ctls.sbf}
   bind $w.en <Return> \
      {entryvalue $tlim_low $tlimt($sup) $find_tmax_win.tmax.ctls.sbf}
   pack $w.sb -side left -fill x -expand 1
   pack $w.en -side left
   pack $w -side top -fill both

   set w $win.tmax.ctls.delta
   frame $w
   button $w.hunplus -text "+100" -font $smallfont -command {DeltaTime 100 $find_tmax_win.tmax.ctls.sbf}
   button $w.tenplus -text "+10" -font $smallfont -command {DeltaTime 10 $find_tmax_win.tmax.ctls.sbf}
   button $w.oneplus -text "+1" -font $smallfont -command {DeltaTime 1 $find_tmax_win.tmax.ctls.sbf}
   pack $w.hunplus $w.tenplus $w.oneplus -side right
   button $w.hunminus -text "-100" -font $smallfont -command {DeltaTime -100 $find_tmax_win.tmax.ctls.sbf}
   button $w.tenminus -text "-10" -font $smallfont -command {DeltaTime -10 $find_tmax_win.tmax.ctls.sbf}
   button $w.oneminus -text "-1" -font $smallfont -command {DeltaTime -1 $find_tmax_win.tmax.ctls.sbf}
   pack $w.hunminus $w.tenminus $w.oneminus -side left
   pack $w -side bottom -fill both
 
   ############### Reset buttons #################
   set w $win.reset
   frame $w
   button $w.reset -text "Reset" -command {ResetTValue}
   button $w.resetall -text "Reset All" -command {ResetTValues}
   pack $w.reset $w.resetall -side left -ipadx 20
   pack $w -side bottom
 
   ############### Tmax Table button #################
   ## Added by Philip Coltharp, coltharp@jlab.org
   set w $win.misc
   frame $w
   button $w.tmaxtable -text "Tmax Table" -command {TmaxTable}
   pack $w.tmaxtable -side left 
   pack $w -side bottom -fill x
 
}

proc DeltaTime {del path} {

   global sec sup
   global tlimt tlim_low
   
   # Get current Setting
   set val [expr [$path.en get]+$del]
   
   # Clear entry and write in new value
   $path.en delete 0 end
   $path.en insert 0 $val
   
   # Update scrollbar
   entryvalue $tlim_low $tlimt($sup) $path

}


proc FindTAll {frac who} {

   global find_tmax_win
   global sec sup
   global setreplotglobal_id
   
   set save_sec $sec
   set save_sup $sup

	# Have the screen update while we're filling the histos
	SetReplotGlobal
   
   for {set sup 1} {[expr $sup<7]} {incr sup} {
      for {set sec 0} {[expr $sec<7]} {incr sec} {
         update
         $find_tmax_win.$who.find.find invoke
      }
      set sec 6
   }
   
   set sup $save_sup
   set sec $save_sec

	after cancel after idle SetReplotGlobal
	after cancel SetReplotGlobal
}


