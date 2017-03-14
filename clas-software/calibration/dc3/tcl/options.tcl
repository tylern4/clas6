#!/usr/bin/wish -f

set showplots 1
set show_function 1
set show_scatter 1
set show_profile 1
set show_predicted_resi 1
set show_zero_line 1

set show_residual 1
set use_corrected_time 1

proc InitOptions {path} {

   global options_path
   global expert
   global showplots show_function show_scatter show_profile
   global show_predicted_resi show_zero_line
   global recalc_residuals recalc_method
   global use_corrected_time

   set options_path $path
   
   checkbutton $path.expert -text "Expert Mode" -variable expert -anchor w 
   checkbutton $path.plot -text "Show Plots" -variable showplots -anchor w
   pack $path.expert $path.plot -side top -fill x
   menubutton $path.plotmenu -relief groove -text "Plot >" -menu $path.plotmenu.m
   pack $path.plotmenu -side top
   frame $path.plottype
   radiobutton $path.plottype.resi -text "Resi" -variable show_residual -value 1
   radiobutton $path.plottype.xvst -text "XvsT" -variable show_residual -value 0
   pack $path.plottype.resi $path.plottype.xvst -side left -fill both
   checkbutton $path.recalc -text "Re-calc. Residuals" -variable recalc_residuals
   frame $path.recalcmethod
   radiobutton $path.recalcmethod.fct -text "Function" -variable recalc_method -value 0
   radiobutton $path.recalcmethod.table -text "Table" -variable recalc_method -value 1
   pack $path.recalcmethod.fct $path.recalcmethod.table -side left -fill both
   checkbutton $path.corrt -text "Corrected t-axis" -variable use_corrected_time -anchor w 
   pack $path.corrt -side top -fill x
   button $path.aec -text "Additional Expert\nControls" \
   	-command {OpenExpertControls}
   
   ########## Make Plot menu ######################################
   set w $path.plotmenu.m
   menu $w
      $w add checkbutton -label "Function" -variable show_function
      $w add checkbutton -label "Scatter" -variable show_scatter
      $w add checkbutton -label "Profile" -variable show_profile
      $w add checkbutton -label "Zero Line" -variable show_zero_line
      $w add checkbutton -label "Predicted Resi" -variable show_predicted_resi

   ######### disable/enable items by tracing variables ###########
   trace variable showplots w {UpdatePlotsMenuState}
   trace variable recalc_residuals w {UpdateRecalcMethodState}

}

proc UpdatePlotsMenuState {name1 name2 op} {
   
   global options_path showplots
   
   if {[expr $showplots==0]} {
      $options_path.plotmenu configure -state disabled \
   } else {
      $options_path.plotmenu configure -state normal \
   }

}

proc UpdateRecalcMethodState {name1 name2 op} {
   
   global options_path recalc_residuals
   
   if {[expr $recalc_residuals==0]} {
      $options_path.recalcmethod.fct configure -state disabled ;\
      $options_path.recalcmethod.table configure -state disabled ;\
   } else {
      $options_path.recalcmethod.fct configure -state normal ;\
      $options_path.recalcmethod.table configure -state normal ;\
   }

}















