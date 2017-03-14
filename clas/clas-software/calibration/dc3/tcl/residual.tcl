#!/usr/bin/wish -f

set resiptype  1
set resi_sup  -1
set resicut    0
set versus     1
set resi_show_field_plot 0

set fixedfont "-misc-fixed-*-*-*-*-12-20-*-*-*-*-*-*"

proc ResidualInit {} {
   global hbookdirectory
   global resiptype resi_sup resicut resi_show_field_plot
   global fixedfont

   frame .resi -relief sunken -borderwidth 5
   
   # superlayer
   frame .resi.sup -relief raised -borderwidth 3
   label .resi.sup.lab -relief groove -text "Superlayer"
   radiobutton .resi.sup.sl1 -value 1 -variable resi_sup -text "SL1"
   radiobutton .resi.sup.sl2 -value 2 -variable resi_sup -text "SL2"
   radiobutton .resi.sup.sl3 -value 3 -variable resi_sup -text "SL3"
   radiobutton .resi.sup.sl4 -value 4 -variable resi_sup -text "SL4"
   radiobutton .resi.sup.sl5 -value 5 -variable resi_sup -text "SL5"
   radiobutton .resi.sup.sl6 -value 6 -variable resi_sup -text "SL6"
   pack .resi.sup.lab .resi.sup.sl1 .resi.sup.sl2 .resi.sup.sl3 \
      	.resi.sup.sl4 .resi.sup.sl5 .resi.sup.sl6 -fill both -side top -expand 1
   pack .resi.sup -side left -expand 1 -fill both

   # The "vs." radiobuttons are packed from C so all members of
   # ntuple can be listed
   frame .resi.vs -relief raised -borderwidth 3
   label .resi.vs.lab -relief groove -text "plot vs."
   pack .resi.vs.lab -side top -fill both
   pack .resi.vs -side left -fill both -expand 1
   
   # pick scatter plot profile, or projection histogram
   frame .resi.ptype -relief raised -borderwidth 3
   label .resi.ptype.lab -relief groove -text "Plot type"
   radiobutton .resi.ptype.proj -value 2 -variable resiptype -text "projection"\
      -anchor w
   radiobutton .resi.ptype.scat -value 1 -variable resiptype -text "scatter"\
      -anchor w
   radiobutton .resi.ptype.prof -value 0 -variable resiptype -text "profile"\
      -anchor w
   pack .resi.ptype.lab .resi.ptype.scat .resi.ptype.prof .resi.ptype.proj \
      -fill both -side top
   pack .resi.ptype -side left  -fill both
   
   # cuts
   frame .resi.cut -relief raised -borderwidth 3
      label .resi.cut.lab -relief groove -text "Cuts"
   
      set win .resi.cut.mid
      frame $win
         scrollbar $win.sb -orient vertical \
            -command {.resi.cut.mid.lb yview}
         listbox $win.lb -width 30 -selectmode single -font $fixedfont\
            -yscrollcommand {.resi.cut.mid.sb set}
         bind $win.lb <ButtonRelease> +UpdateCuts
         frame $win.en
            label $win.en.centerlab -text "center"
            entry $win.en.center
            bind $win.en.center <KeyRelease> +ReadCuts
            bind $win.en.center <Return> {set resi_sup $resi_sup}
            label $win.en.spreadlab -text "spread"
            entry $win.en.spread
            bind $win.en.spread <KeyRelease> +ReadCuts
            bind $win.en.spread <Return> {set resi_sup $resi_sup}
            checkbutton $win.en.nocut -text "Apply Cut" -variable resicut
            bind $win.en.nocut <ButtonRelease> +ReadCuts
            checkbutton $win.en.show -text "Show 1-d plot" \
               -variable resi_show_field_plot
            bind $win.en.show <ButtonRelease> +ReadCuts
         pack $win.en.centerlab $win.en.center $win.en.spreadlab \
            $win.en.spread $win.en.nocut $win.en.show -side top -fill both
      pack $win.lb $win.sb $win.en -side left -fill both
   pack .resi.cut.lab .resi.cut.mid -side top -fill both
   pack .resi.cut -side left -fill both
   
   ######## hbook directory ##########
   frame .resi.dir -relief raised -borderwidth 3
   label .resi.dir.lab -text "Zebra directory" -relief groove
   radiobutton .resi.dir.tbtproton -text "tbt/proton" \
      -value 1 -variable hbookdirectory -anchor w
   radiobutton .resi.dir.tbtelectron -text "tbt/electron" \
      -value 2 -variable hbookdirectory -anchor w
   radiobutton .resi.dir.ssltproton -text "sslt/proton" \
      -value 3 -variable hbookdirectory -anchor w
   radiobutton .resi.dir.ssltelectron -text "sslt/electron" \
      -value 4 -variable hbookdirectory -anchor w
   pack .resi.dir.lab .resi.dir.tbtproton .resi.dir.tbtelectron \
      .resi.dir.ssltproton .resi.dir.ssltelectron -side top -fill both
   pack .resi.dir -side left -fill both


}


ResidualInit

#pack .resi


















