#!/usr/bin/wish -f



proc map_init {path} {

   global map_path
   global mapfile
   global runnumber
   global s1sl1 s1sl2 s1sl3 s1sl4 s1sl5 s1sl6
   global s2sl1 s2sl2 s2sl3 s2sl4 s2sl5 s2sl6
   global s3sl1 s3sl2 s3sl3 s3sl4 s3sl5 s3sl6
   global s4sl1 s4sl2 s4sl3 s4sl4 s4sl5 s4sl6
   global s5sl1 s5sl2 s5sl3 s5sl4 s5sl5 s5sl6
   global s6sl1 s6sl2 s6sl3 s6sl4 s6sl5 s6sl6

   set map_path $path
   
   ######## checkbuttons #######
   set win $map_path.buttons
   frame $win -relief groove -borderwidth 3
   pack $win -side top -fill both
   button $win.all -text "Select All" -command {MapAllOn}
   pack $win.all -side top
   set win $map_path.buttons.bw
   frame $win
   pack $win -side top -fill both
   for {set sec 1} {[expr $sec<=6]} {incr sec} {
      set fname [format "s%d" $sec]
      frame $win.$fname
      for {set sup 1} {[expr $sup<=6]} {incr sup} {
         set name [format  "s%dsl%d" $sec $sup]
         set $name 0
         checkbutton $win.$fname.$name -text $name -variable $name
         pack $win.$fname.$name -side top -fill both
      }
      pack $win.$fname -side left -fill both
   }
   
   ######## Write buttons ######
   set win $map_path.controls
   frame $win
   pack $win -side bottom -fill both -pady 10
   button $win.create -text "Create MapFile" -command {CreateMapFile}
   pack $win.create -side left
   button $win.map  -text "Write to Map" -command {MapWrite}
   pack $win.map -side right

   ######### Mapfile ###########
   set win $map_path.lab
   frame $win
   pack $win -side bottom -fill x -ipady 10
   button $win.lab -text "MapFile:" -command {NewMapFile}
   label $win.mapfile -textvariable mapfile
   pack $win.lab $win.mapfile -side left -fill x

   ######### RunNumber ###########
   set win $map_path.run
   frame $win
   pack $win -side bottom -fill x -ipady 10
   label $win.lab -text "Run Number:"
   label $win.run -textvariable runnumber
   pack $win.lab $win.run -side left -fill x

}

proc NewMapFile {} {

   global mapfile

   set types {
      {{Map files} {.map} }
      {{All files} *      }
   }
   
   set fname [tk_getOpenFile -filetypes $types -title "Select new DC_DOCA.map file"]
   
   if {$fname!=""} {
      set mapfile $fname
   }
}


proc MapAllOn {} {

   global s1sl1 s1sl2 s1sl3 s1sl4 s1sl5 s1sl6
   global s2sl1 s2sl2 s2sl3 s2sl4 s2sl5 s2sl6
   global s3sl1 s3sl2 s3sl3 s3sl4 s3sl5 s3sl6
   global s4sl1 s4sl2 s4sl3 s4sl4 s4sl5 s4sl6
   global s5sl1 s5sl2 s5sl3 s5sl4 s5sl5 s5sl6
   global s6sl1 s6sl2 s6sl3 s6sl4 s6sl5 s6sl6

   for {set sec 1} {[expr $sec<=6]} {incr sec} {
      for {set sup 1} {[expr $sup<=6]} {incr sup} {
         set name [format  "s%dsl%d" $sec $sup]
         set $name 1
      }
   }
}










