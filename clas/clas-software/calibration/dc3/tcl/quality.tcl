#!/usr/bin/wish -f

set smallfont  "-*-helvetica-medium-o-*-*-9-*-*-*-*-*-*-*"
set normalfont  "-*-times-medium-r-*-*-14-*-*-*-*-*-*-*"
set boldfont    "-*-times-bold-o-*-*-10-*-*-*-*-*-*-*"
set bigfont     "-*-helvetica-medium-o-*-*-14-*-*-*-*-*-*-*"
set bigboldfont "-*-times-bold-r-*-*-14-*-*-*-*-*-*-*"
set hugefont     "-*-helvetica-medium-o-*-*-20-*-*-*-*-*-*-*"
set hugeboldfont "-*-times-bold-r-*-*-20-*-*-*-*-*-*-*"
set fixedfont    "-*-fixed-bold-*-*-*-12-*-*-*-*-*-*-*"

# spacing dimensions for tables in inches
set table_xspace 0.8
set table_yspace 0.16

# flag for summary window existing
set summary_win_exists 0

global quality_path


# This is run at program start time and creates widgets in
# the notebook page
proc calibration_quality {path} {

   global quality_path
   global fixedfont

   set quality_path $path
   
   set w $path.left
   frame $w -borderwidth 3 -relief groove
   label $w.lab -text "Quality Test"
   button $w.test -text "Summary" -command {QualityCheck}
   pack $w.lab $w.test -side top
   pack $w -side left -fill both
   
   set w $path.right
   frame $w
   pack $w -side right -fill both
   
   # stats
   set w $path.right.stats
   frame $w
   pack $w -side left -fill both
   label $w.tw -text "Total Width    :    microns" -anchor w -font $fixedfont
   label $w.ns -text "Narrow sigma   :    microns" -anchor w -font $fixedfont
   label $w.ws -text "Wide sigma     :    microns" -anchor w -font $fixedfont
   label $w.mn -text "Mean           :    microns" -anchor w -font $fixedfont
   label $w.ar -text "Amplitude Ratio:           " -anchor w -font $fixedfont
   pack $w.tw $w.ns $w.ws $w.mn $w.ar -side top -fill x
	
}

# This is called from the Tcl-callable C routine QualityCheck.
# It creates the summary window (if it doesn't already exist)
# and draws the header and empty tables.
#
proc QualitySummary {} {

   global normalfont boldfont bigboldfont bigfont smallfont hugeboldfont hugefont
   global filename
   global summary_win_exists

   set w .qualsum
   if {[expr $summary_win_exists==0]} {
      toplevel $w
      frame $w.b
      pack $w.b -side bottom -fill x
   
      ################# Buttons ################
      button $w.b.dismiss -text "Dismiss" \
         -command {set summary_win_exists 0 ; destroy .qualsum}
      button $w.b.print -text "Print..." -command {PrintCanvasPS .qualsum.c.c}
      button $w.b.save -text "Save as postscipt..." -command {SaveCanvasPS .qualsum.c.c}
      pack $w.b.print $w.b.save -side left
      pack $w.b.dismiss -side right
   
      ################ Canvas ###############
      frame $w.c -borderwidth 3 -relief sunken
      pack $w.c -side top -fill both
      canvas $w.c.c -width 8.0i -height 10.0i -background "#FFFFFF"
      pack $w.c.c
      
      set summary_win_exists 1
   }
   
   set can $w.c.c
   $can delete ELEMENTS TABLES
   
   #----------- Header info ------------#
   set title [format "Summary for %s" $filename]
   $can create text 4.0i 0.5i -text $title -font $hugeboldfont -tags TABLES

   set time [exec "date"]
   $can create text 0.2i 0.2i -text $time -font $smallfont -anchor w -tags TABLES

   set user [exec "whoami"]
   $can create text 7.8i 0.2i -text $user -font $smallfont -anchor e -tags TABLES
   
   #---------- Table borders and labels --------#
   DrawTableTop  $can 1.0 1.2 "Time residual sigmas,narrow (microns)" "SL" "avg."
   DrawTableSide $can 1.0 1.2 "Sec" "avg."
   DrawTableTop  $can 1.0 3.0 "Time residual sigmas,wide (microns)" "SL" "avg."
   DrawTableSide $can 1.0 3.0 "Sec" "avg."
   DrawTableTop  $can 1.0 4.8 "Time residual means (microns)" "SL" "avg."
   DrawTableSide $can 1.0 4.8 "Sec" "avg."
   DrawTableTop  $can 1.0 6.6 "Total width (microns)" "SL" "*avg."
   DrawTableSide $can 1.0 6.6 "Sec" "*avg."
   DrawTableTop  $can 1.0 8.3 "Hits per TBT" "sec" "total"
   DrawTableTop  $can 1.0 9.0 "Avg. Chisq. per DOF" "sec" "avg."

}

proc DrawTableTop {can xbase ybase title pre last} {
   
   global normalfont boldfont bigboldfont bigfont smallfont hugeboldfont hugefont
   global table_xspace table_yspace
   
   set y [format "%fi" [expr $ybase-0.25]]
   set x [format "%fi" [expr $xbase]]
   $can create text $x $y -text $title -font $bigboldfont -anchor w -tags TABLES
   set y [format "%fi" $ybase]
   for {set sl 1} {[expr $sl<=6]} {incr sl} {
      set x [format "%fi" [expr $xbase+($sl*$table_xspace)]]
      set lab [format "%s%d" $pre $sl]
      $can create text $x $y -text $lab -font $boldfont -anchor w -tags TABLES
   }
   set x [format "%fi" [expr $xbase+($sl*$table_xspace)]]
   $can create text $x $y -text $last -font $boldfont -anchor w -tags TABLES

}

proc DrawTableSide {can xbase ybase pre last} {
   
   global normalfont boldfont bigboldfont bigfont smallfont hugeboldfont hugefont
   global table_xspace table_yspace
   
   set x [format "%fi" $xbase]
   for {set sl 1} {[expr $sl<=6]} {incr sl} {
      set y [format "%fi" [expr $ybase+($sl*$table_yspace)]]
      set lab [format "%s%d" $pre $sl]
      $can create text $x $y -text $lab -font $boldfont -anchor w -tags TABLES
   }
   set y [format "%fi" [expr $ybase+($sl*$table_yspace)]]
   $can create text $x $y -text $last -font $boldfont -anchor w -tags TABLES
   
   # horizontal line
   set y [format "%fi" [expr $ybase+(6.0*$table_yspace)+0.08]]
   set x2 [format "%fi" [expr $xbase+(7.5*$table_xspace)]]
   $can create line $x $y $x2 $y -tags TABLES

   # vertical line
   set x [format "%fi" [expr $xbase+(6.7*$table_xspace)+0.08]]
   set y [format "%fi" [expr $ybase]]
   set y2 [format "%fi" [expr $ybase+(7.5*$table_yspace)]]
   $can create line $x $y $x $y2 -tags TABLES

}

proc DrawTableElement {can xbase ybase i j val color} {

   global normalfont boldfont bigboldfont bigfont smallfont hugeboldfont hugefont
   global table_xspace table_yspace

   set x [format "%fi" [expr $xbase+($i*$table_xspace)]]
   set y [format "%fi" [expr $ybase+($j*$table_yspace)]]
   $can create text $x $y -text $val -font $normalfont -anchor w \
      -fill $color -tags ELEMENTS
}

proc SaveCanvasPS {path} {
   global filename

   set fname [tk_getSaveFile -initialfile [format "%s.eps" $filename]]
   if {[expr [string length $fname]<1]} {return}

   $path postscript -file $fname
   puts [format "Wrote %s" $fname]
}

proc PrintCanvasPS {path} {
   global filename

   set fname [format "dc3summary.tmp.%s.eps" $filename]

   $path postscript -file $fname
   puts [format "executing: lpr %s" $fname]
   exec lpr $fname
   exec rm -f $fname
}






