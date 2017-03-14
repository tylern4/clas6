#!/usr/bin/wish -f




############################################################
# This routine is called whenever one of the scrollbars is #
# moved. It adjusts the scroll bar to it's new position    #
# and updates the associated entry.                        #
# top and bot indicate the upper and lower limits of the   #
# scrollbar while path would be something like .xvst.tmax  #
# It is assumed the scrollbar is at $path.sb and the entry #
# at $path.en .                                            #
############################################################
proc scrollvalue {top bot path how args} {
   
   set r [expr $top-$bot]
   set val [lindex [$path.sb get] 0]
   set num [lindex $args 0]
   set del 0
   
   switch $how {
      moveto
         {set del [expr ($num)-($val)]}
      scroll
      {
         switch [lindex $args 1] {
            units
               {set del [expr $num/1000.0]}
            pages
               {set del [expr $num/100.0]}
         }
      }
   }
   set newval [expr $val+$del]
   $path.sb set $newval $newval
   set val [lindex [$path.sb get] 0]
   $path.en delete 0 end
   
   # display an appropriate number of decimal points
   set scale $r
   if {[expr $scale<0]} then {set scale -$r}
   if     {[expr $scale<3.0  ]} then {set form "%.3f"} \
   elseif {[expr $scale<10.0 ]} then {set form "%.2f"} \
   elseif {[expr $scale<100.0]} then {set form "%.1f"} \
   else   {set form "%.0f"}
   
   $path.en insert 0 [format $form [expr (((1.0-$val)*$r)+$bot)]]
}

##########################################################
# This routine is called whenever <Return> is pressed    #
# while editing an entry so that the scroll bar position #
# will be updated. See "scrollvalue" comment for         #
# description of args.                                   #
########################################################## 
proc entryvalue {top bot path} {

   set r [expr 1.0*($top-$bot)]
   
   set enval [$path.en get]
   set val [expr 1.0-(($enval-$bot)/$r)]
   scrollvalue $top $bot $path moveto $val

}


##########################################################
# A little routine to help center toplevel widgets on    #
# the screen                                             #
##########################################################
proc CenterWindow {win} {
	wm withdraw $win
	update
	
	set screenwidth [winfo screenwidth .]
	set winwidth [winfo reqwidth $win]
	set screenheight [winfo screenheight .]
	set winheight [winfo reqheight $win]

	set x [expr ($screenwidth-$winwidth)/2]
	set y [expr ($screenheight-$winheight)/2]

	 wm geometry $win +$x+$y
	 wm deiconify $win
}

################################################################
# Calling this routine will start an endless loop of having    #
# this routine called again and setting the global "replot"    #
# variable so that the screen is continually updated. To kill  #
# the loop, one needs to execute the following:                #
#                                                              #
#	after cancel after idle SetReplotGlobal                     #
#  after cancel SetReplotGlobal                                #
################################################################
proc SetReplotGlobal {} {
	global replot

	set replot 1
	after 5000 after idle SetReplotGlobal
}



