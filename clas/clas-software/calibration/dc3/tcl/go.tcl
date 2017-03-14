#!/usr/bin/wish -f

#
# This file is a script for automating the dc calibration process.
# It basically pushes all of the buttons in the sequence that a calibrator
# would push them.
#
# The proc "invokeButton" was written below to help give the user a
# more visual aid as to what is going on.
#
# Useful commands:
#
# 	To raise a notebook page-
#
#      Notebook:raise.page $notebookwin 0  # For T0/Tmax
#      Notebook:raise.page $notebookwin 1  # For Fit
#      Notebook:raise.page $notebookwin 2  # For Calibration Quality
#      Notebook:raise.page $notebookwin 3  # For Database
#
# 	To push a button-
#
#      invokeButton $find_tmax_win.tmax.find.findall	# Find All Tmax values 
#      invokeButton $find_tmax_win.tzero.find.findall	# Find All Tzero values 
#      invokeButton $fit_path.novice.fit.fitall			# Fit all X vs. T parms 
#      invokeButton $quality_path.left.test				# Quality Summary 
#      invokeButton $map_path.controls.map				# Write to Database
#
#
#
# 	Other useful tidbits-
#
#      if {[$summary_win_exists!=0]} {invokeButton .qualsum.b.save}	# "Save as postscript..."
#      if {[$summary_win_exists!=0]} {invokeButton .qualsum.b.print}	# "Print..."
#
#      ModifyDBInfo							# Bring up the dialog to edit DB information
#      tkwait window $changedbinfowin	# Wait for the user to finish editing DB info
#      set seek_first 1						# Turn on "seek first" option
#      set seek_first 0						# Turn off "seek first" option
#
#
#

proc go {} {

	global find_tmax_win fit_path quality_path summary_win_exists changedbinfowin
	global map_path seek_first
	global script_ready

	set notebookwin .middle.n

	# Diable the GO button while we work
	.top.go configure -state disabled

	# Have the screen update more frequently while we do this
	# (gives the illusion that fewer things are going wrong)
	SetReplotGlobal

	####################### Start Doing Stuff ########################
	# Find All Tmax values
	Notebook:raise.page $notebookwin 0 ; update ; update
	invokeButton $find_tmax_win.tmax.find.findall

	# Fit X vs. T function(s)
	Notebook:raise.page $notebookwin 1 ; update ; update
	set seek_first 1
	invokeButton $fit_path.novice.fit.fitall


	####################### Finished Doing Stuff ########################

	# Stop replot loop
	after cancel after idle SetReplotGlobal
	after cancel SetReplotGlobal

	# Enable the GO button
	.top.go configure -state normal
}



#
# Utility to invoke a button. This will highlight the button (as
# with mouseovers), and flash it before invoking it. After the
# invocation is complete, the button is flashed again and set back
# to the normal state.
#
proc invokeButton {b} {

	update ; update
	$b configure -state active
	$b flash
	$b invoke
	$b flash
	$b configure -state normal
	update ; update	
}








