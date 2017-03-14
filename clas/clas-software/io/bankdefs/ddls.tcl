#    sets output file default

set output myclasbanks.ddl

#    asks user to use exisiting ddls or check out a new set

proc start {} {
  frame .f1 -bg grey
  pack .f1 -fill both
  button .f1.exist -bg grey -text "Use existing ddls"
  button .f1.scratch -bg grey -text "Start from scratch"
  pack .f1.exist .f1.scratch -side left -padx 2m -pady 2m

  bind .f1.exist <1> {actions}

#   checks out a set of ddl files

  bind .f1.scratch <1> {eval exec cvs checkout  [glob $env(CLAS_PACK)/bankdefs] >& /dev/null
   actions}}

#   creates window with a series of checkbuttons allowing the user to select 
# which ddl files with which options should be used

proc actions {} {
  global numlist ddls wri cre disp del include write display \
         create delete output ddlist
  toplevel .actions -bg grey 

  frame .actions.f1 -bg grey 
  frame .actions.f2 -bg grey
  frame .actions.f3 -bg grey

  pack .actions.f1 .actions.f2 .actions.f3 -padx 2m -pady 2m 

  canvas .actions.f1.canvas -bg grey -width 20c -height 15c -relief raised \
         -yscrollcommand ".actions.f1.scroll set" 

  scrollbar .actions.f1.scroll -bg grey -relief flat \
            -command ".actions.f1.canvas yview" 

  pack .actions.f1.canvas -side left
  pack .actions.f1.scroll -fill y -side right

  set fr [frame .actions.f1.canvas.f1 -bg grey]
  .actions.f1.canvas create window 0 0 -anchor nw -window $fr

  frame .actions.f1.canvas.f1.f1 -bg grey
  frame .actions.f1.canvas.f1.f2 -bg grey
  frame .actions.f1.canvas.f1.f3 -bg grey
  frame .actions.f1.canvas.f1.f4 -bg grey
  frame .actions.f1.canvas.f1.f5 -bg grey

  pack .actions.f1.canvas.f1.f1 .actions.f1.canvas.f1.f2 \
       .actions.f1.canvas.f1.f3 .actions.f1.canvas.f1.f4 \
       .actions.f1.canvas.f1.f5 -padx 1m -side left

  set ddlist [lsort [glob *.ddl]]

  foreach i $ddlist {
    set loc [lsearch $ddlist $i]
    set f [open $i r]
    while {[gets $f line] >= 0} {
      if [regexp ddls.tcl $line] {
	  set ddlist [lreplace $ddlist $loc $loc]}}
    close $f}

  set num [llength $ddlist]
  set n 0
  while {$n < $num} {append numlist "$n "
   incr n}

  foreach i $numlist {
   set ddls($i) [lrange $ddlist $i $i]
   set include($i) 1
   set cre($i) {}
   set wri($i) {}
   set disp($i) {}
   set del($i) {}
   set write($i) 0
   set display($i) 0
   set create($i) 0
   set delete($i) 0

#     checks ddl files for existence of write create display and delete  if 
#   these words exist after the word TABLE, an array value is set so the 
#   corresponding checkbutton will be preselected when created

   set f [open $ddls($i) r]
   while {[gets $f line] >= 0} {
       if [regexp TABLE $line] {
          if [regexp write $line] {
	      set write($i) 1}
          if [regexp display $line] {
	      set display($i) 1}
          if [regexp create $line] {
	      set create($i) 1}
          if [regexp delete $line] {
	      set delete($i) 1}}}

   close $f

    checkbutton .actions.f1.canvas.f1.f1.inc$i -bg grey -text $ddls($i) \
       -variable include($i) -relief flat 
    checkbutton .actions.f1.canvas.f1.f2.cre$i -bg grey -text Create \
       -variable create($i) -relief flat
    checkbutton .actions.f1.canvas.f1.f3.wri$i -bg grey -text Write \
       -variable write($i) -relief flat 
    checkbutton .actions.f1.canvas.f1.f4.disp$i -bg grey -text Display \
       -variable display($i) -relief flat
    checkbutton .actions.f1.canvas.f1.f5.del$i -bg grey -text Delete \
       -variable delete($i) -relief flat

    pack .actions.f1.canvas.f1.f1.inc$i .actions.f1.canvas.f1.f2.cre$i \
         .actions.f1.canvas.f1.f3.wri$i .actions.f1.canvas.f1.f4.disp$i \
         .actions.f1.canvas.f1.f5.del$i -anchor w}     

  label .actions.f2.loutput -bg grey -text "Ouput File"
  entry .actions.f2.output -bg grey -relief sunken -textvariable output

  pack .actions.f2.loutput .actions.f2.output -padx 2m -side left

  button .actions.f3.write -bg grey -width 12 -text "Write to File"
  button .actions.f3.cancel -bg grey -width 12 -text "Cancel"

  pack .actions.f3.write .actions.f3.cancel -side left -padx 4m

  bind .actions.f3.cancel <1> {exit}

#     checks values for all checkbuttons and then calls mod_ddl, a perl script 
#   which rewrites section of ddl files after TABLE and between ! ! and saves 
#   old ddl as .ddl.old   The tcl script then combines all the selected ddls 
#   and saves them as the file slected as the output file

  bind .actions.f3.write <1> {
    if {[file isfile $output] == 1} {
      exec mv $output $output.old}

    foreach i $numlist {
     if {$include($i) == 1} {append selected_nums "$i "}}
    foreach i $selected_nums {
     if {$create($i) == 1} {set cre($i) create}
     if {$write($i) == 1} {set wri($i) write}
     if {$display($i) == 1} {set disp($i) display}
     if {$delete($i) == 1} {set del($i) delete}
    exec mod_ddl $ddls($i) $cre($i) $wri($i) $disp($i) $del($i)}
    exec touch $output
    set f [open $output w]
    puts $f "#This global ddl file was written using ddls.tcl "
    puts $f "#"
    close $f
    foreach i $selected_nums {exec cat $ddls($i) >> $output}
    exit}}
start









