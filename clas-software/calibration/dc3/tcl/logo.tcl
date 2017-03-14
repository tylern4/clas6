
global splash_win

set splash_win 0

# make sure main window isn't in the way
wm withdraw .

proc dc3_splash {fname} {                                              
    global splash_win

	 set splash_win .x
    toplevel $splash_win -bd 3 -relief raised
    wm withdraw $splash_win
    set sw [winfo screenwidth .]
    set sh [winfo screenheight .]
    image create photo "splash" -file "$fname"
    wm overrideredirect $splash_win 1
    label $splash_win.l -image splash -bd 1 -relief sunken
    pack $splash_win.l -side top -expand 1 -fill both
    set x [expr ($sw - 200)/2]
    set y [expr ($sh - 250)/2]
    wm geometry $splash_win +$x+$y
    wm deiconify $splash_win
    update idletasks

    tkwait visibility $splash_win
    after 2500 {destroy $splash_win}
}
