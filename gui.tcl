package require Tk

set screenWidth 64
set screenHeight 32
set pixelSize 10

frame .main
pack .main -side top -fill both -expand true

canvas .main.display -width [expr {$screenWidth * $pixelSize}] -height [expr {$screenHeight * $pixelSize}] -background black
pack .main.display -side top -pady 20

frame .main.keypad
pack .main.keypad -side top -pady 10

set keys {1 2 3 C 4 5 6 D 7 8 9 E A 0 B F}

set i 0
foreach key $keys {
    button .main.keypad$key -text $key -command "keypress_cb $key" -width 4 -height 2
    grid .main.keypad$key -in .main.keypad -row [expr {$i / 4}] -column [expr {$i % 4}] -padx 2 -pady 2
    incr i
}