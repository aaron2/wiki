proc logger {msg} {
    set ts [clock format [clock seconds] -format {[%T]}]
    #_puts "$ts $msg"
    .m.log.text insert end "\n$ts $msg"
}

proc tray { message ico wparam lparam x y } {
    #_puts "taskbar_cmd with $message,ico:$ico,wParam:$wparam,lParam:$lparam,x:$x,y:$y"
    switch -exact -- $message {
        WM_RBUTTONUP {
        }
        WM_LBUTTONDBLCLK {
            wm deiconify .m
            raise .m
            winico taskbar delete $ico
        }
    }
}

proc show_log_win {b t} {
    variable geom
    set geom(small) [winfo width [winfo toplevel $t]]x[winfo height [winfo toplevel $t]]
    pack $t -expand 1 -fill both
    $b configure -text "Hide log" -command [list [namespace current]::hide_log_win $b $t]
    if {[info exists geom(big)]} {
        wm geometry [winfo toplevel $t] $geom(big)
    }
}

proc hide_log_win {b t} {
    variable geom
    pack forget $t
    $b configure -text "Show log" -command [list [namespace current]::show_log_win $b $t]
    if {[info exists geom(small)]} {
        wm geometry [winfo toplevel $t] $geom(small)
        set geom(big) [winfo width [winfo toplevel $t]]x[winfo height [winfo toplevel $t]]
    }
}

proc hide_main_win {w} {
    variable ico
    if {![string equal [winfo toplevel $w] $w] || ![string equal [wm state $w] iconic]} return
    if {[info exists ico]} {
        winico taskbar add $ico -callback "[namespace current]::tray %m %i %w %l %x %y"
        wm withdraw $w
    }
}

proc hide_config_win {w} {
    destroy $w
    unset ::tmpconfig
}

proc apply_config_win {w} {
    variable config
    unset config
    array set config [array get ::tmpconfig]
    listen $config(port)
    hide_config_win $w
    update_gui .m
}

proc show_config_win {p} {
    array set ::tmpconfig [array get ::httpd::config]
    set w [toplevel .config -padx 5 -pady 5]
    wm title $w "Wiki Configuration"
    wm attributes $w -toolwindow 1
    wm protocol $w WM_DELETE_WINDOW [list [namespace current]::hide_config_win $w]
    ttk::labelframe $w.config -text Configuration -padding 5
    frame $w.config.buttons
    frame $w.config.port
    ttk::checkbutton $w.config.local -text "Private Server" -variable ::tmpconfig(local_only)
    ttk::label $w.config.port.l -text Port -padding {10 0 0 0}
    ttk::spinbox $w.config.port.e -width 4 -from 1 -to 65535 -textvariable ::tmpconfig(port)
    ttk::button $w.config.buttons.ok -text OK -command [list [namespace current]::apply_config_win $w]
    ttk::button $w.config.buttons.cancel -text Cancel -command [list [namespace current]::hide_config_win $w]
    pack $w.config -expand 1 -fill both
    pack $w.config.port.l $w.config.port.e -side left
    grid $w.config.buttons.ok $w.config.buttons.cancel -padx 5 -pady {5 0} -sticky e
    grid $w.config.local $w.config.port
    grid $w.config.buttons -columnspan 2 -sticky e -pady {5 0}
    set x [expr {[winfo x $p] + ([winfo width $p] / 2)}]
    set y [expr {[winfo y $p] + ([winfo height $p] / 2)}]
    wm geometry $w +$x+$y
    grab $w
    focus $w
}

proc update_gui {w} {
    variable config
    variable ico
    $w.main.l configure -text "Listening on port $config(port)"
    if {[info exists ico]} {
        winico text $ico "Wiki listening on port $config(port)"
    }
}

proc browser {} {
    variable config
    global tcl_platform
    switch -exact -- $tcl_platform(platform) {
        windows {
            catch {
                set a [registry get "HKEY_CLASSES_ROOT\\.html" ""]
                set b [registry get "HKEY_CLASSES_ROOT\\$a\\shell" ""]
                set c [registry get "HKEY_CLASSES_ROOT\\$a\\shell\\$b\\command" ""]
                set c [string trim [string map {%1 ""} $c] " \""]
                exec $c http://localhost:$config(port)/ &
            }
        }
    }
}

proc done {} {
    variable ico
    #if {[info exists ico]} {
    #    winico taskbar delete $ico
    #}
    _exit
}

proc gui {} {
    variable config

    wm withdraw .
    set w .m
    toplevel $w
    wm title $w "Local Wiki"
    wm protocol $w WM_DELETE_WINDOW [list [namespace current]::done]
    bind $w <Unmap> "[namespace current]::hide_main_win %W"
    frame $w.main -padx 5 -pady 5
    ttk::label $w.main.l
    ttk::button $w.main.config -text Configure -command [list [namespace current]::show_config_win $w]
    ttk::button $w.main.log
    ttk::button $w.main.browser -text "Launch Browser" -command "[namespace current]::browser"
    grid $w.main.l -sticky ew -columnspan 3 -padx 3 -pady 3
    grid $w.main.config $w.main.log $w.main.browser -padx 3 -pady 3
    pack $w.main -fill both

    frame $w.log -relief sunken -bd 2
    text $w.log.text -bd 0 -yscrollcommand [list $w.log.sb set] -state disabled
    ttk::scrollbar $w.log.sb -orient v  -command [list $w.log.text yview]
    pack $w.log.sb -side right -fill y 
    pack $w.log.text -fill both -expand 1

    hide_log_win $w.main.log $w.log
    update_gui $w
}

if {![catch {package require Winico}]} {
    #set ico [winico create [file join [file dirname [info script]] smiley.ico]]
    set ico [winico load TK [info nameofexecutable]]
}

gui



