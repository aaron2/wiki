if {$::tcl_platform(platform) == "windows"} {
    package ifneeded sqlite3 3.7.3 [list load [file join $dir tclsqlite3.dll]]
} elseif {$::tcl_platform(os) == "Linux"} {
    package ifneeded sqlite3 3.7.3 [list load [file join $dir tclsqlite-3.7.3.so]]
}
