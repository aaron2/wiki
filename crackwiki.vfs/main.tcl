package require starkit
starkit::startup
if {[string match */main.tcl $::argv0]} {
    set ::argv0 [file join {*}[lrange [file split $::argv0] 0 end-1]] 
}
set ::argv0 [file join $::argv0 lib crackwiki httpd.tcl]
package require crackwiki
