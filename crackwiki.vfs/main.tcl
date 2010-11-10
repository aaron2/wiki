  package require starkit
  starkit::startup
  set ::argv0 [file join $::argv0 lib crackwiki httpd.tcl]
  package require crackwiki
