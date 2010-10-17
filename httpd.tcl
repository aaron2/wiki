namespace eval httpd {}

proc httpd::incoming_connection {s addr port} {
    variable incoming_req
    #logger "connection $s $addr $port"
    fconfigure $s -blocking 0 -translation {binary crlf}
    fileevent $s readable [list [namespace current]::read_conn $s $addr]
    dict set incoming_req($s) body {}
    after 5000 [list [namespace current]::timeout $s]
}

proc httpd::timeout {s} {
    variable incoming_req
    unset -nocomplain ::incoming_req($s)
    catch {close $s}
}

proc httpd::read_conn {s addr} {
    variable incoming_req

    #logger "read conn $s $addr"
    if {[eof $s]} {close $s; return}
    after cancel [list [namespace current]::timeout $s]
    after 10000 [list [namespace current]::timeout $s]
 
    dict append incoming_req($s) body [read $s]
 
    if {![dict exists $incoming_req($s) header] && [string first \r\n\r\n [dict get $incoming_req($s) body]] < 0} {
        return
    } elseif {![dict exists $incoming_req($s) header]} {
        if {![regexp {^([A-Z]{3,4})\s+(.*)\s+HTTP/1\.[01]\r\n} [dict get $incoming_req($s) body] -> method uri]} {
           http_error $s 500 "500 Internal Server Error" "Invalid request"
        }
        set uri [string map [list "\\" "\\\\"] $uri]
        regsub -all -- {%([A-Fa-f0-9][A-Fa-f0-9])} $uri {\\u00\1} uri
        set uri [subst -novar -nocommand $uri]
        logger "$s $method $uri"

        dict set incoming_req($s) uri $uri
        dict set incoming_req($s) method $method

        set header [string range [dict get $incoming_req($s) body] 0 [string first \r\n\r\n [dict get $incoming_req($s) body]]]
        dict set incoming_req($s) body [string range [dict get $incoming_req($s) body] [expr {[string length $header] + 1}] end]
        foreach x [lrange [split $header \n] 1 end] {
            set x [split [string trimright $x \r] :]
            dict set incoming_req($s) header [string tolower [lindex $x 0]] [string trim [join [lrange $x 1 end] :]]
        }
    }
    if {[dict exists $incoming_req($s) header content-length] && [string length [dict get $incoming_req($s) body]] < [dict get $incoming_req($s) header content-length]} {
        return
    }

    set tmp $incoming_req($s)
    unset incoming_req($s)
    #logger "request dict: $tmp"

    request $s $addr $tmp
}

proc httpd::set_env {s addr req} {
    global request
    variable config
    unset -nocomplain request
    set request(REMOTE_ADDR) $addr
    if {[dict exists $req header host]} {
        set request(HTTP_HOST) [lindex [split [dict get $req header host] :] 0]
    } else {
        set request(HTTP_HOST) [lindex [fconfigure $s -sockname] 0]
    }
    set request(SERVER_PORT) $config(port)
    set request(DOCUMENT_ROOT) "$config(base_dir)"
    set request(PATH_INFO) [lindex [split [dict get $req uri] ?] 0]
    if {[string match */ $request(PATH_INFO)]} { append request(PATH_INFO) index.html }
    set request(REQUEST_URI) [dict get $req uri]
    set request(PATH_TRANSLATED) "$request(DOCUMENT_ROOT)$request(PATH_INFO)"
    set request(QUERY_STRING) [join [lrange [split [dict get $req uri] ?] 1 end] ?]
    set request(USER) anonymous
    set request(USER_AUTH) 0
    if {[dict exists $req header cookie]} {
        set request(HTTP_COOKIE) [dict get $req header cookie]
    }
    if {[dict get $req method] == "GET"} {
        set request(REQUEST_METHOD) GET
    } elseif {[dict get $req method] == "POST"} {
        set request(REQUEST_METHOD) POST
        set request(CONTENT_TYPE) [dict get $req header content-type]
        set request(BODY) [dict get $req body]
    } else {
        http_error $s 500 "500 Internal Server Error" "Invalid request method"
    }

    #foreach x [lsort [array names request]] {
    #    logger "$x = $request($x)"
    #}
}

proc httpd::request {s addr req} {
    set_env $s $addr $req

    set uri [dict get $req uri]
    if {[string match */favicon.ico $uri] || [string match */files/* $uri] || [string match */include/* $uri]} {
        reply_file $s $addr $req
        return
    }

    if {[catch {service_request} out]} {
        if {$out != "exit"} {
            logger "ERROR: $out"
            if {[dict get $req method] == "POST"} { catch {db eval {rollback transaction}} }
            http_error $s 500 "500 Internal Server Error" "error servicing request: $out<br>$::errorInfo"
        }
    }

    reply $s
}

proc httpd::reply_file {s addr req} {
    set path [split [dict get $req uri] /]
    set i [lsearch $path files]
    set file $::request(DOCUMENT_ROOT)/[join [lrange $path $i end] /]
    if {![file exists $file]} {
        http_error $s 404 "404 Not Found" "File not found"
    }

    _puts $s "HTTP/1.0 200 OK"
    _puts $s "Content-Type: [mimetype $file]"
    _puts $s "Content-Length: [file size $file]\n"

    set fh [open $file r]
    fconfigure $fh -encoding binary -translation lf -eofchar {}
    fconfigure $s -encoding binary -translation lf
    fcopy $fh $s
    close $fh
    close $s
}

proc httpd::reply {s} {
    variable buf

    if {[set i [lsearch $buf {}]] > 0} {
        set reply_header [lrange $buf 0 [expr {$i-1}]]
        set buf [lrange $buf $i end]
    } else {
        set reply_header $buf
        set buf [list {}]
    }

    if {[set i [lsearch $reply_header Status:*]] > -1} {
        set code [join [lrange [split [string trim [lindex $reply_header $i]]] 1 end]]
        set reply_header [lreplace $reply_header $i $i]
        set reply_header [linsert $reply_header 0 "HTTP/1.0 $code"]
    } else {
        if {[lsearch $reply_header Location:*] > -1} {
            set reply_header [linsert $reply_header 0 "HTTP/1.0 302 Moved Temporarily"]
        } else {
            set reply_header [linsert $reply_header 0 "HTTP/1.0 200 OK"]
        }
    }
    #logger "reply_header: $reply_header"
    _puts $s [join [concat $reply_header $buf] \n]
    close $s
    unset buf
}

proc httpd::http_error {s err title body} {
    set body "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<HTML><HEAD>\n<TITLE>$title</TITLE>\n</HEAD><BODY>\n<H1>[lrange [split $title] 1 end]</H1>\n$body<P>\n</BODY></HTML>"
    _puts $s "HTTP/1.0 $err\nDate: [clock format [clock seconds] -gmt 1 -format "%a, %d %b %Y %H:%M:%S"]Content-Type: text/html\nContent-Length: [string length $body]\n\n$body"
    close $s
    return -code return
}

proc httpd::mimetype {file} {
    switch -glob -- [string tolower $file] {
        *.htm*  { return text/html }
        *.txt   { return text/plain }
        *.jpg   { return image/jpeg }
        *.jpeg  { return image/jpeg }
        *.tif*  { return image/tiff }
        *.png   { return image/png }
        *.pdf   { return application/pdf }
        *.doc   { return application/msword }
        *.tar   { return application/x-tar }
        *.gz    { return application/x-tar x-gzip }
        *.tgz   { return application/x-tar x-gzip }
        *.zip   { return application/x-zip x-pkzip }
        default { return application/unknown }
    }
}


proc httpd::listen {port} {
    variable state
    variable config
    if {[info exists state(socket)]} { catch {close $state(socket)} }

    set cmd [list socket -server [list [namespace current]::incoming_connection]]
    if {$config(local_only)} { lappend cmd -myaddr 127.0.0.1 }
    lappend cmd $port
    if {[catch {eval $cmd} socket]} {
        return -code error "cant open socket: $socket"
    }
    set state(socket) $socket
}

proc httpd::setup {cgi} {
    variable config
    variable buf

    fix_env
    set config(base_dir) [pwd]
    namespace eval :: [list source $cgi]
    listen $config(port)

    rename ::exit ::_exit
    rename ::puts ::_puts
    rename ::read ::_read
    namespace export puts exit read
}

proc httpd::exit {args} {
    return -code error exit
}

proc httpd::read {chan args} {
    if {$chan == "stdin"} {
        return $::request(BODY)
    }
    return [eval [list _read $chan] $args]
}

proc httpd::puts {args} {
    #eval logger $args
    if {[lindex $args 0] == "-nonewline"} {
        set args [lrange $args 1 end]
    }
    # write to a fh
    if {[llength $args] > 1} {
        eval _puts $args
        return
    }
    #lappend [namespace current]::buf [string trimright [join $args] \n]
    lappend [namespace current]::buf [lindex $args 0]
}

proc httpd::logger {msg} {
    set ts [clock format [clock seconds] -format {[%T]}]
    _puts "$ts $msg"
}

proc httpd::fix_env {} {
    global env
    set keep [list REMOTE_ADDR HTTP_HOST PATH_INFO \
        REQUEST_URI SERVER_PORT PATH_TRANSLATED \
        DOCUMENT_ROOT QUERY_STRING SCRIPT_FILENAME \
        REQUEST_METHOD CONTENT_TYPE HTTP_COOKIE]
    foreach x [array names env] {
        if {[lsearch -exact $keep $x] < 0} { unset env($x) }
    }
}

proc httpd::parse_cmdline {in} {
    variable config
    foreach {opt arg} $in {
        switch -exact -- $opt {
            -port {
                if {![string is integer -strict $arg]} { usage "port argument must be an integer" }
                set config(port) $arg
            }
            -local_only {
                if {![string is boolean -strict $arg]} { usage "port argument must be boolean" }
                set config(local_only) $arg
            }
            default {
                usage "unknown option $opt"
            }
        }
    }
}

proc httpd::usage {err} {
    ::puts stderr "Error: $err\n"
    ::puts stderr "Usage: $::argv0 \[-opt arg]"
    ::puts stderr "    -port (Port to listen on for http connections) (default: 8080)"
    ::puts stderr "    -local_only (Only allow connections from localhost) (default: true)"
    ::exit 1
}

# this will build the auto load index first, otherwise we cant import [read] later
namespace import httpd::*

# without this our real socket open fails on windows
close [socket -server wtf 8081]

namespace eval httpd {
    variable state
    array set config [list port 8080 local_only 1]
    parse_cmdline $argv
    setup wiki.cgi
}

namespace import httpd::*
if {[catch {package present Tk}]} { vwait forever }
if {[file exists httpd_gui.tcl]} { namespace eval httpd {source httpd_gui.tcl} }
