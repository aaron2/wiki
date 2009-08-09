#!/usr/local/bin/tclsh8.5

#set TimeProfilerMode 1
if {[info exists TimeProfilerMode]} {
    source profile2.tcl
    global ProfilerArray
    array unset ProfilerArray
}

# show the user list page
# input: none
# returns: nothing
proc userlist {} {
    set userlevel [http_auth user view]
    http_header
    html_head "User list"
    puts "<h1>User List</h1><br>"
    get_input a
    set order "lower(user)"
    if {[info exists a(sort)] && $a(sort) == "created"} { set order "created desc" }
    if {[info exists a(sort)] && $a(sort) == "level"} { set order "level desc" }

    puts "<form action=\"[myself]/users\" method=post><table>
         <tr><th>Del</th>
         <th><a href=\"?sort=user\" style=\"text-decoration: none;\">User</a></th>
         <th>Host</th>
         <th>Password</th>
         <th>Name</th><th>Email</th>
         <th><a href=\"?sort=created\" style=\"text-decoration: none;\">Created</a></th>
         <th><a href=\"?sort=level\" style=\"text-decoration: none;\">Level</a></th></tr>"
    db eval "select * from users order by $order" {
        puts "<tr><td align=center><input type=checkbox name=\"delete:$user\"></td>
             <td>$user</td><td>$ip</td>
             <td><input type=password name=\"pass:$user\" size=14></td>
             <td><input name=\"name:$user\" value=\"$name\"></td>
             <td><input name=\"email:$user\" value=\"\"></td>
             <td>[format_time $created]</td><td>"
        # show the level dropdown as disabled if the viewer isnt an admin but the displayed user is
        puts "<select name=\"level:$user\" [expr {($userlevel < 30 && $level >= 30) ? " disabled" : ""}]>"
        foreach x {0 10 20 25 30} y {Blocked Base Edit Privileged Admin} {
            # if user isnt admin and viewer isnt admin then dont show admin level in dropdown
            if {$level < 30 && $x >= 30 && $userlevel < 30} { continue }
            puts -nonewline "<option val=$x [expr {$x == $level ? " selected" : ""}]>$y"
        }
        puts "</select></td></tr>"
    }
    set default 10
    # change the 1 to show multiple new user rows
    for {set i 0} {$i < 1} {incr i} {
        puts "<tr id=adduser$i><td></td>
             <td><input name=user$i size=10></td>
             <td><input name=host$i size=14></td>
             <td><input type=password name=pass$i size=14></td>
             <td><input name=name$i></td>
             <td><input name=email$i></td>
             <td></td><td><select name=level$i>"
        foreach x {0 10 20 25 30} y {Blocked Base Edit Privileged Admin} {
            puts -nonewline "<option val=$x [expr {$x == $default ? " selected" : ""}]>$y"
        }
        puts "</select></td></tr></span>"
    }
    puts "</table><br><input type=submit value=Save></form>"
}

# handles the post of the user list page
# input: form postdata
# returns: nothing
proc saveusers {} {
    get_post input
    for {set i 0} {$i < 10} {incr i} {
        if {![info exists input(level$i)] || ![info exists input(user$i)] || ![info exists input(pass$i)] || ![info exists input(ip$i)] || ![info exists input(name$i)]} { continue }
        if {$input(level$i) >= 10 && ($input(user$i) == "" || $input(pass$i) == "")} { continue }
        if {$input(level$i) < 5 && $input(ip$i) == ""} { continue }
        foreach x {user pass level ip name} { set $x $input($x$i) }

        #db eval {insert into users (user,ip,name,level,created) values($user,$ip,$name,$level,datetime('now'))}
        if {$level >= 10} {
            #modify_passwd add $user $pass
        }
    }
    foreach x [array names input pass%3A*] {
        set user [string range $x 7 end]
        if {![db exists {select user from users where user=$user}]} { continue }
        #if {$input($x) != ""} { modify_passwd modify $user $input($x) }
        if {[info exists input(name%3A$user)]} {
            set name $input(name%3A$user)
            db eval {update users set name=$name where user=$user}
        }
    }
    foreach x [array names input delete%3A*] {
        if {$input($x) != "on"} { continue }
        set user [string range $x 9 end]
        db eval {delete from users where user=$user}
        db eval {delete from cookies where user=$user}
    }
    foreach {entity action target} $auth {
        http_auth $entity $action $target
    }
    foreach cmd $commands {
        eval $cmd
    }
    db eval {commit transaction}
    location wiki:users
}

# show the node editing page
# input: existing node id or "new", optionally a title and initial content if the id is "new"
# returns: nothing
proc editpage {id args} {
    if {$id == "new"} {
        set level [http_auth node create]
        http_header
        html_head "Creating new page"
        puts "<h1>Creating new page</h1><br>"
        set action [myself]/new
        set name [lindex $args 0]
        set content [lindex $args 1]
        set back "history.go(-1)"
        set protect $::settings(PROTECT)
    } else {
        db eval {select name,content,protect from nodes where id=$id} {}
        if {![info exists name]} { http_error 404 "no such node" }
        set level [http_auth node edit $id]
        http_header
        html_head "Editing $name"
        set action [myself]/edit:$id
        set content [string map {& &#38;} $content]
        regsub "^\n" $content "\\&nbsp;\n" content
        set back "document.location='[myself]/node:$id'"
    }

    # this js handles resizing the textarea to fit the browser vertically
    puts {<script language="javascript">
          function maxSize() {
              var content = document.getElementsByName('content');
              var lineHeight = content[0].clientHeight / content[0].rows;
              var diff = document.body.scrollHeight - document.body.clientHeight;
              if (diff != 0) { content[0].rows = content[0].rows - (diff / lineHeight); }
              else {
                  content[0].rows = (document.body.clientHeight / lineHeight);
                  var diff = document.body.scrollHeight - document.body.clientHeight;
                  content[0].rows = content[0].rows - (diff / lineHeight);
              }
          }
          window.onresize = maxSize;
          window.onload = maxSize;
          </script>}
    puts "<form name=form action=\"$action\" method=post>
          <input name=name value=\"$name\" style=\"width: 100%;\"><br><br>
          <textarea name=content rows=30 style=\"width: 100%; margin-bottom: .7em;\">$content</textarea><br>
          <input type=submit value=Save style=\"padding-left: 1em; padding-right: 1em;\">
          <input type=button name=cancel value=Cancel onclick=\"javascript:$back;\" style=\"margin-left: 2em;\">
          <input type=hidden name=editstarted value=[clock seconds]>
          <a style=\"position: absolute; right: 50%;\" href=[myself]/tag:wiki:help>Help</a>
          <select name=protect style=\"position: absolute; right: 1em;\">"
    foreach val {5 10 15 20 25} name [list "Read/Write" "Anon Read-only" "User Read-Only" Private Privileged] {
        # if user is logged in, only show them allowed permissions. if user is anon they will be prompted
        # to log in if selected permissions are above anon priviledge
        if {[info exists ::request(REMOTE_USER)] && $val > $level} { continue }
        puts -nonewline "<option value=$val [expr {$val == $protect ? " selected" : ""}]>$name"
    }
    puts "</select></form></body></html>"
}

# show the wiki configuration page
# input: none
# returns: nothing
proc editconfig {} {
    http_auth wiki config
    http_header
    html_head "Wiki Configuration"
    puts "<h1>Wiki Configuration</h1><br>"
    get_input input
    db eval {select name,val from settings} { set tmpset($name) $val }

    if {[info exists input(status)]} { puts "<center>[filter_html $input(status)]</center><br><br>" }
    puts "<form name=form action=\"[myself]/config\" method=post><table style=\"border: 0px;\"><tr><td style=\"\"><table style=\"border: 0px;\"> 
          <tr><td style=\"border: 0px;\">Title prefix</td>
          <td style=\"border: 0px;\"><input type=text name=NAME size=27 value=\"$tmpset(NAME)\"></td>
          <td style=\"border: 0px;\"></td>
          <tr><td style=\"border: 0px;\">Time zone</td><td style=\"border: 0px;\">[tzselect $tmpset(TZ)]</td> 
          <td rowspan=2 style=\"border: 0px;\">Example:<br>[format_time [clock format [clock seconds] -gmt 1]]<br>
          <a href=http://www.tcl.tk/man/tcl8.5/TclCmd/clock.htm#M26>format help</a></td></tr> 
          <tr><td style=\"border: 0px;\">Time format</td>
          <td style=\"border: 0px;\"><input type=text name=TF size=27 value=\"$tmpset(TF)\"></td></tr>
          <tr><td style=\"border: 0px;\">Filter HTML</td>
          <td style=\"border: 0px;\"><input type=checkbox name=FILTER_HTML [expr {$tmpset(FILTER_HTML) ? "checked" : ""}]></td></tr>
          <tr><td valign=top style=\"border: 0px;\">HTML whitelist</td>
          <td style=\"border: 0px;\"><textarea cols=25 rows=3 name=HTML_WHITELIST>$tmpset(HTML_WHITELIST)</textarea></td></tr>
          <tr><td style=\"border: 0px;\">Allow anon create</td>
          <td style=\"border: 0px;\"><input type=checkbox name=ANON_CREATE [expr {$tmpset(ANON_CREATE) ? "checked" : ""}]></td></tr>
          <tr><td style=\"border: 0px;\">Default permissions</td><td style=\"border: 0px;\"><select name=PROTECT>"
    foreach val {5 10 15 20 25} name [list "Read/Write" "Anon Read-only" "User Read-Only" Private Privileged] {
        puts -nonewline "<option value=$val [expr {$val == $tmpset(PROTECT) ? " selected" : ""}]>$name"
    }
    puts "</select></td></tr></table><br><center><input type=submit value=Save style=\"padding-left: 1em; padding-right: 1em;\">
          <input type=button name=cancel value=Cancel  style=\"margin-left: 2em;\" onclick=\"javascript:history.go(-1);\"></td>
          <td valign=top style=\"border: 0px; padding-left: 3em;\"></td></tr></table> 
          </form></body></html>"
}

# handles the post of the wiki config page
# input: form postdata
# returns: nothing
proc saveconfig {} {
    http_auth wiki config
    get_post input
    # checkboxes dont submit any data when not checked, so keep a list of them
    set checkboxes {FILTER_HTML ANON_CREATE}
    # dont trust the form just look for known valid options. this list must be kept in sync with editconfig and the db
    foreach x {NAME TZ TF FILTER_HTML HTML_WHITELIST ANON_CREATE} {
        if {[info exists input($x)]} {
            if {$x == "TZ" && [catch {clock format [clock seconds] -timezone $input($x)} err]} {
                lappend status "Invalid timezone"
                continue
            }
            # the whitelist must be sorted since we use lsearch -sorted on it in [filter_white_html]
            if {$x == "HTML_WHITELIST"} { set input($x) [lsort [string tolower $input($x)]] }
            # look for checked checkboxes and make them true instead of "on"
            if {[lsearch -exact $checkboxes $x] > -1 && $input($x) == "on"} { set input($x) 1 }
            set val $input($x)
            db eval {update settings set val=$val where name=$x}
        }
    }
    # turn off any unchecked checkboxes
    foreach x $checkboxes {
        if {![info exists input($x)]} { db eval {update settings set val=0 where name=$x} }
    }
    if {![info exists status]} { set status [list "Configuration updated"] }
    db eval {commit transaction}
    location wiki:config?status=[join $status ,]
}

# generate an html <select> list containing all the valid timezones
# input: the name of the tz which should be selected in the list
# returns: html
proc tzselect {sel} {
    set ignore {US Antarctica Eire PRC Libya Kwajalein ROC ROK GB GB-Eire NZ NZ-CHAT CET CST6CDT Etc EET EST EST5EDT GMT GMT+0 GMT-0 GMT0 Greenwich HST MET MST MST7MDT PST8PDT SystemV UCT UTC Universal W-SU WET Zulu}
    set out [list "<select name=TZ>"]
    foreach x [lsort [glob -nocomplain -tails -dir /usr/local/lib/tcl8.5/tzdata/ *]] {
        if {[lsearch -exact $ignore $x] > -1} continue
        if {[file isdirectory /usr/local/lib/tcl8.5/tzdata/$x]} {
            foreach y [lsort [glob -nocomplain -tails -dir /usr/local/lib/tcl8.5/tzdata/$x *]] {
                set opt "<option value=\"$x/$y\""
                if {"$x/$y" == $sel} { append opt " selected" }
                append opt ">$x/$y"
                lappend out $opt
            }
        } else {
            set opt "<option value=\"$x\""
            if {$x == $sel} { append opt " selected" }
            append opt ">$x"
            lappend out $opt
        }
    }
    if {[llength $out] == 1} { puts "<option value=UTC>UTC" }
    lappend out "</select>"
    return [join $out \n]
}

# show the user settings page
# input: optional query string "status" to display result of save
# returns: nothing
proc editprefs {} {
    http_auth auth verify
    http_header
    html_head "Personal Settings"
    get_input input
    if {[info exists input(status)]} { puts "<center>[filter_html $input(status)]</center><br><br>" }
    puts "<form name=form action=\"[myself]/prefs\" method=post><table style=\"border: 0px;\"><tr><td style=\"\">
          <table style=\"border: 0px;\">
          <tr><td style=\"border: 0px;\">Time zone</td><td style=\"border: 0px;\">[tzselect $::settings(TZ)]</td>
          <td rowspan=2 style=\"border: 0px;\">Example:<br>[format_time [clock format [clock seconds] -gmt 1]]<br>
          <a href=http://www.tcl.tk/man/tcl8.5/TclCmd/clock.htm#M26>format help</a></td></tr>
          <tr><td style=\"border: 0px;\">Time format</td>
          <td style=\"border: 0px;\"><input type=text name=TF size=27 value=\"$::settings(TF)\"></td></tr>"
    if {[info exists ::request(REMOTE_USER)]} {
        puts "<tr><td style=\"border: 0px;\">Password</td><td style=\"border: 0px;\"><input type=password name=password size=27><input type=hidden name=user value=\"$::request(REMOTE_USER)\"></td></tr>"
    }
    puts "</table><br><center><input type=submit value=Save style=\"padding-left: 1em; padding-right: 1em;\">
          <input type=button name=cancel value=Cancel style=\"margin-left: 2em;\" onclick=\"javascript:history.go(-1);\"></td>
          <td valign=top style=\"border: 0px; padding-left: 3em;\">Clear the field to return to the default value</td></tr></table>
          </form></body></html>"
}

# implements password requirements
# input: username and potential password
# returns: boolean, true if password should be allowed
proc validpass {user pass} {
    if {$pass == $user} { return 0 }
    if {[string match "pass*" $pass]} { return 0 }
    if {[string length $pass] < 5} { return 0 }
    return 1
}

# implements the password hashing function password() in the db
# input: cleartext password
# output: hashed password
proc hash_pass {pass} {
    #package require md5 2
    source /home/rox/tcllib/modules/md5/md5x.tcl
    return [md5::md5 -hex $pass]
}

# handles the post of the user preferences page
# input: form postdata
# returns: nothing
proc saveprefs {} {
    http_auth auth verify
    get_post input
    db function password {hash_pass}
    if {[info exists input(password)] && $input(password) != "" && [info exists input(user)] && $input(user) != ""} {
        http_auth user password $input(user)
        set user $input(user)
        set pass $input(password)
        if {![validpass $user $pass]} {
           lappend status "Bad password"
        } else {
            db eval {update users set password=password($pass) where user=$user}
            db eval {commit transaction}
        }
    }
    # set cookies for client side settings
    foreach x {TZ TF} {
        if {[info exists input($x)]} {
            set input($x) [string trim $input($x)]
            if {$x == "TZ" && [catch {clock format [clock seconds] -timezone $input($x)} err]} {
                lappend status "Invalid timezone"
                continue
            }
            # escape the % since the cookie will be decoded on reciept, and spaces arent allowed in values
            set input($x) [string map {% %25 " " %20} $input($x)]
            set expires "now + 5 years"
            # if the input is cleared then expire the cookie so the defaults will take effect
            if {$input($x) == ""} { set expires "5 days ago" }
            set_cookie $x $input($x) $expires
        }
    }
    if {![info exists status]} { set status [list "Preferences saved"] }
    location wiki:prefs?status=[join $status ,]
}

# show the difference between 2 revisions
# input: <from id>:<to id>, the : is literal. the ids are the primary key in the history table, not a node id
# <from> may be "curr" for the current node contents, which is found by looking up the original node for the <to> id
# returns: nothing
proc showdiff {diff} {
    set from [lindex [split $diff :] 0]
    set to [lindex [split $diff :] 1]
    db eval {select nodes.id as node,nodes.name as name from nodes,history where nodes.id=history.original and history.id=$to} {}
    if {![info exists node]} { http_error 404 "no such node" }
    http_auth node view $node
    http_header
    html_head "Diffing $name"
    set revs [db eval {select id from history where original=$node}]
    if {$from == "curr"} {
        db eval {select content from nodes where id=$node} {}
        puts "[link node:$node "Revision [expr {[llength $revs] + 1}]"] (current)<br>"
    } else {
        db eval {select content,tf(created) as created,created_by from history where id=$from} {}
        set content [db onecolumn {select content from history where id=$from}]
        puts "[link viewhistory:$from "Revision [expr {[lsearch $revs $from] + 1}]"] saved by $created_by on $created<br>"
    }
    db eval {select content as content2,tf(created) as created,created_by from history where id=$to} {}
    puts "[link viewhistory:$to "Revision [expr {[lsearch $revs $to] + 1}]"] saved by $created_by on $created<br>\n<hr>"
    set content [filter_html $content]
    set content2 [filter_html $content2]
    diff [split $content \n] [split $content2 \n]
    puts "<br><hr>"
}

# used only by [longestCommonSubsequence]
proc K { x y } { set x }

# courtesy kbk from the tclers wiki: http://wiki.tcl.tk/3108
proc longestCommonSubsequence { sequence1 sequence2 } {
     set index 0
     foreach string $sequence2 {
         lappend eqv($string) $index
         incr index
     }
     lappend K [list -1 -1 {}]
     lappend K [list [llength $sequence1] [llength $sequence2] {}]
     set k 0
     set i 0
     foreach string $sequence1 {
         if { [info exists eqv($string)] } {
	     set c [lindex $K 0]
	     set r 0
             foreach j $eqv($string) {
                 set max $k
                 set min $r
                 set s [expr { $k + 1 }]
                 while { $max >= $min } {
                     set mid [expr { ( $max + $min ) / 2 }]
                     set bmid [lindex [lindex $K $mid] 1]
                     if { $j == $bmid } {
                         break
                     } elseif { $j < $bmid } {
                         set max [expr {$mid - 1}]
                     } else {
                         set s $mid
                         set min [expr { $mid + 1 }]
                     }
                 }
                 if { $j == [lindex [lindex $K $mid] 1] || $s > $k} { continue }
                 set newc [list $i $j [lindex $K $s]]
                 lset K $r $c
                 set c $newc
                 set r [expr $s+1]
                 if { $s >= $k } {
                     lappend K [lindex $K end]
                     incr k
                     break
                 }
             }
             lset K $r $c
         }
         incr i
     }
     set q [lindex $K $k]
     set seta [list]
     set setb [list]
     for { set i 0 } { $i < $k } {incr i } {
         lappend seta {}
         lappend setb {}
     }
     while { [lindex $q 0] >= 0 } {
         incr k -1
         lset seta $k [lindex $q 0]
         lset setb $k [lindex $q 1]
         set q [lindex $q 2]
     }
     return [list $seta $setb]
}

# used only by [longestCommonSubsequence]
# outputs the changed lines with the styles applied
proc diff {lines1 lines2} {
    set added "<span style=\"background: #88ee88;\">"
    set removed "<span style=\"background: #ee8888;\">"
    set unchanged "<span>"
    set i 0
    set j 0
    foreach { x1 x2 } [longestCommonSubsequence $lines1 $lines2] {
        foreach p $x1 q $x2 {
            while { $i < $p } {
                puts "$added[lindex $lines1 $i]</span><br>"
                incr i
            }
            while { $j < $q } {
                puts "$removed[lindex $lines2 $j]</span><br>"
                incr j
            }
            puts "$unchanged[lindex $lines1 $i]</span><br>"
            incr i
            incr j
        }
    }
    while { $i < [llength $lines1] } {
        puts "$added[lindex $lines1 $i]</span><br>"
        incr i
    }
    while { $j < [llength $lines2] } {
        puts "$removed[lindex $lines2 $j]</span><br>"
        incr j
    }
}

# authenticate and authorize a request
# input: AUTH cookie and action to authorize
# returns: an html request for authentication, or the authenticated users level
proc http_auth {entity action {target {}}} {
    if {[info exists ::cookies(AUTH)]} {
        set key $::cookies(AUTH)
        set ip $::request(REMOTE_ADDR)
        db eval {select cookies.user,users.name,users.level from users,cookies where cookies.key=$key and cookies.expires>datetime('now') and cookies.ip=$ip and cookies.user=users.user} {}
    }
    # if no valid cookie found, then set user as anonymous
    if {![info exists user]} {
        # expire an invalid auth cookie if it exists
        if {[info exists ::cookies(AUTH)]} { set_cookie AUTH "" "5 days ago" }
        set user anonymous
        set level 5
        set name {}
    } else {
        # this variables existance indicates an authenticated non anonymous user
        set ::request(REMOTE_USER) $user
    }
    # check the db for ip address matches
    db eval {select user as huser,ip,level as hlev from users where ip != '' and (user=$user or user='' or user='*')} {
        if {[match_ip $ip $::request(REMOTE_ADDR)]} {
           # if the username is an exact match then set the new ip dictated level
           # otherwise we will only lower the level
           if {$huser == $user} {
               set level $hlev; break
           } elseif {$hlev < $level} {
               set level $hlev; break
           }
        }
    }
    #http_header
    #puts "$entity $action $target you: $level"
    #parray ::env
    if {$level < 5} {
        http_error 430 Forbidden
        close_databases
        exit
    }
    set reqlevel 30
    switch -glob -- $entity:$action {
        wiki:config { set reqlevel 30 }
        user:password {
            if {$user == "anonymous" || $user != $target} { no_auth }
            set reqlevel 10
        }
        user:* {
            set reqlevel 25
            # if target user is admin level then requestor must be admin also
            if {$target != ""} {
                set tlev [db onecolumn {select level from users where user=$target}]
                if {$tlev != "" && $tlev >= 30} { set reqlevel 30 }
            }
        }
        *:delete { set reqlevel 25 }
        node:edit {
            set reqlevel [db onecolumn {select protect from nodes where id=$target}]
        }
        node:view {
            set reqlevel [db onecolumn {select protect from nodes where id=$target}]
            # nodes protect level is for editing, for viewing lower it by 5 unless its privileged
            if {$reqlevel < 20 && $reqlevel >= 10} { incr reqlevel -5 }
        }
        *:create {
            set reqlevel 10
            if {[db onecolumn {select val from settings where name='ANON_CREATE'}] > 0} {
                 set reqlevel 5
            }
        }
        auth:verify {
            if {$level >= 5 && $level < 20} { incr level 5 }
            set reqlevel 5
        }
    }
    if {$reqlevel > $level} { no_auth }
    return $level
}

# perform exact, glob, or cidr style comparison of ip addresses
# input: pattern and ip
# returns: boolean, true if pattern matches
proc match_ip {pattern ip} {
    if {[string match {*\**} $pattern]} {
        return [string match $pattern $ip]
    } elseif {[string match {*/*} $pattern]} {
        set pattern [split $pattern /]
        set mask [lindex $pattern 1]
        if {![string is integer -strict $mask]} { return 1 }
        binary scan [binary format c4 [split [lindex $pattern 0] .]] Iu1 net
        binary scan [binary format c4 [split $ip .]] Iu1 ip
        return [expr {($net >> (32 - $mask)) == ($ip >> (32 - $mask))}]
    } else {
        return [expr {"$pattern" == "$ip"}]
    }
}

proc no_auth {} {
    set loc [file tail $::request(PATH_INFO)]
    location wiki:login[expr {$loc == "index.html" ? "" : "?href=$loc"}]
    close_databases
    exit
}

proc http_error {code text} {
    puts "Status: $code"
    puts "Content-type: text/html\n"
    puts "<html><head><title>Error $code</title></head><body>"
    puts $text
    close_databases
    exit
}

# handles the post of the login form
# input: form postdata
# returns: nothing
proc do_login {} {
    http_auth auth verify
    get_post input
    db function password {hash_pass}
    if {![info exists input(username)] || ![info exists input(password)]} {
        location wiki:login
        return
    }
    set user $input(username)
    set pass $input(password)
    set ip $::env(REMOTE_ADDR)
    if {![db exists {select user from users where user=$user and password=password($pass) and password<>''}]} {
        location wiki:login?message=login%20failed[expr {[info exists input(href)] ? "&href=$input(href)" : ""}]
        return
    }

    # create random 32 char auth token
    set chars abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
    set size [string length $chars]
    for {set i 0} {$i < 32} {incr i} {
        append key [string index $chars [expr {int(rand() * $size)}]]
    }

    set_cookie AUTH $key "7 days"
    set expires [clock format [clock scan "7 days"] -format "%Y-%m-%d %H:%M:%S" -gmt 1]
    db eval {delete from cookies where user=$user and (expires<datetime('now') or ip=$ip)}
    db eval {insert into cookies (user,ip,key,created,expires) values($user,$ip,$key,datetime('now'),$expires)}
    db eval {commit transaction}

    location [expr {[info exists input(href)] ? "$input(href)" : ""}]
}

# sets an http cookie
# input: key value pair, and expiration date in any format tcl recognizes
proc set_cookie {key val expires} {
    puts "Set-Cookie: $key=$val; PATH=[file dirname $::request(PATH_INFO)]; EXPIRES=[clock format [clock scan $expires] -format "%a, %d-%b-%Y %H:%M:%S" -gmt 1];"
}

# logs a user out by deleting any auth tokens from the db and expiring the auth cookie
proc logout {} {
    http_auth auth verify
    if {![info exists ::request(REMOTE_USER)]} {
        location wiki:login
        return
    }
    set ip $::request(REMOTE_ADDR)
    set user $::request(REMOTE_USER)
    db eval {delete from cookies where user=$user and ip=$ip}
    set_cookie AUTH "" "5 days ago"
    location wiki:login
}

# show the login request page
# input: optional query string "message"
# returns: nothing
proc login {} {
    http_auth auth verify
    http_header
    html_head "$::settings(NAME) Login"
    get_input input

    if {[info exists input(message)]} {
        puts "<span style=\"color:red;\">[filter_html $input(message)]</span><br><br>"
    }
    if {[info exists ::request(REMOTE_USER)]} {
        puts "<span style=\"color:red;\">You are already logged in as user &quot;$::request(REMOTE_USER)&quot;</span><br><br>"
    }
    puts "<form action=\"[myself]/login\" method=post>
         <table style=\"border: 0px;\"><tr><td style=\"border: 0px;\">Username:</td>
         <td style=\"border: 0px;\"><input name=username></td></tr>
         <tr><td style=\"border: 0px;\">Password:</td>
         <td style=\"border: 0px;\"><input type=password name=password></td></tr>"
    if {[info exists input(href)]} { puts "<input type=hidden name=href value=\"$input(href)\">" }
    puts "<tr><td colspan=2 align=center style=\"border: 0px;\"><input type=submit value=\"Log In\"></td></tr></table></form>"
}

# display a node
# input: a node id
# returns: nothing
proc showpage {id} {
    # select some extra columns so that [dynamic_variable] can upvar them during dynamic parsing
    db eval {select name,parsed,modified,modified_by from nodes where id=$id} {}
    if {![info exists name]} { http_error 404 "no such node" }
    http_auth node view $id
    http_header
    html_head $::settings(NAME)$name
    set page [db onecolumn {select parsed from nodes,tags where tags.name='wiki:header' and tags.node=nodes.id order by nodes.modified desc limit 1}]
    append page $parsed
    append page [db onecolumn {select parsed from nodes,tags where tags.name='wiki:footer' and tags.node=nodes.id order by nodes.modified desc limit 1}]
    set time [time {set done [parse_dynamic $id $page]}]
    puts $done
    puts "<!-- dynamic parse time: $time -->"
    puts "</body></html>"
}

# display the search input page
# input: none, optional query string "q" which will display results page instead
# returns: nothing
proc search {} {
    http_auth auth verify
    get_input input
    if {[info exists input(q)]} {
        do_search $input(q)
        return
    }
    http_header
    html_head "Search"
    puts "<h1>Search</h1><br>"
    puts "<center><form name=search method=post action=[myself]/search><input name=string size=60><br><input type=submit value=Search></form></center>"
}

# perform a search and display the results
# input: the search string, or read from form postdata if empty
# see http://www.sqlite.org/cvstrac/wiki?p=FullTextIndex for syntax
# returns: nothing
proc do_search {{q {}}} {
    if {$q == ""} {
        get_post input
    } else {
        set input(string) $q
    }
    if {![info exists input(string)] || $input(string) == ""} {
        location wiki:search
        return
    }
    http_header
    html_head "Search results"

    set level [http_auth auth verify]

    # for tag searches need to remove boolean operators and negated terms to avoid suprious results,
    # and single quotes to prevent sql injection
    set tagterms {}
    foreach x [split $input(string)] {
        if {$x == "" || $x == "OR" || [string match -* $x]} { continue }
        lappend tagterms '[string map {' ""} [string tolower $x]]'
    }

    puts "<h1>Search results</h1><br>"
    puts "Searched for \"[filter_html [join $input(string)]]\"<br>"
    puts "<h3>Tag results</h3>"
    db eval "select nodes.id as id,nodes.name,tags.node,count(tags.name) from tags,nodes where tags.name in ([join $tagterms ,]) and tags.node=nodes.id and nodes.protect<=$level group by tags.node order by count(tags.name) desc" {
        set tags [db eval {select name from tags where node=$id}]
        puts "[link node:${tags.node} ${nodes.name}]&nbsp;&nbsp;&nbsp<span class=tags>("
        foreach x $tags { puts -nonewline "[link tag:$x $x] " }
        puts ")</span><br>"
    }

    puts "<br><h3>Full text results</h3>"
    fts eval {select id,name,snippet(search) as snippet from search where content match $input(string) and protect<=$level} {
        set tags [db eval {select name from tags where node=$id}]

        puts "<b>[link node:$id $name]</b><br>"
        if {$tags != ""} {
            puts -nonewline "<span class=tags>"
            foreach x $tags { puts -nonewline "[link tag:$x $x] " }
            puts -nonewline "</span><br>"
        }
        puts "$snippet<br><br>"
    }
}

# shows a page listing all nodes tagged with a given tag
# input: an exact tag name
# returns: nothing
proc showtag {tag} {
    set level [http_auth auth verify]
    http_header
    html_head "Pages tagged with $tag"
    puts "<h1>Pages tagged with &quot;$tag&quot;</h1><br>"
    db eval {select nodes.id,nodes.name from nodes,tags where nodes.protect<=$level and tags.name=$tag and tags.node=nodes.id} {
        puts "[link node:$id $name]<br>"
    }
}

# returns an http address pointing to the wiki root
proc myself {} {
    return http://$::request(HTTP_HOST)[expr {$::request(SERVER_PORT) != "80" ? ":$::request(SERVER_PORT)" : ""}][string trimright [file dirname $::request(PATH_INFO)] /]
}

proc link {to text} {
    return "<a href=\"[myself]/$to\">$text</a>"
}

proc showhistory {nodeid} {
    db eval {select name,modified_by,content from nodes where id=$nodeid} {}
    if {[info exists name]} {
        set title "Current name &quot;[link node:$nodeid $name]&quot;<br><br>"
    } elseif {[db exists {select id from history where original=$nodeid limit 1}]} {
        set title "Node is deleted<br><br>"
    } else {
        http_error 404 "no such node"
    }
    http_auth node view $nodeid
    http_header
    html_head "History for node $nodeid"
    puts "<h1>Revision history for node $nodeid</h1><br>"
    puts $title

    puts "<table><th>Rev</th><th>Saved</th><th>By</th><th>Line &#916;</th><th>Compare</th></tr>"
    set i [db eval {select count(original) from history where original=$nodeid}]
    puts "<tr><td align=center>[expr {$i + 1}]</td><td align=center>[link node:$nodeid current]</td><td>$modified_by</td>"
    set nextlines [expr {[llength [regexp -all -inline {[^\n]\n} $content]] + 1}]
    set nextid [db eval {select id from history where original=$nodeid order by created desc limit 1}]
    db eval {select id,tf(created) as created,created_by,content from history where original=$nodeid order by created desc} {
        set lines [expr {[llength [regexp -all -inline {[^\n]\n} $content]] + 1}]
        if {$nextid == $id} {
            puts "<td align=center>[expr {$nextlines - $lines}]</td><td align=right>[link diff:curr:$id prev]</td></tr>"
        } else {
            puts "<td align=center>[expr {$nextlines - $lines}]</td><td>[link diff:curr:$nextid current] [link diff:$nextid:$id prev]</td></tr>"
        }
        set nextlines $lines
        set nextid $id
        puts "<tr><td align=center>$i</td><td>[link viewhistory:$id $created]</td><td>$created_by</td>"
        incr i -1
    }
    puts "<td align=center><b>$lines</b></td><td>[link diff:curr:$id current]</td></tr></table>"
}

proc viewhistory {id} {
    db eval {select original,content,tf(created) as created from history where id=$id} {}
    if {![info exists original]} { http_error 404 "no such node" }
    http_auth node view $original
    http_header
    html_head "History for node $original"

    db eval {select name from nodes where id=$original} {}
    if {![info exists name]} {
        puts "Contents of deleted node $original until $created<br><hr>"
    } else {
        puts "Contents of &quot;[link node:$original $name]&quot; until $created<br><hr>"
    }
    set content [string map {& &amp;} $content]
    puts "<pre>[filter_html $content]</pre><hr>"
}

proc deletefile {id} {
    http_auth file delete $id
    db eval {select filename from files where id=$id} {}
    catch {file delete $filename}
    db eval {delete from files where id=$id}
    http_header
    puts "<body onload=javascript:history.go(-2);>"
}

proc showfile {id} {
    db eval {select name,filename,original_name,tf(created) as created,tf(modified) as modified from files where id=$id} {}
    if {![info exists name]} { http_error 404 "no such file" }
    http_header
    html_head "File Information: $name"
    puts "<script type=\"text/javascript\">
        function confirmation() {
	if (confirm(\"Delete file\\n$name?\")) { document.forms\[0].action = \"[myself]/delete:$id\"; document.forms\[0].submit(); }
        }
        </script>"
    puts "<h1>File Information: $name</h1><br><br>
         <form method='POST' enctype='multipart/form-data' action='[myself]/upload:$id'>
         <table><tr><td>Name:</td><td><input type=text name=name value=\"$name\" size=60></td></tr>
         <tr><td>Filename:</td><td>[file tail $filename]</td></tr>
         <tr><td>Size:</td><td>[fsize [file size $filename]]</td></tr>
         <tr><td>Original name:</td><td>$original_name</td></tr>
         <tr><td>Created:</td><td>$created</td></tr>
         <tr><td>Modified:</td><td>$modified</td></tr>
         <tr><td>Link:</td><td><a href=\"[path_to_uri $filename]\">[file tail $filename]</a></td></tr>
         <tr><td colspan=2 align=center><input type=file name=filedata size=60><br>
         <input type=submit value=Update>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         <input type=button onclick=javascript:confirmation() value=Delete></td></tr>
         </form></table><br><br>
         <h3>Nodes containing this file:</h3>"
    unset name
    db eval {select name,id as node from nodes,links where links.type='file' and links.target=$id and links.node=nodes.id} {
        puts "[link node:$node $name]<br>"
    }
    if {![info exists name]} { puts "None<br>" }
    if {[regexp -nocase {\.(jpe?g|bmp|png|gif)$} $filename]} {
        puts "<br><br><center><img src=\"[path_to_uri $filename]\"></center>"
    }
}

# displays node tagged with wiki:<name>
# input: tag name
# returns: nothing
proc wikitag {name} {
    db eval "select id from nodes,tags where tags.name='wiki:$name' and tags.node=nodes.id order by nodes.modified desc limit 1" {}
    if {[info exists id]} {
        showpage $id
        return
    }
    if {$name == "home"} {
        db eval {select id from nodes order by id asc limit 1} {}
        if {![info exists id]} {
            location wiki:new
            return
        }
        location node:$id
    } else {
        http_error 404 "no such node"
    }
}

proc taglist {} {
    get_input a
    set level [http_auth auth verify]
    http_header
    html_head "Tag list"
    set order "tags.name"
    if {[info exists a(sort)] && $a(sort) == "count"} { set order "c desc" }

    puts "<h1>Tag list</h1><br>
         <table><tr><th><a href=\"?sort=name\" style=\"text-decoration: none;\">Tag</a></th>
         <th><a href=\"?sort=count\" style=\"text-decoration: none;\">Links</a></th></tr><tr>"
    db eval "select tags.name as name,count(tags.node) as c from tags,nodes where nodes.protect<=$level and tags.node=nodes.id and tags.name not like 'wiki:%' group by tags.name order by $order" {
        puts "<td>[link tag:$name $name]<td align=center>$c</td></tr>"
    }
    puts "</tr></table>"
}

proc nodelist {} {
    set level [http_auth auth verify]
    get_input a
    http_header
    html_head "Node list"
    array set perm { 5 R/W 10 R/O 15 "User R/O" 20 "Hidden" 25 "Priv" }
    set order "lower(name)"
    if {[info exists a(sort)] && $a(sort) == "created"} { set order "created desc" }
    if {[info exists a(sort)] && $a(sort) == "modified"} { set order "modified desc" }
    if {[info exists a(sort)] && $a(sort) == "perms"} { set order "protect" }
    puts "<h1>Node list</h1><br><br>
         <table><tr><th><a href=\"?sort=name\" style=\"text-decoration: none;\">Name</a></th>
         <th><a href=\"?sort=created\" style=\"text-decoration: none;\">Created</a></th>
         <th><a href=\"?sort=modified\" style=\"text-decoration: none;\">Modified</a></th>
         <th><a href=\"?sort=perms\" style=\"text-decoration: none;\">Perms</a></th>
         <th>Links</th><th>Tags</th></tr>"
    db eval "select id,name,tf(created) as created,tf(modified) as modified,protect from nodes where protect<=$level and id not in (select distinct node from tags where name='wiki:hide') order by $order" {
        puts "<tr><td>[link node:$id $name]</td><td>$created</td><td>$modified</td><td align=center>$perm($protect)</td>"
        puts "<td align=center>[link links:$id [db eval {select count(node) from links where target=$id and type='node'}]]</td><td>"
        db eval {select name from tags where node=$id} {
            puts "[link tag:$name $name] "
        }
        puts "</td></tr>"
    }
    puts "</table>"
}

proc filelist {} {
    http_auth auth verify
    db function fsize {file size}
    get_input a
    http_header
    html_head "File list"
    set order "lower(name)"
    if {[info exists a(sort)] && $a(sort) == "created"} { set order "created desc" }
    if {[info exists a(sort)] && $a(sort) == "modified"} { set order "modified desc" }
    if {[info exists a(sort)] && $a(sort) == "size"} { set order "fsize(filename) desc" }
    puts "<h1>File list</h1><br>
         <table><tr><th><a href=\"?sort=name\" style=\"text-decoration: none;\">Name</a></th>
         <th>Filename</th><th><a href=\"?sort=created\" style=\"text-decoration: none;\">Created</a></th>
         <th><a href=\"?sort=modified\" style=\"text-decoration: none;\">Modified</a></th>
         <th><a href=\"?sort=size\" style=\"text-decoration: none;\">Size</a></th></tr>"
    db eval "select id,name,filename,tf(created) as created,tf(modified) as modified from files order by $order" {
        puts "<tr><td>[link file:$id $name]</td><td><a href=[path_to_uri $filename]>[file tail $filename]</a></td>
             <td>$created</td><td>$modified</td><td>[fsize [file size $filename]]</td></tr>"
    }
    puts "</table>"
}

# nicely formats a file size
# input: size in bytes
proc fsize {size} {
    if {$size > pow(2,20)} {
        return [format %5.2fM [expr {$size / pow(2,20)}]]
    }
    if {$size > pow(2,10)} {
        return [expr {int($size / pow(2,10))}]K
    }
    return "$size bytes"
}

# display the upload file page
# input: none
# returns: nothing
proc upload {} {
    http_auth file create
    http_header
    html_head "File upload"
    puts "<h1>File upload</h1><br><br>"
    puts "<form method='POST' enctype='multipart/form-data' action='[myself]/upload:new'>"
    puts "<table><tr><td><table style=\"border: 0px;\"><tr><td style=\"border: 0px;\">File:</td><td style=\"border: 0px;\"><input type=file name=filedata size=60></td></tr>"
    puts "<tr><td style=\"border: 0px;\">Name:</td><td style=\"border: 0px;\"><input type=text name=name size=60></td></tr></table></td></tr><tr><td colspan=2 align=center>"
puts "<input type=submit value=Upload></td></tr></table></form>"
}

# handles the post of the file upload page
# input: an existing file id or "new"
# returns: nothing
proc upload_post {id} {
    http_auth file create
    fconfigure stdin -encoding binary -translation lf
    set data [read stdin]
    # decode the mime data:
    # get the boundary string from the http header
    # and for each section set pos to the start
    regexp {boundary=(.*)(;|$)} $::request(CONTENT_TYPE) -> boundary
    while {[set pos [string first $boundary $data]] > 0} {
        # trim everything up to the end of the boundary off of $data so we dont see it again
        # set hpos to the end of the mime section header (first 2 consecutive crlfs)
        set data [string range $data [expr {[string length $boundary] + $pos + 2}] end]
        if {[set hpos [string first \r\n\r\n $data]] >= 0} {
            # save the header and trim it off of $data
            # then mime content is everything up to the next boundary
            set header [string range $data 0 [expr {$hpos - 1}]]
            set data [string range $data [expr {$hpos + 4}] end]
            set content [string range $data 0 [expr {[string first --$boundary $data] - 3}]]

            # now parse the mime header for the filename and the user specified name
            regexp {name=\"(.*?)\"} $header -> nameval
            if {$nameval == "name"} {
                set name [string trim $content]
            } elseif {$nameval == "filedata"} {
                set filedata $content
                regexp {filename=\"(.*?)\"} $header -> original
            }
        }
    }
    if {$id == "new"} {
        if {$filedata == ""} {
            location wiki:upload
            return
        }
        # fix up the filename: backslashes, "[1]", spaces, and trim the full path
        set tail [file tail [string map {\\ /} $original]]
        set tail [regsub {\[1\]\.} $tail .]
        if {$name == ""} { set name [file rootname $tail] }
        set base [file dirname $::request(PATH_TRANSLATED)]/files/
        set tail [string map {" " _} $tail]
        # start with 0000 before filename, increment it until we get a unique name
        set num 0
        while {[file exists [set filename [format "%s%04d%s" $base $num $tail]]]} { incr num }
    } else {
        # if its not new and there was no file, just update the name
        if {$filedata == ""} {
            db eval {update files set name=$name where id=$id}
            db eval {commit transaction}
            location file:$id
            return
        }
        # otherwise overwrite the existing file
        set filename [join [db eval {select filename from files where id=$id}]]
    }

    set fh [open $filename w]
    fconfigure $fh -encoding binary -translation lf
    puts -nonewline $fh $filedata
    close $fh

    if {$id == "new"} {
        db eval {insert into files (name,original_name,filename,created,modified) values($name,$original,$filename,datetime('now'),datetime('now'))}
        db eval {commit transaction}
        location file:[db last_insert_rowid]
    } else {
        db eval {update files set modified=datetime('now'),name=$name}
        db eval {commit transaction}
        location file:$id
    }
}

proc http_header {args} {
    puts "Content-type: text/html"
    puts "[join $args \n]\n"
}

proc location {loc args} {
    #set qs [list]
    #foreach x $args {
    #    if {$x != ""} { lappend qs $x }
    #}
    #if {[llength $qs] > 0} { append loc ?[join $qs &] }
    puts "Location: [myself]/$loc\n"
}

proc html_head {title} {
    puts "<head>\n<title>$title</title>"
    db eval {select content as parsed from nodes,tags where tags.name='wiki:style' and tags.node=nodes.id order by nodes.modified desc limit 1} {}
    if {[info exists parsed]} {
        #set parsed [string map {&#123; \{ &#125; \} <br> \n\n} $parsed]
        puts "<style>\n$parsed\n</style>\n"
    }
    puts "</head><body>"
}

# performs parsing of node contents at save time
# input: node id and contents
# returns: node content with static html
proc parse_static {id data} {
    # remove anything like %word{ }% and save it to replace after other parsing
    set indices {}
    foreach i [regexp -all -inline -indices {%[a-z]{3,7}?\{.*?\}%} $data] {
        set indices [eval linsert [list $indices] 0 $i]
    }
    foreach {x y} $indices {
        lappend saved [string range $data $x $y]
        set data [string replace $data $x $y @PASS@]
    }
    if {$::settings(FILTER_HTML)} {
        set data [filter_white_html $data]
    }

    set data [string map {[ &#91; \{ &#123; \} &#125; $ &#36;} $data]
    # link:(name)
    set data [regsub -all {\m([a-z]{3,7}):\(([^\)]+)\)} $data "\[static_call $id \{\\1\} \{\\2\}]"]
    # link:id
    set data [regsub -all {\m([a-z]{3,7}):([[:digit:]]+)} $data "\[static_call $id \{\\1\} \{\\2\}]"]
    # proto:// external links
    set data [regsub -all {(\([^\(\)]+\):)?[a-z]{3,7}://[^	 \"\n<]+} $data {[static_http {\0}]}]
    # pre and ul,ol lists
    set data [regsub -all {(\n  +[^\n]*){1,}} $data "\n\[static_lists \{\\0\}\]"]
    # |tables|
    #set data [regsub -all {(\n\|[^\n]+\|){1,}} $data {[static_table {\0}]}]
    set data [regsub -all -line {(^\|.+\|($|\n))+} $data {[static_table {\0}]}]
    # ?option: vals
    set data [regsub -all -line {^\?([a-z]{3,10}):(.*)$} $data "\[static_options $id \{\\1\} \{\\2\}]"]
    # hr
    set data [regsub -all -line {^----*$} $data "<hr>"]
    # _italic_
    #set data [regsub -all {([^\w])_([^ _][^_]*[^ _])_([^\w])} $data {\1<i>\2</i>\3}]
    set data [regsub -all {_(?!\s)([^_]+)_(?![[:alnum:]])} $data {<i>\1</i>}]
    # *bold*
    #set data [regsub -all {([^\w])\*([^ \*][^\*]*[^ \*])\*([^\w])} $data {\1<b>\2</b>\3}]
    set data [regsub -all {\*(?!\s)([^*]+)\*(?![[:alnum:]])} $data {<b>\1</b>}]
    # =fixed=
    #set data [regsub -all {([^\w])=([^ =][^=]*[^ =])=([^\w])} $data {\1<span class=fixed>\2</span>\3}]
    #set data [regsub -all {=(?!\s)([^=]+)=(?![[:alnum:]\"])} $data {<span class=fixed>\1</span>}]
    # +++headings+
    set data [regsub -all -line {^\+{1,5}.+\+$} $data {[static_heading {\0}]}]
    # +paragraph\n\n
    #set data [regsub -all {\n\+([[:alnum:]].*?)\n\n} $data "\n<p>\\1</p>"]
    set data [regsub -all {(?w)^\+(.*?)(?=\n\n|\Z)(?:\n|\Z)} $data {<p>\1</p>}]
    # %variables%
    set data [regsub -all {%([A-Z]{3,10})%} $data {[static_variable {\1}]}]
    # line breaks
    set data [string map {\n\n\n\n <br><br><br>\n \n\n\n <br><br>\n \n\n <br>\n} $data]

    set data [subst -nobackslashes -novariables $data]

    # replace what we saved at the beginning
    if {[info exists saved]} {
        set indices {}
        foreach i [regexp -all -inline -indices {@PASS@} $data] {
            set indices [eval linsert [list $indices] 0 $i]
        }
        set i 0
        foreach {x y} $indices {
            set pass [lindex $saved $i]
            set data [string replace $data $x $y $pass]
            incr i
        }
    }
    return $data
}

proc static_table {data} {
    set data [split [subst [string map {\" \\\"} $data]] \n]
    if {[lindex $data end] == ""} { set data [lrange $data 0 end-1] }
    set t "\n<table>\n"
    foreach line $data {
        append t "<tr>"
        foreach x [lrange [split $line |] 1 end-1] {
            set tag td
            set span ""
            set align ""
            if {[string index $x 0] == "+"} {
                set x [string range $x 1 end]
                set tag th
            }
            if {[regexp {^(([\-\^])\2*)(.*)} $x -> span one x]} {
                set span " [string map {^ row - col} $one]span=[expr {[string length $span] + 1}]"
            }

            set len [string length $x]
            set x [string trimleft $x]
            set left [expr {$len - [string length $x]}]

            set x [string trimright $x]
            set right [expr {$len - $left - [string length $x]}]

            if {$left >= 2 && $right >= 2} {
                set align " align=center"
            } elseif {$left >= 2} {
                set align " align=right"
            }

            append t "<$tag$align$span>$x</$tag>"
        }
        append t "</tr>\n"
    }
    append t "\n</table>\n"
    return $t
}

proc static_variable {var} {
    switch -exact -- $var {
        TOC {
            upvar data data
            set headings [list {}]
            foreach {crap x} [regexp -all -inline {\[static_heading \{(.*?)\}\]} $data] {
                set depth [expr {[string length $x] - [string length [string trimleft $x +]]}]
                set x [string trim $x +]
                lappend headings "  [string repeat 1 $depth] <a href=#[string map {" " _} $x]>$x</a>"
            }
            if {[info exists headings]} {
                return "<div class=toc>\n[static_lists [join $headings \n]]</div>\n"
            }
        }
        default { return %$var% }
    }
}

proc static_heading {data} {
    set size [expr {[string length $data] - [string length [string trimleft $data +]]}]
    set data [string trim $data +]
    return "<a name=[string map {" " _} $data]><h$size>$data</h$size></a>\n"
}

proc static_options {id opt data} {
    switch -exact -- $opt {
        tags {
            foreach x [split $data] {
                if {$x == ""} { continue }
                db eval {insert or ignore into tags (name,node) values(lower($x),$id)}
            }
            return
        }
        default { return "<!-- unknown option $opt -->" }
    }
}

proc static_http {data} {
    if {[string match {*:*://*} $data]} {
        set data [split $data :]
        set type [string trim [lindex $data 0] ()]
        set data [string trim [join [lrange $data 1 end] :] ()]
        if {$type == "img"} {
            return "<img src=\"$data\">"
        } else {
            return "<a href=\"$data\">$type</a>"
        }
    } else {
        return "<a href=\"$data\">$data</a>"
    }
}

proc static_lists {data} {
    set data [subst [string map {\" \\\" \\ \\\\} $data]]
    set data [lrange [split $data \n] 1 end]

    if {![string match {[*1]*} [string trimleft [lindex $data 0]]]} {
        return "<pre>\n[join $data \n]\n</pre>"
    }

    set ret {}
    set last {}
    foreach line $data {
        set line [split [string trimleft $line]]
        set now [lindex $line 0]

        if {[string match {[*1]*} $now]} {
            if {$now != $last} {
                set i 0
                foreach x [split $now {}] y [split $last {}] {
                    if {$x != $y} break
                    incr i
                }
                append ret [string map {* </ul> 1 </ol>} [string range $last $i end]]\n
                append ret [string map {* <ul> 1 <ol>} [string range $now $i end]]\n
                set last $now
            }
            set line [lrange $line 1 end]
        }
        append ret <li>[string trim [join $line]]\n
    }
    append ret [string map {* </ul> 1 </ol>} $last]\n
    return $ret
}

proc static_call {id cmd data} {
    switch -exact -- $cmd {
        link {
            if {[string is integer $data]} {
                db eval {insert or ignore into links (node,type,target) values($id,'node',$data)}
                return "link:($data)"
            } elseif {![string match {*[:.]*} $data]} {
                if {[set target [db eval {select id from nodes where lower(name)=lower($data) order by modified desc limit 1}]] != ""} {
                    db eval {insert or ignore into links (node,type,target) values($id,'node',$target)}
                    return "link:($target)"
                } else {
                    return [link $data $data]
                }
            } elseif {[regexp {^([[:alnum:] ]+):(.*:.*)} $data -> name href]} {
                return "<a href=\"$href\">$name</a>"
            } else {
                return "<a href=\"$data\">$data</a>"
            }
        }
        file {
            if {[string is integer $data]} {
                db eval {insert or ignore into links (node,type,target) values($id,'file',$data)}
                return "file:($data)"
            } elseif {[set target [db eval {select id from files where name=$data order by modified desc limit 1}]] != ""} {
                db eval {insert or ignore into links (node,type,target) values($id,'file',$target)}
                return "file:($target)"
            } else {
                return [link wiki:upload upload]
            }
        }
        img {
            regexp {(.*) (\d+x\d+)$} $data -> data tns
            if {[string is integer $data]} {
                db eval {select id as imgid,name,filename from files where id=$data} {}
            } elseif {![string match {*://*} $data]} {
                db eval {select id as imgid,name,filename from files where name=$data order by modified,created desc limit 1} {}
            } else {
                return "<img src=\"$data\" alt=\"$data\">"
            }
            if {![info exists filename]} { return "<i>image not found</i>" }
            db eval {insert or ignore into links (node,type,target) values($id,'file',$imgid)}

            if {[info exists tns]} {
                set ofn $filename
                set filename [file dirname $filename]/thumbs/[file rootname [file tail $filename]]_$tns[file extension $filename]
                if {![file exists $filename]} {
                    catch {exec /usr/local/bin/convert -scale $tns $ofn $filename}
                }
                return "<a href=[path_to_uri $ofn]><img src=[path_to_uri $filename] alt=\"$name\"></a>"
            }
            return "<img src=[path_to_uri $filename] alt=\"$name\">"
        }
        tag {
            return [link tag:$data $data]
        }
        wiki {
           if {$data == "searchembed"} {
               return "<form name=search method=post action=[myself]/search><input name=string> <input type=submit value=Search>"
           }
           if {$data == "uploadembed"} {
               return "<form method='POST' enctype='multipart/form-data' action='[myself]/upload:new'>
                       <input type=submit value=Upload><input type=file name=filedata></form>"
           }
           if {[string match tag:* $data]} {
               return [link tag:$data $data]
           }
           return [link wiki:$data $data]
        }
        default {
            return "$cmd: $data"
        }
    }
}

proc path_to_uri {path} {
    return [regsub "^$::request(DOCUMENT_ROOT)" $path {}]
}

# performs parsing of node contents at display time
proc parse_dynamic {id data} {
    #return $data
    parse_tcl $id data
    # escape special chars before subst
    set data [string map {[ \\[ \{ \\\{ \} \\\} $ \\$ \\ \\\\} $data]
    # %variables% - some of these are passed through static parsing
    set data [regsub -all {%([A-Z]{2,10})%} $data {[dynamic_variable {\1}]}]
    # link:(id) and file:(id)
    set data [regsub -all {([a-z]{3,10}):\(([^\(\)]+)\)} $data "\[call_dynamic $id \{\\1\} \{\\2\}]"]
    set data [subst $data]
    return $data
}

proc dynamic_variable {var} {
    switch -exact -- $var {
        MODIFIED { upvar 2 modified modified; return [format_time $modified] }
        MODIFIEDBY { upvar 2 modified_by modified_by; return $modified_by }
        ID { upvar 2 id id; return $id }
        NAME { upvar 2 name name; return [filter_html $name] }
        PATH { return [file dirname $::request(PATH_INFO)]/ }
        TAGS {
            upvar 2 id id
            set out [list]
            db eval {select name from tags where node=$id} {
                lappend out [link tag:$name $name]
            }
            return [join $out]
        }
        LOGIN {
            if {[info exists ::request(REMOTE_USER)]} {
                return "logged in as $::request(REMOTE_USER) - <a href=\"[myself]/wiki:logout\">logout</a>"
            } else {
                puts "<a href=\"[myself]/wiki:login\">login</a>"
            }
        }
        default { return $var }
    }
}

proc filewrapper {cmd args} {
    if {$cmd == "copy" || $cmd == "delete" || $cmd == "rename"} {
        error denied
    }
    return [eval [list file $cmd] $args]
}

proc procwrapper {i name myargs body} {
    if {[interp eval $i info commands $name] != ""} { error denied }
    interp invokehidden $i _proc $name $myargs $body
}

proc db_auth {action table col db view} {
    if {$action == "SQLITE_READ" && ($view != "pages" && $table != "pages" && $table != "tags" && $table != "links" && $table != "files")} {
#puts "DENY $action $table $col $db $view<br>"
        return SQLITE_DENY
    }
#puts "OK $action $table $col $db $view<br>"
    return SQLITE_OK
}

proc setup_interp {} {
    set i [interp create]
    interp alias $i file {} filewrapper
    #interp alias $i proc {} procwrapper $i
    interp alias $i sql {} interp invokehidden $i db eval
    interp alias $i puts {} append output
    foreach x {myself link fsize get_input filter_html unescape format_time db_auth} {
        interp alias $i $x {} $x
    }
    interp eval $i {
        load /usr/local/lib/sqlite3.5.1/libsqlite3.5.1.so Sqlite3
        sqlite3 db wiki.db -readonly 1
        db function tf format_time
        db eval {create temp view pages as select * from nodes where protect<=20}
        # need this to initialize the view before the authorizer
        db eval {select id from pages}
        db authorizer db_auth
    }
    interp eval $i [list array set request [array get ::request]]
    foreach x {load source open exec socket rename proc cd sqlite sqlite3 db} {
        interp hide $i $x
    }
    interp hide $i interp
    return $i
}

proc parse_tcl {id var} {
    upvar $var data
    set indices [list]
    foreach i [regexp -all -inline -indices {%tcl\{.*?\}%} $data] {
        set indices [eval linsert [list $indices] 0 $i]
    }
    if {[llength $indices] == 0} { return }

    set i [setup_interp]
    foreach {x y} $indices {
        set output {}
        interp invokehidden $i db eval "select id,name,tf(modified) as modified,tf(created) as created,modified_by from pages where id='$id'" {}
        if {[catch {interp eval $i [string range $data [expr {$x + 5}] [expr {$y - 2}]]} err]} {
            set output "<i>error in script</i>\n<!--\n$::errorInfo\n-->\n"
        }
        if {$::settings(FILTER_HTML)} {
            set output [filter_white_html $output]
        }
        set data [string replace $data $x $y $output]
    }
    interp delete $i
}

proc call_dynamic {id cmd data} {
    switch -exact -- $cmd {
        link {
            #regexp {^([[:digit:]]+(.*))} $data -> data rest
            db eval {select name from nodes where id=$data} {}
            if {![info exists name]} { return "<i>no such node</i>" }
            return [link node:$data $name]
        }
        file {
            #regexp {^([[:digit:]]+)(.*)} $data -> data rest
            db eval {select name,filename from files where id=$data} {}
            if {![info exists name]} { return [link wiki:upload upload] }
            if {$name == ""} { set name [file tail $filename] }
            return "<a href=\"[path_to_uri $filename]\" class=filelink>$name</a> <span class=fileinfo>([link file:$data info])</span>"
        }
        default {
            return "$cmd: $data"
        }
    }
}

proc get_post {var} {
    upvar $var a
    foreach x [split [read stdin] &] {
        set x [split $x =]
        set string [lindex $x 1]
        set string [string map {+ " "} $string]
        set string [unescape $string]
        set string [string map {\r ""} $string]
        set a([lindex $x 0]) $string
    }
}

proc get_input {var} {
    upvar $var a
    foreach x [split $::request(QUERY_STRING) &] {
        set x [split $x =]
        set a([lindex $x 0]) [unescape [lindex $x 1]]
    }
    if {![info exists a]} { array set a {} }
}

proc get_cookies {var} {
    upvar $var cookies
    unset -nocomplain cookies
    if {![info exists ::request(HTTP_COOKIE)]} { return }
    array set cookies {}
    foreach c [split $::request(HTTP_COOKIE) \;] {
        set c [split $c =]
        set cookies([string trim [lindex $c 0]]) [unescape [string trim [lindex $c 1]]]
    }
}

proc filter_html {data} {
    return [string map {< &lt;} $data]
}

proc filter_white_html {data} {
    set white $::settings(HTML_WHITELIST)
    return [subst -novariables [regsub -all {<[[:space:]/]*([^[:space:]>]*)>?} $data {[filter2_html {\0} {\1}]}]]
}

proc filter2_html {all tag} {
    upvar white white
    if {[lsearch -nocase -sorted $white $tag] > -1} { return $all }
    return [string map {< &lt;} $all]
}

proc unescape {data} {
    #set data [string map {[ &#91; \{ &#123; \} &#125;} $data]
    return [subst -nobackslashes -novariables [regsub -all -nocase {%([[:xdigit:]]{2})} $data {[format %c 0x\1]}]]
}

proc format_time {t} {
    set t [clock scan $t -gmt 1]
    return [clock format $t -format $::settings(TF) -timezone :$::settings(TZ)]
}

# save a node after editing
# input: a node id or "new", and form postdata
# returns: nothing
proc savepage {id} {
    get_post input
    if {$id != "new"} {
        db eval {select content,protect,strftime('%s',modified) as modified from nodes where id=$id} {}
        if {![info exists content]} { http_error 404 "no such node" }
        if {$input(content) == "delete"} {
            http_auth node delete $id
        } else {
            set level [http_auth node edit $id]
        }
        if {$input(editstarted) < $modified} {
            db eval {select tf(modified) as modified,modified_by from nodes where id=$id} {}
            http_error 409 "Edit conflict: modified by $modified_by at $modified"
        }
        if {![string is integer -strict $input(protect)] || $input(protect) > $level} { set input(protect) $protect }
        if {$input(content) == $content} {
            fts eval {update search set name=$input(name),protect=$input(protect) where id=$id}
            db eval {update nodes set name=$input(name),protect=$input(protect) where id=$id}
            db eval {commit transaction}
            location node:$id
            return
        }
        db eval {delete from tags where node=$id}
        db eval {delete from links where node=$id}
        db eval {insert into history (original,content,created,created_by) select id,content,datetime('now'),modified_by from nodes where id=$id}
        if {$input(content) == "delete"} {
            db eval {delete from nodes where id=$id}
            fts eval {delete from search where id=$id}
            db eval {commit transaction}
            location {}
            return
        }
    } else {
        set level [http_auth node create]
        if {![string is integer -strict $input(protect)] || $input(protect) > $level} { set input(protect) $level }
    }

    if {[catch {set time [time {set parsed [parse_static $id $input(content)]}]   } err]} {
        http_error 500 $::errorInfo
    }
    set strip "$input(name) [striphtml $parsed]"
    append parsed "\n<!-- static parse time: $time -->"

    set user [expr {[info exists ::request(REMOTE_USER)] ? "$::request(REMOTE_USER)" : "anonymous"}]@$::request(REMOTE_ADDR)

    if {$id == "new"} {
        db eval {insert into nodes (name,content,parsed,protect,modified,created,modified_by) values($input(name),$input(content),$parsed,$input(protect),datetime('now'),datetime('now'),$user)}
        set id [db last_insert_rowid]
        fts eval {insert into search (id,name,protect,content) values($id,$input(name),$input(protect),$strip)}
    } else {
        db eval {update nodes set name=$input(name),content=$input(content),parsed=$parsed,modified=datetime('now'),modified_by=$user,protect=$input(protect) where id=$id}
        fts eval {update search set name=$input(name),protect=$input(protect),content=$strip where id=$id}
    }
    db eval {commit transaction}
    location node:$id
}

proc striphtml {data} {
    regsub -all {<[^>]*?>} $data { } data
    regsub -all {%[a-z]{3,7}\{.*?\}%} $data { } data
    regsub -all {%[a-z]{2,10}%} $data { } data
    regsub -all {&#[0-9]{2,3};?} $data { } data
    regsub -all {&[a-z]{2,5};} $data { } data
    regsub -all {[a-z]{3,7}:\([^\)]+\)} $data { } data
    return $data
}

# look up a node by name
# if it doesnt exist then start a new one
# input: a node name
# returns: nothing
proc showpagebyname {name} {
    set name [unescape $name]
    set id [db eval {select id from nodes where lower(name)=lower($name)}]
    if {$id == ""} {
        editpage new $name
    } else {
        location node:$id
    }
}

# show a page listing nodes that link to the given node
# input: a node id
# returns: nothing
proc showlinks {id} {
    db eval {select name from nodes where id=$id} {}
    if {![info exists name]} { http_error 404 "no such node" }
    http_auth node view $id
    http_header
    html_head "Pages linking to $name"
    puts "<h1>Links to &quot;$name&quot;</h1><br>"
    db eval {select nodes.id as link,nodes.name from nodes,links where links.type='node' and links.target=$id and links.node=nodes.id} {
        puts "[link node:$link $name]<br>"
    }
    if {![info exists link]} {
        puts "No links"
    }
}

proc open_databases {} {
    #package require sqlite3
    load /usr/local/lib/sqlite3.5.1/libsqlite3.5.1.so Sqlite3
    #set dir [file dirname [pwd]]
set dir [pwd]
    sqlite3 db [file join $dir wiki.db]
    sqlite3 fts [file join $dir fts.db]
    fts eval {PRAGMA synchronous = OFF}
    db function tf {format_time}
}

proc close_databases {} {
    db close
    fts close
}

proc settings {} {
    global cookies settings
    db eval {select * from settings} { set settings($name) $val }
    get_cookies cookies
    foreach x {TZ TF} {
        if {[info exists cookies($x)]} { set settings($x) $cookies($x) }
    }
}

proc service_request {} {
    settings

    #set doc [split [string range $::request(PATH_TRANSLATED) [string length [file dirname [info script]]/] end] :]
    set doc [split [file tail $::request(PATH_INFO)] :]
    set cmd [lindex $doc 0]
    set arg [join [lrange $doc 1 end] :]

    if {$::request(REQUEST_METHOD) == "GET"} {
        switch -exact -- $cmd {
            index.html { wikitag home }
            edit { editpage $arg }
            node { showpage $arg }
            file { showfile $arg }
            tag  { showtag $arg }
            links { showlinks $arg }
            history { showhistory $arg }
            viewhistory { viewhistory $arg }
            diff { showdiff $arg }
            wiki {
                switch -exact -- $arg {
                    nodes { nodelist }
                    files { filelist }
                    tags { taglist }
                    upload { upload }
                    new { editpage new }
                    search { search }
                    config { editconfig }
                    prefs { editprefs }
                    users { userlist }
                    login { login }
                    logout { logout }
                    default { wikitag $arg }
                }
            }
            default { showpagebyname $cmd }
        }
    } elseif {$::request(REQUEST_METHOD) == "POST"} {
        db eval {begin transaction}
        switch -exact -- $cmd {
            edit   { savepage $arg }
            new    { savepage new }
            upload { upload_post $arg }
            delete { deletefile $arg }
            search { do_search }
            users  { saveusers }
            prefs  { saveprefs }
            config { saveconfig }
            login  { do_login }
        }
    }
}

#puts "Content-type: text/plain\n"
#parray env
#exit

set t [time {

open_databases
if {[info exists env(GATEWAY_INTERFACE)]} {
    foreach x {REMOTE_ADDR HTTP_HOST PATH_INFO \
        REQUEST_URI SERVER_PORT PATH_TRANSLATED \
        DOCUMENT_ROOT QUERY_STRING SCRIPT_FILENAME \
        REQUEST_METHOD} { set request($x) $env($x) }
    foreach x {CONTENT_TYPE HTTP_COOKIE} {
        if {[info exists env($x)]} { set request($x) $env($x) }
    }
    service_request
    close_databases

    if {[info exists TimeProfilerMode]} { TimeProfilerDump description }
} else {
    proc close_databases {} {}
}

}]

set t [lindex $t 0]
exec echo [file tail $::env(PATH_TRANSLATED)] [expr {double($t) / 1000}] >> log
