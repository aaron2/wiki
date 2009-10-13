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
    if {![authorized user view]} { no_auth }
    http_header
    get_input a

    if {[info exists a(sort)]} { set sort $a(sort) }
    sortable sort order {user created modified perms} {"lower(user)" "created desc" "level desc"}
    pagination a 50 wiki:users [db onecolumn {select count(user) from users}]

    html_head "User list" {
        <script src="include/jquery-1.3.2.min.js"></script>}

    puts "<h1>User List</h1><br>
        <table style=\"border: 0px; padding: 0px;\">$nav<tr><td style=\"border: 0px; padding: 0px;\" colspan=3>
        <form action=\"[myself]/users\" method=post>
        <input type=hidden name=user>
        <input type=hidden name=action>
        <input type=hidden name=userlevel value=$::request(USER_LEVEL)>
        <table class=wikilist><tbody id=list>
        [th $sort {Edit 0} {User 1} {Host 0} {Password 0} {Name 0} {Email 0} {Created 1} {Level 1}]"
    db eval "select * from users order by $order limit $perpage offset $offset" {
        puts "<tr><td align=center></td>
             <td>$user</td>
             <td>$ip</td>
             <td align=center>&#149;&#149;&#149;&#149;</td>
             <td>$name</td>
             <td>$email</td>
             <td>[format_time $created]</td>
             <td>[string map {10 Read-only 20 Normal 25 Privileged 30 Admin 0 Blocked} $level]</td></tr>"
    }
    puts "</tbody></table><a href=# style=\"padding-left: .5em;\" id=new>new</a></form></td></tr></table>
        <script src=\"include/userlist.js\"></script></body></html>"
}

# handles the ajax post from the user list page
# input: form postdata
# returns: nothing
proc saveusers {} {
    get_post input
    db function password {hash_pass}
    switch -exact -- $input(action) {
        edit {
            if {![authorized user modify]} { no_auth }
            db eval {select * from users where user=$input(user)} old {}
            foreach x {email host level name} {
                if {![info exists input($x)]} { http_error 400 }
                set input($x) [string trim $input($x)]
            }
            set nlevel [string map {10 Read-only 20 Normal 25 Privileged 30 Admin 0 Blocked} $input(level)]

            if {[info exists input(password)] && $input(password) != ""} {
                db eval {update users set password=password($input(password)) where user=$input(user)}
            }

            db eval {update users set email=$input(email),ip=$input(host),level=$input(level),name=$input(name) where user=$input(user)}
            set res "<td></td><td>$input(user)</td><td>$input(host)</td><td align=center>&#149;&#149;&#149;&#149;</td><td>$input(name)</td><td>$input(email)</td><td>[format_time $old(created)]</td><td>$nlevel</td>"
        }
        new {
            if {![authorized user create]} { no_auth }
            foreach x {newuser email host level name} {
                if {![info exists input($x)]} { http_error 400 }
                set input($x) [string trim $input($x)]
            }
            if {$input(newuser) == "" || [db exists {select user from users where user=$input(newuser)}]} {
                http_error 400
            }
            db eval {insert into users (user,ip,password,name,email,created,level) values($input(newuser),$input(host),password($input(password)),$input(name),$input(email),datetime('now'),$input(level))}
            set res "<td></td><td>$input(newuser)</td><td>$input(host)</td><td align=center>&#149;&#149;&#149;&#149;</td><td>$input(name)</td><td>$input(email)</td><td>[format_time [db onecolumn {select created from users where user=$input(newuser)}]]</td><td>$input(level)</td>"
        }
        delete {
            if {![authorized user delete $input(user)]} { no_auth }
            db eval {delete from users where user=$input(user)}
            set res "\{ result : \"ok\", user : \"$input(user)\" \}"
        }
        default {
            http_error 400
        }
    }
    db eval {commit transaction}
    http_header
    puts $res
}

proc editor {userlevel objectlevel action name content back} {
    puts "<form name=form action=\"$action\" method=post>
          <input name=name value=\"$name\" style=\"width: 100%; margin-bottom: .5em;\"><br>
          <textarea name=content id=content rows=30 style=\"width: 100%; margin-bottom: .7em;\">$content</textarea><br>
          <div id=captcha></div>
          <div><input type=submit value=Save style=\"padding-left: 1em; padding-right: 1em;\">
          <input type=button name=cancel value=Cancel onclick=\"javascript:$back;\" style=\"margin-left: 2em;\">
          <input type=hidden name=editstarted value=[clock seconds]>
          <a style=\"position: absolute; right: 50%;\" href=[myself]/tag:wiki:help>Help</a>
          <script src=\"include/editor.js\"></script>"

    if {$objectlevel != "" && $::request(USER_AUTH)} {
        puts "<select name=protect style=\"position: absolute; right: 1em;\">"
        foreach val {5 10 15 20 25} name [list "All Read/Write" "Anon Read-only" "Users Only" "User Read-only" Privileged] {
            if {$val > $userlevel} { continue }
            puts -nonewline "<option value=$val [expr {$val == $objectlevel ? " selected" : ""}]>$name</option>"
        }
        puts "</select>"
    } elseif {!$::request(USER_AUTH)} {
        puts "<a href=\"[myself]/wiki:login\" style=\"position: absolute; right: 1em;\">login</a>"
    }
    puts "</div></form>"
}

# show the node editing page
# input: existing node id or "new", optionally a title and initial content if the id is "new"
# returns: nothing
proc editnode {id args} {
    if {$id == "new"} {
        if {![authorized node create]} { no_auth }
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
        if {![authorized node edit $id]} { no_auth }
        http_header
        html_head "Editing $name"
        set action [myself]/edit:$id
        set content [string map {& &#38;} $content]
        regsub "^\n" $content "\\&nbsp;\n" content
        set back "document.location='[myself]/node:$id'"
    }
    editor $::request(USER_LEVEL) $protect $action $name $content $back
    if {!$::request(USER_AUTH) && $::settings(RECAPTCHA_PRIVATE_KEY) != ""} {
        puts "<script src=\"http://api.recaptcha.net/js/recaptcha_ajax.js\"></script>
            <script>Recaptcha.create('$::settings(RECAPTCHA_PUBLIC_KEY)', 'captcha', { theme: 'clean' });</script>"
    }
    puts "</body></html>"
}

# show the wiki configuration page
# input: none
# returns: nothing
proc editconfig {} {
    if {![authorized wiki config]} { no_auth }
    get_input input
    http_header
    html_head "Wiki configuration"
    puts "<h1>Wiki configuration</h1><br>"
    db eval {select name,val from settings} { set tmpset($name) $val }

    if {[info exists input(status)]} { puts "<center>[filter_html $input(status)]</center><br><br>" }
    puts "<form name=form action=\"[myself]/config\" method=post><table style=\"border: 0px solid black;\"><tr><td style=\"\"><table style=\"border: 0px solid black;\"> 
          <tr><td style=\"border: 0px solid black;\">Title prefix</td>
          <td style=\"border: 0px solid black;\"><input type=text name=NAME size=27 value=\"$tmpset(NAME)\"></td>
          <td style=\"border: 0px solid black;\"></td>
          <tr><td style=\"border: 0px solid black;\">Time zone</td><td style=\"border: 0px solid black;\">[tzselect $tmpset(TZ)]</td> 
          <td rowspan=2 style=\"border: 0px solid black;\">Example:<br><span id=time></span><!--[format_time [clock format [clock seconds] -gmt 1]]--><br>
          <a href=http://www.tcl.tk/man/tcl8.5/TclCmd/clock.htm#M26>format help</a></td></tr> 
          <tr><td style=\"border: 0px solid black;\">Time format</td>
          <td style=\"border: 0px solid black;\"><input type=text name=TF size=27 value=\"$tmpset(TF)\" id=timeinput></td></tr>
          <tr><td style=\"border: 0px solid black;\">Filter HTML</td>
          <td style=\"border: 0px solid black;\"><input type=checkbox name=FILTER_HTML [expr {$tmpset(FILTER_HTML) ? "checked" : ""}]></td></tr>
          <tr><td valign=top style=\"border: 0px solid black;\">HTML whitelist</td>
          <td style=\"border: 0px solid black;\"><textarea cols=25 rows=3 name=HTML_WHITELIST>$tmpset(HTML_WHITELIST)</textarea></td></tr>
          <tr><td style=\"border: 0px solid black;\">Allow anon create</td>
          <td style=\"border: 0px solid black;\"><input type=checkbox name=ANON_CREATE [expr {$tmpset(ANON_CREATE) ? "checked" : ""}]></td></tr>
          <tr><td style=\"border: 0px solid black;\">Default permissions</td><td style=\"border: 0px solid black;\"><select name=PROTECT>"
    foreach val {5 10 15 20 25} name [list "All Read/Write" "Anon Read-only" "Users Only" "User Read-only" Privileged] {
        puts -nonewline "<option value=$val [expr {$val == $tmpset(PROTECT) ? " selected" : ""}]>$name"
    }
    puts "</select></td></tr>
        <tr><td style=\"border: 0px solid black;\">Login expires</td><td style=\"border: 0px solid black;\"><input type=text name=EXPIRES size=27 value=\"$tmpset(EXPIRES)\"></td></tr>
        <tr><td style=\"border: 0px solid black;\">Recaptcha privkey</td><td style=\"border: 0px solid black;\"><input type=text name=RECAPTCHA_PRIVATE_KEY size=27 value=\"$tmpset(RECAPTCHA_PRIVATE_KEY)\"></td></tr>
        <tr><td style=\"border: 0px solid black;\">Recaptcha pubkey</td><td style=\"border: 0px solid black;\"><input type=text name=RECAPTCHA_PUBLIC_KEY size=27 value=\"$tmpset(RECAPTCHA_PUBLIC_KEY)\"></td></tr>"

    puts "
<tr><td style=\"border: 0px solid black;\"></td><td align=center style=\"border: 0px solid black;\">
<br>
<input type=submit value=Save style=\"padding-left: 1em; padding-right: 1em;\">
          <input type=button name=cancel value=Cancel  style=\"margin-left: 2em;\" onclick=\"javascript:history.go(-1);\"></td>
</tr>
</table><br><center>
          <td valign=top style=\"border: 0px solid black; padding-left: 3em;\"></td></tr></table> 
          </form>";#</body></html>
set secs [clock seconds]
puts "<script>\nvar vals = \{"
foreach x {a A b B c C d D e h H k l m M N p P r R S T u y Y z} {
    puts "\"$x\": \"[clock format $secs -format %$x]\","
}
puts "\"Z\": \"[clock format $secs -format %Z]\"\n\};\n"

puts "
var n = document.getElementById('time');
var input = document.getElementById('timeinput');

function update_example() {
  while (n.firstChild) {
    n.removeChild(n.firstChild);
  }
  var str = input.value.replace(/%(.)/g, clockFormat);
  n.appendChild(document.createTextNode(str));
}

function clockFormat(str, chr) {
  return vals\[chr] || str;
}

input.onkeyup = update_example;
update_example();
</script>
"

}

# handles the post of the wiki config page
# input: form postdata
# returns: nothing
proc saveconfig {} {
    if {![authorized wiki config]} { no_auth }
    get_post input
    # checkboxes dont submit any data when not checked, so keep a list of them
    set checkboxes {FILTER_HTML ANON_CREATE}
    # dont trust the form just look for known valid options. this list must be kept in sync with editconfig and the db
    foreach x {NAME TZ TF FILTER_HTML HTML_WHITELIST ANON_CREATE EXPIRES RECAPTCHA_PRIVATE_KEY RECAPTCHA_PUBLIC_KEY} {
        if {[info exists input($x)]} {
            if {$x == "TZ" && [catch {clock format [clock seconds] -timezone $input($x)} err]} {
                lappend status "Invalid timezone"
                continue
            }
            if {$x == "EXPIRES" && ([catch {set tmp [clock scan $input($x)]}] || $tmp < [clock seconds])} {
                lappend status "Invalid expiration"
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
    if {$::request(USER_AUTH)} {
        puts "<tr><td style=\"border: 0px;\">Password</td><td style=\"border: 0px;\"><input type=password name=password size=27><input type=hidden name=user value=\"$::request(USER)\"></td></tr>"
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
    source packages/md5/md5x.tcl
    return [md5::md5 -hex $pass]
}

# handles the post of the user preferences page
# input: form postdata
# returns: nothing
proc saveprefs {} {
    get_post input
    db function password {hash_pass}
    if {[info exists input(password)] && $input(password) != "" && [info exists input(user)] && $input(user) != ""} {
        if {![authorized user password $input(user)]} { no_auth }
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
    db eval {rollback transaction}
}

# show the difference between 2 revisions
# <from> may be "curr" for the current node contents
# returns: nothing
proc showdiff {node from to} {
    if {![db exists {select id from history where original=$node limit 1}]} { http_error 404 "no such node" }
    db eval {select name from nodes where id=$node} {}

    if {[string match c* [string tolower $from]]} {
        if {[info exists name]} { 
            set from $name
        } else {
            set from [db onecolumn {select count(id)-1 from history where original=$node order by created limit 1}]
        }
    }

    if {[info exists name] && $from == $name} {
        db eval {select id,content,modified as created,modified_by as created_by from nodes where id=$node} from_data {}
        set from [db onecolumn {select count(id) from history where original=$node}]
        set link "[link node:$node "Revision $from"] (current) modified by $from_data(created_by) on [format_time $from_data(created)]"
    } else {
        db eval {select id,created as created,created_by from history where original=$node order by created limit 1 offset $from} from_data {}
        if {![info exists from_data(created)]} { http_error 404 "no such rev $from" }
        set from_data(content) [get_history_content $from_data(id)]
        set link "[link history:$node:$from "Revision $from"] modified by $from_data(created_by) on [format_time $from_data(created)]"
    }
    db eval {select id,created,created_by from history where original=$node order by created limit 1 offset $to} to_data {}
    if {![info exists to_data(created)]} { http_error 404 "no such rev $to" }

    if {![info exists name]} { set name "deleted node $node" }

    http_header
    html_head "Diffing $name"
    set to_data(content) [filter_html [get_history_content $to_data(id)]]
    set from_data(content) [filter_html $from_data(content)]
    append link "&nbsp;&nbsp;&nbsp;&nbsp;[link history:$node:[expr {$from - 1}]:$to <]&nbsp;[link history:$node:[expr {$from + 1}]:$to >]"
    puts "[link history:$node:$to "Revision $to"] modified by $to_data(created_by) on [format_time $to_data(created)]
        &nbsp;&nbsp;&nbsp;&nbsp;[link history:$node:$from:[expr {$to - 1}] <]&nbsp;[link history:$node:$from:[expr {$to + 1}] >]<br>
        $link<br>\n<hr>
        <pre>[format_diff [diff [split $from_data(content) \n] [split $to_data(content) \n]] $to_data(content)]</pre>
        <br><hr>"
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
                 set r [expr {$s+1}]
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

# outputs the changed lines with the styles applied
proc format_diff {diff orig} {
    set orig [split $orig \n]
    set new $orig
    set removed [lindex $diff 0]
    set added [lindex $diff 1]
    set aspan "<span style=\"background: #88ee88;\">"
    set rspan "<span style=\"background: #ee8888;\">"

    foreach x $removed {
        set new [lreplace $new $x $x "$rspan[lindex $new $x]</span>"]
    }

    set add_count 0
    foreach {idx text} $added {
        foreach x $removed {
            if {$x+$add_count > $idx} { break }
            incr idx
        }
        incr add_count
        set new [linsert $new $idx "$aspan$text</span>"]
    }
    return [join $new \n]
}

proc create_history_entry {id} {
    db eval {select content,modified,modified_by from nodes where id=$id} {}
    set type full
    #db eval {select id as lastfullid,content as lastfull from history where original=$id and type='full' order by id desc limit 1} {}
    #if {[info exists lastfull] && [db onecolumn {select count(id) from history where original=$id and id>$lastfullid}] < 20} {
    #    set diff [diff [split $lastfull \n] [split $content \n]]
    #    if {[llength [join $diff]] <= 45} {
    #        set type diff
    #        set content $diff
    #    }
    #}
    db eval {insert into history (original,type,content,created,created_by) values($id,$type,$content,$modified,$modified_by)}
}

proc get_history_content {id} {
    db eval {select original,content,type from history where id=$id} {}
    if {$type == "full"} {
        return $content
    }
    db eval {select content as lastfull from history where original=$original and type='full' and id<$id order by id desc limit 1} {}

    set lastfull [split $lastfull \n]
    foreach x [lreverse [lindex $content 0]] {
        set lastfull [lreplace $lastfull $x $x]
    }
    foreach {idx text} [lindex $content 1] {
        set lastfull [linsert $lastfull $idx $text]
    }
    return [join $lastfull \n]
}

proc diff {lines1 lines2} {
    set added [list]
    set removed [list]
    set i 0
    set j 0
    foreach { x1 x2 } [longestCommonSubsequence $lines1 $lines2] {
        foreach p $x1 q $x2 {
            while { $i < $p } {
                lappend added $i [lindex $lines1 $i]
                incr i
            }
            while { $j < $q } {
                lappend removed $j
                incr j
            }
            incr i
            incr j
        }
        while { $i < [llength $lines1] } {
            lappend added $i [lindex $lines1 $i]
            incr i
        }
        while { $j < [llength $lines2] } {
            lappend removed $j
            incr j
        }
    }
    return [list $removed $added]
}

# authenticate and authorize a request
# input: AUTH cookie and action to authorize
# returns: the users level
proc authenticate {} {
    global cookies request
    if {[info exists cookies(AUTH)]} {
        set key $cookies(AUTH)
        set ip $request(REMOTE_ADDR)
        db eval {select cookies.user,users.level from users,cookies where cookies.key=$key and cookies.expires>datetime('now') and cookies.ip=$ip and cookies.user=users.user} {}
    }
    # if no valid cookie found, then set user as anonymous
    if {![info exists user]} {
        # expire an invalid auth cookie if it exists
        if {[info exists cookies(AUTH)]} { set_cookie AUTH "" "5 days ago" }
        set request(USER_AUTH) 0
        set request(USER) anonymous
        set user anonymous
        set level 5
    } else {
        set request(USER_AUTH) 1
        set request(USER) $user
    }
    # check the db for ip address matches
    db eval {select user as huser,ip,level as hlev from users where ip != '' and (user=$user or user='' or user='*')} {
        if {[match_ip $ip $request(REMOTE_ADDR)]} {
           # if the username is an exact match then set the new ip dictated level
           # otherwise we will only lower the level
           if {$huser == $user} {
               set level $hlev; break
           } elseif {$hlev < $level} {
               set level $hlev; break
           }
        }
    }
    if {$level < 5} { http_error 403 Forbidden }
    set request(USER_LEVEL) $level
}

proc authorized {entity action {target {}}} {
    set reqlevel 30
    switch -glob -- $entity:$action {
        wiki:config { set reqlevel 30 }
        user:password {
            if {$::request(USER) != $target} {
                set reqlevel 25
                set tlev [db onecolumn {select level from users where user=$target}]
                if {$tlev != "" && $tlev >= 30} { set reqlevel 30 }
            } else {
                set reqlevel 10
            }
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
        node:wikitag { set reqlevel 25 }
        node:edit {
            set reqlevel [db onecolumn {select protect from nodes where id=$target}]
        }
        node:view {
            if {$target == ""} {
                # if target is empty, return the highest node level the user may view
                return [expr {$::request(USER_LEVEL) + 5}]
            }
            set reqlevel [db onecolumn {select protect from nodes where id=$target}]
            if {$reqlevel == ""} {
                # nonexistent or deleted node
                set reqlevel 25
            } elseif {$reqlevel < 20 && $reqlevel >= 10} {
                # nodes protect level is for editing, for viewing lower it by 5 unless its privileged
                incr reqlevel -5
            }
        }
        *:create {
            set reqlevel 10
            if {$::settings(ANON_CREATE) > 0} { set reqlevel 5 }
        }
    }
    return [expr {$::request(USER_LEVEL) >= $reqlevel}]
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

proc http_error {code {text {An error occured}}} {
    puts "Status: $code"
    puts "Content-type: text/html\n"
    puts "<html><head><title>Error $code</title></head><body>$text</body></html>"
    close_databases
    exit
}

# handles the post of the login form
# input: form postdata
# returns: nothing
proc do_login {} {
    get_post input
    db function password {hash_pass}
    if {![info exists input(username)] || ![info exists input(password)]} {
        location wiki:login
        db eval {rollback transaction}
        return
    }
    set user $input(username)
    set pass $input(password)
    set ip $::request(REMOTE_ADDR)
    if {![db exists {select user from users where user=$user and password=password($pass) and password<>''}]} {
        location wiki:login?message=incorrect%20username%20or%20password[expr {[info exists input(href)] ? "&href=$input(href)" : ""}]
        db eval {rollback transaction}
        return
    }
    set_auth_cookie $user [expr {[info exists input(remember)] && $input(remember) == "on" ? $::settings(EXPIRES) : ""}]
    db eval {commit transaction}
    location [expr {[info exists input(href)] ? "$input(href)" : ""}]
}

proc set_auth_cookie {user expires} {
    # create random 32 char auth token
    set chars abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
    set size [string length $chars]
    for {set i 0} {$i < 32} {incr i} {
        append key [string index $chars [expr {int(rand() * $size)}]]
    }

    set_cookie AUTH $key $expires
    if {$expires == ""} { set expires "2 days" }
    set ip $::request(REMOTE_ADDR)
    set expires [clock format [clock scan $expires] -format "%Y-%m-%d %H:%M:%S" -gmt 1]
    db eval {delete from cookies where user=$user and (expires<datetime('now') or ip=$ip)}
    db eval {insert into cookies (user,ip,key,created,expires) values($user,$ip,$key,datetime('now'),$expires)}
}

# sets an http cookie
# input: key value pair, and expiration date in any format tcl recognizes
proc set_cookie {key val expires} {
    set cookie "$key=$val; PATH=[file dirname $::request(PATH_INFO)];"
    if {$expires != ""} {
        append cookie "EXPIRES=[clock format [clock scan $expires] -format "%a, %d-%b-%Y %H:%M:%S" -gmt 1];"
    }
    puts "Set-Cookie: $cookie"
}

# logs a user out by deleting any auth tokens from the db and expiring the auth cookie
proc logout {} {
    if {!$::request(USER_AUTH)} {
        location wiki:login
        return
    }
    set ip $::request(REMOTE_ADDR)
    set user $::request(USER)
    db eval {delete from cookies where user=$user and ip=$ip}
    set_cookie AUTH "" "5 days ago"
    location wiki:login
}

# show the login request page
# input: optional query string "message"
# returns: nothing
proc login {} {
    http_header
    html_head "Login"
    get_input input
    set user ""

    if {[info exists input(message)]} {
        puts "<span style=\"color:red;\">[filter_html $input(message)]</span><br><br>"
    }
    if {$::request(USER_AUTH)} {
        puts "You are already logged in as user &quot;$::request(USER)&quot;<br>You may switch users by logging in again below.<br>If this is the correct user then it is likely you do not have sufficient privileges for the action you were trying to perform<br>"
    } elseif {$::request(USER) != "anonymous"} {
        set user $::request(USER)
    }
    puts "<form action=\"[myself]/login\" method=post>
         <table style=\"border: 0px;\"><tr><td style=\"border: 0px;\">Username:</td>
         <td style=\"border: 0px;\"><input name=username id=username value=\"$user\"></td></tr>
         <tr><td style=\"border: 0px;\">Password:</td>
         <td style=\"border: 0px;\"><input type=password name=password></td></tr>"
    if {[info exists input(href)]} { puts "<input type=hidden name=href value=\"$input(href)\">" }
    puts {<tr><td align=left style="border: 0px; font-size: 60%;"><input type=checkbox name=remember checked> Remember me</td><td align=center style="border: 0px;"><input type=submit value="Log In"></td></tr></table></form>}
    puts {<script>username = document.getElementById('username'); if (username.value == "") { username.focus(); } else { document.getElementById('password').focus(); }</script>}
    puts {</body></html>}
}

# display a node
# input: a node id
# returns: nothing
proc showpage {id} {
    # select some extra columns so that [dynamic_variable] can upvar them during dynamic parsing
    db eval {select name,parsed,modified,modified_by,created from nodes where id=$id} {}
    if {![info exists name]} { http_error 404 "no such node" }
    if {![authorized node view $id]} { no_auth }
    http_header
    html_head $name
    set page [db onecolumn {select parsed from nodes,tags where tags.name='wiki:header' and tags.node=nodes.id order by nodes.modified desc limit 1}]
    append page $parsed
    append page [db onecolumn {select parsed from nodes,tags where tags.name='wiki:footer' and tags.node=nodes.id order by nodes.modified desc limit 1}]
    set time [time {set done [parse_dynamic $id $page]}]
    puts $done
    puts "<!-- dynamic parse time: $time -->"
    db eval {select content from nodes,tags where tags.name='wiki:script' and tags.node=nodes.id order by nodes.name} {
        puts "<script>\n$content\n</script>\n"
    }
    puts "</body></html>"
}

# display the search input page
# input: none, optional query string "q" which will display results page instead
# returns: nothing
proc search {} {
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
        db eval {rollback transaction}
        return
    }

    set level [authorized node view]
    set matches {}

    # for tag searches need to remove boolean operators and negated terms to avoid suprious results,
    # and single quotes to prevent sql injection
    set tagterms {}
    foreach x [split $input(string)] {
        if {$x == "" || $x == "OR" || [string match -* $x]} { continue }
        lappend tagterms '[string map {' ""} [string tolower $x]]'
    }
    set numterms [llength $tagterms]

    set results ""
    set term [string map {\" &quot;} [filter_html $input(string)]]
    append results "<h3>Tag results</h3>"
    db eval "select nodes.id as id,nodes.name,tags.node,count(tags.name) from tags,nodes where tags.name in ([join $tagterms ,]) and tags.node=nodes.id and nodes.protect<=$level group by tags.node order by count(tags.name) desc" {
        if {$numterms == $count(tags.name)} { lappend matches $id }
        append results "[link node:$node $name]&nbsp;&nbsp;&nbsp;<span class=tags>("
        db eval {select name from tags where node=$id} {
            append results "[link tag:$name $name] "
        }
        append results ")</span><br>"
    }

    append results "<br><h3>Full text results</h3>"
    fts eval {select id,name,snippet(search) as snippet from search where content match $input(string) and protect<=$level} {

        append results "[link node:$id $name]"
        if {[set tags [db eval {select name from tags where node=$id}]] != ""} {
            append results "&nbsp;&nbsp;&nbsp;<span class=tags>( "
            foreach x $tags { append results "[link tag:$x $x] " }
            append results ")</span>"
        }
        append results "<br>$snippet<br><br>"
        lappend matches $id
    }

    if {[llength $matches] != 1} {
        http_header
        html_head "Search results"
        puts "<h1>Search results</h1><br>"
        puts "<form name=search method=post action=[myself]/search>Searched for <input name=string value=\"$term\" size=30> <input type=submit value=\"Search Again\"></form>"
    }

    if {$::request(USER) != "anonymous" && !$::request(USER_AUTH)} {
        puts "You are not logged in. It is likely you will see more results if you [link wiki:login "log in"]"
    } elseif {[llength $matches] == 1} {
        location node:[lindex $matches 0]
        db eval {rollback transaction}
        return
    }

    if {[llength $matches] == 0} {
        # no results
        puts "No pages matched your search terms"
    } else {
        puts $results
    }
    db eval {rollback transaction}
}

# shows a page listing all nodes tagged with a given tag
# input: an exact tag name
# returns: nothing
proc showtag {tag} {
    set level [authorized node view]
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
    if {![authorized node view $nodeid]} { no_auth }
    if {[string match *:* $nodeid]} {
        set nodeid [split $nodeid :]
        if {[llength $nodeid] == 2} {
            eval viewhistory $nodeid
        } elseif {[llength $nodeid] == 3} {
            eval showdiff $nodeid
        } else {
            http_error 500 "invalid arguments"
        }
        return
    }

    db eval {select name,tf(modified) as modified,modified_by,content as currcontent from nodes where id=$nodeid} {}
    if {![info exists name] && ![db exists {select original from history where original=$nodeid limit 1}]} {
        http_error 404 "no such node"
    }
    set currev [db onecolumn {select count(id) from history where original=$nodeid}]
    get_input a
    http_header
    html_head "History for node $nodeid"
    puts "<h1>Revision history for node $nodeid</h1><br>"
    if {$currev == 0} {
        puts "no changes have been made to this node: [link node:$nodeid $name]"
        return
    }

    pagination a 200 history:$nodeid $currev
    unset -nocomplain ::lastfull
    set history [list]
    set rev [expr {$currev - $offset}]
    db eval {select type,created,created_by,content from history where original=$nodeid order by created desc limit $perpage+1 offset $offset} {
        incr rev -1
        lappend history [list $rev [format_time $created] $created_by $type $content]
    }

    if {![info exists name]} {
        puts "Node is deleted<br><br>"
    } else {
        puts "Current name &quot;[link node:$nodeid $name]&quot;<br><br>"
    }
    puts "<table style=\"border: 0px; padding: 0px;\">$nav<tr><td style=\"border: 0px; padding: 0px;\" colspan=3>"
    puts "<table class=wikilist>[th x {Rev 0} {Created 0} {By 0} {"Line &#916" 0} {Compare 0}]"

    if {$page == 0} {
        if {![info exists name]} {
            puts "<tr><td align=center>-</td><td>[lindex $history 0 1]</td><td>[lindex $history 0 2]</td>
                <td align=center>-[linechange 0 $history]</td>
                <td align=right>[link history:$nodeid:[lindex $history 0 0]:[lindex $history 1 0] prev]</td></tr>"
            set history [lrange $history 1 end]
        } else {
            puts "<tr><td align=center>[link node:$nodeid $currev]</td><td>$modified</td><td>$modified_by</td>
                <td align=center>[linechange 0 [linsert $history 0 [list {} {} {} full $currcontent]]]</td>
                <td align=right>[link history:$nodeid:C:[lindex $history 0 0] prev]</td></tr>"
        }
    }

    set next 0
    foreach x [lrange $history 0 end-1] {
        incr next
        foreach {rev created created_by} $x break
        puts "<tr><td align=center>[link history:$nodeid:$rev $rev]</td><td>$created</td><td>$created_by</td>
            <td align=center>[linechange [expr {$next - 1}] $history]</td>
            <td>[link history:$nodeid:C:$rev current] [link history:$nodeid:$rev:[lindex $history $next 0] prev]</td></tr>"
    }

    if {$page == $lastpage} {
        puts "<tr><td align=center>[link history:$nodeid:[lindex $history end 0] [lindex $history end 0]]</td><td>[lindex $history end 1]</td>
            <td>[lindex $history end 2]</td><td align=center><b>[linechange $next [linsert [lrange $history end end] end [list {} {} {} full {}]]]</b></td>
            <td>[link history:$nodeid:C:[lindex $history end 0] current]</td></tr>"
    }
    puts "</table></td></tr>$nav</table></body></html>"
}

proc linechange {i list} {
    global lastfull
    global lines
    if {![info exists lastfull] || $i >= $lastfull} {
        set lastfull [lsearch -exact -index 3 -start $i $list full]
        set lines [expr {[llength [regexp -all -inline {[^\n]\n} [lindex $list $lastfull 4]]] + 1}]
    }
    if {$lastfull == $i} {
        set a $lines
    } else {
        set diff [lindex $list $i 4]
        set a [expr {$lines - [llength [lindex $diff 0]] + ([llength [lindex $diff 1]] / 2)}]
    }

    if {$lastfull > $i+1} {
        set diff [lindex $list $i+1 4]
        set b [expr {(-1 * [llength [lindex $diff 0]]) + ([llength [lindex $diff 1]] / 2) + $lines}]
    } elseif {$lastfull == $i+1} {
        set b $lines
    } else {
        set lastfull [lsearch -exact -index 3 -start [expr {$i + 1}] $list full]
        set lines [expr {[llength [regexp -all -inline {[^\n]\n} [lindex $list $lastfull 4]]] + 1}]
        set diff [lindex $list $i+1 4]
        set b [expr {$lines + 1 - [llength [lindex $diff 0]] + ([llength [lindex $diff 1]] / 2)}]
    }

    return [expr {$a - $b}]
}

proc viewhistory {node rev} {
    db eval {select id,created from history where original=$node order by created limit 1 offset $rev} {}
    if {![info exists created]} { http_error 404 "no such rev" }
    http_header
    html_head "History for node $node"
    set revs [list [expr {$rev - 1}] $rev [expr {$rev + 1}]]

    db eval {select name from nodes where id=$node} {}
    if {![info exists name]} {
        puts "Contents of deleted node $node created [format_time $created]"
    } else {
        puts "Contents of &quot;[link node:$node $name]&quot; created [format_time $created]"
    }
    puts "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[link history:$node:[lindex $revs 0] <] [link history:$node "(rev [lindex $revs 1])"] [link history:$node:[lindex $revs 2] >]"
    puts "&nbsp;&nbsp;&nbsp;&nbsp;[link history:$node:[expr {$rev - 1}]:$rev "compare to previous"]"
    puts "<br><hr>"
    # its a good idea but parsing has side effects
    #get_input a
    #if {[info exists a(formatting)]} {
    #    puts "[parse_dynamic $node [parse_static $node [get_history_content $id]]]"
    #}
    set content [string map {& &amp;} [get_history_content $id]]
    puts "<pre>[filter_html $content]</pre><hr>"
}

proc deletefile {id} {
    if {![authorized file delete $id]} { no_auth }
    db eval {select filename from files where id=$id} {}
    catch {file delete $filename}
    db eval {delete from files where id=$id}
    db eval {commit transaction}
    http_header
    puts "<body onload=javascript:history.go(-2);>"
}

proc showfile {id} {
    db eval {select name,filename,original_name,tf(created) as created,tf(modified) as modified from files where id=$id} {}
    if {![info exists name]} { http_error 404 "no such file" }
    http_header
    html_head "File Information: $name"
    lappend ::auto_path packages
    puts "<script type=\"text/javascript\">
        function confirmation() {
	if (confirm(\"Delete file\\n$name?\")) { document.forms\[0].action = \"[myself]/delete:$id\"; document.forms\[0].submit(); }
        }
        </script>"
    set filepath [filepath $filename]
    if {[file exists $filepath]} {
        lappend left Size:; lappend right [format_filesize [file size $filepath]]
        if {[regexp -nocase {\.(jpe?g)$} $filename] && ![catch {package require jpeg}]} {
            lappend left Dimensions:; lappend right [join [jpeg::dimensions $filepath] x]
            array set exif [jpeg::getExif $filepath] 
            if {[info exists exif(DateTime)]} { lappend left Taken:; lappend right $exif(DateTime) }
            if {[info exists exif(Make)] && [info exists exif(Model)]} { lappend left Camera:; lappend right "$exif(Make) $exif(Model)" }
        } elseif {[regexp -nocase {\.(png)$} $filename] && ![catch {package require png}] && ![catch {package require crc32}]} {
            array set imginfo [png::imageInfo $filepath]
            lappend left Image:; lappend right "$imginfo(width)x$imginfo(height) $imginfo(depth)bpp"
            if {[set ts [png::getTimestamp $filepath]] != ""} { lappend left Timestamp:; lappend right $ts }
        }
        set info "<div style=\"float: left; padding-right: 1em;\">[join $left <br>\n]</div><div>[join $right <br>\n]</div>"
    } else {
        set info "<i>file not found</i>"
    }
    puts "<h1>File Information: $name</h1><br><br>
         <form method='POST' enctype='multipart/form-data' action='[myself]/upload:$id'>
         <table><tr><td>Name:</td><td><input type=text name=name value=\"$name\" size=60></td></tr>
         <tr><td>Filename:</td><td><a href=\"files/$filename\">$filename</a></td></tr>
         <tr><td>Original name:</td><td>$original_name</td></tr>
         <tr><td>Created:</td><td>$created</td></tr>
         <tr><td>Modified:</td><td>$modified</td></tr>
         <tr><td valign=top>Info:</td><td>$info</td></tr>
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
        puts "<br><br><center><img src=\"files/$filename\"></center>"
    }
}

# displays node tagged with wiki:<name>
# input: tag name
# returns: nothing
proc wikitag {name} {
    set name wiki:$name
    db eval {select id from nodes,tags where tags.name=$name and tags.node=nodes.id order by nodes.modified desc limit 1} {}
    if {[info exists id]} {
        showpage $id
        return
    }
    if {$name == "wiki:home"} {
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
    set level [authorized node view]
    http_header
    html_head "Tag list"
    if {[info exists a(sort)]} { set sort $a(sort) }
    sortable sort order {tag links} {"tags.name" "c desc"}

    puts "<h1>Tag list</h1><br><table class=wikilist>[th $sort {Tag 1} {Links 1}]"
    db eval "select tags.name as name,count(tags.node) as c from tags,nodes where nodes.protect<=$level and tags.node=nodes.id and tags.name not like 'wiki:%' group by tags.name order by $order" {
        puts "<td>[link tag:$name $name]<td align=center>$c</td></tr>"
    }
    puts "</tr></table>"
}

proc params {base input_var args} {
    upvar $input_var input
    array set params [array get input]
    foreach {name val} $args { set params($name) $val }
    set tmp [list]
    foreach {name val} [array get params] {
        if {$val == ""} { continue }
        lappend tmp $name=$val
    }
    if {[llength $tmp] > 0} { append base ?[join $tmp &] }
    return $base
}

proc sortable {sort_var order_var valid sql} {
    upvar $sort_var sort
    upvar $order_var order
    set default 0
    if {[info exists sort] && [set i [lsearch -exact $valid $sort]] >= 0} {
        set default $i
    }
    set order [lindex $sql $default]
    set sort [lindex $valid $default]
}

proc pagination {input per link total} {
    set fuzzy 0.1
    upvar perpage perpage
    upvar $input a
    upvar page page
    upvar offset offset
    upvar lastpage lastpage
    upvar nav nav

    set perpage $per
    set page 0
    if {$total <= 0} { set total 1 }
    if {[info exists a(page)] && [string is integer -strict $a(page)]} { set page $a(page) }
    if {[info exists a(perpage)] && [string is integer -strict $a(perpage)]} { set perpage $a(perpage) }
    set lastpage [expr {int(ceil(double($total) / $perpage)) - 1}]
    set overflow [expr {double($total % $perpage) / $total}]
    if {$overflow < $fuzzy} { incr lastpage -1 }
    if {$page > $lastpage} { set page $lastpage }
    set offset [expr {$page * $perpage}]
    if {$overflow < $fuzzy && $page == $lastpage} { set perpage $total }

    if {$lastpage > 0} {
        set nav "<tr>
            <td class=pagination_nav id=prev>[expr {$page > 0 ? [link [params $link a page [expr {$page - 1}]] "&#9668; prev"] : ""}]</td>
            <td class=pagination_nav id=page>[expr {$page + 1}] / [expr {$lastpage + 1}]</td>
            <td class=pagination_nav id=next>[expr {$page < $lastpage ? [link [params $link a page [expr {$page + 1}]] "next &#9658;"] : ""}]</td>
            </tr>"
    } else {
        set nav ""
    }
}

proc nodelist {} {
    set level [authorized node view]
    get_input a
    http_header
    html_head "Node list"

    set perm { 10 R/O 15 User 20 "User R/O" 25 Priv 5 R/W }
    if {[info exists a(sort)]} { set sort $a(sort) }
    sortable sort order {name created modified perms} {"lower(name)" "created desc" "modified desc" protect}
    pagination a 200 wiki:nodes [db onecolumn {select count(id) from nodes}]

    puts "<h1>Node list</h1><br><br>
         <table style=\"border: 0px; padding: 0px;\">$nav<tr><td style=\"border: 0px; padding: 0px;\" colspan=3>
         <table class=wikilist>[th $sort {Name 1} {Created 1} {Modified 1} {Perms 1} {Links 0} {Tags 0}]"
    db eval "select id,name,created,modified,protect from nodes where protect<=$level and id not in (select distinct node from tags where name='wiki:hide') order by $order limit $perpage offset $offset" {
        puts "<tr><td>[link node:$id $name]</td><td>[format_time $created]</td><td>[format_time $modified]</td><td align=center>[string map $perm $protect]</td>"
        puts "<td align=center>[link links:$id [db eval {select count(node) from links where target=$id and type='node'}]]</td><td>"
        db eval {select name from tags where node=$id} {
            puts "[link tag:$name $name] "
        }
        puts "</td></tr>"
    }
    puts "</table></td></tr>$nav</table>"
}

proc th {sort args} {
    set out "<tr>"
    foreach {name sortable} [join $args] {
        if {$sortable} {
            set param [string tolower [string map {" " ""} $name]]
            if {$param == $sort} {
                append out "<th id=sorted>$name</th>"
            } else {
                append out "<th><a href=\"?sort=$param\">$name</a></th>"
            }
        } else {
            append out "<th>$name</th>"
        }
    }
    append out "</tr>"
    return $out
}

proc filelist {} {
    db function fsize {file_size}
    get_input a
    http_header
    html_head "File list"

    pagination a 200 wiki:files [db onecolumn {select count(id) from files}]
    if {[info exists a(sort)]} { set sort $a(sort) }
    sortable sort order {name created modified size} {"lower(name)" "created desc" "modified desc" "fsize(filename) desc"}

    puts "<h1>File list</h1><br>"
    puts "<table style=\"border: 0px; padding: 0px;\">$nav<tr><td style=\"border: 0px; padding: 0px;\" colspan=3>
        <table class=wikilist>[th $sort {Name 1} {Filename 0} {Created 1} {Modified 1} {Size 1}]"
    db eval "select id,name,filename,created,modified from files order by $order limit $perpage offset $offset" {
        set filepath [filepath $filename]
        set size [expr {[file exists $filepath] ? [format_filesize [file size $filepath]] : ""}]
        puts "<tr><td>[link file:$id $name]</td><td><a href=\"files/$filename\">$filename</a></td>
             <td>[format_time $created]</td><td>[format_time $modified]</td><td>$size</td></tr>"
    }
    puts "</table></td></tr>$nav</table>"
}

proc file_size {file} {
    set file [filepath $file]
    if {![file exists $file]} { return 0 }
    return [file size $file]
}

# nicely formats a file size
# input: size in bytes
proc format_filesize {size} {
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
    if {![authorized file create]} { no_auth }
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
    if {![authorized file create]} { no_auth }
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
            db eval {rollback transaction}
            return
        }
        # fix up the filename: backslashes, "[1]", spaces, and trim the full path
        set tail [file tail [string map {\\ /} $original]]
        set tail [regsub {\[1\]\.} $tail .]
        if {$name == ""} { set name [file rootname $tail] }
        set base [filepath]
        set tail [string map {" " _} $tail]
        #set filename [file join $base $tail]
        #set num 1
        #while {[file exists $filename]} {
        #    set filename [file join $base [format %s%d%s [file rootname $base] $num [file extension $base]]]
        #    incr num
        #}
        # start with 0000 before filename, increment it until we get a unique name
        set num 0
        while {[file exists [set filename [format "%s/%04d%s" $base $num $tail]]]} { incr num }
    } else {
        # if its not new and there was no file, just update the name
        if {$filedata == ""} {
            db eval {update files set name=$name where id=$id}
            db eval {commit transaction}
            location file:$id
            return
        }
        # otherwise overwrite the existing file
        set filename [file join $base [db onecolumn {select filename from files where id=$id}]]
    }

    set fh [open $filename w]
    fconfigure $fh -encoding binary -translation lf
    puts -nonewline $fh $filedata
    close $fh
    set user "$::request(USER)@$::request(REMOTE_ADDR)"

    if {$id == "new"} {
        set filename [file tail $filename]
        db eval {insert into files (name,original_name,filename,created,created_by,modified,modified_by) values($name,$original,$filename,datetime('now'),$user,datetime('now'),$user)}
        db eval {commit transaction}
        location file:[db last_insert_rowid]
    } else {
        db eval {update files set modified=datetime('now'),modified_by=$user,name=$name where id=$id}
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

proc html_head {title args} {
    puts "<html><head>\n<title>$::settings(NAME)$title</title>"
    puts [join $args]
    db eval {select content as parsed from nodes,tags where tags.name='wiki:style' and tags.node=nodes.id order by nodes.modified desc limit 1} {
        #set parsed [string map {&#123; \{ &#125; \} <br> \n\n} $parsed]
        puts "<style>\n$parsed\n</style>\n"
    }
    #foreach x [glob -nocomplain auto_include/*.css] {
    #    puts "<link rel=stylesheet href=[myself]/$x>"
    #}
    #foreach x [glob -nocomplain auto_include/*.js] {
    #    puts "<script href=[myself]/$x></script>"
    #}
    puts "</head><body>"
}

# performs parsing of node contents at save time
# input: node id and contents
# returns: node content with static html
proc parse_static {id data} {
    set data [split $data \n]
    set state "NONE"
    set curblock [list]
    set blocks [list]
    foreach line $data {
        if {$state != "NONE"} {
            if {[check_end_sequence $state line]} {
                if {$state == "VAR" || $state == "SCRIPT" || $state == "PREHTML"} {
                    lappend curblock $line
                    state_changed state NONE curblock blocks
                    continue
                }
                state_changed state NONE curblock blocks
                #if {$line == ""} { continue }
                state_changed state [check_start_sequence $id $state line] curblock blocks
            }
        } elseif {$line != ""} {
            state_changed state [check_start_sequence $id $state line] curblock blocks
        }
        lappend curblock $line
        #puts "$state [string trimright $line]"
    }
    lappend blocks $state $curblock
    parse_blocks $id $blocks
}

proc check_start_sequence {id state l} {
    #puts "check starts"
    upvar $l line
    if {[string match {\?*: *} $line]} {
        set line [static_options $id tags $line]
    } elseif {[string match +*+ $line]} {
        set line [static_heading $line]
    } elseif {[regexp {^---+$} $line]} {
        set line <hr>

    } elseif {[string match "|*|" $line]} {
        return TABLE
    } elseif {[string match "%*\{" $line]} {
        return VAR
    } elseif {[string match "*<script*" $line]} {
        return SCRIPT
    } elseif {[regexp {^  *[*1]+ } $line]} {
        return LIST
    } elseif {[string match *<pre>* $line]} {
        return PREHTML
    } elseif {[string match "  *" $line]} {
        return PRE
    } elseif {[regexp {^\s*\+.*[^+]$} $line]} {
        set line [string range $line 1 end]
        return PARA
    }
    return $state
}

proc check_end_sequence {state l} {
    upvar $l line
    if { ($state == "PRE"     && [regexp {^\S} $line])          || \
         ($state == "PREHTML" && [string match *</pre>* $line]) || \
         ($state == "LIST"    && [regexp {^\S*$} $line])        || \
         ($state == "TABLE"   && ![string match "|*|" $line])   || \
         ($state == "PARA"    && [regexp {^ *$} $line])         || \
         ($state == "VAR"     && $line == "\}%")                || \
         ($state == "SCRIPT"  && $line == "</script>")
    } {
        return 1
    }
    return 0
}

proc state_changed {s new c b} {
    upvar $s state
    upvar $c curblock
    upvar $b blocks
    if {$new != $state} {
        if {$curblock != ""} {
            #puts "state changed: $state -> $new"
            lappend blocks $state $curblock
            set curblock [list]
        }
        set state $new
    }
}

proc parse_blocks {id blocks} {
    set output ""
    foreach {type block} $blocks {
        #puts $type
        #puts $block
        #continue
        set block [join $block \n]
        switch -exact -- $type {
            TABLE {
                set block [text_formatting $block]
                set block [static_links $id $block]
                append output [static_table $block]
            }
            PARA {
                set block [text_formatting $block]
                set block [static_links $id $block]
                append output "<p>$block</p>\n"
            }
            LIST {
                set block [text_formatting $block]
                set block [static_links $id $block]
                append output [static_lists $block]
            }
            VAR {
                append output $block
            }
            SCRIPT {
                append output $block
            }
            PRE {
                #set block [text_formatting $block]
                #set block [static_links $id $block]
                append output "<pre>\n$block\n</pre>\n"
            }
            PREHTML {
                #set block [text_formatting $block]
                #set block [static_links $id $block]
                append output $block
            }
            NONE {
                set block [text_formatting $block]
                set block [static_links $id $block]
                set block [string map {\n\n\n\n "<br /><br /><br />\n" \n\n\n "<br /><br />\n" \n\n "<br />\n"} $block]
                append output $block
            }
        }
    }
    if {$::settings(FILTER_HTML)} {
        set output [filter_white_html $output]
    }
    return $output
}

proc static_links {id data} {
    foreach x [lreverse [regexp -all -inline -indices -nocase {%[a-z]*?%} $data]] {
        set var [string trim [string range $data {*}$x] %]
        set t [time {set data [string replace $data {*}$x [static_variable $var]]}]
    }
    foreach {x junk} [lreverse [regexp -all -inline -indices {(?:[\s\|^])((?:\([^\(\)]+\):)?[a-z]{3,7}://[^[:space:]\"\n<]+)} $data]] {
        set url [string range $data {*}$x]
        set data [string replace $data {*}$x [static_http $url]]
    }
    foreach {linkid target name all} [lreverse [regexp -all -inline -indices {\m([a-z]{3,7}):(?:\(([^\)]+)\)|([[:digit:]]+))} $data]] {
        if {$target == "-1 -1"} { set target $linkid }
        set res [static_call $id [string range $data {*}$name] [string range $data {*}$target]]
        set data [string replace $data {*}$all $res]
    }

    return $data
}

proc text_formatting {data} {
    # _italic_
    set data [regsub -all {\m_(?!\s)([^_]*[^_ ])_(?![[:alnum:]])} $data {<i>\1</i>}]

    # *bold*
    set data [regsub -all -linestop  {\Y\*(?!\s\")([^\*]*[^\* ])\*(?![[:graph:]])} $data {<b>\1</b>}]

    # =fixed=
    set data [regsub -all -linestop  {\Y=(?!\s\")([^=]*[^= ])=(?![[:graph:]])} $data {<span class=fixed>\1</span>}]

    # -strikethrough-
    set data [regsub -all -linestop {\Y-(?!\s\")(.*?[^\- ])-(?![[:graph:]])} $data {<del>\1</del>}]

    return $data
}

proc static_table {data} {
    set t "\n<table>\n"
    foreach line [split $data \n] {
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
    switch -exact -nocase -- $var {
        TOC {
            upvar 2 blocks blocks
            set found [list]
            set mindepth 99
            foreach {type data} $blocks {
                if {$type != "NONE"} { continue }
                foreach {all link depth name} [regexp -all -inline {<a name=(.*?)><h(\d)>(.*?)<} [join $data]] {
                    set link [string trim $link \"']
                    set name [string trim $name]
                    #lappend headings "  [string repeat 1 $depth] <a href=#$link>$name</a>"
                    lappend found $depth $link $name
                    if {$depth < $mindepth} { set mindepth $depth }
                }
            }
            if {$found != ""} {
                set subtract [expr {1 - $mindepth}]
                foreach {depth link name} $found {
                    lappend headings "  [string repeat 1 [expr {$depth + $subtract}]] <a href=\"#$link\">$name</a>"
                }
                return "<div class=toc>\n[static_lists [join $headings \n]]</div>\n"
            }
        }
        BLUE -
        RED -
        GREEN -
        YELLOW -
        ORANGE -
        PURPLE -
        AQUA -
        NAVY -
        PINK -
        LIME -
        BROWN -
        GRAY -
        BLACK { return "<span style=\"color: [string tolower $var];\">" }
        END -
        ENDCOLOR { return "</span>" }
        default { return %$var% }
    }
}

proc static_heading {data} {
    set size [expr {[string length $data] - [string length [string trimleft $data +]]}]
    set data [string trim $data +]
    return "<a name=[string map {" " _} [string trim $data]]><h$size>$data</h$size></a>"
}

proc static_options {id opt data} {
    switch -exact -- $opt {
        tags {
            foreach x [lrange [split $data] 1 end] {
                if {$x == ""} { continue }
                if {[string match wiki:* $x] && ![authorized node wikitag]} { continue }
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
        if {[string length $data] > 60} {
           set name [lindex [split $data ?] 0]]
           if {[string length $name] > 60} {
               return "<a href=\"$data\">[string range $name 0 25]...[string range $name end-20 end]</a>"
           }
           return "<a href=\"$data\">$name</a>"
        }
        return "<a href=\"$data\">$data</a>"
    }
}

proc static_lists {data} {
    set data [split $data \n]
    if {[lindex $data 0] == ""} { set data [lrange $data 1 end] }

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
                append ret [string map {* </ul>\n 1 </ol>\n} [string range $last $i end]]
                append ret [string map {* <ul>\n 1 <ol>\n} [string range $now $i end]]
                set last $now
            }
            set line [lrange $line 1 end]
        }
        append ret <li>[string trim [join $line]]</li>\n
    }
    append ret [string map {* </ul>\n 1 </ol>\n} $last]
    return $ret
}

proc static_call {id cmd data} {
    switch -exact -- $cmd {
        node -
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
                return "<a href=\"[join [lrange $s 1 end] :]\">[lindex $s 0]</a>"
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
        image -
        img {
            regexp {(.*) (\d+x\d+|icon|small|med(?:ium)?|large)$} $data -> data tns
            if {[string is integer $data]} {
                db eval {select id as imgid,name,filename from files where id=$data} {}
            } elseif {![string match {*://*} $data]} {
                db eval {select id as imgid,name,filename from files where name=$data order by modified,created desc limit 1} {}
            } else {
                return "<img src=\"$data\" alt=\"$data\" />"
            }
            if {![info exists filename]} { return "<i>image not found</i>" }
            db eval {insert or ignore into links (node,type,target) values($id,'file',$imgid)}

            if {[info exists tns]} {
                set tns [string map {icon 32x32 small 100x100 med 200x200 medium 200x200 large 640x480} $tns]
                set path [filepath]
                set thumb $path/thumbs/[file rootname $filename]_$tns[file extension $filename]
                if {![file exists $thumb]} {
                    catch {exec /usr/local/bin/convert -scale $tns $path/$filename $thumb}
                }
                return "<a href=\"files/$filename\"><img src=\"files/thumbs/[file tail $thumb]\" alt=\"$name\" /></a>"
            }
            return "<img src=\"files/$filename\" alt=\"$name\" />"
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

proc filepath {{file {}}} {
    set path [join [lrange [split $::request(PATH_TRANSLATED) /] 0 end-1] /]/files
    if {$file != ""} { append path /$file }
    return $path
}

# performs parsing of node contents at display time
proc parse_dynamic {id data} {
    #return $data
    subst_commands $id data
    # escape special chars before subst
    set data [string map {[ \\[ \{ \\\{ \} \\\} $ \\$ \\ \\\\} $data]
    # %variables% - some of these are passed through static parsing
    set data [regsub -all -nocase {%([A-Z]{2,10})%} $data {[dynamic_variable {\1}]}]
    # link:(id) and file:(id)
    set data [regsub -all {([a-z]{3,10}):\(([^\(\)]+)\)} $data "\[call_dynamic $id \{\\1\} \{\\2\}]"]
    set data [subst $data]
    return $data
}

proc dynamic_variable {var} {
    switch -exact -nocase -- $var {
        MODIFIED { upvar 2 modified modified; return [format_time $modified] }
        MODIFIEDBY { upvar 2 modified_by modified_by; return $modified_by }
        CREATED { upvar 2 modified_by modified_by; return [format_time $created] }
        ID { upvar 2 id id; return $id }
        NAME { upvar 2 name name; return [filter_html $name] }
        PATH { return [string trimright [file dirname $::request(PATH_INFO)] /]/ }
        WIKI { return [myself] }
        WIKINAME { return $::settings(NAME) }
        TAGS {
            upvar 2 id id
            set out [list]
            db eval {select name from tags where node=$id} {
                lappend out [link tag:$name $name]
            }
            return [join $out]
        }
        USER { return $::request(USER) }
        USERNAME { return [lindex [split $::request(USER) @] 0] }
        USERADDRESS { return [lindex [split $::request(USER) @] 1] }
        LOGIN {
            if {$::request(USER_AUTH)]} {
                return "logged in as $::request(USER) - <a href=\"[myself]/wiki:logout\">logout</a>"
            } else {
                puts "<a href=\"[myself]/wiki:login\">login</a>"
            }
        }
        default { return $var }
    }
}

proc filewrapper {cmd args} {
    if {$cmd == "copy" || $cmd == "delete" || $cmd == "rename"} {
        return -code error "bad option"
    }
    return [eval [list file $cmd] $args]
}

proc procwrapper {i name myargs body} {
    if {[interp eval $i info commands $name] != ""} { return -code error "proc already exists" }
    interp invokehidden $i proc $name $myargs $body
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
    set level [authorized node view]
    interp alias $i file {} filewrapper
    interp alias $i sql {} interp invokehidden $i db eval
    interp alias $i puts {} append output
    foreach x {myself link format_filesize get_input filter_html unescape format_time db_auth} {
        interp alias $i $x {} $x
    }
    interp eval $i "
        load /usr/local/lib/sqlite3.6.17/libsqlite3.6.17.so Sqlite3
        sqlite3 db wiki.db -readonly 1
        db function tf format_time
        db eval \"create temp view pages as select * from nodes where protect<=$level\"
        db authorizer db_auth
    "
    interp eval $i [list array set request [array get ::request]]
    foreach x {load source open exec socket rename proc cd sqlite sqlite3 db interp} {
        interp hide $i $x
    }
    interp alias $i proc {} procwrapper $i
    return $i
}

proc subst_commands {id var} {
    upvar $var data
    set indices [lreverse [regexp -all -inline -indices {%(\w+?)\{(.*?)\}%} $data]]
    if {[llength $indices] == 0} { return }

    set i [setup_interp]
    foreach {contents_i type_i all_i} $indices {
        set output {}
        set type [string range $data [lindex $type_i 0] [lindex $type_i 1]]
        switch -exact -- $type {
            script -
            tcl {
                interp invokehidden $i db eval "select id,name,tf(modified) as modified,tf(created) as created,modified_by from pages where id='$id'" {}
                if {[catch {interp eval $i [string range $data [lindex $contents_i 0] [lindex $contents_i 1]]} err]} {
                    set output "<i>error in script</i>\n<!--\n$::errorInfo\n-->\n"
                }
                if {$::settings(FILTER_HTML)} {
                    set output [filter_white_html $output]
                }
            }
            include {
                set node [string range $data [lindex $contents_i 0] [lindex $contents_i 1]]
                interp eval $i [list set __inc $node]
                if {[string is integer -strict $node]} {
                    set output [interp eval $i [list sql {select id,name,parsed from pages where id=$__inc}]]
                } else {
                    set output [interp eval $i [list sql {select id,name,parsed from pages where lower(name)=lower($__inc)}]]
                }
                if {$output == ""} {
                    set output "<!-- node not found -->"
                } else {
                    set output "<div class=include><div class=include-header>[link node:[lindex $output 0] [lindex $output 1]]</div><div class=include-content>[lindex $output 2]</div></div>"
                }
            }
        }
        set data [string replace $data [lindex $all_i 0] [lindex $all_i 1] $output]
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
            if {$name == ""} { set name $filename }
            return "<a href=\"files/$filename\" class=filelink>$name</a> <span class=fileinfo>([link file:$data info])</span>"
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

proc get_cookies {} {
    global cookies
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

proc verify_captcha {var} {
    if {$::settings(RECAPTCHA_PRIVATE_KEY) == ""} { return 1 }
    upvar $var input
    if {![info exists input(recaptcha_challenge_field)] || ![info exists input(recaptcha_response_field)]} { return 0 }

    package require http
    set query [http::formatQuery privatekey $::settings(RECAPTCHA_PRIVATE_KEY) remoteip $::request(REMOTE_ADDR) challenge $input(recaptcha_challenge_field) response $input(recaptcha_response_field)]
    set t [http::geturl http://api-verify.recaptcha.net/verify -query $query]
    if {[http::ncode $t] != 200} { return 0 }
    set data [split [http::data $t] \n]
    if {[lindex $data 0] != "true"} { return 0 }
    return 1
}

# save a node after editing
# input: a node id or "new", and form postdata
# returns: nothing
proc savepage {id} {
    get_post input
    if {$id != "new"} {
        db eval {select content,protect,strftime('%s',modified) as modified from nodes where id=$id} {}
        if {![info exists content]} { http_error 404 "no such node" }
        if {![authorized node [expr {$input(content) == "delete" ? "delete" : "edit"}] $id]} { no_auth }
        if {$input(editstarted) < $modified} {
            db eval {select tf(modified) as modified,modified_by from nodes where id=$id} {}
            db eval {rollback transaction}
            http_error 409 "Edit conflict: modified by $modified_by at $modified"
        }
        if {!$::request(USER_AUTH) && ![verify_captcha input] } { http_error 403 Forbidden }
        if {![info exists input(protect)] || ![string is integer -strict $input(protect)] || $input(protect) > $::request(USER_LEVEL)} { set input(protect) $protect }
        if {$input(content) == $content} {
            db eval {update nodes set name=$input(name),protect=$input(protect) where id=$id}
            db eval {commit transaction}
            set strip "$input(name) [striphtml [db onecolumn {select parsed from nodes where id=$id}]]"
            fts eval {update search set name=$input(name),protect=$input(protect),content=$strip where id=$id}
            location node:$id
            return
        }
        db eval {delete from tags where node=$id}
        db eval {delete from links where node=$id}
        create_history_entry $id
        if {$input(content) == "delete"} {
            db eval {delete from nodes where id=$id}
            fts eval {delete from search where id=$id}
            set user "$::request(USER)@$::request(REMOTE_ADDR)"
            db eval {insert into history (original,type,content,created,created_by) values($id,'full','',datetime('now'),$user)}
            db eval {commit transaction}
            location {}
            return
        }
        set new 0
    } else {
        if {![authorized node create]} { no_auth }
        if {!$::request(USER_AUTH) && ![verify_captcha input] } { http_error 403 Forbidden }
        if {![string is integer -strict $input(protect)] || $input(protect) > $::request(USER_LEVEL)} { set input(protect) $::request(USER_LEVEL) }
        db eval {insert into nodes (name,content,protect) values($input(name),$input(content),$input(protect))}
        set id [db last_insert_rowid]
        set new 1
    }

    if {[catch { set time [time { set parsed [parse_static $id $input(content)] }] } err]} {
        db eval {rollback transaction}
        http_error 500 $::errorInfo
    }
    set strip "$input(name) [striphtml $parsed]"
    append parsed "\n<!-- static parse time: $time -->"

    set user "$::request(USER)@$::request(REMOTE_ADDR)"

    if {$new} {
        db eval {update nodes set parsed=$parsed,modified=datetime('now'),created=datetime('now'),modified_by=$user where id=$id}
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
        editnode new $name
    } else {
        #location node:$id
        showpage $id
    }
}

# show a page listing nodes that link to the given node
# input: a node id
# returns: nothing
proc showlinks {id} {
    db eval {select name from nodes where id=$id} {}
    if {![info exists name]} { http_error 404 "no such node" }
    if {![authorized node view $id]} { no_auth }
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
    load /usr/local/lib/sqlite3.6.17/libsqlite3.6.17.so Sqlite3
    #set dir [file dirname [pwd]]
    set dir [pwd]
    sqlite3 db [file join $dir wiki.db]
    sqlite3 fts [file join $dir fts.db]
    fts eval {PRAGMA synchronous = OFF}
    db function tf {format_time}
    db timeout 3000
}

proc close_databases {} {
    db close
    fts close
}

proc settings {} {
    global cookies settings
    db eval {select * from settings} { set settings($name) $val }
    foreach x {TZ TF} {
        if {[info exists cookies($x)]} { set settings($x) $cookies($x) }
    }
}

proc service_request {} {
    get_cookies
    authenticate
    settings
    #parray request

    #set doc [split [string range $::request(PATH_TRANSLATED) [string length [file dirname [info script]]/] end] :]
    set doc [split [file tail $::request(PATH_INFO)] :]
    set cmd [lindex $doc 0]
    set arg [join [lrange $doc 1 end] :]
 
    if {$::request(REQUEST_METHOD) == "GET"} {
        switch -exact -- $cmd {
            index.html { wikitag home }
            edit { editnode $arg }
            node { showpage $arg }
            file { showfile $arg }
            tag  { showtag $arg }
            links { showlinks $arg }
            history { showhistory $arg }
            wiki {
                switch -exact -- $arg {
                    nodes { nodelist }
                    files { filelist }
                    tags { taglist }
                    upload { upload }
                    new { editnode new }
                    search { search }
                    config { editconfig }
                    prefs { editprefs }
                    users { userlist }
                    login { login }
                    logout { logout }
                    default { wikitag $arg }
                }
            }
            default { showpagebyname [join $doc :] }
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
#exec echo [file tail $::env(PATH_TRANSLATED)] [expr {double($t) / 1000}] >> log
