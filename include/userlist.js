function selectRow(e) {
  var o = $("#list > .selected");
  $(o).removeClass("selected");
  $(o).find("td:first").empty();

  var n = $(e.target).closest("tr");

  var level = $(n).find("td:eq(7)").text();
  if (userlevel < levels[level]) { return false; }

  $(n).addClass("selected");
  $(n).find("td:first").html("<a href=# id=edit>edit</a><br><br><a href=# id=delete>delete</a>");
  $("#edit").bind("click", editUser);
  $("#delete").bind("click", deleteUser);
}

function editUser(e) {
  var row = $(e.target).closest("tr");
  $(row).find("td:first").html("<a href=# id=save>save</a><br><br><a href=# id=cancel>cancel</a>");
  $(row).find("td:gt(1):lt(4)").each(function(i) {
    t[i] = $(this).text();
    $(this).empty();
    var input = document.createElement("input");
    input.name = names[i];
    input.type = "text";
    input.value = t[i];
    $(this).append(input);
  });
  $(row).find("input[name=password]").attr("value", '');

  var level = $(row).find("td:eq(7)");
  t[7] = $(level).text();
  var html = "<select name=level><option value=0>Blocked</option><option value=10>Base</option><option value=20>Edit</option><option value=25>Privileged</option>";
  if (userlevel >= 30) { html += "<option value=30>Admin</option>"; }
  html += "</select>";
  $(level).html(html);
  $(level).find("option:contains("+t[7]+")").attr("selected", true);

  $("form > input[name='action']").attr("value", "edit");
  $("form > input[name='user']").attr("value", $(row).find("td:eq(1)").text());

  $("#list").unbind("click", selectRow);
  $("#cancel").bind("click", cancelEdit);
  $("#save").bind("click", saveEdit);
  $("#new").hide();
  return false;
}

function cancelEdit(e) {
  var row = $(e.target).closest("tr");
  $(row).removeClass("selected");
  $(row).find("td:first").empty();
  $(row).find("td:gt(1):lt(4)").each(function(i) {
    $(this).empty();
    $(this).text(t[i]);
  });
  $(row).find("td:eq(7)").empty();
  $(row).find("td:eq(7)").text(t[7]);
  $("#list").bind("click", selectRow);
  $("#new").show();
  return false;
}

function deleteUser(e) {
  var user = $(e.target).closest("tr").find("td:eq(1)").text();
  if (!confirm("Delete user " + user + "?")) { 
    return false;
  }
  $("form > input[name='user']").attr("value", user);
  $("form > input[name='action']").attr("value", "delete");
  jQuery.post("http://crack.lighter.net/wiki/users", $("form").serialize(), deleteUserComplete, "json");
  return false;
}

function saveEdit(e) {
  jQuery.post("http://crack.lighter.net/wiki/users", $("form").serialize(), saveEditComplete, "html");
  return false;
}

function deleteUserComplete(result) {
  if (result.result != "ok") {
    return false;
  }
  //$(".selected").slideUp("slow", function() { $(".selected").remove(); $("#list").bind("click", selectRow); });
  $(".selected").remove();
  $("#list").bind("click", selectRow);
}

function saveEditComplete(data) {
  var row = $(".selected");
  $(row).removeClass("selected");
  $(row).html(data);
  $("#list").bind("click", selectRow);
}

function newUser() {
  var html = "<tr> \
    <td align=center><a href=# id=save>save</a><br><br><a href=# id=cancel>cancel</a></td> \
    <td><input type=text name=newuser></td> \
    <td><input type=text name=host></td> \
    <td><input type=text name=password></td> \
    <td><input type=text name=name></td> \
    <td><input type=text name=email></td> \
    <td></td> \
    <td><select name=level> \
      <option value=0>Blocked</option> \
      <option value=10 selected>Base</option> \
      <option value=20>Edit</option> \
      <option value=25>Privileged</option> \
      <option value=30>Admin</option> \
    </td></tr>";

  var o = $("#list > .selected");
  $(o).removeClass("selected");
  $(o).find("td:first").empty();

  $("#list").append(html);
  var row = $("#list > tr:last");
  $(row).addClass("selected");
  $("form > input[name='action']").attr("value", "new");

  $("#list").unbind("click", selectRow);
  $("#cancel").bind("click", function() {
    $("#list > tr:last").remove();
    $("#new").show();
    $("#list").bind("click", selectRow);
  });
  $("#save").bind("click", saveEdit);
  $("#new").hide();
}

var t = new Array();
var names = new Array('host','password','name','email','','level');
var levels = { Blocked : 0, Base : 15, Edit : 20, Privileged : 25, Admin : 30 };
var userlevel = $("form > input[name='userlevel']").attr("value");
$("#list").bind("click", selectRow);
$("#new").bind("click", newUser);
