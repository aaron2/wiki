function selectRow(e) {
  var o = $("#list > .selected_row");
  $(o).removeClass("selected_row");
  $(o).find("td:first").empty();

  var n = $(e.target).closest("tr");

  var level = $(n).find("td:eq(7)").text();
  if (!isadmin && level == 'Admin') { return false; }

  $(n).addClass("selected_row");
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
  var html = "<select name=level><option value=blocked>Blocked</option><option value=read-only>Read-only</option><option value=normal>Normal</option><option value=privileged>Privileged</option>";
  if (isadmin) { html += "<option value=admin>Admin</option>"; }
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
  $(row).removeClass("selected_row");
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
  jQuery.post("users", $("form").serialize(), deleteUserComplete, "json");
  return false;
}

function saveEdit(e) {
  jQuery.post("users", $("form").serialize(), saveEditComplete, "html");
  return false;
}

function deleteUserComplete(result) {
  if (result.result != "ok") {
    return false;
  }
  //$(".selected_row").slideUp("slow", function() { $(".selected_row").remove(); $("#list").bind("click", selectRow); });
  $(".selected_row").remove();
  $("#list").bind("click", selectRow);
}

function saveEditComplete(data) {
  var row = $(".selected_row");
  $(row).removeClass("selected_row");
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
      <option value=blocked>Blocked</option> \
      <option value=read-only>Read-only</option> \
      <option value=normal selected>Normal</option> \
      <option value=privileged>Privileged</option> \
      <option value=admin>Admin</option> \
    </td></tr>";

  var o = $("#list > .selected_row");
  $(o).removeClass("selected_row");
  $(o).find("td:first").empty();

  $("#list").append(html);
  var row = $("#list > tr:last");
  $(row).addClass("selected_row");
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
var isadmin = $("form > input[name='isadmin']").attr("value");
$("#list").bind("click", selectRow);
$("#new").bind("click", newUser);
