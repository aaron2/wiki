function maxSize() {
    var c = document.getElementById('content');
    var lineHeight = c.clientHeight / c.rows;
    var diff = document.body.scrollHeight - document.body.clientHeight;
    if (diff != 0) { c.rows -= diff / lineHeight; }
    else {
        c.rows = document.body.clientHeight / lineHeight;
        c.rows -= (document.body.scrollHeight - document.body.clientHeight) / lineHeight;
    }
    if (c.rows < 7) { c.rows = 7; }
}
window.onresize = maxSize;
window.onload = maxSize;
