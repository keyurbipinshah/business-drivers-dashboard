var right = document.getElementById('split_delta').style.height;
var left = document.getElementById('delta').style.height;
if(left>right)
{
    document.getElementById('split_delta').style.height=left;
}
else
{
    document.getElementById('delta').style.height=right;
}