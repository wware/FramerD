/* -*- Mode: C; -*- */

function toggle_detail(icon,state_id,display_id) {
    var state_cbox=document.getElementById(state_id);
    var display_elt=document.getElementById(display_id);
    if (state_cbox.checked) {
        icon.src='/graphics/unexpanded';
	state_cbox.checked=0;
	display_elt.style.display='none';}
    else {
        icon.src='/graphics/expanded';
	state_cbox.checked=1;
	display_elt.style.display='block';}
    return false;
}
function refile_form (evt) { 
  var cbox=document.getElementById('auto_refile');
  if ((cbox) && (cbox.checked == 0)) return true;
  evt = (evt) ? evt : ((event) ? event : null);
  if (evt) {
    var target = (evt.target) ? evt.target :
      ((evt.srcElement) ? evt.srcElement : null);
    if (target) target.form.submit();}
}

function submit_action(action)
{
 document.forms[0].action=action;
 document.forms[0].submit();
}

function submit_command(cmd)
{
  var elt=document.getElementById('commandvar');
  if (elt) elt.value=cmd;
  document.forms[0].submit();
}

function change_expertmode()
{
  var xbox=document.getElementById("expertbox");
  if (xbox.checked) xbox.checked=false;
  else xbox.checked=true;
  expertmode_changed();
}

function expertmode_changed()
{
  var xbox=document.getElementById("expertbox");
  if (xbox.checked)
    if (xbox.getAttribute("initial")) {
      var expertnodes=document.getElementsByName("expert");
      var i=0; while (i < expertnodes.length) {
	expertnodes[i].style.display='inline';
	i++;}}
    else {
      var formelt=xbox.form;
      var refresh=formelt.getAttribute('refresh');
      if (refresh) formelt.action=refresh;
      formelt.submit();}
  else {
    var expertnodes=document.getElementsByName("expert");
    var i=0; while (i < expertnodes.length) {
      expertnodes[i].style.display='none';
      i++;}}
}

function change_hidehelp()
{
  var hbox=document.getElementById("helpbox");
  if (hbox.checked) hbox.checked=false;
  else hbox.checked=true;
  hidehelp_changed();
}

function hidehelp_changed()
{
  var hbox=document.getElementById("helpbox");
  var helpdivs=document.getElementsByName("help");
  if (hbox.checked) {
    var i=0; while (i < helpdivs.length) {
      helpdivs[i].style.display='none';
      i++;}}
  else {
    var i=0; while (i < helpdivs.length) {
      helpdivs[i].style.display='block';
      i++;}}
  return true;
}

