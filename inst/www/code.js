function changeStuff(){
  var a = document.getSelection().toString();
  var lgg = prompt('Provide a variable name for ' + a + ".");
  document.execCommand("insertText", false, lgg)
}

$(document).on("click", "#done", function(evt) {
  Shiny.setInputValue("content", $("#editable").text());
});