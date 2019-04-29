function changeStuff(){
  var a = document.getSelection().toString();
  var lgg = prompt('What do you want to replace ' + a + " with?");
  document.execCommand("insertText", false, lgg)
}

$(document).on("click", "#done", function(evt) {
  Shiny.setInputValue("content", $("#editable").text());
});