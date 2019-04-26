convert_selection_to_factory <- function() {
  # The input to this function is a user-selected piece of text in an Rstudio
  # window, which must be a (normal) function definition, with or without a name
  # assignment. The goal is to walk them through generalizing it, then send it
  # to make_factory, and finally replace their highlighted text with a
  # well-structured function factory that can produce that function (plus a call
  # to that factory to create the function they started with).

  context <- rstudioapi::getActiveDocumentContext()
  selected_text <- context$selection[[1]]$text

  fun <- eval(parse(text = selected_text))
  if (!inherits(fun, "function")) {
    stop("The selection must be a function.")
  }

  # Load selected_text into a UI. The user can highlight anything in the UI,
  # click a "make variable" button, and enter a variable name to replace that
  # thing with. Prompt them/include a checkbox to make the thing they selected
  # the default for the variable. Either way we need to store the thing they
  # highlighted to use in the call to the factory that will reproduce their
  # function.

  # In the end, eval their variable-replaced function, and send it to
  # make_factory as the first argument, with the variable names (and, when
  # relevant, default values) as ... arguments.

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Convert Selection to Factory"),
    miniUI::miniContentPanel(
      # ???
    )
  )

  server <- function(input, output, session) {

    # Whatever else the server function might do, in the end it puts the result
    # of their make_factory call in place of the text they started with, plus a
    # call to that factory to make their function.
    observeEvent(input$done, {
      rstudioapi::insertText(deparsed_factory)
      stopApp()
    })
  }
}
