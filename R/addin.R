#'@importFrom htmltools htmlDependency
fact_deps <- function(){
  htmlDependency(
    "factory", "0.0.1",
    src = system.file("www", package = "factory"),
    script = "code.js"
  )
}


#'@importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#'@importFrom shiny tags observeEvent stopApp runGadget shinyApp
#'@importFrom attempt stop_if_not stop_if
convert_selection_to_factory <- function() {
  # The input to this function is a user-selected piece of text in an Rstudio
  # window, which must be a (normal) function definition, with or without a name
  # assignment. The goal is to walk them through generalizing it, then send it
  # to make_factory, and finally replace their highlighted text with a
  # well-structured function factory that can produce that function (plus a call
  # to that factory to create the function they started with).

  context <- rstudioapi::getActiveDocumentContext()
  selected_text <- context$selection[[1]]$text
  stop_if(selected_text, ~ .x == "",
          "No selected texte found.")

  fun <- eval(parse(text = selected_text))
  stop_if_not(fun, ~ inherits(.x, "function"),
              "The selection must be a function.")


  # Load selected_text into a UI. The user can highlight anything in the UI,
  # click a "make variable" button, and enter a variable name to replace that
  # thing with. Prompt them/include a checkbox to make the thing they selected
  # the default for the variable. Either way we need to store the thing they
  # highlighted to use in the call to the factory that will reproduce their
  # function.

  # In the end, eval their variable-replaced function, and send it to
  # make_factory as the first argument, with the variable names (and, when
  # relevant, default values) as ... arguments.

  ui <- miniPage(
    gadgetTitleBar("Convert Selection to Factory"),
    miniContentPanel(
      fact_deps(),
      tags$pre(
        id = "editable",
        contenteditable = "true",
        selected_text
      ),
      tags$br(),
      # maybe a shiny::actionButton if you need to observe the click
      # from server side
      tags$button(
        id = "modal",
        "Make variable",
        onclick = "changeStuff();"
      )
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {
      # This is for debug purpose (or you can add it to the final fun)
      # input$content contains the raw text from the edited div
      cli::cat_rule("text found:")
      cli::cat_line(input$content)
      rstudioapi::insertText(location = context$selection[[1]]$range, input$content)
      stopApp()
    })

  }

  runGadget(shinyApp(ui, server))
}
