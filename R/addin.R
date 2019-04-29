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
          "No selected text found.")

  # Get rid of anything in selected_text before a function definition.
  function_definition <- stringr::str_extract(selected_text, "function(.|\\s)*")

  fun <- eval(parse(text = function_definition))
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

  # We want to also capture their comments, so work directly with the text.
  function_body_text <- stringr::str_replace(
    function_definition,
    "^[^{]+\\{\\n*",
    ""
  )
  function_body_text <- stringr::str_replace(
    function_body_text,
    "\\s*\\}\\s*$",
    ""
  )
  function_pre_text <- stringr::str_extract(
    selected_text,
    "[^{]+\\{"
  )

  ui <- miniPage(
    gadgetTitleBar("Convert Selection to Factory"),
    miniContentPanel(
      fact_deps(),
      tags$p(function_pre_text),
      tags$pre(
        id = "editable",
        contenteditable = "true",
        function_body_text
      ),
      "}",
      tags$br(),
      # maybe a shiny::actionButton if you need to observe the click
      # from server side

      # I think we do. We need to store the name and (when applicable) value of
      # the insertions. For example, if I replace "2" with "exp", and click a
      # checkbox saying something like "Use original value as default for this
      # variable.", I need to add "exp = 2" to a list (via reactiveValues, I
      # think) that will be sent to build_factory at the end. Actually I think
      # we need to send "exp = quote(2)" (to make more complicated defaults
      # work), but we'll cross that bridge when we come to it. All this is to
      # say... I *think* we need to pass this info over to server, but maybe we
      # can do it all at the end by having the Done button know about what
      # happened in the JS? -JH
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

      # We need to parse the result using build_factory. First recombine it into
      # a function definition.
      function_start <- stringr::str_extract(
        function_definition,
        "function\\s*\\([^)]*\\)\\s*\\{"
      )
      function_to_parse <- paste(
        function_start,
        input$content,
        "}",
        sep = "\n"
      )

      # We need a list of variables (and, potentially, defaults), created each
      # time they make something a variable (they might do the same name more
      # than once; if so, only keep the last time they did so, unless an earlier
      # one had a default value and this one doesn't).

      # For the moment, I'm ignoring everything before "function" in the
      # selected text. In a future iteration, if they have anything, we should
      # end up with something like this (this example is the result of creating
      # a simple "square" function and then selecting its definition and
      # generalizing into a "power" function (called "square_factory" here
      # because I imagine that'd be our default naming scheme)):

      # square_factory <- function (exp)
      # {
      #   rlang::new_function(as.pairlist(alist(x = )), rlang::expr({
      #     x^`!!`(exp)
      #   }), rlang::caller_env())
      # }
      # square <- square_factory(2)

      # This doesn't work yet, because we don't have
      # list_of_variables_and_defaults:

      # factoryized_function <- build_factory(
      #   fun = eval(parse(text = function_to_parse)),
      #   !!!list_of_variables_and_defaults
      # )
      # factory_as_text <- paste(deparse(factoryized_function), collapse = "\n")

      # For now just replace their selected_text with factory_as_text, before we
      # implement some further parsing of their selected text to decide how to
      # give them back a call to their factory.

      # I'm leaving this piece here, but it does NOT replace with the proper
      # thing.
      recombined <- paste(
        function_pre_text,
        input$content,
        "}",
        sep = "\n"
      )
      rstudioapi::insertText(
        location = context$selection[[1]]$range,
        text = recombined
      )
      stopApp()
    })

  }

  runGadget(shinyApp(ui, server))
}
