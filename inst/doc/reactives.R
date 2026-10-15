## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(shiny)
library(reactives)

reactiveConsole(TRUE)

## -----------------------------------------------------------------------------
a <- reactiveVal(1)
x <- reactives(a = a, b = reactive(2 * 21))

x$a
x[["b"]]
identical(x["a"], a)

## -----------------------------------------------------------------------------
x["c"] <- reactive(x$a + 1)
x["b"] <- NULL
names(x)

## -----------------------------------------------------------------------------
x$a <- 10
a()
x$c

x$d <- "new"
x$d

## -----------------------------------------------------------------------------
x$d <- NULL
names(x)
is.null(x$d)
is.null(x["d"])
is.null(x["e"])

## ----error = TRUE-------------------------------------------------------------
try({
x$c <- 5
})

## -----------------------------------------------------------------------------
y <- reactive_vals(count = 1, label = "one")
y$count <- 2
as_values(y)

## ----error = TRUE-------------------------------------------------------------
try({
w <- reactive_exprs(double = reactive(2 * y$count))
w$double

w$triple <- 6
names(w)
})

## -----------------------------------------------------------------------------
z <- y
reorder(z, c("label", "count"))
names(y)

## -----------------------------------------------------------------------------
x <- reactive_vals(a = 1, b = 2)

a_doubled <- reactive(
  {
    message("Computing a_doubled")
    2 * x$a
  }
)

a_doubled()

x$b <- 3
x$c <- 4
reorder(x, c("c", "b", "a"))
a_doubled()

x$a <- 5
a_doubled()

## -----------------------------------------------------------------------------
has_key(x, c("a", "d"))

## -----------------------------------------------------------------------------
slot_names <- reactive(
  {
    message("Reading the names")
    names(x)
  }
)

slot_names()

x$a <- 6
slot_names()

x["b"] <- NULL
slot_names()

## -----------------------------------------------------------------------------
term_ui <- function(id) {

  ns <- NS(id)

  div(
    id = ns("term"),
    numericInput(ns("value"), id, value = 1),
    actionButton(ns("remove"), "Remove")
  )
}

term_server <- function(id, terms) {
  moduleServer(
    id,
    function(input, output, session) {

      terms[id] <- reactive(input$value)
      session$onDestroy(function() terms[id] <- NULL)

      observeEvent(
        input$remove,
        {
          removeUI(paste0("#", session$ns("term")))
          session$destroy()
        }
      )
    }
  )
}

ui <- fluidPage(
  actionButton("add", "Add a term"),
  div(id = "terms"),
  textOutput("total")
)

server <- function(input, output, session) {

  terms <- reactive_exprs()

  observeEvent(
    input$add,
    {
      id <- paste0("term_", input$add)
      insertUI("#terms", ui = term_ui(id))
      term_server(id, terms)
    }
  )

  output$total <- renderText(
    paste("Total:", sum(unlist(as_values(terms))))
  )
}

## ----eval = FALSE-------------------------------------------------------------
# shinyApp(ui, server)

## -----------------------------------------------------------------------------
snap <- NULL

testServer(
  server,
  {
    session$setInputs(add = 1)
    session$setInputs(add = 2)
    session$setInputs(`term_1-value` = 3, `term_2-value` = 4)
    session$setInputs(`term_1-remove` = 1)
    print(output$total)
    snap <<- snapshot_reactives(terms)
  }
)

as_values(snap)

## ----include = FALSE----------------------------------------------------------
reactiveConsole(FALSE)

