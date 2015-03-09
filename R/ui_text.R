#' @export
ui_text <- '
library(shiny)

parInputs <- function(idx) {
  wellPanel(
    htmlOutput("parInputs")
  )
}

# Define UI for application that draws a histogram
fluidPage(
    fluidRow(column(3, h4("Parameters")), column(6, h4("")), column(3, h4("Dose regimen"))),
    fluidRow(
      column(3, parInputs("a")),
      column(6,
             plotOutput("ind_plot")
      ),
      column(3,
        wellPanel(
          fluidRow(
            column(12, sliderInput("n_ind", "Number of individuals:", min = 1, max = 50, value = 1))),
          fluidRow(
            column(6, textInput("amt", "Amount:", value = "100")),
            column(6, textInput("interval", "Interval:", value = "12"))),
          fluidRow(
            column(12, sliderInput("n", "Number of doses:", min = 1, max = 20, value = 3),
               selectInput("type", "Dose type", c("Bolus", "Infusion"), selected="Infusion"),
               sliderInput("t_inf", "Infusion length:", min = 1, max = 12, value = 2)
            )
          )
        ),
        wellPanel(
          fluidRow(
            h4("Plot")
          ),
          fluidRow(
            column(12, selectInput("plot_show", "Show:", c("all compartments", "observations only"), selected="all compartments")),
            column(12, selectInput("plot_yaxis", "Y-axis:", c("log10", "untransformed"), selected="log10"))
          )
        )
      )
    ),
#    theme = "bootstrap-3.3.0.min.css",
    responsive = TRUE
  )
'
