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
            column(12,
               sliderInput("n_ind", "Number of individuals:", min = 1, max = 50, value = 1),
               sliderInput("amt", "Dose amount:", min = 20, max = 200, value = 100),
               sliderInput("n", "Number of doses:", min = 1, max = 20, value = 3),
               selectInput("interval", "Interval:", c(6, 8, 12, 24, 48, 168), selected=24),
               selectInput("type", "Dose type", c("Bolus", "Infusion"), selected="Infusion"),
               sliderInput("inf_time", "Infusion length:", min = 1, max = 12, value = 2)
            )
          )
        )
      )
    ),
    responsive = TRUE
  )
