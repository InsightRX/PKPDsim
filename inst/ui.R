library(shiny)

parInputs <- function(idx) {
  wellPanel(
    htmlOutput("parInputs")
  )
}

# Define UI for application that draws a histogram
fluidPage(
  fluidRow(p(" ")),
  fluidRow(
    column(3,
           wellPanel(
             h4("Regimen"),
             fluidRow(
               column(12, sliderInput("n_ind", "Number of individuals:", min = 1, max = 50, value = 1))),
             fluidRow(
               column(6, textInput("amt", "Amount:", value = "100")),
               column(6, textInput("interval", "Interval:", value = "12"))),
             fluidRow(
               column(12, sliderInput("n", "Number of doses:", min = 1, max = 20, value = 3),
                      selectInput("type", "Dose type", c("Bolus", "Infusion"), selected="Bolus"),
                      sliderInput("t_inf", "Infusion length:", min = 1, max = 12, value = 2)
               )
             )
           ),
           wellPanel(
             h4("Adherence"),
             fluidRow(
               column(6, sliderInput("adh_p11", "p(1 -> 1)", min=0, max=1, value=1)),
               column(6, sliderInput("adh_p01", "p(0 -> 1)", min=0, max=1, value=1))
             )
           )
    ),
    column(6,
      tabsetPanel(
         tabPanel("Plot",
              plotOutput("ind_plot"),
              wellPanel(
                 fluidRow(
                   column(6, selectInput("plot_show", "Show:", c("all compartments", "observation compartment"), selected="all compartments")),
                   column(4, selectInput("plot_yaxis", "Y-axis:", c("log10", "untransformed"), selected="log10"))
                  ),
                fluidRow(
                  column(4, textInput("target", "Target level", value = ""))
                )
           )
          ),
          tabPanel("Code",
            verbatimTextOutput("code")
          )
        )
    ),
    column(3, parInputs("a"))
  ),
  theme = "style.css"
)
