library(shiny)

htmlGenerated <- function(idx) {
  obj <- list(
    "parInputs" = wellPanel(
       htmlOutput("parInputs")
    ),
    "simSetup" = wellPanel(
       htmlOutput("simSetup")
    )
  )
  return(obj[[idx]])
}

# Define UI for application that draws a histogram
fluidPage(
  fluidRow(p(" ")),
  fluidRow(
    column(3,
           htmlGenerated("simSetup"),
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
                   column(6, selectInput("plot_show", "Show compartment:", c("all", "observation"), selected="all compartments")),
                   column(6, selectInput("plot_yaxis", "Y-axis:", c("log10", "untransformed"), selected="untransformed"))
                  ),
                fluidRow(
                  column(6, selectInput("plot_type", "Type:", c("individuals", "80% CI", "90% CI", "95% CI"), selected="individuals")),
                  column(4, textInput("target", "Target level", value = ""))
                ),
                fluidRow(
                  column(12, em(textOutput("warning_text")))
                )
           )
          ),
          tabPanel("Code",
            verbatimTextOutput("code")
          )
        )
    ),
    column(3, htmlGenerated("parInputs"))
  ),
  theme = "style.css"
)
