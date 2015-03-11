#' @export

server_text <- '
library(shiny)
library(deSolve)
library(dplyr)
library(ggplot2)

ode     <- readRDS("ode.rds")
p       <- readRDS("parameters.rds")
len     <- round(length(names(p))/2)
regimen <- readRDS("regimen.rds")
misc    <- readRDS("misc.rds")
if(is.null(misc$tmax)) {
  if(!is.null(regimen$interval)) {
    misc$tmax <- (regimen$interval * (regimen$n+1))
  } else {
    misc$tmax <- max(regimen$times)
  }
}
shinyServer(function(input, output) {
  output$parInputs <-
      renderUI({
        w <- ""
        for(j in 1:len) {
          idx <- (j-1)*2+1
          if (j < len || len == length(names(p))/2) {
            val1 <- p[[names(p)[idx]]]
            val2 <- p[[names(p)[idx+1]]]
            w <- paste(w, fluidRow(column(6, sliderInput(names(p)[idx], names(p)[idx], min=signif(val1/5, 1), max=signif(val1*5,1), value=val1)),
                                   column(6, sliderInput(names(p)[idx+1], names(p)[idx+1], min=signif(val2/5, 1), max=signif(val2*5,1), value=val2)
                                   )))
          } else {
            val1 <- p[[names(p)[idx]]]
            w <- paste(w, fluidRow(column(6, sliderInput(names(p)[idx], names(p)[idx], min=signif(val1/5, 1), max=signif(val1*5,1), value=val1)
            )))
          }
        }
        HTML(w)
       })
  output$ind_plot <- renderPlot({
    regimen <- new_regimen(
      amt = as.numeric(input$amt),
      n = input$n,
      interval = as.numeric(input$interval),
      type = tolower(input$type),
      t_inf = as.numeric(input$t_inf)
    )
    pars <- p
    for(i in 1:length(names(p))) {
       if (!is.null(input[[names(p)[i]]])) {
        pars[[names(p)[i]]] <- input[[names(p)[i]]]
      }
    }
    dat <- sim_ode (ode = ode,
                    parameters = pars,
                    omega = misc$omega,
                    n_ind = input$n_ind,
                    regimen = regimen,
                    A_init = misc$A_init,
                    adherence = list(type = "markov", markov = list(p01 = input$adh_p01, p11 = input$adh_p11)),
                    tmax = NULL)
    if (input$plot_show != "all compartments") {
      dat <- dat %>% filter(comp == "obs")
    }
    p <- ggplot(dat, aes(x=t, y=y, colour=factor(id), group=id)) +
      geom_line() +
      facet_grid(comp ~ .) +
      theme_plain() +
      scale_colour_discrete(guide = FALSE) +
      xlab("time") + ylab("")
    if(input$plot_yaxis == "log10") {
      p <- p + scale_y_log10()
    }
    return(p)
  })
  outputOptions(output, name = "parInputs")
})
'
