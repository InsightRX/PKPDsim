library(shiny)
library(deSolve)
library(dplyr)
library(ggplot2)

source("../sim_ode.r")
source("../pk_3cmt_iv.R")

ode     <- readRDS("tmp/ode.rds")
p       <- readRDS("tmp/parameters.rds")
regimen <- readRDS("tmp/regimen.rds")
misc    <- readRDS("tmp/misc.rds")
if(is.null(misc$tmax)) {
  misc$tmax <- (regimen$interval * (regimen$n+1))
}
if(is.null(misc$output_cmt)) {
  output_cmt = 1:as.numeric(attr(ode, "size"))
}

shinyServer(function(input, output) {
  output$parInputs <-
    #function(idx) {
#     if (idx == "par") {
#       return(
      renderUI({
        w <- ""
        for(j in 1:len) {
          idx <- (j-1)*2+i
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
#     )
  output$ind_plot <- renderPlot({
    regimen <- list(
      amt = input$amt,
      n = input$n,
      interval = as.numeric(input$interval),
      type = tolower(input$type),
      inf_time = as.numeric(input$inf_time)
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
                    tmax = misc$tmax)
    ggplot(dat, aes(x=t, y=y, colour=factor(id), group=id)) +
      geom_line() +
      scale_y_log10() +
      facet_wrap(~comp)
  })
  outputOptions(output, name = 'parInputs')
})
