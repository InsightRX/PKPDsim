library(shiny)
library(deSolve)
library(dplyr)
library(ggplot2)

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
      w <- h4("Model parameters")
      for(j in 1:len) {
        idx <- (j-1)*2+1
        if (j < len || len == length(names(p))/2) {
          val1 <- p[[names(p)[idx]]]
          val2 <- p[[names(p)[idx+1]]]
          w <- paste(w, fluidRow(column(6, sliderInput(names(p)[idx], names(p)[idx], min=signif(val1/5, 1), max=signif(val1*3,1), value=val1)),
                                 column(6, sliderInput(names(p)[idx+1], names(p)[idx+1], min=signif(val2/5, 1), max=signif(val2*3,1), value=val2)
                                 )))
        } else {
          val1 <- p[[names(p)[idx]]]
          w <- paste(w, fluidRow(column(6, sliderInput(names(p)[idx], names(p)[idx], min=signif(val1/5, 1), max=signif(val1*3,1), value=val1)
          )))
        }
      }
      HTML(w)
    })
  output$code <- renderPrint({
    adh <- ""
    if(input$adh_p01 != 1 || input$adh_p11 != 1) {
      adh <- paste0('      adherence = list(type = "markov", markov = list(p01 = ', input$adh_p01, ', p11 = ', input$adh_p11,')),\n')
    }
    code <- "library(PKPDsim)\nlibrary(ggplot2)\n\n"
    pars_code <- 'pars <- list(\n'
    for(i in 1:length(names(p))) {
      if (!is.null(input[[names(p)[i]]])) {
        pars_code <- paste0(pars_code, '  ', names(p)[i], ' = ', input[[names(p)[i]]])
      }
      if (i == length(names(p))) {
        pars_code <- paste0(pars_code, "\n")
      } else {
        pars_code <- paste0(pars_code, ",\n")
      }
    }
    pars_code <- paste0(pars_code, ")\n")
    code <- paste0(code, pars_code)
    code <- paste0(code, '\n',
                     'regimen <- new_regimen(
  amt = ', as.numeric(input$amt), ',
  n = ', input$n, ',
  interval = ', input$interval, ',
  type = "', input$type, '",
  t_inf = ', input$t_inf, '\n)\n')
    init <- NULL
    if (!is.null(misc$A_init)) {
      init <- paste0(misc$A_init, '\n') # need to make this work better
    }
    omega <- ""
    if (!is.null(misc$omega)) {
        if (is.numeric(misc$omega)) {
          omega <- paste0(" c(", paste0(round(misc$omega,4), collapse=", "), ")")
        }
    }
    code <- paste0(code, '\n',
      'dat <- sim_ode (
  ode = "', misc$ode ,'",
  parameters = pars,
  omega = ', omega, ',
  n_ind = ', input$n_ind, ',
  regimen = regimen,\n',
         init,
         adh,
  '  tmax = NULL\n)\n\n')
   if (input$n_ind == 1) {
        gg <- "ggplot(dat, aes(x=t, y=y)) + "
   } else {
        gg <- "ggplot(dat, aes(x=t, y=y, colour=factor(id), group=id)) + "
   }
   code <- paste0(code, gg, '
  geom_line() +
  facet_grid(comp ~ ., scales = "free") +
  theme_plain() +
  scale_colour_discrete(guide = FALSE) +
  xlab("time") + ylab("")')
   if(!is.null(input$target) && input$target != "") {
     hline_data <- data.frame(z = as.numeric(input$target), comp="obs")
     code <- paste0(code, '+\n  geom_hline(data = hline_data, aes(yintercept = z), colour="red", linetype="dashed")')
   }
   if(input$plot_yaxis == "log10") {
     code <- paste0(code, '+\n  scale_y_log10')
   }
    cat(code)
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
    dat <- sim_ode (ode = misc$ode,
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
    if (input$n_ind == 1) {
      p <- ggplot(dat, aes(x=t, y=y))
    } else {
      p <- ggplot(dat, aes(x=t, y=y, colour=factor(id), group=id))
    }
    p <- p +
      geom_line() +
      facet_grid(comp ~ ., scales = "free") +
      theme_plain() +
      scale_colour_discrete(guide = FALSE) +
      xlab("time") + ylab("")

    if(!is.null(input$target) && input$target != "") {
      hline_data <- data.frame(z = as.numeric(input$target), comp="obs")
      p <- p + geom_hline(data = hline_data, aes(yintercept = z), colour="red", linetype="dashed")
    }
    if(input$plot_yaxis == "log10") {
      p <- p + scale_y_log10()
    }
    return(p)
  })
  outputOptions(output, name = "parInputs")
})
