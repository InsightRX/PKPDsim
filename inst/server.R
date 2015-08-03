library(shiny)
library(deSolve)
library(dplyr)
library(ggplot2)

p       <- readRDS("parameters.rds")
len     <- ceiling(length(names(p))/2)
regimen <- readRDS("regimen.rds")
misc    <- readRDS("misc.rds")

if(!is.null(misc$code)) {
  model <- new_ode_model (code = misc$code, obs = misc$obs)
} else {
  model <- misc$ode
}

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
          pmin1 <- signif(val1/5, 1)
          pmax1 <- signif(val1*3, 1)
          if(val1 == 0) {
            pmin1 <- -1
            pmax1 <-  1
          }
          pmin2 <- signif(val2/5, 1)
          pmax2 <- signif(val2*3, 1)
          if(val2 == 0) {
            pmin2 <- -1
            pmax2 <- 1
          }
          w <- paste(w, fluidRow(column(6, sliderInput(names(p)[idx], names(p)[idx], min=pmin1, max=pmax1, value=val1)),
                                 column(6, sliderInput(names(p)[idx+1], names(p)[idx+1], min=pmin2, max=pmax2, value=val2)
                                 )))
        } else {
          val1 <- p[[names(p)[idx]]]
          pmin1 <- signif(val1/5, 1)
          pmax1 <- signif(val1*3, 1)
          if(val1 == 0) {
            pmin1 <- -1
            pmax1 <-  1
          }
          w <- paste(w, fluidRow(column(6, sliderInput(names(p)[idx], names(p)[idx], min=pmin1, max=pmin2, value=val1)
          )))
        }
      }
      HTML(w)
    })
  output$simSetup <- renderUI({
    list(h4("Regimen"),
    fluidRow(
      column(12, sliderInput("n_ind", "Number of individuals:", min = 1, max = 100, value = 1))),
    fluidRow(
      column(6, textInput("amt", "Amount:", value = regimen$amt)),
      column(6, textInput("interval", "Interval:", value = regimen$interval))),
    fluidRow(
      column(12, sliderInput("n", "Number of doses:", min = 1, max = 20, value = length(regimen$dose_times)),
             selectInput("type", "Dose type", c("Bolus", "Infusion"), selected="Bolus"),
             sliderInput("t_inf", "Infusion length:", min = 1, max = 12, value = regimen$t_inf)
      ))
    )
  })
  output$warning_text <- renderPrint({
    warning_text <- ""
    if (length(grep("CI", input$plot_type))>0 & input$n_ind < 10) {
      warning_text <- "Warning: Please increase number of individuals to >10 to calculate confidence intervals. For precise estimate this number needs to be much higher (n=500 or more depending on the CI)."
    }
    cat(warning_text)
  })
  output$code <- renderPrint({
    adh <- ""
    if(input$adh_p01 != 1 || input$adh_p11 != 1) {
      adh <- paste0('      adherence = list(type = "markov", markov = list(p01 = ', input$adh_p01, ', p11 = ', input$adh_p11,')),\n')
    }
    code <- "library(PKPDsim)\nlibrary(ggplot2)\n\n"
    if(!is.null(misc$code)) {
      code <- paste0(code, "\ncode <- '", misc$code, "'\n")
      code <- paste0(code, "model <- new_ode_model(code = code)\n\n")
    } else {
      model <- misc$ode
    }

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
    omega <- "NULL"
    if (!is.null(misc$omega)) {
      if (is.numeric(misc$omega)) {
        omega <- paste0(" c(", paste0(round(misc$omega,4), collapse=", "), ")")
      }
    }
    ode_txt <- paste0('ode = "', misc$ode, '",\n')
    if (is.null(misc$ode)) { ode_txt <- NULL }
    dde_txt <- paste0('dde = "', misc$dde, '",\n')
    if (is.null(misc$dde)) { dde_txt <- NULL }
    code <- paste0(code, '\n',
                   'dat <- sim_ode (
                   ', ode_txt, dde_txt, '  parameters = pars,
                   omega = ', omega, ',
                   n_ind = ', input$n_ind, ',
                   regimen = regimen,\n',
                   init,
                   adh,
                   '  t_max = NULL\n)\n\n')
    if (length(grep("CI", input$plot_type))>0 & input$n_ind >= 10) {
      ci <- c(0.05, 0.95) # 90%
      if (input$plot_type == "80% CI") {
        ci <- c(0.1, 0.9)
      }
      if (input$plot_type == "95% CI") {
        ci <- c(0.025, 0.975)
      }
      code <- paste0(code, 'dat_ci <- dat %>%\n  group_by(comp, t) %>%\n  summarise(\n    med = median(y),\n    q_low = quantile(y, ',ci[1],'),\n    q_up = quantile(y, ',ci[2],'))\n\n')
      gg <- "ggplot(dat_ci, aes(x=t, y=med)) +
      geom_ribbon(aes(ymin=q_low, ymax=q_up), fill='#bfbfbf', colour=NA) + "
    } else {
      if (input$n_ind == 1) {
        gg <- "ggplot(dat, aes(x=t, y=y)) + "
      } else {
        gg <- "ggplot(dat, aes(x=t, y=y, colour=factor(id), group=id)) + "
      }
    }
    code <- paste0(code, gg, '
                   geom_line() +
                   facet_grid(comp ~ ., scales = "free") +
                   theme_empty() +
                   scale_colour_discrete(guide = FALSE) +
                   xlab("time") + ylab("")')
    if(!is.null(input$target) && input$target != "") {
      hline_data <- data.frame(z = as.numeric(input$target), comp="obs")
      code <- paste0(code, '+\n  geom_hline(data = hline_data, aes(yintercept = z), colour="red", linetype="dashed")')
    }
    if(input$plot_yaxis == "log10") {
      code <- paste0(code, '+\n  scale_y_log10()')
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
    dat <- sim_ode (ode = model,
                    parameters = pars,
                    omega = misc$omega,
                    n_ind = input$n_ind,
                    obs_step_size = misc$obs_step_size,
                    int_step_size = misc$int_step_size,
                    regimen = regimen,
                    A_init = misc$A_init,
                    adherence = list(type = "markov", markov = list(p01 = input$adh_p01, p11 = input$adh_p11)),
                    t_max = misc$t_max,
                    verbose=FALSE)
    if (input$plot_show != "all") {
      if(!is.null(misc$obs$labels)) {
        dat <- dat %>% filter(comp %in% misc$obs$labels)
      }
    }
    if (length(grep("CI", input$plot_type))>0 & input$n_ind > 1) {
      ci <- c(0.05, 0.95) # 90%
      if (input$plot_type == "80% CI") {
        ci <- c(0.1, 0.9)
      }
      if (input$plot_type == "95% CI") {
        ci <- c(0.025, 0.975)
      }
      dat_tmp <- dat %>% group_by(comp, t) %>% summarise(med = median(y), q_low = quantile(y, ci[1]), q_up = quantile(y, ci[2]))
      p <- ggplot(dat_tmp, aes(x=t, y=med)) +
        geom_ribbon(aes(ymin=q_low, ymax=q_up), fill="#bfbfbf", colour=NA) +
        geom_line(aes(y=med))
    } else {
      if (input$n_ind == 1) {
        p <- ggplot(dat, aes(x=t, y=y))
      } else {
        p <- ggplot(dat, aes(x=t, y=y, colour=factor(id), group=id))
      }
    }
    p <- p +
      geom_line() +
      theme_empty() +
      scale_colour_discrete(guide = FALSE) +
      xlab("time") + ylab("")
    if (input$plot_show == "all") {
      p <- p + facet_grid(comp ~ ., scales = "free")
    }
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
