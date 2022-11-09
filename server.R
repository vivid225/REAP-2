#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mgcv)
library(nplr)
library(gtools)
library(ggplot2)
library(ggprism)
library(ggnewscale)
library(scales)
library(msm)
library(fastDummies)
library(kableExtra)
library(dplyr)
source("helpers.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ## Get data -----
  dt.input <- reactiveValues(data = matrix(),
                             cells = character())
  
  observe({
    dt.input$data <- .getData(input$file1$datapath)
    dt.input$cells <- names(dt.input$data)
  })
  
  ## Output warning message -----
  warningmsg <- reactive({
    fulldt <- do.call(rbind, dt.input$data)
    rownames(fulldt) = NULL
    text1 = NULL
    if (any(fulldt[,2] <=0)==TRUE | any(fulldt[,2]>=1)==TRUE){
      text1 = "Observations about the data are unreliable. Some of your drug effects are out of (0,1).
            For better analysis, we truncate the extreme values to be within (0,1), which may increase
            the robustness of the dose response estimation. However, it is
            suggested that you perform the truncation or transformation yourself
            before uploading the dataset. "
    }
    
    text2 = NULL
    if (any(is.na(fulldt))){
      text2 = "Your data contains missing values or some dose levels are non-positive. We delete the relavant observation to
      ensure the following analysis."
    }
    
    text1
  })
  
  output$text_warning1 <- renderText({
    # fulldt <- do.call(rbind, dt.input$data)
    # rownames(fulldt) = NULL
    # text1 = NULL
    # if (any(fulldt[,2] <=0)==TRUE | any(fulldt[,2]>=1)==TRUE){
    #   text1 = "Observations about the data are unreliable. Some of your drug effects are out of (0,1).
    #         For better analysis, we truncate the extreme values to be within (0,1), which may increase
    #         the robustness of the dose response estimation. However, it is
    #         suggested that you perform the truncation or transformation yourself
    #         before uploading the dataset. "
    # }
    # 
    # text1
    warningmsg()[[1]]
  })
  
  output$text_warning2 <- renderText({
    fulldt <- do.call(rbind, dt.input$data)
    rownames(fulldt) = NULL
    text2 = NULL
    if (any(is.na(fulldt))){
      text2 = "Your data contains missing values or some dose levels are non-positive. We delete the relavant observation to
      ensure the following analysis."
    }
    
    text2
  })
  
  
  ## model fitting -----
  mgcvmodels <- reactive({
    
    truncated = 1e-9
    
    if(is.null(dt.input$data))
      return(NULL)
    models <- lapply(dt.input$data, function(tmp){
      ## preprocess the input dataset 
      tmp <- tmp[complete.cases(tmp),]
      tmp <- tmp[tmp[,1] > 0, ]
      tmp[,2][tmp[,2] <= truncated] = truncated
      tmp[,2][tmp[,2] >= 1-truncated] = 1-truncated
      x <- tmp[,1]
      y <- tmp[,2]
      if(!is.numeric(x) || !is.numeric(y))
        return(NULL)
      
      if (input$cbox_logconc){ # if log transform the dose level
        fit.gam <- gam(y~log(x),
                       family=betar(link="logit"),paraPen=list(logdose=list(diag(1))))
      } else{
        fit.gam <- gam(y~x,
                       family=betar(link="logit"),paraPen=list(logdose=list(diag(1))))
      }
    })
    models
  })
  
  
  
  ## Curve plots for each agent -----
  drplots <- reactiveValues()
  
  output$plot123 <- renderPlot({
    
    nms <- dt.input$cells
    dt <- dt.input$data
    fulldt <- do.call(rbind, dt.input$data)
    rownames(fulldt) = NULL
    
    n = length(nms)
    
    minval = min(fulldt[,1])
    maxval = max(fulldt[,1])
    
    nlength <- 1000
    
    beta.pred <- c()
    for (i in 1:n){
      pred <- predict(mgcvmodels()[[i]], list(x = seq(minval, maxval, length.out=nlength)))
      
      beta.pred <- c(beta.pred,inv.logit(pred))
      
    }
    
    beta.pred.total <- data.frame(conc = rep(seq(minval, maxval, length.out=nlength),n),
                                  resp = beta.pred)
    beta.pred.total$cell = rep(nms, each=nlength)# Prediction dataset is set up
    
    fulldt.nms <- colnames(fulldt)
    dt.ci <- Rmisc::summarySE(fulldt, measurevar=fulldt.nms[2], groupvars=c(fulldt.nms[1],fulldt.nms[3]))
    dt.ci <- subset(dt.ci, dt.ci$N > 1)
    
    dt.ci$LCL = dt.ci[,fulldt.nms[2]]-dt.ci$sd
    dt.ci$UCL = dt.ci[,fulldt.nms[2]]+dt.ci$sd # Build std CI
    dt.ci$UCL_l = ifelse(dt.ci$UCL > 1, 1 , NA)
    dt.ci$LCL_l = ifelse(dt.ci$LCL < 0, 0 , NA) # Examine whether CI exceed
    UCL_index = which(dt.ci$UCL > 1)
    LCL_index = which(dt.ci$LCL < 0)
    
    dt.ci$LCL[LCL_index] <- 0
    dt.ci$UCL[UCL_index] <- 1
    
    # Add (0,1) data points or (1,1) data points
    models = mgcvmodels()
    
    if (!input$showAsLog){
      df01 = c()
      for (i in 1:length(nms)){
        if (models[[i]]$coefficients[2] < 0){ # determine whether b1 is pos/neg
          df01 = rbind(df01, c(0,1))
        } else {
          df01 = rbind(df01, c(1,1,nms[i]))
        }
      }
      
      df01 = data.frame(df01)
      df01$X3 = nms
      colnames(df01) = colnames(beta.pred.total)
      beta.pred.total <- rbind(beta.pred.total, df01)
    }
    
    
    
    ## Add IC50/%effect estimation points on x axis
    if (!is.na(input$effectpct)){
      effect_num = 0.01*input$effectpct
      
      ic50dt = data.frame(agent = nms,
                          xval = NA,
                          yval = NA)
      for (i in 1:n){
        dt.pred.select = subset(beta.pred.total, beta.pred.total[,3]==nms[i])
        index = which.min(abs(dt.pred.select[,2]-effect_num))
        ic50dt[i,2] = dt.pred.select[index,1]
        ic50dt[i,3] = -0.03
        # print(dt.pred.select[index,])
        if (min(dt.pred.select[,2]) > effect_num | max(dt.pred.select[,2]) < effect_num){ # if effect estimation does not exist in the graph
          ic50dt[i,2] = NA
          ic50dt[i,3] = NA
        }
      }
      
      for (i in 1:nrow(ic50dt)){
        ind = which(abs(ic50dt$xval - ic50dt$xval[i])==0) 
        if (length(ind) >=2){ # Check if IC50 of any two or more agents stack upon each other
          k=-0.03
          for (j in 2:length(ind)){
            k = k+0.015
            ic50dt$yval[ind[j]] = k
          }
        }
        
      }
    }
    
    
    ## specify parameters for ggplot
    shapenum = 16
    ciwidth = input$lWidth
    legendsize = input$legendSize
    # sizenum=10* (ciwidth+0.2)
    sizenum = input$pSize
    linesize = input$lineSize
    
    if (input$xlabel == ''){
      xlabel = gsub("."," ", fulldt.nms[1],fixed=TRUE)
    } else {
      xlabel = input$xlabel
    }
    
    if (input$ylabel == ''){
      ylabel = gsub("."," ", fulldt.nms[2],fixed=TRUE)
    } else {
      ylabel = input$ylabel
    }
    
    # if (input$cbox_logconc){
    #   xval =
    # }
    
    # Line plot only
    p <- ggplot() +
      geom_line(aes(x = beta.pred.total[,1], y = beta.pred.total[,2], color=beta.pred.total[,3]), size=linesize)+
      xlab(xlabel) + ylab(ylabel) +
      labs(color=fulldt.nms[3], shape=fulldt.nms[3]) 
    
    if (input$showAsLog){
      p <- p + scale_x_continuous(trans = 'log10') 
    }
    
    if (input$Means){
      if (nrow(dt.ci) >= 1) {
        p <- p + 
          geom_point(aes(x=dt.ci[,fulldt.nms[1]], y=dt.ci[,fulldt.nms[2]], color=dt.ci[,fulldt.nms[3]]),shape=shapenum,size=sizenum) 
      }
    }
    
    if (input$Points){
      p <- p + 
        geom_point(aes(x=fulldt[,1], y=fulldt[,2], color=fulldt[,3]),shape=18,size=sizenum) 
    }
    
    if (input$SDerr){
      p <- p + 
        geom_errorbar(aes(x=dt.ci[,1],ymin=dt.ci$LCL, ymax=dt.ci$UCL, color=dt.ci[,fulldt.nms[3]]), width=ciwidth, size=linesize) + 
        geom_errorbar(aes(x = dt.ci[,fulldt.nms[1]][LCL_index],ymin = dt.ci$UCL[LCL_index], ymax = dt.ci$UCL[LCL_index],color=dt.ci[,fulldt.nms[3]][LCL_index]), width=ciwidth, size=linesize) +
        geom_linerange(aes(x = dt.ci[,fulldt.nms[1]][LCL_index],ymin = dt.ci[,fulldt.nms[2]][LCL_index], ymax = dt.ci$UCL[LCL_index],color=dt.ci[,fulldt.nms[3]][LCL_index]), size=linesize) +
        geom_errorbar(aes(x = dt.ci[,fulldt.nms[1]][UCL_index],ymin = dt.ci$LCL[UCL_index], ymax = dt.ci$LCL[UCL_index],color=dt.ci[,fulldt.nms[3]][UCL_index]), width=ciwidth, size=linesize) +
        geom_linerange(aes(x = dt.ci[,fulldt.nms[1]][UCL_index],ymin = dt.ci$LCL[UCL_index], ymax = dt.ci[,fulldt.nms[2]][UCL_index],color=dt.ci[,fulldt.nms[3]][UCL_index]), size=linesize) 
      
      if (nrow(dt.ci) >= 1){
        if (!identical(UCL_index, integer(0)) & (dt.ci[LCL_index,]["UCL"] - dt.ci[LCL_index,]["LCL"] >= 0.1)){
          p <- p +
            geom_segment(aes(x = dt.ci[,fulldt.nms[1]], xend = dt.ci[,fulldt.nms[1]], y = dt.ci[,fulldt.nms[2]], yend = dt.ci$UCL_l, color=dt.ci[,fulldt.nms[3]]),  arrow = arrow(length = unit(ciwidth+0.2, "cm")), size=linesize)
        }
        
        if (!identical(LCL_index, integer(0)) & (dt.ci[LCL_index,]["UCL"] - dt.ci[LCL_index,]["LCL"] >= 0.1)){
          p <- p +
            geom_segment(aes(x = dt.ci[,fulldt.nms[1]], xend = dt.ci[,fulldt.nms[1]], y = dt.ci[,fulldt.nms[2]], yend = dt.ci$LCL_l, color=dt.ci[,fulldt.nms[3]]),  arrow = arrow(length = unit(ciwidth+0.2, "cm")), size=linesize)
        }
      }
      
      
      
    }
    
    if (!is.na(input$effectpct)){
      p <- p + 
        geom_point(aes(x=ic50dt$xval, y=ic50dt$yval,color=ic50dt$agent),shape=17,size=sizenum)
    }
    
    p <- p +
      scale_color_prism("colors") + 
      scale_fill_prism("colors") +
      theme_prism(palette = "colors", base_size = 16) +
      scale_y_continuous(
        limits = c(-0.05, 1),
        # breaks = seq(-100, 500, 100),
        guide = "prism_offset",
        labels = function(x) x*100)
    
    drplots$plotci_scale <- p
    
    drplots$plotci_scale
    
  })
  
  # output$table <- renderTable({
  # validate(
  #     need(dt()[,2] >0 & dt()[,2] < 1,
  #          "Observations about the data is unreliable. Please check your dataset.")
  # )
  
  # if (any(dt()[,2] <=0)==TRUE | any(dt()[,2]>=1)==TRUE){
  #   return(dt.truncated())
  # } else {
  #   return(dt())
  # }
  # test
  # },
  # digits = 3)
  # 
  
  
  model.dt <- reactive({
    
    models = mgcvmodels()
    
    nms <- dt.input$cells
    dt <- dt.input$data
    fulldt <- do.call(rbind, dt.input$data)
    rownames(fulldt) = NULL
    
    n = length(nms)
    fulldt.nms <- colnames(fulldt)
    
    beta.intercept = c()
    beta.slope = c()
    # beta.fit = c()
    beta.slope.se = c()
    beta.slope.pval = c()
    # logic50.se = c()
    logic10.se = c() # can be any % effect
    for (i in 1:n){
      
      d.betareg <- models[[i]]
      beta.intercept = c(beta.intercept, d.betareg$coefficients[1])
      beta.slope = c(beta.slope, d.betareg$coefficients[2])
      # sum <- summary(d.betareg)
      beta.slope.se = c(beta.slope.se, summary(d.betareg)$se[2])
      beta.slope.pval = c(beta.slope.pval, summary(d.betareg)$p.pv[2])
      # logic50.se = c(logic50.se, deltamethod(~-x1/x2, coef(d.betareg)[1:2],vcov(d.betareg)[1:2,1:2]))
      form <- sprintf("~ (%f-x1)/x2",logit(0.01*input$effectpct))
      logic10.se = c(logic10.se, deltamethod(as.formula(form), d.betareg$coefficients, vcov(d.betareg)))
    }
    
    # z-test for slope on |m|>1
    beta.slope.z.pval <- c()
    for (i in 1:length(beta.slope)){
      slope = beta.slope[i]
      se = beta.slope.se[i]
      if (slope > 0){
        z = (slope-1)/se
        pval = pnorm(z, lower.tail = FALSE)
      } else {
        z = (slope+1)/se
        pval = pnorm(z, lower.tail = TRUE)
      }
      beta.slope.z.pval = c(beta.slope.z.pval, pval)
    }
    beta.slope.z.pval = ifelse(beta.slope.z.pval<0.0001,"<.0001",round(beta.slope.z.pval,4))
    
    
    
    # EC10 estimation panel -----
    ic10 = exp((logit(0.01*input$effectpct)-beta.intercept)/beta.slope)
    ic10.se = sqrt((logic10.se)^2 * ic10^2)
    ic10.z = ic10/ic10.se
    
    # Compare ic10
    ic10.count = fulldt %>%
      group_by(fulldt[,3]) %>%
      dplyr::summarise(
        count_agent = n()
      )
    
    ic10.sd<-ic10.se*sqrt(as.list(ic10.count[,2])$count_agent)
    ic10.n<-as.list(ic10.count[,2])$count_agent
    # ic10.ind.anova <- ind.oneway.second(ic10,ic10.sd,ic10.n)
    ic10.ind.anova <- pairwise_compare(m=ic10,sd=ic10.sd,n=ic10.n)
    ic10.anova.pval <- pf(ic10.ind.anova$F[1], 
                          ic10.ind.anova$df[1], 
                          ic10.ind.anova$df[2], lower.tail = FALSE)
    
    
    ic10.pval = 2*pnorm(abs(ic10.z), lower.tail = FALSE)
    ic10.pval = ifelse(ic10.pval<1e-4, "<.0001", round(ic10.pval,4))
    beta.slope.pval = ifelse(beta.slope.pval<1e-4, "<.0001", round(beta.slope.pval,4))
    # models <- as.character(unique(dt.truncated()[,3]))
    
    # ic50 piecewise comparison
    # first rank ic50 from low to high
    dt.ic50 <- data.frame(
      # ic50=ic50,
      # se = ic50.se,
      # sd = ic50.sd,
      # n = ic50.n,
      ic10=ic10,
      se10 = ic10.se,
      sd10 = ic10.sd,
      n10 = ic10.n,
      model = nms,
      beta.slope = beta.slope,
      beta.slope.se = beta.slope.se,
      beta.slope.pval = beta.slope.pval,
      beta.slope.z.pval = beta.slope.z.pval,
      beta.intercept = beta.intercept
    )
    dt.ic50 <- dt.ic50[order(ic10),]
    ic10.pwise.pval <- c()
    
    if (nrow(dt.ic50)==1) {
      ic10.pwise.pval = "-"
    } else {
      for (i in 1:(nrow(dt.ic50)-1)){
        dt <- dt.ic50[c(i,i+1),]
        # ic10.pwise.anova <- ind.oneway.second(dt[,"ic10"],dt[,"sd10"],dt[,"n10"])
        ic10.pwise.anova <- pairwise_compare(m=dt[,"ic10"],sd=dt[,"sd10"],n=dt[,"n10"])
        pval <- pf(ic10.pwise.anova$F[1], 
                   ic10.pwise.anova$df[1], 
                   ic10.pwise.anova$df[2], lower.tail = FALSE)
        ic10.pwise.pval = c(ic10.pwise.pval, pval)
      }
      ic10.pwise.pval <- ifelse(ic10.pwise.pval<0.0001, "<0.0001", round(ic10.pwise.pval,4))
      ic10.pwise.pval = c("-", ic10.pwise.pval)
    }
    
    model <- data.frame(Model = dt.ic50$model,
                        Intercept = round(dt.ic50$beta.intercept,3),
                        Slope = round(dt.ic50$beta.slope,3),
                        Slope.Std.Err = round(dt.ic50$beta.slope.se,3),
                        Slope.Pvalue = dt.ic50$beta.slope.pval,
                        Slope.z.Pvalue = dt.ic50$beta.slope.z.pval,
                        IC10 = round(dt.ic50$ic10,3),
                        IC10.Std.Err = round(dt.ic50$se10,3),
                        IC10.Pvalue = ic10.pwise.pval
    )
    rownames(model) = NULL
    sum.res = c()
    sum.res$model <- model
    # sum.res$ic50.anova.pval <- ifelse(ic50.anova.pval<0.0001, "<.0001",round(ic50.anova.pval,4))
    sum.res$ic10.anova.pval <- ifelse(ic10.anova.pval<0.0001, "<.0001",round(ic10.anova.pval,4))
    sum.res
  })
  
  # Compare models and slopes -----
  compare.pval <- reactive({
    nms <- dt.input$cells
    dt <- dt.input$data
    fulldt <- do.call(rbind, dt.input$data)
    rownames(fulldt) = NULL
    fulldt.nms = colnames(fulldt)
    
    dt.dummy <- dummy_cols(fulldt, select_columns = fulldt.nms[3])
    
    if (length(nms) == 1){
      compare.pval = 0
    } else {
      colnames(dt.dummy)[4:ncol(dt.dummy)] <- paste("dummy",1:length(nms),sep="")
      colnms <- colnames(dt.dummy)
      
      interactpart <- dt.dummy[,4:(ncol(dt.dummy)-1)]*log(dt.dummy[,1])
      colnames(interactpart) <- paste("drug_dummy",1:(length(nms)-1),sep="")
      dt.dummy <- cbind(dt.dummy, interactpart)
      
      # Null model
      y <- dt.dummy[,2] # response value
      logx <- log(dt.dummy[,1])
      model0 = gam(y ~ logx,
                   family=betar(link="logit"),paraPen=list(logdose=list(diag(1)))) 
      df0 = 3 # intercept + slope + intercept for phi
      
      # Multiple intercepts and multiple slopes
      x1 = as.matrix(cbind(dt.dummy[,4:(4+length(nms)-2)],
                           log(dt.dummy[,1]),interactpart))
      model1 = gam(y ~ x1,
                   family=betar(link="logit"),paraPen=list(logdose=list(diag(1)))) 
      df1 = ncol(x1)+2
      
      # Multiple intercepts and one slope
      x2 <- as.matrix(cbind(dt.dummy[,4:(4+length(nms)-2)],
                            log(dt.dummy[,1])))
      model2 = gam(y ~ x2,
                   family=betar(link="logit"),paraPen=list(logdose=list(diag(1)))) 
      df2 = ncol(x2)+2
      
      # Comparison results
      compare01 = 1 - pchisq(2*as.numeric(logLik(model1)) - 2*as.numeric(logLik(model0)), df1-df0)
      compare12 = 1 - pchisq(2*as.numeric(logLik(model1)) - 2*as.numeric(logLik(model2)), df1-df2)
      # compare12 = 1 - pchisq(2*model1$log_lik - 2*model2$log_lik, df1-df2)
      compare01 = ifelse(compare01<0.0001, "<.0001",round(compare01,4))
      compare12 = ifelse(compare12<0.0001, "<.0001",round(compare12,4))
      compare.pval = c(compare01,compare12)
    }
    
    compare.pval
  })
  
  ## model summary table -----
  formulatbl <- reactive({
    
    models = mgcvmodels()
    n = length(models)
    nms <- dt.input$cells
    dt <- dt.input$data
    fulldt <- do.call(rbind, dt.input$data)
    rownames(fulldt) = NULL
    fulldt.nms = colnames(fulldt)
    
    left = paste("logit(E) =", sep="")
    if (input$cbox_logconc){
      right = paste("* log(",fulldt.nms[1],")", sep="")
    } else {
      right = paste("* ",fulldt.nms[1], sep="")
    }
    
    modelsum = c()
    
    for (i in 1:n){
      if (summary(models[[i]])$p.coeff[2] >= 0){
        sgn = "+"
      } else {
        sgn = "-"
      }
      temp = paste(left, round(summary(models[[i]])$p.coeff[1],3), sgn,
                   round(abs(summary(models[[i]])$p.coeff[2]),3), right)
      modelsum = c(modelsum, temp)
    }
    
    ## AIC of each model
    aiclist <- c()
    for ( i in 1:n){
      aiclist <- c(aiclist, AIC(models[[i]]))
    }
    
    modelsum.dt <- data.frame(Model = nms,
                              Formula = modelsum,
                              AIC = round(aiclist,3))
    
  })
  
  output$modelsummary <- renderText({
    
    modelsum.dt <- formulatbl()
    modelsum.dt %>% 
      kable(align = "c") %>%
      kable_styling("striped") 
  })
  
  
  # Model comparison table-----
  
  modelcomparisonresults <- reactive({
    if (length(compare.pval())==1){
      comp_models <- data.frame(col1 = c("Null hypothesis", "Alternative hypothesis",
                                         "P-value"),
                                col2 = c("Fitted models same for all the agents",
                                         "At least one fitted model is different from other agents",
                                         "Only one model for the dataset. No comparison is conducted."))
      
      comp_ic50 <- data.frame(col1 = c("Null hypothesis", "Alternative hypothesis",
                                       "P-value"),
                              col2 = c("Potency estimation same for all the agents",
                                       "At least one potency estimation is different from other agents",
                                       "Only one model for the dataset. No comparison is conducted."))
      
      comp_slope <- data.frame(col1 = c("Null hypothesis", "Alternative hypothesis",
                                        "P-value"),
                               col2 = c("Slope estimation same for all the agents",
                                        "At lease one slope estimation is different from other agents",
                                        "Only one model for the dataset. No comparison is conducted."))
    } else {
      comp_models <- data.frame(col1 = c("Null hypothesis", "Alternative hypothesis",
                                         "P-value"),
                                col2 = c("Fitted models same for all the agents",
                                         "At least one fitted model is different from other agents",
                                         compare.pval()[1]))
      
      comp_ic50 <- data.frame(col1 = c("Null hypothesis", "Alternative hypothesis",
                                       "P-value"),
                              col2 = c("Potency estimation same for all the agents",
                                       "At least one potency estimation is different from other agents",
                                       model.dt()$ic10.anova.pval))
      
      comp_slope <- data.frame(col1 = c("Null hypothesis", "Alternative hypothesis",
                                        "P-value"),
                               col2 = c("Slope estimation same for all the agents",
                                        "At lease one slope estimation is different from other agents",
                                        compare.pval()[2]))
    }
    
    
    comp=NULL
    if (input$cbox_effectest == TRUE){
      comp = comp_ic50
    }
    if (input$cbox_slopes == TRUE){
      comp = comp_slope
    }
    if (input$cbox_effectest == TRUE & input$cbox_slopes == TRUE){
      comp = comp_models
    }
    
    comp
    
  })
  
  output$modelcomparison <- renderText({
    
    comp <- modelcomparisonresults()
    
    if (input$cbox_effectest == FALSE & input$cbox_slopes == FALSE){
      comp
    } else {
      names_spaced <- c(" "," ")
      comp %>% 
        kable(align = "l",col.names = names_spaced) %>%
        kable_styling("striped") 
    }
  })
  
  
  # Estimation summary -----
  
  output$summary <- renderText({
    
    names_spaced <- c(
      ' ', 'Estimate (m)', 'Std.Err.', 
      'm > 1',' ',                 
      'Estimate', 'Std.Err.','Pairwise comparison')
    
    hd = paste("IC/EC",input$effectpct,"Estimation")
    
    dt.footnote <- model.dt()$model
    # names(dt.footnote)[4] <- paste0(names(dt.footnote)[4], 
    #                                 footnote_marker_symbol(1))
    # names(dt.footnote)[4] <- paste0(names(dt.footnote)[7], 
    #                                 footnote_marker_symbol(2))
    
    options(knitr.kable.NA = '')
    dt.footnote %>%
      dplyr::select(Model,Slope,Slope.Std.Err,Slope.z.Pvalue,IC10,IC10.Std.Err,IC10.Pvalue)%>%
      mutate(Slope = abs(Slope)) %>%
      tibble::add_column(new_col = NA, .after = c("Slope.z.Pvalue"))%>%
      kable(align = "c",col.names=names_spaced,format = "html") %>%
      kable_styling("striped") %>%
      column_spec(c(2,4,6,8), width = "6em") %>%
      column_spec(c(5), width = "4em") %>%
      add_header_above(c(" " = 1, "Hill Coefficient" = 3, " "=1, "Potency Estimation" = 3))%>%
      footnote(
        number = c("m > 1: p-value based on one-sided t-test for hypothesis testing on hill coefficient > 1", 
                   "Pairwise comparison: p-value based on ANOVA test (Cohen, 2000). Concentrations that give specified effect (default at 50%) by group were sorted from low to high. Hypothesis testings on equal potency (i.e., concentration for ED50/IC50 by default) were conducted pairwise with the group right above (one rank lower).",
                   "95% confidence intervals can be approximated by Estimate +/- t-value(0.975, df=n-1)*Std.Err.",
                   "Effect estimate is indicated by triangles in the dose-response curve plot.")
      )
    
  })
  
  ## Download report -----
  
  output$downloadReport <- downloadHandler(
    filename <- function(){sprintf("%s.pdf", input$fname)},
    content = function(file) {
      rmarkdown::render("reports/report.Rmd",
                        output_file = file, 
                        params = list(
                          title = "REAP-2 Report", 
                          set_logd = input$cbox_logconc,
                          set_effest = input$effectpct,
                          set_dtpt = input$Points,
                          set_smean = input$Means,
                          set_stderr = input$SDerr,
                          set_plotlog10 = input$showAsLog,
                          plot = drplots$plotci_scale,
                          table_model = formulatbl(),
                          table_esti = model.dt()$model,
                          table_compare = modelcomparisonresults(),
                          warningmsg = warningmsg()
                        ),
                        envir = new.env(),
                        intermediates_dir = tempdir())
    }
  )

 
  
})


