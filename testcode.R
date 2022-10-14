

# Example1_B_cell_lymphoma <- read.csv("~/Documents/GitHub/REAP/REAP/Example1_B_cell_lymphoma.csv")
dt.input <- .getData("~/Documents/GitHub/REAP/REAP/Example1_B_cell_lymphoma.csv")

truncated = 1e-9

models <- lapply(dt.input, function(tmp){
  ## preprocess the input dataset 
  tmp <- tmp[complete.cases(tmp),]
  tmp <- tmp[tmp[,1] > 0, ]
  tmp[,2][tmp[,2] <= truncated] = truncated
  tmp[,2][tmp[,2] >= 1-truncated] = 1-truncated
  x <- tmp[,1]
  y <- tmp[,2]
  if(!is.numeric(x) || !is.numeric(y))
    return(NULL)
  
  fit.gam <- gam(y~log(x),
                 family=betar(link="logit"),paraPen=list(logdose=list(diag(1))))
})

models

nms <- names(dt.input)
fulldt <- do.call(rbind, dt.input)
rownames(fulldt) = NULL

n = length(nms)

dose.dt <- as.data.frame(seq(min(fulldt[,1]), 0.9, length.out=1000))
colnames(dose.dt) <- "x"

if (input$cbox_extrap){
  
  minval <- c()
  maxval <- c()
  
  for ( i in 1:n){
    b0 <- models[[i]]$coefficients[1]
    b1 <- models[[i]]$coefficients[2]
    
    if (b1 > 0) {
      xmin = (logit(0.001)-b0)/b1
      xmax = (logit(0.999)-b0)/b1
      
      if (cbox_logconc){ # if log transform the x
        xmin = exp(xmin)
        xmax = exp(xmax)
      }
    } else if (b1 < 0){ 
      xmin = (logit(0.999)-b0)/b1
      xmax = (logit(0.001)-b0)/b1
      
      if (cbox_logconc){ # if log transform the x
        xmin = exp(xmin)
        xmax = exp(xmax)
      }
    }
    
    minval = c(minval, xmin)
    maxval = c(maxval, xmax)
  }
  minval = max(minval)
  maxval = min(maxval)
} else {
  minval = min(fulldt[,1])
  maxval = max(fulldt[,1])
}

nlength <- 1000

beta.pred <- c()
for (i in 1:n){
  pred <- predict(models[[i]], list(x = seq(minval, maxval, length.out=nlength)))
  
  beta.pred <- c(beta.pred,inv.logit(pred))
  
}

beta.pred.total <- data.frame(conc = rep(seq(minval, maxval, length.out=nlength),n),
                              resp = beta.pred)
beta.pred.total$cell = rep(nms, each=nlength)




# Line plot only
p <- ggplot() +
  geom_line(aes(x = beta.pred.total[,1], y = beta.pred.total[,2], color=beta.pred.total[,3]), size=0.5)+
  xlab(xlabel) + ylab(paste(ylabel,"(in %)")) +
  ylim(-0.05,1) + labs(color=fulldt.nms[3], shape=fulldt.nms[3]) 

if (input$showAsLog){
  p <- p + scale_x_continuous(trans = 'log10') 
}

if (input$Mean){
  p <- p + 
    geom_point(aes(x=dt.ci[,fulldt.nms[1]], y=dt.ci[,fulldt.nms[2]], color=dt.ci[,fulldt.nms[3]]),shape=shapenum,size=sizenum,stroke=linesize) 
}

if (input$Points){
  p <- p + 
    geom_point(aes(x=fulldt[,1], y=fulldt[,2], color=fulldt[,3]),shape=18,size=sizenum,stroke=linesize) 
}

if (input$SDerr){
  p <- p + 
    geom_errorbar(aes(x=dt.ci[,1],ymin=dt.ci$LCL, ymax=dt.ci$UCL, color=dt.ci[,fulldt.nms[3]]), width=ciwidth, size=0.4) + 
    geom_errorbar(aes(x = dt.ci[,fulldt.nms[1]][LCL_index],ymin = dt.ci$UCL[LCL_index], ymax = dt.ci$UCL[LCL_index],color=dt.ci[,fulldt.nms[3]][LCL_index]), width=ciwidth, size=linesize) +
    geom_linerange(aes(x = dt.ci[,fulldt.nms[1]][LCL_index],ymin = dt.ci[,fulldt.nms[2]][LCL_index], ymax = dt.ci$UCL[LCL_index],color=dt.ci[,fulldt.nms[3]][LCL_index]), size=linesize) +
    geom_errorbar(aes(x = dt.ci[,fulldt.nms[1]][UCL_index],ymin = dt.ci$LCL[UCL_index], ymax = dt.ci$LCL[UCL_index],color=dt.ci[,fulldt.nms[3]][UCL_index]), width=ciwidth, size=linesize) +
    geom_linerange(aes(x = dt.ci[,fulldt.nms[1]][UCL_index],ymin = dt.ci$LCL[UCL_index], ymax = dt.ci[,fulldt.nms[2]][UCL_index],color=dt.ci[,fulldt.nms[3]][UCL_index]), size=linesize) 
  
  if (!identical(UCL_index, integer(0)) & (dt.ci[LCL_index,]["UCL"] - dt.ci[LCL_index,]["LCL"] >= 0.1)){
    p <- p +
      geom_segment(aes(x = dt.ci[,fulldt.nms[1]], xend = dt.ci[,fulldt.nms[1]], y = dt.ci[,fulldt.nms[2]], yend = dt.ci$UCL_l, color=dt.ci[,fulldt.nms[3]]),  arrow = arrow(length = unit(ciwidth+0.2, "cm")), size=linesize)
  }
  
  if (!identical(LCL_index, integer(0)) & (dt.ci[LCL_index,]["UCL"] - dt.ci[LCL_index,]["LCL"] >= 0.1)){
    p <- p +
      geom_segment(aes(x = dt.ci[,fulldt.nms[1]], xend = dt.ci[,fulldt.nms[1]], y = dt.ci[,fulldt.nms[2]], yend = dt.ci$LCL_l, color=dt.ci[,fulldt.nms[3]]),  arrow = arrow(length = unit(ciwidth+0.2, "cm")), size=linesize)
  }
  
}

p


p + 
  geom_point(aes(x=ic50dt$xval, y=ic50dt$yval,color=ic50dt$agent),shape=17,size=2)
