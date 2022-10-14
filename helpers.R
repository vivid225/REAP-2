
## Data input -----

.getData <- function(filepath, h=TRUE){
  if (is.null(filepath))
    return(NULL)
  
  else{
    dat <- try(read.delim(filepath, header=h, sep="\t"))
    if(inherits(dat, "try-error") || ncol(dat)<3)
      dat <- try(read.delim(filepath, header=h, sep=","))
    if(inherits(dat, "try-error") || ncol(dat)<3)
      dat <- try(read.delim(filepath, header=h, sep=";"))
    if(inherits(dat, "try-error") || ncol(dat)<3)
      dat <- try(read.delim(filepath, header=h, sep=" "))
    if(inherits(dat, "try-error") || ncol(dat)<3){
      cat("format not supported.\n")
      return(NULL)
    }
    
    # Check whether ',' is used as decimal sep
    if(any(grepl(",", dat[,1])))
      dat[,1] <- as.numeric(as.character(gsub(",", ".", dat[,1])))
    if(any(grepl(",", dat[,2])))
      dat[,2] <- as.numeric(as.character(gsub(",", ".", dat[,2])))
    
    dat <- dat[dat[,1]!=0,]
    return(split(dat, dat[,3]))
  }
}

## Pairwise comparison -----
pairwise_compare <- function(m, sd, n){
  k <- length(m)
  Xg <- sum(n * m)/sum(n)
  dfb <- k - 1
  dfw <- sum(n) - k
  MSb <- sum(n * (m - Xg)^2)/(k - 1)
  MSw <- sum((n - 1) * sd^2)/dfw
  SSb <- dfb * MSb
  SSw <- dfw * MSw
  SSt <- SSb + SSw
  f.value <- MSb/MSw
  anova.table <- data.frame(matrix(NA, ncol = 4, nrow = 3))
  rownames(anova.table) <- c("Between (A)", "Within", "Total")
  colnames(anova.table) <- c("SS", "df", "MS", "F")
  anova.table$SS <- c(SSb, SSw, SSt)
  anova.table$df <- c(dfb, dfw, dfb + dfw)
  anova.table$MS <- c(MSb, MSw, NA)
  anova.table$F <- c(f.value, NA, NA)
  class(anova.table) <- c("anova", "data.frame")
  return(anova.table)
}

