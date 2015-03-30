library(minpack.lm)
library(propagate)
#library(gridExtra)

getTimePoints = function(x) {
    xrange <- range(x$time)
    xseq <- seq(from=xrange[1], to=xrange[2], length=50)## Used 80 before but choose whatever optimises looking OK vs speed
    return(xseq)
}

## Hugely ugly fn likely to be the source of many errors and bugs :-(
## All because lapply doesn't remember the name of the object being passed (i.e. it uses X[[]]not X[])
## Main idea is to get back predicted fit & CIs for each data set (i.e. Line and Sample)
mypredictNLS = function(index, times, modelFits, allNames) {
    ## index is the order in which this is passed from lapply
    need = allNames[index]## Now extract the correct name which is the Line
    xseq = times[[need]]## Find the timepoints that match this Line
    x = modelFits[[need]]## And then the NLS model result for this Line too
    tmp = predictNLS(x, newdata = data.frame(time = xseq), interval = "confidence", do.sim = FALSE)## Pass do.sim=F to propagate() to stop MonteCarlo estimates ## can use alpha to change conf interval
    ## below subset away the other cols if not using MC (just NA or NaN in this case)
    return(tmp$summary[,1:6])## Will crash R if you return all the $prop stuff too as individually they are massive (>100MB!!)
}

fitModels = function(data, FLAG) {
  ## convert data.frame to a list of DF where each
  ## element is a unique combo of block, genetic line and bio repeat
  res = dlply(data, ~ Block + Line + Sample)

  ## Apply non-linear regression using the exponential decay model to each subset
  ## Fit with Levenberg-Marquardt algorithm as its more robust than nls()
  mod = lapply(res, function(x) {nlsLM(value ~ A*exp(b*time), start = list(A=20, b=-0.5), data=x)})
  
  ## cheeky SpeedUp for Marie's data...
  if (FLAG) {
    allResByLine = readRDS("allResByLine.rds")
  } 
  else {
    allTPs = lapply(res, getTimePoints)
    
    if(!identical(names(allTPs), names(mod))){stop("Might be order mismatch between timepoints and NLS fit output")}
  
    pred <- lapply(seq_along(mod), mypredictNLS, times = allTPs, modelFits=mod, allNames=names(mod))## Takes a long time.....
    names(pred) = names(mod)
    
    allTPsDF = melt(as.data.frame(allTPs,optional=TRUE), value.name = "time")## optional=T doesn't change colnames)
    lineAndTPs = with(allTPsDF, cbind(colsplit(variable, pattern = "\\.", names = c("Block","Line.Sample")), time))## 
    lineAndTPs = with(lineAndTPs, cbind(Block, colsplit(Line.Sample, pattern = "\\.", names = c("Line","Sample")), time))
  
    nlsRes = rbind.fill(pred)##ldply(pred) also works here (though check # and name of cols)
    
    allResByLine = data.frame(lineAndTPs, nlsRes)
    names(allResByLine)[5:10] = c("mean1st", "mean2nd", "sd1st", "sd2nd", "CIlow", "CIhigh")
    allResByLine$Sample = factor(allResByLine$Sample)
  }

  return(list(mod=mod, allResByLine=allResByLine))
}

halfLife = function(x) {
    A = x$m$getPars()[["A"]]
    b = x$m$getPars()[["b"]]
    y = 0.5*x$m$lhs()[1]## Half the initial num of pods
    return(data.frame(halfLife=(1.0/b)*log(y/A), halfInitialPods=y))
}


