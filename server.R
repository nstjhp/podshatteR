library(shiny)
library(ggplot2)
library(plyr)
library(reshape2)
library(grid)

source("funcs.R")
## if FLAG = TRUE read the RDS file of all CIs etc from Marie's data
#FLAG = TRUE
FLAG = FALSE
maxBlocks = 10 ## maximum num. of blocks in exp design

shinyServer(function(input, output, session) {
  output$dummyPlot = renderPlot({
    print(emptyPlot())
  })

  getUserData = reactive({
    ## input$file1 will be NULL initially. After the user selects and uploads a 
    ## file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    ## columns. The 'datapath' column will contain the local filenames where the 
    ## data can be found.
    inFile <- input$tidyFile

    if (is.null(inFile)) {return(NULL)}
    
    ## Easier just to match names so independent of order? i.e. do factor(data$whatever) after read.csv
    classes = c("factor", "integer", "factor", "factor", "integer", "numeric")
    userData = read.csv(inFile$datapath, colClasses=classes)##, header=input$header, sep=input$sep, quote=input$quote)
    return(userData)
  })

  output$userData = renderDataTable({
    if(is.null(getUserData())) {return(NULL)}
    results = getUserData()
  }, 
    options = list(lengthMenu = list(c(20, 50, -1), c('20', '50', 'All')),
                   pageLength = 10)
  )

#################################
## Plot input data
##################################
  output$myTabs.data = renderUI({
    if(is.null(getUserData())) {return(NULL)}

    nTabs = length(unique(getUserData()$Block))## input$nTabs
    myDataTabs = lapply(1:nTabs, function(i) {
      plotname <- paste("dataPlot", i, sep="")
      return(
        tabPanel(
          paste('Block', i), 
          plotOutput(plotname, width = "100%")
        )
      )
    })
    do.call(tabsetPanel, myDataTabs)
  })

  for (i in 1:maxBlocks){
    local({
      my_i <- i
      plotname <- paste("dataPlot", my_i, sep="")
 
      output[[plotname]] <- renderPlot({
        dataPlot(my_i)
      })#, height=2000)
    })
  }

  ## output$dataPlot <- renderPlot({
  dataPlot = function(local_i) {
    if(is.null(getUserData())) {return(NULL)}

    userData <<- getUserData()
    blockUserData = userData[userData$Block == local_i,]

    layer0 = ggplot() 
    layer1 = layer0 + geom_point(data = blockUserData, aes(x=time, y=value, colour=Sample, shape=Sample))
    layer2 = layer1 + geom_line(data = blockUserData, aes(x=time, y=value, colour=Sample))
    layer3 = layer2 + facet_wrap(~Line, scales="free_x") + scale_colour_brewer(type="qual",palette=6)
    layer4 = layer3 + theme(legend.position="top", legend.key.width = unit(6, "lines"), legend.key.height = unit(2, "lines"), legend.text = element_text(size = rel(1.5)), legend.title = element_text(size = rel(1.5), face="plain"))
    #layer4 = layer3 + theme(legend.position="top")
    print(layer4)
  }

#################################
## Fit models and plot
##################################
  getModelFits = reactive({
    if(input$action == 0) {return(NULL)}
    
    tmp = fitModels(userData, FLAG)
    mod <<- tmp$mod
    allResByLine = tmp$allResByLine

    return(allResByLine)## Return a list if don't do global mod assignment
  })

  output$modelPlot = renderPlot({
    if(is.null(getModelFits())) {return(NULL)}
    allResByLine = getModelFits()## Might need to change if getModelFits also returns mod for instance

    g0 = ggplot() 
    g1 = g0 + geom_point(data = userData, aes(x=time, y=value, colour=Sample, shape=Sample))
    g2 = g1 + geom_line(data=allResByLine, aes(x=time, y=mean2nd, colour=Sample))
    g3 <<- g2 + geom_ribbon(data=allResByLine, aes(x=time, ymax = CIhigh, ymin = CIlow, fill=Sample), alpha=0.3)
    g4 = g3 + facet_wrap(~Line, scales="free") + scale_colour_brewer(type="qual", palette=6) + scale_fill_brewer(type="qual", palette=6)
    g5 = g4 + theme(legend.position="top", legend.key.width = unit(6, "lines"), legend.key.height = unit(2, "lines"), legend.text = element_text(size = rel(1.5)), legend.title = element_text(size = rel(1.5), face="plain"))
    #g5 = g4 + theme(legend.position="top")
    print(g5)
  }, height=2000)

#################################
## Find half-lives and plot
##################################
## Note to self: shouldn't 1/2 life be independent of starting pods?
  calcHalfLives = reactive({
    if(input$halfAction == 0) {return(NULL)}

    allHalfLives <<- ldply(mod, halfLife)## return DF with half-life and 1/2 # of initial pods per line+rep
    allHalfLives = with(allHalfLives, cbind(colsplit(.id, pattern = "\\.", names = c("Block","Line.Sample")), halfLife, halfInitialPods))
    allHalfLives = with(allHalfLives, cbind(Block, colsplit(Line.Sample, pattern = "\\.", names = c("Line","Sample")), halfLife, halfInitialPods))
    allHalfLives$Sample = factor(allHalfLives$Sample)
    allHalfLives$halfLife = round(allHalfLives$halfLife, 4)

    return(allHalfLives)
  })

## show Block as well - need to check ouput of res = dlply in fitModels (also might need to look at the subsetting [1:6] e.g.
  output$halfLifeTable = renderDataTable({
    if(is.null(calcHalfLives())) {return(NULL)}
    results = calcHalfLives()
  }, 
    options = list(lengthMenu = list(c(20, 50, -1), c('20', '50', 'All')),
                   pageLength = 10,
                   rowCallback = I("function( nRow, aData) {ind = 3; $('td:eq('+ind+')', nRow).html( parseFloat(aData[ind]).toFixed(2) );}")## from http://stackoverflow.com/a/28093512 to display 2 decimal places ## ind=3 means column 4
    )
  )

  output$myTabs.halflives = renderUI({
    if(is.null(calcHalfLives())) {return(NULL)}

    nTabs = length(unique(getUserData()$Block))## input$nTabs
    ## myTabs = lapply(paste('Block', 1: nTabs), tabPanel, plotOutput("dummyPlot"))

    myTabs = lapply(1:nTabs, function(i) {
      plotname <- paste("plot", i, sep="")
      return(
        tabPanel(
          paste('Block', i), 
          plotOutput(plotname, width = "200%")
        )
      )
    })
    do.call(tabsetPanel, myTabs)
  })

  ##https://groups.google.com/forum/#!topic/shiny-discuss/kb6lIswv0ls
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  for (i in 1:maxBlocks){## set to be overly high https://groups.google.com/d/msg/shiny-discuss/kb6lIswv0ls/GhT4hd-qPRQJ
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
 
      output[[plotname]] <- renderPlot({
        halfLifePlot(my_i)
      }, height=2000)
    })
  }

  halfLifePlot = function(local_i){ 
    if(is.null(calcHalfLives())) {return(NULL)}
    if(is.null(getModelFits())) {return(NULL)}
    
    allHalfLives = calcHalfLives()
    allResByLine = getModelFits()## Might need to change if getModelFits also returns mod for instance
    blockUserData = userData[userData$Block == local_i,]
    allResByLine = allResByLine[allResByLine$Block == local_i,]
    allHalfLives = allHalfLives[allHalfLives$Block == local_i,]

    hl0 = ggplot() 
    hl1 = hl0 + geom_point(data = blockUserData, aes(x=time, y=value, colour=Sample, shape=Sample))
    hl2 = hl1 + geom_line(data=allResByLine, aes(x=time, y=mean2nd, colour=Sample))
    hl3 = hl2 + geom_ribbon(data=allResByLine, aes(x=time, ymax = CIhigh, ymin = CIlow, fill=Sample), alpha=0.3)
    hl3 = hl3 + geom_point(data=allHalfLives, aes(x=halfLife, y=halfInitialPods), shape=0, size=3)
    hl4 = hl3 + geom_segment(data=allHalfLives, aes(x=0, xend=halfLife, y=halfInitialPods, yend=halfInitialPods)) + geom_segment(data=allHalfLives, aes(x=halfLife, xend=halfLife, y=0, yend=halfInitialPods))
    hl5 = hl4 + facet_wrap(~Line,scales="free") + scale_colour_brewer(type="qual", palette=6) + scale_fill_brewer(type="qual", palette=6)
    hl6 = hl5 + theme(legend.position="top", legend.key.width = unit(6, "lines"), legend.key.height = unit(2, "lines"), legend.text = element_text(size = rel(1.5)), legend.title = element_text(size = rel(1.5), face="plain"))
    print(hl6)
  }

#################################
## Select plots to display
##################################
# Need to essentially subset the data DF maybe by having checkboxGroupInput boxes of lines you want to show
# pass the ones you want to keep to the DF
# then ggplot the result on button click
 # output$selectedPlots = renderPlot({
  selPlotInput = reactive({
    if(input$displaySelected == 0) {return(NULL)}
    if(is.null(getModelFits())) {return(NULL)}
    if(is.null(calcHalfLives())) {return(NULL)}
    isolate({    
      selLines = input$showPlotChoices
      if(is.null(selLines)) {return(NULL)}

cat(class(selLines),length(selLines), "*****HERE1*********\n")
      userData = userData[userData$Line %in% selLines,]
cat(selLines, "*****HERE2*********\n")
      allResByLine = getModelFits()## Might need to change if getModelFits also returns mod for instance
cat(selLines, "*****HERE3*********\n")
      allResByLine = allResByLine[allResByLine$Line %in% selLines,]
      allHalfLives = calcHalfLives()
      allHalfLives = allHalfLives[allHalfLives$Line %in% selLines,]
      
      sel0 = ggplot() 
      sel1 = sel0 + geom_point(data = userData, aes(x=time, y=value, colour=Sample, shape=Sample))
      sel2 = sel1 + geom_line(data=allResByLine, aes(x=time, y=mean2nd, colour=Sample))
      sel3 = sel2 + geom_ribbon(data=allResByLine, aes(x=time, ymax = CIhigh, ymin = CIlow, fill=Sample), alpha=0.3)
      sel4 = sel3 + geom_point(data=allHalfLives, aes(x=halfLife, y=halfInitialPods), shape=0, size=3)
      sel5 = sel4 + geom_segment(data=allHalfLives, aes(x=0, xend=halfLife, y=halfInitialPods, yend=halfInitialPods)) + geom_segment(data=allHalfLives, aes(x=halfLife, xend=halfLife, y=0, yend=halfInitialPods))
      sel6 = sel5 + scale_colour_brewer(type="qual", palette=6) + scale_fill_brewer(type="qual", palette=6)
      sel7 = sel6 + theme(legend.position="top", legend.key.width = unit(6, "lines"), legend.key.height = unit(2, "lines"), legend.text = element_text(size = rel(1.5)), legend.title = element_text(size = rel(1.5), face="plain"))
  #    print(sel7)
      if(input$sameXaxis=="fixed") {
        sel8 = sel7 + facet_wrap(~Line, scales="fixed")
      }
      else {
        sel8 = sel7 + facet_wrap(~Line,scales="free")
      }
      ##sel9 = grid.arrange(sel8block1, sel8block2, ncol=1))
    })
  })
#  }, height=1000)

  output$selectedPlots = renderPlot({
    if(is.null(selPlotInput())) {return(NULL)}
    ## override free_x if the user wants fixed
#    isolate({    
#      if(input$sameXaxis=="fixed") {
#        print(selPlotInput() + facet_wrap(~Line,scales="fixed"))
#      }
#      else {
    print(selPlotInput())
#      }
#    })
  }, height=1000)

  output$plotSelector <- renderUI({
    if(is.null(calcHalfLives())) {return(NULL)}
##    
##    allHalfLives = calcHalfLives()
#    str(allHalfLives$Line)
    allHalfLives = calcHalfLives()
    plotChoices = unique(allHalfLives$Line)
#cat(as.list(plotChoices), "\n")
    checkboxGroupInput('showPlotChoices', '',
                        choices = as.list(plotChoices),
                        #selected = as.list(plotChoices), 
			inline=TRUE)
  })

## observe({input$selectAll
##   plotChoices = unique(calcHalfLives()$Line)
##     updateCheckboxGroupInput(session, 'showPlotChoices', '',
##                         choices = as.list(plotChoices),
##                         selected = as.list(plotChoices), 
## 			inline=TRUE)
## })

  output$downloadSelPlots <- downloadHandler(
    filename <- function() {
      paste('selected_lines_plot', format(Sys.time(), "_%Y_%m_%d_%X"),'.pdf',sep='') 
    },
    content <- function(file) {
      pdf(file=file, width=12, height=8)
      print(selPlotInput())
      dev.off()
    },
    contentType = 'application/pdf'
  )

  output$downloadHalfLifeData <- downloadHandler(
#    if(is.null(calcHalfLives())) {return(NULL)}
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("halfLives", format(Sys.time(), "_%Y_%m_%d_%X"),'.csv')
    },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(calcHalfLives(), file, sep = ",", row.names = FALSE, quote=FALSE)
    }
  )
})
## #########################################################
## #########################################################
## #########################################################
## 
## 
## source("../models.R")
## conds = read.csv("../data/conditions.csv")
## 
## ## Fixed by defining my own operator as per http://stackoverflow.com/a/2769618
## epsilon <- 1e-10
## '%>=%' <- function(x, y) (x + epsilon > y)
## '%<=%' <- function(x, y) (x - epsilon < y)
## 
## emptyPlot = function() {
##   p = ggplot(data.frame()) + geom_blank() + xlim(0, 100) + ylim(0, 10) +
##     annotate("text", label = "No data!",
##              x = 50, y = 5, size = 10, colour = "red") + theme(
##                                          axis.text=element_blank(),
##                                          axis.title=element_blank(),
##                                          axis.ticks=element_blank())
##   return(p)
## }
## 
## shinyServer(function(input, output) {
## 
##   output$resultsReady = renderText({
##     if(input$calcModels == 0) {return(NULL)}
##     return("Results")
##   })
##   output$chosen = renderText({
##     if(!is.null(selected()))
##       ##      {return()
##       ##    } else {
##       {     paste("Found", nrow(selected()), "matching growth curves")
##           }
##   })
##   output$downloaded <- renderText({ if(!is.null(input$file1)) "Click go to plot this data" })
##   
##   selected = reactive({
##     if (input$search == 0)
##       return(NULL)
##     isolate({
## ###############################
##       ## dplyr way
## ###############################
##       t1 = input$tempRange[1]
##       pH2 = input$pHRange[2]
##       ## DEBUG conds = tbl_df(conds) ## reduces output
##       sel1 = filter(conds, !is.na(pH), !is.na(temp))
##       ## DEBUG print(nrow(sel1))
##       sel = filter(sel1, temp %>=% t1 & temp %<=% t2, pH %>=% pH1 & pH %<=% pH2)
##       ## DEBUG print(nrow(sel))
##     })
## 
##     return(sel)
##   })
##   
##   plotCurves = reactive({
##     if(is.null(selected())) {return()}
##     if(nrow(selected())==0) return(emptyPlot())
##     
##     sel = selected()
##     selectedData = salmonellaData[salmonellaData$key %in% sel$logc,]
## 
##     plot0 = ggplot(data = selectedData, aes(x=time, y=logc, colour=key,group=key))
##     return(plot3)
##   })
##   
##   getUserData = reactive({
##     ## input$file1 will be NULL initially. After the user selects and uploads a 
##     ## file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
##     ## columns. The 'datapath' column will contain the local filenames where the 
##     ## data can be found.
##     
##     inFile <- input$file1
##     
##     if (is.null(inFile))
##       return(NULL)
##     
##     userData = read.csv(inFile$datapath)##, header=input$header, sep=input$sep, quote=input$quote)
##     return(userData)
##   })
##   
##   output$downloadCurvesPlot <- downloadHandler(
##     filename <- function() {
##       paste('selected_curves_plot', Sys.Date(),'.png',sep='') },
##     content <- function(file) {
##       png(file, width = 900, height = 600, units = "px", pointsize = 12,
##           bg = "white", res = NA)
##       print(plotCurves())
##       dev.off()},
##     contentType = 'image/png'
##     )
##   
##   output$fittedModelsPlot <- renderPlot({
##     if(input$calcModels == 0) {return(emptyPlot())}
##     if(is.null(getUserData())) {return(emptyPlot())}
##     NSoutput = doNS()
## ####################
##     ## plotInputs = some selecting of NSoutput or functions using parameters
##     ## e.g. plotting selected models using parameters
## ####################
##     userData = getUserData()
##     layer0 = ggplot()
##     layer1 = layer0 + geom_line(data = plotInputs, aes(x=, y=, colour=model), size=1.1)
##     layer2 = layer1 + geom_point(data = userData, aes(x=time, y=logc, colour=key), size=4)
##     layer3 = layer2
##     print(layer3)
##     print(grid.arrange(H1layer3, H2layer3, H3layer3, nrow=1))
##   })
## 
##   output$fittedModelsSummary = renderTable({
##     if(input$calcModels == 0) {return(NULL)}
##     if(is.null(getUserData())) {return(NULL)}
##     results = doNS()
##   }, include.rownames = FALSE)
##   
## })
