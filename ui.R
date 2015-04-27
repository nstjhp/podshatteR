library(shiny)
options(shiny.trace=TRUE)

cleaned = structure(list(Number = c(110L, 179L, 15L, 89L, 67L, 133L), Line = c(10722L, 
10722L, 10723L, 10723L, 10724L, 10724L), Sample = structure(c(1L, 
2L, 1L, 2L, 2L, 1L), .Label = c("1", "2", "3", "4"), class = "factor"), 
    value = c(20L, 20L, 20L, 20L, 20L, 19L), time = c(0L, 0L, 
    0L, 0L, 0L, 0L)), .Names = c("Number", "Line", "Sample", 
"value", "time"), row.names = c(NA, 6L), class = "data.frame")

shinyUI(navbarPage("RIT analysis",
  tabPanel('About',
    fluidPage(
      fluidRow(
        column(6,
          includeMarkdown('about1.md')## table looks naff...
        ),
        column(6,
          includeMarkdown('about2.md')
        )
      )
    )
  ),
  tabPanel("Upload & Plot data",
    fluidRow(
      column(4,
        wellPanel(
 #         tags$head(tags$link(rel="stylesheet", type="text/css",
 #                        href="simplelittletable.css")),
          fileInput('tidyFile', 'Upload your data: Choose CSV file',
          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
        ),
        dataTableOutput('userData')## Still in column but not wellPanel
      ),
      column(8,
        plotOutput("dataPlot", width = "200%")
      )
    )
  ),
  tabPanel("Fit models",
    verticalLayout(
      wellPanel(
        helpText("Click to fit and plot exponential decay models", 
                 "(this will take some time to calculate the CIs)."),
        actionButton("action", label = "Fit model")
      ),
        plotOutput("modelPlot", width = "200%")
    )
  ),
  tabPanel("Calculate half-lives",
    fluidRow(
      column(4,
        wellPanel(
          helpText("Click to show the half-lives."),
          actionButton("halfAction", label = "Show half-lives"),
            conditionalPanel(condition = "input.halfAction",
              downloadButton('downloadHalfLifeData', 'Download')
            )
        ),
        dataTableOutput('halfLifeTable')## in column but not wellPanel
      ),
      column(8,
uiOutput('mytabs')## http://stackoverflow.com/a/19470562/3275826
#        tabsetPanel(
#          tabPanel("Block1", 
#            plotOutput("dummyPlot")
#          ),
#          tabPanel("Block2", 
#            plotOutput("halfLifePlot", width = "200%")
#          )
#        )
      )
    )
  ),
  tabPanel("Select plots",
    verticalLayout(
      wellPanel(
        helpText("Here you can choose the lines you'd like to see",
                 "as a subset of the full dataset."),
  ## actionButton('selectAll', 'Select All'),
        uiOutput("plotSelector"),
radioButtons('sameXaxis', 'x-axis layout', 
c('Each panel has a individual x-axis'='free_x', 
  'x-axis is fixed across panels'='fixed'), selected='free_x', inline=TRUE),
        actionButton("displaySelected", label = "Show plots"),
        conditionalPanel(condition = "input.displaySelected",
          downloadButton('downloadSelPlots', 'Download')
        )
      ),
      plotOutput("selectedPlots", width = "100%")
    )
  )
))

## #########################################################
## #########################################################
## #########################################################
## foodVars  = c("Beef", "Salmon", "Bread", "Cheese")
## 
## ## Define UI for bacterial growth application
## shinyUI(pageWithSidebar(
##   
##   ## Application title
##   headerPanel(""),
## 
##   ## Sidebar with controls to select 
##   sidebarPanel2(
##     conditionalPanel(
##         condition = "input.Organisms == true",
##       checkboxInput("foodType", "Food Type"), 
##       checkboxGroupInput('showHypChoices', 'Choose hypotheses to compare',
##                          choices = hypChoices, selected = hypChoices),
##       actionButton(inputId="calcModels", label="Calculate")
##         ),
## 
##       sliderInput("NaClRange",
##                   "NaCl:",
##                   min = 0, max = 5, value = c(0.5, 4.7), step = 0.01),
## 
##       actionButton(inputId = "search", label = "Search")
##       ),
## 
##     conditionalPanel(
##       condition="input.theTabs=='Upload & plot data'",
##       fileInput('file1', 'Upload your data: Choose CSV file',
##                 accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
##       conditionalPanel(
##         condition = "output.downloaded == 'Click go to plot this data'",
##         tags$hr(),
##         actionButton(inputId="gobutton", label="Go")
##         ),
##       textOutput("downloaded")
##       ),
##     class = "span3"
##     ),
## 
##   mainPanel2(
##     tags$head(tags$link(rel="stylesheet", type="text/css",
##                         href="simplelittletable.css")),
##     tabsetPanel(
##       id ="theTabs",
##       tabPanel("Find matches", plotOutput("userGrowthCurve"),
##                h5(textOutput("chosen")),
##                tableOutput('userData')),##, value=1),
##                plotOutput("growthCurve"),
##                downloadButton('downloadCurvesPlot','Download Graph')),##, value=2),
##       
##       tabPanel("Compare models",
##                h2(textOutput("resultsReady")),
##                plotOutput("exampleNSPlot"),
##                div(class="row-fluid",
##                    div(class="span5",tableOutput('exampleNStable')),
##                    div(class="span2", h4(textOutput("Interpretation")),
##                        textOutput("InterpretationSentence")),
##                    div(class="span5",tableOutput('JeffreysScale'))),
##                value=3)
##       ),
##     class = "span9"
##     )
##   )
## )
