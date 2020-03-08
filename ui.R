library(shiny)
options(shiny.trace=TRUE)

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
        DT::dataTableOutput('userData')## Still in column but not wellPanel
      ),
      column(8,
         uiOutput('myTabs.data')## http://stackoverflow.com/a/19470562/3275826
         ## plotOutput("dataPlot", width = "200%")
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
        uiOutput('myTabs.modelfits')## http://stackoverflow.com/a/19470562/3275826
        ##plotOutput("modelPlot", width = "200%")
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
        DT::dataTableOutput('halfLifeTable')## in column but not wellPanel
      ),
      column(8,
         uiOutput('myTabs.halflives')## http://stackoverflow.com/a/19470562/3275826
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
        c('Axes vary across rows/columns'='free',
          'Axes are fixed across rows/columns'='fixed'), selected='free', inline=TRUE),
        actionButton("displaySelected", label = "Show plots"),
        sliderInput("selPlotHeight", "Plot height:",
                    min=0, max=20, value=4, step = 0.1, width="40%"),
        sliderInput("selPlotWidth", "Plot width:",
                    min=0, max=20, value=6, step = 0.1, width="40%"),
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
