


#' Constructs UI for Exploratory Regression app
#'
#'
#' @return the UI
createERUI <- function(){
  fluidPage(
     h3("Exploring regression models"),
    #  tags$style(type = "text/css",
    #            "label { font-size: 12px; }"
    # ),
    # tags$head(tags$style(HTML("
    #     .selectize-input, .selectize-dropdown {
    #                           font-size: 12px;}"))),
    #

    fluidRow(column(2, offset=0,
                    selectInput(inputId = "stat",
                                label = "Plot1 statistic",
                                choices = list("t stat", "CI", "CI stdX", "F stat","Adj. SS"),
                                selected = "t stat")),
             column(2, offset=0,
                    selectInput(inputId = "alpha",
                                label = "Alpha",
                                choices = list(.25, .2,.15,.1,.05,.01,.001),
                                selected = .05)),
             column(2, offset=2,
                    selectInput(inputId = "order1",
                                label = "Plot2 order",
                                choices = list("Default", "RevDefault","Forward",
                                               "Backward", "Random", "Interact"),
                                selected = "Default")),
             column(2, offset=0,
                    selectInput(inputId = "order2",
                                label = "Plot3 order",
                                choices = list("Default","RevDefault", "Forward",
                                               "Backward", "Random","Interact"),
                                selected = "RevDefault"))),
    fluidRow(column(4,plotOutput("barPlotA", click="plot_clickA", height="300px")),
             column(6,offset=1,plotOutput("barPlotS",click = "plot_clickS",
                                 dblclick = "plot_dblclickS",height="300px", width = "90%"))),
    fluidRow(column(1,offset=1, actionButton(inputId="all_terms", label="Use all terms")),
             column(8, offset=2,verbatimTextOutput("info"))),
    tags$head(tags$style("#info{font-size: 9px;}")),
    tags$br(),
    sidebarLayout(sidebarPanel(
                                # selectInput(inputId = "PCP",
                                #            label = "Plot what?",
                                #            choices = list("Variables","Residuals", "Hatvalues"),
                                #            selected = "Variables"),
                               radioButtons(inputId = "PCP", "Plot what?",
                                            choices = list("Variables","Residuals", "Hatvalues"), inline=TRUE),

                               checkboxGroupInput("Res", "Residual options:",
                                                  c("Absolute" = "abs",
                                                    "Difference" = "diff"), inline=TRUE),
                               radioButtons(inputId = "From", "Order from?",
                                            c("Plot1" = "Plot1",
                                              "Plot2" = "Plot2","Plot3" = "Plot3"), inline=TRUE),

                               # checkboxInput(inputId = "ResA",
                               #               label = "Absolute Residuals",
                               #               value=FALSE),
                               # checkboxInput(inputId = "ResD",
                               #               label = "Difference Residuals",
                               #               value=FALSE),

                               actionButton(inputId="remove_brushed", label="Remove Brushed"),
                               actionButton(inputId="restore_all", label="All cases")),

                  mainPanel(plotOutput("pcp", dblclick = "pcp_dblclick", height="350px",
                                       brush = brushOpts(id = "plot_brush", resetOnNew = T)))),

    fluidRow(column(8,offset=4,verbatimTextOutput("infoBrushed"))),
    tags$head(tags$style("#infoBrushed{font-size: 9px;}"))
    # tags$head(tags$style("#remove_brushed{font-size: 12px;}")),
    # tags$head(tags$style("#restore_all{font-size: 12px;}")),
    #

  # fluidRow(
  #   column(4, offset=0,fileInput("sourceF", "Choose source file with linear model", accept="R")))

  # if (fileOption)
  #   fluidRow(
  #     column(4, offset=0,fileInput("sourceF", "Choose source file with linear model", accept="R")))


    )
}


#tags$div(fileInput('sourceF',"Choose source file with linear model", accept="R"),id='sourceF')

#fileInput("sourceF", "Choose source file with linear model", accept="R")
