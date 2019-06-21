



createERUIa <- function(tablesOnly=F){
  fluidPage(
     # h3("Exploring regression models"),
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
    fluidRow(column(1,offset=0, actionButton(inputId="all_terms", label="Use all terms")),
             column(2,offset=1, checkboxInput(inputId="fixedscales", label="Fixed scales", value=TRUE)),
             column(7, offset=1,verbatimTextOutput("info"))),
    tags$head(tags$style("#info{font-size: 9px;}")),
    tags$br(),
    if (!tablesOnly) {

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
                                       brush = brushOpts(id = "plot_brush", resetOnNew = T))))
      },

    if (!tablesOnly) fluidRow( column(8,offset=4,verbatimTextOutput("infoBrushed"))),
    tags$head(tags$style("#infoBrushed{font-size: 9px;}"))
     )

}

#' Constructs UI for Exploratory Regression app
#' @param tablesOnly if TRUE, shows Plots 1-3 only.
#'
#' @return the UI
createERUI <- function(tablesOnly=F){
  topflex <- c(3,3)
  if (tablesOnly) topflex <- c(3,NA)
  miniPage(
    gadgetTitleBar("Exploring regression models"),
    tags$style(type = "text/css",
               "label { font-size: 11px; }"
    ),
    # tags$head(tags$style(HTML("
    #     .selectize-input, .selectize-dropdown {
    #                           font-size: 10px;}"))),

    tags$head(tags$style("#info{font-size: 9px;}")),
    tags$head(tags$style("#infoBrushed{font-size: 9px;}")),
    tags$head(tags$style("#all_terms{font-size: 10px;}")),
    tags$head(tags$style("#remove_brushed{font-size: 10px;}")),
    tags$head(tags$style("#restore_all{font-size: 10px;}")),

    tags$style(
      type = 'text/css',
      ".selectize-input { font-size: 10px; line-height: 10px; }
               .selectize-dropdown { font-size: 10px; line-height: 10px; }
  .selectize-control.single div.item {
        padding-top: 0px; padding-bottom: 0px;
      }
               "),

     miniContentPanel(scrollable = FALSE,
    padding=10,

    fillCol(
      flex=topflex,
      fillRow(
        flex=c(1,1),
        fillCol(
          fillRow(
            flex=c(1,NA,1,NA,1,NA,1),
            fillCol(),
            selectInput(inputId = "stat",
                        label = "Plot1 statistic",
                        choices = c("t stat", "CI", "CI stdX", "F stat","Adj. SS"),
                        selected = "t stat", width="80px"),
            fillCol(),
            selectInput(inputId = "alpha",
                        label = "Alpha",
                        choices = c("0.25"=.25,"0.02"= .2,"0.15"=.15,"0.1"=.1,"0.05"=.05,"0.01"=.01,"0.001"=.001),
                        selected = .05, width="70px"),
            fillCol(),

            checkboxInput(inputId="fixedscales", label="Fixed scales", value=TRUE, width="100px"),
            fillCol()
          ),
          fillRow(),
          plotOutput("barPlotA", click="plot_clickA",dblclick = "plot_dblclickA",height="100%",width="100%"),

          flex=c(NA,.2,8)
        ),

        fillCol(
          fillRow(
            flex=c(1,NA,1,NA,1),
            fillCol(),
            selectInput(inputId = "order1",
                        label = "Plot2 order",
                        choices = list("Default", "RevDefault","Forward",
                                       "Backward", "Random", "Interact"),
                        selected = "Default", width="100px"),
            fillCol(),
            selectInput(inputId = "order2",
                        label = "Plot2 order",
                        choices = list("Default", "RevDefault","Forward",
                                       "Backward", "Random", "Interact"),
                        selected = "RevDefault", width="100px"),
            fillCol()
          ),
          fillRow(),
          plotOutput("barPlotS",click = "plot_clickS",
                     dblclick = "plot_dblclickS",height="100%",width="100%"),
          # verbatimTextOutput("info"),

          flex=c(NA,.2,8)
        )
      ),
      if (!tablesOnly)
      fillRow(
        flex=c(2,5),
        fillCol(
          flex=c(NA,NA,NA,1,1),
          # fillRow(),
          radioButtons(inputId = "PCP", "Plot what?",
                       choices = c("Variables",Resid="Residuals", Hat="Hatvalues", "CooksD"), inline=TRUE),
          checkboxGroupInput("Res", "Resid options:",
                             c("Absolute" = "abs",
                               "Difference" = "diff"), inline=TRUE),
          radioButtons(inputId = "From", "Order from?",
                       c("Plot1" = "Plot1",
                         "Plot2" = "Plot2","Plot3" = "Plot3"), inline=TRUE),
          fillRow(actionButton(inputId="remove_brushed", label="Remove Brushed"),
                  actionButton(inputId="restore_all", label="All cases")),
          fillRow()
        ),
        fillCol(
          flex=c(7,2),

          plotOutput("pcp", dblclick = "pcp_dblclick", width="100%",height="100%",
                     brush = brushOpts(id = "plot_brush", resetOnNew = T)),
          fillRow( verbatimTextOutput("infoBrushed"))
        )
      )
    )
  )

)
}

