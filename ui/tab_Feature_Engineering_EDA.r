tab_Feature_Engineering_EDA <- shiny::tabPanel(
  title = "Feature Engineering EDA",
  icon  = icon("chart-bar", lib = "font-awesome" ),
  ############################################################################
  shiny::fluidRow(
    column(
      width = 12,
      box(
        status = "primary",
        solidHeader = TRUE,
        column(
          width = 5,
          shiny::radioButtons("plotType_fe", "Select Plot Type", c("BoxPlot"="p", "Histogram"="h")),
          
          shiny::selectInput(inputId = "plotname_fe", label = "Select Feature Name:", choices = plotname_fe_list)
        ),
        column(width=7, offset = 0,
          box(width = NULL, shinycssloaders::withSpinner(shiny::plotOutput(outputId = "plot_fe_pct")))
        ),
        title = "Plotting SMS Text Dataset Feature Engineering",
        width = NULL
      )
      
    ),
    column(
      # width should be between 1 and 12
      width=12, offset = 0,
      box(
        width = NULL, 
        status = "primary",
        solidHeader = TRUE,
        shinycssloaders::withSpinner(shiny::tableOutput("table_fe_summary")),
        title = "SMS Text Dataset Feature Engineering Summary Table")
    ),
    column(
      # width should be between 1 and 12
      width=12, offset = 0,
      box(
        width = NULL, 
        status = "primary",
        solidHeader = TRUE,
        shinycssloaders::withSpinner(DT::dataTableOutput("table_fe")),
        title = "SMS Text Dataset Feature Engineering"
      )
    )
  )
)
