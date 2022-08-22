tab_Item_row_data <- shiny::tabPanel(
  title = "Row Data EDA",
  icon  = icon("chart-bar", lib = "font-awesome"),
  ############################################################################
  shiny::fluidRow(
    column(
      width = 12,
      box(
        status = "primary",
        solidHeader = TRUE,
        title="Distribution of SMS - Ham / Spam Count",
        width = NULL,
        column(width = 5,
               shinydashboard::valueBox(
                 value =  ncol(sms[,c(1,2)]), 
                 subtitle = "Columns", 
                 icon = shiny::icon("chart-line", lib = "font-awesome"), 
                 color = "aqua", width = 2.5, href = NULL),
               shinydashboard::valueBox(
                 value =  nrow(sms[,c(1,2)]), 
                 subtitle = "Rows", 
                 icon = shiny::icon("users", lib = "font-awesome"), 
                 color = "purple", width = 2.5, href = NULL),
               box(
                 status = "primary",
                 width = NULL,
                 tags$h5("The Dtaset is a Collection of SMS messages Labeled as spam or legitimate")
                 ),
               box(
                 width = NULL,
                 tags$h5("About 86.6% of the messages are legitimate while the remaining 13.4% are spam.")
                 )
               ),
        column(width = 7,
               box(width = NULL,
                   shinycssloaders::withSpinner(shiny::plotOutput(outputId = "plot_column_pct"))
                   )
               )
        )
      ),
    column(width=12, offset = 0,
           box(
             width = NULL, 
             status = "primary",
             solidHeader = TRUE,
             shinycssloaders::withSpinner(DT::dataTableOutput("table11")),
             title = "SMS Text Dataset")
           )
    )
)