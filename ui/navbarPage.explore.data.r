plotname_fe_list <- c("length of each message" = "1",
                      "percentage of capital letters" = "2",
                      "percentage of digit numbers" = "3",
                      "count of '!' from text" = "4",
                      "count of capital letters" = "5",
                      "count of digit numbers" = "6",
                      "count of digit numbers in series as whole" = "7",
                      "check if there a url in text" = "8",
                      "Variable Correlation" = "9")
################################################################################
## Feature Engineering side bar tab
## EXploring data tab
################################################################################
source("ui/tab_Feature_Engineering_EDA.r")


navbarPage.explore.data <- shiny::navbarPage(
  title    = "Explore Data",
  id       = "navbarPage_id",
  selected = NULL,
  position = "static-top",
  inverse  = TRUE,
  fluid    = TRUE,
  icon     = NULL,
  ##########################################################################
  tab_Feature_Engineering_EDA,
  ##########################################################################
  ## Text Analysis
  ## EXploring data tab_tf_idf_EDA <-
  ##########################################################################
  shiny::tabPanel(
    title = "Text Analysis",
    value = "TextAnalysis",
    icon = shiny::icon("acquisitions-incorporated", lib = "font-awesome"),
    fluidRow(
      column(
        # width should be between 1 and 12
        width=12, offset = 0,
        box(
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          title = "Text Length Vs. Ham/Spam \"SMS Text Length\"",
          column(
            width = 6,
            shinycssloaders::withSpinner(shiny::plotOutput(outputId = "plot_ta_tl")),
            shinycssloaders::withSpinner(shiny::plotOutput(outputId = "plot_ta_tl_b"))
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(shiny::plotOutput(outputId = "plot_ta_tlc")),
            shinycssloaders::withSpinner(shiny::plotOutput(outputId = "plot_ta_tlc_b"))
          )

        )
      )
    )
  ),
  ##########################################################################
  ## Text Analysis tables
  ## EXploring data tab_tf_idf_EDA <-
  ##########################################################################
  shiny::tabPanel(
    title = "Text Analysis Tables",
    value = "TextAnalysist",
    icon = icon("acquisitions-incorporated", lib = "font-awesome"),
    fluidRow(
      column(
        width = 12,
        box(
          column(
            width = 4,
            shiny::radioButtons("tableType_ta", "Select Table Content", c("SMS Text With Corpus" = "c" ,"DTM"="d", "TF_IDF"="tf") )
          ),
          column(
            width = 4,
            shiny::textInput(inputId= "start_c", label = "Start Columns",
                      value = 1, width = NULL, placeholder = NULL)
          ),
          column(
            width = 4,
            shiny::textInput(inputId= "end_c", label = "End Columns",
                      value = 5, width = NULL, placeholder = NULL)
          ),
          column(
            # width should be between 1 and 12
            width=12, offset = 0,
            box(
              width = NULL,
              shinycssloaders::withSpinner(dataTableOutput("table_dtm"))
            )
          ),
          status = "primary",
          title = "SMS Text Analysis Table",
          width = NULL
        )
      )
    )
  ),
  ##########################################################################
  ## Word Cloud
  ## EXploring data tab_wordcloud_EDA <-
  ##########################################################################
  shiny::tabPanel(
    title = "Word Cloud",
    value = "WordCloud",
    icon = icon("file-word", lib = "font-awesome"),
    ############################################################################
    fluidRow(
      column(
        width = 12,
        box(
          column(
            width = 3,
            shiny::radioButtons("plotType_wc", "Select Plot Type", c("Horizontal Bar Plot"="b", "Word Cloud"="wc"))
          ),
          column(
            width = 3,
            shiny::textInput(inputId= "top_wc", label = "Top Words",
                      value = 10, width = NULL, placeholder = NULL),
            shiny::textInput(inputId= "top_bp", label = "Minimum Frequency",
                      value = 50, width = NULL, placeholder = NULL)
          ),
          column(
            width = 6,
            shiny::selectInput(inputId = "plotname_wc", label = "Select Feature Name:",
                        choices = c("Top common words in both Ham/spam" = "1",
                                    "TOP Spam Words" = "2",
                                    "TOP Ham Words" = "3")
            )
          ),
          column(
            # width should be between 1 and 12
            width=12, offset = 0,
            box(
              width = NULL,
              status = "primary",
              solidHeader = TRUE,

              shinycssloaders::withSpinner(shiny::plotOutput(outputId = "plot_wc"))
            )
          ),
          title = "Plotting SMS Text Dataset Term Counts",
          width = NULL
        )
      )
    )
  ),

  ##########################################################################
  ## Sentiment Analysis
  ## EXploring data tab_sentiment_EDA <-
  ##########################################################################
  shiny::tabPanel(
    title = "Sentiment Analysis NRC",
    value = "SentimentAnalysis",
    icon = shiny::icon("comment-dots", lib = "font-awesome"),
    ##mainPanel("Sentiment Analysis")
    ##########################################################################
    ## Sentiment Analysis tables
    ##
    ##########################################################################
    fluidRow(
      column(
        # width should be between 1 and 12
        width=12, offset = 0,
        box(
          width = 6,
          status = "primary",
          solidHeader = FALSE,
          title = "Plotting SMS Text Dataset Sentiment Distribution",
          shinycssloaders::withSpinner(shiny::plotOutput(outputId = "plot_sa_dp"))
        ),
        box(
          width = 6,
          status = "primary",
          solidHeader = FALSE,
          title = "Plotting SMS Sentiment Scores with Ham/Spam",
          shinycssloaders::withSpinner(shiny::plotOutput(outputId = "plot_sa_dphs"))
        )
      ),
      column(
        # width should be between 1 and 12
        width=12, offset = 0,
        box(
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          shinycssloaders::withSpinner(dataTableOutput("table_sa")),
          title = "Sentiment Analysis Table"
        )
      )
    )
  )
)
