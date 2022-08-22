
## -------------------------- shinyUI --------------------------------------- ## 
library('shiny')
library('shinythemes')
library('shinymanager')
library('shinycssloaders')
library('shinydashboard')
library("shinyjs")
##library(shiny.semantic)
library("markdown")
library("httr")


library("DT")
library("here")

#setwd("D:/data mining project/sms.r.shiny")
model.matrics <- readRDS(file = "data/model.matrics.rds")
sms <- readRDS(file = "data/sms.rds")

reviews.clean  <- readRDS(file = "data/hotel/reviews.clean.rds")
sentiments.nrc <- readRDS(file = "data/hotel/sentiments.nrc.rds")

################################################################################
## https://cran.r-project.org/web/packages/shinydashboard/shinydashboard.pdf
## https://cran.r-project.org/web/packages/shiny.semantic/shiny.semantic.pdf
################################################################################






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
## raw table
## raw data
################################################################################
source("ui/tab_Item_row_data.r")




################################################################################
## navigation bar main declaration
## EXploring data 
################################################################################
source("ui/navbarPage.explore.data.r")
################################################################################
## navigation bar
## Association Rules
################################################################################
body_assocationmodel <- navbarPage(
  title    = "Assocation Rules",
  id       = "navbarPage_id3", 
  selected = NULL,
  position = "static-top",
  inverse  = TRUE,
  fluid    = TRUE,
  icon     = NULL, 
  ##########################################################################
  ## Plotting
  ##########################################################################
  tabPanel(
    title = "Plotting Association Rules", 
    value = "PlottingAssociationRules", 
    icon  = icon("chart-bar", lib = "font-awesome"),
    fluidRow(
      column(
        width = 12,
        box(
          column(
            width = 6,
            radioButtons("assocsplot", "Select Plotting Type:", c("Confidence Vs Support"="1", 
                                                                  "Words Associated With Spam"="2", 
                                                                  "Words Associated With Ham"="3") )
          ),
          column(
            width = 6,
            textInput(inputId= "Nassocs", label = "Number of Words associated", 
                      value = 34, width = NULL, placeholder = NULL)
          ),
          column(
            # width should be between 1 and 12
            width=12, offset = 0,
            box(
              width = NULL, 
              plotOutput(outputId = "plot_assocs")
              )
            ),
          width = NULL,
          title = "Plotting Rules"
          ) 
        )
      )
    ),
  
  ##########################################################################
  ## Association Rules tables 
  ##########################################################################
  tabPanel(
    title = "Association Rules tables ", 
    value = "AssociationRulestables ", 
    icon = icon("acquisitions-incorporated", lib = "font-awesome"),
    fluidRow(
      column(
        width = 12,
        box(
          width = NULL, 
          status = "primary",
          solidHeader = TRUE,
          column(
            width = 12,
            selectInput(inputId = "sortby", label = "Sort By:",
                      choices = c("lift" = "lift", 
                                  "support" = "support",
                                  "confidence" = "confidence",
                                  "coverage" = "coverage",
                                  "count" = "coverage")
                      )
            ),
          tabBox(
            selected = "tab11",
            width = 12,
            tabPanel(
              title = "Table Rules",
              value = "tab11",
              dataTableOutput("table_assocs")
              ),
            tabPanel(
              title = "Summary Table", 
              value = "tab12",
              verbatimTextOutput("summary", placeholder = TRUE)
              )
            )
          )
        )
      )
  ))
##########################################################################
## Data
## 
##########################################################################
tabPanel_cls_Data <- tabPanel(
  title = "Data", 
  value = "DataModel", 
  icon = icon("acquisitions-incorporated", lib = "font-awesome"),
  fluidRow(
    column(
      width = 12,
      box(
        status = "primary",
        solidHeader = TRUE,
        column(
          # width should be between 1 and 12
          width=7, offset = 0,
          radioButtons("selectdata", "Select Data Tables:", 
                       c("With Sparse Terms not less than 0.995"="d", 
                         "With Sparse Terms not less than 0.995 + features"="df", 
                         "With SVD Dimensionalty reduction"="s",
                         "With SVD Dimensionalty reduction + features"="sf"))
        ),
        column(
          width = 3,
          textInput(inputId= "start_ctr", label = "Start Columns", 
                    value = 1, width = NULL, placeholder = NULL),
          textInput(inputId= "end_ctr", label = "End Columns", 
                    value = 10, width = NULL, placeholder = NULL)
        ),
        fluidRow(
          tabBox(
            side = "right", 
            selected = "tab1",
            width = 12,
            tabPanel(
              title = "Training Data (70%)",
              value = "tab1",
              dataTableOutput("train_table")
            ),
            tabPanel(
              title = "Training Data Summary", 
              value = "tab2",
              verbatimTextOutput("summary_train_table", placeholder = TRUE)
            ),
            tabPanel(
              title = "Testing Data (30%)", 
              value = "tab3",
              dataTableOutput("test_table")
            ),
            tabPanel(
              title = "Testing Data Summary", 
              value = "tab4",
              verbatimTextOutput("summary_test_table", placeholder = TRUE)
            )
          )
        ),
        width = NULL
      )
      
    )
  )
)

tabPanel_cls_Variables_summary <- tabPanel(
  title = "Variables summary", 
  value = "Variables_summary", 
  icon = icon("acquisitions-incorporated", lib = "font-awesome"),
  fluidRow(
    column(
      # width should be between 1 and 12
      width=12, offset = 0,
      box(
        width = NULL, 
        status = "primary",
        column(
          # width should be between 1 and 12
          width=6, offset = 0,
          selectInput(inputId = "cls_target", label = "Independent Variables",
                      choices = "Label"),
          box(
            width = NULL, 
            status = "primary",
            verbatimTextOutput("summary_target", placeholder = TRUE),
            title = "Target variable Summary"
            )
          ),
        column(
          # width should be between 1 and 12
          width=6, offset = 0,
          selectInput(inputId = "cls_feature", label = "Dependent Variables",
                      choices = names(sms[, c(3,4,5,6,7,8)])),
          box(
            width = NULL, 
            status = "primary",
            verbatimTextOutput("summary_feature", placeholder = TRUE),
            title = "Feature variable Summary"
            )
          )
        )
      )
    )
  )

##########################################################################
## model selectionsvalueBoxOutput(outputId="Model"),
##########################################################################
tabPanel_cls_Model_Selection<- tabPanel(
  title = "Model Selection", 
  value = "Model_Selection", 
  icon = icon("acquisitions-incorporated", lib = "font-awesome"),
  fluidRow(
    column(
      # width should be between 1 and 12
      width=12, offset = 0,
      box(
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        column(
          # width should be between 1 and 12
          width=6, offset = 0,
          selectInput(inputId = "selectModel", label = "Select Model:",
                      choices = unique(model.matrics$Model), width = NULL),
          selectInput(inputId = "selectMethod", label = "Select Method:",
                      choices = unique(model.matrics$Method), width = NULL)
          
        ),
        box(
          width = NULL,
          column(
            # width should be between 1 and 12
            width=5, offset = 1,
            fluidRow(
              valueBoxOutput(outputId="TP"),
              valueBoxOutput(outputId="FN")
              ),
            fluidRow(
              valueBoxOutput(outputId="FP"),
              valueBoxOutput(outputId="TN")
              )
            )
          ),
        column(
          width = 5,
          fluidRow(
            width = NULL, 
            valueBoxOutput(outputId="accuracy"), 
            valueBoxOutput(outputId="Precision"),
            valueBoxOutput(outputId="AUC")
            ),
          fluidRow(
            width = NULL, 
            valueBoxOutput(outputId="sensitivity"),
            valueBoxOutput(outputId="Specificity"),
            valueBoxOutput(outputId="negative")
            )
          ),
        column(
          # width should be between 1 and 12
          width=7, offset = 0,
          plotOutput(outputId = "aucplot")
          )
        )
      )
    )
  )




################################################################################
## navigation bar
## Classification
################################################################################
body_clsmodel <- navbarPage(
  title    = "Classification Model",
  id       = "navbarPage_id2", 
  selected = NULL,
  position = "static-top",
  inverse  = TRUE,
  fluid    = TRUE,
  icon     = NULL, 
  ##########################################################################
  ## Data
  ## 
  ##########################################################################
  tabPanel_cls_Data,
  ##########################################################################
  ## Variables summary
  ## 
  ##########################################################################
  tabPanel_cls_Variables_summary,
  ##########################################################################
  ## model selectionsvalueBoxOutput(outputId="Model"),
  ##########################################################################
  tabPanel_cls_Model_Selection,
  ##########################################################################
  ## 
  ##  
  ##########################################################################
  tabPanel(
    title = "Models Comparsion", 
    value = "Models_Comparsion", 
    icon = icon("file-word", lib = "font-awesome"),
    ############################################################################
    fluidRow(
      column(
        # width should be between 1 and 12
        width=12, offset = 0,
        box(
          width = NULL, 
          status = "primary",
          solidHeader = TRUE,
          dataTableOutput("metric_table1"),
          title = "Models Comparsion Summary"
          
        )
      )
    )
  )
)

################################################################################
## navigation bar
## K-Mean Model
################################################################################
body_kmeanmodel <- navbarPage(
  title    = "K-Mean Model",
  id       = "navbarPage_id4", 
  selected = NULL,
  position = "static-top",
  inverse  = TRUE,
  fluid    = TRUE,
  icon     = NULL, 
  ##########################################################################
  ## Text Analysis
  ##  
  ##########################################################################
  tabPanel(
    title = "Best K", 
    value = "BestK", 
    icon = icon("acquisitions-incorporated", lib = "font-awesome"),
    fluidRow(
      column(
        width = 12,
        box(
          column(
            width = 4,
            radioButtons("clustType", "Select Plot Type", c("WSS"="s", "Silhouette"="h") ),
            selectInput(inputId = "kmean_max", label = "Select Max K :",
                        choices = c(20:2)
            )
          ),
          column(
            # width should be between 1 and 12
            width=8, offset = 0,
            box(
              width = NULL, 
              plotOutput(outputId = "plot_bestk")
            )
          ),
          title = "Finding best K For Clustering",
          width = NULL
        )
      )
    )
  ),
  ##########################################################################
  ## 
  ## 
  ##########################################################################
  tabPanel(
    title = "Explore K-mean", 
    value = "Explore_K_mean", 
    icon = icon("acquisitions-incorporated", lib = "font-awesome"),
    fluidRow(
      tabBox(
        side = "left", 
        selected = "tab24",
        width = 12,
        tabPanel(
          title = "Perform Kmean Clustering", 
          value = "tab24",
          box(
            column(
              width = 4,
              selectInput(inputId = "kmean_k", label = "Select K :", choices = c(2:20)),
              radioButtons("clustplottype", "Select Plot Type", c("Cluster Plot"="c", "Barplot"="b") )
            ),
            column(
              # width should be between 1 and 12
              width=8, offset = 0,
              box(
                width = NULL, 
                plotOutput(outputId = "plot_k")
              )
            ),
            width = NULL
          )
        ),
        tabPanel(
          title = "Plotting Cluster Text Terms & Counts",
          value = "tab21",
          box(
            column(
              width = 4,
              radioButtons("plotType_wcc", "Select Plot Type", c("Horizontal Bar Plot"="b", "Word Cloud"="wc")),
              textInput(inputId= "top_wcc", label = "Top Words", 
                        value = 10, width = NULL, placeholder = NULL),
              textInput(inputId= "top_bpc", label = "Minimum Frequency", 
                        value = 50, width = NULL, placeholder = NULL),
              selectInput(inputId = "Clustname_wcc", label = "Select Cluster Name:",
                          choices = c(1:20)
              )
            ),
            column(
              # width should be between 1 and 12
              width = 8, offset = 0,
              box(
                width = NULL, 
                status = "info",
                solidHeader = TRUE,
                plotOutput(outputId = "plot_wcc")
              )
            ),
            width = NULL
          )
        ),
        tabPanel(
          title = "Data with Clustering Summary", 
          value = "tab22",
          verbatimTextOutput("summary_clust_table", placeholder = TRUE)
        ),
        tabPanel(
          title = "Data with Clustering", 
          value = "tab23",
          dataTableOutput("clust_table")
        )
      )
    )
  )
)



################################################################################
## navigation bar
## Hotel Reviews 
################################################################################
body_hotelreview <- navbarPage(
  title    = "Hotel Rviews",
  id       = "navbarPage_id50", 
  selected = NULL,
  position = "static-top",
  inverse  = TRUE,
  fluid    = TRUE,
  ################################################################################
  ## raw table
  ## raw data
  ################################################################################
  tabPanel(
    title = "Raw Data EDA",
    icon  = icon("chart-bar", lib = "font-awesome"),
    ############################################################################
    fluidRow(
      column(
        width = 12,
        box(
          status = "primary",
          solidHeader = TRUE,
          title="Hotel Reviews Summary",
          width = NULL,
          column(
            width = 5,
            valueBox(
              value =  ncol(reviews.clean[,c(1:5)]), 
              subtitle = "Columns", 
              icon = shiny::icon("chart-line", lib = "font-awesome"), 
              color = "aqua", width = 2.5, href = NULL),
            valueBox(
              value =  nrow(reviews.clean[,c(1:5)]), 
              subtitle = "Rows", 
              icon = shiny::icon("users", lib = "font-awesome"), 
              color = "purple", width = 2.5, href = NULL)
          ),
          column(
            width = 7,
            box(
              width = NULL,
              verbatimTextOutput(outputId = "hplot_column_pct", placeholder = TRUE)
            )
          )
        )
      ),
      column(
        # width should be between 1 and 12
        width=12, offset = 0,
        box(
          width = NULL, 
          status = "primary",
          solidHeader = TRUE,
          dataTableOutput("htable11"),
          title = "Hotel Reviews Dataset"
        )
      )
    )
  ),
  ################################################################################
  ## Feature Engineering
  ## EXploring data
  ################################################################################
  tabPanel(
    title = "Exploring Dataset Features",
    icon  = icon("chart-bar", lib = "font-awesome" ),
    ############################################################################
    fluidRow(
      column(
        width = 12,
        tabBox(
          selected = "tab11",
          width = 12,
          tabPanel(
            title = "Feature Plot Selection",
            value = "tab11",
            box(
              status = "primary",
              width = NULL, 
              title = "Exploring Dataset Features & Extracted Features",
              selectInput(inputId = "hplotname_fe", label = "Select Feature Name:",
                          choices = c("length of each message \"Extracted Feature\"" = "1", 
                                      "year \"Extracted Feature\"" = "2",
                                      "month \"Extracted Feature\"" = "3",
                                      "Number Of Reviews Per Location" = "4",
                                      "Number Of Reviews Per Restaurant" = "5")),
              plotOutput(outputId = "hplot_fe_pct")
            )
          ),
          tabPanel(
            title = "Feature Table summary", 
            value = "tab12",
            dataTableOutput("htable_fe")
          )
        )
      )
    )
  ),
  ##########################################################################
  ## Word Cloud
  ## EXploring data  
  ##########################################################################
  tabPanel(
    title = "Exploring Dataset Words", 
    value = "WordCloud", 
    icon = icon("file-word", lib = "font-awesome"),
    ############################################################################
    fluidRow(
      column(
        width = 12,
        box(
          column(
            width = 6,
            radioButtons("hplotType_wc", "Select Plot Type", c("Bar Plot"="b", "Word Cloud"="wc", "n-grams" = "ng"))
          ),
          column(
            width = 6,
            textInput(inputId= "htop_wc", label = "Top Words", 
                      value = 10, width = NULL, placeholder = NULL),
            textInput(inputId= "htop_bp", label = "Minimum Frequency", 
                      value = 5000, width = NULL, placeholder = NULL)
          ),
          column(
            # width should be between 1 and 12
            width=12, offset = 0,
            box(
              width = NULL, 
              status = "primary",
              solidHeader = TRUE,
              plotOutput(outputId = "hplot_wc")
            )
          ),
          title = "Plotting Hotel Review Dataset Term Counts",
          width = NULL
        )
      )
    )
  )
)

################################################################################
## navigation bar
## nrc
################################################################################
body_nrc <- navbarPage(
  title    = "NRC Sentiments",
  id       = "navbarPage_id40", 
  selected = NULL,
  position = "static-top",
  inverse  = TRUE,
  fluid    = TRUE,
  ##########################################################################
  ## 
  ## 
  ##########################################################################
  tabPanel(
    title = "Explore NRC sentiment", 
    value = "Explore_NRC_sentiment1", 
    icon = icon("comment-dots", lib = "font-awesome"),
    ############################################################################
    fluidRow(
      column(
        width = 12,
        box(
          column(
            width = 5,
            selectInput(inputId = "hselect_name_senti", label = "Select variable Name:",
                        choices = names(reviews.clean[, c(2,3,4,6,7)])),
            
            selectizeInput(inputId = "hselected_name_senti",
                           label = "Selected variable Name:", 
                           choices = c(1) , 
                           selected = NULL, 
                           multiple = TRUE,
                           options = list(maxItems = 10)),
            selectInput(inputId = "hselected_senti1", label = "Selected Sentiment Name 1:",
                        choices = unique(sentiments.nrc$sentiment) ),
            selectInput(inputId = "hselected_senti2", label = "Selected Sentiment Name 2:",
                        choices = unique(sentiments.nrc$sentiment) ) 
            
            
          ),
          column(
            # width should be between 1 and 12
            width=7, offset = 0,
            box(
              width = NULL, 
              status = "primary",
              solidHeader = TRUE,
              plotOutput(outputId = "hsentiment_scorenrc")
            )
          ),
          title = "Plotting Hotel Review Dataset sentiment score",
          width = NULL
        )
      )
    )
  ),
  tabPanel(
    title = "Explore NRC sentiment Words", 
    value = "Explore_NRC_sentiment_Words1", 
    icon = icon("comment-dots", lib = "font-awesome"),
    ############################################################################
    fluidRow(
      column(
        width = 12,
        box(
          column(
            width = 5,
            selectInput(inputId = "hselected_sentitment_wc", label = "Selected Sentiment Name :",
                        choices = unique(sentiments.nrc$sentiment) ),
            textInput(inputId= "hmin.freq.text", label = "Top Words", 
                      value = 10, width = NULL, placeholder = NULL)
            
          ),
          column(
            # width should be between 1 and 12
            width=7, offset = 0,
            box(
              width = NULL, 
              status = "primary",
              solidHeader = TRUE,
              plotOutput(outputId = "hsentiment_scorenrc_word")
            )
          ),
          title = "Word Cloud Hotel Review Dataset sentiment",
          width = NULL
        )
      )
    )
  ),
  tabPanel(
    title = "Explore NRC sentiment Distriburtion", 
    value = "Explore_NRC_sentiment_Words1", 
    icon = icon("comment-dots", lib = "font-awesome"),
    ############################################################################
    fluidRow(
      tabBox(
        selected = "tab11",
        width = 12,
        tabPanel(
          title = "nrc sentiment distribution",
          value = "tab11",
          plotOutput(outputId = "hsentiment_nrc_ratio")
        ),
        tabPanel(
          title = "Summary Table", 
          value = "tab12",
          dataTableOutput(outputId = "hsentiment_nrc_ratio_pie")
        )
      )
    )
  ),
  tabPanel(
    title = "sentiment distriburtion over time", 
    value = "NRC_sentiment_distriburtion_time1", 
    icon = icon("comment-dots", lib = "font-awesome"),
    ############################################################################
    fluidRow(
      column(
        width = 12,
        box(
          column(
            width = 5,
            selectInput(inputId = "hselect_name_sentit", label = "Select Restaurant Name:",
                        choices = as.factor(unique(reviews.clean$Restaurant))),
            
            selectInput(inputId = "hselected_name_sentit",
                           label = "Selected variable Name:", 
                           choices = c("over time" ="o", "cummulative sum"="s"), 
                           selected = NULL),
            selectInput(inputId = "hselected_sentit1", label = "Selected Start Date :",
                        choices = unique(sentiments.nrc$Review.Date),
                        selected = tail(unique(sentiments.nrc$Review.Date), 1)),
            selectInput(inputId = "hselected_sentit2", label = "Selected Stop Date :",
                        choices = unique(sentiments.nrc$Review.Date)) 
            
            
          ),
          column(
            # width should be between 1 and 12
            width=7, offset = 0,
            box(
              width = NULL, 
              status = "primary",
              solidHeader = TRUE,
              plotOutput(outputId = "hsentiment_scorenrct")
            )
          ),
          title = "Plotting Hotel Review Dataset sentiment score over time",
          width = NULL
        )
      )
    )
  )
)

################################################################################
## navigation bar
## afinn
################################################################################
body_afinn <- navbarPage(
  title    = "AFINN Sentiments",
  id       = "navbarPage_id410", 
  selected = NULL,
  position = "static-top",
  inverse  = TRUE,
  fluid    = TRUE,
  tabPanel(
    title = "Explore AFINN sentiment Distriburtion", 
    value = "Explore_afinn_sentiment_Words1", 
    icon = icon("comment-dots", lib = "font-awesome"),
    ############################################################################
    fluidRow(
      tabBox(
        selected = "tab11",
        width = 12,
        tabPanel(
          title = "afinn sentiment distribution",
          value = "tab11",
          plotOutput(outputId = "hasentiment_nrc_ratio")
        ),
        tabPanel(
          title = "Summary Table", 
          value = "tab12",
          dataTableOutput(outputId = "hasentiment_nrc_ratio_pie")
        )
      )
    )
  ),
  tabPanel(
    title = "sentiment distriburtion over time", 
    value = "NRC_sentiment_distriburtion_time1", 
    icon = icon("comment-dots", lib = "font-awesome"),
    ############################################################################
    fluidRow(
      column(
        width = 12,
        box(
          column(
            width = 5,
            selectInput(inputId = "haselect_name_sentit", label = "Select Restaurant Name:",
                        choices = as.factor(unique(reviews.clean$Restaurant))),
            
            selectInput(inputId = "haselected_name_sentit",
                        label = "Selected variable Name:", 
                        choices = c("over time" ="o", "cummulative sum"="s"), 
                        selected = NULL),
            selectInput(inputId = "haselected_sentit1", label = "Selected Start Date :",
                        choices = (unique(sentiments.nrc$Review.Date)),
                        selected = tail(unique(sentiments.nrc$Review.Date), 1)),
            selectInput(inputId = "haselected_sentit2", label = "Selected Stop Date :",
                        choices = unique(sentiments.nrc$Review.Date)) 
            
            
          ),
          column(
            # width should be between 1 and 12
            width=7, offset = 0,
            box(
              width = NULL, 
              status = "primary",
              solidHeader = TRUE,
              plotOutput(outputId = "hasentiment_scorenrct")
            )
          ),
          title = "Plotting Hotel Review Dataset sentiment score over time",
          width = NULL
        )
      )
    )
  )
)


################################################################################
## Header
## A dashboard header
################################################################################
header <- dashboardHeader(title = "Data Mining Dashboard")

################################################################################
## Side Bar
## A dashboard SideBar
################################################################################
sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem(text = "SMS", icon = icon("sms", lib = "font-awesome"), startExpanded = TRUE,
             menuSubItem(text = "Row Data", tabName = "row_data", icon = icon("database", lib = "font-awesome")),
             menuSubItem(text = "Explore Data", tabName = "data_explore", icon = icon("glasses", lib = "font-awesome")),
             menuSubItem(text = "Classification Model", tabName = "clsmodel", icon = icon("microchip", lib = "font-awesome")),
             menuSubItem(text = "Association Rules Mining", tabName = "assocationmodel", icon = icon("ruler", lib = "font-awesome")),
             menuSubItem(text = "Apply K-Means Clustering", tabName = "kmeanmodel", icon = icon("sitemap", lib = "font-awesome"))
             ),
    menuItem(text = "Hotel Reviews", icon = icon("comment-dots", lib = "font-awesome"), startExpanded = TRUE,
             menuSubItem("Explore Dataset", tabName = "hotelreview", icon = icon("database", lib = "font-awesome")),
             menuSubItem("NRC", tabName = "NRC", icon = icon("comments", lib = "font-awesome")),
             menuSubItem("AFINN", tabName = "AFINN", icon = icon("comments", lib = "font-awesome"))
             )
    )
  )

################################################################################
## body
## A dashboard body
################################################################################

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "row_data",
      tab_Item_row_data
    ),
    tabItem(
      tabName = "data_explore",
      navbarPage.explore.data
    ),
    tabItem(
      tabName = "clsmodel",
      body_clsmodel
    ),
    tabItem(
      tabName = "assocationmodel",
      body_assocationmodel
    ),
    tabItem(
      tabName = "kmeanmodel",
      body_kmeanmodel
      
    ),
    tabItem(
      tabName = "hotelreview",
      body_hotelreview
    ),
    tabItem(
      tabName = "NRC",
      body_nrc
    ),
    tabItem(
      tabName = "AFINN",
      body_afinn
    )
  )
)



################################################################################
## 
##
##
##
################################################################################
server <- function(input, output, session) { 
  ## Load library
  #setwd("D:/data mining project/sms.r.shiny")
  source("script/Libraries.R")
  source("script/func.r.R")
  
  dtm             <- readRDS(file = "data/dtm.rds")
  dtm.scale       <- readRDS(file = "data/dtm.scale.rds")
  dtm.svd.df.t.f  <- readRDS(file = "data/dtm.svd.df.t.f.rds")
  dtm.svd.df.t    <- readRDS(file = "data/dtm.svd.df.t.rds")
  dtm.svd.df.tr.f <- readRDS(file = "data/dtm.svd.df.tr.f.rds")
  dtm.svd.df.tr   <- readRDS(file = "data/dtm.svd.df.tr.rds")
  
  glm.performance1   <- readRDS(file = "data/glm.performance1.rds")
  glm.performance2   <- readRDS(file = "data/glm.performance2.rds")
  glm.performance3   <- readRDS(file = "data/glm.performance3.rds")
  glm.performance4   <- readRDS(file = "data/glm.performance4.rds")
  glm.performance5   <- readRDS(file = "data/glm.performance5.rds")
  glm.performance6   <- readRDS(file = "data/glm.performance6.rds")
  
  model.matrics      <- readRDS(file = "data/model.matrics.rds")
  
  rules.sorted       <- readRDS(file = "data/rules.sorted.rds")
  
  sms.bag                     <- readRDS(file = "data/sms.bag.rds")
  sms.clean                   <- readRDS(file = "data/sms.clean.rds")
  sms.raw                     <- readRDS(file = "data/sms.raw.rds")
  sms                         <- readRDS(file = "data/sms.rds")
  sms.sentiments.nrc          <- readRDS(file = "data/sms.sentiments.nrc.rds")
  sms.sentiments              <- readRDS(file = "data/sms.sentiments.rds")
  sms_summary                 <- readRDS(file = "data/sms_summary.rds")
  
  stat_summary_table          <- readRDS(file = "data/stat_summary_table.rds")
  
  
  tokens.df         <- readRDS(file = "data/tokens.df.rds")
  tokens.tfidf.df   <- readRDS(file = "data/tokens.tfidf.df.rds")
  
  tokens.dtm.sparseWords.df       <- readRDS(file = "data/tokens.dtm.sparseWords.df.rds")
  tokens.dtm.sparseWords.df.tr.f  <- readRDS(file = "data/tokens.dtm.sparseWords.df.tr.f.rds")
  tokens.dtm.sparseWords.df.t     <- readRDS(file = "data/tokens.dtm.sparseWords.df.t.rds")
  tokens.dtm.sparseWords.df.tr    <- readRDS(file = "data/tokens.dtm.sparseWords.df.tr.rds")
  tokens.dtm.sparseWords.df.t.f   <- readRDS(file = "data/tokens.dtm.sparseWords.df.t.f.rds")
  
  
  svm.performance1   <- readRDS(file = "data/svm.performance1.rds")
  svm.performance2   <- readRDS(file = "data/svm.performance2.rds")
  svm.performance3   <- readRDS(file = "data/svm.performance3.rds")
  svm.performance4   <- readRDS(file = "data/svm.performance4.rds")
  svm.performance5   <- readRDS(file = "data/svm.performance5.rds")
  svm.performance6   <- readRDS(file = "data/svm.performance6.rds")
  
  tree.performance1   <- readRDS(file = "data/tree.performance1.rds")
  tree.performance2   <- readRDS(file = "data/tree.performance2.rds")
  tree.performance3   <- readRDS(file = "data/tree.performance3.rds")
  tree.performance4   <- readRDS(file = "data/tree.performance4.rds")
  tree.performance5   <- readRDS(file = "data/tree.performance5.rds")
  tree.performance6   <- readRDS(file = "data/tree.performance6.rds")
  
  tree.model1        <- readRDS(file = "data/tree.model1.rds")
  tree.model2        <- readRDS(file = "data/tree.model2.rds")
  tree.model3        <- readRDS(file = "data/tree.model3.rds")
  tree.model4        <- readRDS(file = "data/tree.model4.rds")
  tree.model5        <- readRDS(file = "data/tree.model5.rds")
  tree.model6        <- readRDS(file = "data/tree.model6.rds")
  
  
  sms.clean$TextLength_c <- nchar(sms.clean$Text)
  ## Check the number of spam and ham messages
  sms.raw$Label <- as.factor(sms.raw$Label)
  
  ## this just keeps our answers consistent
  set.seed(42) 
  dtm.scale_subsample <- dtm.scale[sample(nrow(dtm.scale), 1000, replace = FALSE), ]
  
  
  afinn.doc                   <- readRDS(file = "data/hotel/afinn.doc.rds")
  bigram.counts.sentiment     <- readRDS(file = "data/hotel/bigram.counts.sentiment.rds")
  hotel.afinn                 <- readRDS(file = "data/hotel/hotel.afinn.rds")
  hotel.bag                   <- readRDS(file = "data/hotel/hotel.bag.rds")
  hotel.nrc                   <- readRDS(file = "data/hotel/hotel.nrc.rds")
  hs.afinn.date.cumsum        <- readRDS(file = "data/hotel/hs.afinn.date.cumsum.rds")
  hs.afinn.date               <- readRDS(file = "data/hotel/hs.afinn.date.rds")
  hs.nrc.date.cumsum          <- readRDS(file = "data/hotel/hs.nrc.date.cumsum.rds")
  nrc.doc                     <- readRDS(file = "data/hotel/nrc.doc.rds")
  nrc.pivot                   <- readRDS(file = "data/hotel/nrc.pivot.rds")
  nrc.pivot.word              <- readRDS(file = "data/hotel/nrc.pivot.word.rds")
  
  hs.afinn.date               <- readRDS(file = "data/hotel/hs.afinn.date.rds")

  reviews.clean               <- readRDS(file = "data/hotel/reviews.clean.rds")
  reviews.raw                 <- readRDS(file = "data/hotel/reviews.raw.rds")
  sentiments.nrc              <- readRDS(file = "data/hotel/sentiments.nrc.rds")
  
  ##############################################################################
  output$table11 <- renderDataTable({
    sms.raw
  })
  
  ##############################################################################
  output$plot_column_pct <- renderPlot({
    ggplot(sms.raw,aes(x=Label, fill=Label))+
      geom_bar(stat="count")+
      scale_fill_manual(values=c("#003767", "#ff7f50"))+
      labs(title = "Distribution of SMS")
  })
  ##############################################################################
  box_plot1 <- reactive({
    as.character(input$plotType_fe)
  })
  ##############################################################################
  plotname_fe_s <- reactive({
    as.character(input$plotname_fe)
  })
  ##############################################################################
  output$plot_fe_pct <- renderPlot({
    if((box_plot1() == "p") && (plotname_fe_s() == "1")){
      boxplot_features(sms, x = sms$Label, y=sms$TextLength, 
                       title = "Text Length")
    }
    else if(box_plot1() == "p" && plotname_fe_s() == "2"){
      boxplot_features(sms, x = sms$Label, y=sms$pct_caps, 
                       title = "percentage of capital letters")
    }
    else if(box_plot1() == "p" && plotname_fe_s() == "3"){
      boxplot_features(sms, x = sms$Label, y=sms$pct_digits, 
                       title = "percentage of digit numbers")
    }
    else if(box_plot1() == "p" && plotname_fe_s() == "4"){
      boxplot_features(sms, x = sms$Label, y=sms$num_exclamations, 
                       title = "count of '!' from text")
    }
    else if(box_plot1() == "p" && plotname_fe_s() == "5"){
      boxplot_features(sms, x = sms$Label, y=sms$num_caps, 
                       title = "count of capital letters")  
    }
    else if(box_plot1() == "p" && plotname_fe_s() == "6"){
      boxplot_features(sms, x = sms$Label, y=sms$num_digits, 
                       title = "count of digit numbers")
    }
    else if(box_plot1() == "p" && plotname_fe_s() == "7"){
      boxplot_features(sms, x = sms$Label, y=sms$consecutive_digits, 
                       title = "count of digit numbers in series as whole")
    }
    else if(box_plot1() == "p" && plotname_fe_s() == "8"){
      boxplot_features(sms, x = sms$Label, y=sms$url_exist, title = "url in text")
    }
    else if(box_plot1() == "h" && plotname_fe_s() == "1"){
      hist_features(sms, x=sms$TextLength, fill=sms$Label, title= "SMS length")
    }
    else if(box_plot1() == "h" && plotname_fe_s() == "2"){
      hist_features(sms, x=sms$pct_caps, fill=sms$Label, 
                    title= "percentage of capital letters" , binwidth = 0.005)
    }
    else if(box_plot1() == "h" && plotname_fe_s() == "3"){
      hist_features(sms, x=sms$pct_digits, fill=sms$Label, 
                    title= "percentage of digit numbers", binwidth = 0.01)
    }
    else if(box_plot1() == "h" && plotname_fe_s() == "4"){
      hist_features(sms, x=sms$num_exclamations, fill=sms$Label, 
                    title= "count of '!' from text")
    }
    else if(box_plot1() == "h" && plotname_fe_s() == "5"){
      hist_features(sms, x=sms$num_caps, fill=sms$Label, 
                    title= "count of capital letters") 
    }
    else if(box_plot1() == "h" && plotname_fe_s() == "6"){
      hist_features(sms, x=sms$num_digits, fill=sms$Label, 
                    title= "count of digit numbers")
    }
    else if(box_plot1() == "h" && plotname_fe_s() == "7"){
      hist_features(sms, x=sms$consecutive_digits, fill=sms$Label, 
                    title= "count of digit numbers in series as whole")
    }
    else if(box_plot1() == "h" && plotname_fe_s() == "8"){
      hist_features(sms, x=sms$url_exist, fill=sms$Label, title= "url in text")
    }
    else if(plotname_fe_s() == "9"){
      corrplot(
        cor( sms %>% select(-Label,-Text)), 
        type = "lower", 
        method="shade",
        tl.col="black", 
        tl.srt=45, 
        addCoef.col="black"
      )
    }
    
  })
  
  ##############################################################################
  output$table_fe <- renderDataTable({
    sms[,-c(2)]
  })
  ##############################################################################
  output$table_fe_summary <- renderTable({
    sms_summary
  })
  ##############################################################################
  select_col <- reactive({
    c(as.integer(input$start_c) : as.integer(input$end_c))
    
  })
  ##############################################################################
  table_dtm_s<- reactive({
    switch(input$tableType_ta, d = tokens.df[,select_col()], 
           tf = tokens.tfidf.df[,select_col()], 
           c = sms.clean[,select_col()])
  })
  ##############################################################################
  output$table_dtm <- renderDataTable({
    table_dtm_s()
  })
  ##############################################################################
  output$plot_ta_tl <- renderPlot({
    hist_features(sms.clean, x=sms.clean$TextLength, 
                  fill=sms.clean$Label, title= "SMS length")
  })
  ##############################################################################
  output$plot_ta_tlc <- renderPlot({
    hist_features(sms.clean, x=sms.clean$TextLength_c, 
                  fill=sms.clean$Label, title= "Corpus length")
  })
  ##############################################################################
  output$plot_ta_tl_b <- renderPlot({
    boxplot_features(sms.clean, x = sms.clean$Label, 
                     y=sms.clean$TextLength, 
                     title = "SMS length")
  })
  ##############################################################################
  output$plot_ta_tlc_b <- renderPlot({
    boxplot_features(sms.clean, x = sms.clean$Label, 
                     y=sms.clean$TextLength_c, 
                     title = "Corpus length")
  })
  ##############################################################################
  plot_wc_s <- reactive({
    if (input$plotType_wc == "b" && input$plotname_wc == "1") {
      ## "Top common words in both Ham/spam" = "1"
      "1"
      }
    else if(input$plotType_wc == "b" && input$plotname_wc == "2"){
      ## "TOP Spam Words" = "2"
      "2"
    }
    else if(input$plotType_wc == "b" && input$plotname_wc == "3"){
      ## "TOP Spam Words" = "2"
      "3"
    }
    else if (input$plotType_wc == "wc" && input$plotname_wc == "1") {
      ## "Top common words in both Ham/spam" = "1"
      "4"
    }
    else if(input$plotType_wc == "wc" && input$plotname_wc == "2"){
      ## "TOP Spam Words" = "2"
      "5"
    }
    else if(input$plotType_wc == "wc" && input$plotname_wc == "3"){
      ## "TOP Spam Words" = "2"
      "6"
    }
      
  })
  ##############################################################################
  output$plot_wc <- renderPlot({
    if (plot_wc_s() == "1") {
      ##########################################################################
      ## "Top common words in both Ham/spam" = "1"
      ## Top words in both
      sms.bag %>% 
        top_n(as.integer(input$top_wc)) %>% 
        ggplot(aes(reorder(word, n), n, fill = Label)) +
        geom_col()+
        scale_fill_manual(values=c("#003767","#ff7f50"))+
        labs(title = "Top common words in both Ham/spam",
             x = "Words",
             y = "Count") +
        coord_flip()
      }
    ## "TOP Spam Words" = "2"
    else if(plot_wc_s() == "2"){
      sms.bag[sms.bag$Label == "spam", ] %>% 
        anti_join(sms.bag[sms.bag$Label == "ham", ], by ="word") %>%
        top_n(as.integer(input$top_wc)) %>% 
        ggplot(aes(reorder(word, n), n, fill = Label)) +
        geom_col()+
        scale_fill_manual(values=c("#ff7f50"))+
        labs(title = "TOP Spam Words",
             y = "Count",
             x = "Word") +
        coord_flip()
      }
    ## "TOP Ham Words" = "3"
    else if(plot_wc_s() == "3"){
      sms.bag[sms.bag$Label == "ham", ] %>% 
        anti_join(sms.bag[sms.bag$Label == "spam", ], by ="word") %>%
        top_n(as.integer(input$top_wc)) %>% 
        ggplot(aes(reorder(word, n), n, fill = Label)) +
        geom_col()+
        scale_fill_manual(values=c("#003767"))+
        labs(title = "TOP Ham Words",
             y = "Count",
             x = "Word") +
        coord_flip()
    }
    ## "Top common words in both Ham/spam" = "1"
    else  if(plot_wc_s() == "4"){
      sms.bag %>%
        with(wordcloud(words = word, 
                       freq = n,
                       min.freq=as.integer(input$top_bp),
                       random.color = FALSE,
                       colors = brewer.pal(8, 'Dark2')))
      }
    ## "TOP Spam Words" = "2"
    else if(plot_wc_s() == "5"){
      sms.bag[sms.bag$Label == "ham", ] %>% 
        anti_join(sms.bag[sms.bag$Label == "spam", ], by ="word") %>%
        top_n(50) %>%
        with(wordcloud(words = word, freq = n,
                       min.freq=as.integer(input$top_bp),
                       random.color = FALSE,
                       colors = brewer.pal(8, 'Dark2'))
        )
      }
    ## "TOP Ham Words" = "3"
    else if(plot_wc_s() == "6"){
      sms.bag[sms.bag$Label == "spam", ] %>% 
        anti_join(sms.bag[sms.bag$Label == "ham", ], by ="word") %>%
        top_n(15) %>%
        with(wordcloud(words = word, freq = n,
                       min.freq=as.integer(input$top_bp),
                       random.color = FALSE,
                       colors = brewer.pal(8, 'Dark2'))
        )
      }
  })
  ##############################################################################
  output$plot_sa_dp <- renderPlot({
    sms.sentiments %>%
      inner_join(get_sentiments("nrc"), by = "word") %>%
      group_by(Label) %>%
      count(sentiment) %>%
      ggplot(aes(sentiment, n, fill = Label)) +
      geom_col()+
      scale_fill_manual(values=c("#003767","#ff7f50"))+
      labs(title = "Sentiment Distribution",
           y = "Counts") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  ##############################################################################
  output$plot_sa_dphs <- renderPlot({
    boxplot_features(sms.sentiments.nrc, 
                     x = sms.sentiments.nrc$Label, 
                     y=sms.sentiments.nrc$nrc_score, 
                     title = "Sentiment score")
  })
  ##############################################################################
  output$table_sa <- renderDataTable({
    sms.sentiments.nrc[,-c(2,3,4)]
  })
  ##############################################################################
  sortby_r <- reactive({
    sort(rules.sorted, by = input$sortby)[1:200]
  })
  ##############################################################################
  output$table_assocs <- renderDataTable({
    as.data.frame(arules::inspect(sortby_r()))
  })
  ##############################################################################
  output$summary <- renderPrint({ summary(rules.sorted) })
  ##############################################################################
  assocsplot_s <- reactive({
    input$assocsplot
  })
  ##############################################################################
  Nassocs_s <- reactive({
    input$Nassocs
  })
  
  ##############################################################################
  output$plot_assocs <- renderPlot({
    if (assocsplot_s() == "1"){
      ## Visualizing Association Rules
      ## Package arulesViz supports visualization of association rules with 
      ## scatter plot, balloon plot, graph, parallel coordinates plot, etc.
      plot(rules.sorted)
    }
    else if (assocsplot_s() == "2"){
      plot(head(rules.sorted, n = 32, by = "lift"), method = "graph")
    }
    else if (assocsplot_s() == "3"){
      plot(head(rules.sorted[35:50], n = Nassocs_s(), by = "lift"), method = "graph")
    }
    
  })  
  ##############################################################################
  col_select_cls <- reactive({
    as.integer(input$start_ctr):as.integer(input$end_ctr)
  })
  ##############################################################################
  selectdata_c <- reactive({
    input$selectdata
  })
  ##############################################################################
  cls_data <- reactive({
    if (selectdata_c() == "d"){
      list(df1 = tokens.dtm.sparseWords.df.t, df2 = tokens.dtm.sparseWords.df.tr)
    }
    else if (selectdata_c() == "df"){
      list(df1 = tokens.dtm.sparseWords.df.t.f, df2 = tokens.dtm.sparseWords.df.tr.f)
      
    }
    else if (selectdata_c() == "s"){
      list(df1 = dtm.svd.df.t, df2 = dtm.svd.df.tr)
      
    }
    else if (selectdata_c() == "sf"){
      list(df1 = dtm.svd.df.t.f, df2 = dtm.svd.df.tr.f)
    }
  })
  ##############################################################################
  output$test_table <- renderDataTable({
    cls_data()[['df1']][, col_select_cls()]
  })
  ##############################################################################
  output$summary_test_table <- renderPrint({ 
    summary(cls_data()[['df1']][, col_select_cls()]) 
  })
  ##############################################################################
  output$train_table <- renderDataTable({
    cls_data()[['df2']][, col_select_cls()]
  })
  ##############################################################################
  output$summary_train_table <- renderPrint({ 
    summary(cls_data()[['df2']][, col_select_cls()]) 
  })
  ##############################################################################
  cls_target_s <- reactive({
    input$cls_target
  })
  ##############################################################################
  cls_feature_s <- reactive({
    input$cls_feature
  })
  ##############################################################################
  output$summary_target <- renderPrint({ 
    print("Target variable Summary")
    summary(sms[,cls_target_s()])
    
  })
  ##############################################################################
  output$summary_feature <- renderPrint({ 
    print("Feature variable Summary")
    summary(sms[,cls_feature_s()]) 
  })  
  ##############################################################################
  selectModel_c <- reactive({
    input$selectModel
  })
  ##############################################################################
  selectMethod_c <- reactive({
    input$selectMethod
  })
  ##############################################################################
  
  ############################################################################## red maroon chevron-down
  output$TN <- renderInfoBox({
    valueBox(
      value = model.matrics[((model.matrics$Model == selectModel_c())&(model.matrics$Method==selectMethod_c())), "TP"], 
      subtitle = "True Negative",
      icon =  shiny::icon("chevron-up", lib="font-awesome"), 
      color = "green", width = 3, href = NULL)
  })
  ############################################################################## red maroon chevron-down
  output$FN <- renderInfoBox({
    valueBox(
      value = model.matrics[((model.matrics$Model == selectModel_c())&(model.matrics$Method==selectMethod_c())), "FP"],
      subtitle = "False Negative",
      icon = shiny::icon("chevron-down", lib="font-awesome"), 
      color = "maroon", width = 3, href = NULL)
  })
  ############################################################################## red maroon chevron-down
  output$TP <- renderInfoBox({
    valueBox(
      value = model.matrics[((model.matrics$Model == selectModel_c())&(model.matrics$Method==selectMethod_c())), "TN"], 
      subtitle = "True Positive",
      icon = shiny::icon("chevron-up", lib="font-awesome"), 
      color = "green", width = 3, href = NULL)
  })
  ############################################################################## red maroon chevron-down
  output$FP <- renderInfoBox({
    valueBox(
      value = model.matrics[((model.matrics$Model == selectModel_c())&(model.matrics$Method==selectMethod_c())), "FN"], 
      subtitle = "False Positive",
      icon = shiny::icon("chevron-down", lib="font-awesome"), 
      color = "maroon", width = 3, href = NULL)
  })
  ##############################################################################
  output$aucplot <- renderPlot({
    ind <- which((model.matrics$Model == selectModel_c())&(model.matrics$Method==selectMethod_c()), arr.ind=TRUE)
    
    if (selectModel_c() == "Logistic Regression"){
      glm.performance_s <- paste("glm.performance", ind, sep = "")
      plot(eval(as.name(glm.performance_s)), main = "Area Under Curve", col=2, lwd=2)
      abline(a=0,b=1,lwd=2,lty=3,col="black")
    }
    else if(selectModel_c() == "DT"){
      tree.model_s <- paste("tree.performance", (ind-12), sep = "")
      plot(eval(as.name(tree.model_s)), main = "Area Under Curve", col=2, lwd=2)
      abline(a=0,b=1,lwd=2,lty=3,col="black")
    }
    else{
      svm.performance_s <- paste("svm.performance", (ind-6), sep = "")
      plot(eval(as.name(svm.performance_s)), main = "Area Under Curve", col=2, lwd=2)
      abline(a=0,b=1,lwd=2,lty=3,col="black")
      ##plot.new()
      
    }
  })
  ############################################################################## red maroon chevron-down
  output$accuracy <- renderInfoBox({
    valueBox(
      value = model.matrics[((model.matrics$Model == selectModel_c())&(model.matrics$Method==selectMethod_c())), "accuracy"],
      subtitle = "accuracy",
      icon = shiny::icon("bell", lib="font-awesome"), 
      color = "green", width = 2.5, href = NULL)
  })
  ############################################################################## red maroon chevron-down
  output$Precision <- renderInfoBox({
    valueBox(
      value = model.matrics[((model.matrics$Model == selectModel_c())&(model.matrics$Method==selectMethod_c())), "Precision"],
      subtitle = "Precision",
      icon = shiny::icon("bell", lib="font-awesome"), 
      color = "maroon", width = 2.5, href = NULL)
  })
  ############################################################################## red maroon chevron-down
  output$negative <- renderInfoBox({
    valueBox(
      value = model.matrics[((model.matrics$Model == selectModel_c())&(model.matrics$Method==selectMethod_c())), "ERR"],
      subtitle = "ERR",
      icon = shiny::icon("bell", lib="font-awesome"), 
      color = "maroon", width = 2.5, href = NULL)
  })
  ############################################################################## red maroon chevron-down
  output$sensitivity <- renderInfoBox({
    valueBox(
      value = model.matrics[((model.matrics$Model == selectModel_c())&(model.matrics$Method==selectMethod_c())), "sensitivity"],
      subtitle = "sensitivity",
      icon = shiny::icon("bell", lib="font-awesome"), 
      color = "green", width = 2.5, href = NULL)
  })
  ############################################################################## red maroon chevron-down
  output$Specificity <- renderInfoBox({
    valueBox(
      value = model.matrics[((model.matrics$Model == selectModel_c())&(model.matrics$Method==selectMethod_c())), "Specificity"],
      subtitle = "Specificity",
      icon = shiny::icon("bell", lib="font-awesome"), 
      color = "maroon", width = 2.5, href = NULL)
  })
  ############################################################################## red maroon chevron-down
  output$AUC <- renderInfoBox({
    valueBox(
      value = round((model.matrics[((model.matrics$Model == selectModel_c())&(model.matrics$Method==selectMethod_c())), "AUC"]), 4),
      subtitle = "AUC",
      icon = shiny::icon("bell", lib="font-awesome"), 
      color = "maroon", href = NULL)
  })
  ##############################################################################
  output$metric_table1 <- renderDataTable({
    model.matrics
  })
  
  ##############################################################################
  selectplotclust_c <- reactive({
    input$clustType
  })
  ##############################################################################
  kmean_max_c <- reactive({
    as.integer(input$kmean_max)
  })
  ##############################################################################
  output$plot_bestk <- renderPlot({
    if (selectplotclust_c() == "s"){
      fviz_nbclust(dtm.scale_subsample, kmeans, method = "wss", k.max = kmean_max_c(), print.summary = TRUE)
    }
    else if (selectplotclust_c() == "h"){
      fviz_nbclust(dtm.scale_subsample, kmeans, method = "silhouette", k.max = kmean_max_c())
    }
    
  })
  ##############################################################################
  kmean_k_c <- reactive({
    ## make this example reproducible
    set.seed(42)
    ## perform k-means clustering 
    kmeans(dtm.scale, centers = as.integer(input$kmean_k), nstart = 25)
  })
  ##############################################################################
  clustplottype_c <- reactive({
    input$clustplottype
  })
  ##############################################################################
  final_data_c <- reactive({
    cbind(sms.clean, cluster = (kmean_k_c()$cluster))
  })
  ##############################################################################
  output$plot_k <- renderPlot({
    if (clustplottype_c() == "c"){
      ## plot results of final k-means model
      ## Visualize kmeans clustering
      ## use repel = TRUE to avoid overplotting
      ## Show points only
      fviz_cluster(kmean_k_c(), data = dtm.scale, 
                   repel = TRUE, 
                   ellipse.type = "norm",
                   geom = "point")
    }
    else if (clustplottype_c() == "b"){
      ## count of each cluster
      data.frame(table(kmean_k_c()$cluster)) %>% 
        ggplot(aes(reorder(Var1, Freq), Freq)) +
        geom_col()+
        labs(title = "Number Of Observation Per Clusters",
             x = "Cluster Number",
             y = "Number Of Observation") +
        coord_flip()
    }
    
  })
  
  ##############################################################################
  plot_wc_sc <- reactive({
    input$plotType_wcc
  })
  
  ##############################################################################
  Clustname_wcc_c <- reactive({
    as.integer(input$Clustname_wcc)
  })
  ##############################################################################
  top_wcc_c <- reactive({
    as.integer(input$top_wcc)
  })
  ##############################################################################
  top_bpc_c <- reactive({
    as.integer(input$top_bpc)
  })
  ##############################################################################
  output$summary_clust_table <- renderPrint({ 
    ## count of each cluster
    data.frame(table(kmean_k_c()$cluster)) 
    
  })
  ##############################################################################
  output$clust_table <- renderDataTable({
    final_data_c()
  })
  ##############################################################################
  sms.bag.cluster_c <- reactive({
    final_data_c() %>% 
      filter(cluster == Clustname_wcc_c()) %>%
      unnest_tokens(word, Text, token = 'words') %>% 
      anti_join(stop_words) %>% 
      group_by(Label, cluster) %>% 
      count(word, sort = TRUE)
  })
  ##############################################################################
  output$plot_wcc <- renderPlot({
    ## "Horizontal Bar Plot"="b"
    if (plot_wc_sc() == "b") {
      ##########################################################################
      ## Top words in both
      head(arrange(sms.bag.cluster_c(), desc(n)), n = top_wcc_c())%>%
        ggplot(aes(reorder(word, n), n, fill = Label)) +
        geom_col()+
        scale_fill_manual(values=c("#003767","#ff7f50"))+
        labs(title = "Top common words in both Ham/spam per cluster",
             x = "Words",
             y = "Count") +
        coord_flip()
    }
    ## "Word Cloud"="wc"
    else if(plot_wc_sc() == "wc"){
      sms.bag.cluster_c() %>%
        with(wordcloud(words = word, 
                       freq = n,
                       min.freq = top_bpc_c(),
                       random.color = FALSE,
                       colors = brewer.pal(8, 'Dark2')))
    }
    
    
  })
  ##############################################################################
  output$htable11 <- renderDataTable({
    reviews.clean[,c(1:5)]
  })
  ##############################################################################
  output$hplot_column_pct <- renderPrint({ 
    list(
      summary(as.factor(reviews.clean$year)), 
      summary(as.factor(reviews.clean$Location),
              ## check for NaN
              sum(is.na(reviews.clean)) )
    )
  })
  ##############################################################################
  hplotname_fe_c <- reactive({
    if (input$hplotname_fe == "1"){
      as.data.frame(sort(table(reviews.clean$TextLength), decreasing = TRUE), stringsAsFactors = TRUE)
    }
    else if (input$hplotname_fe == "2"){
      as.data.frame(sort(table(reviews.clean$year), decreasing = TRUE), stringsAsFactors = TRUE)
      
    }
    else if (input$hplotname_fe == "3"){
      as.data.frame(sort(table(reviews.clean$month), decreasing = TRUE), stringsAsFactors = TRUE) 
      
    }
    else if (input$hplotname_fe == "4"){
      as.data.frame(sort(table(reviews.clean$Location), decreasing = TRUE), stringsAsFactors = TRUE) 
      
    }
    else if (input$hplotname_fe == "5"){
      as.data.frame(sort(table(reviews.clean$Restaurant), decreasing = TRUE), stringsAsFactors = TRUE) 
      
    }
  })
  ##############################################################################
  output$hplot_fe_pct <- renderPlot({
    hplotname_fe_c() %>%
      head(10)%>%
      ggplot(aes(Var1, Freq)) +
      geom_bar(stat="identity", width=0.75, fill="steelblue") +
      geom_text(aes(label=Freq), vjust=1.6, color="white", size=2)+
      theme_minimal()+
      labs(title = "",
           y = "Frequent",
           x = "") +
      coord_flip()
  })
  
  ##############################################################################
  output$htable_fe <- renderDataTable({
    hplotname_fe_c()
  })
  
  ##############################################################################
  hplotType_wc_c <- reactive({
    input$hplotType_wc
  })
  ##############################################################################
  htop_wc_c <- reactive({
    as.integer(input$htop_wc)
  })
  ##############################################################################
  htop_bp_c <- reactive({
    as.integer(input$htop_bp)
  })
  ##############################################################################
  output$hplot_wc <- renderPlot({
    if ( hplotType_wc_c() == "b") {
      ## "TOP 15 Words" 
      hotel.bag %>%   
        top_n(htop_wc_c()) %>% 
        ggplot(aes(reorder(word, n), n)) +
        geom_col(fill="steelblue") +
        labs(title = "TOP 15 Words",
             y = "word Count",
             x = "Words" ) +
        coord_flip()
      
    }
    else if(hplotType_wc_c() == "wc" ){
      hotel.bag %>%
        with(wordcloud(words = word, freq = n,
                       min.freq=htop_bp_c(),
                       random.color = FALSE,
                       colors = brewer.pal(8, 'Dark2')))
      
    }
    else if(hplotType_wc_c() == "ng"){
      bigram.graph <- bigram.counts.sentiment %>%
        filter(n >= 400) %>%
        graph_from_data_frame()
      ggraph(bigram.graph, layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), 
                       show.legend = FALSE,
                       arrow = grid::arrow(type = "closed", length = unit(2, "mm")), 
                       end_cap = circle(1, "mm")) +
        geom_node_point(color = "lightblue", size = 4) +
        geom_node_text(aes(label = name), size = 4) +
        theme_void()
      
    }
  })
  ##############################################################################
  r <- reactiveValues(value = "Review.Date")
  
  ##############################################################################
  value_x <- reactive({
    input$hselected_name_senti
  })
  ##############################################################################
  observe({
    hselect_name_senti_c <- input$hselect_name_senti
    input$hselected_senti2
    input$hselected_senti1
    
    if ( hselect_name_senti_c == "Location"){
      hselect_name_senti_cc <- reviews.clean$Location
      
      output$hsentiment_scorenrc <- renderPlot({
        sentiments.nrc %>%
          filter((sentiment %in% c(input$hselected_senti1, input$hselected_senti2))) %>%
          filter(Location %in% value_x()) %>%
          group_by(Location) %>%
          count(sentiment)%>%
          ggplot(aes(reorder(Location, n), n, fill=sentiment)) +
          geom_col()+
          labs(title = "Location Sentiment Score",
               x = "Location",
               y = "Count") +
          coord_flip()
      })
    }
    else if (hselect_name_senti_c == "Review.Date"){
      hselect_name_senti_cc <- reviews.clean$Review.Date
      output$hsentiment_scorenrc <- renderPlot({
        sentiments.nrc %>%
          filter((sentiment %in% c(input$hselected_senti1, input$hselected_senti2))) %>%
          filter(as.character(Review.Date) %in% (value_x())) %>%
          group_by(Review.Date) %>%
          count(sentiment) %>%
          ggplot(aes(reorder(Review.Date, n), n, fill=sentiment)) +
          geom_col()+
          labs(title = "Review.Date Sentiment Score",
               x = "Review.Date",
               y = "Count") +
          coord_flip()
      })
    }
    else if (hselect_name_senti_c == "year"){
      hselect_name_senti_cc <- reviews.clean$year
      output$hsentiment_scorenrc <- renderPlot({
        sentiments.nrc %>%
          filter((sentiment %in% c(input$hselected_senti1, input$hselected_senti2))) %>%
          filter(year %in% value_x()) %>%
          group_by(year) %>%
          count(sentiment) %>%
          ggplot(aes(reorder(year, n), n, fill=sentiment)) +
          geom_col()+
          labs(title = "year Sentiment Score",
               x = "year",
               y = "Count") +
          coord_flip()
      })
    }
    else if (hselect_name_senti_c == "month"){
      hselect_name_senti_cc <-  reviews.clean$month
      output$sentiment_scorenrc <- renderPlot({
        sentiments.nrc %>%
          filter((sentiment %in% c(input$hselected_senti1, input$hselected_senti2))) %>%
          filter(month %in% value_x()) %>%
          group_by(month) %>%
          count(sentiment)%>%
          ggplot(aes(reorder(month, n), n, fill=sentiment)) +
          geom_col()+
          labs(title = "month Sentiment Score",
               x = "month",
               y = "Count") +
          coord_flip()
      })
    }
    else if (hselect_name_senti_c == "Restaurant"){
      hselect_name_senti_cc <- reviews.clean$Restaurant
      output$hsentiment_scorenrc <- renderPlot({
        sentiments.nrc %>%
          filter((sentiment %in% c(input$hselected_senti1, input$hselected_senti2))) %>%
          filter(Restaurant %in% value_x()) %>%
          group_by(Restaurant) %>%
          count(sentiment)%>%
          ggplot(aes(reorder(Restaurant, n), n, fill=sentiment)) +
          geom_col()+
          labs(title = "Restaurant Sentiment Score",
               x = "Restaurant",
               y = "Count") +
          coord_flip()
      })
    }
    
    updateSelectizeInput(session, 'hselected_name_senti',
                         label = paste("Select ", hselect_name_senti_c),
                         choices = as.factor(unique(hselect_name_senti_cc)), 
                         selected = as.factor(unique(hselect_name_senti_cc)),
                         server = TRUE)
  })
  ##############################################################################
  observe({
    hselected_sentitment_wc_c <- input$hselected_sentitment_wc
    hmin.freq.observe <- input$hmin.freq.text
    if ( hselected_sentitment_wc_c == "joy"){
      output$hsentiment_scorenrc_word <- renderPlot({
        nrc.pivot.word %>% 
          with(wordcloud(word, joy, 
                         min.freq=hmin.freq.observe,
                         random.color = FALSE,
                         colors = brewer.pal(12, "Dark2")))})
    }
    else if (hselected_sentitment_wc_c == "anger"){
      output$hsentiment_scorenrc_word <- renderPlot({
        nrc.pivot.word %>% 
          with(wordcloud(word, anger, 
                         min.freq=hmin.freq.observe,
                         random.color = FALSE,
                         colors = brewer.pal(12, "Dark2")))})
    }
    else if (hselected_sentitment_wc_c == "positive"){
      output$hsentiment_scorenrc_word <- renderPlot({
        nrc.pivot.word %>% 
          with(wordcloud(word, positive, 
                         min.freq=hmin.freq.observe,
                         random.color = FALSE,
                         colors = brewer.pal(12, "Dark2")))})
    }
    else if (hselected_sentitment_wc_c == "negative"){
      output$hsentiment_scorenrc_word <- renderPlot({
        nrc.pivot.word %>% 
          with(wordcloud(word, negative, 
                         min.freq=hmin.freq.observe,
                         random.color = FALSE,
                         colors = brewer.pal(12, "Dark2")))})
    }
    else if (hselected_sentitment_wc_c == "trust"){
      output$hsentiment_scorenrc_word <- renderPlot({
        nrc.pivot.word %>% 
          with(wordcloud(word, trust, 
                         min.freq=hmin.freq.observe,
                         random.color = FALSE,
                         colors = brewer.pal(12, "Dark2")))})
    }
    else if (hselected_sentitment_wc_c == "sadness"){
      output$hsentiment_scorenrc_word <- renderPlot({
        nrc.pivot.word %>% 
          with(wordcloud(word, sadness, 
                         min.freq=hmin.freq.observe,
                         random.color = FALSE,
                         colors = brewer.pal(12, "Dark2")))})
    }
    else if (hselected_sentitment_wc_c == "anticipation"){
      output$hsentiment_scorenrc_word <- renderPlot({
        nrc.pivot.word %>% 
          with(wordcloud(word, anticipation, 
                         min.freq=hmin.freq.observe,
                         random.color = FALSE,
                         colors = brewer.pal(12, "Dark2")))})
    }
    else if (hselected_sentitment_wc_c == "surprise"){
      output$hsentiment_scorenrc_word <- renderPlot({
        nrc.pivot.word %>% 
          with(wordcloud(word, surprise, 
                         min.freq=hmin.freq.observe,
                         random.color = FALSE,
                         colors = brewer.pal(12, "Dark2")))})
    }
    else if (hselected_sentitment_wc_c == "fear"){
      output$hsentiment_scorenrc_word <- renderPlot({
        nrc.pivot.word %>% 
          with(wordcloud(word, fear, 
                         min.freq=hmin.freq.observe,
                         random.color = FALSE,
                         colors = brewer.pal(12, "Dark2")))})
    }
    else if (hselected_sentitment_wc_c == "disgust"){
      output$hsentiment_scorenrc_word <- renderPlot({
        nrc.pivot.word %>% 
          with(wordcloud(word, disgust, 
                         min.freq=hmin.freq.observe,
                         random.color = FALSE,
                         colors = brewer.pal(12, "Dark2")))})
    }
  })
  ##############################################################################
  output$hsentiment_nrc_ratio <- renderPlot({
    nrc.doc %>%
      ggplot(aes(reorder(sentiment, n), n)) +
      geom_col(fill="steelblue")+
      labs(title = "Overall Sentiment Distribution For dataset",
           x = "sentiment",
           y = "Count") +
      coord_flip()
    
  })
  ##############################################################################
  output$hsentiment_nrc_ratio_pie <- renderDataTable({
    nrc.doc %>% arrange(desc(n))
    
  })
  ##############################################################################
  value_xt <- reactive({
    input$hselected_name_sentit
    
  })
  ##############################################################################
  observe({
    hselect_name_senti_ct <- input$hselect_name_sentit
    input$hselected_sentit1
    input$hselected_sentit2
    
    if (value_xt() == "o"){
       output$hsentiment_scorenrct <- renderPlot({
         hs.nrc.date.cumsum %>%
           filter(Restaurant == hselect_name_senti_ct) %>%
        ggplot(aes(x=Review.Date, y=nrc_score)) +
           geom_line() +
        xlab("") +
        labs(title = paste("Score over time"),
             x = paste("Time" , input$hselected_sentit1, ":", input$hselected_sentit2),
             y = "Score") +
        theme_ipsum() +
        theme(axis.text.x=element_text(angle=60, hjust=1)) +
        scale_x_date(limit=c(as.Date(input$hselected_sentit1),as.Date(input$hselected_sentit2)))
      })
    }
    else if (value_xt() == "s"){
      output$hsentiment_scorenrct <- renderPlot({
        hs.nrc.date.cumsum %>%
          filter(Restaurant == hselect_name_senti_ct) %>%
          ggplot(aes(x=Review.Date, y=nrc_score_cumsum)) +
          geom_line() +
          xlab("") +
          labs(title = paste("Cummulative Sum Score over time"),
               x = paste("Time" , input$hselected_sentit1, ":", input$hselected_sentit2),
               y = "Score") +
          theme_ipsum() +
          theme(axis.text.x=element_text(angle=60, hjust=1)) +
          scale_x_date(limit=c(as.Date(input$hselected_sentit1),as.Date(input$hselected_sentit2)))
      })
    }
    })
 
  
  
  
  
  
  
  ##############################################################################
  output$hasentiment_nrc_ratio <- renderPlot({
    afinn.doc %>%
      ggplot(aes(reorder(afinn_score, n), n)) +
      geom_col(fill="steelblue")+
      labs(title = "Overall Sentiment Distribution For dataset",
           x = "sentiment",
           y = "Count") +
      coord_flip()
    
  })
  ##############################################################################
  output$hasentiment_nrc_ratio_pie <- renderDataTable({
    afinn.doc %>% arrange(desc(n))
    
  })
  
  
  
  
  ##############################################################################
  value_xta <- reactive({
    input$haselected_name_sentit
    
  })
  ##############################################################################
  observe({
    haselect_name_senti_ct <- input$haselect_name_sentit
    input$haselected_sentit1
    input$haselected_sentit2
    
    if (value_xta() == "o"){
      output$hasentiment_scorenrct <- renderPlot({
        hs.afinn.date %>%
          filter(Restaurant == haselect_name_senti_ct) %>%
          ggplot(aes(x=Review.Date, y=value)) +
          geom_line() +
          xlab("") +
          labs(title = paste("Score over time"),
               x = paste("Time" , input$haselected_sentit1, ":", input$haselected_sentit2),
               y = "Score") +
          theme_ipsum() +
          theme(axis.text.x=element_text(angle=60, hjust=1)) +
          scale_x_date(limit=c(as.Date(input$haselected_sentit1),as.Date(input$haselected_sentit2)))
      })
    }
    else if (value_xt() == "s"){
      output$hasentiment_scorenrct <- renderPlot({
        hs.afinn.date.cumsum %>%
          filter(Restaurant == haselect_name_senti_ct) %>%
          ggplot(aes(x=Review.Date, y=value)) +
          geom_line() +
          xlab("") +
          labs(title = paste("Cummulative Sum Score over time"),
               x = paste("Time" , input$haselected_sentit1, ":", input$haselected_sentit2),
               y = "Score") +
          theme_ipsum() +
          theme(axis.text.x=element_text(angle=60, hjust=1)) +
          scale_x_date(limit=c(as.Date(input$haselected_sentit1),as.Date(input$haselected_sentit2)))
      })
    }
  })
}




################################################################################
##  dashboard Page
################################################################################
shinyApp(
  ## This creates a dashboard page for use in a Shiny app.
  ui = dashboardPage(
    ## Create a header for a dashboard page dashboardHeader(),
    header = header,
    ## Create a dashboard sidebar
    ## sidebar = dashboardSidebar(),
    sidebar = sidebar,
    ## The main body of a dashboard page.
    body = body,
    ## A title to display in the browser's title bar. 
    ## If no value is provided, it will try to extract the title from the dashboardHeader.
    title = "SMS TEXT Dataset", 
    ## A color theme. One of "blue", "black", "purple", "green", "red", or "yellow".
    skin = "blue"
  ),
  server = server
)
