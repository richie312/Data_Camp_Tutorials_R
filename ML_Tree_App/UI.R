## Load the necessary packages

pacman::p_load(shiny, shinythemes,gbm, randomForest,ggplot2,caret,ipred,ROCR,dplyr,ModelMetrics,
               rpart,rpart.plot,rattle,ggthemes,gridExtra,DT,corrplot) 


ui<- shinyUI(fluidPage(
  theme = shinytheme("darkly"),
  themeSelector(),
  
  navbarPage(
    title = "Data Modelling",
    id= "nav",
    tabPanel("Data", value ="Data",style="color: #FF7D33",
             
             (titlePanel("Data ToolBar")),
             
             sidebarLayout(
               
               sidebarPanel( includeCSS("mystyle.css"),
                             fileInput(inputId = "file",label = "File", buttonLabel="Upload",
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                             helpText("Default max. file size is 100MB"),
                             tags$hr(),
                             h5(helpText("Select the read.table parameters below")),
                             checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                             checkboxInput(inputId = "stringAsFactors", label = "stringAsFactors", value= FALSE),
                             br(),
                             radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                             downloadButton("downloadfile",label="Download"),
                             br(),br(),
                             actionButton(inputId = 'plot', label = 'Correlogram'), width=3),
               
               
               
               mainPanel(
                 DTOutput("table"),
                 br(), br(),
                 width = 9,
                 h5(helpText("Blocks which are blanks are statistically insignificant @ p=0.01",
                             style="color: #FF7D33")),
                 plotOutput('plot')
                 
                 
               )
               
             )
    ),
    
    tabPanel("Data Modelling", value = "Model",style="color: #FF7D33",
             
             (titlePanel("Pre-Processing and Modelling")),
             
             
             sidebarLayout(
               
               sidebarPanel( titlePanel("Missing Values"),
                             tags$hr(),
                             h5(helpText("Select the Appropriate Missing Imputation criterion")),
                             br(),
                             radioButtons(inputId = 'Imputation', label = 'Variable', 
                                          choices = c("Continous", "Categorical", "Both"), 
                                          selected = "Both"),
                             br(),
                             actionButton(inputId = "Impute", label = "Impute"),
                             tags$hr(),
                             h5(helpText("Select the column number for the response variable, you
                                         can get the column number just by not selecting the header on Data tab.")),
                             selectInput(inputId = "col_num",label="col_num", choices=c(1:50),selected = 17),
                             h5(helpText("Split the Dataset")),
                             br(),br(),
                             sliderInput(inputId = "split_ratio", label="Split Ratio", min=0, max=1, value = 0.7),
                             actionButton(inputId = "Train", label="Train"),
                             actionButton(inputId = "Test", label="Test"),
                             br(), br(),
                             tags$hr(),
                             h5(helpText("Select the Model")),
                             selectInput(inputId = "Model", label= "Model",c("Decision Tree","Random Forest",
                                                                             "Logistic Regression", "Bagging")),
                             br(),
                             sliderInput(inputId = "nTrees", label="nTrees", min=0, max=10000, value = 950),
                             br(),
                             selectInput(inputId = 'cv', label = 'Cross Validation', c(0:10)),
                             tags$hr(),
                             h5(helpText("Specifiy the response variable")),
                             textInput(inputId = "Response", label = "Response Variable", value = "Enter the response var"),
                             tags$hr(),
                             h5(helpText("Fit the Model on training data")),
                             actionButton(inputId = "Fit", label ="Fit")
                             
                             ),
               
               
               
               
               
               mainPanel(tabsetPanel(
                 tabPanel("PreProcess_Tables", value ="PreProcess",style="color: #FF7D33",
                          DTOutput("table_Imputation"),
                          br(),br(),
                          textOutput("Text_train"),
                          textOutput("obs_train"),
                          textOutput("Text_test"),
                          textOutput("obs_test")
                          
                 ),
                 tabPanel("Model_Summary", value = "Summary",
                          verbatimTextOutput("model")
                          
                 ),
                 tabPanel("Plot", value="Model_Plot",
                          
                          plotOutput("Model_Plot"),
                          br(),br(),
                          plotOutput("plot_cptable")
                          
                          
                          
                 )
                 
               )
               
               )
             )
             
    ),
    tabPanel("ROC & Confusion Matrix", value ="ROC", style="color: #FF7D33",
             (titlePanel("ROC & Confusion Matrix")),
             
             
             sidebarLayout(
               
               sidebarPanel( titlePanel("Confusion Matrix"),
                             tags$hr(),
                             h5(helpText("Generate the prediction list for each model")),
                             actionButton(inputId = "Predict_List", label = "Predict_List"),
                             br(), br(),
                             actionButton(inputId = "AUC", label="AUC"),
                             br(),br(),
                             sliderInput("threshold", label = "threshold",min=0,max=1, value=0.6),
                             br(),br(),
                             actionButton(inputId = "ConfusionMatrix", label="confusionMatrix")
                             
                             
                             
               ),
               
               
               
               
               
               mainPanel(tabsetPanel(
                 tabPanel("ROC & AUC", value ="ROC",style="color: #FF7D33",
                          textOutput("text_predict"),
                          DTOutput("List"),
                          br(), br(),
                          textOutput("text_auc"),
                          textOutput("AUC"),
                          br(),br(),
                          plotOutput("ROC_Plot")
                          
                          
                 ),
                 tabPanel("ConfusionMatrix", value = "Matrix",style="color: #FF7D33",
                          
                          plotOutput("Matrix_table"),
                          br(),br(),
                          plotOutput("Result")
                          
                          
                          
                 )
                 
               )
               
               )
             )    
    ) 
    
  )
)
)




