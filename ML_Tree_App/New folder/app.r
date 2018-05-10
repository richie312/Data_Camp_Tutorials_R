## Load the necessary packages

pacman::p_load(shiny, shinythemes,gbm, randomForest,ggplot2,ipred,caret,ROCR,dplyr,ModelMetrics) 

PATH<-"D:/Documents/R_Projects/Data_Camp_Tutorials/ML_Tree_App/New folder/www/mystyle.css"
ui<- shinyUI(fluidPage(
  theme = shinytheme("darkly"),
  themeSelector(),
  
  navbarPage(
    title = "Data Modelling",
    id= "nav",
    tabPanel("Data", value ="Data",style="color: #FF7D33",
             
             (titlePanel("Data ToolBar")),
             
             sidebarLayout(
               
               sidebarPanel( includeCSS(PATH),
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
                 tableOutput("table"),
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
                             h5(helpText("Split the Dataset")),
                             br(),br(),
                             sliderInput(inputId = "split_ratio", label="Split Ratio", min=0, max=1, value = 10),
                             actionButton(inputId = "Train", label="Train"),
                             actionButton(inputId = "Test", label="Test"),
                             br(), br(),
                             tags$hr(),
                             h5(helpText("Select the Model")),
                             selectInput(inputId = "Model", label= "Model",c("Decision Tree","Random Forest",
                                                                             "Logistic Regression", "Bagging")),
                             br(),
                             sliderInput(inputId = "nTrees", label="nTrees", min=0, max=10000, value = 10000),
                             br(),
                             selectInput(inputId = 'cv', label = 'Cross Validation', c(2,3,4,5,6,7,8,9,10)),
                             tags$hr(),
                             h5(helpText("Specifiy the response variable column number")),
                             textInput(inputId = "Response", label = "response", value = "Enter the response var"),
                             tags$hr(),
                             h5(helpText("Select the column number in order to convert the Response 
                                         variable into factor (only for Random Forest)")),
                             tags$hr(),
                             selectInput(inputId = "col_num",label="col_num", choices=c(1:50)),
                             h5(helpText("Fit the Model on training data")),
                             actionButton(inputId = "Fit", label ="Fit")
                             
               ),
               
               
               
               
               
               mainPanel(tabsetPanel(
                 tabPanel("PreProcess_Tables", value ="PreProcess",style="color: #FF7D33",
                 tableOutput("table_Imputation"),
                 br(),br(),
                 tableOutput("std_table"),
                 br(),br(),
                 textOutput("Text_train"),
                 textOutput("obs_train"),
                 textOutput("Text_test"),
                 textOutput("obs_test")
                 
                 ),
                 tabPanel("Model_Summary", value = "Summary",
                          verbatimTextOutput("model")
                          
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
                             h5(helpText("Select the Model for confusionMatrix Computation")),
                              radioButtons(inputId = 'confusionMatrixc', label = 'confusionMatrix', 
                                          choices = c("DecisionTree", "Logistic Regression","RandomForest","Bagging"), 
                                          selected = "RandomForest"),
                             tags$hr(),
                             h5(helpText("Select the threshold")),
                             sliderInput(inputId = "threshold", label="Threshold", min=0, max=1, value = 10),
                             tags$hr(),
                             h5(helpText("Select the Model for ROC plot")),
                             selectInput(inputId = "Model", label= "Model",c("DecisionTree","RandomForest",
                                                                             "Logistic Regression", "Bagging","AllTogether"), selected = "AllTogether")
                             
                             
               ),
               
               
               
               
               
               mainPanel(
                 
                 
                 
                 
               )
               
             )
    ),
    
    tabPanel("Optimization and Tuning", value="Optimization", style="color: #FF7D33",
             (titlePanel("Hyperparameter Tuning")),
             
             
             sidebarLayout(
               
               sidebarPanel( titlePanel("Optimization"),
                             tags$hr(),
                             h5(helpText("Select the Model for Optimization")),
                             radioButtons(inputId = 'Optimization', label = 'Optimization', 
                                          choices = c("DecisionTree", "GBM","RandomForest","Bagging"), 
                                          selected = "RandomForest"),
                             br(),
                             actionButton(inputId ="get_opt", label = "Get Optmisation Value"),
                             br(),br(),
                             actionButton(inputId = "Predict", label= "Predict on test set"),
                             br(),br(),
                             actionButton(inputId = "Compare", label = "Plot and Compare"),
                             br(), br(),
                             fileInput(inputId = "test_file", label = "Unseen Test File"),
                             br(), br(),
                             actionButton(inputId = "Predict2", label= "Predict on unseen test set"),
                             br(), br(),
                             downloadButton(outputId = "submission", label="Download Submission File")
                             
                             
               ),
               
               
               
               
               
               mainPanel(
                 
                 
                 
               )
               
             )
             
    ) 
  )
)
)





server<- shinyServer(function(input,output){
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.csv(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })
  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    head(data())
  })
  output$downloadfile<-downloadHandler(
    filename = function(){
      paste("downloadfile_",Sys.Date(),'.csv',sep='')},
    content = function(file){
      write.csv(data_num()
                , file)
    }
  )
  
  ## Numeric class for the data
  
  data_num<-reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    temp<- read.csv(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    temp<-as.data.frame(lapply(temp, as.factor))
    temp<-as.data.frame(lapply(temp, as.numeric))
    ## Missing values
    temp[,sapply(temp,is.numeric)]<-lapply(temp[,sapply(temp,is.numeric)], function(x) replace(x, is.na(x), mean(x[!is.na(x)])))
    temp[,!sapply(temp,is.numeric)]<-lapply(temp[,!sapply(temp,is.numeric)], function(x) replace(x, is.na(x), Mode(x[!is.na(x)])))
    temp
  })
  
  
  
  event<-eventReactive(input$plot, {
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    library(corrplot)
    source("D:/Documents/R_Projects/Data_Camp_Tutorials/ML_Tree_App/functions.R")
    library(RColorBrewer)
    runif(input$plot == 1)
    
    num<-cor(data_num())
    par(mfrow=c(1,2))
    corrplot(num, method ='color',type='upper',tl.col='black',
             order='hclust',number.cex  = 0.7,addCoef.col='#A9A9A9',
             p.mat = cor.mtest(num), sig.level = 0.01,insig='blank')
    corrplot(num, type="upper", order="hclust",
             col=brewer.pal(n=8, name="RdYlBu"))
    
  }
  
  
  )
  output$plot<- renderPlot({event()})
  
  ## Imputation for missing values
  ## For numeric variables
  
  
  num_na<-reactive({
    file1<-input$file
    temp1<-read.csv(file=file1$datapath, sep=input$sep, 
               header = input$header, stringsAsFactors = input$stringAsFactors)
  # create logical set for numeric columns
  
    temp1[,sapply(temp1,is.numeric)]<-lapply(temp1[,sapply(temp1,is.numeric)], function(x) replace(x, is.na(x), mean(x[!is.na(x)])))
    temp1
})
  categorical_na<-reactive({
    source("D:/Documents/R_Projects/Data_Camp_Tutorials/ML_Tree_App/functions.R")
    file1<-input$file
    temp2<-read.csv(file=file1$datapath, sep=input$sep, 
                    header = input$header, stringsAsFactors = input$stringAsFactors)
    # create logical set for numeric columns
    
    temp2[,!sapply(temp2,is.numeric)]<-lapply(temp2[,!sapply(temp2,is.numeric)], function(x) replace(x, is.na(x), Mode(x[!is.na(x)])))
    temp2
  })    
  
  both_na<-reactive({
    source("D:/Documents/R_Projects/Data_Camp_Tutorials/ML_Tree_App/functions.R")
    file1<-input$file
    temp3<-read.csv(file=file1$datapath, sep=input$sep, 
                    header = input$header, stringsAsFactors = input$stringAsFactors)
    # create logical set for numeric columns
    temp3[,sapply(temp3,is.numeric)]<-lapply(temp3[,sapply(temp3,is.numeric)], function(x) replace(x, is.na(x), mean(x[!is.na(x)])))
    temp3[,!sapply(temp3,is.numeric)]<-lapply(temp3[,!sapply(temp3,is.numeric)], function(x) replace(x, is.na(x), Mode(x[!is.na(x)])))
    temp3
  })    
  
  event_impute<-eventReactive(input$Impute, {
    runif(input$Impute == 1)
    if(input$Imputation == "Continous"){
      num_na()
      }
    if(input$Imputation == "Categorical"){
      categorical_na()
      }
    else
      both_na()
     
  })
  
  output$table_Imputation<- renderTable({
   head(event_impute())
  })
  
 
  
  ## Event_Split
  
  
  event_Train<-eventReactive(input$Train, {
    source("D:/Documents/R_Projects/Data_Camp_Tutorials/ML_Tree_App/functions.R")
    
    runif(input$Train == 1)
    train<-get_dataset(both_na(),split_ratio = input$split_ratio, set = "train")
    train
    
  })
  
  
  
  event_Test<-eventReactive(input$Test, {
    source("D:/Documents/R_Projects/Data_Camp_Tutorials/ML_Tree_App/functions.R")
    
    runif(input$Test == 1)
    test<-get_dataset(both_na(),split_ratio = input$split_ratio, set = "test")
    test
    
  })
  output$obs_train<-renderPrint({nrow(event_Train())})
  output$obs_test<-renderPrint({nrow(event_Test())})
  
  output$Text_train<-renderText({"The number of observation in train set is,"})
  output$Text_test<-renderText({"The number of observation in  test set is,"})
  
  
  ## Fit the model
  

  
  event_fit<-eventReactive(input$Fit,{ 

    
    
    f<-reactive({as.formula(paste(input$Response, "~ ."))})
    
    runif(input$Fit == 1)
    if(input$Model == "Bagging"){
  
      
    bag<- bagging(formula = f(), 
                 distribution = "bernoulli",
                 data = event_Train(),
                 n.trees = input$nTrees,
                 cv.fold= input$cv)
     return(bag)
    }
    if(input$Model == "Logistic Regression"){
      
      GLM<- glm(formula = f(), 
                    family=binomial(link='logit'),
                    data = event_Train())
      return(GLM)
    }
    if(input$Model == "Random Forest"){
      
      RF<- randomForest(formula = f(), 
                    distribution = "bernoulli",
                    data = event_Train(),
                    n.trees = input$nTrees,
                    cv.fold= input$cv)
      return(RF)
      
      
    }
    
    })
  
  output$model<-renderPrint({summary(event_fit())})
  
  
}
)

# Run the application 
shinyApp(ui = ui, server = server)