## Set the working directory
setwd("D:/Documents/R_Projects/Data_Camp_Tutorials/ML_Tree_App/New folder/www")

library(shiny)
library(shinythemes)

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
                             fileInput(inputId = "file",label = "Upload the file"), # fileinput() function is used to get the file upload contorl option
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
                 
                 
               ),position = c("left", "right")
               
             )
    ),
    
    tabPanel("Data Modelling", value = "Model",style="color: #FF7D33",
             
             (titlePanel("Pre-Processing and Modelling")),
             
             
             sidebarLayout(
               
               sidebarPanel( titlePanel("Missing Values"),
                             tags$hr(),
                             h5(helpText("Select the Appropriate Missing Imputation criterion")),
                             
                             br(),br(),
                             radioButtons(inputId = 'Imputation', label = 'Variable', 
                                          choices = c(Continous="Continous",Categorical = "Categorical"), 
                                          selected = "Continous"),
                             tags$hr(),
                             h5(helpText("Select the Appropriate scaling and centering (if required) criterion")),
                             radioButtons(inputId = 'Scaling', label = 'Scaling & Centering', 
                                          choices = c(Normalization ="Normalization",Standardization = "Standardization"), 
                                          selected = "Standardization"),
                             tags$hr(),
                             h5(helpText("Split the Dataset")),
                             br(),br(),
                             sliderInput(inputId = "split_ratio", label="Split Ratio", min=0, max=1, value = 10),
                             actionButton(inputId = "Split", label="Split"),
                             br(), br(),
                             tags$hr(),
                             h5(helpText("Select the Model")),
                             selectInput(inputId = "Model", label= "Model",c("Decision Tree","Random Forest",
                                                                             "GBM", "Bagging")),
                             br(),
                             sliderInput(inputId = "nTrees", label="nTrees", min=0, max=10000, value = 10000),
                             br(),
                             selectInput(inputId = 'cv', label = 'Cross Validation', c(2,3,4,5,6,7,8,9,10))
                             
               ),
               
               
               
               
               
               mainPanel(
                 
                 tableOutput("table"),
                 br(), br(),
                 width = 9,
                 h5(helpText("",
                             style="color: #FF7D33")),
                 plotOutput('plot')
                 
                 
               )
               
               
               
               
               
             )
             
    ),
    tabPanel("ROC & Confusion Matrix", value ="ROC", style="color: #FF7D33",
             (titlePanel("ROC & Confusion Matrix")),
             
             
             sidebarLayout(
               
               sidebarPanel( titlePanel("Confusion Matrix"),
                             tags$hr(),
                             h5(helpText("Select the Model for confusionMatrix Computation")),
                             radioButtons(inputId = 'Scaling', label = 'Scaling & Centering', 
                                          choices = c("DecisionTree", "GBM","RandomForest","Bagging"), 
                                          selected = "RandomForest"),
                             tags$hr(),
                             h5(helpText("Select the threshold")),
                             sliderInput(inputId = "threshold", label="Threshold", min=0, max=1, value = 10),
                             tags$hr(),
                             h5(helpText("Select the Model for ROC plot")),
                             selectInput(inputId = "Model", label= "Model",c("DecisionTree","RandomForest",
                                                                             "GBM", "Bagging","AllTogether"), selected = "AllTogether")
                             
               ),
               
               
               
               
               
               mainPanel(
                 
                 tableOutput("table"),
                 br(), br(),
                 width = 9,
                 h5(helpText("",
                             style="color: #FF7D33")),
                 plotOutput('plot')
                 
                 
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
                 
                 tableOutput("table"),
                 br(), br(),
                 width = 9,
                 h5(helpText("",
                             style="color: #FF7D33")),
                 plotOutput('plot')
                 
                 
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
    read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })
  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    head(data())
  })
  output$downloadfile<-downloadHandler(
    filename = function(){
      paste("downloadfile_",Sys.Date(),'.csv',sep='')},
    content = function(file){
      write.csv(data()
                , file)
    }
  )
  
  ## Numeric class for the data
  
  data_num<-reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    temp<- read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    temp<-as.data.frame(lapply(temp, as.factor))
    temp<-as.data.frame(lapply(temp, as.numeric))
    temp
  })
  
  
  
  event<-eventReactive(input$plot, {
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    library(corrplot)
    source("D:/Documents/R_Projects/Data_Camp_Tutorials/ML_Tree_App/p-value_mat.R")
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
}
)

# Run the application 
shinyApp(ui = ui, server = server)