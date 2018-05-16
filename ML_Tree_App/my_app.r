
library(rsconnect)
rsconnect::setAccountInfo(name='richiekalpa',
                          token='040A79FED1BB0C3491371EF2D2463CA7',
                          secret='JcwL+7hxAjDeS9wBfZ1Ijb8cSmhQS5Pj4PrbwWsA')
deployApp(appName = 'my_app')

## Load the necessary packages
## Load the necessary packages

library(shiny)
library(shinythemes)
library(gbm)
library(randomForest)
library(ggplot2)
library(ipred)
library(caret)
library(ROCR)
library(dplyr)
library(ModelMetrics)
library(rpart)
library(rpart.plot)
library(ggthemes)
library(gridExtra)
library(DT)
library(corrplot)

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





server<- shinyServer(function(input,output){
  
  ## Set the seed
  
  set.seed(123)
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.csv(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })
  
  output$table <- renderDataTable({
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
    
    source("functions.R")
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
    source("functions.R")
    file1<-input$file
    temp2<-read.csv(file=file1$datapath, sep=input$sep, 
                    header = input$header, stringsAsFactors = input$stringAsFactors)
    # create logical set for numeric columns
    
    temp2[,!sapply(temp2,is.numeric)]<-lapply(temp2[,!sapply(temp2,is.numeric)], function(x) replace(x, is.na(x), Mode(x[!is.na(x)])))
    temp2
  })    
  
  both_na<-reactive({
    source("functions.R")
    
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
  
  output$table_Imputation<- renderDataTable({
   head(event_impute())
  })
  
 
  
  ## Event_Split
  
  
  event_Train<-eventReactive(input$Train, {
    source("functions.R")
    
    runif(input$Train == 1)
    train<-get_dataset(both_na(),split_ratio = input$split_ratio, set = "train")
    train
    
  })
  
  
  
  event_Test<-eventReactive(input$Test, {
    source("functions.R")
    
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
                    data = event_Train(),
                    n.trees = input$nTrees,
                    cv.fold= input$cv)
      return(RF)
      
      
    }
    if(input$Model == "Decision Tree"){
      
      Tree<-rpart(formula = f(),
                  data = event_Train())
      return(Tree)
    }
    
    })
  ## Model Summary
  output$model<-renderPrint({summary(event_fit())})
  
  ## Model Plots(Insight)
  cp_table<-reactive({as.data.frame(event_fit()$cptable)})
  
 plot_cptable_xerror<-reactive({
    
    ggplot(data=cp_table(),aes(x=CP, y=xerror))+
      geom_smooth(method='loess')+
      theme_economist()+
     ggtitle("cp parameter vs xerror")+
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"),
            axis.line = element_line(colour = "black"))+
      theme(legend.position="none")
  })
  
 plot_cptable_nsplit<-reactive({
    ggplot(data=cp_table(),aes(x=nsplit, y=xerror))+
      geom_smooth(method='loess')+
      theme_economist()+
     ggtitle("nsplit vs xerror")+
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"),
            axis.line = element_line(colour = "black"))+
      theme(legend.position="none")
  })
  output$plot_cptable<-renderPlot({
    if (input$Model == "Decision Tree"){
    grid.arrange(plot_cptable_xerror(),plot_cptable_nsplit(),ncol=2,nrow=1)
    }
    else{return()}
    
  })
  
  ## Model Plot
  
  output$Model_Plot<-renderPlot({
    
    if(input$Model == "Decision Tree"){
      par(bg="#E8F6F3")
      fancyRpartPlot({event_fit()}, cex = 0.9)
    }
   
    if(input$Model == "Random Forest"){
      par(mfrow=c(1,2),bg = "#EAF2F8")
      varImpPlot({event_fit()}, main="Variable Importance")
      plot(event_fit())
      legend(x = "right", 
             legend = colnames(event_fit()$err.rate),
             fill = 1:ncol(event_fit()$err.rate))
    }
    if(input$Model == "Logistic Regression"){
      par(bg="#F4ECF7")
      plot({event_fit()$residuals}, main= "Residual plot")
    }
    else{
      return()
    }
    
  }
  )
  ## ROC
    
    ## List of Predicted values for each Models
 
    Pred<-eventReactive(input$Predict_List,{
      runif(input$Predict_List == 1)
      if(input$Model == "Bagging"){ 
        predict<-predict(object=event_fit(),
                newdata = event_Test(),
                       type='prob')
        return(predict)
      }
      if(input$Model == "Random Forest"){ 
        predict<-predict(object=event_fit(),
                         newdata = event_Test(),
                         type='prob')
        return(predict)
      }
      if(input$Model == "Logistic Regression"){ 
        predict<-predict(object=event_fit(),
                         newdata = event_Test(),
                         type='response')
        return(predict)
      }
      if(input$Model == "Decision Tree"){ 
        predict<-predict(object=event_fit(),
                         newdata = event_Test(),
                         type='prob')
        return(predict)
      }
      
      else{return(NULL)}
    })
    
    ## Output for Prediction
    output$text_predict<-renderText({
      if(input$Predict_List == 1){
        paste("The first five predicted values of the selected model,")
      }
    })
    
    output$List<-renderDataTable({head(Pred())})
    
    ## AUC
    AUC<-eventReactive(input$AUC,{
      runif(input$AUC == 1)
        if(input$Model == "Bagging"){
        score<-auc(actual = event_Test()[,as.numeric(paste(input$col_num))], predicted = Pred()[,"yes"])
        return(score)  
        }
      if(input$Model == "Logistic Regression"){
        score<-auc(actual = event_Test()[,as.numeric(paste(input$col_num))], predicted = Pred())
        return(score)  
      }
      if(input$Model == "Random Forest"){
        score<-auc(actual = ifelse(event_Test()[,as.numeric(paste(input$col_num))] == "yes", 1, 0), 
                   predicted = Pred()[,"yes"]) 
        return(score)  
      }
      if(input$Model == "Decision Tree"){
        score<-auc(actual = event_Test()[,as.numeric(paste(input$col_num))], predicted = Pred()[,"yes"])
        return(score)  
      }
      else{return(NULL)}
      
      })
      
     ## Output for AUC Score
    
    output$text_auc<-renderText({
      if(input$AUC == 1){
        paste("The AUC of the selected model is,")
      }
    })
    
    output$AUC<-renderPrint({as.numeric(round(AUC(),3))})
    
    
    ## ROC plots

    output$ROC_Plot<-renderPlot({ 
      if(input$Model == "Random Forest"){
      pred<-prediction(Pred()[,"yes"], ifelse(event_Test()[,as.numeric(paste(input$col_num))] == "yes", 1, 0))
      par(bg = "#D1F2EB")
      roc<-performance(pred,"tpr","fpr")
      plot(roc, main = "Test Set ROC Curves",col = "green")
      legend("bottomright", legend = input$Model)
      }
      
      if(input$Model == "Logistic Regression" ){
        pred<-prediction(Pred(), ifelse(event_Test()[,as.numeric(paste(input$col_num))] == "yes", 1, 0))
        roc<-performance(pred,"tpr","fpr")
        par(bg = "#D1F2EB")
        plot(roc, main = "Test Set ROC Curves",col = "brown")
        legend("bottomright", legend = input$Model)
      }
      if(input$Model == "Decision Tree" ){
        pred<-prediction(Pred()[,"yes"], ifelse(event_Test()[,as.numeric(paste(input$col_num))] == "yes", 1, 0))
        par(bg = "#EBDEF0")
        roc<-performance(pred,"tpr","fpr")
        plot(roc, main = "Test Set ROC Curves",col = "red")
        legend("bottomright", legend = input$Model)
      }
      if(input$Model == "Bagging" ){
        pred<-prediction(Pred()[,"yes"], ifelse(event_Test()[,as.numeric(paste(input$col_num))] == "yes", 1, 0))
        par(bg = "#FEF5E7")
        roc<-performance(pred,"tpr","fpr")
        plot(roc, main = "Test Set ROC Curves", col = "blue")
        legend("bottomright", legend = input$Model)
      }
      
        else{return()}
      
  })
    
    ## Confusion Matrix
    
    event_Matrix<-eventReactive(input$ConfusionMatrix,{
      
     
      
      pred<-reactive({pred<-factor(ifelse(Pred()[,"yes"]>input$threshold,"yes","no"))
                      pred<-relevel(pred,"yes")
                      })
      ## For Logisitic Regression
      pred1<-reactive({pred1<-factor(ifelse(Pred()>input$threshold,"yes","no"))
      pred1<-relevel(pred1,"yes")
      })
      
      runif(input$ConfusionMatrix == 1)
      if(input$Model == "Logistic Regression"){
        cm = caret::confusionMatrix(reference = event_Test()[,as.numeric(paste(input$col_num))] ,
                                    data = pred1())
        return(cm)
        
      }
      else{
        cm = caret::confusionMatrix(reference = event_Test()[,as.numeric(paste(input$col_num))] ,
                             data = pred())
       return(cm)
     
      }
    })
    
    output$Matrix_table<-renderPlot({
      par(bg="#E9F7EF")
      
      fourfold_table<-as.table(as.matrix(event_Matrix()$table))
      fourfoldplot(fourfold_table, color = c("#CC6666", "#99CC99"),
                   conf.level = 0, margin = 1, main = "Confusion Matrix")
      
      })
    
    output$Result<-renderPlot({
      ##ADD in the byClass result
      par(bg="#EAFAF1")
      plot(c(100,0),c(100,0), type = "n", xlab="",ylab="", main ="Details",xaxt='n',yaxt='n')
      text(10, 85, names(event_Matrix()$byClass[1]), cex=1.2, font=2)
      text(10, 70, round(as.numeric(event_Matrix()$byClass[1]), 3), cex=1.2)
      text(30, 85, names(event_Matrix()$byClass[2]), cex=1.2, font=2)
      text(30, 70, round(as.numeric(event_Matrix()$byClass[2]), 3), cex=1.2)
      text(50, 85, names(event_Matrix()$byClass[5]), cex=1.2, font=2)
      text(50, 70, round(as.numeric(event_Matrix()$byClass[5]), 3), cex=1.2)
      text(70, 85, names(event_Matrix()$byClass[6]), cex=1.2, font=2)
      text(70, 70, round(as.numeric(event_Matrix()$byClass[6]), 3), cex=1.2)
      text(90, 85, names(event_Matrix()$byClass[7]), cex=1.2, font=2)
      text(90, 70, round(as.numeric(event_Matrix()$byClass[7]), 3), cex=1.2)
      
      ## Add in the overall result
      
      text(30, 35, names(event_Matrix()$overall[1]), cex=1.5, font=2)
      text(30, 20, round(as.numeric(event_Matrix()$overall[1]), 3), cex=1.4)
      text(70, 35, names(event_Matrix()$overall[2]), cex=1.5, font=2)
      text(70, 20, round(as.numeric(event_Matrix()$overall[2]), 3), cex=1.4)  
      
      })
    
    
}

)

    


# Run the application 
shinyApp(ui = ui, server = server)