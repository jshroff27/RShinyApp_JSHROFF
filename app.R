#Packaged that are needed to run the projects/web application
library(shiny)
library(ggplot2)
library(plotly)
library(DataExplorer)
library(dplyr)
library(plyr)
library(tidyverse)
library(caret)
library(randomForest)
library(tidymodels)
library(data.table)
library(table1)
library(ROSE)
library(pROC)
library(ggpubr)
#setwd("~/Desktop/Jay/Harrisburg University/Semester 5/Software Validation and Testing/StrokeAnalysis/All Code and Report/Code/")
data <- read.csv("Stroke-Dataset.csv")
cols <- c(2,5,7, 8,9,12,14)
data[cols] <- lapply(data[cols], as.factor)
data <- data[-c(1)] ## as.factor() could also be used
#Cleaning the data and manipulation
dat <- data %>%
    filter(!is.na(bmi),!is.na(smoking_status)) %>%
    mutate(hypertension = as.factor(hypertension),
           heart_disease = as.factor(heart_disease),
           stroke = as.factor(stroke))
#Further Manipulation of data and creating test and training partition
dat_model <- dat %>%
    mutate(stroke = mapvalues(stroke,
                              from = c(0, 1),
                              to = c("No", "Yes")) %>% as.factor)
dat_stroke <- dat_model %>%
    filter(stroke == "Yes")
dat_stroke_no <- dat_model %>%
    filter(stroke == "No") %>%
    sample_n(4000)
dat_model <- rbind(dat_stroke, dat_stroke_no)
dat_model <- dat_model %>% mutate_if(is.character,as.factor)
dat_model <- dat_model[,-c(1,4,5,13)]
rm(dat_stroke, dat_stroke_no)

#Test/Train/Validation split
set.seed(3)
dat_indx <- createDataPartition(dat_model$stroke, 
                                p = 0.6, 
                                list = FALSE) 

dat_trn <- dat_model[dat_indx,]
dat_val <- dat_model[-dat_indx,]
# training data oversampling
dat_trn_bal <- ROSE(stroke ~ ., data=dat_trn, seed=123)$data
# partition 2
set.seed(4)
dat_indx_2 <- createDataPartition(dat_val$stroke, 
                                  p = 0.5, 
                                  list = FALSE)
dat_tst <- dat_val[dat_indx_2,]
dat_val <- dat_val[-dat_indx_2,]
# clean workspace
rm(dat_indx, dat_indx_2)

#Log Regression Model
model_logR_bal <- train(stroke ~ .,  
                        data = dat_trn_bal,
                        method = "glm", 
                        family = "binomial",
                        preProcess = c("center", "scale"),
                        trControl = trainControl(method = "cv", number = 10),
                        tuneLength = 10)

preds_logR_bal <- predict(model_logR_bal, newdata = dat_tst, type = "prob")
logR_bal_preds_class <- predict(model_logR_bal, newdata = dat_tst)
cm_logR_bal <- confusionMatrix(logR_bal_preds_class, dat_tst$stroke)


# gbm model - balanced
model_gbm_bal <- train(stroke ~., 
                       data = dat_trn_bal, 
                       method = "gbm",
                       verbose = F,
                       metric = "Accuracy",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "cv", number = 10),
                       tuneLength = 10)

preds_gbm_bal <- predict(model_gbm_bal, newdata = dat_tst, type = "prob")
gbm_bal_preds_class <- predict(model_gbm_bal, newdata = dat_tst)
cm_gbm_bal <- confusionMatrix(gbm_bal_preds_class, dat_tst$stroke)


# aggregate model metrics
model_names <- c("cm_logR_bal", "cm_gbm_bal")

metrics_model <- bind_rows(cm_logR_bal$byClass,
                           cm_gbm_bal$byClass,
                           .id = 'Model') %>%
    mutate(model = c("Logistic.Regression.Balanced",
                     "Gradient.Boosting.Balanced"))

metrics_model <- metrics_model %>%
    rename_all(funs(make.names(.))) %>%
    select(model, Balanced.Accuracy, everything()) %>%
    arrange(-Balanced.Accuracy)


# ROC Curves
names_preds <- c("preds_logR_bal",
                 "preds_gbm_bal")

metrics_roc <- list()

for (i in names_preds) {
    roc_model <- roc(response = dat_tst$stroke,
                     predictor = eval(as.name(i))[,2],
                     levels = c("No", "Yes"),
                     plot = F)
    metrics_roc[[i]] <- roc_model
}




############################# MAIN APP ############################
#The code for the application
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # App title ----
    headerPanel("Stroke Prediction"),
    
    mainPanel("Dashboard",
              fluidRow(
                  splitLayout(cellWidths = c("70%", "50%"),plotlyOutput("plot"), plotOutput("plotgraph2"))
              ),
              
              
              selectInput("y_varb", label="Y-axis variable",choices=names(data)[c(13)]),
              
              #2 Select any variable in dataset as x-variable
              selectInput("x_varb", label="X-axis variable", choices=names(data)),
              # Main panel for displaying outputs ----
              
              #3. Reset plot1 output after each selection
              #plotOutput("plot", dblclick = "plot_reset"),
              #plotOutput("plotgraph2")
    )  
)
# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
    
    remaining <- reactive({
        names(data)[-match(input$y_varb,names(data))]
    })
    
    observeEvent(remaining(),{
        choices <- remaining()
        updateSelectInput(session = getDefaultReactiveDomain(),inputId = "x_varb", choices = choices)
    })
    
    output$plot <- renderPlotly({
        
        ggplotly(ggplot(data=data,aes_string(x = input$x_varb, fill = input$y_varb)) +
                     geom_density(alpha = 0.5) +
                     labs(title = "Density Plot",
                          subtitle = "by Stroke Status",
                          y = input$y_varb, 
                          x = input$x_varb,
                          fill = "Stroke Status\n0=No 1=Yes") +
                     theme(legend.position = "bottom"))
        
    })
    
    output$plotgraph2 <- renderPlot({
        ggroc(metrics_roc) + scale_color_manual( values = c("#003D73","#0878A4","#1ECFD6","#C05640")) +
            labs(x = "False Positive Rate",
                 y = "True Positive Rate",
                 title = "ROC Curves by Model",
                 color = "Model") +
            theme(legend.position = c(0.8, 0.3),
                  plot.title = element_text(face = "bold", hjust = 0.5))
    })
}


shinyApp(ui, server)