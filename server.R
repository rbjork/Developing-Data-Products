# Server

library(e1071)
library(caret)
library(MASS)
library(ROCR)
library(shiny)

trainfile <- "pml-reduced.csv";
dataPML <- read.csv(trainfile,stringsAsFactors=TRUE)
dataPML <- dataPML[,-1]

#debugset=sample(2000,50)
#dataPML <- dataPML[debugset,]

len <- length(dataPML[,1])
set.seed(122)
trainIndex=sample(len,3*len/4)
train = trainIndex
x <- dataPML[,-which(names(dataPML) == "classe")]; # though should be matrix
y <- dataPML$classe # though should be numeric 1 or 2
catv = vector(mode="character",length=len)

for( i in 1:len){
  if(dataPML[i,]$classe == "D" || dataPML[i,]$classe == "E"){
    catv[i] <- c("SELECT")
  }else{
    catv[i] <- c("OTHER")
  }
}
classeSelect = as.factor(catv)
print(classeSelect)
#data <- data[,-which(names(data) == "classe")]
dat = data.frame(roll_belt=x[,1],pitch_belt=x[,2],y=classeSelect) 
#print(dat)

# Since the data below works perhaps 
# best strategy is to simply shape the data of interest, that above,
# into that below:
# set.seed(1)
# x=matrix(rnorm(200*2),ncol=2)
# x[1:100,]=x[1:100,]+2
# x[101:150,]=x[101:150,]-2
# y=c(rep(1,150),rep(2,50))
# dat2=data.frame(x=x,y=as.factor(y))
# 
# train=sample(200,100)

assignGroups <- function(a,b,c,d,e){
  
  groupvalues <- c("z")
  
  if(a)groupvalues = append(groupvalues,"A")
  if(b)groupvalues = append(groupvalues,"B")
  if(c)groupvalues = append(groupvalues,"C")
  if(d)groupvalues = append(groupvalues,"D")
  if(e)groupvalues = append(groupvalues,"E")
  #print(groupvalues)
  catv = vector(mode="character",length=len)
  for( i in 1:len){
    if(any(dataPML[i,]$classe == groupvalues)){
      catv[i] <- c("SELECT")
    }else{
      catv[i] <- c("OTHER")
    }
  }
  
  classeSelect = as.factor(catv)
  dat <- data.frame(roll_belt=x[,1],pitch_belt=x[,2],y=classeSelect) 
}

rocplot <- function(pred,truth, ...){
  predob = prediction(pred,truth)
  perf = performance(predob,"tpr","fpr")
  plot(perf,...)
 # print(pred)
}

shinyServer(
  function(input, output) {
   
    gammaset <- reactive({input$gammaset})
    
    costset <- reactive({input$costset})
    
    datv <- reactive({
      input$goButton
      isolate(assignGroups(input$A,input$B,input$C,input$D,input$E))
    })
    
    
    modelFit <- reactive({
      svm(y~., data=datv()[train,],kernel="radial",gamma=gammaset(),cost=costset(),decision.values=T)
    })
    
    fitted <- reactive({attributes(predict(modelFit(), datv()[train,], decision.values=TRUE))$decision.values})
    predictionsTest <- reactive({predict(modelFit(),datv()[-train,],decision.values=T)})
    fittedTest <- reactive({attributes(predictionsTest())$decision.values})
    
    output$myROC <- renderPlot({
      rocplot(fitted(),datv()[train,"y"],main="ROC - black:Train data, red:Test data")
      rocplot(fittedTest(),datv()[-train,"y"],add=T,col="red")
    })
   
    output$myROC2 <- renderPlot({
       plot(modelFit(),datv()[-train,],page=FALSE,new=TRUE)
    })
    
    cm <- reactive({confusionMatrix(predictionsTest(),datv()[-train,"y"])})
      
    output$overallAccuracy <- renderPrint({
      cm()$overall[1]
    })
      
    output$myConfusion <- renderPrint({
      cm()$table
    })
    
    output$myTable <- renderTable({
      data.frame(datv()[-train,],predictionsTest())
    })
  }
)