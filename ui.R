# UI for ROC
shinyUI(pageWithSidebar(
  headerPanel("ROC of SVM exercise prediction by roll and pitch belt"),
  sidebarPanel(
    wellPanel(
      h5("Select outputs to group for prediction"),
      checkboxInput("A","A", FALSE),
      checkboxInput("B","B", FALSE),
      checkboxInput("C","C", TRUE),
      checkboxInput("D","D", TRUE),
      checkboxInput("E","E", FALSE),
      actionButton("goButton", "Apply"),
      h5("HELP"),
      p("Documentation:",a("ROC plot from SVM",href="helprocsvm.html"))
    ),
    sliderInput('gammaset', label='Set the Gamma for SVM',value = 10, min = 0, max = 20, step = .5),
    sliderInput('costset', label='Set the Cost for SVM',value = 10, min = 0, max = 20, step = .5),
    verbatimTextOutput("overallAccuracy"),
    verbatimTextOutput("myConfusion")
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plots",
          plotOutput('myROC'),
          plotOutput('myROC2')   
      ),
      tabPanel("Table of Data",
           h5("Random sample of 2000 from 20,000"),
           tableOutput('myTable')    
      )
    )
    
  )
))