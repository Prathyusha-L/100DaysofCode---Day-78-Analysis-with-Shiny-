#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Linear Regression"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("Income",
                     "Select Income:",
                     min = 40,
                     max = 250,
                     value = 45)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderTable({
     CustCsv <- read.csv("c:\\R\\Customer_Age_Income.csv");
     Income_DF <- data.frame(Inc = CustCsv$Income[1:6], Spend = CustCsv$SalesAmt[1:6]);
     Income_DF
     
     Model_lm <- lm(Spend ~ Inc, data=Income_DF)
     NewInc <- data.frame(Inc=input$Income)
     NewInc
     Spend_Value <- predict(Model_lm,NewInc)
     Spend_Value
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

