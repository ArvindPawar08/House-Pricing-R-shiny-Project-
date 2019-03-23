#Teammates- Arvind Pawar and Ashish Salunkhe
#UI
#install.packages("sqldf")
#install.packages("shinydashboard")
library(shiny)
library(sqldf)

library(shinydashboard)
library (ggplot2)
library(scales)
library(dplyr)
setwd("C:/Users/Arvind/Desktop/Communication and Visualization for Data Analytics") 
getwd()

house <- read.csv("House_Price_data.csv")


ui <- shinyUI(fluidPage(pageWithSidebar(
  headerPanel('NextGen Realty Company'),
  
  sidebarPanel(
    
    selectInput('Neighborhood',label='Neighborhood',
                c("CollgCr","Veenker","Crawfor","NoRidge","Mitchel","Somerst","NWAmes","OldTown","BrkSide",
                  "Sawyer","NridgHt","SawyerW","IDOTRR","MeadowV","Edwards","Timber","Gilbert","StoneBr","ClearCr"
                  ,"NPkVill" ,"Blmngtn","BrDale","SWISU","Blueste")
    ),
    
    actionButton("Go", "Average sale price in this Neighborhood")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Output1", plotOutput("plot")),
      tabPanel("SalePrice Distribution",plotOutput("s")),
      tabPanel("SalePrice Vs Age",plotOutput("a")),
      tabPanel("SalePrice in Neighborhood",plotOutput("Highest_SalePrice")))
  ))) )


#server

server<-shinyServer(function(input, output) {
  
  output$s <- renderPlot({
    s<-ggplot(data=house[!is.na(house$SalePrice),], aes(x=SalePrice)) +
      geom_histogram(fill="orange", binwidth = 10000) +
      scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
    s
  })
    
  output$a<- renderPlot({
    house$Remod= ifelse(house$YearBuilt==house$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
    
    house$Age_Of_Houses <- house$YrSold-house$YearRemodAdd
    a<-ggplot(data=house[!is.na(house$SalePrice),], aes(x=Age_Of_Houses, y=SalePrice))+
      geom_point(col='red') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
      scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
    a
  })
  
  
  output$Highest_SalePrice<-renderPlot({
    y=aggregate(house,list(house$Neighborhood),mean)
    x=y[,c('Group.1','SalePrice')]
    x<-x %>% arrange(SalePrice)
    x
    x$Group.1 = factor(x$Group.1,x$Group.1)  	 
    Highest_SalePrice<-ggplot(tail(x,15), xlab="Neighborhood") + geom_bar(aes(x=reorder(Group.1,-SalePrice),y=SalePrice, fill=SalePrice),stat='identity')
    Highest_SalePrice
  })
  
 
  
  
  
  output$plot <- renderPlot({
    seldata <- reactive({
      abc <- input$Neighborhood
      print(abc)
      
      if (abc == "CollgCr")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'CollgCr'
      group by YrSold"
      }
      if (abc == "Veenker")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'Veenker'
      group by YrSold"
      }
      if (abc == "Crawfor")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'Crawfor'
      group by YrSold"
      }
      if (abc == "NoRidge")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'NoRidge'
      group by YrSold"
      }
      if (abc == "Mitchel")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'Mitchel'
      group by YrSold"
      }
      if (abc == "Somerst")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'Somerst'
      group by YrSold"
      }
      if (abc == "NWAmes")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'NWAmes'
      group by YrSold"
      }
      if (abc == "OldTown")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'OldTown'
      group by YrSold"
      }
      if (abc == "BrkSide")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'BrkSide'
      group by YrSold"
      }
      if (abc == "Sawyer")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'Sawyer'
      group by YrSold"
      }
      if (abc == "NridgHt")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'NridgHt'
      group by YrSold"
      }
      if (abc == "SawyerW")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'SawyerW'
      group by YrSold"
      }
      if (abc == "IDOTRR")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'IDOTRR'
      group by YrSold"
      }
      if (abc == "MeadowV")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'MeadowV'
      group by YrSold"
      }
      if (abc == "Edwards")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'Edwards'
      group by YrSold"
      }
      if (abc == "Timber")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'Timber'
      group by YrSold"
      }
      if (abc == "Gilbert")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'Gilbert'
      group by YrSold"
      }
      if (abc == "StoneBr")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'StoneBr'
      group by YrSold"
      }
      if (abc == "ClearCr")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from 
      where Neighborhood = 'ClearCr'
      group by YrSold"
      }
      if (abc == "NPkVill")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'NPkVill'
      group by YrSold"
      }
      if (abc == "Blmngtn")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'Blmngtn'
      group by YrSold"
      }
      if (abc == "BrDale")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'BrDale'
      group by YrSold"
      }
      if (abc == "SWISU")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from 
      where Neighborhood = 'SWISU'
      group by YrSold"
      }
      if (abc == "Blueste")
      {sql = "SELECT avg(SalePrice) as SalePrice, Neighborhood, YrSold
      from house
      where Neighborhood = 'Blueste'
      group by YrSold"
      }
      sqldf(sql)
      }
    )
    if(input$Go)
    {
      ggplot(seldata(),mapping = aes(x=YrSold,y=SalePrice))+
        geom_line()
      
    }
    
      }) 
      }
  )
shinyApp(ui,server)
















