library(shiny)
library(leaflet)
library(ggmap)
library(RColorBrewer)
library(sqldf)

remove(Clusters)
Clusters<-data.frame()

num <- length(CleanedList)

#num is 1326
for (i in 1:num){
  Clusters[i,1] <- CleanedList[[i]]$CentID
  Clusters[i,2] <- as.numeric(CleanedList[[i]]$CentLoc[1])
  Clusters[i,3] <- as.numeric(CleanedList[[i]]$CentLoc[2])
  Clusters[i,4] <- CleanedList[[i]]$CentWgt
}

colnames(Clusters)[1] <- "ClusID"
colnames(Clusters)[2] <- "Latitude"
colnames(Clusters)[3] <- "Longitude"
colnames(Clusters)[4] <- "CentWgt"

head(Clusters, 5)

ui<-fluidPage(
  
  # Application title
  titlePanel("Cleaned Clusters Plot!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("clusters",
                  "Number of clusters",
                  min = 1,
                  max = 1326,
                  value =c(1,1326),width="100%"),
      sliderInput("ClusterSize",
                  "Cluster Size/Weight",
                  min = 1,
                  max = 196063,
                  value =c(1,196063),width="100%")
      ,width = 2
    ),
    
    #Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map", width="120%", height="700px")
    )
  )
)

server <- function(input, output) {
  selectedData <- reactive({
    #Compose data frame
    Clusters[input$clusters[1]:input$clusters[2],]
    
    #strOne<-paste("select * from Clusters where ClusID >=", input$clusters[1], 
    #              "AND ClusID<=", input$clusters[2])
    #,
    #"AND CentWgt>=", input$ClusterSize[1],
    #"AND CentWgt<=", input$ClusterSize[2])
    #print(strOne)
    #data<-sqldf(strOne,stringsAsFactors = FALSE)
    
  })
  
  output$map <- renderLeaflet({
    leaflet(selectedData()) %>% addTiles() %>% 
      addMarkers(clusterOptions = markerClusterOptions())
  })
}

shinyApp(ui = ui, server = server)
