## Load pkgs
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)

## Load data 
station <- read.csv("Data/station0704.csv", header = T)
rf <- read.csv("Data/rf0708.csv", header = T)

## User Interface
if(interactive()) {
  ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title="Dashboard"),
    dashboardSidebar(
      # width = 200,
      sidebarMenu(
        menuItem("Interactive Map", tabName="map", icon=icon("map-marker", lib = "font-awesome")),
        menuItem("Widgets", tabName="widget", icon=icon("globe", lib = "font-awesome"))
      )
    ),
    
    ## Body content
    dashboardBody(
      tabItems(
        ## First tab 
        tabItem(
          tabName = "map",
          fluidPage(
            column(
              width = 4,
              fluidRow(
                box(
                  width = 12,
                  title = "Location",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("select",  
                              h3("Choose a city/county"),
                              choices = list(
                                "宜蘭縣"= 1, "花蓮縣"= 2, "金門縣"= 3, "南投縣"= 4, "屏東縣"= 5, "苗栗縣"= 6,
                                "桃園市"= 7, "高雄市"= 8, "基隆市"= 9, "連江縣"=10, "雲林縣"=11, "新北市"=12,
                                "新竹市"=13, "新竹縣"=14, "嘉義市"=15, "嘉義縣"=16, "彰化縣"=17, "臺中市"=18,
                                "臺北市"=19, "臺東縣"=20, "臺南市"=21, "澎湖縣"=22))
                ) # selectInput box
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Histogram",
                  status = "warning",
                  solidHeader = TRUE,
                  plotOutput("myplot", height="300px")
                ) # histogram box
              )
            ), # column 4
            column(
              width = 8,
              fluidRow(
                tags$style(type="text/css", "#map {height: calc(100vh - 80px) !important;}"),
                box(
                  width = 15,
                  title = "Map",
                  status = "danger",
                  solidHeader = TRUE,
                  leafletOutput("mymap", height="500px")
                ) # map box
              )
            ), # column 8
            column(
              width = 6,
              fluidRow(
                box(
                  width = 12,
                  title = "Summary Table",
                  status = "info", 
                  solidHeader = TRUE,
                  tableOutput("mytable")
                ) # table box
              )
            ) # column 6
          ) # fluidPage
        ), # tabItem for map
        ## Second tab 
        tabItem(
          tabName = "widget",
          h3("Another Tab", align="center"),
          box()
        )
      ) # tabItems (several tabs)
    ) # dashboardBody
    
  )# dashboardPage
  
  server <- function(input, output, session) {
    index <- reactive({
      switch(input$select,
             "1" = which(station$cityID == "1"),
             "2" = which(station$cityID == "2"),
             "3" = which(station$cityID == "3"),
             "4" = which(station$cityID == "4"),
             "5" = which(station$cityID == "5"),
             "6" = which(station$cityID == "6"),
             "7" = which(station$cityID == "7"),
             "8" = which(station$cityID == "8"),
             "9" = which(station$cityID == "9"),
             "10" = which(station$cityID == "10"),
             "11" = which(station$cityID == "11"),
             "12" = which(station$cityID == "12"),
             "13" = which(station$cityID == "13"),
             "14" = which(station$cityID == "14"),
             "15" = which(station$cityID == "15"),
             "16" = which(station$cityID == "16"),
             "17" = which(station$cityID == "17"),
             "18" = which(station$cityID == "18"),
             "19" = which(station$cityID == "19"),
             "20" = which(station$cityID == "20"),
             "21" = which(station$cityID == "21"),
             "22" = which(station$cityID == "22"))
    })
    
    ## Interactive Map
    output$mymap <- renderLeaflet({
      leaflet(data = station) %>% addTiles() %>% addProviderTiles(providers$Thunderforest.SpinalMap) %>%
        addMarkers(~station$Longitude[index()], station$Latitude[index()], 
                   popup = paste("ID:", station$ID[index()], "<br>",
                                 "Station:", station$Station[index()], "<br>",
                                 "Altitude:", station$Altitude[index()],"m", "<br>",
                                 "Longitude:", station$Longitude[index()], "<br>",
                                 "Latitude:", station$Latitude[index()]))
    })
    
    ## Histogram
    output$myplot <- renderPlot({
      p <- input$mymap_marker_click
      if(!is.null(p)){
        id.clicked <- station[station$Latitude==p$lat & station$Longitude==p$lng, 1]
        id.match <- which(station$ID==id.clicked)
        comds <- paste0('rf$', names(rf)[id.match+1]) # the 1st column is Time
        evalp <- eval(parse(text=comds))
        hist(evalp, col=rgb(red=0.1, green=0.4, blue=0.9, alpha=0.3)
             , xlab='Rainfall', ylab=' ', main=paste0("Histogram of Rainfall at ", id.clicked))
      }
    })
    
    ## Summary table the clicked station
    output$mytable <- renderTable({
      p <- input$mymap_marker_click
      if(!is.null(p)){
        id.clicked <- station[station$Latitude==p$lat & station$Longitude==p$lng, 1]
        id.match <- which(station$ID==id.clicked)
        comds <- paste0('rf$', names(rf)[id.match+1]) # the 1st column is Time
        evalp <- eval(parse(text = comds))
        ## Summary table
        tb <- matrix(summary(evalp), ncol=6, nrow=1)
        colnames(tb) <- c('Min.','Q1','Median','Mean','Q3','Max.')
        rownames(tb) <- c(' ')
        return(tb)
      }
    })
    
  }
  runApp(shinyApp(ui, server), launch.browser=TRUE)
}




