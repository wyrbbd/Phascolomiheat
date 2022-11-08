library(tidyverse)
library(sf)
library(DT)
library(furrr)
library(lubridate)
library(rnaturalearth)
library(dplyr)
library(geojsonsf)
library(shinydashboard)
library(gganimate)
library(dashboardthemes)
library(shiny)
library(shinycustomloader)
library(shinyjs)
library(shinyBS)
library(rintrojs)
# Define UI for application that draws a histogram
library(leaflet)
library(formattable)
library(cranlogs)
ui <- dashboardPage(
  dashboardHeader(title = "Real-Time Hotspot"),
  dashboardSidebar(
    introjsUI(),
    sidebarMenu(
    menuItem("Real-Time Map",
             menuSubItem("Map", tabName = "Map1",icon = icon("list")),
             menuSubItem("Data", tabName = "table1")),
    menuItem("Historical Hotspots",
             menuSubItem("Map", tabName = "Map2",icon = icon("list")),
             menuSubItem("Data", tabName = "table2"))
  )),
  dashboardBody(
    useShinyjs(),
    tags$div(id = "welcome",
             style = 'color:pink;',
             list(h4("Hello and welcome to our shiny dashboard. This is a welcome message and I will disappear in 60s!"),
                  hr())),
    #bsModal(id = 'startupModal', title = 'Dum Dum', trigger = '',
           # size = 'large', p("here is my mumbo jumbo")),
   # Shiny.setInputValue(Warning, value),
    
    tabItems(
    tabItem(tabName = "Map1",
            #####dashboard theme######
            shinyDashboardThemes(theme = "blue_gradient"),
            ####Title####
            h2("Hotspot Map: Real Time ", 
               style = 	"color:#4682B4"), 
            actionButton("check", "Check"),
            actionButton("help", "Press for instructions"),
            fluidRow(
                #####map output with loader###
                box(withLoader(leafletOutput("map1", height =800,width = "200%"),type = "html", loader = "loader1")))
    ),
    tabItem(tabName = "table1",
            shinyDashboardThemes(theme = "blue_gradient"),
            h2("Data: Real Time Hotspot  ", 
               style = 	"color:#4682B4"), 
            
            fluidRow(dataTableOutput(outputId = "Table1"))
    ),
    tabItem(tabName = "Map2",
            #####dashboard theme######
            shinyDashboardThemes(theme = "blue_gradient"),
            ####Title####
            h2("Historical Hotspots", 
               style = 	"color:#4682B4"), 
            fluidRow(
              box(selectInput("Year", "Years:",
                              c("2002-2003","2003-2004","2004-2005","2005-2006",
                                "2006-2007","2007-2008","2008-2009","2009-2010",
                                "2010-2011","2011-2012","2012-2013","2013-2014",
                                "2014-2015","2015-2016","2016-2017","2017-2018",
                                "2018-2019","2019-2020","2020-2021","2021-2022")))),
            fluidRow(
              box(withLoader(plotOutput("history", height =600,width = "100%"),type = "html", loader = "loader1")),
              #####map output with loader###
              box(withLoader(leafletOutput("map2", height =600,width = "100%"),type = "html", loader = "loader1"))),
            fluidRow(sliderInput("DatesMerge", "Dates:",
                        min = as.Date("2020-10-01","%Y-%m-%d"),
                        max = as.Date("2021-02-28","%Y-%m-%d"),
                        value=as.Date("2020-10-01","%Y-%m-%d"),
                        width = "100%",
                        animate =
                          animationOptions(interval = 500, loop = F)
                        ))),
    tabItem(tabName = "table2",
            shinyDashboardThemes(theme = "blue_gradient"),
            h2("Data: Real Time Hotspot", 
               style = 	"color:#4682B4"), 
            fluidRow(dataTableOutput(outputId = "Table2")))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  steps <- reactive(
    data.frame(
      element=c(".sidebar-menu", ".main-header", ".sidebar-toggle", ".active", "#map1"),
      intro=c(
        "This is a sidebar. Note that we access it with '.' instead of '#', because we track its class and not its id.",
        "This is a header.",
        "This is a button that allows to close and open the sidebar.",
        "This is the active element of the sidebar.",
        "Here is the map that will be updated automatically every 10 minutes."
      ),
      position=c("right", "bottom", "bottom", "right", "top")
    )
  )
  
  observeEvent(input$help,
               introjs(session,
                       options = list(steps=steps(),
                                      "nextLabel"="Next",
                                      "prevLabel"="Previous",
                                      "skipLabel"="Skip"
                       ),
                       events = list("oncomplete"=I('alert("Done")'))
               )
  )
  
  # welcome message
  delay(60000,hide("welcome"))
  
  au_map <-ne_states(country = c("australia"), returnclass ="sf")%>%
    select(state = name, geometry)
  vic_map <- au_map%>%
    filter(state == "Victoria")
  
  data_now <-reactive({
    invalidateLater(600000)  #1000毫秒之后重新执行
    recent_hotspot_data <- geojsonsf::geojson_sf("https://hotspots.dea.ga.gov.au/data/recent-hotspots.json")
    vic_hotspot1 <- recent_hotspot_data %>%
      st_set_crs("WGS 84")
    vic_map <- vic_map %>%st_set_crs("WGS 84")
    recent_hotspot_data <- recent_hotspot_data %>%st_set_crs("WGS 84")
    vic_hotspot2 = st_intersects(vic_map$geometry, recent_hotspot_data$geometry)
    vic_hotspot2 = vic_hotspot1[vic_hotspot2[[1]],]
    vic_hotspot2
  })
  
  t <- reactive({
    
    hotspot_show <- data_now()%>%
      filter(confidence > 50)
    hotspot_show$hours_since_hotspot_class <- cut(hotspot_show$hours_since_hotspot,
                                                  breaks = c(0,2,6,24,48,72),
                                                  labels =c("0-2","2-6","6-24","24-48","48-72"))
    hotspot_show$datetime <- str_replace(hotspot_show$datetime,"T"," ")
    hotspot_show$datetime <- str_replace(hotspot_show$datetime,"Z"," ")
    hotspot_show$datetime <- as_datetime(hotspot_show$datetime) + dhours(10)
    hotspot_show
    
    
  })
  
  pals = colorFactor(palette =c("#800000","#FF0000","#FF4500","#FF8C00","#F5DEB3"),
                     levels = c("0-2","2-6","6-24","24-48","48-72"))
  
  basemap <- leaflet() %>%
    setView(145,-30,zoom = 5)%>%
    # add different provider tiles
    addProviderTiles(
      "OpenStreetMap",
      # give the layer a name
      group = "OpenStreetMap"
    ) %>%
    addProviderTiles(
      "Stamen.Toner",
      group = "Stamen.Toner"
    ) %>%
    addProviderTiles(
      "Stamen.Terrain",
      group = "Stamen.Terrain"
    ) %>%
    addProviderTiles(
      "Esri.WorldStreetMap",
      group = "Esri.WorldStreetMap"
    ) %>%
    addProviderTiles(
      "Wikimedia",
      group = "Wikimedia"
    ) %>%
    addProviderTiles(
      "CartoDB.Positron",
      group = "CartoDB.Positron"
    ) %>%
    addProviderTiles(
      "Esri.WorldImagery",
      group = "Esri.WorldImagery"
    ) %>%
    # add a layers control
    addLayersControl(
      baseGroups = c(
        "Esri.WorldImagery", "Stamen.Toner",
        "Stamen.Terrain", "Esri.WorldStreetMap",
        "Wikimedia", "CartoDB.Positron","OpenStreetMap"
      ),
      # position it on the topleft
      position = "topleft"
    )
  output$map1 <- renderLeaflet({
    map_1 <- basemap %>%
      addCircles(
        data = t(),
        color = ~pals(hours_since_hotspot_class),
        # create custom labels
        label = paste(
          "Time: ", t()$datetime, "<br>",
          "Hours: ",t()$hours_since_hotspot,  "<br>",
          "Satellite:",t()$satellite_operating_agency
        ) %>%
          lapply(htmltools::HTML)
      ) %>%
      # add a legend
      addLegend(
        colors = c("#800000","#FF0000","#FF4500","#FF8C00","#F5DEB3"),
        labels = c("0 - 2 hours","2 - 6 hours","6 - 24 hours","24 - 48 hours","48 - 72 hours"),
        title = "Hours",
        opacity = 1, 
        position = "bottomleft")
    map_1
    
  })
  
  ## warning message
  
   warning <- reactive({ 
      warning_data <- data_now()%>%
        select(latitude,temp_kelvin,datetime,longitude,
               satellite,confidence,australian_state,geometry)
      warning_data$datetime <- str_replace(warning_data$datetime,"T"," ")
      warning_data$datetime <- str_replace(warning_data$datetime,"Z"," ")
      warning_data$datetime <- as_datetime(warning_data$datetime)
      warning_data2 <- warning_data %>%
        mutate(time = as.numeric(Sys.time() - datetime,units = "mins"))%>%
        filter(time <= 20)
      warning_data2
      
      
      })
   #toggleModal(session, "startupModal", toggle = "open")
   
  
   observeEvent(input$check,if (is.null(warning())) {
     alert("Warning: A new Hotspot has appeared!")
   }
   else
   {alert("No New Hotspot!")})
  # observeEvent(output$map1,alert("Warning: A new Hotspot has appeared!"))
  
  output$Table1 <- renderDataTable({ 
    
    datatable(data_now(),
              rownames = FALSE,
              filter = 'top',
              caption = "Table 1: Recent VIC Hotspots",
              extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy','csv','excel','pdf')))
    
  })
  
  
  hist_data <- read_csv("./hist_data.csv")
  hist_data <- hist_data %>%
    mutate(date = as.Date(datetime))
  
  
  timeData <- reactive({
    hist_data2 <- hist_data%>%
      filter(year == input$Year)
    hist_data2
  })
  

 
  output$history <- renderPlot({
    
    plot<- ggplot()+
      geom_sf(data = vic_map)+
      geom_point(data = timeData(), aes(longitude, latitude),color = "red")+
      theme_bw() +
      transition_states(date)+
      enter_recolor(fill = "#f0f5f9") +
      labs(subtitle = "Date:{previous_state}")
    plot
    
  })
  

  output$map2 <- renderLeaflet({
    map_2 <- basemap %>%
      addCircles(
        data = timeData(),
        color = "red",
        # create custom labels
        label = paste(
          "Time: ", timeData()$datetime, "<br>",
          "Latitude: ",timeData()$latitude,  "<br>",
          "Longitude:",timeData()$longitude ,"<br>",
          "Power: ", timeData()$power
        )
      )
    map_2
    
  })
  
  output$Table2 <- renderDataTable({ 
    
    datatable(test,
              rownames = FALSE,
              filter = 'top',
              caption = "Table 2: Historical Hotspots",
              extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy','csv','excel','pdf')))
    
  })

}
# Run the application 
shinyApp(ui = ui, server = server)
