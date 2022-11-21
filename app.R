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
library(ggthemes)
library(spotoroo)
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
             list(h4("Hello and welcome to our shiny dashboard. This is a welcome message and I will disappear in 30s!"),
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
            actionButton("check", "Check new bushfire"),
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
            actionButton("help2", "Press for instructions"),
            fluidRow(
              box(selectInput(inputId = "Year", "Years:",
                              c("2021-2022","2020-2021","2019-2020","2018-2019",
                                "2017-2018","2016-2017","2015-2016","2014-2015",
                                "2013-2014","2012-2013","2011-2012","2010-2011",
                                "2009-2010","2008-2009"),width = "100%")),
              box(sliderInput(inputId = "Hours", "Days Since Fire Season:",
                              min = 0,
                              max = 150,
                              value= 0,
                              width = "100%",
                              # animate =
                              #   animationOptions(interval = 500, loop = F)
              ))
            ),
            fluidRow(
              box(withLoader(uiOutput("history"),type = "html", loader = "loader1"))
              ,
              #####map output with loader###
              box(withLoader(plotOutput("map2",height =600),type = "html", loader = "loader1"))
              )),
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
      element=c(".sidebar-menu",".sidebar-toggle", "#map1","#check"),
      intro=c(
        "Here is the master control, which you can choose at will.",
        "Click here to put away the master control.",
        "Here are the current fire points monitored by remote sensing data, 
        the colours represent the difference in time and the map is updated every ten minutes.",
        "Click here to check if any new bushfires have appeared since opening this app."
      ),
      position=c("right", "bottom", "bottom", "right")
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
  delay(30000,hide("welcome"))
  
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
      filter(confidence > 49)%>%
      filter(power>10)%>%
      filter(temp_kelvin >300)
    hotspot_show$hours_since_hotspot_class <- cut(hotspot_show$hours_since_hotspot,
                                                  breaks = c(0,2,6,24,48,72),
                                                  labels =c("0-2","2-6","6-24","24-48","48-72"))
    hotspot_show$datetime <- str_replace(hotspot_show$datetime,"T"," ")
    hotspot_show$datetime <- str_replace(hotspot_show$datetime,"Z"," ")
    hotspot_show$datetime <- as_datetime(hotspot_show$datetime) + dhours(10)
    hotspot_show
    
    
  })
  
  recent_hotspot_data_1 <- geojsonsf::geojson_sf("https://hotspots.dea.ga.gov.au/data/recent-hotspots.json")
  vic_hotspot1_1 <- recent_hotspot_data_1 %>%
    st_set_crs("WGS 84")
  vic_map <- vic_map %>%st_set_crs("WGS 84")
  recent_hotspot_data_1 <- recent_hotspot_data_1 %>%st_set_crs("WGS 84")
  vic_hotspot2_1 = st_intersects(vic_map$geometry, recent_hotspot_data_1$geometry)
  vic_hotspot2_1 = vic_hotspot1_1[vic_hotspot2_1[[1]],]
  hotspot_show1 <- vic_hotspot2_1%>%
    filter(confidence > 49)%>%
    filter(power>10)%>%
    filter(temp_kelvin >300)
  hotspot_show1$datetime <- str_replace(hotspot_show1$datetime,"T"," ")
  hotspot_show1$datetime <- str_replace(hotspot_show1$datetime,"Z"," ")
  hotspot_show1$datetime <- as_datetime(hotspot_show1$datetime) + dhours(10)
  
  if(nrow(hotspot_show1)< 200){            
    result_numbership = 0
  }                                        
  else if(nrow(hotspot_show1) > 200){            
    result <- hotspot_cluster(hotspot_show1,
                              lon = "longitude",
                              lat = "latitude",
                              obsTime = 'datetime',
                              activeTime = 24,
                              adjDist = 3000,
                              minPts = 4,
                              minTime = 3,
                              ignitionCenter = "mean",
                              timeUnit = "h",
                              timeStep = 1
    )
    result_numbership <- max(result$hotspots$membership)
  } 
  
  
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
      addCircleMarkers(
        data = t(),
        radius = 4,
        color  = ~pals(hours_since_hotspot_class),
        fillOpacity = 1,
        # create custom labels
        label = paste(
          "Time: ", t()$datetime, "<br>",
          "Hours: ",t()$hours_since_hotspot,  "<br>",
          "Satellite:",t()$satellite_operating_agency,  "<br>",
          "Power:",t()$power,  "<br>",
          "Temp:",t()$temp_kelvin
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
     
     warning_data <- t()%>%
       select(latitude,datetime,longitude)
     # warning_data$datetime <- str_replace(warning_data$datetime,"T"," ")
     # warning_data$datetime <- str_replace(warning_data$datetime,"Z"," ")
     # warning_data$datetime <- as_datetime(warning_data$datetime)
     # warning_data2 <- warning_data %>%
     #   mutate(time = as.numeric(Sys.time() - datetime,units = "mins"))%>%
     #   filter(time <= 20)
     # warning_data2
     
     
     if(nrow(warning_data)< 200){            
       check_warning = 0
     }                                        
     else if(nrow(warning_data) > 200){            
       result2 <- hotspot_cluster(warning_data,
                                  lon = "longitude",
                                  lat = "latitude",
                                  obsTime = 'datetime',
                                  activeTime = 24,
                                  adjDist = 3000,
                                  minPts = 2,
                                  minTime = 3,
                                  ignitionCenter = "mean",
                                  timeUnit = "h",
                                  timeStep = 1
       )
       new_result_numbership <- max(result2$hotspots$membership)
       check_warning <- new_result_numbership - result_numbership
     } 

      check_warning
      
      })
   #toggleModal(session, "startupModal", toggle = "open")
   
  
   observeEvent(input$check,if (warning() > 0) {
     alert("Warning: A new Fire has appeared!")
   }
   else
   {alert("No New Fire!")})
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
  
  
  # hist_data <- read_csv("./hist_data.csv")
  # hist_data <- hist_data %>%
  #   mutate(date = as.Date(datetime))
  # 
  # 
  # timeData <- reactive({
  #   hist_data2 <- hist_data%>%
  #     filter(year == input$Year)
  #   hist_data2
  # })

  steps2 <- reactive(
    data.frame(
      element=c("Year","#Hours", "#history","#map2"),
      intro=c(
        "Here you can select the fire season you want to see, 
        from 1 October to March each year. Data for 2021-2022 is displayed by default.",
        "Drag the sliding axis to see the daily occurrence of fires after the start of the selected fire season.",
        "Dynamic map of fire points for the selected year, with different colours representing different fires.",
        "The black density map represents the extent of fire occurrence for the year 
        and the orange dots represent the fires that occurred on the day of selection."
      ),
      position=c("bottom", "bottom", "bottom", "right")
    )
  )
  
  observeEvent(input$help2,
               introjs(session,
                       options = list(steps=steps2(),
                                      "nextLabel"="Next",
                                      "prevLabel"="Previous",
                                      "skipLabel"="Skip"
                       ),
                       events = list("oncomplete"=I('alert("Done")'))
               )
  )
 
  output$history <- renderUI({
    
    if(input$Year == "2009-2010"){            
      img(height =600, width = 700,src = "2009_2010.gif")
    }                                        
    else if(input$Year == "2010-2011"){            
      img(height =600, width = 700,src = "2010_2011.gif")
    } 
    else if(input$Year == "2011-2012"){            
      img(height =600, width = 700,src = "2011_2012.gif")
    } 
    else if(input$Year == "2012-2013"){            
      img(height =600, width = 700,src = "2012_2013.gif")
    } 
    else if(input$Year == "2013-2014"){            
      img(height =600, width = 700,src = "2013_2014.gif")
    } 
    else if(input$Year == "2014-2015"){            
      img(height =600, width = 700,src = "2014_2015.gif")
    } 
    else if(input$Year == "2015-2016"){            
      img(height =600, width = 700,src = "2015_2016.gif")
    } 
    else if(input$Year == "2016-2017"){            
      img(height =600, width = 700,src = "2016_2017.gif")
    } 
    else if(input$Year == "2017-2018"){            
      img(height =600, width = 700,src = "2017_2018.gif")
    } 
    else if(input$Year == "2018-2019"){            
      img(height =600, width = 700,src = "2018_2019.gif")
    } 
    else if(input$Year == "2019-2020"){            
      img(height =600, width = 700,src = "2019_2020.gif")
    } 
    else if(input$Year == "2020-2021"){            
      img(height =600, width = 700,src = "2020_2021.gif")
    } 
    else if(input$Year == "2021-2022"){            
      img(height =600, width = 700,src = "2021_2022.gif")
    }
    else if(input$Year == "2008-2009"){            
      img(height =600, width = 700,src = "2008_2009.gif")
    }
    
  })
  

  output$map2 <- renderPlot({
    
    if(input$Year == "2021-2022"){            
      map2_data <- read_csv("data/hotspot_2021_2022.csv")%>%
        mutate(start_time = as_datetime("2021-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    }
    
    else if(input$Year == "2009-2010"){            
      map2_data <- read_csv("data/hotspot_2009_2010.csv")%>%
        mutate(start_time = as_datetime("2009-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    }                                        
    else if(input$Year == "2010-2011"){            
      map2_data <- read_csv("data/hotspot_2010_2011.csv")%>%
        mutate(start_time = as_datetime("2010-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    } 
    else if(input$Year == "2011-2012"){            
      map2_data <- read_csv("data/hotspot_2011_2012.csv")%>%
        mutate(start_time = as_datetime("2011-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    } 
    else if(input$Year == "2012-2013"){            
      map2_data <- read_csv("data/hotspot_2012_2013.csv")%>%
        mutate(start_time = as_datetime("2012-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    } 
    else if(input$Year == "2013-2014"){            
      map2_data <- read_csv("data/hotspot_2013_2014.csv")%>%
        mutate(start_time = as_datetime("2013-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    } 
    else if(input$Year == "2014-2015"){            
      map2_data <- read_csv("data/hotspot_2014_2015.csv")%>%
        mutate(start_time = as_datetime("2014-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    } 
    else if(input$Year == "2015-2016"){            
      map2_data <- read_csv("data/hotspot_2015_2016.csv")%>%
        mutate(start_time = as_datetime("2015-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    } 
    else if(input$Year == "2016-2017"){            
      map2_data <- read_csv("data/hotspot_2016_2017.csv")%>%
        mutate(start_time = as_datetime("2016-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    } 
    else if(input$Year == "2017-2018"){            
      map2_data <- read_csv("data/hotspot_2017_2018.csv")%>%
        mutate(start_time = as_datetime("2017-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    } 
    else if(input$Year == "2018-2019"){            
      map2_data <- read_csv("data/hotspot_2018_2019.csv")%>%
        mutate(start_time = as_datetime("2018-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    } 
    else if(input$Year == "2019-2020"){            
      map2_data <- read_csv("data/hotspot_2019_2020.csv")%>%
        mutate(start_time = as_datetime("2019-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    } 
    else if(input$Year == "2020-2021"){            
      map2_data <- read_csv("data/hotspot_2020_2021.csv")%>%
        mutate(start_time = as_datetime("2020-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    } 

    else if(input$Year == "2008-2009"){            
      map2_data <- read_csv("data/hotspot_2008_2009.csv")%>%
        mutate(start_time = as_datetime("2008-10-01 00:00:00"))%>%
        mutate(since_start_time = as.numeric(obsTime - start_time,units = "days"))
    }
    
    map2_data$since_start_time<-round(map2_data$since_start_time,0)
    
    plot <- ggplot() + 
      geom_density_2d_filled(data=map2_data, aes(x=lon, y=lat)) +
      geom_sf(data = vic_map, colour="white", fill=NA) +
      xlim(c(140, 150.5)) + ylim(c(-39.5, -33.5)) +
      scale_fill_grey() +
      theme_map() +
      theme(legend.position="none")+
      geom_point(map2_data %>% dplyr::filter(since_start_time == input$Hours), 
                 mapping=aes(x=lon, y=lat), 
                 colour="orange") 
    
    plot
    
    
    # map2_data2 <- map2_data %>%
    #   mutate(month_day = str_sub(date,6,10))%>%
    #   filter(month_day == input$Year)
    
    # map_2 <- basemap %>%
    #   addCircles(
    #     data = map2_data2,
    #     color = "red",
    #     # create custom labels
    #     label = paste(
    #       "Time: ", map2_data2$datetime, "<br>",
    #       "Latitude: ",map2_data2$lat,  "<br>",
    #       "Longitude:",map2_data2$lon,"<br>",
    #       "Power: ", map2_data2$power
    #     )
    #   )
    # map_2

  })
  
  output$Table2 <- renderDataTable({
    datatable_data <- read_csv("data/vic_hist_hotspot.csv")%>%
      mutate(date = as.Date(datetime))%>%
      mutate(obsTime = as_datetime(datetime))%>%
      mutate(obsTime = as.POSIXct(obsTime))%>%
      extract(geometry, c('lon', 'lat'), '\\((.*), (.*)\\)', convert = TRUE)

    datatable(datatable_data,
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
