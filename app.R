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
              box(withLoader(uiOutput("history", height =600,width = "100%"),type = "html", loader = "loader1")),
              #####map output with loader###
              box(withLoader(leafletOutput("map2", height =600,width = "100%"),type = "html", loader = "loader1"))),
            fluidRow(
              box(selectInput("Year", "Years:",
                              c("2008-2022","2008-2009","2009-2010",
                                "2010-2011","2011-2012","2012-2013","2013-2014",
                                "2014-2015","2015-2016","2016-2017","2017-2018",
                                "2018-2019","2019-2020","2020-2021","2021-2022"))),
            box(sliderInput("DatesMerge", "Dates:",
                        min = as.Date("10-01","%m-%d"),
                        max = as.Date("02-28","%m-%d"),
                        value=as.Date("10-01","%m-%d"),
                        width = "100%",
                        animate =
                          animationOptions(interval = 500, loop = F)
                        )))),
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
      filter(confidence > 50)%>%
      filter(power>100)
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

  
 
  output$history <- renderUI({
    
    # plot<- ggplot()+
    #   geom_sf(data = vic_map)+
    #   geom_point(data = timeData(), aes(longitude, latitude),color = "red")+
    #   theme_bw() +
    #   transition_states(date)+
    #   enter_recolor(fill = "#f0f5f9") +
    #   labs(subtitle = "Date:{previous_state}")
    # plot
    if(input$Year == "2008-2022"){            
      img(height =600, width = 700,src = "all.gif")
    }                                        
    else if(input$Year == "2009-2010"){            
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
  

  output$map2 <- renderLeaflet({
    
    if(input$Year == "2008-2022"){            
      map2_data <- read_csv("data/vic_hist_hotspot.csv")%>%
        mutate(date = as.Date(datetime))%>%
        mutate(obsTime = as_datetime(datetime))%>%
        mutate(obsTime = as.POSIXct(obsTime))%>%
        extract(geometry, c('lon', 'lat'), '\\((.*), (.*)\\)', convert = TRUE)
    }                                        
    else if(input$Year == "2009-2010"){            
      map2_data <- read_csv("data/hotspot_2009_2010.csv")
    }                                        
    else if(input$Year == "2010-2011"){            
      map2_data <- read_csv("data/hotspot_2010_2011.csv")
    } 
    else if(input$Year == "2011-2012"){            
      map2_data <- read_csv("data/hotspot_2011_2012.csv")
    } 
    else if(input$Year == "2012-2013"){            
      map2_data <- read_csv("data/hotspot_2012_2013.csv")
    } 
    else if(input$Year == "2013-2014"){            
      map2_data <- read_csv("data/hotspot_2013_2014.csv")
    } 
    else if(input$Year == "2014-2015"){            
      map2_data <- read_csv("data/hotspot_2014_2015.csv")
    } 
    else if(input$Year == "2015-2016"){            
      map2_data <- read_csv("data/hotspot_2015_2016.csv")
    } 
    else if(input$Year == "2016-2017"){            
      map2_data <- read_csv("data/hotspot_2016_2017.csv")
    } 
    else if(input$Year == "2017-2018"){            
      map2_data <- read_csv("data/hotspot_2017_2018.csv")
    } 
    else if(input$Year == "2018-2019"){            
      map2_data <- read_csv("data/hotspot_2018_2019.csv")
    } 
    else if(input$Year == "2019-2020"){            
      map2_data <- read_csv("data/hotspot_2019_2020.csv")
    } 
    else if(input$Year == "2020-2021"){            
      map2_data <- read_csv("data/hotspot_2020_2021.csv")
    } 
    else if(input$Year == "2021-2022"){            
      map2_data <- read_csv("data/hotspot_2021_2022.csv")
    }
    else if(input$Year == "2008-2009"){            
      map2_data <- read_csv("data/hotspot_2008_2009.csv")
    }
    
    map2_data2 <- map2_data %>%
      mutate(month_day = str_sub(date,6,10))%>%
      filter(month_day == input$Year)
    
    map_2 <- basemap %>%
      addCircles(
        data = map2_data2,
        color = "red",
        # create custom labels
        label = paste(
          "Time: ", map2_data2$datetime, "<br>",
          "Latitude: ",map2_data2$lat,  "<br>",
          "Longitude:",map2_data2$lon,"<br>",
          "Power: ", map2_data2$power
        )
      )
    map_2

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
