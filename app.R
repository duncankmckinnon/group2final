### Imports ----
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(waiter)
library(shinyBS)
library(memoise)
library(RColorBrewer)
library(lubridate)
library(tidyverse)
library(shinyjs)
library(plotly)
library(DT) # for pagination


# Initial File Loading and Setup - ALL ----
### Load the data and construct the map before rendering the page. This makes the map and data accessible within the server. MJS
first <- ifelse(exists('Abandoned_Property_Parcels') && exists('City_Council_Districts') , F, T)
#if (first) {

# load abandoned properties data MJS
Abandoned_Property_Parcels <- readOGR(dsn="Abandoned_Property_Parcels", layer = "Abandoned_Property_Parcels", stringsAsFactors = FALSE)  
City_Council_Districts <- readOGR(dsn="City_Council_Districts", layer="City_Council_Districts", stringsAsFactors = FALSE)  

# load council districts DM
City_Council_Districts@data$Num <- as.factor(City_Council_Districts@data$Num)
districts <- levels(factor(City_Council_Districts@data$Num))
outcomes <- levels(fct_explicit_na(Abandoned_Property_Parcels@data$Outcome_St))
dates <- parse_date_time(as.character(Abandoned_Property_Parcels@data$Date_of_Ou), 'ymdHMS')

#Load schools data AS
School_Boundaries <- readOGR(dsn="School_Boundaries", 
                             layer = "School_Boundaries", 
                             stringsAsFactors = FALSE) 

#Create "school rating" column, populated with random numbers AS
School_Boundaries@data$SchoolRating <- sample(10, size = nrow(School_Boundaries@data), replace = TRUE)

# Load Business Licenses data - EM
Business_Licenses_geocoded <- read_csv("Business_Licenses_geocoded.csv")

# Convert the date field in the abandoned properties data to be a date
Abandoned_Property_Parcels@data$Date_of_Ou <- ymd_hms(Abandoned_Property_Parcels@data$Date_of_Ou)
Abandoned_Property_Parcels@data$clean_date <- date(Abandoned_Property_Parcels@data$Date_of_Ou)


# Limit the business data to only include those that mapped to the SB area - EM
Business_Licenses_geocoded <- filter(Business_Licenses_geocoded,
                                     X >= -87, X <= -86,
                                     Y >= 41, Y <= 42)

# Pull out the council district more clearly in the business data - EM
Business_Licenses_geocoded <- mutate(Business_Licenses_geocoded, council_district = str_sub(Council_Di, -1))

# Factorize license fields - EM
Business_Licenses_geocoded$council_district <- fct_explicit_na(as.factor(Business_Licenses_geocoded$council_district))
Abandoned_Property_Parcels@data$council_district <- fct_explicit_na(as.factor(Abandoned_Property_Parcels@data$Council_Di))

# Convert the business licenses into a spatial data frame - EM
biz_spatial <- SpatialPointsDataFrame(
  coords = Business_Licenses_geocoded[, c('X', 'Y')],
  data = Business_Licenses_geocoded,
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Combine the abandoned property data and the business data into one data frame for plotting
# purposes -EM
propPlotData <- Abandoned_Property_Parcels@data %>%
  select(Outcome_St, date_of_interest = clean_date, council_district) %>%
  mutate(Class = "Abandoned Property")
busPlotData <- Business_Licenses_geocoded %>%
  select(Business_N, date_of_interest = Issue_Date, council_district) %>%
  mutate(Class = "Business")

# Min and max dates for the abandoned property data that will be used for both sliders
min_slider_date_bus <- min(Abandoned_Property_Parcels@data$clean_date, na.rm = TRUE)
max_slider_date_bus <- max(Abandoned_Property_Parcels@data$clean_date, na.rm = TRUE)
min_slider_date_prop <- min(Abandoned_Property_Parcels@data$clean_date, na.rm = TRUE)
max_slider_date_prop <- max(Abandoned_Property_Parcels@data$clean_date, na.rm = TRUE)

# change dates to datetime object DM
Abandoned_Property_Parcels@data$Date_of_Ou <- dates

# get factor colors DM
district_colors <- brewer.pal(length(districts), name = 'Set1')
outcome_colors <- brewer.pal(length(outcomes), name = 'Spectral')

# create scale fills DM
district_scale <- scale_fill_manual(name = 'Num', values = district_colors)
abandoned_scale <- scale_fill_manual(name = 'Outcome', values = outcome_colors)

# set color names DM
names(district_colors) <- districts
names(outcome_colors) <- outcomes

# Styles - only used to highlight what the code is doing MJS ----
propertiesStyle <- tags$style(HTML("
                                   h1 {
                                   color: darkblue;
                                   font-family: 'Impact'
                                   }
                                   "))

schoolsStyle <- tags$style(HTML("
                                h1 {
                                color: darkblue;
                                font-family: 'Impact'
                                }
                                "))


# GUI function MJS ----
ui <- dashboardPage(
  dashboardHeader(title = 'SouthBend'),
  dashboardSidebar(
    actionGroupButtons(
      inputIds = c("Home", "Outcomes", "Schools", "Businesses"),
      labels = list(tags$span(class= "pull-left", icon("home"), "Home Page"),
                    tags$span(class= "pull-left", icon("university"), "Outcomes"), 
                    tags$span(class= "pull-left", icon("building"), "Schools"), 
                    tags$span(class= "pull-left", icon("industry"), "Businesses")),
      direction = "vertical",
      status = "primary text-left"
    )
  ),
  dashboardBody(
    useShinyjs(),
    use_waiter(include_js = FALSE), # do not include js
    waiter::use_butler(),
    show_waiter_on_load(html = spin_fading_circles()),
    fluidPage(
      tags$style(HTML("
                      #mapPanel {
                      height: 500px;
                      }
                      #widgetPanel {
                      height: 500px;
                      }
                      #dataPanel {
                      margin-top: 10px; 
                      min-height: 200px;
                      max-height: 400px
                      }
                      #plot-container {
                      position: relative;
                      }
                      #loading-spinner {
                      position: absolute;
                      left: 50%;
                      top: 50%;
                      z-index: -1;
                      margin-top: -33px;  /* half of the spinner's height */
                      margin-left: -33px; /* half of the spinner's width */
                      }
                      #plot.recalculating {
                      z-index: -2;
                      }
                      .fa { margin-right: 10px; }
                      #home_pic {
                        width: 100%;
                        object-fit: fill;
                      }
                      #welcome_text {
                        display: inline-block;
                        line-height: 1.5em;
                        font-size: 18px;
                        color: darkblue;
                      }
                      ")),
      fluidRow(
        column(id='pageTitle', width= 12,
               uiOutput("pageTitle")),
        column(id='mapPanel', width = 9,
               leafletOutput("map", height="480px")
        ),
        column(id='widgetPanel', width = 3, style='padding:30px;',
                # uiOutput('widgets_ui'),
                # district widgets DM
                selectInput(
                 "district",
                 "Select District to display: ",
                 choices = c(1:6, 'All'),
                 selected = 'All'
               ),
               sliderInput(
                 inputId = "dates",
                 label = "Abandoned Property Dates: ",
                 min = as_date(min(dates, na.rm = T)),
                 max = as_date(max(dates, na.rm = T)),
                 value = c(as_date(min(dates, na.rm = T)), as_date("2015-09-22"))
               ),
               # end district widgets
               # school widgets - AH
               sliderInput(inputId = "schoolRating",
                           label = "School Rating",
                           min = 1,
                           max = 10,
                           value = c(1,10)),
              # end schoool widgets
              # businesses widgets
              sliderInput(inputId = "businessDate",
                          label = "Business License Issue Date",
                          min = min_slider_date_bus,
                          max = max_slider_date_bus,
                          value = c(min_slider_date_bus, max_slider_date_bus))
        )
      ),
      fluidRow(
        column(id='dataPanel', width=9, 
               uiOutput('data_ui')
        ),
        column(id='textPanel', width=3, 
               uiOutput('text_ui')
        )
        
      )
      )
    )
)


district_filter <- function(data, x, inp){
  return(filter(ifelse(inp == 'All', 
                             x %in% c('1','2','3','4','5','6'),
                             x == inp)))
}

# shiny server code ----
server <- shinyServer(function(input, output, session) {
  #dates_ <- reactive(input$dates)
  ## Convenience Functions - MJS ----
  
  # toggle widgets to match selected page
  toggleWidgets <- function(selectedPage) {
    if(selectedPage == "Outcomes") {
      shinyjs::enable('district')
      shinyjs::enable('dates')
      shinyjs::disable('schoolRating')
      shinyjs::disable('businessDate')
      }
    if(selectedPage == "Schools") {
      shinyjs::disable('district')
      shinyjs::disable('dates')
      shinyjs::enable('schoolRating')
      shinyjs::disable('businessDate')
    }
    if(selectedPage == "Businesses") {
      shinyjs::disable('district')
      shinyjs::enable('dates')
      shinyjs::disable('schoolRating')
      shinyjs::enable('businessDate')
    }
    if(selectedPage == "HomePage") {
      shinyjs::disable('district')
      shinyjs::disable('dates')
      shinyjs::disable('schoolRating')
      shinyjs::disable('businessDate')
    }
  }
  
  ### Initialization Code
  # disable widgets
  toggleWidgets('HomePage')
  
  # Set up initial page header
  output$pageTitle <- renderUI(list( propertiesStyle ,
                                     h1("'1000 Homes in a 1000 Days' - Anatomy of a City's Initiative to clean up Abandoned Properties Cleanup Initiative")))
  
  # Set up initial text panel
  output$text_ui <- renderUI(
    fluidPage(
    fluidRow(
      column(12, 
             includeHTML("widgets_guide.html"))
    )
    )
  )
  
  # Set up intial data panel
  output$data_ui <- renderUI(
    fluidRow(
      column(12, 
             includeHTML("welcome.html"))
    )
  )

  ## Intial Feather Map Rendering - All ----
  output$map <- renderLeaflet({
    leaflet()  %>%
      addProviderTiles(providers$OpenStreetMap)  %>%
      addPolygons(
        data = City_Council_Districts,
        group = 'District Boundaries',
        fillColor = ~colorFactor(district_colors, Num)(Num),
        highlightOptions = highlightOptions(
          color = "black",
          weight = 2,
          bringToFront = FALSE
        ),
        popup = paste(
          '<b>Council: </b>',
          City_Council_Districts$Council_Me,
          '<br><b>Email: </b>',
          City_Council_Districts$Email,
          '<br><b>District: </b>',
          City_Council_Districts$Dist,
          sep = ''
        )
      ) %>%
      addPolygons(
        data = Abandoned_Property_Parcels, 
        group = 'Abandoned Properties',
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 1,
        fillColor = ~ colorFactor(outcome_colors, fct_explicit_na(Outcome_St))(fct_explicit_na(Outcome_St)),
        highlightOptions = highlightOptions(
          color = "black",
          weight = 0.2,
          bringToFront = TRUE
        ),
        popup = paste(
          '<b>Outcome: </b>',
          Abandoned_Property_Parcels$Outcome_St,
          '<br><b>District: </b>',
          Abandoned_Property_Parcels$Council_Di,
          '<br><b>Street: </b>',
          Abandoned_Property_Parcels$Street_Nam,
          '<br><b>Zip: </b>',
          Abandoned_Property_Parcels$Zip_Code,
          '<br><b>Outcome Date: </b>',
          Abandoned_Property_Parcels$Date_of_Ou,
          sep = ''
        )
      ) %>%
      addLayersControl(
        baseGroups = c("District Boundaries"),
        overlayGroups = c("Abandoned Properties", "Schools", "Businesses"),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      hideGroup('Abandoned Properties') %>%
     # hideGroup('Schools') %>%
      htmlwidgets::onRender(
        # remove loading message
        hide_waiter()
      )
  })
  
  ## This draws the homepage data panel- MJS ----
  observeEvent(input$Home, { 
    toggleWidgets('HomePage')
    output$pageTitle <- renderUI(list( propertiesStyle ,
                                       h1("'1000 Homes in a 1000 Days' - Anatomy of a City's Abandoned Properties Cleanup Initiative")))
    
    output$data_ui <- renderUI(
      fluidRow(
        column(12, 
             includeHTML("welcome.html"))
      )
    )
  })
  
  ## This draws the Outcomes widgets and data panels - DM ----
  ### Reactive function in response to district selection - DM
  observeEvent(input$Outcomes, { 
    toggleWidgets('Outcomes')
    
    ReactOut <- properties_reactive()
    # print(ReactOut)
    filterPropertiesMapData <- ReactOut$properties_$mapOutput
    filterDistrictMapData <- ReactOut$districts_$mapOutput
    propertiesTable <- ReactOut$properties_$table
    districtsTable <- ReactOut$districts_$table
    
    leafletProxy('map', session) %>%
      clearGroup('District Boundaries') %>%
      clearGroup('Abandoned Properties') %>%
      hideGroup('Schools') %>%
      addPolygons(
        data = filterDistrictMapData,
        group = 'District Boundaries',
        fillColor = ~colorFactor(district_colors, Num)(Num),
        highlightOptions = highlightOptions(
          color = "black",
          weight = 2,
          bringToFront = FALSE
        ),
        popup = paste(
          '<br>Council: ',
          districtsTable$Council_Me,
          '</br>Email: ',
          districtsTable$Email,
          '<br>District: ',
          districtsTable$Dist,
          '</br>',
          sep = ''
        )
      ) %>%
      addPolygons(
        data = filterPropertiesMapData, 
        group = 'Abandoned Properties',
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 1,
        fillColor = ~ colorFactor(outcome_colors, fct_explicit_na(Outcome_St))(fct_explicit_na(Outcome_St)),
        highlightOptions = highlightOptions(
          color = "black",
          weight = 0.2,
          bringToFront = TRUE
        ),
        popup = paste(
          '<br>Outcome: ',
          propertiesTable$Outcome_St,
          '</br>District: ',
          propertiesTable$Council_Di,
          '<br>Street: ',
          propertiesTable$Street_Nam,
          '</br>Zip: ',
          propertiesTable$Zip_Code,
          '<br>Outcome Date: ',
          propertiesTable$Date_of_Ou,
          sep = ''
        )
      ) %>%
      showGroup("Abandoned Properties") %>%
      showGroup("District Boundaries") %>%
      hideGroup('Street Lights')
    output$pageTitle <- renderUI(list( propertiesStyle ,
                                       h1("Abandoned Properties by City Council District")))
    output$data_ui <- renderUI({
      fluidPage(
        fluidRow(
          renderPlotly({
            propertiesTable <- properties_reactive()$properties_$table
            busPlot <- propertiesTable %>%
              mutate(District_Chr = as.numeric(Council_Di) + 1300,
                     District = factor(ifelse(is.na(District_Chr), 'NA', District_Chr)),
                     Date = Date_of_Ou,
                     Outcome = fct_explicit_na(Outcome_St)) %>%
              ggplot() +
              geom_histogram(aes(x = Outcome, fill = Outcome), stat = 'count', binwidth = 1) +
              abandoned_scale + 
              labs(x = 'Outcome', y = 'Count', title = 'Abandoned Properties') +
              theme_bw() + 
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
            ggplotly(busPlot)
          })
        )
      )
    })
  })
  
  district_ <- reactive(input$district)
  dates_ <- reactive(input$dates)
  
  properties_reactive <- reactive({
    district <- district_()
    dates <- dates_()
    properties <- Abandoned_Property_Parcels@data %>%
      filter(is.na(clean_date) || 
               (clean_date >= dates[1] && 
                  clean_date <= dates[2]))
    mapProperties <- Abandoned_Property_Parcels[(((na.omit(Abandoned_Property_Parcels@data$clean_date) >= dates[1]) && 
                                                    (na.omit(Abandoned_Property_Parcels@data$clean_date) <= dates[2])) ||
                                                   (is.na(Abandoned_Property_Parcels@data$clean_date))),]
    districts <- City_Council_Districts@data
    mapDistricts <- City_Council_Districts
    
    if(district == 'All'){
      district = c('1','2','3','4','5','6')
    }
    properties <- properties %>% filter(Council_Di %in% district)
    mapProperties <- mapProperties[((mapProperties@data$Council_Di %in% district)),]
    districts <- districts %>% filter(Num %in% district)
    mapDistricts <- mapDistricts[(mapDistricts@data$Num %in% district),]
    
    
    ### redraw map in reactive function as this doesn't seem to work with a leaflet function ----
    leafletProxy('map', session) %>%
     # hideGroup('Schools') %>%
     # clearGroup('Schools') %>%
      clearGroup('Businesses') %>%
      clearGroup('Abandoned Properties') %>%
      clearGroup('District Boundaries') %>%
      addPolygons(
        data = mapDistricts,
        group = 'District Boundaries',
        fillColor = ~colorFactor(district_colors, Num)(Num),
        highlightOptions = highlightOptions(
          color = "black",
          weight = 2,
          bringToFront = FALSE
        ),
        popup = paste(
          '<br><b>Council: </b>',
          districts$Council_Me,
          '</br><b>Email: </b>',
          districts$Email,
          '<br><b>District: </b>',
          districts$Dist,
          sep = ''
        )
      ) %>%
      addPolygons(
        data = mapProperties, 
        group = 'Abandoned Properties',
        weight = 1,
        fillColor = ~ colorFactor(outcome_colors, fct_explicit_na(Outcome_St))(fct_explicit_na(Outcome_St)),
        highlightOptions = highlightOptions(
          color = "black",
          weight = 0.2,
          bringToFront = TRUE
        ),
        popup = paste(
          '<b>Outcome: </b>',
          properties$Outcome_St,
          '<br><b>District: </b>',
          properties$Council_Di,
          '<br><b>Street: </b>',
          properties$Street_Nam,
          '<br><b>Zip: </b>',
          properties$Zip_Code,
          '<br><b>Outcome Date: </b>',
          properties$Date_of_Ou,
          sep = ''
        )
      ) %>%
      hideGroup('Street Lights') %>%
      showGroup('Abandoned Properties') %>%
      showGroup('District Boundaries') 
    
    
    return(list(districts_ = list(table = districts, mapOutput = mapDistricts),
                properties_ = list(table = properties, mapOutput = mapProperties)))
  })
  
  # This draws the Schools widgets and data panels - AS ----
  observeEvent(input$Schools, {  
    toggleWidgets("Schools")
    
    # Get filtered data from the reactive function
    ReactOut <- schoolReactive()
    filterSchoolsMapData = ReactOut$mapOutput
    
    # update shared map
    leafletProxy('map', session) %>%  
      clearGroup('Schools') %>%
      addPolygons(data = filterSchoolsMapData, weight = 2, color = "blue", group = "Schools",
                  popup = paste(
                    '<br><b>School: </b>',
                    School_Boundaries$School,
                    '<br><b>Type: </b>',
                    School_Boundaries$SchoolType,
                    '<br><b>Rating: </b>',
                    School_Boundaries$SchoolRating,
                    sep = ''
                  )) %>%
      hideGroup("Abandoned Properties") %>% hideGroup('Street Lights') %>% showGroup('Schools')
    
    
    output$pageTitle <- renderUI(list(schoolsStyle, h1("Abandoned Properties by Schools District")))
  
    output$data_ui <- renderUI({
      fluidPage(
        fluidRow(
          DT::renderDT({
            tableDataFiltered <- schoolReactive()$table
            tableDataFiltered %>%
              select(School, SchoolType, SchoolRating)
          }, server = F, options = list(pageLength = 8, lengthMenu = c(8)))
        )
      )
    })
  })
  
  # Reactive function in response to rating selection - AS
  schoolReactive <- reactive({
    output <- filter(School_Boundaries@data, 
                     SchoolRating >= input$schoolRating[1],
                     SchoolRating <= input$schoolRating[2])
    
    mapDataOut <- School_Boundaries[((School_Boundaries@data$SchoolRating >= input$schoolRating[1] & 
                                        School_Boundaries@data$SchoolRating <= input$schoolRating[2])),] 
    # redraw map in reactive function as this doesn't seem to work with a leaflet function
    leafletProxy('map', session) %>%
      clearGroup('Schools') %>%
      showGroup('Schools') %>%
      addPolygons(data = mapDataOut, weight = 2, color = "blue", group = "Schools",
                  popup = paste(
                    '<br><b>School: </b>',
                    School_Boundaries$School,
                    '<br><b>Type: </b>',
                    School_Boundaries$SchoolType,
                    '<br><b>Rating: </b>',
                    School_Boundaries$SchoolRating,
                    sep = ''
                  ))
    list(table = output, mapOutput = mapDataOut)
  })
  
  # This draws the Businesses widgets and data panels - EM ----
  observeEvent(input$Businesses, { 
    toggleWidgets('Businesses')
    output$pageTitle <- renderUI(list( propertiesStyle ,
                                       h1("Abandoned Properties and Businesses")))
    ReactOut <- businessReactive()
    # print(ReactOut)
    filterMapData = ReactOut$mapOutput
    propMapData = ReactOut$propOutput
    
    
    
    
    output$data_ui <- renderUI({
      fluidPage(
        fluidRow(
          renderPlotly({
            plotDataFiltered <- businessReactive()$hist
            # Plot a histogram of business licenses issued and abandoned properties found
            plot <- ggplot(data = plotDataFiltered, aes(x = date_of_interest, fill = Class)) +
              geom_histogram(position = "identity", alpha = 0.5) +
              labs(x = "Date", y = "Number of Properties", fill = "Type of Property", title = "Abandoned Properties & Businesses") +
              theme(text = element_text(size = 14))
            ggplotly(plot)
          })
        )
      )
    })
  })
  
  # Reactive function to control the dates, at first only those that are fed to the plot at the bottom
  businessReactive <- reactive({
    
    # Filter the data for both data sets if one of the districts is selected
    if (input$district != "All") {
      busPlotData <- filter(busPlotData, council_district == input$district)
      propPlotData <- filter(propPlotData, council_district == input$district)
      biz_spatial <- biz_spatial[biz_spatial@data$council_district == input$district,]
      Abandoned_Property_Parcels <- Abandoned_Property_Parcels[Abandoned_Property_Parcels@data$council_district == input$district,]
    }
    
    output <- filter(busPlotData, 
                     date_of_interest >= input$businessDate[1],
                     date_of_interest <= input$businessDate[2])
    out2 <- filter(propPlotData,
                   date_of_interest >= input$dates[1],
                   date_of_interest <= input$dates[2])
    output <- bind_rows(output, out2)
    
    mapDataOut <- biz_spatial[((biz_spatial@data$Issue_Date >= input$businessDate[1] & 
                                  biz_spatial@data$Issue_Date <= input$businessDate[2]) |
                                 is.na(biz_spatial@data$Issue_Date)),]
    #print(Abandoned_Property_Parcels@data$clean_date)
    properties <- Abandoned_Property_Parcels@data %>%
      filter(!is.na(clean_date)) %>%
      filter(clean_date >= input$dates[1],
             clean_date <= input$dates[2])
    
    mapProperties <- Abandoned_Property_Parcels[((na.omit(Abandoned_Property_Parcels@data$clean_date) >= input$dates[1]) && 
                                                   (na.omit(Abandoned_Property_Parcels@data$clean_date) <= input$dates[2]) || (is.na(Abandoned_Property_Parcels@data$clean_date))),]
    if(input$district != "All") {
      mapProperties <- mapProperties[((mapProperties@data$Council_Di %in% input$district)),]
    }
    # redraw map in reactive function as this doesn't seem to work with a leaflet function
    leafletProxy('map', session) %>%
      clearGroup('Businesses') %>%
      showGroup('Abandoned Properties') %>%
      hideGroup('Schools') %>%
      clearGroup('Abandoned Properties') %>%
      addPolygons(
        data = mapProperties, 
        group = 'Abandoned Properties',
        weight = 1,
        fillColor = ~ colorFactor(outcome_colors, fct_explicit_na(Outcome_St))(fct_explicit_na(Outcome_St)),
        highlightOptions = highlightOptions(
          color = "black",
          weight = 0.2,
          bringToFront = TRUE
        ),
        popup = paste(
          '<br>Outcome: ',
          properties$Outcome_St,
          '</br>District: ',
          properties$Council_Di,
          '<br>Street: ',
          properties$Street_Nam,
          '</br>Zip: ',
          properties$Zip_Code,
          '<br>Outcome Date: ',
          properties$Date_of_Ou,
          sep = ''
        )
      ) %>%
      addCircleMarkers(data = mapDataOut, radius = 3, fillOpacity = 0.2, stroke=F, group = "Businesses", color="green") 
    
    list(hist = output, mapOutput = mapDataOut)
    
  })
})

### App creation ----
shinyApp(ui, server)
