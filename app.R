library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(plotly)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(rnoaa)
library(tidyr)
library(corrplot)
library(heatmaply)
library(shinyWidgets)
library(geosphere)
library(tidyquant)
library(weatherr)
library(bigrquery)
library(shinycssloaders)
library(darksky)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black", 
                    
                    dashboardHeader(title = "PFAS Analysis and Intervention", titleWidth =400),
                    
                    dashboardSidebar(width = 300,
                                     sidebarMenu(
                                         menuItem("User Guide", tabName = "userguide", icon = icon("question-circle")),
                                         menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar"), selected = TRUE)
                                         ),
                                     
                                     radioButtons(
                                         inputId = "dataType", 
                                         label = "Data to Analyze:", 
                                         choices = list("User Defined" = "userdefined",
                                                        "Quarterly PFAS Testing 2019" = "quarterly",
                                                        "EDT Library" = "edt"),
                                         selected = "quarterly"
                                         ),
                                     
                                     conditionalPanel(condition = "input.dataType == 'userdefined'",
                                                      helpText("Please enter a user defined Site ID, latitude, and longitude."),
                                                      textInput(inputId = "siteID", label = h5("Site ID"), value = "Sacramento"),
                                                      fluidRow(column(width = 6,
                                                                      numericInput(
                                                                          "lat", 
                                                                          label = h5("Latitude"), 
                                                                          value = 38.5816, 
                                                                          min = -90, 
                                                                          max = 90)
                                                      ),
                                                      column(width = 6,
                                                             numericInput("long",
                                                                          label = h5("Longitude"),
                                                                          value = -121.4944, 
                                                                          min = -180, 
                                                                          max = 180)
                                                      )
                                                      ),
                                                      pickerInput(
                                                          inputId = "dataset", 
                                                          label = "Facilities:",
                                                          choices = list("Landfills" = "Landfill",
                                                                         "Military Sites" = "Military Site",
                                                                         "DTSC Hazardous Waste Sites" = "DTSC Hazardous Waste Site"),
                                                          selected = c("Landfill", "Military Site", "DTSC Hazardous Waste Site"),
                                                          multiple = TRUE,
                                                          options = list(`actions-box` = TRUE)
                                                      ),
                                                      numericInput(
                                                          "radius", 
                                                          "Facilities Within How Many Miles:", 
                                                          value = 10, 
                                                          min = 1, 
                                                          max = 170,000),
                                                      helpText("Upload a .csv of test results for one test site. Your file should have a column named   \"Parameter\", \"Date\", and \"Result.Value\"."),
                                                      fileInput(
                                                          "file", 
                                                          "Choose a CSV File",  
                                                          multiple = FALSE, 
                                                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                                     ),
                                     
                                     conditionalPanel(
                                         condition = "input.dataType == 'quarterly'",
                                         
                                         uiOutput("quarterlySelect"),
                                         
                                         pickerInput(
                                             inputId = "dataset", 
                                             label = "Facilities:",
                                             choices = list("Landfills" = "Landfill",
                                                            "Military Sites" = "Military Site",
                                                            "DTSC Hazardous Waste Sites" = "DTSC Hazardous Waste Site"),
                                             selected = c("Landfill", "Military Site", "DTSC Hazardous Waste Site"),
                                             multiple = TRUE,
                                             options = list(`actions-box` = TRUE)
                                         ),
                                         
                                         numericInput(
                                             "radius", 
                                             "Facilities Within How Many Miles:", 
                                             value = 10, 
                                             min = 1, 
                                             max = 170,000)
                                     ),
                                     
                                     conditionalPanel(
                                         condition = "input.dataType == 'edt'",
                                         
                                         uiOutput("edtLibrarySelect"),
                                         
                                         pickerInput(
                                             inputId = "dataset", 
                                             label = "Facilities:",
                                             choices = list("Landfills" = "Landfill",
                                                            "Military Sites" = "Military Site",
                                                            "DTSC Hazardous Waste Sites" = "DTSC Hazardous Waste Site"),
                                             selected = c("Landfill", "Military Site", "DTSC Hazardous Waste Site"),
                                             multiple = TRUE,
                                             options = list(`actions-box` = TRUE)
                                         ),
                                         
                                         numericInput(
                                             "radius", 
                                             "Facilities Within How Many Miles:", 
                                             value = 10, 
                                             min = 1, 
                                             max = 170,000)
                                          ),
                            
                                     actionButton("goButton", "GO")
                                     ),
                    
                    dashboardBody(
                        tabItems(
                            tabItem(
                                tabName = "userguide",
                                fluidRow(
                                    tabBox(width = 12,
                                           tabPanel("General", 
                                                    h3("What are PFAS?"),
                                                    h4("PFAS or Per- and polyfluoroalkyl substances are a group of manmade 
                                                       chemicals that are becoming commonly known as the forever chemicals.
                                                       These forever chemicals are persistent not only in the environment,
                                                       but the human body, meaning they do not breakdown and can start to 
                                                       accumulate. Which can possibly lead to adverse effects. The more common
                                                       PFAS you may have heard of are PFOA or PFOS. These forever chemicals 
                                                       are commonly used in things such a non-stick cookware, food packaging,
                                                       water resistant clothing, and the list goes on. While PFAS have been 
                                                       used since the 1940’s it is not until recent years that testing and 
                                                       notification levels in drinking water have been implemented. In 2019 
                                                       the CA State Water Board issued specific orders to airports, landfills,
                                                       and adjacent water systems, identified as potential PFAS source locations,
                                                       to start collecting data."),
                                                    
                                                    h4("Some of this test data was the foundation of this project. This data led
                                                       to discussions of whether rain events were a cause of influx of PFAS in 
                                                       each test site. If there was a rain event, did certain contaminant levels,
                                                       go up. If so, which facilities in the area could be contributing to this 
                                                       increase. If there is not a rain event, then what did cause an increase to
                                                       contaminant levels? These ideas came together to produce the PFAS Analysis
                                                       and Intervention Application."),
                                                    
                                                    h4("The App begun as a tool to analyze user inputted data and has now grown
                                                       to not only analyze user inputted data, but also the data gathered in the
                                                       2019 efforts as well as additional data collected and published by the CA
                                                       Water Boards. The user inputted data section can also be scaled up to different
                                                       contaminants, not just PFAS."),
                                                    
                                                    h3("Useful Links"),
                                                    h4(tags$div(tags$a(href="https://www.waterboards.ca.gov/pfas/", "CA Water Boards: Per- and Polyfluoroalkyl Substances (PFAS)", target="_blank"))),
                                                    h4(tags$div(tags$a(href="https://www.epa.gov/pfas", "EPA: Per- and Polyfluoroalkyl Substances (PFAS)", target="_blank"))),
                                                    h4(tags$div(tags$a(href="https://tdb.epa.gov/tdb/findcontaminant", "EPA: Find a Contaminant Search", target = "_blank"))),
                                                    h4(tags$div(tags$a(href="https://comptox.epa.gov/dashboard", "EPA: CompTox Chemicals Dashboard", target = "_blank"))),
                                                    h4(tags$div(tags$a(href="https://www.epa.gov/water-research/epa-drinking-water-research-methods", "EPA Drinking Water Research Methods", target="_blank"))),
                                                    
                                                    h3("Data Links"),
                                                    h4(tags$div(tags$a(href="https://www.waterboards.ca.gov/pfas/drinking_water.html", "PFAS Monitoring 2019", target="_blank"))),
                                                    h4(tags$div(tags$a(href="https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/EDTlibrary.html", "EDT Library", target="_blank"))),
                                                    h4(tags$div(tags$a(href="https://www.epa.gov/lmop/project-and-landfill-data-state", "Landfill and Projects Data", target="_blank")))),
                                           tabPanel("Dashboard", 
                                                    h2("How the App is Intended to be Used"),
                                                    h4("Under \"Data to Analyze:\" the user can choose \"User Defined\", \"Quarterly PFAS Testing 2019\", or
                                                       \"EDT Library\"."),
                                                    h3("User Defined"),
                                                    h4(tags$hr(style="border-color: black;")),
                                                    h4("When a user wants to analyze their own data, choose \"User Defined\". The user
                                                    must enter in a user defined Site ID, Latitude and Longitude of the site, and upload
                                                       data from one test site. The data uploaded must be a csv, in long format, with 
                                                       the columns: \"Date\", \"Parameter\", and \"Result.Value\"."),
                                                    DT::dataTableOutput("exampleUpload", height = 275),
                                                    br(),
                                                    h3("Quarterly PFAS Testing 2019"),
                                                    h4(tags$hr(style="border-color: black;")),
                                                    h4("The following test site's data is available when \"Quarterly PFAS Testing 2019\" is chosen. 
                                                       All test sites with a yearly total of 0 for all contaminats were removed from the original list."),
                                                    helpText("Note: Contaminant levels may have fluctuated between data points depending on the length of duration between two data points. Please interpret wisely."),
                                                    DT::dataTableOutput("quarterlyLocations", height = 500),
                                                    br(),
                                                    h3("EDT Library"),
                                                    h4(tags$hr(style="border-color: black;")),
                                                    h4("The following test site's data is available when \"EDT Library\" is chosen."),
                                                    helpText("Note: Contaminant levels may have fluctuated between data points depending on the length of duration between two data points. Please interpret wisely."),
                                                    DT::dataTableOutput("edtLocations", height = 500)
                                                    
                                                    )
                                           ) # tabB box
                                    ) # fluid row
                                ), #tabItem
                            
                            tabItem(
                                tabName = "dashboard",
                                fluidRow(column(width =  7,
                                                tabBox(width = NULL,
                                                       tabPanel(
                                                           "Contaminant Levels",
                                                           plotlyOutput("conLevels", height = 325) %>% 
                                                        withSpinner(color="#1b6d96")
                                                        ),
                                           # tabPanel("Difference Log(x)",
                                           #          plotlyOutput("diffLog", height = 325)),
                                                       tabPanel(
                                                           "Correlation Matrix",
                                                           plotlyOutput("corrMatrix", height = 325) %>%
                                                               withSpinner(color="#1b6d96")
                                                           )
                                                       ),
                                                tabBox(width = NULL,
                                                       tabPanel(
                                                           "Historical Precipitation",
                                                           plotlyOutput("histPRCP", height = 325) %>%
                                                               withSpinner(color="#1b6d96")
                                                           ),
                                                       tabPanel("Forecasted Precipitation",
                                                                plotlyOutput("forecastPRCP", height = 325) %>% 
                                                                    withSpinner(color="#1b6d96")
                                                                )
                                                       )
                                           ),
                                         
                                         column(width = 5,
                                                tabBox(width = NULL,
                                                       tabPanel("Map",
                                                                leafletOutput("map", height = 730) %>% 
                                                                    withSpinner(color="#1b6d96")
                                                                )
                                                       )
                                                )
                                          ),
                                
                                fluidRow(column(width = 12,
                                                tabBox(width = NULL, 
                                                       tabPanel(
                                                           "Facilities List",
                                                           DT::dataTableOutput("facilities", height = 750) %>% 
                                                               withSpinner(color="#1b6d96")
                                                           ),
                                                       tabPanel("Inputed Data",
                                                                DT::dataTableOutput("rawData", height = 750) %>% 
                                                                    withSpinner(color="#1b6d96")
                                                                )
                                                       )
                                                )
                                         )
                                )
                        )
                    )
)

server <- function(input, output) {
    
    output$exampleUpload <- renderDataTable({
        DT::datatable(read.csv("Upload Example.csv", fileEncoding = "UTF-8-BOM"),
                      rownames = FALSE, 
                      options = list(paging = FALSE, 
                                     dom = 't'))
    })
        
    bq_auth(cache = ".secrets", email = "melissaaa.salazar@gmail.com", use_oob = TRUE)
    
    # Store the project id
    projectid = "pfas-286915"
    
    quarterly <- reactive({
        
        sql<-paste("SELECT District_LPA, System_No_, Sample_Point_ID, System_Name, Sampling_Point_Name, Latitude, Longitude, Location___ , SUM(Finding__ng_L___) AS Total,
                   FROM `pfas-286915.pfas_opendata.PFA_RANDOM_SAMPLES` 
                   GROUP BY District_LPA, System_No_, Sample_Point_ID, System_Name, Sampling_Point_Name, Latitude, Longitude, Location___")
        
        # Run the query and store the data in a dataframe
        quarterly <- bq_project_query(projectid, sql)
        
        quarterly <- bq_table_download(quarterly)
        
        quarterly <- quarterly %>%
            filter(Total > 0) %>%
            arrange(desc(Total)) %>%
            mutate(Sample_Point_ID = as.character(Sample_Point_ID)) %>%
            mutate(PRIM_STA_C = ifelse(nchar(Sample_Point_ID) == 1, paste0(System_No_,"-00", Sample_Point_ID),
                                       ifelse(nchar(Sample_Point_ID) == 2, paste0(System_No_,"-0", Sample_Point_ID),
                                              paste0(System_No_,"-",Sample_Point_ID))))
            
    })
    
    output$quarterlyLocations <- renderDataTable({
        DT::datatable(quarterly() %>%
                         select(District_LPA, System_No_, Sample_Point_ID, PRIM_STA_C, System_Name, Latitude, Longitude, Location___) %>%
                         dplyr::rename("District LPA" = District_LPA, 
                                "System Number" = System_No_, 
                                "Sample Point ID" = Sample_Point_ID,
                                "Primary Station Code" = PRIM_STA_C,
                                "System Name" = System_Name, 
                                "Location" = Location___),
                     rownames = FALSE, 
                     options = list(paging = FALSE, 
                                    dom = 'ilft',
                                    buttons = c('csv', 'excel', 'pdf', 'print'),
                                    scrollX = TRUE, 
                                    scrollY = "400px"))
    })
    
    output$quarterlySelect <- renderUI({
        
        selectizeInput(inputId="quarterlySite",
                       label="Test Site:",
                       choices= quarterly()$PRIM_STA_C,
                       selected= quarterly()$PRIM_STA_C[1],
                       multiple = FALSE)
        
    })
    
    edtLibrary <- reactive({
        
        sql<-paste("SELECT DISTINCT District_LPA, PRIM_STA_C, Sampling_Point_Name, Latitude, Longitude
                           FROM `pfas-286915.pfas_opendata.latlongpfas`")
        
        # Run the query and store the data in a dataframe
        edtLibrary <- bq_project_query(projectid, sql)
        
        edtLibrary <- bq_table_download(edtLibrary)
    })
    
    output$edtLocations <- renderDataTable({
        DT::datatable(edtLibrary() %>%
                          select(District_LPA, PRIM_STA_C, Sampling_Point_Name, Latitude, Longitude) %>%
                          dplyr::rename("District LPA" = District_LPA, 
                                        "Primary Station Code" = PRIM_STA_C,
                                        "System Name" = Sampling_Point_Name),
                      rownames = FALSE, 
                      options = list(paging = FALSE, 
                                     dom = 'ilft',
                                     buttons = c('csv', 'excel', 'pdf', 'print'),
                                     scrollX = TRUE, 
                                     scrollY = "400px"))
    })
    
    output$edtLibrarySelect <- renderUI({
        
        selectizeInput(inputId="edtSite",
                       label="Test Site:",
                       choices= edtLibrary()$PRIM_STA_C,
                       selected= edtLibrary()$PRIM_STA_C[1],
                       multiple = FALSE)
        
    })
    
    dataset <- eventReactive(input$goButton, {
        
        if (input$dataType == 'userdefined') {
            
            df <- read.csv(input$file$datapath, fileEncoding = "UTF-8-BOM") %>%
                mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
            
        } else if (input$dataType == 'quarterly') {
            
            location <- quarterly() %>%
                filter(PRIM_STA_C == input$quarterlySite)
            
            sql<-paste("SELECT * FROM `pfas-286915.pfas_opendata.PFA_RANDOM_SAMPLES` 
                       WHERE System_No_ = ",location$System_No_[1]," AND
                       Sample_Point_ID = ",location$Sample_Point_ID[1],"")
            
            # Run the query and store the data in a dataframe
            df <- bq_project_query(projectid, sql)
            
            df <- bq_table_download(df) 
            
            df <- df %>%
                mutate(Sample_Point_ID = as.character(Sample_Point_ID)) %>%
                mutate(PRIM_STA_C = ifelse(nchar(Sample_Point_ID) == 1, paste0(System_No_,"-00", Sample_Point_ID),
                                           ifelse(nchar(Sample_Point_ID) == 2, paste0(System_No_,"-0", Sample_Point_ID),
                                                  paste0(System_No_,"-",Sample_Point_ID)))) %>%
                mutate(Parameter = sub(".*\\((.*)\\)", "\\1", Chemical)) %>%
                mutate(Parameter = ifelse(Parameter == "N-ETHYL PERFLUOROOCTANESULFONAMIDOACETIC ACID", "NEtFOSAA",
                                          ifelse(Parameter == "9-CHLOROHEXADECAFLUORO-3-OXANONE-1-SULFONIC ACID", "9Cl-PF3ONS",
                                                 ifelse(Parameter == "11-CHLOROEICOSAFLUORO-3-OXAUNDECANE-1-SULFONIC ACI", "11Cl-PF3OUdS",
                                                        ifelse(Parameter == "N-METHYL PERFLUOROOCTANESULFONAMIDOACETIC ACID", "NMeFOSAA", trimws(Parameter)))))) %>%
                filter(Parameter != "PFOS + PFOA") %>%
                mutate(Date = as.factor(paste0("Q", trimws(Round)))) %>%
                rename(Result.Value = Finding__ng_L___) %>%
                select(Date, Parameter, Result.Value, PRIM_STA_C)
            
        } else if(input$dataType == 'edt'){
            
            sql <- paste0("SELECT * FROM `pfas_opendata.pfas_main`
                         WHERE PRI_STA_C = '",input$edtSite,"'")
            
            df <- bq_project_query(projectid, sql)
            
            df <- bq_table_download(df) %>%
                dplyr::rename(Date = date,
                              Parameter = parameter,
                              Result.Value = ResultValue) %>%
                rename(PRIM_STA_C = PRI_STA_C)
            
        } 
        
        important <- c("PFOA", "PFOS")
        
        df <- df %>%
            mutate(Parameter = droplevels(factor(Parameter,
                                                 levels = c(important, sort(setdiff(Parameter, important)))))) %>%
            arrange(Parameter, Date)
        
        return(df)
    })
    
    ### Contamination Levels ###
    
    contaminationLevels <- eventReactive(input$goButton, {
        
        if(input$dataType == 'quarterly'){
            
            ggplotly(ggplot(dataset(), aes(x = Date, y = Result.Value, fill = Parameter, group = Parameter)) +
                         ggtitle(paste0("Quarterly 2019 Contaminant Levels")) +
                         geom_point(aes(color = Parameter)) +
                         geom_line(aes(color = Parameter)) +
                         labs(x = "") +
                         #geom_bar(position = "stack", stat = "identity", width = 2)+
                         theme_tq() #+
                     # scale_fill_tq() +
                     # theme(axis.text.x = element_text(angle = 30, hjust = 1),
                     #       axis.title.x=element_blank()
                     #       )
            ) %>% 
                layout(legend = list(orientation = "h", 
                                     x= 0.1, y = -0.2)
                ) %>%
                config(displaylogo = FALSE,
                       modeBarButtonsToRemove = c('select2d','lasso2d', 'zoom2d','pan2d',
                                                  'autoScale2d', 'zoomIn2d', 'zoomOut2d',
                                                  'sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'
                       )
                )
            
        } else {
            ggplotly(ggplot(dataset(), aes(x = Date, y = Result.Value)) +
                         ggtitle(paste0("Contaminant Levels")) +
                         geom_point(aes(color = Parameter)) +
                         geom_line(aes(color = Parameter)) +
                         labs(x = "") +
                         #geom_bar(position = "stack", stat = "identity")+
                         theme_tq() #+
                     # scale_fill_tq() +
                     # theme(axis.text.x = element_text(angle = 30, hjust = 1),
                     #       axis.title.x=element_blank()
                     #       )
            ) %>% 
                layout(legend = list(orientation = "h", 
                                     x= 0.1, y = -0.2)
                ) %>%
                config(displaylogo = FALSE,
                       modeBarButtonsToRemove = c('select2d','lasso2d', 'zoom2d','pan2d',
                                                  'autoScale2d', 'zoomIn2d', 'zoomOut2d',
                                                  'sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'
                       )
                )
        }
        
        })
    
    output$conLevels <- renderPlotly({contaminationLevels()})
    
    ### Difference Log Plot ###
    
    differenceLog <- eventReactive(input$goButton, {
        
        req(input$file)
        
        df <- read.csv(input$file$datapath)
        
        diffLog<- df %>% 
            mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
            group_by(Parameter) %>%
            mutate(Differences = as.numeric(c("0", diff(log(Result.Value)))))%>%
            select(Date,Parameter,Differences) %>%
            ungroup(Parameter)
        
        casted_Differences<- reshape::cast(
            diffLog, 
            Date ~ Parameter, 
            fun.aggregate = sum, 
            value = 'Differences')
        
        ggplotly(ggplot(diffLog, aes(x=Date, y=Differences, group=Parameter)) +
                     geom_line(aes(color=Parameter))+
                     geom_point(aes(color=Parameter))+
                     theme_tq() +
                     scale_color_tq() +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1),
                           axis.title.x=element_blank()
                     )
                 ) %>% 
            config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c('select2d','lasso2d', 'zoom2d','pan2d',
                                              'autoScale2d', 'zoomIn2d', 'zoomOut2d',
                                              'sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian')
                   )
        })
    
    output$diffLog <- renderPlotly({differenceLog()})
    
    ### Correlation Matrix ###
    
    output$corrMatrix <- renderPlotly({
        
        df <- dataset()
        
        casted <- reshape::cast(df, Date ~ Parameter, fun.aggregate = sum, value = 'Result.Value')
        
        PFAS_corr <- cor(casted)
        round(PFAS_corr, 4)
        
        heatmaply(PFAS_corr,
                  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                      low = "#0043b0", 
                      high = "#e00000", 
                      midpoint = 0, 
                      limits = c(-1, 1)
                  ),
                  na.rm = TRUE
                  )
        
        # corrplot(PFAS_corr, method = "color",
        #          type = "upper", 
        #          addCoef.col = "black", # Add coefficient of correlation
        #          tl.col = "black", tl.srt = 90, # Text label color and rotation
        #          diag = FALSE, # hide correlation coefficient on the principal diagonal
        #          mar=c(0,0,1,0))
        
    })
    
    ### Precipitation ###
    
    allStations <- eventReactive(input$goButton, {
        
        ghcnd_stations(var = "PRCP") %>%
            filter(state == "CA") %>%
            filter(element == "PRCP")
        
    })
    
    site <- eventReactive(input$goButton, {
        
        if(input$dataType == 'userdefined'){
            
            df <- data.frame(id = paste(input$siteID), latitude = input$lat, longitude = input$long)
            
        } else if(input$dataType == 'quarterly'){
            
            df <- quarterly() %>%
                filter(PRIM_STA_C == input$quarterlySite) %>%
                distinct(PRIM_STA_C, Longitude, Latitude) %>%
                rename(id = PRIM_STA_C, latitude= Latitude, longitude = Longitude) %>%
                select(id, latitude, longitude)
            
        } else if(input$dataType == 'edt'){
            
            df <- edtLibrary() %>%
                filter(PRIM_STA_C == input$edtSite) %>%
                distinct(PRIM_STA_C, Longitude, Latitude) %>%
                rename(id = PRIM_STA_C, latitude= Latitude, longitude = Longitude) %>%
                select(id, latitude, longitude)
        } 
        return(df)
        
    })
    
    station <- eventReactive(input$goButton, {
        
        if(input$dataType == 'quarterly'){
            as.data.frame(meteo_nearby_stations(lat_lon_df = site(),
                                                station_data = allStations(),
                                                limit = 1,
                                                var = "PRCP",
                                                year_min = 2018,
                                                year_max = as.integer(format(Sys.Date()))
                                                )
            )
        } else {
            as.data.frame(meteo_nearby_stations(lat_lon_df = site(),
                                                station_data = allStations(),
                                                limit = 1,
                                                var = "PRCP",
                                                year_min = as.integer(format(min(dataset()$Date), "%Y")),
                                                year_max = as.integer(format(Sys.Date()))
                                                )
            )
        }
    })
    
    ### Historical Precipitation ###
    
    historicalPrecipitation <- eventReactive(input$goButton, {
        
        if(input$dataType == 'quarterly'){
            
            precipitation <- meteo_tidy_ghcnd(
                station()[[1]], 
                keep_flags = FALSE, 
                var = "PRCP",
                date_min = "2019-01-01", 
                date_max = "2019-12-31"
            )
            
        } else {
            precipitation <- meteo_tidy_ghcnd(
                station()[[1]], 
                keep_flags = FALSE, 
                var = "PRCP",
                date_min = min(dataset()$Date), 
                date_max = max(dataset()$Date)
            ) 
        }
        precipitation[is.na(precipitation)] <- 0
    
        precipitation <- precipitation %>%
            mutate(DATE = date)
        
        precipitation <- separate(
            data = precipitation, 
            col = date, 
            into = c("year", "month", "day"), 
            sep = "-"
            )
        
        ggplotly(ggplot(precipitation, aes(x=DATE, y=prcp)) +
                     labs(
                         title = paste0("Station ", station()[[1]], " Precipitation"),
                         x = "",
                         y = "Preciptation (mm)"
                         ) +
                     #geom_point() +
                     geom_line(color = "#0d7591") +
                 theme_tq() +
                 scale_fill_tq() +
                 theme(axis.text.x = element_text(angle = 30, hjust = 1),
                       axis.title.x=element_blank()
                       )
                 ) %>% 
            config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c('select2d','lasso2d', 'zoom2d',
                                              'pan2d', 'autoScale2d', 'zoomIn2d', 'zoomOut2d',
                                              'sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
    })
    
    output$histPRCP <- renderPlotly(historicalPrecipitation())
    
    ### Forecasted Precipitation ###
    
    forecast <- eventReactive(input$goButton, {
        
        if(input$dataType == 'userdefined'){
            
            forecast <- locationforecast(lat = input$lat,
                                         lon = input$long,
                                         elevation=NULL,
                                         location=NULL,
                                         exact=FALSE,
                                         tz=Sys.timezone()) %>%
                mutate(date = as.Date(timefrom)) %>%
                group_by(date) %>%
                summarise(precipitation = sum(precipitation))
            
        } else {
            
            forecast <- locationforecast(lat = as.numeric(site()$latitude),
                                         lon = as.numeric(site()$longitude),
                                         elevation=NULL,
                                         location=NULL,
                                         exact=FALSE,
                                         tz=Sys.timezone()) %>%
                mutate(date = as.Date(timefrom)) %>%
                group_by(date) %>%
                summarise(precipitation = sum(precipitation))
            
        }
    })
    
    forecastedPrecipitation <- eventReactive(input$goButton, {
        
        ggplotly(ggplot(forecast(), aes(x=date, y=precipitation)) +
                     labs(title = paste0("Forecasted Precipitation"),
                          x = "",
                          y = "Precipitation (mm)") +
                     geom_line(color = "#0d7591") +
                     geom_point(color = "#0d7591") +
                     #geom_bar(stat = "identity", fill = "#0d7591")+
                     theme_tq() +
                     theme(axis.text.x = element_text(angle = 30, hjust = 1),
                           axis.title.x=element_blank()
                           )
                 ) %>% 
            config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c('select2d','lasso2d', 'zoom2d',
                                              'pan2d', 'autoScale2d', 'zoomIn2d', 
                                              'zoomOut2d', 'sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
    })
    
    output$forecastPRCP <- renderPlotly(forecastedPrecipitation())
    
    allFacilities <- eventReactive(input$goButton, {
        
        ### Locations ###
        
        DTSC_Facilities <- read.csv("CA DTSC Hazardous Waste Sites.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM") %>%
            mutate(FACILITY.TYPE = "DTSC Hazardous Waste Site") %>%
            select(c(1,2,9,3:8))
        
        Landfills <- read.csv("CA Landfills Sept 2020.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM") %>%
            mutate(FACILITY.TYPE = "Landfill") %>%
            select(c(1,2,9,3:8))
        
        Military_Sites <- read.csv("CA Military Sites Sept 2020.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM") %>%
            mutate(FACILITY.TYPE = "Military Site") %>%
            select(c(1,2,9,3:8))
        
        allLocations <- DTSC_Facilities %>%
            add_row(Landfills) %>%
            add_row(Military_Sites) %>%
            arrange(FACILITY.TYPE) %>%
            mutate(icon = as.factor(stringr::str_replace_all(FACILITY.TYPE, " ", "_")))
        
        return(allLocations)
    })
    
    facilitiesList <- eventReactive(input$goButton, {
        
        allFacilities() %>%
            filter(FACILITY.TYPE %in% input$dataset) %>%
            rowwise() %>%
            mutate(dist_in_meters = distm(c(LONGITUDE, LATITUDE), c(site()$longitude, site()$latitude), fun=distHaversine),
                   dist_in_miles = round((dist_in_meters/1000) * 0.621371, digits = 2)) %>%
            filter(dist_in_miles <= input$radius)
    })
    
    ### Map ###

    output$map <- renderLeaflet({
        
        userSiteID <- eventReactive(input$goButton, {
            
            if(input$dataType == 'userdefined'){
                input$siteID
            } else {
                unique(dataset()$PRIM_STA_C)
            }  
        })
        
        points <- eventReactive(input$goButton, {
            
            if(input$dataType == 'userdefined') {
                
                cbind(input$long, input$lat)
                
            } else if(input$dataType == 'quarterly'){
                
                quarterly() %>%
                    filter(PRIM_STA_C == input$quarterlySite) %>%
                    select(Longitude, Latitude) %>%
                    dplyr:: rename(long = Longitude,
                                   lat = Latitude) %>%
                    as.matrix()
                
            } else if(input$dataType == 'edt'){
                
                edtLibrary() %>%
                    filter(PRIM_STA_C == input$edtSite) %>%
                    select(Longitude, Latitude) %>%
                    dplyr:: rename(long = Longitude,
                                   lat = Latitude) %>%
                    as.matrix()
                
            } 
            
        })
        
        icons <- iconList(
            DTSC_Hazardous_Waste_Site <- makeIcon(iconUrl = "skull-47879.png",
                                    iconWidth = 18, iconHeight = 18),
            Landfill <- makeIcon(iconUrl = "trash-can-icon-28691.png",
                                     iconWidth = 18, iconHeight = 18),
            Military_Site <- makeIcon(iconUrl = "stars-png-621.png",
                                   iconWidth = 18, iconHeight = 18)
        )
        
        pal <- colorFactor(
            palette = c('#fcaa12', '#fc1212', '#02bf28'),
            domain = c("Landfill", "DTSC Hazardous Waste Site", "Military Site")
        )
        
        leaflet() %>%
            addTiles() %>%
            addCircleMarkers(data = facilitiesList(), 
                             lat = ~LATITUDE, 
                             lng = ~LONGITUDE,
                             label = ~SITE.NAME, 
                             radius = 5, 
                             color = ~pal(FACILITY.TYPE)) %>%
            #addMarkers(data = facilitiesList(), lat = ~LATITUDE, lng = ~LONGITUDE, icon = ~ icons[as.numeric(icon)]) %>%
            addCircleMarkers(data = points(), 
                             label = userSiteID(), 
                             color = "#000000") %>%
            addCircleMarkers(lat = station()[[3]],
                             lng = station()[[4]],
                             label =  station()[[1]],
                             color = "#00a1a3") %>%
            leaflet::addLegend(title = "Legend",
                      position = 'bottomleft',
                      opacity = 1,
                      colors = c("#000000", "#19defc", '#fcaa12', '#fc1212', '#02bf28'),
                      labels = c("Test Site", "Weather Station", "Landfill", "DTSC Hazardous Waste Site", "Military Site")) 
    })
    
### Facilities Table ###
    
    output$facilities <- renderDataTable({
        
        DT::datatable(facilitiesList() %>% 
                          arrange(dist_in_miles) %>% 
                          select(-icon, -dist_in_meters) %>%
                          dplyr::rename("Site Name" = SITE.NAME,
                                        "Global ID" = GLOBAL.ID,
                                        "Facility Type" = FACILITY.TYPE,
                                        "Site Type" = SITE_TYPE,
                                        "Status" = STATUS,
                                        "Address" = ADDRESS,
                                        "City" = CITY,
                                        "Latitude" = LATITUDE,
                                        "Longitude" = LONGITUDE,
                                        "Miles Away" = dist_in_miles), 
                      extensions = 'Buttons',
                      rownames = FALSE, 
                      options = list(paging = FALSE, 
                                     dom = 'Bilft', 
                                     buttons = c('csv', 'excel', 'pdf', 'print'),
                                     scrollX = TRUE, 
                                     scrollY = "600px"))
        
    })
    
    ### Raw Data ###
    
    output$rawData <- renderDataTable({
        
        if(input$dataType == 'userdefined'){
            DT::datatable(dataset() %>%
                              select(Parameter, Date, Result.Value) %>%
                              dplyr::rename("Result Value" = Result.Value), 
                          rownames = FALSE, 
                          options = list(paging = FALSE, 
                                         dom = 'ilft',
                                         buttons = c('csv', 'excel', 'pdf', 'print'),
                                         scrollX = TRUE, 
                                         scrollY = "600px")
            )
        } else if(input$dataType == 'quarterly'){
            
            DT::datatable(dataset() %>%
                              select(PRIM_STA_C, Parameter, Date, Result.Value) %>%
                              dplyr::rename("Result Value" = Result.Value), 
                          rownames = FALSE, 
                          options = list(paging = FALSE, 
                                         dom = 'ilft',
                                         scrollX = TRUE, 
                                         scrollY = "600px"))
            
        }else if(input$dataType == 'edt'){
            DT::datatable(dataset() %>%
                              select(PRIM_STA_C, Parameter, Date, Result.Value) %>%
                              dplyr::rename("Result Value" = Result.Value), 
                          rownames = FALSE, 
                          options = list(paging = FALSE, 
                                         dom = 'ilft',
                                         scrollX = TRUE, 
                                         scrollY = "600px")
        )}
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
