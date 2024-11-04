# Shiny Monster Movies

### Libraries
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(DT)
library(maps)
library(mapdata)
library(leaflet)
library(DT)
library(rnaturalearth)
library(sf)


### INITIAL COMMANDS ----

#Set data path 

data_path <- "C:/WBG/GitHub/Tidy_Tuesday-s/10_29"

#Load data set for test

wwbi <- read_dta(file.path(data_path, "WWBI_2022_Kyrgyz_Republic_R.dta"))

selected_data2 <- data_table[, .(year, month, gender, urb)]


wwbi_select <- as.data.frame(selected_data2)

country_data <- data.frame(
  country = c("USA", "Canada", "Brazil", "India", "China"),
  indicator_name = c("Gender", "Education Level", "Age", "Wage", "Industry")
)

# Load world spatial data

world_spdf <- ne_countries(scale = "medium", returnclass = "sf")

# Create a color palette for countries
color_palette <- colorFactor(c("lightgreen", "lightgray"), domain = c("reported", "not_reported"))



#Countries 

countries_data <- c(
  "Aruba", "Afghanistan", "Angola", "Anguilla", "Albania", "United Arab Emirates", 
  "Argentina", "Armenia", "Antigua and Barbuda", "Australia", "Austria", "Azerbaijan", 
  "Burundi", "Belgium", "Benin", "Burkina Faso", "Bangladesh", "Bulgaria", 
  "Bahrain", "Bahamas", "The, Bosnia and Herzegovina", "Belarus", "Belize", 
  "Bermuda", "Bolivia", "Brazil", "Barbados", "Brunei Darussalam", "Bhutan", 
  "Botswana", "Central African Republic", "Canada", "Switzerland", "Chile", 
  "China", "Cote d'Ivoire", "Cameroon", "Congo, Republic of", "Colombia", 
  "Comoros", "Cabo Verde", "Costa Rica", "Curacao", "Cayman Islands", 
  "Cyprus", "Czech Republic", "Germany", "Djibouti", "Dominica", "Denmark", "Dominican Republic", 
  "Algeria", "Ecuador", "Egypt", "Arab Republic of", "Eritrea", "Spain", 
  "Estonia", "Ethiopia", "Finland", "Fiji", "France", "Micronesia", 
  "Federated States of, Gabon", "United Kingdom", "Georgia", 
  "Ghana", "Guinea", "Gambia", "The, Guinea-Bissau", "Equatorial Guinea", 
  "Greece", "Grenada", "Guatemala", "Guyana", "Hong Kong SAR", "China", "Honduras", 
  "Croatia", "Haiti", "Hungary", "Indonesia", "India", "Ireland", "Iran", 
  "Islamic Republic of, Iraq", "Iceland", "Israel", "Italy", "Jamaica", "Jordan", 
  "Japan", "Kazakhstan", "Kenya", "Kyrgyz Republic", "Cambodia", "Kiribati", "St. Kitts and Nevis", 
  "Korea, Republic of, Kuwait", "Lao People's Democratic Republic, Lebanon", "Liberia", 
  "Libya", "St. Lucia", "Sri Lanka", "Lesotho", "Lithuania", "Luxembourg", "Latvia", 
  "Macao SAR", "China", "Morocco", "Moldova", "Madagascar", "Maldives", "Mexico", 
  "Marshall Islands", "North Macedonia", "Mali", "Malta", "Myanmar", "Montenegro", "Mongolia", 
  "Mozambique", "Mauritania", "Montserrat", "Mauritius", "Malawi", "Malaysia", "Namibia", 
  "Niger", "Nigeria", "Nicaragua", "Netherlands", "Norway", "Nepal", "Nauru", "New Zealand", 
  "Oman", "Pakistan", "Panama", "Peru", "Philippines", "Palau", "Papua New Guinea", 
  "Poland", "Puerto Rico", "Portugal", "Paraguay", "Qatar", "Romania", 
  "Russian Federation", "Rwanda", "Saudi Arabia", "Sudan", "Senegal", 
  "Singapore", "Solomon Islands", "Sierra Leone", "El Salvador", "San Marino", 
  "Somalia", "South Sudan", "Sao Tome and Principe", "Suriname", "Slovak Republic", 
  "Slovenia", "Sweden", "Eswatini", "Sint Maarten (Dutch part)", "Seychelles", 
  "Syrian Arab Republic", "Turks and Caicos Islands", "Chad", 
  "Togo", "Thailand", "Tajikistan", "Turkmenistan", "Tonga", 
  "Trinidad and Tobago", "Tunisia", "Türkiye", "Tuvalu", "Taiwan", 
  "China", "Tanzania", "Uganda", "Ukraine", "Uruguay", "United States", 
  "Uzbekistan", "St. Vincent and the Grenadines", "Venezuela", 
  "Republica Bolivariana de", "Vietnam", "Vanuatu", "Samoa", "Kosovo","South Africa",
  "Zambia", "Zimbabwe")


# Sample data frame (replace this with your actual wwbi data)
wwbi_select <- data.frame(
  Variable = c("year", "month", "Gender", "urb"),
  Description = c("Year of Observation", "Month", "Gender", "Area")
)

### Define UI ----

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "WWB Indicators"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
      menuItem("Variable List", tabName = "variableList", icon = icon("table")),
      menuItem("Graphs", tabName = "graphs", icon = icon("chart-simple")), 
      menuItem("Indicators Status", tabName = "indicators", icon = icon("chart-simple"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Dashboard Description", status = "primary", solidHeader = TRUE, width = 12,
                    "Welcome to the World Bank Indicators Dashboard! The Worldwide Bureaucracy Indicators (WWBI) 
                    database is a unique cross-national dataset on public sector employment and wages that 
                    aims to fill an information gap, thereby helping researchers, development practitioners, 
                    and policymakers gain a better understanding of the personnel dimensions of state 
                    capability, the footprint of the public sector within the overall labor market, 
                    and the fiscal implications of the public sector wage bill."
                )
              )
      ),
      
      # Widgets tab
      tabItem(tabName = "widgets",
              fluidRow(
                infoBoxOutput("numberIndicatorsBox", width = 6),
                infoBoxOutput("numberCountriesBox", width = 6),
                infoBoxOutput("temporalCoverageAnnualBox", width = 6)
              ),
              fluidRow(
                infoBoxOutput("temporalCoverageYearsBox", width = 6),
                infoBoxOutput("lastUpdatedBox", width = 6)
              ),
              fluidRow(
                box(title = "Countries Covered", status = "primary", solidHeader = TRUE, width = 12,
                    h3("List of Countries:"),
                    tags$ul(
                      lapply(countries_data, function(country) {
                        tags$li(country)
                      })
                    )
                )
              )
      ),
      
      # Variable List tab
      tabItem(tabName = "variableList",
              fluidRow(
                box(title = "Available Variables", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("variableTable")
                )
              )
      ),
      
      # Graphs tab
      tabItem(tabName = "graphs",
              fluidRow(
                box(selectInput("indicator", "Select a WWB Indicator", 
                                choices = c("Gender", "Education Level", "Age", "Labor Status", "Wage", "Industry")),
                    title = "Worldwide Bureaucracy Indicators", status = "primary", solidHeader = TRUE, width = 4)
              ),
              conditionalPanel(
                condition = "input.indicator == 'Gender'",
                box(title = "Graph of Gender Distribution", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("genderPlot", width = "100%"),
                    downloadButton("downloadgenderPlot", "Download Plot", class = "btn btn-primary")
                )
              ),
              conditionalPanel(
                condition = "input.indicator == 'Education Level'",
                box(title = "Graph of Education Level", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("educationPlot", width = "100%"),
                    downloadButton("downloadeducationPlot", "Download Plot", class = "btn btn-primary")
                )
              ),
              conditionalPanel(
                condition = "input.indicator == 'Age'",
                box(title = "Graph of Age", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("agePlot", width = "100%"),
                    downloadButton("downloadagePlot", "Download Plot", class = "btn btn-primary")
                )
              ),
              conditionalPanel(
                condition = "input.indicator == 'Labor Status'",
                box(title = "Graph of Labor Status", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("laborstatusPlot", width = "100%"),
                    downloadButton("downloadlaborstatusPlot", "Download Plot", class = "btn btn-primary")
                )
              ),
              conditionalPanel(
                condition = "input.indicator == 'Wage'",
                box(title = "Graph of Wage", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("wagePlot", width = "100%"),
                    downloadButton("downloadwagePlot", "Download Plot", class = "btn btn-primary")
                )
              ),
              conditionalPanel(
                condition = "input.indicator == 'Industry'",
                box(title = "Graph of Industry", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("industryPlot", width = "100%"),
                    downloadButton("downloadindustryPlot", "Download Plot", class = "btn btn-primary")
                )
              )
      ),
      
      # Indicators Status tab with a world map
      tabItem(tabName = "indicators",
              fluidRow(
                box(title = "Indicator Status Across Countries", status = "primary", solidHeader = TRUE, width = 12,
                    "This map shows which countries have reported data for the selected indicator."
                ),
                box(title = "Select Indicator", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("indicatorSelect", "Choose Indicator", 
                                choices = c("Gender", "Education Level", "Age", "Labor Status", "Wage", "Industry"))
                ),
                box(title = "World Map", status = "primary", solidHeader = TRUE, width = 12,
                    leafletOutput("worldMap", height = 500)
                )
              )
      )
    )
  )
)



### Define Server ----

server <- function(input, output, session) {
  # Render dynamic infoBox outputs
  output$numberIndicatorsBox <- renderInfoBox({
    infoBox("Number of Indicators", "300", icon = icon("flag"), color = "light-blue")
  })
  
  output$numberCountriesBox <- renderInfoBox({
    infoBox("Number of Countries", "200", icon = icon("earth-americas"), color = "light-blue")
  })
  
  output$temporalCoverageAnnualBox <- renderInfoBox({
    infoBox("Temporal Coverage", "Annual", icon = icon("check"), color = "light-blue")
  })
  
  output$temporalCoverageYearsBox <- renderInfoBox({
    infoBox("Temporal Coverage", "2000 to 2020", icon = icon("timeline"), color = "light-blue")
  })
  
  output$lastUpdatedBox <- renderInfoBox({
    infoBox("Last Updated on", "Sep 15, 2022", icon = icon("calendar"), color = "light-blue")
  })
  
  # Render the data frame as a table
  output$variableTable <- renderDT({
    datatable(wwbi_select, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # Render the gender-related plot only when "Gender" is selected
  output$genderPlot <- renderPlot({
    req(input$indicator == "Gender")  # Render only when "Gender" is selected
    
    # Ensure gender is a factor with appropriate labels
    wwbi$gender <- factor(wwbi$gender, levels = c(1, 2), labels = c("Male", "Female"))
    
    ggplot(wwbi, aes(x = gender)) +  
      geom_bar(fill = "#B3CDE3") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +  # Add count labels
      labs(title = "Gender Distribution", x = "Gender", y = "Count") +
      theme_minimal()
  })
  output$educationPlot <- renderPlot({
    req(input$indicator == "Education Level")  # Render only when "Education Level" is selected
    ggplot(wwbi, aes(x = edulevel3)) +
      geom_bar(fill = "#B3CDE3") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +  # Add count labels
      labs(title = "Education Level Distribution", x = "Education Level", y = "Count") +
      theme_minimal()
  })
  output$agePlot <- renderPlot({
    req(input$indicator == "Age")  # Render only when "Age" is selected
    ggplot(wwbi, aes(x = age)) +
      geom_bar(fill = "#B3CDE3") +
      labs(title = "Age Distribution", x = "Age", y = "Count") +
      theme_minimal()
  })
  output$laborstatusPlot <- renderPlot({
    req(input$indicator == "Labor Status")  # Render only when "Gender" is selected
    
    # Ensure gender is a factor with appropriate labels
    wwbi$lstatus <- factor(wwbi$lstatus, levels = c(1, 2, 3), labels = c("Employed", "Unemployed", 
                                                                         "Non-LF"))
    
    ggplot(wwbi, aes(x = lstatus)) +  
      geom_bar(fill = "#B3CDE3") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +  # Add count labels
      labs(title = "Labor Status Distribution", x = "Labor Status", y = "Count") +
      theme_minimal()
  })
  # Render Wage Distribution Plot
  output$wagePlot <- renderPlot({
    req(input$indicator == "Wage")
    ggplot(wwbi, aes(x = wage)) +
      geom_histogram(binwidth = 100, fill = "#B3CDE3", color = "#B3CDE3") +
      labs(title = "Wage Distribution", x = "Wage", y = "Count") +
      scale_x_continuous(limits = c(0, 45000)) +  # Adjust limits as needed
      scale_y_continuous(limits = c(0, 100)) +  # Adjust limits as needed
      theme_minimal()
  })
  # Render Industry Distribution Plot
  output$industryPlot <- renderPlot({
    req(input$indicator == "Industry")
    ggplot(wwbi, aes(x = industry1)) +
      geom_bar(fill = "#B3CDE3") +  # Pastel blue
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +  # Add count labels
      labs(title = "Industry Distribution", x = "Industry", y = "Count") +
      theme_minimal()
  })
  output$downloadgenderPlot <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Recreate the plot here to save it with `ggsave`
      plot <- ggplot(wwbi, aes(x = gender)) + 
        geom_bar(fill = "#B3CDE3") +  # Pastel blue
        labs(title = "Industry Distribution", x = "Industry", y = "Count") +
        theme_minimal()
      
      # Save the plot to the specified file
      ggsave(filename = file, plot = plot, device = "png", width = 6, height = 4, units = "in")
    }
  )
  
  output$downloadeducationPlot <- downloadHandler(
    filename = function() {
      paste("education_plot", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file)
      print(renderPlot({
        # Your plot code here
      }))
      dev.off()
    }
  )
  output$downloadagePlot <- downloadHandler(
    filename = function() {
      paste("age_plot", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file)
      print(renderPlot({
        # Your plot code here
      }))
      dev.off()
    }
  )
  output$downloadlaborstatusPlot <- downloadHandler(
    filename = function() {
      paste("laborstatus_plot", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file)
      print(renderPlot({
        # Your plot code here
      }))
      dev.off()
    }
  )
  output$downloadwagePlot <- downloadHandler(
    filename = function() {
      paste("wage_plot", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file)
      print(renderPlot({
        # Your plot code here
      }))
      dev.off()
    }
  )
  output$downloadindustryPlot <- downloadHandler(
    filename = function() {
      paste("industry_plot", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file)
      print(renderPlot({
        # Your plot code here
      }))
      dev.off()
    }
  )
  # Render the map
  output$worldMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  # Update the map when an indicator is selected
  observe({
    req(input$indicatorSelect)
    
    # Filter countries reporting the selected indicator
    reported_countries <- country_data %>%
      filter(indicator_name == input$indicatorSelect) %>%
      pull(country)
    
    # Clear previous layers
    leafletProxy("worldMap") %>%
      clearShapes() %>%
      
      # Add polygons for each country
      addPolygons(data = world_spdf,  # Add polygons for world countries
                  fillColor = ~color_palette(ifelse(world_spdf$name %in% reported_countries, "reported", "not_reported")),
                  fillOpacity = 0.7,  # Adjust fill opacity for better visibility
                  color = "#FFFFFF",  # White borders for contrast
                  weight = 1,  # Border weight
                  smoothFactor = 0.5,  # Smooth edges
                  highlightOptions = highlightOptions(
                    color = "#FFD700",  # Gold color when highlighted
                    weight = 2,  # Heavier border when highlighted
                    fillOpacity = 0.9  # More opaque fill when highlighted
                  ),
                  label = ~name,  # Show country names as labels
                  labelOptions = labelOptions(
                    style = list("font-weight" = "bold"),  # Bold labels
                    textsize = "12px",  # Adjust label text size
                    direction = "auto"  # Automatic label direction
                  ),
                  popup = ~paste("<strong>Country:</strong>", name)  # Popup with country name
      )
  })
}



### Run the app ----
shinyApp(ui, server)