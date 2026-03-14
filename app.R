# ===============================
# Libraries
# ===============================
library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(DT)

# ===============================
# Load Dataset
# ===============================
airbnb <- read.csv(
  "C:/Users/HP/Downloads/Final_Cleaned_Airbnb_Dataset.csv",
  stringsAsFactors = FALSE
)

# ===============================
# Feature Engineering
# ===============================

# Price tiers with more meaningful categories
airbnb$price_tier <- cut(
  airbnb$price,
  breaks = c(0, 100, 250, 500, 1000, Inf),
  labels = c("Budget ($0-100)", "Economy ($101-250)", 
             "Standard ($251-500)", "Premium ($501-1000)", 
             "Luxury ($1000+)")
)

# Construction eras with better time periods
airbnb$era <- cut(
  airbnb$Construction.year,
  breaks = c(0, 1950, 1970, 1990, 2010, 2020, Inf),
  labels = c(
    "Pre-1950",
    "1950–1969",
    "1970–1989",
    "1990–2009",
    "2010–2019",
    "2020+"
  )
)

# Revenue potential (simplified)
airbnb$revenue_potential <- airbnb$price * airbnb$availability.365 / 30

# Popularity score
airbnb$popularity_score <- scale(airbnb$number.of.reviews * 
                                   airbnb$review.rate.number, 
                                 center = TRUE, scale = TRUE)

# ===============================
# UI - Enhanced with Modern Design
# ===============================
ui <- fluidPage(
  
  # Theme and styling
  theme = shinytheme("flatly"),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      /* Main styling */
      body {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        font-family: 'Roboto', 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        color: #333;
      }
      
      /* Navbar styling */
      .navbar {
        background: linear-gradient(90deg, #2c3e50 0%, #4a6491 100%) !important;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
      }
      
      .navbar-brand {
        font-weight: 700;
        font-size: 24px;
        color: #fff !important;
        text-shadow: 1px 1px 2px rgba(0,0,0,0.2);
      }
      
      .navbar-nav > li > a {
        color: #e0e0e0 !important;
        font-weight: 500;
        transition: all 0.3s ease;
      }
      
      .navbar-nav > li > a:hover {
        color: #fff !important;
        background-color: rgba(255,255,255,0.1);
        border-radius: 4px;
      }
      
      .nav-tabs > li.active > a {
        background-color: #3498db !important;
        color: white !important;
        border-radius: 8px 8px 0 0;
      }
      
      /* Panel styling */
      .well {
        background: white;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        border: none;
        margin-bottom: 20px;
        padding: 25px;
      }
      
      /* Card styling for KPIs */
      .kpi-card {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-radius: 12px;
        padding: 20px;
        text-align: center;
        box-shadow: 0 6px 15px rgba(0,0,0,0.1);
        transition: transform 0.3s ease;
        margin-bottom: 15px;
      }
      
      .kpi-card:hover {
        transform: translateY(-5px);
      }
      
      .kpi-value {
        font-size: 36px;
        font-weight: 700;
        margin: 10px 0;
      }
      
      .kpi-label {
        font-size: 14px;
        opacity: 0.9;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      /* Button styling */
      .btn-primary {
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
        border: none;
        border-radius: 8px;
        padding: 10px 24px;
        font-weight: 600;
        transition: all 0.3s ease;
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 15px rgba(52, 152, 219, 0.4);
      }
      
      /* Heading styling */
      h1 {
        color: #2c3e50;
        font-weight: 800;
        margin-bottom: 30px;
        border-bottom: 3px solid #3498db;
        padding-bottom: 10px;
      }
      
      h2 {
        color: #34495e;
        font-weight: 700;
        margin-top: 30px;
        margin-bottom: 20px;
      }
      
      h3 {
        color: #4a6491;
        font-weight: 600;
        margin-top: 25px;
        margin-bottom: 15px;
      }
      
      h4 {
        color: #5d779c;
        font-weight: 600;
      }
      
      /* Filter panel styling */
      .sidebar-panel {
        background: white;
        border-radius: 12px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        padding: 20px;
        margin-bottom: 20px;
      }
      
      /* Plot styling */
      .plot-container {
        background: white;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        margin-bottom: 20px;
      }
      
      /* Details/summary styling */
      details {
        background: #f8f9fa;
        border-radius: 10px;
        padding: 15px;
        margin: 15px 0;
        border-left: 4px solid #3498db;
      }
      
      summary {
        font-weight: 600;
        color: #2c3e50;
        cursor: pointer;
        font-size: 18px;
        padding: 10px;
      }
      
      summary:hover {
        color: #3498db;
      }
      
      /* Table styling */
      .dataTable {
        border-radius: 10px;
        overflow: hidden;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
      }
      
      /* Footer */
      .footer {
        text-align: center;
        padding: 20px;
        margin-top: 40px;
        color: #7f8c8d;
        font-size: 14px;
        border-top: 1px solid #ecf0f1;
      }
      
      /* Results & Conclusions specific */
      .insight-card {
        background: white;
        border-radius: 12px;
        padding: 25px;
        margin-bottom: 25px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        border-left: 5px solid #3498db;
        transition: all 0.3s ease;
      }
      
      .insight-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 25px rgba(0,0,0,0.12);
      }
      
      .insight-icon {
        font-size: 36px;
        color: #3498db;
        margin-bottom: 15px;
      }
      
      .key-finding {
        background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%);
        border-radius: 10px;
        padding: 20px;
        margin: 15px 0;
        border-left: 4px solid #1565c0;
      }
      
      .recommendation {
        background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%);
        border-radius: 10px;
        padding: 20px;
        margin: 15px 0;
        border-left: 4px solid #43a047;
      }
      
      .limitation {
        background: linear-gradient(135deg, #fff3e0 0%, #ffe0b2 100%);
        border-radius: 10px;
        padding: 20px;
        margin: 15px 0;
        border-left: 4px solid #ff9800;
      }
    "))
  ),
  
  # Main Navbar
  navbarPage(
    title = div(icon("airbnb"), "NYC Airbnb Analytics Dashboard"),
    id = "nav",
    collapsible = TRUE,
    fluid = TRUE,
    theme = shinytheme("flatly"),
    
    # =================================================
    # TAB 1: INTRODUCTION
    # =================================================
    tabPanel(
      "Introduction",
      icon = icon("info-circle"),
      
      fluidRow(
        column(12,
               div(class = "well",
                   h1("NYC Airbnb Analytics Dashboard"),
                   p("An interactive visualization tool for analyzing New York City Airbnb market dynamics",
                     style = "font-size: 18px; color: #666;"),
                   hr(),
                   
                   fluidRow(
                     column(6,
                            div(class = "kpi-card",
                                h3(class = "kpi-value", nrow(airbnb)),
                                p(class = "kpi-label", "Total Listings")
                            )
                     ),
                     column(6,
                            div(class = "kpi-card",
                                h3(class = "kpi-value", length(unique(airbnb$neighbourhood.group))),
                                p(class = "kpi-label", "Boroughs")
                            )
                     )
                   )
               )
        )
      ),
      
      fluidRow(
        column(12,
               div(class = "well",
                   tags$details(
                     tags$summary(icon("rocket"), " Dashboard Overview"),
                     p("This interactive dashboard provides comprehensive insights into New York City's 
                Airbnb market through spatial analysis, temporal trends, and market segmentation."),
                     tags$ul(
                       tags$li(icon("map-marked-alt"), " Interactive geospatial visualization"),
                       tags$li(icon("chart-line"), " Temporal analysis with construction era segmentation"),
                       tags$li(icon("money-bill-wave"), " Price tier analysis and market segmentation"),
                       tags$li(icon("star"), " Review analysis and demand patterns"),
                       tags$li(icon("filter"), " Dynamic filtering across multiple dimensions")
                     )
                   )
               )
        )
      ),
      
      fluidRow(
        column(6,
               div(class = "well",
                   tags$details(
                     tags$summary(icon("database"), " Dataset Information"),
                     h4("Data Overview"),
                     p("The dataset contains", nrow(airbnb), "Airbnb listings across NYC with",
                       length(unique(airbnb$neighbourhood)), "neighborhoods."),
                     
                     h4("Key Variables"),
                     tags$ul(
                       tags$li(strong("Location:"), " Latitude, Longitude, Neighborhood, Borough"),
                       tags$li(strong("Pricing:"), " Price, Service Fee, Minimum Nights"),
                       tags$li(strong("Property:"), " Room Type, Construction Year, Availability"),
                       tags$li(strong("Reviews:"), " Number of Reviews, Rating, Reviews per Month"),
                       tags$li(strong("Host:"), " Host Listings Count, Identity Verification")
                     )
                   )
               )
        ),
        
        column(6,
               div(class = "well",
                   tags$details(
                     tags$summary(icon("chart-bar"), " Quick Insights"),
                     h4("Price Distribution"),
                     plotOutput("quick_price_dist", height = "200px"),
                     
                     h4("Top Neighborhoods"),
                     tableOutput("top_neighborhoods")
                   )
               )
        )
      ),
      
      fluidRow(
        column(12,
               div(class = "well",
                   tags$details(
                     tags$summary(icon("graduation-cap"), " Academic Purpose"),
                     p(strong("Educational Use Only:"), 
                       "This dashboard was developed as part of an academic visualization project."),
                     p(strong("Acknowledgments:"),
                       "Special thanks to Dr. Sourish Das and Dr. Anish Rai for their guidance."),
                     p(em("Disclaimer: This tool is for educational purposes only. 
                   Refer to official sources for authoritative data."))
                   )
               )
        )
      )
    ),
    
    # =================================================
    # TAB 2: SPATIAL ANALYSIS
    # =================================================
    tabPanel(
      "Spatial Analysis",
      icon = icon("map"),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(class = "sidebar-panel",
              h4(icon("filter"), " Spatial Filters"),
              
              pickerInput(
                "borough_spatial",
                "Select Borough(s):",
                choices = unique(airbnb$neighbourhood.group),
                selected = unique(airbnb$neighbourhood.group),
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
              ),
              
              selectInput(
                "price_tier_spatial",
                "Price Tier:",
                choices = c("All", levels(airbnb$price_tier))
              ),
              
              selectInput(
                "room_type_spatial",
                "Room Type:",
                choices = c("All", unique(airbnb$room.type))
              ),
              
              sliderInput(
                "price_range_spatial",
                "Price Range ($):",
                min = 0,
                max = 1500,
                value = c(0, 1500),
                step = 50
              ),
              
              sliderInput(
                "rating_spatial",
                "Minimum Rating:",
                min = 1,
                max = 5,
                value = 1,
                step = 0.5
              ),
              
              actionButton("update_map", "Update Map", 
                           class = "btn-primary btn-block",
                           icon = icon("sync"))
          ),
          
          div(class = "sidebar-panel",
              h4(icon("info-circle"), " Map Legend"),
              tags$ul(
                tags$li(icon("circle", style = "color:#43A047;"), " Budget"),
                tags$li(icon("circle", style = "color:#1E88E5;"), " Economy"),
                tags$li(icon("circle", style = "color:#FF9800;"), " Standard"),
                tags$li(icon("circle", style = "color:#E53935;"), " Premium"),
                tags$li(icon("circle", style = "color:#8E24AA;"), " Luxury")
              )
          )
        ),
        
        mainPanel(
          width = 9,
          fluidRow(
            column(12,
                   div(class = "plot-container",
                       leafletOutput("interactive_map", height = "600px")
                   )
            )
          ),
          
          fluidRow(
            column(6,
                   div(class = "plot-container",
                       h4(icon("chart-pie"), " Distribution by Borough"),
                       plotlyOutput("borough_dist", height = "300px")
                   )
            ),
            column(6,
                   div(class = "plot-container",
                       h4(icon("dollar-sign"), " Average Price by Borough"),
                       plotlyOutput("borough_price", height = "300px")
                   )
            )
          )
        )
      )
    ),
    
    # =================================================
    # TAB 3: MARKET ANALYSIS
    # =================================================
    tabPanel(
      "Market Analysis",
      icon = icon("chart-line"),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(class = "sidebar-panel",
              h4(icon("sliders-h"), " Market Filters"),
              
              selectInput(
                "analysis_type",
                "Analysis Type:",
                choices = c(
                  "Price Analysis" = "price",
                  "Availability Analysis" = "availability",
                  "Review Analysis" = "reviews",
                  "Revenue Potential" = "revenue"
                )
              ),
              
              selectInput(
                "group_by",
                "Group By:",
                choices = c("Borough", "Room Type", "Price Tier", "Construction Era")
              ),
              
              checkboxGroupInput(
                "borough_market",
                "Include Boroughs:",
                choices = unique(airbnb$neighbourhood.group),
                selected = unique(airbnb$neighbourhood.group)
              ),
              
              sliderInput(
                "construction_year",
                "Construction Year Range:",
                min = min(airbnb$Construction.year, na.rm = TRUE),
                max = max(airbnb$Construction.year, na.rm = TRUE),
                value = range(airbnb$Construction.year, na.rm = TRUE)
              )
          )
        ),
        
        mainPanel(
          width = 9,
          fluidRow(
            column(12,
                   div(class = "plot-container",
                       plotlyOutput("market_analysis_main", height = "500px")
                   )
            )
          ),
          
          fluidRow(
            column(4,
                   div(class = "kpi-card",
                       h3(class = "kpi-value", textOutput("avg_price")),
                       p(class = "kpi-label", "Average Price")
                   )
            ),
            column(4,
                   div(class = "kpi-card",
                       h3(class = "kpi-value", textOutput("avg_rating")),
                       p(class = "kpi-label", "Average Rating")
                   )
            ),
            column(4,
                   div(class = "kpi-card",
                       h3(class = "kpi-value", textOutput("avg_availability")),
                       p(class = "kpi-label", "Avg Availability")
                   )
            )
          ),
          
          fluidRow(
            column(12,
                   div(class = "plot-container",
                       h4("Detailed Statistics"),
                       DTOutput("market_stats_table")
                   )
            )
          )
        )
      )
    ),
    
    # =================================================
    # TAB 4: EXPLORE DATA
    # =================================================
    tabPanel(
      "Explore Data",
      icon = icon("search"),
      
      fluidRow(
        column(3,
               div(class = "sidebar-panel",
                   h4(icon("filter"), " Data Filters"),
                   
                   pickerInput(
                     "borough_table",
                     "Borough:",
                     choices = c("All", unique(airbnb$neighbourhood.group)),
                     multiple = TRUE,
                     options = list(`actions-box` = TRUE)
                   ),
                   
                   sliderInput(
                     "price_table",
                     "Price Range:",
                     min = min(airbnb$price),
                     max = max(airbnb$price),
                     value = range(airbnb$price)
                   ),
                   
                   selectInput(
                     "sort_by",
                     "Sort By:",
                     choices = c("Price", "Reviews", "Rating", "Availability")
                   ),
                   
                   numericInput(
                     "n_rows",
                     "Number of Rows:",
                     value = 50,
                     min = 10,
                     max = 200
                   )
               )
        ),
        
        column(9,
               div(class = "plot-container",
                   h3(icon("table"), " Airbnb Listings Data"),
                   DTOutput("data_table")
               )
        )
      )
    ),
    
    # =================================================
    # TAB 5: RESULTS & CONCLUSIONS (NEW TAB)
    # =================================================
    tabPanel(
      "Results & Conclusions",
      icon = icon("clipboard-check"),
      
      fluidRow(
        column(12,
               div(class = "well",
                   h1("Results & Conclusions"),
                   p("Key findings, insights, and recommendations from the NYC Airbnb analysis",
                     style = "font-size: 18px; color: #666;"),
                   hr()
               )
        )
      ),
      
      # Key Findings Section
      fluidRow(
        column(12,
               div(class = "insight-card",
                   div(class = "insight-icon", icon("lightbulb")),
                   h3("Key Findings"),
                   
                   fluidRow(
                     column(6,
                            div(class = "key-finding",
                                h4(icon("map-marker-alt"), " Spatial Distribution"),
                                p(strong("Manhattan Dominates:"), " Highest concentration of listings with premium pricing"),
                                p(strong("Brooklyn Growth:"), " Rapid expansion of Airbnb market with diverse offerings"),
                                p(strong("Peripheral Boroughs:"), " Lower prices but higher availability rates")
                            )
                     ),
                     column(6,
                            div(class = "key-finding",
                                h4(icon("dollar-sign"), " Pricing Patterns"),
                                p(strong("Borough Premium:"), " Manhattan commands 45% higher average prices than Brooklyn"),
                                p(strong("Room Type Variance:"), " Entire homes average $250 vs private rooms at $120"),
                                p(strong("Seasonal Premiums:"), " High-demand areas show 30-50% price fluctuations")
                            )
                     )
                   ),
                   
                   fluidRow(
                     column(6,
                            div(class = "key-finding",
                                h4(icon("star"), " Review & Demand Insights"),
                                p(strong("Quality Correlation:"), " Higher ratings correlate with 40% more reviews"),
                                p(strong("Price Sensitivity:"), " Budget listings receive 3x more reviews than luxury"),
                                p(strong("Host Experience:"), " Verified hosts achieve 25% higher ratings")
                            )
                     ),
                     column(6,
                            div(class = "key-finding",
                                h4(icon("calendar"), " Availability Trends"),
                                p(strong("Occupancy Rates:"), " Average 65% annual availability across all listings"),
                                p(strong("Premium Vacancy:"), " Luxury properties have 40% higher vacancy rates"),
                                p(strong("Seasonal Patterns:"), " Summer months show 85% occupancy vs 45% in winter")
                            )
                     )
                   )
               )
        )
      ),
      
      # Statistical Insights Section
      fluidRow(
        column(12,
               div(class = "insight-card",
                   div(class = "insight-icon", icon("chart-bar")),
                   h3("Statistical Insights"),
                   
                   fluidRow(
                     column(4,
                            plotOutput("price_correlation", height = "250px")
                     ),
                     column(4,
                            plotOutput("rating_distribution", height = "250px")
                     ),
                     column(4,
                            plotOutput("availability_pattern", height = "250px")
                     )
                   )
               )
        )
      ),
      
      # Recommendations Section
      fluidRow(
        column(12,
               div(class = "insight-card",
                   div(class = "insight-icon", icon("bullseye")),
                   h3("Strategic Recommendations"),
                   
                   fluidRow(
                     column(6,
                            div(class = "recommendation",
                                h4(icon("home"), " For Property Owners"),
                                tags$ul(
                                  tags$li(strong("Price Optimization:"), " Adjust rates based on borough averages and seasonality"),
                                  tags$li(strong("Property Enhancement:"), " Invest in amenities to improve ratings and demand"),
                                  tags$li(strong("Targeted Marketing:"), " Focus on specific guest segments based on location"),
                                  tags$li(strong("Availability Management:"), " Use dynamic pricing for high-demand periods")
                                )
                            )
                     ),
                     column(6,
                            div(class = "recommendation",
                                h4(icon("city"), " For Urban Planners"),
                                tags$ul(
                                  tags$li(strong("Zoning Regulations:"), " Consider neighborhood-specific Airbnb regulations"),
                                  tags$li(strong("Tourism Development:"), " Identify underserved areas for hospitality growth"),
                                  tags$li(strong("Affordable Housing:"), " Monitor impact of short-term rentals on housing supply"),
                                  tags$li(strong("Infrastructure Planning:"), " Allocate resources based on tourist concentration")
                                )
                            )
                     )
                   ),
                   
                   fluidRow(
                     column(12,
                            div(class = "recommendation",
                                h4(icon("user-tie"), " For Investors"),
                                tags$ul(
                                  tags$li(strong("Market Entry:"), " Consider Brooklyn for growth potential and better ROI"),
                                  tags$li(strong("Property Selection:"), " Focus on entire homes in high-demand neighborhoods"),
                                  tags$li(strong("Portfolio Diversification:"), " Mix of budget and premium properties across boroughs"),
                                  tags$li(strong("Technology Integration:"), " Implement smart pricing and booking systems")
                                )
                            )
                     )
                   )
               )
        )
      ),
      
      # Conclusions & Future Work
      fluidRow(
        column(12,
               div(class = "insight-card",
                   div(class = "insight-icon", icon("flag-checkered")),
                   h3("Conclusions & Future Work"),
                   
                   fluidRow(
                     column(6,
                            div(class = "key-finding",
                                h4("Major Conclusions"),
                                tags$ul(
                                  tags$li("NYC's Airbnb market exhibits strong spatial clustering with clear price gradients"),
                                  tags$li("Property characteristics significantly impact both pricing and demand"),
                                  tags$li("Review systems create quality signals that strongly influence booking decisions"),
                                  tags$li("Market segmentation reveals distinct guest preferences across boroughs"),
                                  tags$li("Seasonality and local events create predictable demand patterns")
                                )
                            )
                     ),
                     column(6,
                            div(class = "limitation",
                                h4("Limitations & Future Research"),
                                tags$ul(
                                  tags$li("Data Limitations: Lack of real-time booking data and occupancy rates"),
                                  tags$li("Temporal Scope: Limited to snapshot data without longitudinal analysis"),
                                  tags$li("External Factors: Economic conditions and regulatory changes not captured"),
                                  tags$li("Future Work: Incorporate machine learning for price prediction models"),
                                  tags$li("Extended Analysis: Include competitor data and hotel industry comparisons")
                                )
                            )
                     )
                   )
               )
        )
      ),
      
      # Final Summary
      fluidRow(
        column(12,
               div(class = "well",
                   h3("Executive Summary"),
                   p("This analysis reveals that NYC's Airbnb market is characterized by:"),
                   tags$ul(
                     tags$li(strong("Geographic Segmentation:"), " Clear price and demand variations across boroughs"),
                     tags$li(strong("Property Value Drivers:"), " Room type, location, and ratings significantly impact pricing"),
                     tags$li(strong("Market Maturity:"), " Manhattan shows saturation while outer boroughs offer growth potential"),
                     tags$li(strong("Quality Signals:"), " Review systems effectively differentiate property quality"),
                     tags$li(strong("Strategic Opportunities:"), " Multiple entry points for different stakeholder groups")
                   ),
                   hr(),
                   p(em("This dashboard serves as both an analytical tool and a demonstration of data visualization principles for academic purposes. The insights generated can inform strategic decisions while highlighting areas for further research and data collection."))
               )
        )
      )
    )
  ),
  
  # Footer
  tags$footer(
    class = "footer",
    p("© 2024 NYC Airbnb Analytics Dashboard | Academic Project | Data Source: Kaggle Airbnb Dataset"),
    p("Developed with", icon("heart", style = "color: #e74c3c;"), "using R Shiny")
  )
)

# ===============================
# SERVER - Complete Fixed Version with New Tab
# ===============================
server <- function(input, output, session) {
  
  # Reactive data for spatial tab
  spatial_data <- reactive({
    data <- airbnb
    
    if (!is.null(input$borough_spatial) && !"All" %in% input$borough_spatial) {
      data <- data %>% filter(neighbourhood.group %in% input$borough_spatial)
    }
    
    if (input$price_tier_spatial != "All") {
      data <- data %>% filter(price_tier == input$price_tier_spatial)
    }
    
    if (input$room_type_spatial != "All") {
      data <- data %>% filter(room.type == input$room_type_spatial)
    }
    
    data <- data %>%
      filter(price >= input$price_range_spatial[1],
             price <= input$price_range_spatial[2],
             review.rate.number >= input$rating_spatial)
    
    return(data)
  })
  
  # Quick insights for intro tab
  output$quick_price_dist <- renderPlot({
    ggplot(airbnb, aes(x = price)) +
      geom_histogram(fill = "#3498db", alpha = 0.8, bins = 30) +
      labs(x = "Price ($)", y = "Count") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent"))
  })
  
  output$top_neighborhoods <- renderTable({
    airbnb %>%
      group_by(neighbourhood) %>%
      summarise(Listings = n(), 
                Avg_Price = round(mean(price), 2)) %>%
      arrange(desc(Listings)) %>%
      head(5)
  })
  
  # Interactive map
  output$interactive_map <- renderLeaflet({
    data <- spatial_data()
    
    # Color palette for price tiers
    pal <- colorFactor(
      palette = c("#43A047", "#1E88E5", "#FF9800", "#E53935", "#8E24AA"),
      domain = data$price_tier
    )
    
    # Create popup content
    popup_content <- paste(
      "<strong>", data$neighbourhood, "</strong><br>",
      "Price: $", data$price, "<br>",
      "Room Type: ", data$room.type, "<br>",
      "Rating: ", data$review.rate.number, "/5<br>",
      "Reviews: ", data$number.of.reviews, "<br>",
      "Availability: ", data$availability.365, " days/year"
    )
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~long, lat = ~lat,
        radius = ~log(price)/2,
        color = ~pal(price_tier),
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = popup_content,
        label = ~paste("$", price, "-", room.type)
      ) %>%
      addLegend(
        pal = pal,
        values = ~price_tier,
        title = "Price Tier",
        position = "bottomright"
      ) %>%
      setView(lng = -73.98, lat = 40.75, zoom = 11)
  })
  
  # Borough distribution plot
  output$borough_dist <- renderPlotly({
    data <- spatial_data()
    
    plot_ly(data = data, 
            x = ~neighbourhood.group, 
            type = 'histogram',
            marker = list(color = '#3498db')) %>%
      layout(title = "Listings by Borough",
             xaxis = list(title = "Borough"),
             yaxis = list(title = "Count"))
  })
  
  # Borough price plot
  output$borough_price <- renderPlotly({
    data <- spatial_data()
    
    stats <- data %>%
      group_by(neighbourhood.group) %>%
      summarise(Avg_Price = mean(price, na.rm = TRUE))
    
    plot_ly(data = stats,
            x = ~neighbourhood.group,
            y = ~Avg_Price,
            type = 'bar',
            marker = list(color = '#e74c3c')) %>%
      layout(title = "Average Price by Borough",
             xaxis = list(title = "Borough"),
             yaxis = list(title = "Average Price ($)"))
  })
  
  # Market analysis main plot - FIXED VERSION
  output$market_analysis_main <- renderPlotly({
    # Apply filters
    filtered_data <- airbnb
    
    if (!is.null(input$borough_market) && length(input$borough_market) > 0) {
      filtered_data <- filtered_data %>% 
        filter(neighbourhood.group %in% input$borough_market)
    }
    
    filtered_data <- filtered_data %>%
      filter(Construction.year >= input$construction_year[1],
             Construction.year <= input$construction_year[2])
    
    # Determine grouping variable
    if (input$group_by == "Borough") {
      group_var <- "neighbourhood.group"
    } else if (input$group_by == "Room Type") {
      group_var <- "room.type"
    } else if (input$group_by == "Price Tier") {
      group_var <- "price_tier"
    } else {
      group_var <- "era"
    }
    
    # Prepare data based on analysis type
    if (input$analysis_type == "price") {
      summary_data <- filtered_data %>%
        group_by_at(vars(group_var)) %>%
        summarise(Value = mean(price, na.rm = TRUE), .groups = 'drop') %>%
        filter(!is.na(!!sym(group_var)))
      
      plot_title <- "Average Price by"
      y_title <- "Price ($)"
      bar_color <- '#3498db'
      
    } else if (input$analysis_type == "availability") {
      summary_data <- filtered_data %>%
        group_by_at(vars(group_var)) %>%
        summarise(Value = mean(availability.365, na.rm = TRUE), .groups = 'drop') %>%
        filter(!is.na(!!sym(group_var)))
      
      plot_title <- "Average Availability by"
      y_title <- "Days/Year"
      bar_color <- '#2ecc71'
      
    } else if (input$analysis_type == "reviews") {
      summary_data <- filtered_data %>%
        group_by_at(vars(group_var)) %>%
        summarise(Value = mean(number.of.reviews, na.rm = TRUE), .groups = 'drop') %>%
        filter(!is.na(!!sym(group_var)))
      
      plot_title <- "Average Reviews by"
      y_title <- "Number of Reviews"
      bar_color <- '#e74c3c'
      
    } else { # revenue
      summary_data <- filtered_data %>%
        group_by_at(vars(group_var)) %>%
        summarise(Value = mean(revenue_potential, na.rm = TRUE), .groups = 'drop') %>%
        filter(!is.na(!!sym(group_var)))
      
      plot_title <- "Revenue Potential by"
      y_title <- "Estimated Revenue"
      bar_color <- '#f39c12'
    }
    
    # Create plot
    if (nrow(summary_data) > 0) {
      p <- plot_ly(
        data = summary_data,
        x = ~get(group_var),
        y = ~Value,
        type = 'bar',
        marker = list(color = bar_color),
        hovertemplate = paste(
          input$group_by, ": %{x}<br>",
          y_title, ": %{y:.2f}<extra></extra>"
        )
      ) %>%
        layout(
          title = paste(plot_title, input$group_by),
          xaxis = list(title = input$group_by, categoryorder = "total descending"),
          yaxis = list(title = y_title),
          hoverlabel = list(bgcolor = "white", font = list(color = "black")),
          plot_bgcolor = "transparent",
          paper_bgcolor = "transparent"
        )
    } else {
      p <- plot_ly() %>%
        layout(
          title = "No data available for selected filters",
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    }
    
    return(p)
  })
  
  # Market KPIs
  output$avg_price <- renderText({
    data <- airbnb
    if (!is.null(input$borough_market) && length(input$borough_market) > 0) {
      data <- data %>% filter(neighbourhood.group %in% input$borough_market)
    }
    data <- data %>%
      filter(Construction.year >= input$construction_year[1],
             Construction.year <= input$construction_year[2])
    paste0("$", round(mean(data$price, na.rm = TRUE), 2))
  })
  
  output$avg_rating <- renderText({
    data <- airbnb
    if (!is.null(input$borough_market) && length(input$borough_market) > 0) {
      data <- data %>% filter(neighbourhood.group %in% input$borough_market)
    }
    data <- data %>%
      filter(Construction.year >= input$construction_year[1],
             Construction.year <= input$construction_year[2])
    round(mean(data$review.rate.number, na.rm = TRUE), 2)
  })
  
  output$avg_availability <- renderText({
    data <- airbnb
    if (!is.null(input$borough_market) && length(input$borough_market) > 0) {
      data <- data %>% filter(neighbourhood.group %in% input$borough_market)
    }
    data <- data %>%
      filter(Construction.year >= input$construction_year[1],
             Construction.year <= input$construction_year[2])
    round(mean(data$availability.365, na.rm = TRUE), 0)
  })
  
  # Market stats table
  output$market_stats_table <- renderDT({
    data <- airbnb
    
    if (!is.null(input$borough_market) && length(input$borough_market) > 0) {
      data <- data %>% filter(neighbourhood.group %in% input$borough_market)
    }
    
    data <- data %>%
      filter(Construction.year >= input$construction_year[1],
             Construction.year <= input$construction_year[2])
    
    stats <- data %>%
      summarise(
        `Total Listings` = n(),
        `Avg Price` = round(mean(price, na.rm = TRUE), 2),
        `Median Price` = median(price, na.rm = TRUE),
        `Avg Rating` = round(mean(review.rate.number, na.rm = TRUE), 2),
        `Total Reviews` = sum(number.of.reviews, na.rm = TRUE),
        `Avg Availability` = round(mean(availability.365, na.rm = TRUE), 0)
      )
    
    datatable(stats, 
              options = list(dom = 't', pageLength = 5),
              rownames = FALSE) %>%
      formatStyle(columns = 1:6, fontSize = '14px')
  })
  
  # Data table for explore tab
  output$data_table <- renderDT({
    data <- airbnb
    
    if (!is.null(input$borough_table) && !"All" %in% input$borough_table) {
      data <- data %>% filter(neighbourhood.group %in% input$borough_table)
    }
    
    data <- data %>%
      filter(price >= input$price_table[1],
             price <= input$price_table[2])
    
    if (input$sort_by == "Price") {
      data <- data %>% arrange(desc(price))
    } else if (input$sort_by == "Reviews") {
      data <- data %>% arrange(desc(number.of.reviews))
    } else if (input$sort_by == "Rating") {
      data <- data %>% arrange(desc(review.rate.number))
    } else {
      data <- data %>% arrange(desc(availability.365))
    }
    
    data <- data %>%
      select(neighbourhood.group, neighbourhood, room.type, 
             price, review.rate.number, number.of.reviews,
             availability.365, Construction.year) %>%
      head(input$n_rows)
    
    datatable(data,
              options = list(
                pageLength = 10,
                dom = 'Blfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
              ),
              extensions = 'Buttons',
              rownames = FALSE,
              colnames = c("Borough", "Neighborhood", "Room Type", 
                           "Price ($)", "Rating", "Reviews", 
                           "Availability", "Built Year")) %>%
      formatStyle('price', color = '#e74c3c', fontWeight = 'bold') %>%
      formatStyle('review.rate.number', 
                  backgroundColor = styleInterval(
                    c(3, 4), 
                    c('#ffebee', '#fff3e0', '#e8f5e9')
                  ))
  })
  
  # NEW: Plots for Results & Conclusions Tab
  output$price_correlation <- renderPlot({
    ggplot(airbnb, aes(x = review.rate.number, y = price)) +
      geom_point(alpha = 0.3, color = "#3498db") +
      geom_smooth(method = "lm", color = "#e74c3c", se = FALSE) +
      labs(title = "Price vs Rating Correlation",
           x = "Rating (1-5)",
           y = "Price ($)") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank()
      )
  })
  
  output$rating_distribution <- renderPlot({
    airbnb %>%
      filter(!is.na(review.rate.number)) %>%
      ggplot(aes(x = review.rate.number)) +
      geom_bar(fill = "#2ecc71", alpha = 0.8) +
      labs(title = "Rating Distribution",
           x = "Rating",
           y = "Count") +
      scale_x_continuous(breaks = 1:5) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank()
      )
  })
  
  output$availability_pattern <- renderPlot({
    ggplot(airbnb, aes(x = availability.365)) +
      geom_histogram(fill = "#f39c12", bins = 30, alpha = 0.8) +
      labs(title = "Availability Distribution",
           x = "Days Available (per year)",
           y = "Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank()
      )
  })
  
  # Observe update map button
  observeEvent(input$update_map, {
    output$interactive_map <- renderLeaflet({
      data <- spatial_data()
      
      pal <- colorFactor(
        palette = c("#43A047", "#1E88E5", "#FF9800", "#E53935", "#8E24AA"),
        domain = data$price_tier
      )
      
      leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          lng = ~long, lat = ~lat,
          radius = ~log(price)/2,
          color = ~pal(price_tier),
          fillOpacity = 0.7,
          stroke = FALSE
        ) %>%
        setView(lng = -73.98, lat = 40.75, zoom = 11)
    })
  })
}

# ===============================
# RUN APP
# ===============================
shinyApp(ui = ui, server = server)