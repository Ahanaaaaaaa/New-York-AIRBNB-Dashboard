# New-York-AIRBNB-Dashboard
Interactive Airbnb Data Analysis Dashboard built with R Shiny for exploring pricing, availability, and listing trends.
An interactive R Shiny dashboard for exploring and analyzing Airbnb listing data. The dashboard provides visual insights into pricing patterns, availability, and listing characteristics using interactive charts and filters.

The application allows users to quickly explore Airbnb datasets and identify trends that may help understand the short-term rental market.

🔗 Live Dashboard:
https://ahanasen.shinyapps.io/AIRBNB_DASHBOARD/

Features

📊 Interactive Data Visualizations for Airbnb listings

💰 Price Distribution Analysis

📍 Location-based insights into listings

📅 Availability analysis of properties

🎛 User-friendly filters for dynamic exploration

⚡ Built using R Shiny for real-time interaction

Tech Stack

R

Shiny

ggplot2

dplyr

shinydashboard / shiny widgets

Project Structure
AIRBNB_DASHBOARD/
│
├── app.R                # Main Shiny application
├── data/                # Airbnb dataset
├── www/                 # Static assets (if any)
└── README.md            # Project documentation
How to Run Locally

Clone the repository

git clone https://github.com/your-username/AIRBNB_DASHBOARD.git

Open the project in RStudio

Install required packages

install.packages(c("shiny","ggplot2","dplyr","shinydashboard"))

Run the app

shiny::runApp()
Use Cases

This dashboard can be useful for:

Exploring Airbnb market trends

Understanding price distributions across listings

Analyzing availability patterns

Performing quick exploratory data analysis on Airbnb datasets

Future Improvements

Add geographic maps for listing visualization

Include advanced filtering options

Integrate predictive analytics for price estimation

Improve UI/UX for better user experience
