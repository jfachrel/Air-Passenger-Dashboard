header <- dashboardHeader(title = "US Airline Passenger")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Passenger", 
             tabName = "pass",
             icon = icon("users")),
    menuItem(text = "Satisfaction Level", 
             tabName = "satlev", 
             icon = icon("signal"),
             badgeLabel = "By Category",
             badgeColor = "green"),
    menuItem(text = "Satisfaction", 
             tabName = "satis", 
             icon = icon("smile")),
    menuItem(text = "Data",
             tabName = "dat", 
             icon = icon("database")),
    menuItem(text= "About",
             tabName = 'about',
             icon = icon("user-tie"))

    )
)
body <- dashboardBody(
  tabItems(
# Tab Passenger
    tabItem(tabName = "pass",
            fluidPage(
              fluidRow(
                infoBoxOutput("PassengerBox",width = 4),
                infoBoxOutput("male",width = 4),
                infoBoxOutput("female",width = 4)
              ),
              fluidRow(
                box(
                  width = 6,
                  title = "Flight Distance Histogram",
                  plotlyOutput(outputId = "dist_hist"),
                  background = "green",
                  solidHeader = TRUE
                ),
                box(
                  width = 6,
                  title = "Age Histogram",
                  plotlyOutput(outputId = "age_hist"),
                  background = "green",
                  solidHeader = TRUE
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  sliderInput(inputId = "dist",
                              label = "Set Flight Distance Range",
                              min = min(airpass$Flight.Distance),
                              max = max(airpass$Flight.Distance),
                              value = c(min(airpass$Flight.Distance),max(airpass$Flight.Distance)),
                              post = " Km"),
                  background = "navy",
                  solidHeader = TRUE
                )
              )
            )),
# Tab Level
    tabItem(tabName = "satlev",
            fluidPage(
              fluidRow(
                box(
                  width = 12,
                  plotlyOutput(outputId = "level"),
                  background = "green"
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  selectInput(
                    inputId="cat",
                    label="Select Category",
                    choices=cat),
                  background = "navy"
                ),
                box(
                  width = 6,
                  checkboxGroupInput(
                    inputId = "lev",
                    label = "Select Satisfaction Level",
                    choices = c(1,2,3,4,5),
                    selected = c(1,2,3,4,5),
                    inline = T),
                  background = "navy"
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  highchartOutput(outputId = "satis_cat"),
                  title = "Satisfaction Category",
                  background = "green",
                  solidHeader = TRUE,
                  align = "center"
                ),
                box(
                  width = 6,
                  highchartOutput(outputId = "cust"),
                  title = "Customer Type",
                  background = "green",
                  solidHeader = TRUE,
                  align = "center"
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  highchartOutput(outputId = "trav"),
                  title = "Type of Travel",
                  background = "green",
                  solidHeader = TRUE,
                  align = "center"
                ),
                box(
                  width = 6,
                  highchartOutput(outputId = "class"),
                  title = "Class Category",
                  background = "green",
                  solidHeader = TRUE,
                  align = "center"
                )
              )
            )
            ),
# Tab Satisfaction
    tabItem(tabName = "satis",
            fluidPage(
              fluidRow(
              box(
                width = 12,
                highchartOutput(outputId = "satisfaction"),
                title = "Categories That Affect Satisfaction",
                background = "green",
                solidHeader = TRUE,
                align = "center"
              )
              ),
              fluidRow(
                box(
                  width = 6,
                  title = "Highest Regression Coefficient",
                  background = "green",
                  solidHeader = TRUE,
                  infoBoxOutput("max",width = 12)
                ),
                box(
                  width = 6,
                  title = "Lowest Regression Coefficient",
                  background = "red",
                  solidHeader = TRUE,
                  infoBoxOutput("min",width = 12)
                )
              ),
              fluidRow(
                p(strong("Note: The regression coefficient shows how much influence the above categorical variables have on satisfaction"))
              )
              )
            ),
# Tab Data
    tabItem(tabName = "dat",
            fluidPage(
              fluidRow(
                h1('US Airline Passenger Dataset',align = "center")
              ),
              fluidRow(
                box(
                  width = 12,
                  dataTableOutput(outputId = "data"),
                  background = "green",
                )
              )
            )
            ),
# Tab About
    tabItem(tabName = "about",
            fluidPage(
              h1("US Airline Passenger Satisfaction Visualization"),
              h5("By ", a("Julio Fachrel", href = "https://www.linkedin.com/in/julio-fachrel")),
              h2("Dataset"),
              p("this dataset was taken from",a("Kaggle.", href = "https://www.kaggle.com/teejmahal20/airline-passenger-satisfaction")),
              h2("Library"),
              p("Several R library that used to create this visualization are:"),
              p("-  shiny"),
              p("-  shinydashboard"),
              p("-  tidyverse"),
              p("-  scales"),
              p("-  highcharter"),
              p("-  plotly"),
              p("-  glue"),
              p("-  DT"),
              h2("Code"),
              p("Find out my code in ", a("GitHub", href = "https://github.com/jfachrel/Air-Passenger-Dashboard"))
            )
            )
  )
  )
dashboardPage(skin = "green",
              header = header,
              body = body,
              sidebar = sidebar
)