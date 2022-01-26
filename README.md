# US Airline Passenger Satisfaction Dashboard

This is an interactive dashboard powered by a shiny dashboard. Shiny is an open package from RStudio, which provides a web application framework to create interactive web applications called `Shiny apps`. the ease of working with Shiny has made it popular among R users.

This dashboard based on an open dataset from kaggle.com that contains an US Airline passenger satisfaction survey. There are several factors in the questions in this survey such as Gender, Type of Customer, Age, etc. Using this dashboard will help you to perform an airline passenger satisfaction analysis and draw some assumptions easily. Because Shiny provides automatic reactive binding between input and output, so you can set the input as you wish and with ease.

## What are in repo

- **airpass.csv** : The US Airline Passenger satisfaction dataset.
- **global.R** : R script for setting-up the apps’ environment, e.g., libraries, import and initial cleaning dataset.
- **Server.R** : R script for processing user’s input and render it into output.
- **ui.R** : User Interface (UI) for displaying input and output

Check here to see the dasboard :
https://jfachrel.shinyapps.io/Airline_Passenger/
