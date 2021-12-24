# library setup
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(highcharter)
library(plotly)
library(glue)
library(DT)

airpass <- read_csv('airpass.csv')
airpass <- airpass[,-1]

cat <- c("Inflight Wifi Service",
         "Departure or Arrival Time Convenient",
         "Online Booking",
         "Gate Location",
         "Food and Drink",
         "Online Boarding",
         "Seat Comfort",
         "Inflight Entertainment",
         "On-board Service",
         "Leg Room Service",
         "Baggage Bandling",
         "Check-in Service",
         "Inflight Service",
         "Cleanliness")

reglog <- glm(as.factor(satisfaction)~Inflight.wifi.service+
                Departure.Arrival.time.convenient+
                Ease.of.Online.booking+
                Gate.location+
                Food.and.drink+
                Online.boarding+
                Seat.comfort+
                Inflight.entertainment+
                On.board.service+
                Leg.room.service+
                Baggage.handling+
                Checkin.service+
                Inflight.service+
                Cleanliness,
              family = "binomial",
              data=airpass)

reg_satis <- data.frame(coef=reglog$coefficients[-1])
rownames(reg_satis) <- cat