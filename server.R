function(input, output) { 
## Tab Passenger
# Info Box
  output$PassengerBox <- renderInfoBox({
    infoBox(
      "TOTAL PASSENGER",
      comma(nrow(airpass),big.mark=","),
      icon=icon("users"),
      color = "green",
      fill = T
    )
  })
  output$male <- renderInfoBox({
    infoBox(
      "MALE",
      comma(nrow(airpass[airpass$Gender=="Male",]),big.mark=","),
      icon=icon("male"),
      color = "blue",
      fill = T
    )
  })
  output$female <- renderInfoBox({
    infoBox(
      "FEMALE",
      comma(nrow(airpass[airpass$Gender=="Female",]),big.mark=","),
      icon=icon("female"),
      color = "red",
      fill = T
    )
  })
# Fligh Distance Histogram
  output$dist_hist <- renderPlotly({
    dist_hist <- airpass %>% filter(Flight.Distance <= input$dist[2] & Flight.Distance >= input$dist[1]) %>% 
      ggplot(aes(Flight.Distance,
                 text=glue("Fligh Distance: {comma(Flight.Distance,big.mark=",")} Km")))+
      geom_histogram(fill="darkblue")+
      scale_x_continuous(labels=scales::comma_format(suffix = " Km",big.mark = '.'),
                         breaks = seq(0,5000,500)) +
      labs(y=NULL,
           x= NULL)+
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(dist_hist,tooltip = "text")
  })

# Age Distance Histogram
  output$age_hist <- renderPlotly({
    age_hist <- airpass %>% filter(Flight.Distance <= input$dist[2] & Flight.Distance >= input$dist[1]) %>% 
      ggplot(aes(Age,
                 text=glue("Age: {Age}")))+
      geom_histogram(fill="darkred")+
      labs(y=NULL,
           x= NULL)+
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(age_hist,tooltip = "text")
  })
## Tab Level
# Level Plot
  output$level <- renderPlotly({
    if(input$cat == "Inflight Wifi Service"){
      satis_level <- airpass %>% group_by(Inflight.wifi.service) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Inflight.wifi.service,y=freq,
                   text=glue("Satisfaction level: {Inflight.wifi.service}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of The Inflight Wifi Service",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else if(input$cat == "Departure or Arrival Time Convenient"){
      satis_level <- airpass %>% group_by(Departure.Arrival.time.convenient) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Departure.Arrival.time.convenient,y=freq,
                   text=glue("Satisfaction level: {Departure.Arrival.time.convenient}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of The Departure or Arrival Time Convenient",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else if(input$cat == "Online Booking"){
      satis_level <- airpass %>% group_by(Ease.of.Online.booking) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Ease.of.Online.booking,y=freq,
                   text=glue("Satisfaction level: {Ease.of.Online.booking}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of Online Booking",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else if(input$cat == "Gate Location"){
      satis_level <- airpass %>% group_by(Gate.location) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Gate.location,y=freq,
                   text=glue("Satisfaction level: {Gate.location}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of Gate Location",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else if(input$cat == "Food and Drink"){
      satis_level <- airpass %>% group_by(Food.and.drink) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Food.and.drink,y=freq,
                   text=glue("Satisfaction level: {Food.and.drink}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of Food and Drink",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else if(input$cat == "Online Boarding"){
      satis_level <- airpass %>% group_by(Online.boarding) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Online.boarding,y=freq,
                   text=glue("Satisfaction level: {Online.boarding}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of Online Boarding",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else if(input$cat == "Seat Comfort"){
      satis_level <- airpass %>% group_by(Seat.comfort) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Seat.comfort,y=freq,
                   text=glue("Satisfaction level: {Seat.comfort}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of Seat Comfort",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else if(input$cat == "Inflight Entertainment"){
      satis_level <- airpass %>% group_by(Inflight.entertainment) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Inflight.entertainment,y=freq,
                   text=glue("Satisfaction level: {Inflight.entertainment}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of Inflight Entertainment",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else if(input$cat == "On-board Service"){
      satis_level <- airpass %>% group_by(On.board.service) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=On.board.service,y=freq,
                   text=glue("Satisfaction level: {On.board.service}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of On-board Service",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else if(input$cat == "Leg Room Service"){
      satis_level <- airpass %>% group_by(Leg.room.service) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Leg.room.service,y=freq,
                   text=glue("Satisfaction level: {Leg.room.service}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of Leg Room Service",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else if(input$cat == "Baggage Bandling"){
      satis_level <- airpass %>% group_by(Baggage.handling) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Baggage.handling,y=freq,
                   text=glue("Satisfaction level: {Baggage.handling}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of Baggage Bandling",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else if(input$cat == "Check-in Service"){
      satis_level <- airpass %>% group_by(Checkin.service) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Checkin.service,y=freq,
                   text=glue("Satisfaction level: {Checkin.service}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of Check-in Service",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else if(input$cat == "Inflight Service"){
      satis_level <- airpass %>% group_by(Inflight.service) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Inflight.service,y=freq,
                   text=glue("Satisfaction level: {Inflight.service}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level of Inflight Service",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    else{
      satis_level <- airpass %>% group_by(Cleanliness) %>% 
        summarise(freq = n()) %>% 
        ggplot(aes(x=Cleanliness,y=freq,
                   text=glue("Satisfaction level: {Cleanliness}
                       Total Votes: {comma(freq,big.mark=",")}")))+
        geom_col(fill="darkred")+
        labs(title="Satisfaction Level ofr Cleanliness",
             y=NULL,
             x=NULL)+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
    ggplotly(satis_level,tooltip = "text")
  })
  
# Satisfaction Category Pie Chart
  output$satis_cat <- renderHighchart({
    if(input$cat=="Inflight Wifi Service"){
      SatisCat <- airpass %>% filter(Inflight.wifi.service %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Departure or Arrival Time Convenient"){
      SatisCat <- airpass %>% filter(Departure.Arrival.time.convenient %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Online Booking"){
      SatisCat <- airpass %>% filter(Ease.of.Online.booking %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Gate Location"){
      SatisCat <- airpass %>% filter(Gate.location %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Food and Drink"){
      SatisCat <- airpass %>% filter(Food.and.drink %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Online Boarding"){
      SatisCat <- airpass %>% filter(Online.boarding %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Seat Comfort"){
      SatisCat <- airpass %>% filter(Seat.comfort %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>%
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Inflight Entertainment"){
      SatisCat <- airpass %>% filter(Inflight.entertainment %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="On-board Service"){
      SatisCat <- airpass %>% filter(On.board.service %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Leg Room Service"){
      SatisCat <- airpass %>% filter(Leg.room.service %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Baggage Bandling"){
      SatisCat <- airpass %>% filter(Baggage.handling %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Check-in Service"){
      SatisCat <- airpass %>% filter(Checkin.service %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Inflight Service"){
      SatisCat <- airpass %>% filter(Inflight.service %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else{
      SatisCat <- airpass %>% filter(Cleanliness %in% (input$lev)) %>%
        group_by(satisfaction) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(satisfaction), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    SatisCat
  })
# Customer Type Pie chart    
  output$cust <- renderHighchart({
    if(input$cat=="Inflight Wifi Service"){
      CustType <- airpass %>% filter(Inflight.wifi.service %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Departure or Arrival Time Convenient"){
      CustType <- airpass %>% filter(Departure.Arrival.time.convenient %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Online Booking"){
      CustType <- airpass %>% filter(Ease.of.Online.booking %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Gate Location"){
      CustType <- airpass %>% filter(Gate.location %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Food and Drink"){
      CustType <- airpass %>% filter(Food.and.drink %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Online Boarding"){
      CustType <- airpass %>% filter(Online.boarding %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Seat Comfort"){
      CustType <- airpass %>% filter(Seat.comfort %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Inflight Entertainment"){
      CustType <- airpass %>% filter(Inflight.entertainment %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="On-board Service"){
      CustType <- airpass %>% filter(On.board.service %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Leg Room Service"){
      CustType <- airpass %>% filter(Leg.room.service %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Baggage Bandling"){
      CustType <- airpass %>% filter(Baggage.handling %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Check-in Service"){
      CustType <- airpass %>% filter(Checkin.service %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Inflight Service"){
      CustType <- airpass %>% filter(Inflight.service %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else{
      CustType <- airpass %>% filter(Cleanliness %in% (input$lev)) %>%
        group_by(Customer.Type) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Customer.Type), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    CustType
  })
  # Type of Travel Pie chart    
  output$trav <- renderHighchart({
    if(input$cat=="Inflight Wifi Service"){
      TravType <- airpass %>% filter(Inflight.wifi.service %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Departure or Arrival Time Convenient"){
      TravType <- airpass %>% filter(Departure.Arrival.time.convenient %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Online Booking"){
      TravType <- airpass %>% filter(Ease.of.Online.booking %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Gate Location"){
      TravType <- airpass %>% filter(Gate.location %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Food and Drink"){
      TravType <- airpass %>% filter(Food.and.drink %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Online Boarding"){
      TravType <- airpass %>% filter(Online.boarding %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Seat Comfort"){
      TravType <- airpass %>% filter(Seat.comfort %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Inflight Entertainment"){
      TravType <- airpass %>% filter(Inflight.entertainment %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="On-board Service"){
      TravType <- airpass %>% filter(On.board.service %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Leg Room Service"){
      TravType <- airpass %>% filter(Leg.room.service %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Baggage Bandling"){
      TravType <- airpass %>% filter(Baggage.handling %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Check-in Service"){
      TravType <- airpass %>% filter(Checkin.service %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Inflight Service"){
      TravType <- airpass %>% filter(Inflight.service %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else{
      TravType <- airpass %>% filter(Cleanliness %in% (input$lev)) %>%
        group_by(Type.of.Travel) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Type.of.Travel), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    TravType
  })
  # Class Pie chart    
  output$class <- renderHighchart({
    if(input$cat=="Inflight Wifi Service"){
      ClassType <- airpass %>% filter(Inflight.wifi.service %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>%
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Departure or Arrival Time Convenient"){
      ClassType <- airpass %>% filter(Departure.Arrival.time.convenient %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Online Booking"){
      ClassType <- airpass %>% filter(Ease.of.Online.booking %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Gate Location"){
      ClassType <- airpass %>% filter(Gate.location %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Food and Drink"){
      ClassType <- airpass %>% filter(Food.and.drink %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Online Boarding"){
      ClassType <- airpass %>% filter(Online.boarding %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Seat Comfort"){
      ClassType <- airpass %>% filter(Seat.comfort %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Inflight Entertainment"){
      ClassType <- airpass %>% filter(Inflight.entertainment %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="On-board Service"){
      ClassType <- airpass %>% filter(On.board.service %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Leg Room Service"){
      ClassType <- airpass %>% filter(Leg.room.service %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Baggage Bandling"){
      ClassType <- airpass %>% filter(Baggage.handling %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Check-in Service"){
      ClassType <- airpass %>% filter(Checkin.service %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else if(input$cat=="Inflight Service"){
      ClassType <- airpass %>% filter(Inflight.service %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    else{
      ClassType <- airpass %>% filter(Cleanliness %in% (input$lev)) %>%
        group_by(Class) %>% 
        summarise(freq=n()) %>%
        mutate(prop=round((freq/sum(freq))*100,2)) %>% 
        hchart(
          "pie", hcaes(x = str_to_title(Class), y = prop),
          name= "Propotion",
          tooltip=list(valueSuffix = "%")) %>% 
        hc_add_theme(hc_theme_elementary()) 
    }
    ClassType
  })
## Tab Satisfaction
# Polar Plot
  output$satisfaction <- renderHighchart({
    reg_plot <- highchart() %>%
      hc_chart(type = "line", polar = TRUE) %>% 
      hc_xAxis(categories = rownames(reg_satis)) %>% 
      hc_series(list(
        name = "Regression Coefficient",
        data = round(reg_satis$coef,2),
        pointPlacement = "on",
        type = "line",
        color = "steelblue",
        showInLegend = FALSE
      )) %>% 
      hc_add_theme(hc_theme_elementary())
    reg_plot
  })
# Info Box
  output$max <- renderInfoBox({
    infoBox(
      "ONLINE BOARDING",
      round(max(reg_satis),2),
      icon=icon("angle-double-up"),
      color = "green",
      fill = T
    )
  })
  output$min <- renderInfoBox({
    infoBox(
      "DEPARTURE OR ARRIVAL TIME CONVENIENT",
      round(min(reg_satis),2),
      icon=icon("angle-double-down"),
      color = "red",
      fill = T
    )
  })
## Tab Data  
  output$data <- renderDataTable({
    datatable(data = airpass, options = list(scrollX = T))
    })
  }