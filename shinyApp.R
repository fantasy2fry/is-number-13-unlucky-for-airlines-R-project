library(shiny)
library(ggplot2)
library(tidyr)
options(browser = "chromium")    # u mnie bez tej opcji shiny nie działa

#sidebarLayout
ui <- fluidPage(
  titlePanel("Czy liczba 13 jest nieszczęśliwa?"),
  sidebarLayout(
    sidebarPanel(
      selectInput("option1",
                  "Który atrybut nas interesuje?:",
                  choices = c("Opóźnienia", "Odwołania"),
                  selected = "Odwołania"),
      selectInput("option2",
                  "Gdzie szukamy liczby 13?:",
                  choices = c("W dniach", "W numerach lotów", "W numerach samolotów"),
                  selected = "W dniach")
    ),
    mainPanel(
      plotOutput("selected_plot")
    )
  )
)

server <- function(input, output) {
  
  output$selected_plot <- renderPlot({
    if (input$option1 == "Opóźnienia" & input$option2 == "W dniach") {
      opoznienia_bez_13_Day <- read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/opoznienia_bez_13_Day.csv")
      colnames(opoznienia_bez_13_Day)[4] <- "srednia"
      colnames(opoznienia_bez_13_Day)[1] <- "rok"
      
      opoznienia_z_13_Day <- read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/opoznienia_z_13_Day.csv")
      colnames(opoznienia_z_13_Day)[4] <- "srednia"
      colnames(opoznienia_z_13_Day)[1] <- "rok"
      
      #złączenie obydwu tabel
      opoznienia_13_day <- cbind(opoznienia_bez_13_Day, opoznienia_z_13_Day)
      colnames(opoznienia_13_day) <- c("rok", "suma_opoznien_bez_13", "ilosc_lotow_bez_13", "inny niż 13", "rok2", "suma_opoznen_z_13", "losc lotow_z_13", "13")
      
      wykres_opoznienia_13_day <- gather(opoznienia_13_day,"srednia_bez13","srednia_z13", c(4,8))
      
      #tworzenie wykresu
      ggplot(data = wykres_opoznienia_13_day,aes(x=rok,y=srednia_z13, color=srednia_bez13)) + 
        labs(y = "średnia opóźnień na lot [min]", title= "średnia spoźnień lotów zależnie od dnia", fill = "dzień wylotu: ", color = "dzien wylotu:") +
        geom_bar(stat = "identity",position = "dodge", fill="white", size=1.7, width=0.8, alpha=0.7)+
        #scale_fill_manual(values = c("white", "#E69F00"))+
        scale_color_manual(values = c("#999999", "#E69F00"))+
        scale_linetype_manual("dashed")+
        scale_x_continuous(breaks = seq(min(2003), max(2008), by = 1)) +
        coord_cartesian(ylim = c(0, 13)) +
        scale_y_continuous(breaks = seq(min(0), max(13), by = 1)) +
        theme(legend.position = "bottom")
      
      
      
    } else if (input$option1 == "Opóźnienia" & input$option2 == "W numerach lotów") {
      #wczytanie odpowiednich tabel
      opoznienia_bez_13_FlightNum <- read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/opoznienia_bez_13_FlightNum.csv")
      colnames(opoznienia_bez_13_FlightNum)[4] <- "srednia"
      colnames(opoznienia_bez_13_FlightNum)[1] <- "rok"
      
      opoznienia_z_13_FlightNum <- read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/opoznienia_z_13_FlightNum.csv")
      colnames(opoznienia_z_13_FlightNum)[4] <- "srednia"
      colnames(opoznienia_z_13_FlightNum)[1] <- "rok"
      
      #złączenie obydwu tabel
      opoznienia_13_FlightNum <- cbind(opoznienia_bez_13_FlightNum, opoznienia_z_13_FlightNum)
      colnames(opoznienia_13_FlightNum) <- c("rok", "suma_opoznien_bez_13", "ilosc_lotow_bez_13", "niezawierający numeru 13", "rok2", "suma_opoznen_z_13", "losc lotow_z_13", "zawierający numeru 13")
      
      wykres_opoznienia_13_FlightNum <- gather(opoznienia_13_FlightNum,"procent_bez13","procent_z13", c(4,8))
      
      #tworzenie wykresu
      ggplot(data = wykres_opoznienia_13_FlightNum,aes(x=rok,y=procent_z13, color=procent_bez13)) + 
        labs(y = "średnia opóźnień na lot [min]", title= "średnia spoźnień lotów zależnie od numeru lotu", fill = "numer lotu: ", color = "numer lotu:") +
        geom_bar(stat = "identity",position = "dodge", fill="white", size=1.7, width=0.8, alpha=0.7)+
        #scale_fill_manual(values = c("white", "#E69F00"))+
        scale_color_manual(values = c("#999999", "#E69F00"))+
        scale_linetype_manual("dashed")+
        scale_x_continuous(breaks = seq(min(2003), max(2008), by = 1)) +
        coord_cartesian(ylim = c(0, 13)) +
        scale_y_continuous(breaks = seq(min(0), max(13), by = 1)) +
        theme(legend.position = "bottom")
      
      
      
    } else if (input$option1 == "Opóźnienia" & input$option2 == "W numerach samolotów") {
      #wczytanie odpowiednich tabel
      opoznienia_bez_13_TailNum <- read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/opoznienia_bez_13_TailNum.csv")
      colnames(opoznienia_bez_13_TailNum)[4] <- "srednia"
      colnames(opoznienia_bez_13_TailNum)[1] <- "rok"
      
      opoznienia_z_13_TailNum <- read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/opoznienia_z_13_TailNum.csv")
      colnames(opoznienia_z_13_TailNum)[4] <- "srednia"
      colnames(opoznienia_z_13_TailNum)[1] <- "rok"
      
      #złączenie obydwu tabel
      opoznienia_13_TailNum <- cbind(opoznienia_bez_13_TailNum, opoznienia_z_13_TailNum)
      colnames(opoznienia_13_TailNum) <- c("rok", "suma_opoznien_bez_13", "ilosc_lotow_bez_13", "niezawierający numeru 13", "rok2", "suma_opoznen_z_13", "losc lotow_z_13", "zawierający numeru 13")
      
      wykres_opoznienia_13_TailNum <- gather(opoznienia_13_TailNum,"procent_bez13","procent_z13", c(4,8))
      
      #tworzenie wykresu
      ggplot(data = wykres_opoznienia_13_TailNum,aes(x=rok,y=procent_z13, color=procent_bez13)) + 
        labs(y = "średnia opóźnień na lot [min]", title= "średnia spoźnień lotów zależnie od identyfikatora samolotu", fill = "identyfikator samolotu: ", color = "identyfikator samolotu:") +
        geom_bar(stat = "identity",position = "dodge", fill="white", size=1.7, width=0.8, alpha=0.7)+
        #scale_fill_manual(values = c("white", "#E69F00"))+
        scale_color_manual(values = c("#999999", "#E69F00"))+
        scale_linetype_manual("dashed")+
        scale_x_continuous(breaks = seq(min(2003), max(2008), by = 1)) +
        coord_cartesian(ylim = c(0, 13)) +
        scale_y_continuous(breaks = seq(min(0), max(13), by = 1)) +
        theme(legend.position = "bottom")
      
      
      
    } else if (input$option1 == "Odwołania" & input$option2 == "W dniach") {
      
      odwolania_bez_13_Day <- read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_bez_13_Day.csv")
      colnames(odwolania_bez_13_Day)[4] <- "procent"
      colnames(odwolania_bez_13_Day)[1] <- "rok"
      odwolania_z_13_Day <- read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_z_13_Day.csv")
      colnames(odwolania_z_13_Day)[4] <- "procent"
      colnames(odwolania_z_13_Day)[1] <- "rok"
      
      odwolania_13_day <- cbind(odwolania_bez_13_Day, odwolania_z_13_Day)
      colnames(odwolania_13_day) <- c("rok", "odwolane_bez_13", "nieodwolane_bez_13", "inny niż 13", "rok2", "odwolane_z_13", "nieodwolane_z_13", "13")
      
      wykres_odwolania_13_day <- gather(odwolania_13_day,"procent_bez13","procent_z13", c(4,8))
      
      
      ggplot(data = wykres_odwolania_13_day,aes(x=rok,y=procent_z13, color=procent_bez13)) + 
        labs(y = "stosunek lotów odwołanych do nieodwoląnych [%]", title= "stosunek odwołań lotów zależnie od dnia", fill = "dzień wylotu: ", color = "dzien wylotu:") +
        geom_bar(stat = "identity",position = "dodge", fill="white", size=1.7, width=0.8, alpha=0.7)+
        #scale_fill_manual(values = c("white", "#E69F00"))+
        scale_color_manual(values = c("#999999", "#E69F00"))+
        scale_linetype_manual("dashed")+
        scale_x_continuous(breaks = seq(min(2003), max(2008), by = 1)) +
        coord_cartesian(ylim = c(0, 4)) +
        scale_y_continuous(breaks = seq(min(0), max(4), by = 1)) +
        theme(legend.position = "bottom")

      
    } else if (input$option1 == "Odwołania" & input$option2 == "W numerach lotów") {
      odwolania_bez_13_FlightNum <- read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_bez_13_FlightNum.csv")
      colnames(odwolania_bez_13_FlightNum)[4] <- "procent"
      colnames(odwolania_bez_13_FlightNum)[1] <- "rok"
      
      odwolania_z_13_FlightNum <- read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_z_13_FlightNum.csv")
      colnames(odwolania_z_13_FlightNum)[4] <- "procent"
      colnames(odwolania_z_13_FlightNum)[1] <- "rok"
      
      #złączenie obydwu tabel
      odwolania_13_FlightNum <- cbind(odwolania_bez_13_FlightNum, odwolania_z_13_FlightNum)
      colnames(odwolania_13_FlightNum) <- c("rok", "odwolane_bez_13", "nieodwolane_bez_13", "niezawierający numeru 13", "rok2", "odwolane_z_13", "nieodwolane_z_13", "zawierający numeru 13")
      
      wykres_odwolania_13_FlightNum <- gather(odwolania_13_FlightNum,"procent_bez13","procent_z13", c(4,8))
      
      #tworzenie wykresu
      ggplot(data = wykres_odwolania_13_FlightNum,aes(x=rok,y=procent_z13, color=procent_bez13)) + 
        labs(y = "stosunek lotów odwołanych do nieodwoląnych [%]", title= "stosunek odwołań lotów zależnie od numeru lotu", fill = "numer lotu: ", color = "numer lotu:") +
        geom_bar(stat = "identity",position = "dodge", fill="white", size=1.7, width=0.8, alpha=0.7)+
        #scale_fill_manual(values = c("white", "#E69F00"))+
        scale_color_manual(values = c("#999999", "#E69F00"))+
        scale_linetype_manual("dashed")+
        scale_x_continuous(breaks = seq(min(2003), max(2008), by = 1)) +
        coord_cartesian(ylim = c(0, 4)) +
        scale_y_continuous(breaks = seq(min(0), max(4), by = 1)) +
        theme(legend.position = "bottom")
      
    } else if (input$option1 == "Odwołania" & input$option2 == "W numerach samolotów") {
      #wczytanie odpowiednich tabel
      odwolania_bez_13_TailNum <- read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_bez_13_TailNum.csv")
      colnames(odwolania_bez_13_TailNum)[4] <- "procent"
      colnames(odwolania_bez_13_TailNum)[1] <- "rok"
      
      odwolania_z_13_TailNum <- read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_z_13_TailNum.csv")
      colnames(odwolania_z_13_TailNum)[4] <- "procent"
      colnames(odwolania_z_13_TailNum)[1] <- "rok"
      
      #złączenie obydwu tabel
      odwolania_13_TailNum <- cbind(odwolania_bez_13_TailNum, odwolania_z_13_TailNum)
      colnames(odwolania_13_TailNum) <- c("rok", "odwolane_bez_13", "nieodwolane_bez_13", "niezawierający numeru 13", "rok2", "odwolane_z_13", "nieodwolane_z_13", "zawierający numeru 13")
      
      wykres_odwolania_13_TailNum <- gather(odwolania_13_TailNum,"procent_bez13","procent_z13", c(4,8))
      
      #tworzenie wykresu
      ggplot(data = wykres_odwolania_13_TailNum,aes(x=rok,y=procent_z13, color=procent_bez13)) + 
        labs(y = "stosunek lotów odwołanych do nieodwoląnych [%]", title= "stosunek odwołań lotów zależnie od identyfikatora samolotu", fill = "identyfikator lotu: ", color = "identyfikator lotu:") +
        geom_bar(stat = "identity",position = "dodge", fill="white", size=1.7, width=0.8, alpha=0.7)+
        #scale_fill_manual(values = c("white", "#E69F00"))+
        scale_color_manual(values = c("#999999", "#E69F00"))+
        scale_linetype_manual("dashed")+
        scale_x_continuous(breaks = seq(min(2003), max(2008), by = 1)) +
        coord_cartesian(ylim = c(0, 4)) +
        scale_y_continuous(breaks = seq(min(0), max(4), by = 1)) +
        theme(legend.position = "bottom")
      
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
