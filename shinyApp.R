library(shiny)
library(ggplot2)
library(tidyr)
options(browser = "chromium")    # u mnie bez tej opcji shiny nie działa
#sidebarLayout
ui <- fluidPage(
  titlePanel("Czy liczba 13 jest nieszczęśliwa?"),
  fluidPage(
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
      ggplot(mtcars, aes(x = mpg, y = disp)) +
        geom_point() +
        labs(title = "Scatter Plot: mpg vs. disp")
      
    } else if (input$option1 == "Opóźnienia" & input$option2 == "W numerach lotów") {
      ggplot(mtcars, aes(x = mpg, y = hp)) +
        geom_point() +
        labs(title = "Scatter Plot: mpg vs. hp")
      
    } else if (input$option1 == "Opóźnienia" & input$option2 == "W numerach samolotów") {
      ggplot(mtcars, aes(x = mpg, y = wt)) +
        geom_point() +
        labs(title = "Scatter Plot: mpg vs. wt")
      
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
      ggplot(mtcars, aes(x = disp, y = hp)) +
        geom_point() +
        labs(title = "Scatter Plot: disp vs. hp")
      
    } else if (input$option1 == "Odwołania" & input$option2 == "W numerach samolotów") {
      ggplot(mtcars, aes(x = disp, y = wt)) +
        geom_point() +
        labs(title = "Scatter Plot: disp vs. wt")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
