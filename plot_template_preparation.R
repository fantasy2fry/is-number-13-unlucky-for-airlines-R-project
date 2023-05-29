
# przykład jak możesz te pliczki wczytywać
# folderze tabelki są wszystkie i są dosc intuicyjnie nazwane
test = read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_bez_13_FlightNum.csv")
# trzeba teraz magie zrobic bo rownames się zapisało jak kolumna
rownames(test) = test[[1]]
test=test[-1]
# weź to jako template i sobie to ponazywaj jak potrzebujesz itp itd, możesz moją sciężkę do pliczku ^^^ zakomentować


#------------------------
library(ggplot2)

library(tidyr)

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


