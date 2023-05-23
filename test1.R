options(browser = "chromium") # inaczej mi shiny nie działało
library(data.table)
test<-as.data.table( read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/2007.csv.bz2"))
test2<-as.data.table( read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/2003.csv.bz2"))
test<-as.data.table( read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/2008.csv.bz2"))
library(sqldf)
sqldf("SELECT * FROM test 
      WHERE TailNum LIKE '%13%'")-> test2
library(dplyr)
rm(test) #usuwanie zmiennych :)

result <- test %>%
  filter(grepl("13", FlightNum)) %>%
  select_if(~!any(is.na(.)))

# 'result' will contain the filtered data frame with columns having no NA values

odwolania <- function(czyZ13, column){
  wynik=as.data.frame(NULL)
  for (iterator in 3:8) {
  sciezka=paste("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/200", iterator, ".csv.bz2" , sep="")
  tabela=read.csv(sciezka)
  if(czyZ13==TRUE){
    result <- tabela[grepl("13", tabela[[column]]), ]
  }else{
    result <- tabela[!grepl("13", tabela[[column]]), ]
  }
    result2=as.data.frame(sum(result$Cancelled))
    colnames(result2) <- paste("odwolane")
    nieodwolane = as.data.frame(nrow(result[result$Cancelled==0, ]))
    colnames(nieodwolane) = "nieodwolane"
    test2=cbind(result2, nieodwolane)
    rownames(test2) = paste("200", iterator, sep="")
    if(nrow(wynik)==0){
      wynik=test2
    }else{
      wynik=rbind(wynik,test2)
  }
  
  rm(tabela)
  }
  return(wynik)
}

wynik_odwolania_z_13_w_TailNum = odwolania(TRUE, "TailNum")

wynik_odwolania_z_13_w_FlightNum = odwolania(TRUE, "FlightNum")
library(ggplot2) # moze

dodatkowa_kolumna = as.data.frame(wynik_odwolania_z_13[,1]/wynik_odwolania_z_13[,2])*100
colnames(dodatkowa_kolumna) = "[,1]/[,2] %"
wynik_prim=cbind(wynik_odwolania_z_13, dodatkowa_kolumna)
