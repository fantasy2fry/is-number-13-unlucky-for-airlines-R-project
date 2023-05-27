options(browser = "chromium") # inaczej mi shiny nie działało
# library(data.table)
# #od '95 jest TailNum
# test<-read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/1994.csv.bz2")
# test2<-as.data.table( read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/2003.csv.bz2"))
# test<-as.data.table( read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/2008.csv.bz2"))
# library(sqldf)
# sqldf("SELECT * FROM test
#       WHERE TailNum LIKE '%13%'")-> test2
# library(dplyr)
# rm(test) #usuwanie zmiennych :)
# 
# result <- test %>%
#   filter(grepl("13", FlightNum)) %>%
#   select_if(~!any(is.na(.)))


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
  dodatkowa_kolumna = as.data.frame(wynik[,1]/wynik[,2])*100
  colnames(dodatkowa_kolumna) = "[,1]/[,2] %"
  wynik=cbind(wynik, dodatkowa_kolumna)
  return(wynik)
}

wynik_odwolania_z_13_w_TailNum = odwolania(TRUE, "TailNum")

wynik_odwolania_z_13_w_FlightNum = odwolania(TRUE, "FlightNum")

wynik_odwolania_z_13_w_Day = odwolania(TRUE, "DayofMonth")

wynik_odwolania_bez_13_w_TailNum = odwolania(FALSE, "TailNum")

wynik_odwolania_bez_13_w_FlightNum = odwolania(FALSE, "FlightNum")

wynik_odwolania_bez_13_w_Day = odwolania(FALSE, "DayofMonth")

write.csv(wynik_odwolania_z_13_w_Day, 
          file="~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_z_13_Day.csv")

write.csv(wynik_odwolania_z_13_w_TailNum, 
          file="~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_z_13_TailNum.csv")

write.csv(wynik_odwolania_z_13_w_FlightNum, 
          file="~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_z_13_FlightNum.csv")
write.csv(wynik_odwolania_bez_13_w_Day, 
          file="~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_bez_13_Day.csv")
write.csv(wynik_odwolania_bez_13_w_TailNum, 
          file="~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_bez_13_TailNum.csv")
write.csv(wynik_odwolania_bez_13_w_FlightNum, 
          file="~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_bez_13_FlightNum.csv")

# library(ggplot2) # moze

# dodatkowa_kolumna = as.data.frame(wynik_odwolania_z_13[,1]/wynik_odwolania_z_13[,2])*100
# colnames(dodatkowa_kolumna) = "[,1]/[,2] %"
# wynik_prim=cbind(wynik_odwolania_z_13, dodatkowa_kolumna)

# hist(wynik_odwolania_z_13_w_TailNum[[1]])
# 
# test = read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/carriers.csv")
# test = read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/plane-data.csv")
# test = read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/variable-descriptions.csv")
# test = read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/airports.csv")

# 1. wygenerowac troche tabel i zapisac do pliku
# 2. przygotowac pliki z tabelka dla Pani Dominiki
# 3. przygotuwac pliczek R zeby tylko wrzucila swoje sciezki


opoznienia_wylotow <- function(czyZ13, column){
  wynik=as.data.frame(NULL)
  for (iterator in 3:8) {
  sciezka=paste("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/200", iterator, ".csv.bz2" , sep="")
  tabela=read.csv(sciezka)
  if(czyZ13==TRUE){
    result <- tabela[grepl("13", tabela[[column]]), ]
  }else{
    result <- tabela[!grepl("13", tabela[[column]]), ]
  }
    opoznienia=as.data.frame(sum(result$DepDelay, na.rm=TRUE))
    colnames(opoznienia) <- paste("suma opoznien")
    ilosc_lotow = as.data.frame(nrow(result))
    colnames(ilosc_lotow) = "ilosc lotow"
    test2=cbind(opoznienia, ilosc_lotow)
    rownames(test2) = paste("200", iterator, sep="")
    if(nrow(wynik)==0){
      wynik=test2
    }else{
      wynik=rbind(wynik,test2)
  }
  rm(tabela)
  }
  dodatkowa_kolumna = as.data.frame(wynik[,1]/wynik[,2])
  colnames(dodatkowa_kolumna) = "średnio opóźnień (w minutach) na lot "
  wynik=cbind(wynik, dodatkowa_kolumna)
  return(wynik)
}
wynik_opoznienia_z_13_w_TailNum = opoznienia_wylotow(TRUE, "TailNum")
wynik_opoznienia_bez_13_w_TailNum = opoznienia_wylotow(FALSE, "TailNum")

wynik_opoznienia_z_13_w_FlightNum = opoznienia_wylotow(TRUE, "FlightNum")
wynik_opoznienia_bez_13_w_FlightNum = opoznienia_wylotow(FALSE, "FlightNum")

wynik_opoznienia_z_13_w_Day = opoznienia_wylotow(TRUE, "DayofMonth")
wynik_opoznienia_bez_13_w_Day = opoznienia_wylotow(FALSE, "DayofMonth")


write.csv(wynik_opoznienia_z_13_w_Day, 
          file="~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/opoznienia_z_13_Day.csv")
write.csv(wynik_opoznienia_bez_13_w_Day, 
          file="~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/opoznienia_bez_13_Day.csv")
write.csv(wynik_opoznienia_z_13_w_TailNum, 
          file="~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/opoznienia_z_13_TailNum.csv")
write.csv(wynik_opoznienia_bez_13_w_TailNum, 
          file="~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/opoznienia_bez_13_TailNum.csv")
write.csv(wynik_opoznienia_z_13_w_FlightNum, 
          file="~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/opoznienia_z_13_FlightNum.csv")
write.csv(wynik_opoznienia_bez_13_w_FlightNum, 
          file="~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/opoznienia_bez_13_FlightNum.csv")
