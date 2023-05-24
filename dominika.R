
# przykład jak możesz te pliczki wczytywać
# folderze tabelki są wszystkie i są dosc intuicyjnie nazwane
test = read.csv("~/Dokumenty/informatyczne/iadstudia/pdu/pd4/tabelki/odwolania_bez_13_FlightNum.csv")
# trzeba teraz magie zrobic bo rownames się zapisało jak kolumna
rownames(test) = test[[1]]
test=test[-1]
# weź to jako template i sobie to ponazywaj jak potrzebujesz itp itd, możesz moją sciężkę do pliczku ^^^ zakomentować