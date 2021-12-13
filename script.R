### MASTERARBEIT SS20
### Bearbeiter: Matthias Priehs (ID: 454225)

# Pakete laden
library(ggplot2) # Grafiken
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(forcats)
library(gridExtra)
library(tidyr)
library(reshape2)
library(table1)
library(car)

# Verzeichnis wechseln
setwd("H:/Google Drive/Uni/Uni Münster/Projektstudium MA/Masterarbeit/Fragebogen/Daten/R")

# Datensatz laden und anpassen
data1 <- as.data.frame(read.csv("Bearbeitet.csv", dec = ",", sep = ";"))
attach(data1)
data1 <- data1[data1[,5]=="teilgenommen und beendet",]
data1 <- data1[,-c(1:4)]

# Blaue Farbenpalette
cbp1 <- c("#b3cde0", "#6497b1", "#005b96", "#03396c",
          "#011f4b")

# Barplot Anzahl Jahre
levels(data1$X1..Seit.wie.vielen.Jahren.verkaufen.Sie.schon.auf.Amazon.)[2] <- ">10 Jahre"
data1$X1..Seit.wie.vielen.Jahren.verkaufen.Sie.schon.auf.Amazon. <- factor(data1$X1..Seit.wie.vielen.Jahren.verkaufen.Sie.schon.auf.Amazon.,
                                                                           levels = c("1 Jahr","2 Jahre","3 Jahre","4 Jahre","5 Jahre",
                                                                                      "6 Jahre","7 Jahre","8 Jahre","9 Jahre","10 Jahre",">10 Jahre"))

ggplot(data = data1)+
  aes(data1$X1..Seit.wie.vielen.Jahren.verkaufen.Sie.schon.auf.Amazon.,y = (..count..)/sum(..count..))+
  geom_bar(width=0.7,fill="#006E89",show.legend=FALSE)+
  scale_y_continuous(labels = scales::percent)+
  theme_calc()+
  theme(text=element_text(family="serif",size=15,colour = "black"))+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# Barplot Umsatz
levels(data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....)[2:6] <- c(">5.000.000???","0-100.000???","1.000.001-5.000.000???","100.001-500.000???","500.001-1.000.000???")
data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in.... <- factor(data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....,
                                                                           levels = c("0-100.000???","100.001-500.000???","500.001-1.000.000???","1.000.001-5.000.000???",">5.000.000???"))                                                                              

ggplot(data1,aes(data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....,y = (..count..)/sum(..count..)))+
  geom_bar(width=0.7,show.legend=TRUE)+
  scale_y_continuous(labels = scales::percent)+
  theme(text=element_text(family="serif",size=17,colour = "black"))+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....<- fct_rev(data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....)
ggplot(data1,aes(x="",y=(..count..)/sum(..count..),fill=data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....))+
  geom_bar(width=1)+
  scale_y_continuous(labels = scales::percent)+
  theme_calc()+
  scale_fill_manual(values=cbp1)+
  theme(text=element_text(family="serif",size=17,colour = "black"),
                          axis.title.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.title.y=element_blank(),
                          legend.title=element_blank())

# Barplot Produktkategorien
data1[is.na(data1)] <- 0
data2 <- data1[,c(4:15)]
data2 <- as.data.frame(colSums(data2))
rownames_data2 <- cbind(rownames(data2))
data2 <- data.frame(rownames_data2,data2[,1])
names(data2)[names(data2) == "data2...1."] <- "Anzahl"
names(data2)[names(data2) == "rownames_data2"] <- "Kategorien"
data2 <- data2[order(data2$Anzahl),]

par(las=2)
par(mar=c(5,8,4,2))
barplot(data2,col="#006E89",horiz = TRUE,cex.names = 0.8,cex.axis = 0.8,family="serif",xlim=c(0,120))
grid(NULL,NA,lwd=1,lty="solid")
box(which="plot",lty="solid")

ggplot(data2,aes(x=reorder(data2$Kategorien,data2$Anzahl),y=data2$Anzahl))+
  geom_col(width=0.7,fill="#006E89")+
  geom_text(aes(label=data2$Anzahl), position=position_dodge(width=0.9), vjust=0, hjust=0.05)+
  theme_calc()+
  coord_flip()+
  theme(text=element_text(family="serif",size=17,colour = "black"))+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# Barplot Artikelarten
artikelarten <- ggplot(data1,aes(x="",y=(..count..)/sum(..count..),fill=data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an.))+
  geom_bar(width=1)+
  scale_y_continuous(labels = scales::percent)+
  theme_calc()+
  scale_fill_manual(values=cbp1)+
  theme(text=element_text(family="serif",size=17,colour = "black"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank())

# Summary Table
# Option 1
data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....<- fct_rev(data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....)

levels(data1$X5..Wie.hoch.ist.der.durchschnittliche.Verkaufspreis.Ihrer.Waren.auf.Amazon..in....)[2:7] <- c(">500???","0-10???","10,01-25???","100,01-500???","25,01-50???","50,01-100???")
data1$X5..Wie.hoch.ist.der.durchschnittliche.Verkaufspreis.Ihrer.Waren.auf.Amazon..in.... <- factor(data1$X5..Wie.hoch.ist.der.durchschnittliche.Verkaufspreis.Ihrer.Waren.auf.Amazon..in....,
                                                                          levels = c("0-10???","10,01-25???","25,01-50???","50,01-100???","100,01-500???",">500???"))                                                                              

levels(data1$X11..Wie.hoch.ist.Ihre.durchschnittliche.produktbezogene.Marge..in....)[2:6] <- c(">50%","0-5%","11-20%","21-50%","6-10%")
data1$X11..Wie.hoch.ist.Ihre.durchschnittliche.produktbezogene.Marge..in.... <- factor(data1$X11..Wie.hoch.ist.Ihre.durchschnittliche.produktbezogene.Marge..in....,
                                                                                                    levels = c("0-5%","6-10%","11-20%","21-50%",">50%"))                                                                              

label(data1$X1..Seit.wie.vielen.Jahren.verkaufen.Sie.schon.auf.Amazon.) <- "Anzahl der aktiven Jahre auf Amazon"
label(data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....) <- "Jährlicher Umsatz"
label(data1$X5..Wie.hoch.ist.der.durchschnittliche.Verkaufspreis.Ihrer.Waren.auf.Amazon..in....) <- "Durchschnittlicher Verkaufspreis"

table1(~ data1$X1..Seit.wie.vielen.Jahren.verkaufen.Sie.schon.auf.Amazon. +
         data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in.... +
         data1$X5..Wie.hoch.ist.der.durchschnittliche.Verkaufspreis.Ihrer.Waren.auf.Amazon..in.... | data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an., data=data1, 
       topclass="Rtable1-times", overall="Gesamt")

# Option 2 (Mit Spaltenüberschriften)
# --> Als HTML Speichern, in Excel öffnen und bearbeiten, Dann in Word kopieren (Größe an Fenster anpassen)
levels(data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an.)[2:4] <- c("Eigen- und Fremdmarken","Eigenmarken","Fremdmarken")
labels <- list(variables=list(X1..Seit.wie.vielen.Jahren.verkaufen.Sie.schon.auf.Amazon.="Anzahl der aktiven Jahre",
                              X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....="Jährlicher Umsatz",
                              X5..Wie.hoch.ist.der.durchschnittliche.Verkaufspreis.Ihrer.Waren.auf.Amazon..in....="Durchschnittlicher Verkaufspreis",
                              X11..Wie.hoch.ist.Ihre.durchschnittliche.produktbezogene.Marge..in....="Durchschnittliche Marge"),
               groups=list("Artikelart",""))

strata <- c(split(data1,data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an.,drop=TRUE),list(Gesamt=data1)) 
table1(strata, labels, groupspan=c(3,1),topclass="Rtable1-times")

# Konkurrenzsituation (H1)
# Datensatz Fremdmarken
data1_sub_fremd <- data.frame(data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an.,
                              data1$X13..Konkurrieren.Sie.mit.Amazon.direkt.um.die..Buy.Box..bei.bestimmten.Artikeln.,
                              data1$X14..Wie.viel.Prozent.Ihrer.Artikel.sind.von.der.direkten.Konkurrenz.durch.Amazon.betroffen.,
                              data1$X15..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen.,
                              data1$Stark.reduziert..1....Stark.erhöht..7.,
                              data1$X17..Haben.Sie.schon.einmal.ein.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen.,
                              data1$X23..Konkurrieren.Sie.mit.Amazon.direkt.um.die..Buy.Box..bei.bestimmten.Artikeln.,
                              data1$X24..Wie.viel.Prozent.Ihrer.Artikel.sind.von.der.direkten.Konkurrenz.durch.Amazon.betroffen.,
                              data1$X25..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen.,
                              data1$Stark.reduziert..1....Stark.erhöht..7..2,
                              data1$X27..Haben.Sie.schon.einmal.ein.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen.)
levels(data1_sub_fremd$data1.X12..Welche.Arten.von.Artikeln.bieten.Sie.an.)[levels(data1_sub_fremd$data1.X12..Welche.Arten.von.Artikeln.bieten.Sie.an.)=="Eigen- und Fremdmarken"] <- "Fremdmarken"
data1_sub_fremd <- data1_sub_fremd[data1_sub_fremd[,1] == "Fremdmarken",]

data1_sub_fremd$data1.X13..Konkurrieren.Sie.mit.Amazon.direkt.um.die..Buy.Box..bei.bestimmten.Artikeln. <- paste(data1_sub_fremd$data1.X13..Konkurrieren.Sie.mit.Amazon.direkt.um.die..Buy.Box..bei.bestimmten.Artikeln.,
                                                                                                                 data1_sub_fremd$data1.X23..Konkurrieren.Sie.mit.Amazon.direkt.um.die..Buy.Box..bei.bestimmten.Artikeln.)
data1_sub_fremd$data1.X13..Konkurrieren.Sie.mit.Amazon.direkt.um.die..Buy.Box..bei.bestimmten.Artikeln. <- trimws(data1_sub_fremd$data1.X13..Konkurrieren.Sie.mit.Amazon.direkt.um.die..Buy.Box..bei.bestimmten.Artikeln.)

data1_sub_fremd$data1.X14..Wie.viel.Prozent.Ihrer.Artikel.sind.von.der.direkten.Konkurrenz.durch.Amazon.betroffen. <- paste(data1_sub_fremd$data1.X14..Wie.viel.Prozent.Ihrer.Artikel.sind.von.der.direkten.Konkurrenz.durch.Amazon.betroffen.,
                                                                                                                            data1_sub_fremd$data1.X24..Wie.viel.Prozent.Ihrer.Artikel.sind.von.der.direkten.Konkurrenz.durch.Amazon.betroffen.)
data1_sub_fremd$data1.X14..Wie.viel.Prozent.Ihrer.Artikel.sind.von.der.direkten.Konkurrenz.durch.Amazon.betroffen. <- trimws(data1_sub_fremd$data1.X14..Wie.viel.Prozent.Ihrer.Artikel.sind.von.der.direkten.Konkurrenz.durch.Amazon.betroffen.)

data1_sub_fremd$data1.X15..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen. <- paste(data1_sub_fremd$data1.X15..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen.,
                                                                                                                            data1_sub_fremd$data1.X25..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen.)
data1_sub_fremd$data1.X15..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen. <- trimws(data1_sub_fremd$data1.X15..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen.)

data1_sub_fremd$data1.Stark.reduziert..1....Stark.erhöht..7.[data1_sub_fremd$data1.Stark.reduziert..1....Stark.erhöht..7.==0]  <- ""
data1_sub_fremd$data1.Stark.reduziert..1....Stark.erhöht..7..2[data1_sub_fremd$data1.Stark.reduziert..1....Stark.erhöht..7..2==0]  <- ""

data1_sub_fremd$data1.Stark.reduziert..1....Stark.erhöht..7. <- paste(data1_sub_fremd$data1.Stark.reduziert..1....Stark.erhöht..7.,
                                                                      data1_sub_fremd$data1.Stark.reduziert..1....Stark.erhöht..7..2)
data1_sub_fremd$data1.Stark.reduziert..1....Stark.erhöht..7. <- trimws(data1_sub_fremd$data1.Stark.reduziert..1....Stark.erhöht..7.)

data1_sub_fremd$data1.X17..Haben.Sie.schon.einmal.ein.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen. <- paste(data1_sub_fremd$data1.X17..Haben.Sie.schon.einmal.ein.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen.,
                                                                                                                                                                            data1_sub_fremd$data1.X27..Haben.Sie.schon.einmal.ein.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen.) 
data1_sub_fremd$data1.X17..Haben.Sie.schon.einmal.ein.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen. <- trimws(data1_sub_fremd$data1.X17..Haben.Sie.schon.einmal.ein.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen.)

data1_sub_fremd <- data1_sub_fremd[,-c(7:11)] # Entferne überflüssige Spalten

names(data1_sub_fremd)[names(data1_sub_fremd) == "data1.X12..Welche.Arten.von.Artikeln.bieten.Sie.an."] <- "Artikelart" # Variablen umbenennen
names(data1_sub_fremd)[names(data1_sub_fremd) == "data1.X13..Konkurrieren.Sie.mit.Amazon.direkt.um.die..Buy.Box..bei.bestimmten.Artikeln."] <- "Konkurrenz"
names(data1_sub_fremd)[names(data1_sub_fremd) == "data1.X14..Wie.viel.Prozent.Ihrer.Artikel.sind.von.der.direkten.Konkurrenz.durch.Amazon.betroffen."] <- "Wie viele Artikel sind betroffen?"
names(data1_sub_fremd)[names(data1_sub_fremd) == "data1.X15..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen."] <- "Einstiegszeitpunkt"
names(data1_sub_fremd)[names(data1_sub_fremd) == "data1.Stark.reduziert..1....Stark.erhöht..7."] <- "Margenveraenderung"
names(data1_sub_fremd)[names(data1_sub_fremd) == "data1.X17..Haben.Sie.schon.einmal.ein.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen."] <- "Musste Produkt gelöscht werden?"

# Datensatz Eigenmarken
data1_sub_eigen <- data.frame(data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an.,
                              data1$X18..Bietet.Amazon.vergleichbare.Produkte.Ihrer.Eigenmarken.unter.eigenen.Marken..z.B..AmazonBasics.o.Ä...an.,
                              data1$X19..Wie.viel.Prozent.Ihrer.Eigenmarken.Artikel.sind.von.der.Konkurrenz.durch.Amazon.Eigenmarken.betroffen.,
                              data1$X20..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen.,
                              data1$Stark.reduziert..1....Stark.erhöht..7..1,
                              data1$X22..Haben.Sie.schon.einmal.ein.Eigenmarken.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen.,
                              data1$X28..Bietet.Amazon.vergleichbare.Produkte.Ihrer.Eigenmarken.unter.eigenen.Marken..z.B..AmazonBasics.o.Ä...an.,
                              data1$X29..Wie.viel.Prozent.Ihrer.Eigenmarken.Artikel.sind.von.der.Konkurrenz.durch.Amazon.Eigenmarken.betroffen.,
                              data1$X30..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen.,
                              data1$Stark.reduziert..1....Stark.erhöht..7..3,
                              data1$X32..Haben.Sie.schon.einmal.ein.Eigenmarken.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen.)
levels(data1_sub_eigen$data1.X12..Welche.Arten.von.Artikeln.bieten.Sie.an.)[levels(data1_sub_eigen$data1.X12..Welche.Arten.von.Artikeln.bieten.Sie.an.)=="Eigen- und Fremdmarken"] <- "Eigenmarken"
data1_sub_eigen <- data1_sub_eigen[data1_sub_eigen[,1] == "Eigenmarken",]

data1_sub_eigen$data1.X18..Bietet.Amazon.vergleichbare.Produkte.Ihrer.Eigenmarken.unter.eigenen.Marken..z.B..AmazonBasics.o.Ä...an. <- paste(data1_sub_eigen$data1.X18..Bietet.Amazon.vergleichbare.Produkte.Ihrer.Eigenmarken.unter.eigenen.Marken..z.B..AmazonBasics.o.Ä...an.,
                                                                                                                                             data1_sub_eigen$data1.X28..Bietet.Amazon.vergleichbare.Produkte.Ihrer.Eigenmarken.unter.eigenen.Marken..z.B..AmazonBasics.o.Ä...an.)
data1_sub_eigen$data1.X18..Bietet.Amazon.vergleichbare.Produkte.Ihrer.Eigenmarken.unter.eigenen.Marken..z.B..AmazonBasics.o.Ä...an. <- trimws(data1_sub_eigen$data1.X18..Bietet.Amazon.vergleichbare.Produkte.Ihrer.Eigenmarken.unter.eigenen.Marken..z.B..AmazonBasics.o.Ä...an.)

data1_sub_eigen$data1.X19..Wie.viel.Prozent.Ihrer.Eigenmarken.Artikel.sind.von.der.Konkurrenz.durch.Amazon.Eigenmarken.betroffen. <- paste(data1_sub_eigen$data1.X19..Wie.viel.Prozent.Ihrer.Eigenmarken.Artikel.sind.von.der.Konkurrenz.durch.Amazon.Eigenmarken.betroffen.,
                                                                                                                                           data1_sub_eigen$data1.X29..Wie.viel.Prozent.Ihrer.Eigenmarken.Artikel.sind.von.der.Konkurrenz.durch.Amazon.Eigenmarken.betroffen.)
data1_sub_eigen$data1.X19..Wie.viel.Prozent.Ihrer.Eigenmarken.Artikel.sind.von.der.Konkurrenz.durch.Amazon.Eigenmarken.betroffen. <- trimws(data1_sub_eigen$data1.X19..Wie.viel.Prozent.Ihrer.Eigenmarken.Artikel.sind.von.der.Konkurrenz.durch.Amazon.Eigenmarken.betroffen.)

data1_sub_eigen$data1.X20..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen. <- paste(data1_sub_eigen$data1.X20..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen.,
                                                                                                                              data1_sub_eigen$data1.X30..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen.)
data1_sub_eigen$data1.X20..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen. <- trimws(data1_sub_eigen$data1.X20..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen.)

data1_sub_eigen$data1.Stark.reduziert..1....Stark.erhöht..7..1[data1_sub_eigen$data1.Stark.reduziert..1....Stark.erhöht..7..1==0]  <- ""
data1_sub_eigen$data1.Stark.reduziert..1....Stark.erhöht..7..3[data1_sub_eigen$data1.Stark.reduziert..1....Stark.erhöht..7..3==0]  <- ""

data1_sub_eigen$data1.Stark.reduziert..1....Stark.erhöht..7..1 <- paste(data1_sub_eigen$data1.Stark.reduziert..1....Stark.erhöht..7..1,
                                                                        data1_sub_eigen$data1.Stark.reduziert..1....Stark.erhöht..7..3)
data1_sub_eigen$data1.Stark.reduziert..1....Stark.erhöht..7..1 <- trimws(data1_sub_eigen$data1.Stark.reduziert..1....Stark.erhöht..7..1)

data1_sub_eigen$data1.X22..Haben.Sie.schon.einmal.ein.Eigenmarken.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen. <- paste(data1_sub_eigen$data1.X22..Haben.Sie.schon.einmal.ein.Eigenmarken.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen.,
                                                                                                                                                                                        data1_sub_eigen$data1.X32..Haben.Sie.schon.einmal.ein.Eigenmarken.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen.)
data1_sub_eigen$data1.X22..Haben.Sie.schon.einmal.ein.Eigenmarken.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen. <- trimws(data1_sub_eigen$data1.X22..Haben.Sie.schon.einmal.ein.Eigenmarken.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen.)

data1_sub_eigen <- data1_sub_eigen[,-c(7:11)] # Entferne überflüssige Spalten

names(data1_sub_eigen)[names(data1_sub_eigen) == "data1.X12..Welche.Arten.von.Artikeln.bieten.Sie.an."] <- "Artikelart" # Variablen umbenennen
names(data1_sub_eigen)[names(data1_sub_eigen) == "data1.X18..Bietet.Amazon.vergleichbare.Produkte.Ihrer.Eigenmarken.unter.eigenen.Marken..z.B..AmazonBasics.o.Ä...an."] <- "Konkurrenz"
names(data1_sub_eigen)[names(data1_sub_eigen) == "data1.X19..Wie.viel.Prozent.Ihrer.Eigenmarken.Artikel.sind.von.der.Konkurrenz.durch.Amazon.Eigenmarken.betroffen."] <- "Wie viele Artikel sind betroffen?"
names(data1_sub_eigen)[names(data1_sub_eigen) == "data1.X20..Ist.Amazon.größtenteils.vor.oder.nach.Ihnen.als.Konkurrent.in.Ihr.Sortiment.eingestiegen."] <- "Einstiegszeitpunkt"
names(data1_sub_eigen)[names(data1_sub_eigen) == "data1.Stark.reduziert..1....Stark.erhöht..7..1"] <- "Margenveraenderung"
names(data1_sub_eigen)[names(data1_sub_eigen) == "data1.X22..Haben.Sie.schon.einmal.ein.Eigenmarken.Produkt.aufgrund.von.Verkaufsproblemen.durch.die.direkte.Konkurrenz.von.Amazon.vom.Marktplatz.nehmen.müssen."] <- "Musste Produkt gelöscht werden?"

# Merge beide Datensätze und Bearbeite
data1_sub_fremd <- rbind(data1_sub_fremd, data1_sub_eigen)
data1_sub_eigen_fremd <- data1_sub_fremd
rm(data1_sub_fremd)
rm(data1_sub_eigen)
attach(data1_sub_eigen_fremd)
data1_sub_eigen_fremd <- data1_sub_eigen_fremd[data1_sub_eigen_fremd[,2]!="Ja Nein",] 

data1_sub_eigen_fremd$`Wie viele Artikel sind betroffen?`[data1_sub_eigen_fremd$Konkurrenz == "Nein"] <- "Keine Konkurrenz"
data1_sub_eigen_fremd$Einstiegszeitpunkt[data1_sub_eigen_fremd$Konkurrenz == "Nein"] <- "Keine Konkurrenz"
data1_sub_eigen_fremd$Margenveraenderung[data1_sub_eigen_fremd$Konkurrenz == "Nein"] <- "Keine Konkurrenz"
data1_sub_eigen_fremd$`Musste Produkt gelöscht werden?`[data1_sub_eigen_fremd$Konkurrenz == "Nein"] <- "Keine Konkurrenz"
data1_sub_eigen_fremd$`Wie viele Artikel sind betroffen?`[data1_sub_eigen_fremd$Konkurrenz == "Ich weiß es nicht"] <- "Nicht bekannt"
data1_sub_eigen_fremd$Einstiegszeitpunkt[data1_sub_eigen_fremd$Konkurrenz == "Ich weiß es nicht"] <- "Nicht bekannt"
data1_sub_eigen_fremd$Margenveraenderung[data1_sub_eigen_fremd$Konkurrenz == "Ich weiß es nicht"] <- "Nicht bekannt"
data1_sub_eigen_fremd$`Musste Produkt gelöscht werden?`[data1_sub_eigen_fremd$Konkurrenz == "Ich weiß es nicht"] <- "Nicht bekannt"
data1_sub_eigen_fremd$Margenveraenderung[data1_sub_eigen_fremd$Einstiegszeitpunkt == "Ich weiß es nicht"] <- "Nicht bekannt"

data1_sub_eigen_fremd$Konkurrenz[data1_sub_eigen_fremd$Konkurrenz == "Ich weiß es nicht"] <- "Nicht bekannt"
data1_sub_eigen_fremd$Einstiegszeitpunkt[data1_sub_eigen_fremd$Einstiegszeitpunkt == "Ich weiß es nicht"] <- "Nicht bekannt"
data1_sub_eigen_fremd$Margenveraenderung[data1_sub_eigen_fremd$Einstiegszeitpunkt == "Vorher"] <- "Einstieg vorher"
data1_sub_eigen_fremd$Margenveraenderung[data1_sub_eigen_fremd$Margenveraenderung == ""] <- "Nicht bekannt"

data1_sub_eigen_fremd$`Wie viele Artikel sind betroffen?` <- as.factor(data1_sub_eigen_fremd$`Wie viele Artikel sind betroffen?`)
levels(data1_sub_eigen_fremd$`Wie viele Artikel sind betroffen?`)[1:6] <- c(">50%","0-10%","11-20%","21-30%","31-40%","41-50%")
data1_sub_eigen_fremd$`Wie viele Artikel sind betroffen?` <- factor(data1_sub_eigen_fremd$`Wie viele Artikel sind betroffen?`,
                                                                    levels = c("0-10%","11-20%","21-30%","31-40%","41-50%",">50%","Nicht bekannt",
                                                                               "Keine Konkurrenz")) 

data1_sub_eigen_fremd$Einstiegszeitpunkt <- as.factor(data1_sub_eigen_fremd$Einstiegszeitpunkt)
data1_sub_eigen_fremd$Einstiegszeitpunkt <- factor(data1_sub_eigen_fremd$Einstiegszeitpunkt,
                                                   levels = c("Nachher","Vorher","Nicht bekannt","Keine Konkurrenz"))

data1_sub_eigen_fremd$Margenveraenderung <- as.factor(data1_sub_eigen_fremd$Margenveraenderung)
levels(data1_sub_eigen_fremd$Margenveraenderung)[1:6] <- c("Sehr stark reduziert","Stark reduziert","Mäßig reduziert","Nicht verändert","Mäßig erhöht","Sehr stark erhöht")
data1_sub_eigen_fremd$Margenveraenderung <- factor(data1_sub_eigen_fremd$Margenveraenderung,
                                                   levels = c("Sehr stark reduziert","Stark reduziert","Mäßig reduziert","Nicht verändert","Mäßig erhöht","Sehr stark erhöht","Einstieg vorher",
                                                              "Nicht bekannt","Keine Konkurrenz"))

data1_sub_eigen_fremd$`Musste Produkt gelöscht werden?` <- as.factor(data1_sub_eigen_fremd$`Musste Produkt gelöscht werden?`)
levels(data1_sub_eigen_fremd$`Musste Produkt gelöscht werden?`)[1:4] <- c("Ja","Keine Konkurrenz","Nein","Nicht bekannt")
data1_sub_eigen_fremd$`Musste Produkt gelöscht werden?` <- factor(data1_sub_eigen_fremd$`Musste Produkt gelöscht werden?`,
                                                                  levels = c("Ja","Nein","Nicht bekannt","Keine Konkurrenz"))


labels <- list(variables=list(Konkurrenz="Ist Amazon Konkurrent?",
                              `Wie viele Artikel sind betroffen?`="Wie viele Artikel sind betroffen?",
                              Einstiegszeitpunkt="Wann ist Amazon eingestiegen?",
                              Margenveraenderung="Wie hat sich Marge verändert?",
                              `Musste Produkt gelöscht werden?`="Musste Produkt schon gelöscht werden?"),
               groups=list("Artikelart",""))

strata <- c(split(data1_sub_eigen_fremd,data1_sub_eigen_fremd$Artikelart,drop=TRUE),list(Gesamt=data1_sub_eigen_fremd)) 
table1(strata, labels, groupspan=c(3,1),topclass="Rtable1-times")

# Abhängigkeitspotenzial bestimmen (H2)
data3 <- data.frame(data1$X4..Bitte.geben.Sie.den.Anteil.Ihrer.Umsätze.auf.den.jeweiligen.Verkaufskanälen.an.Ihrem.Gesamtumsatz.an..in...,data1$Amazon,data1$Ebay,data1$Eigener.Online.Shop,data1$Andere.Plattformen,data1$Stationär..offline.)
names(data3)[names(data3) == "data1.X4..Bitte.geben.Sie.den.Anteil.Ihrer.Umsätze.auf.den.jeweiligen.Verkaufskanälen.an.Ihrem.Gesamtumsatz.an..in..."] <- "Hilfsvektor"
names(data3)[names(data3) == "data1.Amazon"] <- "Amazon"
names(data3)[names(data3) == "data1.Ebay"] <- "Ebay"
names(data3)[names(data3) == "data1.Eigener.Online.Shop"] <- "Online-Shop"
names(data3)[names(data3) == "data1.Andere.Plattformen"] <- "Andere Plattformen"
names(data3)[names(data3) == "data1.Stationär..offline."] <- "Stationär"

data3 <- melt(data=data3,
                id.vars       = c("Hilfsvektor"),
                measure.vars  = c("Amazon","Ebay","Online-Shop","Andere Plattformen","Stationär"),
                variable.name = "Verkaufskanal",
                value.name    = "Wert")

data3 <- data3[,-1]

theme_set(theme_calc())
ggplot(data3, aes(x=data3$Verkaufskanal,y=data3$Wert/10))+ 
  geom_boxplot(varwidth=T,fill="Grey")+
  scale_y_continuous(labels = scales::percent)+
  theme(text=element_text(family="serif",size=12,colour = "black"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank())

theme_set(theme_calc())
ggplot(data3, aes(x=data3$Verkaufskanal,y=data3$Wert))+ 
  geom_count(show.legend=F)+
  theme(text=element_text(family="serif",size=17,colour = "black"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank())

data1[,18:22][data1[,18:22] == 1] <- 0
data1[,18:22][data1[,18:22] == 2] <- 1
data1[,18:22][data1[,18:22] == 3] <- 2
data1[,18:22][data1[,18:22] == 4] <- 3
data1[,18:22][data1[,18:22] == 5] <- 4
data1[,18:22][data1[,18:22] == 6] <- 5
data1[,18:22][data1[,18:22] == 7] <- 6
data1[,18:22][data1[,18:22] == 8] <- 7
data1[,18:22][data1[,18:22] == 9] <- 8
data1[,18:22][data1[,18:22] == 10] <- 9
data1[,18:22][data1[,18:22] == 11] <- 10

labels2 <- list(variables=list(Amazon="Amazon",
                              Ebay="Ebay",
                              Eigener.Online.Shop="Eigener Online.Shop",
                              Andere.Plattformen="Andere Plattformen",
                              Stationär..offline.="Stationär"),
               groups=list("Artikelart",""))
strata2 <- c(split(data1,data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an.,drop=TRUE),list(Gesamt=data1)) 
table1(strata2, labels2, groupspan=c(3,1),topclass="Rtable1-times")

# Genereller Konkurrenzdruck und zukünftige Bedeutung von Amazon (H1 & H2)
labels3 <- list(variables=list(Sehr.gering..1....Sehr.stark..7.="Genereller Konkurrenzdruck",
                               Sehr.unwichtig..1....Sehr.wichtig..7.="Zukünftige Bedeutung von Amazon"),
                groups=list("Artikelart",""))
strata3 <- c(split(data1, data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an.,drop=TRUE),list(Gesamt=data1))
table1(strata3, labels3, groupspan=c(3,1),topclass="Rtable1-times",test=TRUE)

summary(aov(data1$Sehr.gering..1....Sehr.stark..7. ~ data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an.)) # P-Wert
TukeyHSD(aov(data1$Sehr.gering..1....Sehr.stark..7. ~ data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an.)) # Welche Gruppen unterscheiden sich signifikant?

summary(aov(data1$Sehr.unwichtig..1....Sehr.wichtig..7. ~ data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an.)) # P-Wert
TukeyHSD(aov(data1$Sehr.unwichtig..1....Sehr.wichtig..7. ~ data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an.)) # Welche Gruppen unterscheiden sich signifikant?

# Willkürliche Artikellöschung und Datenauswertung durch Amazon (H3)
data1$X33..Wurde.einer.Ihrer.Artikel.schon.einmal.ohne.offensichtlichen.Grund.seitens.Amazon.gelöscht.oder.deaktiviert. <- droplevels(data1$X33..Wurde.einer.Ihrer.Artikel.schon.einmal.ohne.offensichtlichen.Grund.seitens.Amazon.gelöscht.oder.deaktiviert.)
labels4 <- list(variables=list(X33..Wurde.einer.Ihrer.Artikel.schon.einmal.ohne.offensichtlichen.Grund.seitens.Amazon.gelöscht.oder.deaktiviert.="Willkürliche Löschung durch Amazon",
                              Sehr.unwahrscheinlich..1....Sehr.wahrscheinlich..7.="Datenauswertung durch Amazon"),
               groups=list("Artikelart",""))
strata4 <- c(split(data1,data1$X12..Welche.Arten.von.Artikeln.bieten.Sie.an.,drop=TRUE),list(Gesamt=data1)) 
table1(strata4, labels4, groupspan=c(3,1),topclass="Rtable1-times",test=TRUE)

# Auswertung betroffener Händler
data1['Betroffen'] <- NA
data1$Betroffen <- ifelse(data1$X13..Konkurrieren.Sie.mit.Amazon.direkt.um.die..Buy.Box..bei.bestimmten.Artikeln.=="Ja","Ja",
                    ifelse(data1$X18..Bietet.Amazon.vergleichbare.Produkte.Ihrer.Eigenmarken.unter.eigenen.Marken..z.B..AmazonBasics.o.Ä...an.=="Ja","Ja",
                           ifelse(data1$X23..Konkurrieren.Sie.mit.Amazon.direkt.um.die..Buy.Box..bei.bestimmten.Artikeln.=="Ja","Ja",
                                  ifelse(data1$X28..Bietet.Amazon.vergleichbare.Produkte.Ihrer.Eigenmarken.unter.eigenen.Marken..z.B..AmazonBasics.o.Ä...an.=="Ja","Ja","Nein"
                                  ))))
data1$Betroffen <- as.factor(data1$Betroffen)

labels5 <- list(variables=list(X33..Wurde.einer.Ihrer.Artikel.schon.einmal.ohne.offensichtlichen.Grund.seitens.Amazon.gelöscht.oder.deaktiviert.="Löschung oder Deaktivierung durch Amazon",
                               Sehr.unwahrscheinlich..1....Sehr.wahrscheinlich..7.="Datenauswertung durch Amazon",
                               Sehr.unwichtig..1....Sehr.wichtig..7.="Zukünftige Bedeutung von Amazon"),
                groups=list("Betroffen durch direkte Konkurrenz",""))
strata5 <- c(split(data1, data1$Betroffen),list(Gesamt=data1))
table1(strata5, labels5, groupspan=c(2,1),topclass="Rtable1-times")

ggplot(data1,aes(x=data1$Betroffen,fill=data1$X33..Wurde.einer.Ihrer.Artikel.schon.einmal.ohne.offensichtlichen.Grund.seitens.Amazon.gelöscht.oder.deaktiviert.))+
  geom_bar(position="dodge",width=0.7,show.legend=TRUE)+
  theme_calc()+
  scale_fill_manual(values = cbp1, name = "Betroffen von willkürlichen \nLöschungen?", labels = c("Ja", "Nein"))+
  scale_x_discrete(labels=c("Direkte Konkurrenz \nzu Amazon", "Keine direkte Konkurrenz \nzu Amazon"))+
  theme(text=element_text(family="serif",size=15,colour = "black"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

ggplot(data1,aes(x=data1$Betroffen,y=(..count..)/sum(..count..),fill=data1$X33..Wurde.einer.Ihrer.Artikel.schon.einmal.ohne.offensichtlichen.Grund.seitens.Amazon.gelöscht.oder.deaktiviert.))+
  geom_bar(width=0.7,show.legend=TRUE)+
  theme_calc()+
  scale_fill_manual(values = cbp1, name = "Betroffen von willkürlichen \nLöschungen?", labels = c("Ja", "Nein"))+
  scale_x_discrete(labels=c("Direkte Konkurrenz \nzu Amazon", "Keine direkte Konkurrenz \nzu Amazon"))+
  scale_y_continuous(labels = scales::percent)+
  theme(text=element_text(family="serif",size=15,colour = "black"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

leveneTest(data1$Sehr.gering..1....Sehr.stark..7., data1$Betroffen)
t.test(data1$Sehr.gering..1....Sehr.stark..7. ~ data1$Betroffen, var.equal = FALSE)

leveneTest(data1$Sehr.unwichtig..1....Sehr.wichtig..7.,data1$Betroffen)
t.test(data1$Sehr.unwichtig..1....Sehr.wichtig..7. ~ data1$Betroffen, var.equal = TRUE)

# Regression
## Modell: Betroffen ~ Marge + Umsatz + Verkaufspreis + Fragen + Versandkosten
levels(data1$X11..Wie.hoch.ist.Ihre.durchschnittliche.produktbezogene.Marge..in....)[1:5] <- c("1","2","3","4","5")
data1$X11..Wie.hoch.ist.Ihre.durchschnittliche.produktbezogene.Marge..in.... <- as.integer(data1$X11..Wie.hoch.ist.Ihre.durchschnittliche.produktbezogene.Marge..in....)
names(data1$X11..Wie.hoch.ist.Ihre.durchschnittliche.produktbezogene.Marge..in....) <- "Marge"


levels(data1$X5..Wie.hoch.ist.der.durchschnittliche.Verkaufspreis.Ihrer.Waren.auf.Amazon..in....)[1:6] <- c("1","2","3","4","5","6")
data1$X5..Wie.hoch.ist.der.durchschnittliche.Verkaufspreis.Ihrer.Waren.auf.Amazon..in.... <- as.integer(data1$X5..Wie.hoch.ist.der.durchschnittliche.Verkaufspreis.Ihrer.Waren.auf.Amazon..in....)

levels(data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....)[1:5] <- c("1","2","3","4","5")
data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in.... <- as.integer(data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....)

levels(data1$X6..Bitte.schätzen.Sie..Wie.viele.Fragen.werden.im.Durchschnitt.zu.Ihren.Produkten.auf.der.Produktdetailseite.gestellt.)[2:7] <- c("6","1","2","4","5","3")
data1$X6..Bitte.schätzen.Sie..Wie.viele.Fragen.werden.im.Durchschnitt.zu.Ihren.Produkten.auf.der.Produktdetailseite.gestellt. <- factor(data1$X6..Bitte.schätzen.Sie..Wie.viele.Fragen.werden.im.Durchschnitt.zu.Ihren.Produkten.auf.der.Produktdetailseite.gestellt.,
                                                                                                    levels = c("1","2","3","4","5","6"))                                                                              
data1$X6..Bitte.schätzen.Sie..Wie.viele.Fragen.werden.im.Durchschnitt.zu.Ihren.Produkten.auf.der.Produktdetailseite.gestellt. <- as.integer(data1$X6..Bitte.schätzen.Sie..Wie.viele.Fragen.werden.im.Durchschnitt.zu.Ihren.Produkten.auf.der.Produktdetailseite.gestellt.)

levels(data1$X9..Wie.hoch.sind.Ihre.durchschnittlichen.Versandkosten.pro.Sendung..in....)[2:6] <- c("5","1","3","4","2")
data1$X9..Wie.hoch.sind.Ihre.durchschnittlichen.Versandkosten.pro.Sendung..in.... <- factor(data1$X9..Wie.hoch.sind.Ihre.durchschnittlichen.Versandkosten.pro.Sendung..in....,
                                                                                                                                        levels = c("1","2","3","4","5"))                                                                             
data1$X9..Wie.hoch.sind.Ihre.durchschnittlichen.Versandkosten.pro.Sendung..in.... <- as.integer(data1$X9..Wie.hoch.sind.Ihre.durchschnittlichen.Versandkosten.pro.Sendung..in....)

library(varhandle)
levels(data1$Betroffen)[1:2] <- c("1","0")
data1$Betroffen <- unfactor(data1$Betroffen)

names(data1)[37] <- "Marge"
names(data1)[16] <- "Umsatz"
names(data1)[23] <- "Verkaufspreis"
names(data1)[24] <- "Fragen"
names(data1)[33] <- "Versandkosten"

model <- lm(data1$Betroffen ~ data1$Marge + data1$Umsatz + data1$Verkaufspreis + data1$Fragen + data1$Versandkosten,data = data1)
summary(model)

model2 <- glm(data1$Betroffen ~ data1$Marge + data1$Umsatz + data1$Verkaufspreis + data1$Fragen + data1$Versandkosten, family = binomial(link = "logit"), data = data1)
summary(model2)

# Nützliches für später
labels6 <- list(variables=list(X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....="UMSATZ",
                               Sehr.unwahrscheinlich..1....Sehr.wahrscheinlich..7.="Datenauswertung durch Amazon",
                               Sehr.unwichtig..1....Sehr.wichtig..7.="Zukünftige Bedeutung von Amazon"),
                groups=list("Betroffen durch direkte Konkurrenz",""))
strata6 <- c(split(data1, data1$Betroffen),list(Gesamt=data1))
table1(strata6, labels6, groupspan=c(2,1),topclass="Rtable1-times")

mytable <- table(data1$X1..Seit.wie.vielen.Jahren.verkaufen.Sie.schon.auf.Amazon.,data1$X3..Wie.hoch.ist.Ihr.aktueller..jährlicher.Umsatz..in....)
                 
ftable(mytable)

barplot(data1[,c(4:15)])

plot(data1$X7..Wie.viele.unterschiedliche.Artikel.bieten.Sie.auf.Amazon.an.)

mysub <- function(x) {sub(" ","",x)}
lapply(data1_sub_eigen_fremd[,3], mysub)

table1(data1, data1$Sehr.gering..1....Sehr.stark..7.,data1$Sehr.unwichtig..1....Sehr.wichtig..7.,
       splitby = "X12..Welche.Arten.von.Artikeln.bieten.Sie.an.", test=TRUE)


ggplot(data4,aes(fill=data4$Konkurrenz,x=data4$Artikelart))+ #Stacked Barplot
  geom_bar(position="dodge",width=0.7,show.legend=TRUE)+
  theme_calc()+
  scale_fill_manual(values=cbp1, name="Konkurrenz durch Amazon")+
  theme(text=element_text(family="serif",size=17,colour = "black"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())
  

