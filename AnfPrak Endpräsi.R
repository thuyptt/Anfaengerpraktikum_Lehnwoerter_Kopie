####################################################################################################
#                              Anfängerpraktikum - WiSe 2019/20                                    #
# Lehnwörter im Spanischen im Rahmen des Korpusprojekts Fonología del Español Contemporáneo (FEC)  #
#                         Kommilitone A, Kommilitone B, Thi Thuy Pham                             
####################################################################################################


######################################### Vorbereitung #############################################

# Arbeitsverzeichnis setzen
setwd("E:\\AnfPrak")

#....................................... Pakete laden ..............................................

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(vcd)
library(DescTools)

#...................................... Daten einlesen .............................................

library(readxl)
data <- read_excel("Lehnwörter_UniWien.xlsx")
#View(data)

############################ erster Überblick über die Daten #######################################

head(data)
str(data)
summary(data)
#inspect(data)

###################### erster Überblick über die Zielvariable "Aussprache" #########################

#........................... Ausprägungen der Zielvariable umordnen ................................

data$Aussprache <- factor(data$Aussprache, levels = c("mit", "unsicher", "ohne", "FALSE"))

#............................ Häufigstabelle (abs. und rel. Hfgk.) .................................

table_y_rel <- round(prop.table(table(data$Aussprache)), digits=2)
table_y_rel

#............................. Balkendiagramme (2 Alternativen) ....................................

library(ggplot2)


barplot_y_rel <- ggplot(data = data, aes(x=factor(Aussprache), fill=factor(Aussprache))) +
                  geom_bar(aes(y=(..count..)/sum(..count..))) +
                  labs(title = "Verteilung von Aussprache", x="Aussprache", y="relative Häufigkeiten") +
                  scale_fill_discrete(name="Aussprache")  + 
                  theme(plot.title = element_text(size = 20, face = "bold"), 
                        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), 
                        axis.text=element_text(size=12),
                        legend.title = element_text(size = 12),
                        legend.text = element_text(size = 12)) +
                  ylim(0,1)
barplot_y_rel

################## Zielvariable "Aussprache" in Abhängigkeit von den Kovariablen ###################

#.................................. Daten aufbereiten ..............................................

# die Ausprägungen "k.A." in den Variablen "Ausbildung" und "Studium" sollen als NA-Werte aufgenommen werden
data <- na_if(data, "k.A.")  

# Anteil der NA-Werte in den Kovariablen 
missing_col <- sapply(data, function(x) sum(is.na(x))) / nrow(data)
missing_col[missing_col > 0]

# nur Ausprägungen "mit", "unsicher" und "ohne" der Variable "Aussprache" für unsere Analysen von Interesse, 
# und entferne die Zeilen in denen "Ausbildung" oder "Studium" NA-Werte enthalten sowie die Spalte "Qualität"
data_of_interest <- data[(data$Aussprache != "FALSE") & (!is.na(data$Ausbildung)) & (!is.na(data$Studium)),] %>%
                    select(-Qualität)
unique(data_of_interest$Aussprache)
unique(data_of_interest$Ausbildung)
unique(data_of_interest$Studium)

# Extrem lange Kovariablennamen kürzen
data_of_interest <- data_of_interest %>%
  rename(Silbenstruktur = `Silbenstruktur (Anzahl Silben, erste Silbe offen oder geschlossen?)`, 
         Frequenz = `Frequenz (Anzahl dokumentierter Fälle im Referenzkorpus)`)

# Leerzeichen & Bindestrich in Kovariablennamen durch _ ersetzen
names(data_of_interest) <- gsub(" |-", "_", names(data_of_interest))
names(data_of_interest)

#......................... Ausprägungen von einigen Kovariablen umordnen ...........................

data_of_interest$Ausbildung <- factor(data_of_interest$Ausbildung, levels=c("keine", "Grundschule", "Sekundarstufe I", "Sekundarstufe II", "Universität", "Doktorat") )
data_of_interest$Fremdsprachen <- factor(data_of_interest$Fremdsprachen, levels=c("keine", 1:5))
data_of_interest$Fremdsprachen_Art <- factor(data_of_interest$Fremdsprachen_Art, levels=c("keine", "Eng", "Fran", "Eng, Fran", "Eng, Deu", "Eng, Dän", "Eng, Fran, Deu", "Eng, Fran, Rus", "andere"))

#.................................... Häufigkeitstabelle ...........................................

table_y_x_rel <- lapply(select(data, -Aussprache), function(x) {round(prop.table(table(data$Aussprache,x)), digits=4)})
table_y_x_rel

#..................................... Balkendiagramme (siehe Folie 15-18) ..............................................

# alle Character-Vektoren in Faktorvariablen umformen
library(dplyr)
library(tidyr)
data_of_interest <- data_of_interest %>% mutate_if(is.character, as.factor)
str(data_of_interest)

# Datensatz in Longformat darstellen
library(reshape2)
data_m <- melt(data_of_interest, "Aussprache") 

library(ggplot2)
# alle Plots in einem Fenster anzeigen
barplot_y_all.x_rel <- ggplot(data_m, aes(x=value, fill=Aussprache)) +
  geom_bar(aes(y=(..count..)/sum(..count..)), position = position_stack(reverse = TRUE)) +
  facet_wrap(~variable, scales="free") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Verteilung von Aussprache in Abhängigkeit von verschiedenen Einflussgrößen",
       x="verschiedene Einflussgrößen",
       y="relative Häufigkeit")
barplot_y_all.x_rel

# Plot einzeln anzeigen
# Verteilung von Aussprache in Abhk. von Wort gruppiert nach Herkunft und sortiert nach Eintrittsdatum oder Frequenz
barplot_y_x2_rel <- ggplot(data=data_of_interest, aes(x=factor(Wort, levels=c("coñac", "chalet","champán",  "club", "rosbif", "kétchup")), fill=Aussprache)) +
                    geom_bar(aes(y=(..count..)/sum(..count..)), position = position_fill(reverse = TRUE), width=0.7) +
                    labs(title="Verteilung von Aussprache in Abhängigkeit von dem Wort",
                         x="Wort", y="relative Häufigkeit") +
                    theme(plot.title = element_text(size = 30, face = "bold"), 
                          axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), 
                          axis.text=element_text(size=15),
                          legend.title = element_text(size = 20),
                          legend.text = element_text(size = 15)) +
                    guides(fill = guide_legend(reverse = TRUE))  +
                    scale_fill_discrete(labels = c("mit Endkonsonant", "unsicher", "ohne Endkonsonant")) +
                    facet_grid(~factor(Herkunft, levels=c("französisch", "englisch")),  scale="free_x") +
                    theme(strip.text.x = element_text(size = 20))
barplot_y_x2_rel 
# alternative Sortierung nach Eintrittsdatum der Lehnwörter, indem man folgende Levels verwendet: levels=c("coñac", "champán", "chalet", "rosbif", "kétchup", "club")


######################## Zusammenhang Zwischen Aussprache und Qualität #############################

#.................................... Daten aufbereiten ............................................

# von Interresse ist nun der Zusammenhang zwischen "Aussprache" und "Qualität", wobei die Qualität nur
# bei den Wörtern gemessen wurde, bei dem der letzte Konsonant ausgesprochen wurde
data_of_interest_2 <- data %>% 
  filter(Aussprache=="mit") %>%
  filter(!is.na(Qualität))

#.................................... Häufigkeitstabelle ...........................................

table_y_x19_abs <- table(data_of_interest_2$Aussprache, data_of_interest_2$Qualität)
table_y_x19_rel <- round(prop.table(table_y_x19_abs), digits=2)
table_y_x19_rel

#..................................... Balkendiagramme (siehe Folie 36- ..............................................

# welche Kombinationen von Wort und Qualität kommen vor, falls der Endkonsonant realisiert wurde
data[data$Aussprache=="mit", c("Wort","Qualität")] %>% unique()
data_of_interest_2[data_of_interest_2$Aussprache=="mit", c("Wort","Qualität")] %>% unique()

# "Sibilant" mit "Frikativ" gleichsetzen
data_of_interest_2[data_of_interest_2$Qualität=="Sibilant", "Qualität"] <- "Frikativ"

# Plot: Qualität vs. Aussprache (insg.)
barplot_y_x19_rel_Wort <- ggplot(data=data_of_interest_2, mapping=aes(x=factor(Qualität, levels=c("Plosiv", "Frikativ", "Sibilant")), fill=Aussprache)) +
                          geom_bar(aes(y=(..count..)/sum(..count..)), width=0.7) +
                          labs(title="Verteilung von Aussprache mit Endkonsonant in Abhängigkeit von der Qualität", y="relative Häufigkeit",
                               x="Qualität")  + 
                          theme(plot.title = element_text(size = 20, face = "bold"), 
                                axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), 
                                axis.text=element_text(size=12),
                                legend.title = element_text(size = 12),
                                legend.text = element_text(size = 12),
                                legend.position = "none") +
                          ylim(0,1) 
barplot_y_x19_rel_Wort

# Plot: Qualität vs. Aussprache (wortspez. Plot, Wörter nach Frequenz sortiert)
barplot_y_x19_rel_Wort + 
  facet_wrap(~factor(Wort, levels=c("coñac", "champán", "chalet", "rosbif", "kétchup", "club"))) +
  theme(strip.text = element_text(size = 20)) +
  ylim(0, 0.35)

# Plot: Qualität vs. Aussprache (spezifisch für "club" an jedem Untersuchungsort)
barplot_y_x19_rel_Ort <- ggplot(data=filter(data_of_interest_2, Wort == "club"), 
                                mapping=aes(x=factor(Qualität, levels=c("Plosiv", "Frikativ", "Sibilant")), fill=Aussprache)) +
                          geom_bar(aes(y=(..count..)/sum(..count..)), width=0.7) +
                          labs(title="Verteilung von Aussprache mit Endkonsonant in Abhängigkeit von der Qualität für das Wort club",
                               y="relative Häufigkeit",
                               x="Qualität")  + 
                          theme(plot.title = element_text(size = 20, face = "bold"), 
                                axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), 
                                axis.text=element_text(size=12),
                                legend.title = element_text(size = 12),
                                legend.text = element_text(size = 12),
                                legend.position = "none") +
                          # Orte gruppiert in Spanien und Lateinamerika, und aufsteigend sortiert nach Einwohnerzahl
                          facet_wrap(~factor(Ort, levels=c("Menorca","Galicien", "Bilbao", "Sevilla", "Madrid",  
                                                           "La Plata", "Havana", "Quito","Guayaquil","Bogotá", 
                                                           "Mexico City")), ncol=5) + 
                          # alternativ: Orte aufsteigend sortiert nach Entfernung zur US bzw. UK/Frkr
                          #facet_wrap(~factor(Ort, levels=c("Bilbao","Galicien","Madrid","Menorca", "Sevilla",   
                          #                                 "Havana","Mexico City","Bogotá","Quito","Guayaquil",
                          #                                 "La Plata")), ncol=5) + 
                          theme(strip.text = element_text(size = 20))  +
                          ylim(0, 0.35)
barplot_y_x19_rel_Ort


