# set working directory
setwd("E:\\AnfPrak")

#....................................... load libraries ..............................................

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(vcd)

#...................................... read data .............................................

data <- read_excel("Lehnwörter_UniWien.xlsx")
data <- type.convert(data)

##################### Prelim. Data Inspection & Cleaning #####################

# Umcodierung von fehlenden Daten in NA
nas <- c("k.A.", "FALSE")
data <- type.convert(data, na.strings = nas)

# Extrem lange Variablennamen kürzen
data <- data %>%
  rename(Silbenstruktur = `Silbenstruktur (Anzahl Silben, erste Silbe offen oder geschlossen?)`, 
         Frequenz = `Frequenz (Anzahl dokumentierter Fälle im Referenzkorpus)`)

# Leerzeichen & Bindestrich in Variablennamen durch _ ersetzen
names(data) <- gsub(" ", "_", names(data))
names(data) <- gsub("-", "_", names(data))

# Anteil fehlender Werte pro Spalte
missing_col <- sapply(data, function(x) sum(is.na(x))) / nrow(data)
# Spalten mit fehlenden Werten
missing_col[missing_col > 0]

# Zeilen mit fehlenden Werten in Ausbildung, Studium oder Aussprache entfernen
data <- data[!is.na(data$Aussprache) & !is.na(data$Studium) & !is.na(data$Ausbildung),]

################################ DATA FRAME NICHT-SPRACHLICHE FAKTOREN ################################

names(data)
sprecher_features <- c("Wort", "Aussprache", "Sprecher_ID", "Ort", "Kontinent", "Lage", "Einwohnerzahl", "Jahrgang_SprecherIn", "Geschlecht", "Studium", "Ausbildung", "Anzahl_Muttersprachen", "Fremdsprachen", "Fremdsprachen_Art")

df <- data[ ,sprecher_features]

df$Aussprache <- factor(df$Aussprache, levels = c("ohne", "unsicher", "mit"))
df$Wort <- factor(df$Wort, levels = c("coñac", "champán", "chalet", "rosbif", "kétchup", "club"))

my_colors <- c("#619CFF", "#00BA38", "#F8766D")

################################ ALTER ################################

# Alterskategorien gemäß Folie #3
library(lubridate)

calc_spk_age <- function(spk_year) {
  spk_age <- as.integer(year(Sys.Date())) - spk_year
  return(spk_age)
}

df <- df %>%
  mutate(Altersgruppen = case_when(calc_spk_age(Jahrgang_SprecherIn) > 65 ~ '>65',
                                       calc_spk_age(Jahrgang_SprecherIn) >= 31  & calc_spk_age(Jahrgang_SprecherIn) <= 65 ~ '31-65', 
                                       TRUE ~ '18-30'))

# Convert to ordered factor
df$Altersgruppen <- factor(df$Altersgruppen, levels = c("18-30", "31-65", ">65"))

p_altersgruppen_all <- ggplot(df, aes(x = factor(Altersgruppen), fill = Aussprache)) +
                        geom_bar(position = "fill", width=0.7) +
                        scale_fill_manual(values = my_colors, labels = c("ohne Endkonsonant", "unsicher", "mit Endkonsonant")) +
                        labs(title = "Verteilung von Aussprache in Abhängigkeit vom Alter",
                             x = "Altersgruppen der Sprecher", y = "relative Häufigkeit") +
                        theme(plot.title = element_text(size = 20, face = "bold"), 
                              axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), 
                              axis.text = element_text(size=12),
                              #axis.text.x = element_text(angle=45, hjust=1),
                              legend.title = element_text(size = 12),
                              legend.text = element_text(size = 12)) #+
                        #theme(axis.text.x = element_text(angle = 45))
p_altersgruppen_all

p_altersgruppen_facet <- p_altersgruppen_all + facet_wrap(vars(Wort))  +
                          theme(strip.text = element_text(size = 20))
p_altersgruppen_facet

################################ GESCHLECHT ################################

p_geschlecht_all <- ggplot(df, aes(x = factor(Geschlecht), fill = Aussprache)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = my_colors, labels = c("ohne Endkonsonant", "unsicher", "mit Endkonsonant")) +
  labs(title = "Verteilung von Aussprache in Abhängigkeit vom Geschlecht",
       x = "Geschlecht", y = "relative Häufigkeit") +
  scale_x_discrete(labels = c('männlich','weiblich'))
  #theme(axis.text.x = element_text(angle = 45))
p_geschlecht_all

p_geschlecht_facet <- p_geschlecht_all + facet_wrap(vars(Wort))
p_geschlecht_facet

################################ HERKUNFT ################################

# Levels sortieren
ort_lvls <- c("Havana", "Mexico City", "Bogotá", "Quito", "Guayaquil", "La Plata", "Bilbao", "Galicien", "Madrid", "Menorca", "Sevilla")
df$Ort <- factor(df$Ort, levels = ort_lvls)

p_ort_all <- ggplot(df, aes(x = factor(Ort), fill = Aussprache)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = my_colors, labels = c("ohne Endkonsonant", "unsicher", "mit Endkonsonant")) +
  labs(title = "Verteilung von Aussprache in Abhängigkeit vom Herkunftsort",
       x = "Herkunftsort", y = "relative Häufigkeit") +
  theme(axis.text.x = element_text(angle = 45))
p_ort_all

p_ort_facet <- p_ort_all + facet_wrap(vars(Wort))
p_ort_facet


my_colors <- c("#619CFF", "#00BA38", "#F8766D")
my_greys <- c("#808080", "#A9A9A9", "#D3D3D3")

my_list <- c("Havana", "Mexico City", "Bogotá", "Quito", "Guayaquil", "La Plata")

test <- ggplot(df, aes(x = factor(Ort), fill = Aussprache)) +
  #geom_bar(aes(alpha = Ort == "Havana"), position = "fill") +
  geom_bar(aes(alpha = ifelse(Ort %in% my_list, T, F)), position = "fill") +
  scale_fill_manual(values = my_colors, labels = c("ohne Endkonsonant", "unsicher", "mit Endkonsonant")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.4), guide = F) +
  theme(plot.title = element_text(size = 20, face = "bold"), 
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), 
        axis.text=element_text(size=12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
        #legend.position = "bottom"
        )
  
test



################################ BILDUNG ################################

# Levels sortieren
ausbildung_lvls <- c("keine", "Grundschule", "Sekundarstufe I", "Sekundarstufe II", "Universität", "Doktorat")
df$Ausbildung <- factor(df$Ausbildung, levels = ausbildung_lvls)

# Alle plotten
p_ausbildung_all <- ggplot(df, aes(x = factor(Ausbildung), fill = Aussprache)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = my_colors, labels = c("ohne Endkonsonant", "unsicher", "mit Endkonsonant")) +
  labs(title = "Verteilung von Aussprache in Abhängigkeit vom Bildungsgrad",
       x = "Bildungsgrad", y = "relative Häufigkeit") +
  theme(axis.text.x = element_text(angle = 45))
p_ausbildung_all

p_ausbildung_facet <- p_ausbildung_all + facet_wrap(vars(Wort))
p_ausbildung_facet

################################ MUTTERSPRACHEN ################################

# Levels sortieren
muttersprachen_num_lvls <- c("1", "2", "3")
df$`Anzahl Muttersprachen` <- factor(df$Anzahl_Muttersprachen, levels = muttersprachen_num_lvls)

# Alle plotten
p_muttersprachen_all <- ggplot(df, aes(x = factor(Anzahl_Muttersprachen), fill = Aussprache)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = my_colors, labels = c("ohne Endkonsonant", "unsicher", "mit Endkonsonant")) +
  labs(title = "Verteilung von Aussprache in Abhängigkeit von der Anzahl gesprochener Muttersprachen",
       x = "Anzahl gesprochener Muttersprachen", y = "relative Häufigkeit") #+
  #theme(axis.text.x = element_text(angle = 45))
p_muttersprachen_all

p_muttersprachen_facet <- p_muttersprachen_all + facet_wrap(vars(Wort))
p_muttersprachen_facet

################################ FREMDSPRACHEN ################################

# Levels sortieren
fremdsprachen_num_lvls <- c("keine", "1", "2", "3", "4", "5")
df$Fremdsprachen <- factor(df$Fremdsprachen, levels = fremdsprachen_num_lvls)

# Alle plotten
p_fremdsprachen_all <- ggplot(df, aes(x = factor(Fremdsprachen), fill = Aussprache)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = my_colors, labels = c("ohne Endkonsonant", "unsicher", "mit Endkonsonant")) +
  labs(title = "Verteilung von Aussprache in Abhängigkeit von der Anzahl gesprochener Fremdsprachen",
       x = "Anzahl gesprochener Fremdsprachen", y = "relative Häufigkeit") +
  theme(axis.text.x = element_text(angle = 45))
p_fremdsprachen_all

p_fremdsprachen_facet <- p_fremdsprachen_all + facet_wrap(vars(Wort))
p_fremdsprachen_facet


################################ FREMDSPRACHEN ART ################################

# Levels sortieren
fremdsprachen_art_lvls <- c("keine", "Eng","Eng, Dän","Eng, Deu", "Eng, Fran", "Eng, Fran, Deu", "Eng, Fran, Rus", "Fran", "andere")
df$Fremdsprachen_Art <- factor(df$Fremdsprachen_Art, levels = fremdsprachen_art_lvls)

# Alle plotten
p_fremdsprachen_art_all <- ggplot(df, aes(x = factor(Fremdsprachen_Art), fill = Aussprache)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = my_colors, labels = c("ohne Endkonsonant", "unsicher", "mit Endkonsonant")) +
  labs(title = "Verteilung von Aussprache in Abhängigkeit von der Art gesprochener Fremdsprachen",
       x = "Art gesprochener Fremdsprachen", y = "relative Häufigkeit") +
  theme(axis.text.x = element_text(angle = 45))
p_fremdsprachen_art_all

p_fremdsprachen_art_facet <- p_fremdsprachen_art_all + facet_wrap(vars(Wort))
p_fremdsprachen_art_facet






