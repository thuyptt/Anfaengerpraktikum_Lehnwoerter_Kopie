library(readxl)
library(tidyverse)

# Read data
#setwd("../")
setwd("E:\\AnfPrak")
data_original <- read_excel("Lehnwörter_UniWien.xlsx")
# Show structure of all features
summary(data_original)

# Copy dataset to data frame, convert to appropriate initial data types
data <- type.convert(data_original)

#.................................... Häufigkeitstabelle ...........................................

data <- data %>%
  rename(Silbenstruktur = `Silbenstruktur (Anzahl Silben, erste Silbe offen oder geschlossen?)`, 
         Frequenz = `Frequenz (Anzahl dokumentierter Fälle im Referenzkorpus)`)

table_y_x_abs <- lapply(select(data, -Aussprache), function(x) {table(data$Aussprache,x)})
table_y_x_abs

#........................ Kontingenzkoeffiezenten (Stärke des Zusammenhangs) .......................

library(vcd)
cont.coef <- list()
length(cont.coef) <- length(table_y_x_abs)
names(cont.coef) <- names(table_y_x_abs)
for (i in 1:length(table_y_x_abs)) {
  cont.coef[i] <- lapply(table_y_x_abs, assocstats)[[i]][4]  
}
cont.coef

# korrigierter Kontingenzkoeffizient
library(DescTools)
corr_cont.coef <- list()
length(corr_cont.coef) <- length(table_y_x_abs)
names(corr_cont.coef) <- names(table_y_x_abs)
for (i in 1:length(table_y_x_abs)) {
  corr_cont.coef[i] <- lapply(table_y_x_abs, function(x) ContCoef(x,correct=T))[[i]] 
}
corr_cont.coef

# Interpretation: - Wert = 0:     empir. unahb.
#                 - kleiner Wert: schw. Zsmhg.
#                 - großer Wert:  st. Zsmhg.



######## Experiments ####################
# Library
library(fmsb)

#str(cont.coef)

# Convert named list to data frame
df_contcoef <- as.data.frame(corr_cont.coef)

# Add rows with max/min values
df_contcoef <- rbind(rep(1,19) , rep(0,19) , df_contcoef)

# select only columns where no NAs present
df_contcoef <- df_contcoef[sapply(df_contcoef, function(x) !any(is.na(x)))]

#str(df_contcoef)
#summary(df_contcoef)

####################### Radarchart ###############################

# Sources:
# https://cran.r-project.org/web/packages/radarchart/vignettes/preparingData.html
# https://www.ggplot2-exts.org/ggradar.html
# https://www.rdocumentation.org/packages/fmsb/versions/0.7.0/topics/radarchart

# The default radar chart 
radarchart(df_contcoef, axistype = 1,
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
           #custom labels
           vlcex=0.8 )





####################### barplot - not executed, just experimental ####################################

# Bc of critique on radar charts, consider horizontal bar plot

# Barplot
df_bar <- as.data.frame(corr_cont.coef)
df_bar <- df_bar[sapply(df_bar, function(x) !any(is.na(x)))]
library(reshape2)
df.long <- melt(df_bar, id.vars = "Sprecher.ID")

library(hrbrthemes)
ggplot(df.long) +
  aes(x=variable, y=value) +
  geom_segment( aes(x=variable ,xend=variable, y=0, yend=value, color="gray"), data = df.long) +
  geom_point( aes(size=5), data = df.long) +
  geom_point(size=5, color="#69b3a2") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text( size=48 ),
    legend.position="none"
  ) +
  ylim(0,1) +
  ylab("mark") +
  xlab("")

a <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(a) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )
a <- rbind(rep(20,10) , rep(0,10) , a)


library(qdapTools)
library(ggplot2)

# Create df from cont.coef list
cc_df <- list2df(corr_cont.coef)
cc_df <- cc_df[c("X2", "X1")]
names(cc_df) <- c("variablen", "kontingenzkoeffizienten")

library(reshape2)

melt(df_contcoef, id.vars = )
       
       ggplot(cc_df, aes(x = variablen, y = kontingenzkoeffizienten)) +
       annotate("text", x = 1, y = 0:1, label = 0:1, hjust = 1) +
       geom_polygon(size = 1, alpha = 0.2) +
       ggtitle("Radar")  +
       scale_y_continuous(labels = NULL) +
       scale_x_discrete() +
       #scale_color_manual(values= c("Red", "Blue","Black"))+
       #scale_fill_manual(values= c("Red", "Blue","Black"))+
       theme_light()+
       coord_polar()








