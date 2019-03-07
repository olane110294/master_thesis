library(readxl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(readxl)
library(xtable)

dataset <- read_excel("vinmonopolet_auksjoner.xlsx")
colnames(dataset)[3]="object_id"
colnames(dataset)[19]="buyer_id"
colnames(dataset)[16]="tilslag_flaske"
colnames(dataset)[15]="tilslag_totalt"
colnames(dataset)[12]="verdi_flaske"

################ lage datasett for alle rundene til figur 3.1
alle_runder <- split(dataset,dataset$Dato)
auksjoner_Runde <- sapply(alle_runder, NROW) 
# lager en rask for loop for å finne hvor mye hver runde har generert
# i omsetning
dato_runder <- names(auksjoner_Runde)
verdi_runde=0
for (i in 1:length(auksjoner_Runde)){
verdi_runde[i] <- sum(alle_runder[[dato_runder[i]]][["tilslag_totalt"]])}

df_figur1 <- data.frame(dato=as.factor(dato_runder), 
                        omsetning=as.numeric(verdi_runde),
                        antall_auksjoner = as.numeric(unname(auksjoner_Runde)))
# omgjør tall til millioner
df_figur1$omsetning <- paste(format(round(df_figur1$omsetning / 1e6, 1), trim = TRUE), "M")

# en ting mangler: sentrere datoene rett under ticksene
figure_1 <- ggplot(df_figur1, aes(x=dato, y=antall_auksjoner)) + 
    geom_bar(stat="identity") + theme_bw() + ylim(0,1300) +
    theme(panel.grid=element_blank(), axis.text.x = element_text(angle=90)) +
    ylab("Antall auksjoner") + xlab("") 
  
foo <- ggplot_build(figure_1)$data[[1]]

figure_1 + annotate("text", x=foo$x, y=foo$y + 1, label = df_figur1$omsetning,
                    vjust=-0.5, size=2.5)

################ lage datasett for alle rundene til figur 3.2 antall kjøpere og hvor ofte de kjøper
alle_bydere <- split(dataset,dataset$buyer_id)
kjøp_byder <- sapply(alle_bydere, NROW) 

# lager en dataframe og lager factor verdier av ID som er sortert etter størrelse på antall
df_figur32 <- data.frame(antall=round(as.numeric(unname(kjøp_byder)))) 
# her er order by factor, hvis vi skal plotte
  #df_figur32$id <- factor(df_figur32$id, levels=df_figur32$id[order(df_figur32$antall)])
# men siden vi skal lage data frame over til table, så må vi order direkte
df_figur32 <- sort(df_figur32, decreasing = TRUE)

xtable(t(df_figur32[1:42]))
# lager en enkel plot  
figure_32 <- ggplot(df_figur32, aes(x=id, y=antall)) + geom_bar(stat="identity") + theme_bw() +
  theme(panel.grid=element_blank(), axis.text.x = element_text(angle=90)) 

foo2 <- ggplot_build(figure_32)$data[[1]]

figure_1 + annotate("text", x=foo2$x, y=foo2$y + 1, label = df_figur32$antall,
                    vjust=2, size=2.5)






