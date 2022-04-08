########################## PACCHETTI UTILIZZATI ##################################
library(tidyverse)
library(randomForest)
library(lubridate)
library(ggcorrplot)
library(ggrepel)
library(ggtext)
library(directlabels)
library(wesanderson)
library(remotes)
library(ggdark)
library(gridExtra)
library(gridtext)
library(grid)
library(ggthemes)
library(cowplot)
library(scales)
library(corrplot)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import()
extrafont::loadfonts(device = "win", quiet = TRUE)

########################## CARICAMENTO DATASET ##################################

dati <- read.csv2("9.csv",h=T, sep=',')
str(dati)
#################################################################################
#################################################################################

########################## PULIZIA DATASET ######################################
dati <- separate(dati, datetime, c('date','hour'), sep = " ", 
                 convert = FALSE, remove = FALSE ) ## SEPARO ORA E DATA

date_seq <- seq(as.POSIXct("2011-01-01"), 
                as.POSIXct("2012-12-31"), 
                by=(60*60))
df <- data.frame(Date = strftime(date_seq, format="%Y-%m-%d"),
                 Time = strftime(date_seq, format="%H:%M:%S")) 
df.n <- df %>%
  mutate(day = day(Date))%>%
  filter(day <= 19) ## CREAZIONE NUOVO DATASET CONTENENTE GIORNI E ORE DAL 2011 AL 2012

data_def <- left_join(df.n,dati, by=c("Date"="date", "Time" = "hour")) ## UNISCO I DUE DATASET 

data_def$count[is.na(data_def$count)] <- 0
data_def$casual[is.na(data_def$casual)] <- 0
data_def$registered[is.na(data_def$registered)] <- 0
data_def.2 <- data_def%>%  
  fill(season, holiday, workingday,weather, temp,atemp,humidity,windspeed) ## IMPLEMENTO I DATI MANCANTI PER TUTTE LE VARIABILI
data_def.2 <- data_def.2 %>%
  mutate(month = month(Date))%>%
  mutate(year = year(Date))%>%
  mutate(day = day(Date))
data_def.2 <- separate(data_def.2, Time, c('hour',"minutes","seconds"), sep = ":", 
                       convert = FALSE, remove = FALSE ) 

data_def.2$hour <- as.integer(data_def.2$hour)
data_def.2 <- data_def.2%>% 
  mutate(date_hour = make_datetime(year,month,day,hour)) 
data_def.2 <- data_def.2[,c(-1:-7,-18:-20)] ## RIMUOVO ALCUNE VARIABILI DAL DATASET

data_def.2 <- data_def.2 %>% 
  select(date_hour, season, holiday,workingday,weather,temp,
         atemp, humidity, windspeed, casual, registered) 

data_def.2$season <- ifelse(data_def.2$season %in% 1, "Inverno",
                            ifelse(data_def.2$season %in% 2, "Primavera",
                                   ifelse(data_def.2$season %in% 3, "Estate", "Autunno")))

data_def.2$weather <- ifelse(data_def.2$weather %in% 1, "Buona",
                             ifelse(data_def.2$weather %in% 2, "Normale",
                                    ifelse(data_def.2$weather %in% 3, "Discreta", "Brutta")))

data_def.2$holiday <- ifelse(data_def.2$holiday %in% 0, "Feriale", "Festivo")
data_def.2$workingday <- ifelse(data_def.2$workingday %in% 0, "Fine_settimana", "Giorno_settimana") ## RINOMINO LE CATEGORIE DI ALCUNE VARIABILI

str(data_def.2)
data_def.2 <- data_def.2%>%
  mutate_at(c(2:5), as.factor)%>%
  mutate_at(c(6:9), as.numeric) ## CAMBIO NATURA ALLE VARIABILI
#############################################################################################################################
#############################################################################################################################

################################ DISTRIBUZIONE DELLE VARIABILI CASUAL E REGISTERED ##########################################

medie_anni <- data_def.2%>%
  group_by(year(date_hour))%>%
  summarise(media_reg = round(mean(registered),0), media_cas = round(mean(casual),0))## AFFITTI MEDI ANNUALI
colnames(medie_anni)[1] <- "Anno"
colnames(medie_anni)[2] <- "Media affitti per utenti registrati"
colnames(medie_anni)[3] <- "Media affitti per utenti non registrati"
DT::datatable(medie_anni)

cas_reg <- data_def.2%>%
  group_by(format(date_hour, format = "%Y %m"))%>%
  summarise(reg = sum(registered), cas = sum(casual))
colnames(cas_reg)[1] <- "YMD"

plot_temp <- ggplot(cas_reg, aes(x=YMD, group = 1)) + 
  geom_line(mapping = aes(y = reg, color = "Registrati"),size=1.5) + 
  geom_line(mapping = aes(y = cas, color = "Non Registrati"),size=1.5)+
  labs(title = "Distribuzione temporale degli affitti di bici", 
  subtitle = "Utenti registrati e non")+
  theme_bw() + 
  ylab("FREQUENZE") + xlab("ANNI 2011/2012") + labs(color='UTENTI')+
  scale_x_discrete(expand=c(0, 1.5)) +
  scale_color_manual(values = c("Registrati" = "deepskyblue1", "Non Registrati" = "forestgreen")) +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(family = "Candara"),
        title = element_text(size = 25,face="bold"),
        plot.subtitle = element_text(size = 20,face="bold"),
        axis.title = element_text(size = 20,face="bold", vjust = 2),
        axis.text.x = element_text(size = 15,angle = 45,face="bold",colour="black", vjust =  1, hjust = 1),
        axis.text.y = element_text(colour="black",size = 15,face="bold"),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = c(0.85,0.55), legend.box = "horizontal",
        legend.title=element_text(size = 20, face = "bold"), 
        legend.text=element_text(size = 15, face = "bold"))## DISTRIBUZIONE TEMPORALE DEGLI AFFITTI
#############################################################################################################################
#############################################################################################################################

############################################## ANALISI DESCRITTIVA ##########################################################
data_c <- data_def.2 %>% 
  group_by(season)%>%
  summarise(sum_c = sum(casual))%>%
  mutate(perc_c = (sum_c)/sum(sum_c))%>%
  arrange(sum_c)

data_r <- data_def.2 %>% 
  group_by(season)%>%
  summarise(sum_r = sum(registered))%>%
  mutate(perc_r = (sum_r)/sum(sum_r))%>%
  arrange(sum_r)


plot_stag_reg <- ggplot(data = data_r, mapping = aes(x=reorder(season,sum_r), y = perc_r))+
  geom_bar(width=0.5,stat = "identity",fill = "deepskyblue1", color = "forestgreen")+
  geom_text(aes(label = percent(perc_r), y = perc_r, group = season),
            position = position_dodge(width = 1), color = "black",
            vjust = -0.5, hjust = 0.5,size = 6, fontface = "bold",family = "Candara") +
  scale_y_continuous(labels=percent, limits = c(0,0.40))+
  labs(
    subtitle = "
    Utenti registrati")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        text=element_text(family = "Candara"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(face = "bold",colour="black", angle = 45,size = 15, vjust = 1, hjust = 1),
        axis.text.y = element_text(face = "bold",colour="black", size = 15),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "bottom") 

plot_stag_cas <- ggplot(data = data_c, mapping = aes(x=reorder(season,sum_c), y = perc_c))+
  geom_bar(width=0.5,stat = "identity",fill ="forestgreen", color = "darkslategray1")+
  geom_text(aes(label = percent(perc_c), y = perc_c, group = season),
            position = position_dodge(width = 1), color = "black",
            vjust = -0.5, hjust = 0.5,size = 6, fontface = "bold",family = "Candara") +
  scale_y_continuous(labels=percent, limits = c(0,0.40))+
  labs(
    subtitle = "
    Utenti non registrati")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        text=element_text(family = "Candara"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(face = "bold",angle = 45,colour="black", size = 15, vjust = 1, hjust = 1),
        axis.text.y = element_text(face = "bold",colour="black", size = 15),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "bottom") 

p1 <- list(plot_stag_reg, plot_stag_cas) %>% map(~.x + labs(x=NULL, y=NULL))

top1 <- textGrob("DISTRIBUZIONE DEGLI AFFITTI RISPETTO ALLA STAGIONE",
                 gp=gpar(fontsize=30,fontfamily="Candara", col = "black",fontface = "bold",vjust = 0.5))

yleft1 <- textGrob("PERCENTUALE AFFITTI",rot = 90,gp = gpar(fontsize = 20, fontface = "bold",
                                                            fontfamily = "Candara", col = "black"))

bottom1 <- textGrob("STAGIONE", gp = gpar(fontsize = 20,fontface = "bold",col = "black",
                                          fontfamily = "Candara"))

uni1 <- grid.arrange(grobs=p1, ncol = 2,top = top1, left = yleft1, bottom = bottom1) 

g1 <- ggdraw(uni1) + 
  theme(plot.background = element_rect(fill = "white", color = NA))## DISTRIBUZIONE VARIABILI RISPOSTA RISPETTO ALLA STAGIONE
#############################################################################################################################
#############################################################################################################################

plot_hol_reg <- ggplot (data_def.2, aes(x=reorder(holiday,-registered) , y = registered))  + 
  geom_boxplot (fill = "deepskyblue1", color = "blue")  + 
  labs(
    subtitle = "
    Utenti registrati")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        text=element_text(family = "Candara"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(face = "bold",colour="black", angle = 45,size = 15, vjust = 1, hjust = 1),
        axis.text.y = element_text(face = "bold",colour="black", size = 15),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "bottom")

plot_hol_cas <- ggplot (data_def.2, aes(x=reorder(holiday,-casual) , y = casual))  + 
  geom_boxplot (fill = "forestgreen", color = "blue")  + 
  labs(
    subtitle = "
    Utenti non registrati")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        text=element_text(family = "Candara"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(face = "bold",colour="black", angle = 45,size = 15, vjust = 1, hjust = 1),
        axis.text.y = element_text(face = "bold",colour="black", size = 15),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "bottom")

p2 <- list(plot_hol_reg, plot_hol_cas) %>% map(~.x + labs(x=NULL, y=NULL))

top2 <- textGrob("DISTRIBUZIONE DEGLI AFFITTI RISPETTO AL TIPO DI GIORNATA",
                 gp=gpar(fontsize=30,fontfamily="Candara", col = "black",fontface = "bold",vjust = 0.5))

yleft2 <- textGrob("AFFITTI",rot = 90,gp = gpar(fontsize = 20, fontface = "bold",
                   fontfamily = "Candara", col = "black"))

bottom2 <- textGrob("TIPO DI GIORNATA", gp = gpar(fontsize = 20,fontface = "bold",col = "black",
                   fontfamily = "Candara"))

uni2 <- grid.arrange(grobs=p2, ncol = 2, top = top2, left = yleft2, bottom = bottom2) 

g2 <- ggdraw(uni2) + 
  theme(plot.background = element_rect(fill = "white", color = NA))## DISTRIBUZIONE VARIABILI RISPOSTA RISPETTO AL TIPO DI GIORNATA
###################################################################################################################################
###################################################################################################################################

plot_wet_reg <- ggplot (data_def.2, aes(x=reorder(weather,-registered) , y = registered))  + 
  geom_boxplot (fill = "deepskyblue1", color = "blue")  + 
  labs(subtitle = "Utenti registrati")+xlab("Situazione meteorologica") + ylab("Affitti")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        text=element_text(family = "Candara"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(face = "bold",colour="black", angle = 45,size = 15, vjust = 1, hjust = 1),
        axis.text.y = element_text(face = "bold",colour="black", size = 15),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "bottom")

plot_wet_cas <- ggplot (data_def.2, aes(x=reorder(weather,-casual) , y = casual))  + 
  geom_boxplot (fill = "forestgreen", color = "blue")  + 
  labs(subtitle = "Utenti non registrati")+xlab("Situazione meteorologica")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        text=element_text(family = "Candara"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(face = "bold",colour="black", angle = 45,size = 15, vjust = 1, hjust = 1),
        axis.text.y = element_text(face = "bold",colour="black", size = 15),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "bottom")


p3 <- list(plot_wet_reg, plot_wet_cas) %>% map(~.x + labs(x=NULL, y=NULL))

top3 <- textGrob("AFFITTI E SITUAZIONE METEOROLOGICA",
                 gp=gpar(fontsize=30,fontfamily="Candara", col = "black",fontface = "bold",vjust = 0.5))

yleft3 <- textGrob("AFFITTI",rot = 90,gp = gpar(fontsize = 20, fontface = "bold",
                    fontfamily = "Candara", col = "black"))

bottom3 <- textGrob("SITUAZIONE METEOROLOGICA", gp = gpar(fontsize = 20,fontface = "bold",col = "black",
                    fontfamily = "Candara"))

uni3 <- grid.arrange(grobs=p3, ncol = 2,top = top3, left = yleft3, bottom = bottom3) 

g3 <- ggdraw(uni3) + 
  theme(plot.background = element_rect(fill = "white", color = NA))## DISTRIBUZIONE VARIABILI RISPOSTA RISPETTO ALLA SITUAZIONE METEOROLOGICA
###################################################################################################################################
###################################################################################################################################

only_num <- data_def.2[, c(6:11)]
c <- cor(only_num)
corrplot(c, type="upper",addCoef.col = "black", tl.cex = 1,number.cex = 1)## CORRELAZIONE
###################################################################################################################################
###################################################################################################################################

plot_um_reg <- ggplot(data = data_def.2,mapping = aes(x = humidity, y = registered))+
  geom_point(shape = 1,color = "blue")+geom_smooth(size = 2,color = "deepskyblue1",
                                                   method = "gam",formula = y ~ s(x, bs = "cs"))+
  labs(
    subtitle = "
    Utenti registrati")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        text=element_text(family = "Candara"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(face = "bold",colour="black", size = 15, vjust = 1, hjust = 0.5),
        axis.text.y = element_text(face = "bold",colour="black", size = 15),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "bottom")

plot_um_cas <- ggplot(data = data_def.2,mapping = aes(x = humidity, y = casual))+
  geom_point(shape = 1,color = "blue")+geom_smooth(size = 2,color = "forestgreen",
                                                   method = "gam",formula = y ~ s(x, bs = "cs"))+
  labs(
    subtitle = "
    Utenti non registrati")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        text=element_text(family = "Candara"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(face = "bold",colour="black", size = 15, vjust = 1, hjust = 0.5),
        axis.text.y = element_text(face = "bold",colour="black", size = 15),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "bottom")

p4 <- list(plot_um_reg, plot_um_cas) %>% map(~.x + labs(x=NULL, y=NULL))

yleft4 <- textGrob("Affitti",rot = 90,gp = gpar(fontsize = 20, fontface = "bold",
                    fontfamily = "Candara", col = "black"))

top4 <- textGrob("DISTRIBUZIONE AFFITTI E TASSO DI UMIDITÀ",
                 gp=gpar(fontsize=30,fontfamily="Candara", col = "black",fontface = "bold",vjust = 0.5))

bottom4 <- textGrob("Umidità", gp = gpar(fontsize = 20,fontface = "bold",col = "black",
                    fontfamily = "Candara"))

uni4 <- grid.arrange(grobs=p4, ncol = 2, top = top4,left = yleft4, bottom = bottom4) 

g4 <- ggdraw(uni4) + 
  theme(plot.background = element_rect(fill = "white", color = NA))## DISTRIBUZIONE VARIABILI RISPOSTA RISPETTO AL TASSO DI UMIDITà
###################################################################################################################################
###################################################################################################################################

plot_tem_reg <- ggplot(data = data_def.2,mapping = aes(x = temp, y = registered))+
  geom_point(shape = 1,color = "blue")+geom_smooth(size = 2,color = "deepskyblue1",
                                    method = "gam",formula = y ~ s(x, bs = "cs"))+
  labs(
    subtitle = "
    Utenti registrati")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        text=element_text(family = "Candara"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(face = "bold",colour="black", size = 15, vjust = 1, hjust = 0.5),
        axis.text.y = element_text(face = "bold",colour="black", size = 15),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "bottom")

plot_tem_cas <- ggplot(data = data_def.2,mapping = aes(x = temp, y = casual))+
  geom_point(shape = 1,color = "blue")+geom_smooth(size = 2,color = "forestgreen",
                                     method = "gam",formula = y ~ s(x, bs = "cs"))+
  labs(
    subtitle = "
    Utenti non registrati")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        text=element_text(family = "Candara"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(face = "bold",colour="black", size = 15, vjust = 1, hjust = 0.5),
        axis.text.y = element_text(face = "bold",colour="black", size = 15),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "bottom")

p5 <- list(plot_tem_reg, plot_tem_cas) %>% map(~.x + labs(x=NULL, y=NULL))

yleft5 <- textGrob("AFFITTI",rot = 90,gp = gpar(fontsize = 20, fontface = "bold",
                          fontfamily = "Candara", col = "black"))

top5 <- textGrob("DISTRIBUZIONE AFFITTI E TEMPERATURA",
                 gp=gpar(fontsize=30,fontfamily="Candara", col = "black",fontface = "bold",vjust = 0.5))

bottom5 <- textGrob("TEMPERARTURA", gp = gpar(fontsize = 20,fontface = "bold",col = "black",
                          fontfamily = "Candara"))

uni5 <- grid.arrange(grobs=p5, ncol = 2,top = top5, left = yleft5, bottom = bottom5) 

g5 <- ggdraw(uni5) + 
  theme(plot.background = element_rect(fill = "white", color = NA))## DISTRIBUZIONE VARIABILI RISPOSTA RISPETTO ALLA TEMPERATURA
###################################################################################################################################
###################################################################################################################################

##################################################### RANDOM FOREST REGISTERED ###############################################################
set.seed(500)

data_def.2.1 <- data_def.2%>%
  mutate(hour = hour(date_hour))## CREAZIONE NUOVO DATASET

data_def.2.1$hour <- as.integer(data_def.2.1$hour)
data_def.2.reg<-data_def.2.1[,-c(10)]## DATASET CON VARIABILE RISPOSTA REGISTERED 

sampl_reg<- sample(nrow(data_def.2.reg), 0.75*nrow(data_def.2.reg), replace = FALSE)## CAMPIONE SENZA SOSTITUZIONE DEL 75% DELLE RIGHE DEL DATASET

trainset_reg <- data_def.2.reg[sampl_reg,]## TRAIN SET (75% OSSERVAZIONI)
testset_reg <- data_def.2.reg[-sampl_reg,]## TEST SET  (25% OSSERVAZIONI)

xtest_reg<-testset_reg[,-10]## VARIABILI ESPLICATIVE NEL TEST SET 
ytest_reg<-testset_reg[,10]## VARIABILE REGISTERED NEL TEST SET
xtrain_reg<-trainset_reg[,-10]## VARIABILI ESPLICATIVE NEL TRAIN SET
ytrain_reg<-trainset_reg[,10]## VARIABILE REGISTERED NEL TEST SET

model4_reg<-randomForest(xtrain_reg,ytrain_reg,xtest_reg,ytest_reg,mtry=5,importance=TRUE)
model7_reg<-randomForest(xtrain_reg,ytrain_reg,xtest_reg,ytest_reg,mtry=8,importance=TRUE)
model9_reg<-randomForest(xtrain_reg,ytrain_reg,xtest_reg,ytest_reg,mtry=10,importance=TRUE)## CREAZIONE DI DIVERSI MODELLI CON VALORI DI MTRY DIVERSI

oob.err_reg<-c(model4_reg$mse[500],model7_reg$mse[500],model9_reg$mse[500])## OUT OF BAG ERRORS ESTIMATES

test.err_reg<-c(model4_reg$test$mse[500],
                model7_reg$test$mse[500],model9_reg$test$mse[500])## MEAN SQUARED TEST ERROR
###################################################################################################################################
###################################################################################################################################

##################################################### RANDOM FOREST CASUAL ###############################################################
data_def.2.cas<-data_def.2.1[,-c(11)]## DATASET CON VARIABILE RISPOSTA CASUAL

sampl_cas<- sample(nrow(data_def.2.cas), 0.75*nrow(data_def.2.cas), replace = FALSE)## CAMPIONE SENZA SOSTITUZIONE DEL 75% DELLE RIGHE DEL DATASET
trainset_cas <- data_def.2.cas[sampl_cas,] #train set (75% osservazioni)
testset_cas <- data_def.2.cas[-sampl_cas,] #test set  (25% osservazioni)

xtest_cas<-testset_cas[,-10]## VARIABILI ESPLICATIVE NEL TEST SET 
ytest_cas<-testset_cas[,10]## VARIABILE CASUAL NEL TEST SET 
xtrain_cas<-trainset_cas[,-10]## VARIABILI ESPLICATIVE NEL TRAIN SET 
ytrain_cas<-trainset_cas[,10]## VARIABILE CASUAL NEL TRAIN SET 

model4_cas<-randomForest(xtrain_cas,ytrain_cas,xtest_cas,ytest_cas,mtry=5,importance=TRUE)
model7_cas<-randomForest(xtrain_cas,ytrain_cas,xtest_cas,ytest_cas,mtry=8,importance=TRUE)
model9_cas<-randomForest(xtrain_cas,ytrain_cas,xtest_cas,ytest_cas,mtry=10,importance=TRUE)## CREAZIONE DI DIVERSI MODELLI CON VALORI DI MTRY DIVERSI

oob.err_cas<-c(model4_cas$mse[500],model7_cas$mse[500],model9_cas$mse[500])## OUT OF BAG ERRORS ESTIMATES 
test.err_cas<-c(model4_cas$test$mse[500], model7_cas$test$mse[500],model9_cas$test$mse[500])## MEAN SQUARED TEST ERROR
###################################################################################################################################
###################################################################################################################################

##################################################### CONFRONTO MSE MODELLI ################################################################
mse.4c <- model4_cas$mse
mse.7c <- model7_cas$mse
mse.9c <- model9_cas$mse
mseC <- data.frame(mse.4c,mse.7c,mse.9c)
mseC$numeroAlberi <- rep(1:500)## CREAZIONE DATASET CON I VALORI DI MSE

msepC <- ggplot(mseC, aes(x=numeroAlberi, group = 1)) + 
  geom_line(mapping = aes(y = mse.4c, color = "5"),size=1.5) + 
  geom_line(mapping = aes(y = mse.7c, color = "8"),size=1.5)+
  geom_line(mapping = aes(y = mse.9c, color = "10"),size=1.5)+
  labs(subtitle = "Modelli con variabile risposta casual")+
  ylab("MSE") + xlab("Numero di alberi") + labs(color='Mtry')+
  scale_color_manual(values = c("5" = "gold1", "8" = "green4", "10" = "blue2")) +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(family = "Candara"),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        axis.title = element_text(size = 15,face="bold", vjust = 2),
        axis.text.x = element_text(size = 15,face="bold",colour="black", vjust =  1, hjust = 1),
        axis.text.y = element_text(colour="black",size = 15,face="bold"),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "right",
        legend.title=element_text(size = 20, face = "bold"), 
        legend.text=element_text(size = 15, face = "bold"))## GRAFICO DEI VALORI DI MSE DEI MODELLI

mse.4r <- model4_reg$mse
mse.7r <- model7_reg$mse
mse.9r <- model9_reg$mse
mseR <- data.frame(mse.4r,mse.7r,mse.9r)
mseR$numeroAlberi <- rep(1:500)


msepR <- ggplot(mseR, aes(x=numeroAlberi, group = 1)) + 
  geom_line(mapping = aes(y = mse.4r, color = "5"),size=1.5) + 
  geom_line(mapping = aes(y = mse.7r, color = "8"),size=1.5)+
  geom_line(mapping = aes(y = mse.9r, color = "10"),size=1.5)+
  labs(subtitle = "Modelli con variabile risposta registered")+
  ylab("MSE") + xlab("Numero di alberi") + labs(color='Mtry')+
  scale_color_manual(values = c("5" = "gold1", "8" = "green4", "10" = "blue2")) +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(family = "Candara"),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        axis.title = element_text(size = 15,face="bold", vjust = 2),
        axis.text.x = element_text(size = 15,face="bold",colour="black", vjust =  1, hjust = 1),
        axis.text.y = element_text(colour="black",size = 15,face="bold"),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "none",
        legend.title=element_text(size = 20, face = "bold"), 
        legend.text=element_text(size = 15, face = "bold"))## GRAFICO DEI VALORI DI MSE DEI MODELLI



p6 <- list(msepR, msepC) %>% map(~.x + labs(x=NULL, y=NULL))

yleft6 <- textGrob("MSE",rot = 90,gp = gpar(fontsize = 15, fontface = "bold",
                                            fontfamily = "Candara", col = "black"))

top6 <- textGrob("CONFRONTO DEI VALORI DI MSE DEI MODELLI",
                 gp=gpar(fontsize=30,fontfamily="Candara", col = "black",fontface = "bold",vjust = 0.5))

bottom6 <- textGrob("Numero di alberi", gp = gpar(fontsize = 15,fontface = "bold",col = "black",
                                                  fontfamily = "Candara"))

uni6 <- grid.arrange(grobs=p6, ncol = 2, top = top6,left = yleft6, bottom = bottom6) 
###################################################################################################################################
###################################################################################################################################

##################################################### VARIABILI PIù IMPORTANTI ################################################################
impR <- importance(model7_reg)
impR <- as.data.frame(impR)
impR$varnames <- rownames(impR) # row names to column
rownames(impR) <- NULL  
colnames(impR)[1] <- "IncMse"

plot_imp_reg <- ggplot(impR, aes(x=reorder(varnames, IncMse), y = IncMse)) + 
  geom_point(color = "deepskyblue1", size = 2) +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=IncMse), color = "deepskyblue1", size = 1) +
  labs(subtitle = "Utenti registrati")+
  ylab("%IncMse") + scale_y_continuous(limits = c(0,620))+
  xlab("Variable Name") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        text=element_text(family = "Candara"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(face = "bold",colour="black", size = 15, vjust = 1, hjust = 0.5),
        axis.text.y = element_text(face = "bold",colour="black", size = 15),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "bottom") +
  coord_flip()

impC <- importance(model4_cas)
impC <- as.data.frame(impC)
impC$varnames <- rownames(impC) # row names to column
rownames(impC) <- NULL  
colnames(impC)[1] <- "IncMse"


plot_imp_cas <- ggplot(impC, aes(x=reorder(varnames, IncMse), y = IncMse)) + 
  geom_point(color = "forestgreen", size = 2) +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=IncMse), color = "forestgreen", size = 1) +
  labs(subtitle = "Utenti non registrati")+
  ylab("%IncMse") +
  xlab("Variable Name") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 20, family = "Candara", face = "bold"),
        text=element_text(family = "Candara"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(face = "bold",colour="black", size = 15, vjust = 1, hjust = 0.5),
        axis.text.y = element_text(face = "bold",colour="black", size = 15),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.position = "bottom") +
  coord_flip()


p7 <- list(plot_imp_reg, plot_imp_cas) %>% map(~.x + labs(x=NULL, y=NULL))

yleft7 <- textGrob("Nome Variabili",rot = 90,gp = gpar(fontsize = 20, fontface = "bold",
                                                       fontfamily = "Candara", col = "black"))

bottom7 <- textGrob("%IncMse", gp = gpar(fontsize = 20,fontface = "bold",col = "black",
                                        fontfamily = "Candara"))

top7 <- textGrob("Variabili esplicative più importanti nei modelli scelti",
                 gp=gpar(fontsize=30,fontfamily="Candara", col = "black",fontface = "bold",vjust = 0.5))

uni7 <- grid.arrange(grobs=p7, ncol = 2, top = top7, left = yleft7, bottom = bottom7) 

g7 <- ggdraw(uni7) + 
  theme(plot.background = element_rect(fill = "white", color = NA))
###################################################################################################################################
###################################################################################################################################



 
