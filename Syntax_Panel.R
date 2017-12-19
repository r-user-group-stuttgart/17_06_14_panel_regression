#############################################################################################################
##############     Panel-Daten mit R     ####################################################################
##############         14.06.2017        ####################################################################
##############        Hannes Weber       ####################################################################
#############################################################################################################

setwd("Z:/5 - Vorträge/UniStuttgart R Panel 2017")

#install.packages(c("plm", "stargazer", "ggplot2", "lmtest"))

rm(list=ls())
library(plm) #Funktionen fuer Paneldatenanalyse 
library(stargazer) #Exportiert Tabellen nach HTML/LaTeX
library(ggplot2) 
library(lmtest)


#######################################################################
###Daten einlesen
daten <- read.csv("Datensatz_Panel.csv", header=T,sep=";")
head(daten)


########################################################################
###Daten in Panelform (plm-Objekt) transformieren
plmdaten <- plm.data(daten,index=c("Country","Year"))
summary(plmdaten)


#########################################################################
###Regressionsmodelle
#########################################################################
### a) Pooled OLS
OLS_1 <- plm(VoiceAcc ~ YearsSchooling+GDP_growth+log(GDP_cap)
             +log(Pop), data=plmdaten, model = "pooling")
summary(OLS_1)

#Test auf serielle Korrelation: Breusch-Godfrey-LM-Test
pbgtest(OLS_1)


#########################################################################
### b) Pooled OLS mit lagged dependent variable (LDV)
OLS_2 <- plm(VoiceAcc ~ lag(VoiceAcc, 1)+YearsSchooling+GDP_growth+
               log(GDP_cap)+log(Pop), data=plmdaten, model = "pooling")
summary(OLS_2)

#Mit "Panel-Corrected-Standard-Errors" (Beck/Katz 1995)
coeftest(OLS_2, vcov=vcovBK)

#Lag(s) auch fuer UV: Autoregressive Distributed Lag (ADL)-Modell
ADL_1 <- plm(VoiceAcc ~ lag(VoiceAcc, 1)+YearsSchooling+
               lag(YearsSchooling, 1)+GDP_growth+log(GDP_cap)+
               log(Pop), data=plmdaten, model = "pooling")
summary(ADL_1) #hohe Multikollinearitaet

#Lag kann auch erhoeht werden. Nachteil: Fallzahl geht verloren.
OLS_3 <- plm(VoiceAcc ~ lag(VoiceAcc, 5)+YearsSchooling+GDP_growth+
               log(GDP_cap)+log(Pop), data=plmdaten, model = "pooling")
summary(OLS_3)


#########################################################################
### c) Fixed-Effects (wie Dummy-Variablen fuer Laender)
FE_1 <- plm(VoiceAcc ~ YearsSchooling+GDP_growth+
              log(GDP_cap)+log(Pop), data=plmdaten, model = "within")
summary(FE_1)

### Fixed-Effects (Dummy-Variablen fuer Laender) UND LDV
FE_2 <- plm(VoiceAcc ~ lag(VoiceAcc, 5)+YearsSchooling+GDP_growth+
              log(GDP_cap)+log(Pop), data=plmdaten, model = "within")
summary(FE_2)

#F-Test, ob Fixed-Effects besser als OLS ist
pFtest(FE_2, OLS_3)

###Fixed-Effects (Laender UND Jahre)
FE_3 <- plm(VoiceAcc ~ lag(VoiceAcc, 5)+YearsSchooling+
              GDP_growth+log(GDP_cap)+log(Pop)+factor(Year), 
              data=plmdaten, model = "within")
summary(FE_3)

#F-Test, ob Modell mit Time-Dummies besser ist als ohne
pFtest(FE_3, FE_2)


#########################################################################
###Random-Effects (Laenderspezifischer Intercept variiert zufaellig)
RE_1 <- plm(VoiceAcc ~ lag(VoiceAcc, 5)+YearsSchooling+GDP_growth+
              log(GDP_cap)+log(Pop), data=plmdaten, model = "random")
summary(RE_1)

#Hausman-Test: Wenn Null-Hypothese abgelehnt wird, ist RE inkonsistent und FE besser
phtest(RE_1,FE_2)


#########################################################################
#First-Difference Model

FD_1 <- plm(VoiceAcc ~ YearsSchooling+GDP_growth+log(GDP_cap)+
              log(Pop), data=plmdaten, model = "fd")
summary(FD_1)



#########################################################################
###Tabellen exportieren
#########################################################################

stargazer(OLS_1, OLS_2, OLS_3, FE_1, FE_2, RE_1, FD_1, type="html",
          title="Determinants of Democracy (Worldbank 'Voice and Accountability')",
          out="panel_models.htm")


#######################################################################
#######################################################################
#######################################################################
###Anhang: Plots auf ersten Folien
datenquerschnitt <- daten[daten$Year==2014,]
ggplot(datenquerschnitt, aes(x=YearsSchooling, y=VoiceAcc)) +
  geom_point() + geom_smooth(method="lm", se=T, color="red")

Beispiellaender <- c("AFGHANISTAN", "ALBANIA", "ALGERIA", "ANGOLA", "ARGENTINA")
datenkurz <- subset(daten, Country %in% Beispiellaender, select=c(Country, Year, VoiceAcc, YearsSchooling))
ggplot(data=datenkurz, aes(x=Year, color=Country), par(xaxp=c(1996,2014,19))) + 
  geom_line(aes(y=scale(VoiceAcc), group=Country)) +
  geom_line(aes(y=scale(YearsSchooling), group=Country),linetype="dashed")


