library(stringr)
library(rvest)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(rvest)
library(DataCombine)
library(tidyr)
library(pmdplyr)
library(plm)
library(RgoogleMaps)
library(naniar)
library(readr)
library(readxl)
library(lubridate)
library(magrittr)
library(filesstrings)
rm(afgcovid19)
setwd("F:/R Directory/Covid19_data")
rm(covid_tolo)
covid_tolo<- read_html("https://tolonews.com/covid-19-tracker")
province<- covid_tolo %>% 
  html_nodes(".views-field-field-countries-provinces") %>% 
  html_text()
confirmed<- covid_tolo %>% 
  html_nodes(".views-field-field-cases") %>% 
  html_text() 
recovered<- covid_tolo %>% 
  html_nodes(".views-field-field-recovered") %>% 
  html_text()
deaths<- covid_tolo %>% 
  html_nodes(".views-field-field-deaths") %>% 
  html_text()
date<- Sys.Date()

afgcovidtolo<- cbind(province, confirmed, deaths, recovered)
afgcovidtolo<- afgcovidtolo [-c(1:3),]
afgcovidtolo<- as.data.frame(afgcovidtolo)
afgcovidtolo <- afgcovidtolo %>% 
  mutate(date=date)
rm(confirmed, date, deaths, province, recovered, covid_tolo)
afgcovidtolo$confirmed<- as.double(as.character(afgcovidtolo$confirmed))
afgcovidtolo$deaths<- as.double(as.character(afgcovidtolo$deaths))
afgcovidtolo$recovered<- as.double(as.character(afgcovidtolo$recovered))
#1.joining datasets
covidold<- read.csv(paste("covidtolo",(Sys.Date()-1)), stringsAsFactors = FALSE)
covidold<- covidold[,-1]

covidold$date<- as.Date.character(covidold$date, format = c("%Y-%m-%d"))
yesterdaydata<- covidold
#1.2 Join new with old
afgcovid19<- rbind(yesterdaydata, afgcovidtolo)
afgcovid19$province<- gsub(" ", "", afgcovid19$province)
#Saving new dataset
write.csv(afgcovid19, paste0("F:/R Directory/Covid19_data/covidtolo"
                             , Sys.Date(), ".csv"))
rm(afgcovidtolo, covid_tolo, covidold, ff, merge, tolopastcovid, confirmed, deaths, date, date2, name, province, recovered)
# calculate daily rates
afgcovid19 <- afgcovid19 %>% 
  mutate(ID=id_variable(province,
                        .method = "number"))
afgcovid19<- afgcovid19[order(afgcovid19[,6], afgcovid19[,2], decreasing = TRUE),]
afgcovid19<- afgcovid19 %>% 
  slide(Var = "confirmed", GroupVar = "ID", slideBy = +1) 
afgcovid19<- afgcovid19 %>% 
  mutate(confrate= confirmed-`confirmed1`)
afgcovid19<- afgcovid19 %>% 
  slide(Var = "deaths", GroupVar = "ID", slideBy = +1) 
afgcovid19<- afgcovid19 %>% 
  mutate(dthrate=deaths - `deaths1`)
afgcovid19<- afgcovid19 %>% 
  slide(Var = "recovered", GroupVar = "ID", slideBy = +1) 
afgcovid19<- afgcovid19 %>% 
  mutate(rcvrdrate=recovered - `recovered1`)
afgcovid19$`confirmed1`<- NULL
afgcovid19$`deaths1`<- NULL
afgcovid19$`recovered1`<- NULL
afgcovid19<- afgcovid19 %>% 
  mutate(confrate=replace(confrate, confrate<0,0))
afgcovid19<- afgcovid19 %>% 
  mutate(dthrate=replace(dthrate, dthrate<0,0))
afgcovid19<- afgcovid19 %>% 
  mutate(rcvrdrate=replace(rcvrdrate, rcvrdrate<0,0))
afgcovid19$confirmed<- as.numeric(afgcovid19$confirmed)
afgcovid19$deaths<- as.numeric(afgcovid19$deaths)
afgcovid19$recovered<- as.numeric(afgcovid19$recovered)
rm(yesterdaydata,covidold)
#attaching geo points
geoinfo<- read_xlsx("provinc_geoinfo.xlsx")
afgcovid19<- afgcovid19 %>% 
  left_join(geoinfo, c("province" = "Province"))
rm(geoinfo)
#pushing data to github

setwd("F:/R Directory/covid19Afghanistan")
write.csv(afgcovid19, paste0("F:/R Directory/covid19Afghanistan/Update_data"
                             , Sys.Date(), ".csv"))
file.move(paste0("F:/R Directory/covid19Afghanistan/Update_data"
                 , Sys.Date(), ".csv"), "F:/R Directory/covid19Afghanistan/Update_data" )
setwd("F:/R Directory")

#country mobility data of AFghanistan
afgmobdata<- read.csv("afgmobility.csv", stringsAsFactors = FALSE)
afgmobdata<- afgmobdata %>% 
  filter(country_region=="Afghanistan")
afgmobdata$date<- as.Date.character(afgmobdata$date, format = c("%Y-%m-%d"))
afgmobdata<- afgmobdata[,-c(1,2,3,4)]
afgurl<-"https://www.gstatic.com/covid19/mobility/2020-04-26_AF_Mobility_Report_en.pdf"
afgmobilityupdate<-get_national_data(afgurl)
