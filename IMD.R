library(foreign)
library(tmap)
library(rgdal)
library(tidyverse)

setwd("~/Introduction to Data Science/Coursework/English IMD 2019")
IMD <- read.dbf('IMD_2019.dbf')
IMD %>% order_by(IMD_Rank) 

setwd("~/Introduction to Data Science/Coursework")
UKShape <- readOGR(dsn = './English IMD 2019/', layer =  'IMD_2019')

IMDEducation <- IMD %>%
  select(lsoa11cd,lsoa11nm,LADnm, IMD_Rank, EduScore, EduRank,EduDec,
         CYPScore, CYPRank,CYPDec, ASScore, ASRank, ASDec)
IMDEducation <- arrange(IMDEducation, desc(CYPScore))
View(IMDEducation)

IMDGB <- IMD %>%
  select(lsoa11cd,lsoa11nm,LADnm,IMD_Rank,GBScore,GBRank,GBDec,
         BHSScore,BHSRank,BHSDec)
IMDGB <- arrange(IMDGB, desc(GBScore))
View(IMDGB)

View(IMD)
View(UKShape@data)

qtm(UKShape, style = 'natural', fill = 'GBRank')
