library(rgdal)
library(ggplot2)
library(maptools)
library(gpclib)
library(plyr)
library(tidyverse)
library(ggrepel)
library(corrplot)
library(huxtable)
library(flextable)

#------defining variables needed---------------------

UKShape <- readOGR(dsn=".", layer="IMD_2019")

#----------- Convert to dataframe--------------------
gpclibPermit()
UKShape@data$id = rownames(UKShape@data)
UKShape.points = fortify(UKShape, region="id")
UKShape.df = join(UKShape.points, UKShape@data, by="id")

IMD <- as(UKShape, "data.frame")
IMD <- IMD %>% arrange(IMD_Rank)
which(is.na(IMD), arr.ind=TRUE) #----Check for na

#View(IMD)
IMD[c('IMD_Rank', 'IncRank', 'EmpRank','EduRank',
      'HDDRank','CriRank','BHSRank','EnvRank',
      'IDCRank','IDORank','CYPRank','ASRank','GBRank',
      'WBRank','IndRank','OutRank')]  <- lapply(IMD[c('IMD_Rank', 'IncRank', 'EmpRank','EduRank',
                                                      'HDDRank','CriRank','BHSRank','EnvRank',
                                                      'IDCRank','IDORank','CYPRank','ASRank','GBRank',
                                                     'WBRank','IndRank','OutRank')], as.numeric)
IMD <- IMD %>% rename(HealthRank = HDDRank,
                      HousingRank = BHSRank,
                      Inc_ChildRank = IDCRank,
                      Inc_OldRank = IDORank,
                      Children_YoungPeopleRank = CYPRank,
                      AdultSkillsRank = ASRank,
                      GeoBarrierRank = GBRank,
                      WiderBarrierRank = WBRank,
                      
)
#---------------------Getting the desired deciles -------------
IMDMostDeprivedDecile <- filter(IMD, IMD_Decile == 1)
IMDLeastDeprivedDecile <- filter(IMD, IMD_Decile == 10)
GDataIMDMostDeprivedDecile <- filter(UKShape.df, IMD_Decile == 1)
GDataIMDLeastDeprivedDecile <- filter(UKShape.df, IMD_Decile == 10)

#---------------Proportion --------------------

IMDLSOACount <- as.data.frame(table(IMD$LADnm)) %>%
  rename(
    LADnm = Var1,
    Total_LSOA_count = Freq
  )

#----------------Summary---------------------------------
summary(IMDMostDeprivedDecile$LADnm)
summary(IMDLeastDeprivedDecile$LADnm)
summary(IMDLeastDeprivedDecile)

#------------Local Authorities in these deciles ------------------------
MostDeprivedDecileLADs <- as.data.frame(table(IMDMostDeprivedDecile$LADnm))
colnames(MostDeprivedDecileLADs) <- c('LADNames','LSOA_Count')
MostDeprivedDecileLADs <- MostDeprivedDecileLADs %>% left_join(IMDLSOACount,
                                    by = c('LADNames' = 'LADnm'))
MostDeprivedDecileLADs <- mutate(MostDeprivedDecileLADs,Proportion = LSOA_Count/Total_LSOA_count)%>%
  arrange(desc(Proportion))

LeastDeprivedDecileLADs <- as.data.frame(table(IMDLeastDeprivedDecile$LADnm))
colnames(LeastDeprivedDecileLADs) <- c('LADNames','LSOA_Count')
LeastDeprivedDecileLADs <- LeastDeprivedDecileLADs %>% left_join(IMDLSOACount,
                                                               by = c('LADNames' = 'LADnm'))
LeastDeprivedDecileLADs <- mutate(LeastDeprivedDecileLADs,Proportion = LSOA_Count/Total_LSOA_count)%>%
  arrange(desc(Proportion))

EnglandLAD <- read.csv('england_lad_2011.csv') #----Fetching the Local authority boundary details

MostDeprivedDecileLADs <-left_join(MostDeprivedDecileLADs, EnglandLAD, 
                                      by=c('LADNames'='name'))
LeastDeprivedDecileLADs <-left_join(LeastDeprivedDecileLADs, EnglandLAD, 
                                       by=c('LADNames'='name'))

top10mostdeprived <-head(MostDeprivedDecileLADs,n = 10)
top10Leastdeprived <-head(LeastDeprivedDecileLADs,n = 10)

top10mostdeprived[top10mostdeprived == 'Kingston upon Hull, City of'] <- 'Hull'

#--------------------Visualising the data in map form -----------------------

#------------------------Most Deprived----------------------------------------
ggplot() +
  geom_polygon(data = UKShape.df, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3)+
  geom_point(data = GDataIMDMostDeprivedDecile, aes(x = long, y= lat),colour ='#F6D7DD')+
  geom_text_repel( data=top10mostdeprived %>% arrange(LSOA_Count), aes(x=x, y=y, label=LADNames), size=4)+
  geom_point( data=top10mostdeprived %>% arrange(LSOA_Count), aes(x=x, y=y, size=LSOA_Count), color= '#C9C494')+
  theme_void()+ coord_equal()+theme(legend.position="none")

#---------------------------Least Deprived----------------------------------
ggplot() +
  geom_polygon(data = UKShape.df, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3)+
  geom_point(data = GDataIMDLeastDeprivedDecile, aes(x = long, y= lat),colour ='#BAD3FA', alpha = 0.9)+
  geom_text_repel( data=top10Leastdeprived %>% arrange(LSOA_Count), 
                   aes(x=x, y=y, label=LADNames), size=4,min.segment.length = 0.01)+
  geom_point( data=top10Leastdeprived %>% arrange(LSOA_Count), aes(x=x, y=y, size=LSOA_Count), color= '#C9C494')+
  theme_void()+ coord_equal()+theme(legend.position="none")

#----------------------Regression and Correlation-----------------
#--------------------Training and test sample sets-----------------------

IMDMostDeprivedRandomized <- IMDMostDeprivedDecile[sample(1:nrow(IMDMostDeprivedDecile)), ] 
sampleMD <- sample(c(TRUE, FALSE), nrow(IMDMostDeprivedRandomized), replace=TRUE, prob=c(0.7,0.3))
IMDMostDeprived_train <- IMDMostDeprivedRandomized[sampleMD, ]
IMDMostDeprived_test <- IMDMostDeprivedRandomized[!sampleMD, ]

IMDLeastDeprivedRandomized <- IMDLeastDeprivedDecile[sample(1:nrow(IMDLeastDeprivedDecile)), ]
sampleLD <- sample(c(TRUE, FALSE), nrow(IMDLeastDeprivedRandomized), replace=TRUE, prob=c(0.7,0.3))
IMDLeastDeprived_train <- IMDLeastDeprivedRandomized[sampleLD, ]
IMDLeastDeprived_test <- IMDLeastDeprivedRandomized[!sampleLD, ]

View(IMDLeastDeprived_test)

#--------------------Correlation Tests--------------------------------------------------------
#--------------------------Cleveland Correlation Plots--------------
LeastDeprivedCorrelationmatrix<-
  as.data.frame(as.table(cor(IMDLeastDeprived_train[, c('IMD_Rank', 'IncRank', 'EmpRank','EduRank',
                                                        'HealthRank','CriRank','HousingRank','EnvRank',
                                                        'Inc_ChildRank','Inc_OldRank','Children_YoungPeopleRank','AdultSkillsRank','GeoBarrierRank',
                                                        'WiderBarrierRank','IndRank','OutRank')],method = 'kendall'))) 
MostDeprivedCorrelationmatrix<-
  as.data.frame(as.table(cor(IMDMostDeprived_train[, c('IMD_Rank', 'IncRank', 'EmpRank','EduRank',
                                                       'HealthRank','CriRank','HousingRank','EnvRank',
                                                       'Inc_ChildRank','Inc_OldRank','Children_YoungPeopleRank','AdultSkillsRank','GeoBarrierRank',
                                                       'WiderBarrierRank','IndRank','OutRank')], method = 'kendall')))

ClevelandLDCorrelationMatrix <- LeastDeprivedCorrelationmatrix %>%
  filter(Var2 == "IMD_Rank")
ClevelandMDCorrelationMatrix <- MostDeprivedCorrelationmatrix %>%
  filter(Var2 == "IMD_Rank")

colnames(ClevelandMDCorrelationMatrix)[colnames(ClevelandMDCorrelationMatrix) == "Freq"] <- "Value1"
colnames(ClevelandLDCorrelationMatrix)[colnames(ClevelandLDCorrelationMatrix) == "Freq"] <- "Value2"

ClevelandPlotData <- ClevelandMDCorrelationMatrix %>% 
  left_join(ClevelandLDCorrelationMatrix, by = 'Var1') %>%
  select(Var1,Var2.x,Value1,Value2) %>% rename(Var2 = Var2.x, 
                                               MD = Value1,
                                               LD = Value2)

ClevelandPlotData <- ClevelandPlotData %>%
  rowwise() %>%
  mutate(crmean = mean(c(MD,LD) ))%>%
  arrange(crmean)
colors <- c('#80B9C8','#E98A5A')

ggplot(ClevelandPlotData) +
  geom_segment( aes(x=Var1, xend=Var1, y=MD, yend=LD), color="grey") +
  geom_point( aes(x=Var1, y=MD, color='MD'), size=4) +
  geom_point( aes(x=Var1, y=LD, color='LD'), size=4) +
  
  scale_color_manual(labels = c('Least Deprived Areas', 'Most Deprived Areas'),
                     values = colors)+
  coord_flip()+
  labs(x="Indices of Deprivation", y="Correlation with IMD")+
  theme(legend.position = "bottom",legend.title=element_blank()) 

#----------Correlation Matrix------------------------------------------------------------------

corrplot(cor(IMDMostDeprived_train[, c('IMD_Rank', 'IncRank', 'EmpRank','EduRank',
                                       'HealthRank','CriRank','HousingRank','EnvRank',
                                       'Inc_ChildRank','Inc_OldRank','Children_YoungPeopleRank','AdultSkillsRank','GeoBarrierRank',
                                       'WiderBarrierRank','IndRank','OutRank')],method = 'kendall'), method="color",
         tl.col="black", tl.srt = 90,addCoef.col = "black",number.cex = 0.5)

corrplot(cor(IMDLeastDeprived_train[, c('IMD_Rank', 'IncRank', 'EmpRank','EduRank',
                                        'HealthRank','CriRank','HousingRank','EnvRank',
                                        'Inc_ChildRank','Inc_OldRank','Children_YoungPeopleRank','AdultSkillsRank','GeoBarrierRank',
                                        'WiderBarrierRank','IndRank','OutRank')],method = 'kendall'), method="color",
         tl.col="black", tl.srt = 90,addCoef.col = "black",number.cex = 0.5)
#----------------Regression for most deprived areas------------------------------------------

mod_IMD_md <- lm(
  formula = IMD_Rank~IncRank+EmpRank+EduRank+HealthRank+CriRank+HousingRank+EnvRank+Inc_ChildRank
  +Inc_OldRank+Children_YoungPeopleRank+AdultSkillsRank + GeoBarrierRank + WiderBarrierRank +IndRank+ OutRank,
  data = IMDMostDeprived_train
)

summary(mod_IMD_md)
IMDMostDeprived_resid <- IMDMostDeprived_train
#View(IMDMostDeprived_resid)
IMDMostDeprived_resid$IMDPredicted <- predict(mod_IMD_md)
IMDMostDeprived_resid$IMDResiduals <- residuals(mod_IMD_md)
#View(IMDMostDeprived_resid[1:10,c(6,64,65,66)])

IMDMostDeprived_test_copy <- IMDMostDeprived_test
IMDMostDeprived_test_copy$predicted <- predict(mod_IMD_md,
        newdata = IMDMostDeprived_test_copy,
        interval = 'confidence')

IMDMostDeprived_test_copy$residuals <- IMDMostDeprived_test_copy$predicted -
  IMDMostDeprived_test_copy$IMD_Rank
#View(IMDMostDeprived_test_copy)
IMDMostDeprived_test_copy$residuals
#View(IMDMostDeprived_test_copy[1:10,c(6,65,66)])

sse_md <- sum(IMDMostDeprived_test_copy$residuals**2)

#-------------------------Least deprived Regression----------
mod_IMD_ld <- lm(
  formula = IMD_Rank~IncRank+EmpRank+EduRank+HealthRank+CriRank+HousingRank+EnvRank+Inc_ChildRank
  +Inc_OldRank+Children_YoungPeopleRank+AdultSkillsRank + GeoBarrierRank + WiderBarrierRank +IndRank+ OutRank,
  data = IMDLeastDeprived_train
)

summary(mod_IMD_ld)
IMDLeastDeprived_resid <- IMDLeastDeprived_train
IMDLeastDeprived_resid$IMDPredicted <- predict(mod_IMD_ld)
IMDLeastDeprived_resid$IMDResiduals <- residuals(mod_IMD_ld)

IMDLeastDeprived_test_copy <- IMDLeastDeprived_test
IMDLeastDeprived_test_copy$predicted <- predict(mod_IMD_ld,
                                                newdata = IMDLeastDeprived_test_copy,
                                                interval = 'confidence')
IMDLeastDeprived_test_copy$residuals <- IMDLeastDeprived_test_copy$predicted - 
  IMDLeastDeprived_test_copy$IMD_Rank

#View(IMDLeastDeprived_test_copy[1:10,c(6,65,66)])

sse_ld <- sum(IMDLeastDeprived_test_copy$residuals**2)

coef(mod_IMD_ld)
summary(mod_IMD_ld)
#plot(mod_IMD_ld,which =1)


ht <- huxreg('Least Deprived Areas' = mod_IMD_ld, 'Most Deprived Areas' = mod_IMD_md, error_format = "({statistic})", 
       statistics = c("N. obs." = "nobs", "R squared" = "r.squared", "F statistic" = "statistic",
                                                      "P value" = "p.value"),
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),note = "{stars}. T statistics in brackets.")

quick_docx(ht)
