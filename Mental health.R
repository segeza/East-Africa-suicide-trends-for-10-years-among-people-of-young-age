library(tidyverse)

# Loading data set into R
library(readr)
suicide_rates_among_young_people <- read_csv("Documents/
                                             Geoph with R/Data Sets/
                                             suicide-rates-among-young-people.csv")
View(suicide_rates_among_young_people)

# Data Cleaning, creatinng an east african data set (easuicide) from 2009 to 2019.
# teenager = 15-19 years old.
# earlytwenties = 20-24 years old.
# youngadults = 25-34 years old.

easuicide <- suicide_rates_among_young_people %>% 
 filter(Code == "KEN"|Code=="UGA"|Code=="TZA"|Code=="BDI"|Code == "RWA") %>% 
  filter(Year>=2009) %>% 
  rename(countrycode = Code, country = Entity, teenager=
           "death_rate100k - Cause: Self-harm - Sex: 
         Both sexes - Age_group: YEARS15-19",
         earlytwenties="death_rate100k - Cause: Self-harm - Sex: 
         Both sexes - Age_group: YEARS20-24",
         youngadults="death_rate100k - Cause: Self-harm - Sex: 
         Both sexes - Age_group: YEARS25-34")
view(easuicide)

#Suicide rates are expected to be decreasing with years 
#This is because of improvement in mental health services
#Null hypothesis; Suicide trends do not decrease as years going.

ggplot(data=easuicide)+
  geom_point(mapping = aes(x=Year, y=earlytwenties, color=countrycode, shape=countrycode))+
  geom_smooth(mapping = aes(x=Year, y=earlytwenties))
cor(easuicide$Year,easuicide$teenager)
cor.test(easuicide$Year,easuicide$teenager) 

#East Africa Suicide Rates among teenager
ggplot(data=easuicide)+
  geom_point(mapping = aes(x=Year, y=teenager, color=countrycode, shape=countrycode))+
  geom_smooth(mapping = aes(x=Year, y=teenager))+
  facet_wrap(~country, nrow=2)+
  scale_x_discrete(breaks=c("2009","2010","2011","2012",
                            "2013","2014","2015","2016","2017","2018","2019"))

#East Africa Suicide Rates among earlytwenties
ggplot(data=easuicide)+
  geom_point(mapping = aes(x=Year, y=earlytwenties, color=countrycode, shape=countrycode))+
  geom_smooth(mapping = aes(x=Year, y=earlytwenties))+
  facet_wrap(~country, nrow=2)+
  scale_x_discrete(breaks=c("2009","2010","2011","2012",
                            "2013","2014","2015","2016","2017","2018","2019"))

#East Africa Suicide Rates among youngadults
ggplot(data=easuicide)+
  geom_point(mapping = aes(x=Year, y=youngadults, color=countrycode, shape=countrycode))+
  geom_smooth(mapping = aes(x=Year, y=youngadults))+
  facet_wrap(~country, nrow=2)+
  scale_x_discrete(breaks=c("2009","2010","2011","2012",
                            "2013","2014","2015","2016","2017","2018","2019"))

#Visualization of suicide rates per age group in Tanzania.
suicidetz <- easuicide %>% filter(countrycode=="TZA")
#For teenagers
ggplot(data=suicidetz)+
  geom_smooth(mapping = aes(x=Year,y=teenager))+
  scale_x_discrete(breaks=c("2009","2010","2011","2012",
                            "2013","2014","2015","2016","2017","2018","2019"))
#For early twenties youths
ggplot(data=suicidetz)+
  geom_smooth(mapping = aes(x=Year,y=earlytwenties))+
  scale_x_discrete(breaks=c("2009","2010","2011","2012","2013",
                            "2014","2015","2016","2017","2018","2019"))
#For young adults
ggplot(data=suicidetz)+
  geom_smooth(mapping = aes(x=Year,y=youngadults))+
  scale_x_discrete(breaks=c("2009","2010","2011","2012",
                            "2013","2014","2015","2016","2017","2018","2019"))
