#install.packages("wbstats")
#install.packages("devtools")
#install.packages("wb")
#devtools::install_github("nset-ornl/wbstats")
#install.packages("WDI")
install.packages("openxlsx")

library(tidyverse)
library(wbstats)
library(dplyr)
library(readxl)
library(WDI)
library(countrycode)
library(openxlsx)

str(wb_cachelist, max.level=1)

?wbcache

wbindicators(lang = "en")

new_cache <- wb_cache()


#Search
WDIsearch()



################################ GDP Growth#########################


GDP_Growth <- WDI(indicator = "NY.GDP.MKTP.KD.ZG")

GDP_Growth <- GDP_Growth %>% 
  filter(year >= 1997) %>% 
 # rename(GDP_Growth=NY.GDP.MKTP.KD.ZG) %>% 
  select(-country)


################################# GDP Per Capita###########################



GDP_cap <- WDI(indicator = "NY.GDP.PCAP.CD")



################################# Women in parliament ###########################

Prop_women<- WDI(indicator = "SG.GEN.PARL.ZS")

################################# Trade #######################################

Trade <- WDI(indicator = "NE.TRD.GNFS.ZS")


################################# Investment ##################################
Investment <- WDI(indicator = "NE.GDI.TOTL.ZS" )


################################# Total School enrollment ######################

School_enrol<- WDI(indicator ="SE.PRM.ENRR" )

################################# School GPI ##################################


School_GPI <- WDI(indicator = "SE.ENR.PRSC.FM.ZS")

################################# R&D ##########################################

Res_Dev<- WDI(indicator = "GB.XPD.RSDV.GD.ZS")


################################# Population growth: ###########################


Pop_growth <- WDI(indicator = "SP.POP.GROW")




####################### Women, Business and the Law #############################


WBL_ind <- WDI(indicator = "SG.LAW.INDX")

#Filter

WBL_ind<- WBL_ind %>%
  filter(year >= 1997)





############# GINI INDEX ******************************************************


Gini_ind <- WDI(indicator ="SI.POV.GINI")

Gini_ind <- Gini_ind %>% 
  filter(year >= 1997)

################################# Mercosur #####################################

Mercosure <- c()


################################ Merging the datasets ##########################



result_df <- GDP_Growth

result_df <- result_df %>%
  left_join(Prop_women, by = c("iso2c", "year")) %>%
  left_join(Trade, by = c("iso2c", "year")) %>%
  left_join(Investment, by = c("iso2c", "year")) %>%
  left_join(GDP_cap, by = c("iso2c", "year")) %>%
  left_join(WBL_ind, by = c("iso2c", "year"))
  
result_df <- result_df %>% 
  select(-iso3c.y.y.y,-country.y.y.y,-iso3c.x.x.x,-country.x.x.x,-iso3c.y.y,-country.y.y,-iso3c.x.x,-country.x.x,-iso3c.y,-country.y,-iso3c.x,country.x)

result_df <- result_df %>% 
  select(-country.x)


All_count_Res <- result_df %>% 
  filter(iso2c %in% All_countries_list)



All_count_Res <- All_count_Res %>% 
  rename(GDP_Growth=NY.GDP.MKTP.KD.ZG, 
  Prop_Women = SG.GEN.PARL.ZS,
  Trade = NE.TRD.GNFS.ZS,
  Capital_formation = NE.GDI.TOTL.ZS,
  GDP_Cap=NY.GDP.PCAP.CD,
  WBL_ind=SG.LAW.INDX)

All_count_Res_1997 <- All_count_Res %>% 
  filter(year >= 1997)

All_count_Res_1997 <- All_count_Res_1997 %>% 
  filter(year < 2023)
