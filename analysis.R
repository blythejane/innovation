
# Blythe Adamson and Emilie Sam
# FDA, EMA, and HAS

rm(list=ls())

library(readr)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(epitools)

# set working directory
setwd("~/Dropbox/School/PhD/Research Projects/French Innovation/analysis")


d <- read_csv("~/Dropbox/School/PhD/Research Projects/French Innovation/data/FDA_HAS_raw.csv", 
              col_types = cols(asmr_date = col_date(format = "%m/%d/%y"), 
              asmr_obtained = col_factor(levels = c("I","II", "III", "IV", "V")), 
              asmr_value_asked = col_factor(levels = c("I","II", "III", "IV", "V")), 
              ema_ma_date = col_date(format = "%m/%d/%y")))

d <- subset(d, (!is.na(d[,"approval_year"])) & (d$approval_year >2012)) #FDA breakthrough classification began in 2012

summary(as.factor(d$indication)) # how many cancer? 
d$cancer <- 0
d$cancer[d$indication =="Cancer"] <- 1

d$asmr_II[d$asmr_obtained == "I" | d$asmr_obtained == "III" | d$asmr_obtained == "IV" | d$asmr_obtained == "V"] <- 0
d$asmr_II[d$asmr_obtained == "II"] <- 1

# indicator for innovation based on ASMR value
d$has_i <- NA
d$has_i[d$asmr_obtained == "I" | d$asmr_obtained == "II" | d$asmr_obtained == "III"] <- 1 
d$has_i[d$asmr_obtained == "IV" | d$asmr_obtained == "V"] <- 0 

#2x2 contingency tables
table0 <- xtabs(~has_i+fda_breakthrough, data=d)
table0 # print table 
sum(d$has_i, na.rm= TRUE)
sum(d$fda_breakthrough, na.rm= TRUE)

table1 <- xtabs(~asmr_obtained+fda_breakthrough, data=d)
table1 

table2 <- xtabs(~ema_accelerated_assessment+fda_accelerated, data=d)
table2
epitab(table2, method = "oddsratio")

table3 <- table(d$ema_orphan, d$fda_orphan)
table3 <- xtabs(~ema_orphan+fda_orphan, data=d)
table3
epitab(table3, method = "oddsratio")



#------------------------------------------------------------------
# Drugs with discordant designations 

# trade name for discordant innovation pairs
d$trade_name[d$has_i==1 & d$fda_breakthrough==0]
d$trade_name[d$has_i==0 & d$fda_breakthrough==1]

d$discordant <- NA
d$discordant[d$has_i==1 & d$fda_breakthrough==0] <- 1
d$discordant[d$has_i==0 & d$fda_breakthrough==1] <- 1
d$discordant[d$has_i==0 & d$fda_breakthrough==0] <- 0
d$discordant[d$has_i==1 & d$fda_breakthrough==1] <- 0
sum(d$discordant, na.rm = TRUE)
xtabs(~discordant+ ema_orphan, data = d)
summary(d$discordant)

# discordant orphan designations
d$trade_name[d$ema_orphan==1 & d$fda_orphan==0]
d$trade_name[d$ema_orphan==0 & d$fda_orphan==1]



#########################################################################
#regression to predict discordancy in designation
summary(fit <- glm(discordant~,data=d,family=binomial()))

summary(fit <- glm(asmr_II~fda_breakthrough + fda_accelerated + fda_orphan,data=d,family=binomial()))
#########################################################################


ggplot(d, aes(ema_ma_date, prob_innovative)) +
  geom_smooth() + 
  labs(title="Probability of Innovative Classification",
       x ="Year of Approval", y = "Probability of ASMR I or II") +
  theme_classic()
