usa_00001 <- read.csv("usa_00005.csv")
install.packages("dplyr", verbose = T)
install.packages("ggplot2", verbose = T)
install.packages("ggthemes", verbose = T)
install.packages("expss", verbose = T)
install.packages("tidyr", verbose = T)
install.packages("fabricatr", verbose = T)
install.packages("coefplot", verbose = T)
install.packages("RColorBrewer", verbose = T)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(RColorBrewer)

#clean by age
data1 <- usa_00001 %>% select(YEAR, COUNTYFIP, STATEFIP,AGE,RACE, HISPAN, SEX, BPL, FTOTINC)
data1 <- data1 %>% filter(AGE > 16)

data2006 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2006) %>% filter(AGE < 19)
data2007 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2007) %>% filter(AGE < 19)
data2008 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2008) %>% filter(AGE < 19)
data2009 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2009) %>% filter(AGE < 19)
data2010 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2010) %>% filter(AGE < 19)
data2011 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2011) %>% filter(AGE < 19)
data2012 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2012) %>% filter(AGE < 19)
data2013 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2013) %>% filter(AGE < 19)
data2014 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2014) %>% filter(AGE < 19)
data2015 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2015) %>% filter(AGE < 19)
data2016 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2016) %>% filter(AGE < 19)
data2017 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2017) %>% filter(AGE < 19)
data2018 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2018) %>% filter(AGE < 19)
data2019 <- data1 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2019) %>% filter(AGE < 19)

data_by_age <- rbind(data2006, data2007, data2008, data2009, data2010, data2011, data2012, data2013, data2014, data2015, data2016, data2017, data2018, data2019)

#Remove dfs
rm(data2006, data2007, data2008, data2009, data2010, data2011, data2012, data2013, data2014, data2015, data2016, data2017, data2018, data2019)

##### ADDING MISSING COUNTIES ######
miss_data <- read.csv("cc-est2019-alldata-19.csv")
miss_data <- miss_data %>% filter(AGEGRP == 4)
miss_data1 <- miss_data %>% filter(COUNTY == 119)
miss_data2 <- miss_data %>% filter(COUNTY == 167)
miss_data <- rbind(miss_data1, miss_data2)

rm(miss_data1, miss_data2)

miss_data <- miss_data %>% select(STATE, COUNTY, YEAR, TOT_POP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, IA_MALE, IA_FEMALE, AA_MALE, AA_FEMALE, NA_MALE, NA_FEMALE, H_MALE, H_FEMALE)
miss_data$white <- miss_data$WA_MALE + miss_data$WA_FEMALE
miss_data$black <- miss_data$BA_MALE + miss_data$BA_FEMALE
miss_data$native_american <- miss_data$IA_MALE + miss_data$IA_FEMALE
miss_data$asian <- miss_data$AA_MALE + miss_data$AA_FEMALE
miss_data$hispanic <- miss_data$H_MALE + miss_data$H_FEMALE

miss_data <- miss_data %>% select(STATE, COUNTY, YEAR, white, black, native_american, asian, hispanic)

library(stringr)
miss_data$FIPS <- paste(miss_data$STATE, miss_data$COUNTY, sep = "")
miss_data$STATE <- NULL
miss_data$COUNTY <- NULL

miss_data <- miss_data %>% filter(YEAR > 2)

miss_data$YEAR <- ifelse(miss_data$YEAR == 3, 2010, ifelse(miss_data$YEAR == 4, 2011, ifelse(miss_data$YEAR==5,2012, ifelse(miss_data$YEAR==6, 2013, ifelse(miss_data$YEAR==7, 2014, ifelse(miss_data$YEAR==8, 2015, ifelse(miss_data$YEAR==9, 2016, ifelse(miss_data$YEAR==10, 2017, ifelse(miss_data$YEAR==11, 2018, 2019)))))))))

# adding SD missing data
miss_dataSD <- read.csv("cc-est2019-alldata-46.csv")

miss_dataSD <- miss_dataSD %>% filter(AGEGRP == 4)
miss_dataSD <- miss_dataSD %>% filter(COUNTY == 83)

miss_dataSD <- miss_dataSD %>% select(STATE, COUNTY, YEAR, TOT_POP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, IA_MALE, IA_FEMALE, AA_MALE, AA_FEMALE, NA_MALE, NA_FEMALE, H_MALE, H_FEMALE)
miss_dataSD$white <- miss_dataSD$WA_MALE + miss_dataSD$WA_FEMALE
miss_dataSD$black <- miss_dataSD$BA_MALE + miss_dataSD$BA_FEMALE
miss_dataSD$native_american <- miss_dataSD$IA_MALE + miss_dataSD$IA_FEMALE
miss_dataSD$asian <- miss_dataSD$AA_MALE + miss_dataSD$AA_FEMALE
miss_dataSD$hispanic <- miss_dataSD$H_MALE + miss_dataSD$H_FEMALE

miss_dataSD <- miss_dataSD %>% select(STATE, COUNTY, YEAR, white, black, native_american, asian, hispanic)

miss_dataSD$FIPS <- paste(miss_dataSD$STATE, miss_dataSD$COUNTY, sep = "")
miss_dataSD$STATE <- NULL
miss_dataSD$COUNTY <- NULL

miss_dataSD <- miss_dataSD %>% filter(YEAR > 2)

miss_dataSD$YEAR <- ifelse(miss_dataSD$YEAR == 3, 2010, ifelse(miss_dataSD$YEAR == 4, 2011, ifelse(miss_dataSD$YEAR==5,2012, ifelse(miss_dataSD$YEAR==6, 2013, ifelse(miss_dataSD$YEAR==7, 2014, ifelse(miss_dataSD$YEAR==8, 2015, ifelse(miss_dataSD$YEAR==9, 2016, ifelse(miss_dataSD$YEAR==10, 2017, ifelse(miss_dataSD$YEAR==11, 2018, 2019)))))))))

#making data into percentages
miss_data$total_pop <- miss_data$white + miss_data$black + miss_data$native_american + miss_data$asian + miss_data$hispanic
miss_data$white <- miss_data$white / miss_data$total_pop
miss_data$black <- miss_data$black / miss_data$total_pop
miss_data$native_american <- miss_data$native_american / miss_data$total_pop
miss_data$asian <- miss_data$asian / miss_data$total_pop
miss_data$hispanic <- miss_data$hispanic / miss_data$total_pop

miss_data$other <- miss_data$black + miss_data$native_american + miss_data$asian

miss_data$black <- NULL
miss_data$native_american <- NULL
miss_data$asian <- NULL

#making SD into percentages
miss_dataSD$total_pop <- miss_dataSD$white + miss_dataSD$black + miss_dataSD$native_american + miss_dataSD$asian + miss_dataSD$hispanic
miss_dataSD$white <- miss_dataSD$white / miss_dataSD$total_pop
miss_dataSD$black <- miss_dataSD$black / miss_dataSD$total_pop
miss_dataSD$native_american <- miss_dataSD$native_american / miss_dataSD$total_pop
miss_dataSD$asian <- miss_dataSD$asian / miss_dataSD$total_pop
miss_dataSD$hispanic <- miss_dataSD$hispanic / miss_dataSD$total_pop

miss_dataSD$other <- miss_dataSD$black + miss_dataSD$native_american + miss_dataSD$asian

miss_dataSD$black <- NULL
miss_dataSD$native_american <- NULL
miss_dataSD$asian <- NULL

#combing the two dfs
miss_data <- rbind(miss_data, miss_dataSD)
rm(miss_dataSD)

#Summarizing data
miss_data <- miss_data %>% select(YEAR, white, hispanic, other, total_pop, FIPS) %>% group_by(YEAR) %>% summarise(white = mean(white), hispanic = mean(hispanic), other = mean(other), total_pop = sum(total_pop))

#Make race binary variables
data_by_age$white <- ifelse(data_by_age$RACE == 1, 1, 0)
data_by_age$black <- ifelse(data_by_age$RACE == 2, 1, 0)
data_by_age$native_american <- ifelse(data_by_age$RACE == 3, 1, 0)
data_by_age$asian <- ifelse(data_by_age$RACE == 4 | data_by_age$RACE == 5 | data_by_age$RACE == 6, 1, 0)


# add 0 to county and state codes
data_by_age$STATEFIP <- str_pad(data_by_age$STATEFIP, 2, pad = "0")
data_by_age$COUNTYFIP <- str_pad(data_by_age$COUNTYFIP, 3, pad = "0")

#merge state and county columns
data_by_age$FIPS <- paste(data_by_age$STATEFIP, data_by_age$COUNTYFIP, sep = "")
#eliminate state and county columns, we now have FIPS column
data_by_age$COUNTYFIP <- NULL
data_by_age$STATEFIP <- NULL

# NWC data
library(readxl)
id_zip_w_FIPS_ <- read_excel("id & zip (w FIPS).xlsx")

id_zip_w_FIPS_$STCOUNTYFP <- str_pad(id_zip_w_FIPS_$STCOUNTYFP, 5, pad = "0")
id_zip_w_FIPS_ <- id_zip_w_FIPS_ %>% drop_na(STCOUNTYFP)

#figure out groups
length(unique(id_zip_w_FIPS_$STCOUNTYFP))
## there are 342 unique FIPS codes

# data frame of FIPS codes ordered by frequency
FIPS_by_freq <- sort(table(id_zip_w_FIPS_$STCOUNTYFP),decreasing=T)
FIPS_by_freq <- as.data.frame(FIPS_by_freq)
FIPS_by_freq$Var1 <- as.numeric(as.character(FIPS_by_freq$Var1))

# adding 0 to the Fips codes
FIPS_by_freq$Var1 <- str_pad(FIPS_by_freq$Var1, 5, pad = "0")

######### BELOW IS ONLY FOR REGRESSIONS BASED ON GROUPS ######### 

#separate into quintiles 
g1 <- FIPS_by_freq[1:7,]
g2 <- FIPS_by_freq[8:46,]
g3 <- FIPS_by_freq[47:342,]

# Add IPUMS data to each quintile and drop NA rows
g1 <- left_join(g1, data_by_age, by=c("Var1" = "FIPS"))
g1 <- g1 %>% drop_na(YEAR)
g2 <- left_join(g2, data_by_age, by=c("Var1" = "FIPS"))
g2 <- g2 %>% drop_na(YEAR)
g3 <- left_join(g3, data_by_age, by=c("Var1" = "FIPS"))
g3 <- g3 %>% drop_na(YEAR)

# Making BPL binary
g1$foreign <- ifelse(g1$BPL > 120, 1, 0)
g1$BPL <- NULL
g2$foreign <- ifelse(g2$BPL > 120, 1, 0)
g2$BPL <- NULL
g3$foreign <- ifelse(g3$BPL > 120, 1, 0)
g3$BPL <- NULL

# Fixing hispanic data
g1$hispanic <- ifelse(g1$HISPAN == 1 | g1$HISPAN == 2 | g1$HISPAN == 3 | g1$HISPAN == 4, 1, 0)
g2$hispanic <- ifelse(g2$HISPAN == 1 | g2$HISPAN == 2 | g2$HISPAN == 3 | g2$HISPAN == 4, 1, 0)
g3$hispanic <- ifelse(g3$HISPAN == 1 | g3$HISPAN == 2 | g3$HISPAN == 3 | g3$HISPAN == 4, 1, 0)

g1$white <- ifelse(g1$white == 1 & g1$hispanic == 1, 0, ifelse(g1$white == 1, 1, 0))
g2$white <- ifelse(g2$white == 1 & g2$hispanic == 1, 0, ifelse(g2$white == 1, 1, 0))
g3$white <- ifelse(g3$white == 1 & g3$hispanic == 1, 0, ifelse(g3$white == 1, 1, 0))

# Summarizing data 
g1_summarized <- g1 %>% select(YEAR, white, black, asian, native_american, hispanic, foreign) %>% group_by(YEAR)%>% summarise(avg_white = mean(white), avg_black = mean(black), avg_asian = mean(asian), avg_nat = mean(native_american), avg_hispanic = mean(hispanic), avg_foreign = mean(foreign))
g2_summarized <- g2 %>% select(YEAR, white, black, asian, native_american, hispanic, foreign) %>% group_by(YEAR)%>% summarise(avg_white = mean(white), avg_black = mean(black), avg_asian = mean(asian), avg_nat = mean(native_american), avg_hispanic = mean(hispanic), avg_foreign = mean(foreign))
g3_summarized <- g3 %>% select(YEAR, white, black, asian, native_american, hispanic, foreign) %>% group_by(YEAR)%>% summarise(avg_white = mean(white), avg_black = mean(black), avg_asian = mean(asian), avg_nat = mean(native_american), avg_hispanic = mean(hispanic), avg_foreign = mean(foreign))

#Making white, hispanic and other
g1_summarized$avg_other <- g1_summarized$avg_black + g1_summarized$avg_nat + g1_summarized$avg_asian
g1_summarized$avg_black <- NULL
g1_summarized$avg_nat <- NULL
g1_summarized$avg_asian <- NULL
g2_summarized$avg_other <- g2_summarized$avg_black + g2_summarized$avg_nat + g2_summarized$avg_asian
g2_summarized$avg_black <- NULL
g2_summarized$avg_nat <- NULL
g2_summarized$avg_asian <- NULL
g3_summarized$avg_other <- g3_summarized$avg_black + g3_summarized$avg_nat + g3_summarized$avg_asian
g3_summarized$avg_black <- NULL
g3_summarized$avg_nat <- NULL
g3_summarized$avg_asian <- NULL

#Merging dfs
g1_summarized <-  left_join(g1_summarized, miss_data, by="YEAR")

g1_summarized$g1_pop <- c(NA, NA, NA, NA, 6127, 6726, 6420, 6415, 6247, 6299, 6094, 6972, 5998, 5676)
g1_summarized$total <- g1_summarized$total_pop + g1_summarized$g1_pop
g1_summarized$q1_w <- g1_summarized$g1_pop / g1_summarized$total
g1_summarized$md_w <- g1_summarized$total_pop / g1_summarized$total
g1_summarized$q1_pop <- NULL
g1_summarized$total <- NULL
g1_summarized$total_pop <- NULL

g1_summarized$per_white <- (g1_summarized$avg_white * g1_summarized$q1_w) + (g1_summarized$white * g1_summarized$md_w)
g1_summarized$per_hispanic <- (g1_summarized$avg_hispanic * g1_summarized$q1_w) + (g1_summarized$hispanic * g1_summarized$md_w)
g1_summarized$per_other <- (g1_summarized$avg_other * g1_summarized$q1_w) + (g1_summarized$other * g1_summarized$md_w)

g1_summarized <- g1_summarized %>% filter(YEAR > 2009) %>% select(YEAR, per_white, per_hispanic, per_other, avg_foreign)

#Creating g1_control
g1_control <- FIPS_by_freq[c(1:8, 10:15, 18:25),]
g1_control <- left_join(g1_control, data_by_age, by=c("Var1" = "FIPS"))
g1_control <- g1_control %>% drop_na(YEAR)
g1_control$foreign <- ifelse(g1_control$BPL > 120, 1, 0)
g1_control$BPL <- NULL
g1_control$hispanic <- ifelse(g1_control$HISPAN == 1 | g1_control$HISPAN == 2 | g1_control$HISPAN == 3 | g1_control$HISPAN == 4, 1, 0)
g1_control$white <- ifelse(g1_control$white == 1 & g1_control$hispanic == 1, 0, ifelse(g1_control$white == 1, 1, 0))

g1_c_summarized <- g1_control %>% select(YEAR, white, black, asian, native_american, hispanic, foreign) %>% group_by(YEAR)%>% summarise(avg_white = mean(white), avg_black = mean(black), avg_asian = mean(asian), avg_nat = mean(native_american), avg_hispanic = mean(hispanic), avg_foreign = mean(foreign))
g1_c_summarized$avg_other <- g1_c_summarized$avg_black + g1_c_summarized$avg_nat + g1_c_summarized$avg_asian
g1_c_summarized$avg_black <- NULL
g1_c_summarized$avg_nat <- NULL
g1_c_summarized$avg_asian <- NULL

g1_c_summarized <-  left_join(g1_c_summarized, miss_data, by="YEAR")

g1_c_summarized$g1_c_pop <- c(NA, NA, NA, NA, 1542, 1639, 1495, 1443, 1522, 1634, 1537, 1589, 1574, 1592)
g1_c_summarized$total <- g1_c_summarized$total_pop + g1_c_summarized$g1_c_pop
g1_c_summarized$q1_w <- g1_c_summarized$g1_c_pop / g1_c_summarized$total
g1_c_summarized$md_w <- g1_c_summarized$total_pop / g1_c_summarized$total
g1_c_summarized$q1_pop <- NULL
g1_c_summarized$total <- NULL
g1_c_summarized$total_pop <- NULL

g1_c_summarized$per_white <- (g1_c_summarized$avg_white * g1_c_summarized$q1_w) + (g1_c_summarized$white * g1_c_summarized$md_w)
g1_c_summarized$per_hispanic <- (g1_c_summarized$avg_hispanic * g1_c_summarized$q1_w) + (g1_c_summarized$hispanic * g1_c_summarized$md_w)
g1_c_summarized$per_other <- (g1_c_summarized$avg_other * g1_c_summarized$q1_w) + (g1_c_summarized$other * g1_c_summarized$md_w)

g1_c_summarized <- g1_c_summarized %>% filter(YEAR > 2009) %>% select(YEAR, per_white, per_hispanic, per_other, avg_foreign)

################################### Begin Regressions ###################################
# Remove data.frames no longer in use
rm(data_by_age, data1, FIPS_by_freq, usa_00001, id_zip_w_FIPS_, miss_data)

###based on g1
ggplot(aes(x=YEAR, y=per_white), data = g1_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of White People", title = "Percentage of White People in Group 1 counties") + theme_economist()
reg_g1_white <- lm(per_white ~ YEAR, data = g1_summarized)
summary(reg_g1_white)

ggplot(aes(x=YEAR, y=per_hispanic), data = g1_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of Hispanic People", title = "Percentage of Hispanic People in Group 1 counties") + theme_economist()
reg_g1_hispanic <- lm(per_hispanic ~ YEAR, data = g1_summarized)
summary(reg_g1_hispanic)

ggplot(aes(x=YEAR, y=per_other), data = g1_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of Other Races", title = "Percentage of Other Races in Group 1 counties") + theme_economist()
reg_g1_other <- lm(per_other ~ YEAR, data = g1_summarized)
summary(reg_g1_other)

ggplot(aes(x=YEAR, y=avg_foreign), data = g1_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of Foreign-Born People", title = "Percentage of Foreign Born People in Group 1 counties") + theme_economist()
reg_g1_foreign <- lm(avg_foreign ~ YEAR, data = g1_summarized)
summary(reg_g1_foreign)

###based on g2
ggplot(aes(x=YEAR, y=avg_white), data = g2_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of White People", title = "Percentage of White People in Group 2 counties") + theme_economist()
reg_g2_white <- lm(avg_white ~ YEAR, data = g2_summarized)
summary(reg_g2_white)

ggplot(aes(x=YEAR, y=avg_hispanic), data = g2_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of Hispanic People", title = "Percentage of Hispanic People in Group 2 counties") + theme_economist()
reg_g2_hispanic <- lm(avg_hispanic ~ YEAR, data = g2_summarized)
summary(reg_g2_hispanic)

ggplot(aes(x=YEAR, y=avg_other), data = g2_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of Other Races", title = "Percentage of Other Races in Group 2 counties") + theme_economist()
reg_g2_other <- lm(avg_other ~ YEAR, data = g2_summarized)
summary(reg_g2_other)

ggplot(aes(x=YEAR, y=avg_foreign), data = g2_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of Foreign-Born People", title = "Percentage of Foreign Born People in Group 2 counties") + theme_economist()
reg_g2_foreign <- lm(avg_foreign ~ YEAR, data = g2_summarized)
summary(reg_g2_foreign)

### based on g3
ggplot(aes(x=YEAR, y=avg_white), data = g3_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of White People", title = "Percentage of White People in Group 3 counties") + theme_economist()
reg_g3_white <- lm(avg_white ~ YEAR, data = g3_summarized)
summary(reg_g3_white)

ggplot(aes(x=YEAR, y=avg_hispanic), data = g3_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of Hispanic People", title = "Percentage of Hispanic People in Group 3 counties") + theme_economist()
reg_g3_hispanic <- lm(avg_hispanic ~ YEAR, data = g3_summarized)
summary(reg_g3_hispanic)

ggplot(aes(x=YEAR, y=avg_other), data = g3_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of Other Races", title = "Percentage of Other Races in Group 3 counties") + theme_economist()
reg_g3_other <- lm(avg_other ~ YEAR, data = g3_summarized)
summary(reg_g3_other)

ggplot(aes(x=YEAR, y=avg_foreign), data = g3_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of Foreign-Born People", title = "Percentage of Foreign Born People in Group 3 counties") + theme_economist()
reg_g3_foreign <- lm(avg_foreign ~ YEAR, data = g3_summarized)
summary(reg_g3_foreign)

### based on g1_control
ggplot(aes(x=YEAR, y=per_white), data = g1_c_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of White People", title = "Percentage of White People in Group 1 (controlled) counties") + theme_economist()
reg_g1c_white <- lm(per_white ~ YEAR, data = g1_c_summarized)
summary(reg_g1c_white)

ggplot(aes(x=YEAR, y=per_hispanic), data = g1_c_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of Hispanic People", title = "Percentage of Hispanic People in Group 1 (controlled) counties") + theme_economist()
reg_g1c_hispanic <- lm(per_hispanic ~ YEAR, data = g1_c_summarized)
summary(reg_g1c_hispanic)

ggplot(aes(x=YEAR, y=per_other), data = g1_c_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of Other Races", title = "Percentage of Other Races in Group 1 (controlled) counties") + theme_economist()
reg_g1c_other <- lm(per_other ~ YEAR, data = g1_c_summarized)
summary(reg_g1c_other)

ggplot(aes(x=YEAR, y=avg_foreign), data = g1_c_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of Foreign-Born People", title = "Percentage of Foreign Born People in Group 1 (controlled) counties") + theme_economist()
reg_g1c_foreign <- lm(avg_foreign ~ YEAR, data = g1_c_summarized)
summary(reg_g1c_foreign)


##################### exporting data.frames
write.csv(g1,"g1.csv", row.names = FALSE)
write.csv(g2,"g2.csv", row.names = FALSE)
write.csv(g3,"g3.csv", row.names = FALSE)
write.csv(g1_control,"g1_control.csv", row.names = FALSE)
write.csv(g1_summarized,"g1_summarized.csv", row.names = FALSE)
write.csv(g2_summarized,"g2_summarized.csv", row.names = FALSE)
write.csv(g3_summarized,"g3_summarized.csv", row.names = FALSE)
write.csv(g1_c_summarized,"g1_c_summarized.csv", row.names = FALSE)

############################## 2020 and beyond ###################
Data2 <- read.csv("usa_00005.csv")

#clean by age
data2 <- Data2 %>% select(YEAR, COUNTYFIP, STATEFIP,AGE,RACE, HISPAN, SEX, BPL, FTOTINC)
data2 <- data2 %>% filter(AGE > 2)

data2006 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2006) %>% filter(AGE < 5)
data2007 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2007) %>% filter(AGE < 5)
data2008 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2008) %>% filter(AGE < 5)
data2009 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2009) %>% filter(AGE < 5)
data2010 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2010) %>% filter(AGE < 5)
data2011 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2011) %>% filter(AGE < 5)
data2012 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2012) %>% filter(AGE < 5)
data2013 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2013) %>% filter(AGE < 5)
data2014 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2014) %>% filter(AGE < 5)
data2015 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2015) %>% filter(AGE < 5)
data2016 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2016) %>% filter(AGE < 5)
data2017 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2017) %>% filter(AGE < 5)
data2018 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2018) %>% filter(AGE < 5)
data2019 <- data2 %>% select(YEAR, AGE, RACE, HISPAN, COUNTYFIP, STATEFIP, SEX, BPL, FTOTINC) %>% filter(YEAR==2019) %>% filter(AGE < 5)

data_by_age2 <- rbind(data2006, data2007, data2008, data2009, data2010, data2011, data2012, data2013, data2014, data2015, data2016, data2017, data2018, data2019)

#Remove dfs
rm(data2006, data2007, data2008, data2009, data2010, data2011, data2012, data2013, data2014, data2015, data2016, data2017, data2018, data2019)

##### ADDING MISSING COUNTIES ######
miss_data <- read.csv("cc-est2019-alldata-19.csv")
miss_data <- miss_data %>% filter(AGEGRP == 1)
miss_data1 <- miss_data %>% filter(COUNTY == 119)
miss_data2 <- miss_data %>% filter(COUNTY == 167)
miss_data <- rbind(miss_data1, miss_data2)

rm(miss_data1, miss_data2)

miss_data <- miss_data %>% select(STATE, COUNTY, YEAR, TOT_POP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, IA_MALE, IA_FEMALE, AA_MALE, AA_FEMALE, NA_MALE, NA_FEMALE, H_MALE, H_FEMALE)
miss_data$white <- miss_data$WA_MALE + miss_data$WA_FEMALE
miss_data$black <- miss_data$BA_MALE + miss_data$BA_FEMALE
miss_data$native_american <- miss_data$IA_MALE + miss_data$IA_FEMALE
miss_data$asian <- miss_data$AA_MALE + miss_data$AA_FEMALE
miss_data$hispanic <- miss_data$H_MALE + miss_data$H_FEMALE

miss_data <- miss_data %>% select(STATE, COUNTY, YEAR, white, black, native_american, asian, hispanic)

library(stringr)
miss_data$FIPS <- paste(miss_data$STATE, miss_data$COUNTY, sep = "")
miss_data$STATE <- NULL
miss_data$COUNTY <- NULL

miss_data <- miss_data %>% filter(YEAR > 2)

miss_data$YEAR <- ifelse(miss_data$YEAR == 3, 2010, ifelse(miss_data$YEAR == 4, 2011, ifelse(miss_data$YEAR==5,2012, ifelse(miss_data$YEAR==6, 2013, ifelse(miss_data$YEAR==7, 2014, ifelse(miss_data$YEAR==8, 2015, ifelse(miss_data$YEAR==9, 2016, ifelse(miss_data$YEAR==10, 2017, ifelse(miss_data$YEAR==11, 2018, 2019)))))))))

# adding SD missing data
miss_dataSD <- read.csv("cc-est2019-alldata-46.csv")

miss_dataSD <- miss_dataSD %>% filter(AGEGRP == 1)
miss_dataSD <- miss_dataSD %>% filter(COUNTY == 83)

miss_dataSD <- miss_dataSD %>% select(STATE, COUNTY, YEAR, TOT_POP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, IA_MALE, IA_FEMALE, AA_MALE, AA_FEMALE, NA_MALE, NA_FEMALE, H_MALE, H_FEMALE)
miss_dataSD$white <- miss_dataSD$WA_MALE + miss_dataSD$WA_FEMALE
miss_dataSD$black <- miss_dataSD$BA_MALE + miss_dataSD$BA_FEMALE
miss_dataSD$native_american <- miss_dataSD$IA_MALE + miss_dataSD$IA_FEMALE
miss_dataSD$asian <- miss_dataSD$AA_MALE + miss_dataSD$AA_FEMALE
miss_dataSD$hispanic <- miss_dataSD$H_MALE + miss_dataSD$H_FEMALE

miss_dataSD <- miss_dataSD %>% select(STATE, COUNTY, YEAR, white, black, native_american, asian, hispanic)

miss_dataSD$FIPS <- paste(miss_dataSD$STATE, miss_dataSD$COUNTY, sep = "")
miss_dataSD$STATE <- NULL
miss_dataSD$COUNTY <- NULL

miss_dataSD <- miss_dataSD %>% filter(YEAR > 2)

miss_dataSD$YEAR <- ifelse(miss_dataSD$YEAR == 3, 2010, ifelse(miss_dataSD$YEAR == 4, 2011, ifelse(miss_dataSD$YEAR==5,2012, ifelse(miss_dataSD$YEAR==6, 2013, ifelse(miss_dataSD$YEAR==7, 2014, ifelse(miss_dataSD$YEAR==8, 2015, ifelse(miss_dataSD$YEAR==9, 2016, ifelse(miss_dataSD$YEAR==10, 2017, ifelse(miss_dataSD$YEAR==11, 2018, 2019)))))))))

#making data into percentages
miss_data$total_pop <- miss_data$white + miss_data$black + miss_data$native_american + miss_data$asian + miss_data$hispanic
miss_data$white <- miss_data$white / miss_data$total_pop
miss_data$black <- miss_data$black / miss_data$total_pop
miss_data$native_american <- miss_data$native_american / miss_data$total_pop
miss_data$asian <- miss_data$asian / miss_data$total_pop
miss_data$hispanic <- miss_data$hispanic / miss_data$total_pop

miss_data$other <- miss_data$black + miss_data$native_american + miss_data$asian

miss_data$black <- NULL
miss_data$native_american <- NULL
miss_data$asian <- NULL

#making SD into percentages
miss_dataSD$total_pop <- miss_dataSD$white + miss_dataSD$black + miss_dataSD$native_american + miss_dataSD$asian + miss_dataSD$hispanic
miss_dataSD$white <- miss_dataSD$white / miss_dataSD$total_pop
miss_dataSD$black <- miss_dataSD$black / miss_dataSD$total_pop
miss_dataSD$native_american <- miss_dataSD$native_american / miss_dataSD$total_pop
miss_dataSD$asian <- miss_dataSD$asian / miss_dataSD$total_pop
miss_dataSD$hispanic <- miss_dataSD$hispanic / miss_dataSD$total_pop

miss_dataSD$other <- miss_dataSD$black + miss_dataSD$native_american + miss_dataSD$asian

miss_dataSD$black <- NULL
miss_dataSD$native_american <- NULL
miss_dataSD$asian <- NULL

#combing the two dfs
miss_data <- rbind(miss_data, miss_dataSD)
rm(miss_dataSD)

#Summarizing data
miss_data <- miss_data %>% select(YEAR, white, hispanic, other, total_pop, FIPS) %>% group_by(YEAR) %>% summarise(white = mean(white), hispanic = mean(hispanic), other = mean(other), total_pop = sum(total_pop))

#Make race binary variables
data_by_age2$white <- ifelse(data_by_age2$RACE == 1, 1, 0)
data_by_age2$black <- ifelse(data_by_age2$RACE == 2, 1, 0)
data_by_age2$native_american <- ifelse(data_by_age2$RACE == 3, 1, 0)
data_by_age2$asian <- ifelse(data_by_age2$RACE == 4 | data_by_age2$RACE == 5 | data_by_age2$RACE == 6, 1, 0)


# add 0 to county and state codes
data_by_age2$STATEFIP <- str_pad(data_by_age2$STATEFIP, 2, pad = "0")
data_by_age2$COUNTYFIP <- str_pad(data_by_age2$COUNTYFIP, 3, pad = "0")

#merge state and county columns
data_by_age2$FIPS <- paste(data_by_age2$STATEFIP, data_by_age2$COUNTYFIP, sep = "")
#eliminate state and county columns, we now have FIPS column
data_by_age2$COUNTYFIP <- NULL
data_by_age2$STATEFIP <- NULL

# nwc data
library(readxl)
id_zip_w_FIPS_ <- read_excel("id & zip (w FIPS).xslx")

id_zip_w_FIPS_$STCOUNTYFP <- str_pad(id_zip_w_FIPS_$STCOUNTYFP, 5, pad = "0")
id_zip_w_FIPS_ <- id_zip_w_FIPS_ %>% drop_na(STCOUNTYFP)

#figure out Group
length(unique(id_zip_w_FIPS_$STCOUNTYFP))
## there are 342 unique FIPS codes

# data frame of FIPS codes ordered by frequency
FIPS_by_freq <- sort(table(id_zip_w_FIPS_$STCOUNTYFP),decreasing=T)
FIPS_by_freq <- as.data.frame(FIPS_by_freq)
FIPS_by_freq$Var1 <- as.numeric(as.character(FIPS_by_freq$Var1))

# adding 0 to the Fips codes
FIPS_by_freq$Var1 <- str_pad(FIPS_by_freq$Var1, 5, pad = "0")

#separate into groups
g4 <- FIPS_by_freq[1:7,]
g5 <- FIPS_by_freq[8:47,]
g6 <- FIPS_by_freq[48:342,]

# Add IPUMS data to each quintile and drop NA rows
g4 <- left_join(g4, data_by_age2, by=c("Var1" = "FIPS"))
g4 <- g4 %>% drop_na(YEAR)
g5 <- left_join(g5, data_by_age2, by=c("Var1" = "FIPS"))
g5 <- g5 %>% drop_na(YEAR)
g6 <- left_join(g6, data_by_age2, by=c("Var1" = "FIPS"))
g6 <- g6 %>% drop_na(YEAR)

# Making BPL binary
g4$foreign <- ifelse(g4$BPL > 120, 1, 0)
g4$BPL <- NULL
g5$foreign <- ifelse(g5$BPL > 120, 1, 0)
g5$BPL <- NULL
g6$foreign <- ifelse(g6$BPL > 120, 1, 0)
g6$BPL <- NULL

# Fixing hispanic data
g4$hispanic <- ifelse(g4$HISPAN == 1 | g4$HISPAN == 2 | g4$HISPAN == 3 | g4$HISPAN == 4, 1, 0)
g5$hispanic <- ifelse(g5$HISPAN == 1 | g5$HISPAN == 2 | g5$HISPAN == 3 | g5$HISPAN == 4, 1, 0)
g6$hispanic <- ifelse(g6$HISPAN == 1 | g6$HISPAN == 2 | g6$HISPAN == 3 | g6$HISPAN == 4, 1, 0)

g4$white <- ifelse(g4$white == 1 & g4$hispanic == 1, 0, ifelse(g4$white == 1, 1, 0))
g5$white <- ifelse(g5$white == 1 & g5$hispanic == 1, 0, ifelse(g5$white == 1, 1, 0))
g6$white <- ifelse(g6$white == 1 & g6$hispanic == 1, 0, ifelse(g6$white == 1, 1, 0))

# Summarizing data 
g4_summarized <- g4 %>% select(YEAR, white, black, asian, native_american, hispanic, foreign) %>% group_by(YEAR)%>% summarise(avg_white = mean(white), avg_black = mean(black), avg_asian = mean(asian), avg_nat = mean(native_american), avg_hispanic = mean(hispanic), avg_foreign = mean(foreign))
g5_summarized <- g5 %>% select(YEAR, white, black, asian, native_american, hispanic, foreign) %>% group_by(YEAR)%>% summarise(avg_white = mean(white), avg_black = mean(black), avg_asian = mean(asian), avg_nat = mean(native_american), avg_hispanic = mean(hispanic), avg_foreign = mean(foreign))
g6_summarized <- g6 %>% select(YEAR, white, black, asian, native_american, hispanic, foreign) %>% group_by(YEAR)%>% summarise(avg_white = mean(white), avg_black = mean(black), avg_asian = mean(asian), avg_nat = mean(native_american), avg_hispanic = mean(hispanic), avg_foreign = mean(foreign))

#Making white, hispanic and other
g4_summarized$avg_other <- g4_summarized$avg_black + g4_summarized$avg_nat + g4_summarized$avg_asian
g4_summarized$avg_black <- NULL
g4_summarized$avg_nat <- NULL
g4_summarized$avg_asian <- NULL

g5_summarized$avg_other <- g5_summarized$avg_black + g5_summarized$avg_nat + g5_summarized$avg_asian
g5_summarized$avg_black <- NULL
g5_summarized$avg_nat <- NULL
g5_summarized$avg_asian <- NULL

g6_summarized$avg_other <- g6_summarized$avg_black + g6_summarized$avg_nat + g6_summarized$avg_asian
g6_summarized$avg_black <- NULL
g6_summarized$avg_nat <- NULL
g6_summarized$avg_asian <- NULL

#Merging dfs
g4_summarized <-  left_join(g4_summarized, miss_data, by="YEAR")

g4_summarized$g4_pop <- c(NA, NA, NA, NA, 5383, 5324, 5265, 5230, 4995, 4773, 4780, 4704, 4657, 4479)
g4_summarized$total <- g4_summarized$total_pop + g4_summarized$g4_pop
g4_summarized$q1_w <- g4_summarized$g4_pop / g4_summarized$total
g4_summarized$md_w <- g4_summarized$total_pop / g4_summarized$total
g4_summarized$q1_pop <- NULL
g4_summarized$total <- NULL
g4_summarized$total_pop <- NULL

g4_summarized$per_white <- (g4_summarized$avg_white * g4_summarized$q1_w) + (g4_summarized$white * g4_summarized$md_w)
g4_summarized$per_hispanic <- (g4_summarized$avg_hispanic * g4_summarized$q1_w) + (g4_summarized$hispanic * g4_summarized$md_w)
g4_summarized$per_other <- (g4_summarized$avg_other * g4_summarized$q1_w) + (g4_summarized$other * g4_summarized$md_w)

g4_summarized <- g4_summarized %>% filter(YEAR > 2009) %>% select(YEAR, per_white, per_hispanic, per_other, avg_foreign)

#Creating g1_control
g4_control <- FIPS_by_freq[c(1:8, 10:15, 18:25),]
g4_control <- left_join(g4_control, data_by_age2, by=c("Var1" = "FIPS"))
g4_control <- g4_control %>% drop_na(YEAR)
g4_control$foreign <- ifelse(g4_control$BPL > 120, 1, 0)
g4_control$BPL <- NULL
g4_control$hispanic <- ifelse(g4_control$HISPAN == 1 | g4_control$HISPAN == 2 | g4_control$HISPAN == 3 | g4_control$HISPAN == 4, 1, 0)
g4_control$white <- ifelse(g4_control$white == 1 & g4_control$hispanic == 1, 0, ifelse(g4_control$white == 1, 1, 0))

g4_c_summarized <- g4_control %>% select(YEAR, white, black, asian, native_american, hispanic, foreign) %>% group_by(YEAR)%>% summarise(avg_white = mean(white), avg_black = mean(black), avg_asian = mean(asian), avg_nat = mean(native_american), avg_hispanic = mean(hispanic), avg_foreign = mean(foreign))
g4_c_summarized$avg_other <- g4_c_summarized$avg_black + g4_c_summarized$avg_nat + g4_c_summarized$avg_asian
g4_c_summarized$avg_black <- NULL
g4_c_summarized$avg_nat <- NULL
g4_c_summarized$avg_asian <- NULL

g4_c_summarized <-  left_join(g4_c_summarized, miss_data, by="YEAR")

g4_c_summarized$g4_c_pop <- c(NA, NA, NA, NA, 1471, 1398, 1375, 1381, 1319, 1334, 1378, 1365, 1320, 1261)
g4_c_summarized$total <- g4_c_summarized$total_pop + g4_c_summarized$g4_c_pop
g4_c_summarized$q1_w <- g4_c_summarized$g4_c_pop / g4_c_summarized$total
g4_c_summarized$md_w <- g4_c_summarized$total_pop / g4_c_summarized$total
g4_c_summarized$q1_pop <- NULL
g4_c_summarized$total <- NULL
g4_c_summarized$total_pop <- NULL

g4_c_summarized$per_white <- (g4_c_summarized$avg_white * g4_c_summarized$q1_w) + (g4_c_summarized$white * g4_c_summarized$md_w)
g4_c_summarized$per_hispanic <- (g4_c_summarized$avg_hispanic * g4_c_summarized$q1_w) + (g4_c_summarized$hispanic * g4_c_summarized$md_w)
g4_c_summarized$per_other <- (g4_c_summarized$avg_other * g4_c_summarized$q1_w) + (g4_c_summarized$other * g4_c_summarized$md_w)

g4_c_summarized <- g4_c_summarized %>% filter(YEAR > 2009) %>% select(YEAR, per_white, per_hispanic, per_other, avg_foreign)

#remove dfs no longer in use 
rm(data_by_age2, data2, Data2, FIPS_by_freq, id_zip_w_FIPS_, miss_data)

#changing 2010s to 2020s (YEAR)
g4_summarized$YEAR <- ifelse(g4_summarized$YEAR == "2010", "2023", ifelse(g4_summarized$YEAR=="2011", "2024", ifelse(g4_summarized$YEAR=="2012", "2025", ifelse(g4_summarized$YEAR=="2013", "2026", ifelse(g4_summarized$YEAR=="2014", "2027", ifelse(g4_summarized$YEAR=="2015", "2028", ifelse(g4_summarized$YEAR=="2016", "2029", ifelse(g4_summarized$YEAR=="2017", "2030", ifelse(g4_summarized$YEAR=="2018", "2031", ifelse(g4_summarized$YEAR=="2019", "2032", 0))))))))))
g4_c_summarized$YEAR <- ifelse(g4_c_summarized$YEAR == "2010", "2023", ifelse(g4_c_summarized$YEAR=="2011", "2024", ifelse(g4_c_summarized$YEAR=="2012", "2025", ifelse(g4_c_summarized$YEAR=="2013", "2026", ifelse(g4_c_summarized$YEAR=="2014", "2027", ifelse(g4_c_summarized$YEAR=="2015", "2028", ifelse(g4_c_summarized$YEAR=="2016", "2029", ifelse(g4_c_summarized$YEAR=="2017", "2030", ifelse(g4_c_summarized$YEAR=="2018", "2031", ifelse(g4_c_summarized$YEAR=="2019", "2032", 0))))))))))
g5_summarized$YEAR <- ifelse(g5_summarized$YEAR == "2010", "2024", ifelse(g5_summarized$YEAR=="2011", "2025", ifelse(g5_summarized$YEAR=="2012", "2026", ifelse(g5_summarized$YEAR=="2013", "2027", ifelse(g5_summarized$YEAR=="2014", "2028", ifelse(g5_summarized$YEAR=="2015", "2029", ifelse(g5_summarized$YEAR=="2016", "2030", ifelse(g5_summarized$YEAR=="2017", "2031", ifelse(g5_summarized$YEAR=="2018", "2032", ifelse(g5_summarized$YEAR=="2019", "2033", ifelse(g5_summarized$YEAR=="2009", "2023", ifelse(g5_summarized$YEAR=="2008", "2022", ifelse(g5_summarized$YEAR=="2007", "2021", ifelse(g5_summarized$YEAR=="2006", "2020", 0))))))))))))))
g6_summarized$YEAR <- ifelse(g6_summarized$YEAR == "2010", "2024", ifelse(g6_summarized$YEAR=="2011", "2025", ifelse(g6_summarized$YEAR=="2012", "2026", ifelse(g6_summarized$YEAR=="2013", "2027", ifelse(g6_summarized$YEAR=="2014", "2028", ifelse(g6_summarized$YEAR=="2015", "2029", ifelse(g6_summarized$YEAR=="2016", "2030", ifelse(g6_summarized$YEAR=="2017", "2031", ifelse(g6_summarized$YEAR=="2018", "2032", ifelse(g6_summarized$YEAR=="2019", "2033", ifelse(g6_summarized$YEAR=="2009", "2023", ifelse(g6_summarized$YEAR=="2008", "2022", ifelse(g6_summarized$YEAR=="2007", "2021", ifelse(g6_summarized$YEAR=="2006", "2020", 0))))))))))))))

################################# BEGIN REGRESSIONS ###########################################
###based on g4
ggplot(aes(x=YEAR, y=per_white), data = g4_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of White People", title = "Percentage of White People in Group 1 counties") + theme_economist()
reg_g4_white <- lm(per_white ~ YEAR, data = g4_summarized)
summary(reg_g4_white)

ggplot(aes(x=YEAR, y=per_hispanic), data = g4_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of Hispanic People", title = "Percentage of Hispanic People in Group 1 counties") + theme_economist()
reg_g4_hispanic <- lm(per_hispanic ~ YEAR, data = g4_summarized)
summary(reg_g4_hispanic)

ggplot(aes(x=YEAR, y=per_other), data = g4_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of Other Races", title = "Percentage of Other Races in Group 1 counties") + theme_economist()
reg_g4_other <- lm(per_other ~ YEAR, data = g4_summarized)
summary(reg_g4_other)

ggplot(aes(x=YEAR, y=avg_foreign), data = g4_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of Foreign-Born People", title = "Percentage of Foreign Born People in Group 1 counties") + theme_economist()
reg_g4_foreign <- lm(avg_foreign ~ YEAR, data = g4_summarized)
summary(reg_g4_foreign)

###based on g5
ggplot(aes(x=YEAR, y=avg_white), data = g5_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of White People", title = "Percentage of White People in Group 2 counties") + theme_economist()
reg_g5_white <- lm(avg_white ~ YEAR, data = g5_summarized)
summary(reg_g5_white)

ggplot(aes(x=YEAR, y=avg_hispanic), data = g5_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of Hispanic People", title = "Percentage of Hispanic People in Group 2 counties") + theme_economist()
reg_g5_hispanic <- lm(avg_hispanic ~ YEAR, data = g5_summarized)
summary(reg_g5_hispanic)

ggplot(aes(x=YEAR, y=avg_other), data = g5_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of Other Races", title = "Percentage of Other Races in Group 2 counties") + theme_economist()
reg_g5_other <- lm(avg_other ~ YEAR, data = g5_summarized)
summary(reg_g5_other)

ggplot(aes(x=YEAR, y=avg_foreign), data = g5_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of Foreign-Born People", title = "Percentage of Foreign Born People in Group 2 counties") + theme_economist()
reg_g5_foreign <- lm(avg_foreign ~ YEAR, data = g5_summarized)
summary(reg_g5_foreign)

### based on g6
ggplot(aes(x=YEAR, y=avg_white), data = g6_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of White People", title = "Percentage of White People in Group 3 counties") + theme_economist()
reg_g6_white <- lm(avg_white ~ YEAR, data = g6_summarized)
summary(reg_g6_white)

ggplot(aes(x=YEAR, y=avg_hispanic), data = g6_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of Hispanic People", title = "Percentage of Hispanic People in Group 3 counties") + theme_economist()
reg_g6_hispanic <- lm(avg_hispanic ~ YEAR, data = g6_summarized)
summary(reg_g6_hispanic)

ggplot(aes(x=YEAR, y=avg_other), data = g6_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of Other Races", title = "Percentage of Other Races in Group 3 counties") + theme_economist()
reg_g6_other <- lm(avg_other ~ YEAR, data = g6_summarized)
summary(reg_g6_other)

ggplot(aes(x=YEAR, y=avg_foreign), data = g6_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of Foreign-Born People", title = "Percentage of Foreign Born People in Group 3 counties") + theme_economist()
reg_g6_foreign <- lm(avg_foreign ~ YEAR, data = g6_summarized)
summary(reg_g6_foreign)

### based on g4_control
ggplot(aes(x=YEAR, y=per_white), data = g4_c_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of White People", title = "Percentage of White People in Group 1 (controlled) counties") + theme_economist()
reg_g4c_white <- lm(per_white ~ YEAR, data = g4_c_summarized)
summary(reg_g4c_white)

ggplot(aes(x=YEAR, y=per_hispanic), data = g4_c_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of Hispanic People", title = "Percentage of Hispanic People in Group 1 (controlled) counties") + theme_economist()
reg_g4c_hispanic <- lm(per_hispanic ~ YEAR, data = g4_c_summarized)
summary(reg_g4c_hispanic)

ggplot(aes(x=YEAR, y=per_other), data = g4_c_summarized) + geom_point() + geom_smooth(method="lm")  + labs(x = "Time", y = "Percentage of Other Races", title = "Percentage of Other Races in Group 1 (controlled) counties") + theme_economist()
reg_g4c_other <- lm(per_other ~ YEAR, data = g4_c_summarized)
summary(reg_g4c_other)

ggplot(aes(x=YEAR, y=avg_foreign), data = g4_c_summarized) + geom_point() + geom_smooth(method="lm") + labs(x = "Time", y = "Percentage of Foreign-Born People", title = "Percentage of Foreign Born People in Group 1 (controlled) counties") + theme_economist()
reg_g4c_foreign <- lm(avg_foreign ~ YEAR, data = g4_c_summarized)
summary(reg_g4c_foreign)

##################### exporting data.frames
write.csv(g4,"g4.csv", row.names = FALSE)
write.csv(g5,"g5.csv", row.names = FALSE)
write.csv(g6,"g6.csv", row.names = FALSE)
write.csv(g4_control,"g4_control.csv", row.names = FALSE)
write.csv(g4_summarized,"g4_summarized.csv", row.names = FALSE)
write.csv(g5_summarized,"g5_summarized.csv", row.names = FALSE)
write.csv(g6_summarized,"g6_summarized.csv", row.names = FALSE)
write.csv(g4_c_summarized,"g4_c_summarized.csv", row.names = FALSE)
