data1 <- read.csv("honors research.csv")
data1 <- data1[-c(61:64), ]

library(dplyr)

#Separate Data into two datasets
data_test1 <- data1[c(1:30), ]
data_test2 <- data1[c(31:60), ]

data_test2$pass <- NULL
data_test2$test <- NULL

#Datasets of P and NP
d1_P <- data_test1 %>% select(test, pass, coffee, enjoy, pref, WTP1, WTP2) %>% filter(pass == 1)
d1_NP <- data_test1 %>% select(test, pass, coffee, enjoy, pref, WTP1, WTP2) %>% filter(pass == 0)

#T-tests
t.test(d1_P$WTP2, data_test2$WTP2)
t.test(d1_NP$WTP2, data_test2$WTP2)

#ANOVA test
data1$aov <- ifelse(data1$test == 1 & data1$pass == 1, 1, ifelse(data1$test == 1 & data1$pass == 0, 2, ifelse(data1$test == 0, 3, 4)))
data_aov <- aov(data1$WTP2 ~ data1$aov)
summary(data_aov)

#pass no pass
data1$test_pass <- ifelse(data1$aov == 1, 1, 0)
data1$test_np <- ifelse(data1$aov == 2, 1, 0)
data1$no_test <- ifelse(data1$aov == 3, 1, 0)

#Regression, WTP on Pass/No Pass
reg_d1 <- lm(WTP2 ~ test_pass + test_np, data=data1)
summary(reg_d1)
BIC(reg_d1)

reg_d2 <- lm(WTP2 ~ test_pass + test_np + coffee + enjoy, data=data1)
summary(reg_d2)
BIC(reg_d2)

reg_d3 <- lm(pref ~ test_pass + test_np + coffee + enjoy, data=data1)
summary(reg_d3)
BIC(reg_d3)

summary(data1)
summary(d1_P)
summary(d1_NP)
summary(data_test2)