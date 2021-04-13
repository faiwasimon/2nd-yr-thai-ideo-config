setwd("C:/Users/tosur/OneDrive/Desktop/2ndYearProj")
library(haven)  # library for read dta files in R
install.packages("tidyverse") 
library(dplyr) # in tidyverse for merging
## package to read excel files
install.packages("readxl")
library(readxl)
## package to remove na's
install.packages("naniar") 
library(naniar)
## for the scree plot part, I think
install.packages("psych")
library(psych)
install.packages("lavaan", dependencies = TRUE)
library(lavaan)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#### Loading the dataframe and get related questions ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#rm(list=ls(all=TRUE))
## ********   YOU CAN SKIP THIS PART AND GO TO CFA PART. I PUT THE DATA THERE AS WELL.

df <- read_dta("ABAROMETERstataWLabel.dta")
df1 <- filter(df, country == 5, wave == 1)
df2 <- filter(df, country == 5, wave == 2)
df3 <- filter(df, country == 5, wave == 3)
df4 <- filter(df, country == 5, wave == 4)

q.numbers <- c(8,15,16,17,19,50,52,55,60,89,117,118,119,122,123,124,133,134,135,136,137,138,139,140,141,143)

###Questions with 2 to 5 as meaningful coding
q.2to5 <- c(8,15,16,17,19,50,52,55,60,122,123,124,133,134,135,136,137,138,139,140,141,143)
q.2to5.labels <- paste("q", q.2to5, sep = "")
df1.p1 <- df1[,q.2to5.labels]
df2.p1 <- df2[,q.2to5.labels]
df3.p1 <- df3[,q.2to5.labels]
df4.p1 <- df4[,q.2to5.labels]
#View(df1.p1)
summary(df4.p1)
df1.p1.na <- replace_with_na_all(df1.p1, condition = ~.x %in% c(1,6,7,8))
df2.p1.na <- replace_with_na_all(df2.p1, condition = ~.x %in% c(1,6,7,8))
df3.p1.na <- replace_with_na_all(df3.p1, condition = ~.x %in% c(1,6,7,8))
df4.p1.na <- replace_with_na_all(df4.p1, condition = ~.x %in% c(1,6,7,8))
#View(df1.p1.na)
#summary(df1.p1.na)

###Question 89
df1$q89.na <- ifelse(df1$q89 > 10, NA,
                     ifelse(df1$q89 < 1, NA, df1$q89))
df2$q89.na <- ifelse(df2$q89 > 10, NA,
                     ifelse(df2$q89 < 1, NA, df2$q89))
df3$q89.na <- ifelse(df3$q89 > 10, NA,
                     ifelse(df3$q89 < 1, NA, df3$q89))
df4$q89.na <- ifelse(df4$q89 > 10, NA,
                     ifelse(df4$q89 < 1, NA, df4$q89))


###Question 117 with 2 to 4 as meaningful coding
df1$q117.na <- ifelse(df1$q117 > 4, NA,
                      ifelse(df1$q117 < 2, NA, df1$q117))
df2$q117.na <- ifelse(df2$q117 > 4, NA,
                      ifelse(df2$q117 < 2, NA, df2$q117))
df3$q117.na <- ifelse(df3$q117 > 4, NA,
                      ifelse(df3$q117 < 2, NA, df3$q117))
df4$q117.na <- ifelse(df4$q117 > 4, NA,
                      ifelse(df4$q117 < 2, NA, df4$q117))


###Question 118 with 2 to 3 as meaningful coding
df1$q118.na <- ifelse(df1$q118 > 3, NA,
                      ifelse(df1$q118 < 2, NA, df1$q118))
df2$q118.na <- ifelse(df2$q118 > 3, NA,
                      ifelse(df2$q118 < 2, NA, df2$q118))
df3$q118.na <- ifelse(df3$q118 > 3, NA,
                      ifelse(df3$q118 < 2, NA, df3$q118))
df4$q118.na <- ifelse(df4$q118 > 3, NA,
                      ifelse(df4$q118 < 2, NA, df4$q118))


###Question 119 with 2 to 6 as meaningful coding but 6 will be recoded as the middle value
df1$q119.na <- ifelse(df1$q119 > 6, NA,
                      ifelse(df1$q119 < 2, NA, 
                             ifelse(df1$q119 == 6, 4, 
                                    ifelse(df1$q119 == 4, 5,
                                           ifelse(df1$q119 == 5,6,df1$q119)))))
df2$q119.na <- ifelse(df2$q119 > 6, NA,
                      ifelse(df2$q119 < 2, NA, 
                             ifelse(df2$q119 == 6, 4, 
                                    ifelse(df2$q119 == 4, 5,
                                           ifelse(df2$q119 == 5,6,df2$q119)))))
df3$q119.na <- ifelse(df3$q119 > 6, NA,
                      ifelse(df3$q119 < 2, NA, 
                             ifelse(df3$q119 == 6, 4, 
                                    ifelse(df3$q119 == 4, 5,
                                           ifelse(df3$q119 == 5,6,df3$q119)))))
df4$q119.na <- ifelse(df4$q119 > 6, NA,
                      ifelse(df4$q119 < 2, NA, 
                             ifelse(df4$q119 == 6, 4, 
                                    ifelse(df4$q119 == 4, 5,
                                           ifelse(df4$q119 == 5,6,df4$q119)))))

###Putting all the questions together
df1.cfa.na <- cbind(df1.p1.na, df1$q89.na, df1$q117.na, df1$q118.na, df1$q119.na)
df1.cfa <- na.omit(df1.cfa.na)
df2.cfa.na <- cbind(df2.p1.na, df2$q89.na, df2$q117.na, df2$q118.na, df2$q119.na)
df2.cfa <- na.omit(df2.cfa.na)
df3.cfa.na <- cbind(df3.p1.na, df3$q89.na, df3$q117.na, df3$q118.na, df3$q119.na)
df3.cfa <- na.omit(df3.cfa.na)
df4.cfa.na <- cbind(df4.p1.na, df4$q89.na, df4$q117.na, df4$q118.na, df4$q119.na)
df4.cfa <- na.omit(df4.cfa.na)


library(data.table)
setnames(df1.cfa, old = c('df1$q89.na', 'df1$q117.na', 'df1$q118.na', 'df1$q119.na'), new = c('q89','q117','q118','q119'))
setnames(df2.cfa, old = c('df2$q89.na', 'df2$q117.na', 'df2$q118.na', 'df2$q119.na'), new = c('q89','q117','q118','q119'))
setnames(df3.cfa, old = c('df3$q89.na', 'df3$q117.na', 'df3$q118.na', 'df3$q119.na'), new = c('q89','q117','q118','q119'))
setnames(df4.cfa, old = c('df4$q89.na', 'df4$q117.na', 'df4$q118.na', 'df4$q119.na'), new = c('q89','q117','q118','q119'))


###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#### Stack 4 Waves together ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

df1.cfa$Wave <- rep(1, nrow(df1.cfa))
df2.cfa$Wave <- rep(2, nrow(df2.cfa))
df3.cfa$Wave <- rep(3, nrow(df3.cfa))
df4.cfa$Wave <- rep(4, nrow(df4.cfa))
df.cfa <- bind_rows(df1.cfa, df2.cfa, df3.cfa, df4.cfa) 

save(df.cfa, file = "dfcfa.RData")
#rm(list = ls())







library(psych)
install.packages("GPArotation")
library(GPArotation)
efa1 <- fa(df.cfa.r1[,1:12], nfactors = 3, rotate = "oblimin", fm = "pa")
efa1 <- fa(df.r1.1[,1:12], nfactors = 3, rotate = "oblimin", fm = "pa")
efa2 <- fa(df.r1.2[,1:12], nfactors = 3, rotate = "oblimin", fm = "pa")
efa3 <- fa(df.r1.3[,1:12], nfactors = 3, rotate = "oblimin", fm = "pa")
efa4 <- fa(df.r1.4[,1:12], nfactors = 3, rotate = "oblimin", fm = "pa")

efa1
efa2
efa3
efa4




###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#### Run multiple group CFA on WAVE 1-4 ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
install.packages("lavaan", dependencies = TRUE)
library(lavaan)

df.cfa <- load("dfcfa.RData")

df.cfa.r <- df.cfa[,c("q138","q141","q117","q118","q89","q137","q133","q139","q143","q15","q19","Wave")]

df.cfa.r1 <- df.cfa[,c("q138","q141","q117","q118","q89","q137","q133","q139","q143","q135","q136","q52","q140","Wave")]
df.r1.1 <- filter(df.cfa.r1, Wave == 1)
df.r1.2 <- filter(df.cfa.r1, Wave == 2)
df.r1.3 <- filter(df.cfa.r1, Wave == 3)
df.r1.4 <- filter(df.cfa.r1, Wave == 4)


mg.CFA2 <- '
         democraci =~ q117 + q118 + q89
         unchecked =~ q137 + q138 + q141 
         centrali =~ q140 + q136 + q135 
  '

df.cfa.r234 <- df.cfa.r1[939:3465,] 


fit.c2 <- cfa(mg.CFA2, ordered = c("q138","q141","q117","q118","q137","q140","q136", "q135"), data = df.cfa.r1, group = "Wave", std.lv = TRUE, orthogonal = FALSE, group.equal = c("loadings"))
summary(fit.c2, fit.measures = TRUE, standardized = TRUE)

sink("cfa_results_c2.txt")
print(summary(fit.c2, fit.measures = TRUE, standardized = TRUE))
sink()  # returns output to the console




