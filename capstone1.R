install.packages("haven")
library(haven)
brfss <- read_xpt(file="Desktop/capstone/capstonedata.XPT")

dim(brfss) #do things on a subset if data is too big
age <- brfss[!(brfss$"_AGEG5YR" %in% c(10, 11, 12, 13, 14)), ]
brfss_female <- age[age$BIRTHSEX == 2, ] #keep only female
dim(brfss_female) #showing 392583*303
brfss_af <- brfss_female[brfss_female$"_IMPRACE" == 3, ] #keep only Asian female //CROSS OFF
dim(brfss_af)
#brfss_c <- brfss_af[brfss_af$"CERVSCRN" %in% c(1, 2), ]
#dim(brfss_c)
brfss_c <- brfss_female[brfss_female$"CERVSCRN" %in% c(1, 2), ] #look at descriptions
dim(brfss_c)
table(brfss_c$"_IMPRACE")
colSums(is.na(brfss_c))[which(colSums(is.na(brfss_c))>0)]

dim(brfss_af) #showing 342114

names(brfss_c)[names(brfss_c) == "_AGEG5YR"] <- "AGE"
names(brfss_c)[names(brfss_c) == "_IMPRACE"] <- "RACE"
names(brfss_c)[names(brfss_c) == "_HLTHPLN"] <- "HC"

install.packages("pillar")
library("pillar")

CERVSCRN <- as.factor(brfss_c$CERVSCRN)

install.packages("dplyr")
library("dplyr")

brfss_c <- brfss_c %>%
  mutate(CERVSCRN_new = recode(CERVSCRN,
                              `2` = 0,
                              .default = CERVSCRN
  )) %>%
  mutate(CERVSCRN_new = factor(CERVSCRN_new))

brfss_c <- brfss_c %>%
  mutate(AGE_new = recode(AGE,
                           `14` = 0, 
                           .default = AGE
  )) %>%
  mutate(AGE_new = factor(AGE_new))

brfss_c <- brfss_c %>%
  mutate(RACE_new = recode(RACE,
                            ` ` = 0, # Assuming the blank value is a space character
                            .default = RACE
  )) %>%
  mutate(RACE_new = factor(RACE_new))

brfss_c <- brfss_c %>%
  mutate(EDUCA_new = recode(EDUCA,
                            `9` = 0,
                            ` ` = 0, # Assuming the blank value is a space character
                            .default = EDUCA
  )) %>%
  mutate(EDUCA_new = factor(EDUCA_new))

brfss_c <- brfss_c %>%
  mutate(INCOME3_new = recode(INCOME3,
                              `77` = 0,
                              `99` = 0,
                              ` ` = 0, # Assuming the blank value is a space character
                              .default = INCOME3
  )) %>%
  mutate(INCOME3_new = factor(INCOME3_new))


brfss_c <- brfss_c %>%
  mutate(EMPLOY1_new = recode(EMPLOY1,
                              `9` = 0,
                              ` ` = 0, # Assuming the blank value is a space character
                              .default = EMPLOY1
  )) %>%
  mutate(EMPLOY1_new = as.factor(EMPLOY1_new))

brfss_c <- brfss_c %>%
  mutate(HC_new = recode(HC,
                         `2` = 0,
                         .default = HC
  )) %>%
  mutate(HC_new = as.factor(HC_new))

brfss_c <- brfss_c %>%
  mutate(MARITAL_new = recode(MARITAL,
                              `9` = 0,
                              ` ` = 0, # Assuming the blank value is a space character
                              .default = MARITAL
  )) %>%
  mutate(MARITAL_new = as.factor(MARITAL_new))

brfss_c <- brfss_c %>%
  mutate(MEDCOST1_new = recode(MEDCOST1,
                               `7` = 0,
                               `9` = 0,
                               ` ` = 0, # Assuming the blank value is a space character
                               .default = MEDCOST1
  )) %>%
  mutate(MEDCOST1_new = as.factor(MEDCOST1_new))

brfss_c <- brfss_c %>%
  mutate(RENTHOM1_new = recode(RENTHOM1,
                               `7` = 0,
                               `9` = 0,
                               ` ` = 0, # Assuming the blank value is a space character
                               .default = RENTHOM1
  )) %>%
  mutate(RENTHOM1_new = as.factor(RENTHOM1_new))

summary(brfss_c$CERVSCRN_new) #only value 1(yes) and 2(no)
summary(brfss_c$AGE_new) #no missing value
summary(brfss_c$RACE_new) #no missing value
summary(brfss_c$INCOME3_new) #303 missing (16.3%)
summary(brfss_c$EMPLOY1_new) #19 missing
summary(brfss_c$HC_new) # 70 missing (3.7%)
summary(brfss_c$MARITAL_new) # missing
summary(brfss_c$MEDCOST1_new) # 5 missing 
summary(brfss_c$RENTHOM1_new) # 15 missing value out of 1856 (0.8%)
summary(brfss_c$EDUCA_new) # 4 missing

library(ggplot2)
ggplot(brfss_c, aes(x = factor(INCOME3_new))) +
  geom_bar() +
  labs(x = "INCOME3_new Categories", y = "Count", title = "Distribution of INCOME3_new") +
  theme_minimal()

brfss_c <- brfss_c[brfss_c$RENTHOM1_new != 0,]
brfss_c <- brfss_c[brfss_c$MEDCOST1_new != 0,]
brfss_c <- brfss_c[brfss_c$MARITAL_new != 0,]
brfss_c <- brfss_c[brfss_c$HC_new != 9,]
brfss_c <- brfss_c[brfss_c$EMPLOY1_new != 0,]
brfss_c <- brfss_c[brfss_c$EDUCA_new != 0,]


selected_variables <- c("CERVSCRN_new", "AGE_new", "RACE_new", "EDUCA_new", "INCOME3_new", 
                        "EMPLOY1_new", "MARITAL_new", "RENTHOM1_new", "HC_new", "MEDCOST1_new")
brfssnew <- subset(brfss_c, select = selected_variables)
x <- c("AGE_new", "RACE_new", "EDUCA_new", "INCOME3_new", "EMPLOY1_new", "MARITAL_new", "RENTHOM1_new", "HC_new", "MEDCOST1_new")
y <- "CERVSCRN_new"



fullmodel <- glm(CERVSCRN_new ~ ., data = brfssnew, family = binomial)
summary(fullmodel)$coefficients
summary(fullmodel)

#income*HC + EDUCA*EMPLOY + MARITAL*RENT + HC*COST aren't significant (p value greater than 0.05)
fullmodelInt <-glm(CERVSCRN_new ~ AGE_new + RACE_new + EDUCA_new + 
                     INCOME3_new + EMPLOY1_new + MARITAL_new + RENTHOM1_new + 
                     HC_new + MEDCOST1_new + INCOME3_new*HC_new + EMPLOY1_new*EDUCA_new + 
                     MARITAL_new*RENTHOM1_new + MEDCOST1_new*HC_new, data = brfss_c, family = "binomial")
summary(fullmodelInt)

#does not work
install.packages("car")
library("car")
vif(model)

AIC(fullmodel)
AIC(modelAIC)
BIC(fullmodel)

install.packages("leaps")
library(leaps)
result_cp=leaps(x,y, method="Cp")
#Error in `colnames<-`(`*tmp*`, value = c(as.character(1:9), LETTERS)[1:NCOL(x)]) : 
#attempt to set 'colnames' on an object with less than two dimensions


install.packages("MASS")
library(MASS)
stepback_aic=step(fullmodel, direction="both", k=2) 
#Step:  AIC=2140.27
#CERVSCRN_new ~ AGE_new + RACE_new + EDUCA_new + HC_new
stepback_bic=step(fullmodel, direction="both", k=log(length(brfssnew$CERVSCRN_new)))
#CERVSCRN_new ~ AGE_new + EDUCA_new + HC_new

stepback_aic
stepback_bic

#use this as the finalized model
modelAIC <-glm(CERVSCRN_new ~ AGE_new + RACE_new + EDUCA_new + 
                 HC_new, data = brfssnew, family = "binomial")
summary(modelAIC)

modelBIC <-glm(CERVSCRN_new ~ AGE_new + EDUCA_new + 
                 HC_new, data = brfssnew, family = "binomial")
summary(modelBIC)


install.packages("ggplot2")
library("ggplot2")

screening_percentage <- brfss_c %>%
  group_by(HC_new, CERVSCRN_new) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(CERVSCRN_new == 1)

hc_labels <- c("0 - Without HC Plan", "1 - WIth HC Plan")

ggplot(screening_percentage, aes(x = HC_new, y = percentage, fill = HC_new)) +
  geom_bar(stat = "identity") +
  labs(x = "Healthcare Coverage", y = "Percentage of Women Screened", title = "Cervical Cancer Screening by Healthcare Coverage") +
  scale_x_discrete(labels = hc_labels) +
  scale_fill_discrete(guide = FALSE) +
  ylim(0, 80) +
  theme_minimal()



screening_percentage <- brfss_c %>%
  group_by(RACE_new, CERVSCRN_new) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(CERVSCRN_new == 1)

race_labels <- c("1 - White", "2 - Black", "3 - Asian", "4 - Indigenous", "5 - Hispanic", "6 - Other")

ggplot(screening_percentage, aes(x = RACE_new, y = percentage, fill = RACE_new)) +
  geom_bar(stat = "identity") +
  labs(x = "Race", y = "Percentage of Women Screened", title = "Cervical Cancer Screening by Race") +
  scale_x_discrete(labels = race_labels) +
  scale_fill_discrete(guide = FALSE) +
  ylim(0, 80) +
  theme_minimal()


screening_percentage_edu <- brfss_c %>%
  group_by(EDUCA_new, CERVSCRN_new) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(CERVSCRN_new == 1)

edu_labels <- c("2 - Elementary", "3 - Some High School", "4 - High School Grad", "5 - Some College", "6 - College Grad")

ggplot(screening_percentage_edu, aes(x = EDUCA_new, y = percentage, fill = EDUCA_new)) +
  geom_bar(stat = "identity") +
  labs(x = "Education", y = "Percentage of Women Screened", title = "Cervical Cancer Screening by Education Level") +
  scale_x_discrete(labels = edu_labels) +
  scale_fill_discrete(guide = FALSE) +
  ylim(0, 80) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




screening_percentage_age <- brfss_c %>%
  group_by(AGE_new, CERVSCRN_new) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(CERVSCRN_new == 1)

age_labels <- c("1 - Age 18-24", "2 - Age 25-29", "3 - Age 30-34", "4 - Age 35-39", "5 - Age 40-44", "6 - Age 45-49", "7 - Age 50-54", "8 - Age 55-59", "9 - Age 60-64")

ggplot(screening_percentage_age, aes(x = AGE_new, y = percentage, fill = AGE_new)) +
  geom_bar(stat = "identity") +
  labs(x = "Age Group", y = "Percentage of Women Screened", title = "Cervical Cancer Screening by Age Group") +
  scale_fill_discrete(name = "Age Group", labels = age_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

screening_percentage_age <- brfss_c %>%
  group_by(AGE_new, CERVSCRN_new) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(CERVSCRN_new == 1)

age_labels <- c("1 - Age 18-24", "2 - Age 25-29", "3 - Age 30-34", "4 - Age 35-39", "5 - Age 40-44", "6 - Age 45-49", "7 - Age 50-54", "8 - Age 55-59", "9 - Age 60-64")

ggplot(screening_percentage_age, aes(x = AGE_new, y = percentage, fill = AGE_new)) +
  geom_bar(stat = "identity") +
  labs(x = "Age Group", y = "Percentage of Women Screened", title = "Cervical Cancer Screening by Age Group") +
  scale_x_discrete(labels = age_labels) +
  scale_fill_discrete(guide = FALSE) +
  ylim(0, 80) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



screening_percentage_income <- brfss_c %>%
  group_by(INCOME3_new, HC_new) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(HC_new == 1)

income_labels <- c("0 - Missing", "1 - Less than $10,000", "2 - $10,000 to less than $15,000", "3 - $15,000 to less than $20,000", "4 - $20,000 to less than $25,000", "5 - $25,000 to less than $35,000", "6 - $35,000 to less than $50,000", 
                   "7 - $50,000 to less than $75,000", "8 - $75,000 to less than $100,000", "9 - $100,000 to less than $150,000", "10 - $150,000 to less than $200,000", "11 - $200,000 or more")

ggplot(screening_percentage_income, aes(x = INCOME3_new, y = percentage, fill = INCOME3_new)) +
  geom_bar(stat = "identity") +
  labs(x = "Income Group", y = "Percentage of Women with HC Plan", title = "HC Plan by Income Group") +
  scale_fill_discrete(name = "Income Group", labels = income_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


screening_percentage_income <- brfss_c %>%
  group_by(INCOME3_new, HC_new) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(HC_new == 1)

income_labels <- c("0 - Missing", "1 - Less than $10,000", "2 - $10,000 to less than $15,000", "3 - $15,000 to less than $20,000", "4 - $20,000 to less than $25,000", "5 - $25,000 to less than $35,000", "6 - $35,000 to less than $50,000", 
                   "7 - $50,000 to less than $75,000", "8 - $75,000 to less than $100,000", "9 - $100,000 to less than $150,000", "10 - $150,000 to less than $200,000", "11 - $200,000 or more")

ggplot(screening_percentage_income, aes(x = INCOME3_new, y = percentage, fill = INCOME3_new)) +
  geom_bar(stat = "identity") +
  labs(x = "Income Group", y = "Percentage of Women with HC Plan", title = "HC Plan by Income Group") +
  scale_x_discrete(labels = income_labels) +
  scale_fill_discrete(guide = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










