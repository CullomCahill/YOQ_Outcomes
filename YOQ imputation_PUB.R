###### File Setup ----

library(tidyverse)
library(simputation)   # Includes imputation tools
library(naniar)        # Allows to work with missing data
library(mice)           # Multiple data imputation
library(rstatix)
library(patchwork)



### Data Setup ----

setwd("C:/Users/cullo/OneDrive/Desktop/One Cloud/ELEMENTS")

# Load YOQ
YOQ_full <- readRDS("YOQ/rds/YOQ_ver1.rds")  # 4110 obs
# Load clinical cutoff scale
cutoff <- readRDS("YOQ/rds/YOQ clinical cutoffs.rds")


### Data Clean ----
# Select only want's wanted  and add age column
YOQ_full <- select(YOQ_full, otClientId, clientName, instance, 
                   surveyID, assignedToRelationship, ID:TOTAL, age)
# Remove duplicates
YOQ_full_nodup <- YOQ_full[!duplicated(YOQ_full[1:5]),]   # 3912




##### "TOTAL" = TOTAL SUBSCORE IMPUTATION  -----

##  DATA PREP

# Prep data from original df (YOQ_full_nodup)
# Spread data
# Remove all rows with missing cases at MO6 OR YR1 AND all cases at DOA AND DOD
# Add column to denote when data is missing and when it is complete for both MO6 and YR1

total_spread_prep <- select(YOQ_full_nodup, otClientId, clientName, 
                            instance, surveyID, assignedToRelationship, TOTAL) #3912
total_spread <- total_spread_prep %>%                                            # 1684
  spread(key = "instance",
         value = "TOTAL")
total_miss <- total_spread %>% 
  subset(!is.na(MO6) | !is.na(YR1)) %>% 
  subset(!is.na(DOA & DOD))   #574
total_imput0 <- total_miss %>% 
  mutate(MO6_missing = label_missings(total_miss$MO6, 
                                      missing = "missing",
                                      complete = "complete") ) %>% 
  mutate(YR1_missing = label_missings(total_miss$YR1,
                                      missing = "missing",
                                      complete = "complete"))

missing_tally_id <- total_imput0 %>% 
  group_by(surveyID, MO6_missing, YR1_missing) %>% tally()

missing_tally_yoq <- total_imput0 %>% 
  group_by(MO6_missing, YR1_missing) %>% tally()
# saveRDS(missing_tally_id, "YOQ/rds/yoq missing tally.rds")


## Imputate to fill in missing scores

# 1. Change numbers to doubles (impute_lm requires this)
# 2. Imput missing 6 Month data using simputation::impute_lm
# 3. Round to two decimal places
# 4. Bring back to long
# 5. Make factor


total_imput1 <- total_imput0 %>% mutate(MO6=as.double(MO6), YR1=as.double(YR1))
# total_imput_MO6 <- total_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1)
# total_imput_YR1 <- total_imput_MO6 %>% impute_lm(YR1 ~ DOA +DOD + MO6)
total_imput <- total_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1) %>% impute_lm(YR1 ~ DOA +DOD + MO6)
total_imput$MO6 <- round(total_imput$MO6, 2)
total_imput$YR1 <- round(total_imput$YR1, 2)
total_imput_long <- gather(total_imput, key = "instance", value = "TOTAL", DOA:YR1)   #2956
total_imput_long[,c("instance", "MO6_missing", "YR1_missing")] <- lapply(total_imput_long[,c("instance", "MO6_missing", "YR1_missing")], factor)


## Test Stats
# select (client id, surveyid, and MO6 OR YR1), group_by survey Id, and give 5num stat with mean and sd

# MO6
total_miss.6 <-  total_miss %>% select(c(1,3,7)) %>%  
  na.omit %>% group_by(surveyID) %>% 
  summarise(n = n(), min = fivenum(MO6)[1], Q1 = fivenum(MO6)[2], 
            median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], 
            max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
total_imput.6 <- total_imput %>% group_by(surveyID) %>% 
  summarise(n = n(), min = fivenum(MO6)[1],  Q1 = fivenum(MO6)[2], 
            median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], 
            max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
# YR1
total_miss.12 <- total_miss %>% select(c(1,3,8)) %>%  
  na.omit %>% group_by(surveyID) %>% 
  summarise(n = n(), min = fivenum(YR1)[1], Q1 = fivenum(YR1)[2], 
            median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], 
            max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))

total_imput.12 <- total_imput %>% group_by(surveyID) %>% 
  summarise(n = n(), min = fivenum(YR1)[1],  Q1 = fivenum(YR1)[2], 
            median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], 
            max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))
# Add "dataframe" column denoting where values came from
total_miss.6$dataframe <- "total_miss.6" 
total_imput.6$dataframe <- "total_imput.6" 
total_miss.12$dataframe <- "total_miss.12" 
total_imput.12$dataframe <- "total_imput.12" 
# Bind together and round decimals 
missing_data_summary <- rbind(total_miss.6, total_imput.6, total_miss.12, total_imput.12)
missing_data_summary[,-c(1,10)] <- round(missing_data_summary[,-c(1,10)],2)




################ "ID" = Intrapersonal Distress SUBSCORE IMPUTATION ----

##  DATA PREP

# Prep data from original df (YOQ_full_nodup)
# Spread data
# Remove all rows with missing cases at MO6 OR YR1 AND all cases at DOA AND DOD
# Add column to denote when data is missing and when it is complete for both MO6 and YR1

ID_spread_prep <- select(YOQ_full_nodup, otClientId, clientName, instance, surveyID, assignedToRelationship, ID) #3912
ID_spread <- ID_spread_prep %>%                                            # 1684
  spread(key = "instance",
         value = "ID")
ID_miss <- ID_spread %>% subset(!is.na(MO6) | !is.na(YR1)) %>% subset(!is.na(DOA & DOD))   #574
ID_imput0 <- ID_miss %>% mutate(MO6_missing = label_missings(ID_miss$MO6, 
                                                                   missing = "missing",
                                                                   complete = "complete") ) %>% 
                          mutate(YR1_missing = label_missings(ID_miss$YR1,
                                      missing = "missing",
                                      complete = "complete"))
missing_tally <- ID_imput0 %>% group_by(surveyID, MO6_missing, YR1_missing) %>% tally()  # Same as TOTAL


## IMPUTATION 

# Change numbers to doubles (impute_lm requires this)
# Imput missing 6 Month data using simputation::impute_lm
# Round to two decimal places
# Bring back to long
# Make factor

ID_imput1 <- ID_imput0 %>% mutate(MO6=as.double(MO6), YR1=as.double(YR1))
# ID_imput_MO6 <- ID_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1)
# ID_imput_YR1 <- ID_imput_MO6 %>% impute_lm(YR1 ~ DOA +DOD + MO6)
ID_imput <- ID_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1) %>% impute_lm(YR1 ~ DOA +DOD + MO6)
ID_imput$MO6 <- round(ID_imput$MO6, 2)
ID_imput$YR1 <- round(ID_imput$YR1, 2)
ID_imput_long <- gather(ID_imput, key = "instance", value = "ID", DOA:YR1)   #2956
ID_imput_long[,c("instance", "MO6_missing", "YR1_missing")] <- lapply(ID_imput_long[,c("instance", "MO6_missing", "YR1_missing")], factor)


## Test Stats
# select (client id, surveyid, and MO6 OR YR1), group_by survey Id, and give 5num stat with mean and sd

# MO6
ID_miss.6 <-  ID_miss %>% select(c(1,3,7)) %>%  na.omit %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(MO6)[1], Q1 = fivenum(MO6)[2], median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
ID_imput.6 <- ID_imput %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(MO6)[1],  Q1 = fivenum(MO6)[2], median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
# YR1
ID_miss.12 <- ID_miss %>% select(c(1,3,8)) %>%  na.omit %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(YR1)[1], Q1 = fivenum(YR1)[2], median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))
ID_imput.12 <- ID_imput %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(YR1)[1],  Q1 = fivenum(YR1)[2], median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))
# Add "dataframe" column denoting where values came from
ID_miss.6$dataframe <- "ID_miss.6" 
ID_imput.6$dataframe <- "ID_imput.6" 
ID_miss.12$dataframe <- "ID_miss.12" 
ID_imput.12$dataframe <- "ID_imput.12" 
# Bind together and round decimals 
missing_data_summary <- rbind(missing_data_summary, ID_miss.6, ID_imput.6, ID_miss.12, ID_imput.12)
missing_data_summary[,-c(1,10)] <- round(missing_data_summary[,-c(1,10)],2)




# Pull values to Excel Spreadsheet (missing data summary.xlxs)


id.imp.plot <- ggplot(ID_imput_long, aes(color = surveyID)) + 
  stat_summary(aes(x=instance, y=ID), fun = mean, geom = "point") +
  stat_summary(aes(x=instance, y=ID, group = surveyID), fun = mean, geom = "line", size = 1)+
  geom_abline(intercept = 16, slope = 0, linetype = "dashed") +
  labs(title = "ID new imputation data", caption = "red = parent, blue = student")+
  coord_cartesian(ylim = c(10,35))+
  theme_minimal()+
  theme(legend.position="none")
  
# To compare apples to apples make a separate object: ID_miss_long
ID_miss_long <- gather(ID_miss, key = "instance", value = "ID", DOA:YR1)   #2956

id.org.plot <- ggplot(ID_miss_long, aes(color = surveyID)) + 
  stat_summary(aes(x=instance, y=ID), fun = mean, geom = "point") +
  stat_summary(aes(x=instance, y=ID, group = surveyID), fun = mean, geom = "line", size = 1)+
  geom_abline(intercept = 16, slope = 0, linetype = "dashed") +
  labs(title = "ID NO imputation data")+
  coord_cartesian(ylim = c(10,35))+
  theme_minimal()+
  theme(legend.position="none")


id.plot <- id.org.plot + id.imp.plot
# ggsave("YOQ/fig/id_plot.pdf", id.plot, height = 10, width = 14)





################ "S" = Somatic Problems SUBSCORE IMPUTATION ----

##  DATA PREP

# Prep data from original df (YOQ_full_nodup)
# Spread data
# Remove all rows with missing cases at MO6 OR YR1 AND all cases at DOA AND DOD
# Add column to denote when data is missing and when it is complete for both MO6 and YR1

S_spread_prep <- select(YOQ_full_nodup, otClientId, clientName, instance, surveyID, assignedToRelationship, S) #3912
S_spread <- S_spread_prep %>%                                            # 1684
  spread(key = "instance",
         value = "S")

###? ISSUE
S_miss <- S_spread %>% subset(!is.na(MO6) | !is.na(YR1)) %>% subset(!is.na(DOA & DOD))   #592
colSums(is.na(S_miss)) # missing 9 doa and dod ?? MO6:62, YR1:231
S_miss_test <- S_spread %>% subset(!is.na(MO6) | !is.na(YR1)) %>% subset(!is.na(DOA)) %>% subset(!is.na(DOD))   #574
colSums(is.na(S_miss_test)) # missing MO6: 58, YR1: 223

# Looks like that resolves it...
S_miss <- S_miss_test


S_imput0 <- S_miss %>% mutate(MO6_missing = label_missings(S_miss$MO6, 
                                                             missing = "missing",
                                                             complete = "complete") ) %>% 
  mutate(YR1_missing = label_missings(S_miss$YR1,
                                      missing = "missing",
                                      complete = "complete"))
missing_tally <- S_imput0 %>% group_by(surveyID, MO6_missing, YR1_missing) %>% tally()  # Same as TOTAL


## IMPUTATION 

# Change numbers to doubles (impute_lm requires this)
# Imput missing 6 Month data using simputation::impute_lm
# Round to two decimal places
# Bring back to long
# Make factor

S_imput1 <- S_imput0 %>% mutate(MO6=as.double(MO6), YR1=as.double(YR1))
# S_imput_MO6 <- S_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1)
# S_imput_YR1 <- S_imput_MO6 %>% impute_lm(YR1 ~ DOA +DOD + MO6)
S_imput <- S_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1) %>% impute_lm(YR1 ~ DOA +DOD + MO6)
S_imput$MO6 <- round(S_imput$MO6, 2)
S_imput$YR1 <- round(S_imput$YR1, 2)
S_imput_long <- gather(S_imput, key = "instance", value = "S", DOA:YR1)   #2956
S_imput_long[,c("instance", "MO6_missing", "YR1_missing")] <- lapply(S_imput_long[,c("instance", "MO6_missing", "YR1_missing")], factor)


## Test Stats
# select (client id, surveyid, and MO6 OR YR1), group_by survey Id, and give 5num stat with mean and sd

# MO6
S_miss.6 <-  S_miss %>% select(c(1,3,7)) %>%  na.omit %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(MO6)[1], Q1 = fivenum(MO6)[2], median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
S_imput.6 <- S_imput %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(MO6)[1],  Q1 = fivenum(MO6)[2], median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
# YR1
S_miss.12 <- S_miss %>% select(c(1,3,8)) %>%  na.omit %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(YR1)[1], Q1 = fivenum(YR1)[2], median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))


S_imput.12 <- S_imput %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(YR1)[1],  
            Q1 = fivenum(YR1)[2], median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4],
            max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))

# Add "dataframe" column denoting where values came from
S_miss.6$dataframe <- "S_miss.6" 
S_imput.6$dataframe <- "S_imput.6" 
S_miss.12$dataframe <- "S_miss.12" 
S_imput.12$dataframe <- "S_imput.12" 
# Bind together and round decimals 
missing_data_summary <- rbind(missing_data_summary, S_miss.6, S_imput.6, S_miss.12, S_imput.12)
missing_data_summary[,-c(1,10)] <- round(missing_data_summary[,-c(1,10)],2)




# Pull values to Excel Spreadsheet (missing data summary.xlxs)


S.imp.plot <- ggplot(S_imput_long, aes(color = surveyID)) + 
  stat_summary(aes(x=instance, y=S), fun = mean, geom = "point") +
  stat_summary(aes(x=instance, y=S, group = surveyID), fun = mean, geom = "line", size = 1)+
  geom_abline(intercept = 6, slope = 0, linetype = "dashed") +
  labs(title = "S new imputation data", caption = "red = parent, blue = student")+
  coord_cartesian(ylim = c(0,10))+
  theme_minimal()+
  theme(legend.position="none")

# To compare apples to apples make a separate object: S_miss_long
S_miss_long <- gather(S_miss, key = "instance", value = "S", DOA:YR1)   #2956

S.org.plot <- ggplot(S_miss_long, aes(color = surveyID)) + 
  stat_summary(aes(x=instance, y=S), fun = mean, geom = "point") +
  stat_summary(aes(x=instance, y=S, group = surveyID), fun = mean, geom = "line", size = 1)+
  geom_abline(intercept = 6, slope = 0, linetype = "dashed") +
  labs(title = "S NO imputation data")+
  coord_cartesian(ylim = c(0,10))+
  theme_minimal()+
  theme(legend.position="none")


S.plot <- S.org.plot + S.imp.plot
# ggsave("YOQ/fig/S_plot.pdf", S.plot, height = 10, width = 14)











##### "IR" = Interpersonal Relationships SUBSCORE IMPUTATION  -----

##  DATA PREP

# Prep data from original df (YOQ_full_nodup)
# Spread data
# Remove all rows with missing cases at MO6 OR YR1 AND all cases at DOA AND DOD
# Add column to denote when data is missing and when it is complete for both MO6 and YR1

IR_spread_prep <- select(YOQ_full_nodup, otClientId, clientName, instance, surveyID, 
                            assignedToRelationship, IR) #3912
IR_spread <- IR_spread_prep %>%                                            # 1684
  spread(key = "instance",
         value = "IR")
IR_miss <- IR_spread %>% subset(!is.na(MO6) | !is.na(YR1)) %>% subset(!is.na(DOA)) %>% subset(!is.na(DOD))   #574
IR_imput0 <- IR_miss %>% mutate(MO6_missing = label_missings(IR_miss$MO6, 
                                                                   missing = "missing",
                                                                   complete = "complete") ) %>% 
  mutate(YR1_missing = label_missings(IR_miss$YR1,
                                      missing = "missing",
                                      complete = "complete"))
missing_tally <- IR_imput0 %>% group_by(surveyID, MO6_missing, YR1_missing) %>% tally()


## Imputate to fill in missing scores

# 1. Change numbers to doubles (impute_lm requires this)
# 2. Imput missing 6 Month data using simputation::impute_lm
# 3. Round to two decimal places
# 4. Bring back to long
# 5. Make factor


IR_imput1 <- IR_imput0 %>% mutate(MO6=as.double(MO6), YR1=as.double(YR1))
# IR_imput_MO6 <- IR_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1)
# IR_imput_YR1 <- IR_imput_MO6 %>% impute_lm(YR1 ~ DOA +DOD + MO6)
IR_imput <- IR_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1) %>% impute_lm(YR1 ~ DOA +DOD + MO6)
IR_imput$MO6 <- round(IR_imput$MO6, 2)
IR_imput$YR1 <- round(IR_imput$YR1, 2)
IR_imput_long <- gather(IR_imput, key = "instance", value = "IR", DOA:YR1)   #2956
IR_imput_long[,c("instance", "MO6_missing", "YR1_missing")] <- lapply(IR_imput_long[,c("instance", "MO6_missing", "YR1_missing")], factor)


## Test Stats
# select (client id, surveyid, and MO6 OR YR1), group_by survey Id, and give 5num stat with mean and sd

# MO6
IR_miss.6 <-  IR_miss %>% select(c(1,3,7)) %>%  na.omit %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(MO6)[1], Q1 = fivenum(MO6)[2], median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
IR_imput.6 <- IR_imput %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(MO6)[1],  Q1 = fivenum(MO6)[2], median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
# YR1
IR_miss.12 <- IR_miss %>% select(c(1,3,8)) %>%  na.omit %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(YR1)[1], Q1 = fivenum(YR1)[2], median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))
IR_imput.12 <- IR_imput %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(YR1)[1],  Q1 = fivenum(YR1)[2], median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))
# Add "dataframe" column denoting where values came from
IR_miss.6$dataframe <- "IR_miss.6" 
IR_imput.6$dataframe <- "IR_imput.6" 
IR_miss.12$dataframe <- "IR_miss.12" 
IR_imput.12$dataframe <- "IR_imput.12" 
# Bind together and round decimals 
missing_data_summary <- rbind(missing_data_summary, IR_miss.6, IR_imput.6, IR_miss.12, IR_imput.12)
missing_data_summary[,-c(1,10)] <- round(missing_data_summary[,-c(1,10)],2)


## Plots

IR.imp.plot <- ggplot(IR_imput_long, aes(color = surveyID)) + 
  stat_summary(aes(x=instance, y=IR), fun = mean, geom = "point") +
  stat_summary(aes(x=instance, y=IR, group = surveyID), fun = mean, geom = "line", size = 1)+
  geom_abline(intercept = 3, slope = 0, linetype = "dashed") +
  labs(title = "IR new imputation data", caption = "red = parent, blue = stu")+
  # coord_cartesian(ylim = c(30,100))+
  theme_minimal()+
  theme(legend.position="none")


# To compare apples to apples make a separate object: ID_miss_long
IR_miss_long <- gather(IR_miss, key = "instance", value = "IR", DOA:YR1)   #2956

IR.org.plot <- ggplot(IR_miss_long, aes(color = surveyID)) + 
  stat_summary(aes(x=instance, y=IR), fun = mean, geom = "point") +
  stat_summary(aes(x=instance, y=IR, group = surveyID), fun = mean, geom = "line", size = 1)+
  geom_abline(intercept = 3, slope = 0, linetype = "dashed") +
  labs(title = "IR NO imputation data")+
  # coord_cartesian(ylim = c(30,100))+
  theme_minimal() +
  theme(legend.position="none")


IR.plot <- IR.org.plot + IR.imp.plot
ggsave("YOQ/fig/IR_plot.pdf", IR.plot, height = 10, width = 14)









##### "CI" = Critical Items SUBSCORE IMPUTATION  -----

##  DATA PREP

# Prep data from original df (YOQ_full_nodup)
# Spread data
# Remove all rows with missing cases at MO6 OR YR1 AND all cases at DOA AND DOD
# Add column to denote when data is missing and when it is complete for both MO6 and YR1

CI_spread_prep <- select(YOQ_full_nodup, otClientId, clientName, instance, surveyID, 
                            assignedToRelationship, CI) #3912
CI_spread <- CI_spread_prep %>%                                            # 1684
  spread(key = "instance",
         value = "CI")
CI_miss <- CI_spread %>% subset(!is.na(MO6) | !is.na(YR1)) %>% subset(!is.na(DOA)) %>% subset(!is.na(DOD))   #574
CI_imput0 <- CI_miss %>% mutate(MO6_missing = label_missings(CI_miss$MO6, 
                                                                   missing = "missing",
                                                                   complete = "complete") ) %>% 
  mutate(YR1_missing = label_missings(CI_miss$YR1,
                                      missing = "missing",
                                      complete = "complete"))
missing_tally <- CI_imput0 %>% group_by(surveyID, MO6_missing, YR1_missing) %>% tally()


## Imputate to fill in missing scores

# 1. Change numbers to doubles (impute_lm requires this)
# 2. Imput missing 6 Month data using simputation::impute_lm
# 3. Round to two decimal places
# 4. Bring back to long
# 5. Make factor


CI_imput1 <- CI_imput0 %>% mutate(MO6=as.double(MO6), YR1=as.double(YR1))
# CI_imput_MO6 <- CI_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1)
# CI_imput_YR1 <- CI_imput_MO6 %>% impute_lm(YR1 ~ DOA +DOD + MO6)
CI_imput <- CI_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1) %>% impute_lm(YR1 ~ DOA +DOD + MO6)
CI_imput$MO6 <- round(CI_imput$MO6, 2)
CI_imput$YR1 <- round(CI_imput$YR1, 2)
CI_imput_long <- gather(CI_imput, key = "instance", value = "CI", DOA:YR1)   #2956
CI_imput_long[,c("instance", "MO6_missing", "YR1_missing")] <- lapply(CI_imput_long[,c("instance", "MO6_missing", "YR1_missing")], factor)


## Test Stats
# select (client id, surveyid, and MO6 OR YR1), group_by survey Id, and give 5num stat with mean and sd

# MO6
CI_miss.6 <-  CI_miss %>% select(c(1,3,7)) %>%  na.omit %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(MO6)[1], Q1 = fivenum(MO6)[2], median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
CI_imput.6 <- CI_imput %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(MO6)[1],  Q1 = fivenum(MO6)[2], median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
# YR1
CI_miss.12 <- CI_miss %>% select(c(1,3,8)) %>%  na.omit %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(YR1)[1], Q1 = fivenum(YR1)[2], median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))
CI_imput.12 <- CI_imput %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(YR1)[1],  Q1 = fivenum(YR1)[2], median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))
# Add "dataframe" column denoting where values came from
CI_miss.6$dataframe <- "CI_miss.6" 
CI_imput.6$dataframe <- "CI_imput.6" 
CI_miss.12$dataframe <- "CI_miss.12" 
CI_imput.12$dataframe <- "CI_imput.12" 
# Bind together and round decimals 
missing_data_summary <- rbind(missing_data_summary, CI_miss.6, CI_imput.6, CI_miss.12, CI_imput.12)
missing_data_summary[,-c(1,10)] <- round(missing_data_summary[,-c(1,10)],2)


## Plots

CI.imp.plot <- ggplot(CI_imput_long, aes(color = surveyID)) + 
  stat_summary(aes(x=instance, y=CI), fun = mean, geom = "point") +
  stat_summary(aes(x=instance, y=CI, group = surveyID), fun = mean, geom = "line", size = 1)+
  geom_abline(intercept = 6, slope = 0, linetype = "dashed") +
  labs(title = "CI new imputation data", caption = "red = parent, blue = stu")+
  coord_cartesian(ylim = c(0,30))+
  theme_minimal()+
  theme(legend.position="none")


# To compare apples to apples make a separate object: ID_miss_long
CI_miss_long <- gather(CI_miss, key = "instance", value = "CI", DOA:YR1)   #2956

CI.org.plot <- ggplot(CI_miss_long, aes(color = surveyID)) + 
  stat_summary(aes(x=instance, y=CI), fun = mean, geom = "point") +
  stat_summary(aes(x=instance, y=CI, group = surveyID), fun = mean, geom = "line", size = 1)+
  geom_abline(intercept = 6, slope = 0, linetype = "dashed") +
  labs(title = "CI NO imputation data")+
  coord_cartesian(ylim = c(0,30))+
  theme_minimal() +
  theme(legend.position="none")


CI.plot <- CI.org.plot + CI.imp.plot
ggsave("YOQ/fig/CI_plot.pdf", CI.plot, height = 10, width = 14)









##### "SP" = Social Problems SUBSCORE IMPUTATION  -----

##  DATA PREP

# Prep data from original df (YOQ_full_nodup)
# Spread data
# Remove all rows with missing cases at MO6 OR YR1 AND all cases at DOA AND DOD
# Add column to denote when data is missing and when it is complete for both MO6 and YR1

SP_spread_prep <- select(YOQ_full_nodup, otClientId, clientName, instance, surveyID, 
                            assignedToRelationship, SP) #3912
SP_spread <- SP_spread_prep %>%                                            # 1684
  spread(key = "instance",
         value = "SP")
SP_miss <- SP_spread %>% subset(!is.na(MO6) | !is.na(YR1)) %>% subset(!is.na(DOA)) %>% subset(!is.na(DOD))   #574
SP_imput0 <- SP_miss %>% mutate(MO6_missing = label_missings(SP_miss$MO6, 
                                                                   missing = "missing",
                                                                   complete = "complete") ) %>% 
  mutate(YR1_missing = label_missings(SP_miss$YR1,
                                      missing = "missing",
                                      complete = "complete"))
tota_missing_tally <- SP_imput0 %>% group_by(surveyID, MO6_missing, YR1_missing) %>% tally()


## Imputate to fill in missing scores

# 1. Change numbers to doubles (impute_lm requires this)
# 2. Imput missing 6 Month data using simputation::impute_lm
# 3. Round to two decimal places
# 4. Bring back to long
# 5. Make factor


SP_imput1 <- SP_imput0 %>% mutate(MO6=as.double(MO6), YR1=as.double(YR1))
# SP_imput_MO6 <- SP_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1)
# SP_imput_YR1 <- SP_imput_MO6 %>% impute_lm(YR1 ~ DOA +DOD + MO6)
SP_imput <- SP_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1) %>% impute_lm(YR1 ~ DOA +DOD + MO6)
SP_imput$MO6 <- round(SP_imput$MO6, 2)
SP_imput$YR1 <- round(SP_imput$YR1, 2)
SP_imput_long <- gather(SP_imput, key = "instance", value = "SP", DOA:YR1)   #2956
SP_imput_long[,c("instance", "MO6_missing", "YR1_missing")] <- lapply(SP_imput_long[,c("instance", "MO6_missing", "YR1_missing")], factor)


## Test Stats
# select (client id, surveyid, and MO6 OR YR1), group_by survey Id, and give 5num stat with mean and sd

# MO6
SP_miss.6 <-  SP_miss %>% select(c(1,3,7)) %>%  na.omit %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(MO6)[1], Q1 = fivenum(MO6)[2], median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
SP_imput.6 <- SP_imput %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(MO6)[1],  Q1 = fivenum(MO6)[2], median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
# YR1
SP_miss.12 <- SP_miss %>% select(c(1,3,8)) %>%  na.omit %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(YR1)[1], Q1 = fivenum(YR1)[2], median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))
SP_imput.12 <- SP_imput %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(YR1)[1],  Q1 = fivenum(YR1)[2], median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))
# Add "dataframe" column denoting where values came from
SP_miss.6$dataframe <- "SP_miss.6" 
SP_imput.6$dataframe <- "SP_imput.6" 
SP_miss.12$dataframe <- "SP_miss.12" 
SP_imput.12$dataframe <- "SP_imput.12" 
# Bind together and round decimals 
missing_data_summary <- rbind(missing_data_summary, SP_miss.6, SP_imput.6, SP_miss.12, SP_imput.12)
missing_data_summary[,-c(1,10)] <- round(missing_data_summary[,-c(1,10)],2)


## Plots

SP.imp.plot <- ggplot(SP_imput_long, aes(color = surveyID)) + 
  stat_summary(aes(x=instance, y=SP), fun = mean, geom = "point") +
  stat_summary(aes(x=instance, y=SP, group = surveyID), fun = mean, geom = "line", size = 1)+
  geom_abline(intercept = 3, slope = 0, linetype = "dashed") +
  labs(title = "SP new imputation data", caption = "red = parent, blue = stu")+
  coord_cartesian(ylim = c(-2,20))+
  theme_minimal()+
  theme(legend.position="none")


# To compare apples to apples make a separate object: ID_miss_long
SP_miss_long <- gather(SP_miss, key = "instance", value = "SP", DOA:YR1)   #2956

SP.org.plot <- ggplot(SP_miss_long, aes(color = surveyID)) + 
  stat_summary(aes(x=instance, y=SP), fun = mean, geom = "point") +
  stat_summary(aes(x=instance, y=SP, group = surveyID), fun = mean, geom = "line", size = 1)+
  geom_abline(intercept = 3, slope = 0, linetype = "dashed") +
  labs(title = "SP NO imputation data")+
  coord_cartesian(ylim = c(-2, 20))+
  theme_minimal() +
  theme(legend.position="none")


SP.plot <- SP.org.plot + SP.imp.plot
ggsave("YOQ/fig/SP_plot.pdf", SP.plot, height = 10, width = 14)


##### "BD" = Behavioral Dysfunction SUBSCORE IMPUTATION  -----

##  DATA PREP

# Prep data from original df (YOQ_full_nodup)
# Spread data
# Remove all rows with missing cases at MO6 OR YR1 AND all cases at DOA AND DOD
# Add column to denote when data is missing and when it is complete for both MO6 and YR1

BD_spread_prep <- select(YOQ_full_nodup, otClientId, clientName, instance, surveyID, 
                            assignedToRelationship, BD) #3912
BD_spread <- BD_spread_prep %>%                                            # 1684
  spread(key = "instance",
         value = "BD")
BD_miss <- BD_spread %>% subset(!is.na(MO6) | !is.na(YR1)) %>% subset(!is.na(DOA)) %>% subset(!is.na(DOD))   #574
BD_imput0 <- BD_miss %>% mutate(MO6_missing = label_missings(BD_miss$MO6, 
                                                                   missing = "missing",
                                                                   complete = "complete") ) %>% 
  mutate(YR1_missing = label_missings(BD_miss$YR1,
                                      missing = "missing",
                                      complete = "complete"))
BD_missing_tally <- BD_imput0 %>% group_by(surveyID, MO6_missing, YR1_missing) %>% tally()


## Imputate to fill in missing scores

# 1. Change numbers to doubles (impute_lm requires this)
# 2. Imput missing 6 Month data using simputation::impute_lm
# 3. Round to two decimal places
# 4. Bring back to long
# 5. Make factor


BD_imput1 <- BD_imput0 %>% mutate(MO6=as.double(MO6), YR1=as.double(YR1))
# BD_imput_MO6 <- BD_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1)
# BD_imput_YR1 <- BD_imput_MO6 %>% impute_lm(YR1 ~ DOA +DOD + MO6)
BD_imput <- BD_imput1 %>% impute_lm(MO6 ~ DOA + DOD + YR1) %>% impute_lm(YR1 ~ DOA +DOD + MO6)
BD_imput$MO6 <- round(BD_imput$MO6, 2)
BD_imput$YR1 <- round(BD_imput$YR1, 2)
BD_imput_long <- gather(BD_imput, key = "instance", value = "BD", DOA:YR1)   #2956
BD_imput_long[,c("instance", "MO6_missing", "YR1_missing")] <- lapply(BD_imput_long[,c("instance", "MO6_missing", "YR1_missing")], factor)


## Test Stats
# select (client id, surveyid, and MO6 OR YR1), group_by survey Id, and give 5num stat with mean and sd

# MO6
BD_miss.6 <-  BD_miss %>% select(c(1,3,7)) %>%  na.omit %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(MO6)[1], Q1 = fivenum(MO6)[2], median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
BD_imput.6 <- BD_imput %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(MO6)[1],  Q1 = fivenum(MO6)[2], median = fivenum(MO6)[3],  Q3 = fivenum(MO6)[4], max = fivenum(MO6)[5], sd = sd(MO6), mean = mean(MO6))
# YR1
BD_miss.12 <- BD_miss %>% select(c(1,3,8)) %>%  na.omit %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(YR1)[1], Q1 = fivenum(YR1)[2], median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))
BD_imput.12 <- BD_imput %>% group_by(surveyID) %>% summarise(n = n(), min = fivenum(YR1)[1],  Q1 = fivenum(YR1)[2], median = fivenum(YR1)[3],  Q3 = fivenum(YR1)[4], max = fivenum(YR1)[5], sd = sd(YR1), mean = mean(YR1))
# Add "dataframe" column denoting where values came from
BD_miss.6$dataframe <- "BD_miss.6" 
BD_imput.6$dataframe <- "BD_imput.6" 
BD_miss.12$dataframe <- "BD_miss.12" 
BD_imput.12$dataframe <- "BD_imput.12" 
# Bind together and round decimals 
missing_data_summary <- rbind(missing_data_summary, BD_miss.6, BD_imput.6, BD_miss.12, BD_imput.12)
missing_data_summary[,-c(1,10)] <- round(missing_data_summary[,-c(1,10)],2)


## Plots

BD.imp.plot <- ggplot(BD_imput_long, aes(color = surveyID)) + 
  stat_summary(aes(x=instance, y=BD), fun = mean, geom = "point") +
  stat_summary(aes(x=instance, y=BD, group = surveyID), fun = mean, geom = "line", size = 1)+
  geom_abline(intercept = 11, slope = 0, linetype = "dashed") +
  labs(title = "BD new imputation data", caption = "red = parent, blue = stu")+
  coord_cartesian(ylim = c(0,30))+
  theme_minimal()+
  theme(legend.position="none")


# To compare apples to apples make a separate object: ID_miss_long
BD_miss_long <- gather(BD_miss, key = "instance", value = "BD", DOA:YR1)   #2956

BD.org.plot <- ggplot(BD_miss_long, aes(color = surveyID)) + 
  stat_summary(aes(x=instance, y=BD), fun = mean, geom = "point") +
  stat_summary(aes(x=instance, y=BD, group = surveyID), fun = mean, geom = "line", size = 1)+
  geom_abline(intercept = 11, slope = 0, linetype = "dashed") +
  labs(title = "BD NO imputation data")+
  coord_cartesian(ylim = c(0,30))+
  theme_minimal() +
  theme(legend.position="none")


BD.plot <- BD.org.plot + BD.imp.plot
ggsave("YOQ/fig/BD_plot.pdf", BD.plot, height = 10, width = 14)








#####################  MERGE ALL SCORES TOGETHER ----

# objects to merge: ID_imput_long, S_imput_long, IR_imput_long, CI_imput_long, 
# SP_imput_long, BD_imput_long, total_imput_long

object <- c("otClientId", "clientName", "surveyID", 
            "assignedToRelationship", "MO6_missing", "YR1_missing", "instance")

mer1 <- left_join(ID_imput_long, S_imput_long, 
                  by = object)
mer2 <- left_join(mer1, IR_imput_long, by = object)
mer3 <- left_join(mer2, CI_imput_long, by = object)
mer4 <- left_join(mer3, SP_imput_long, by = object)
mer5 <- left_join(mer4, BD_imput_long, by = object)
YOQ_imput_FULL <- left_join(mer5, total_imput_long, by = object)

YOQ_imput_FULL1 <- YOQ_imput_FULL
YOQ_imput_FULL$assignedToRelationship <- recode(YOQ_imput_FULL$assignedToRelationship, mother = "Mother", father = "Father")
# unique(YOQ_imput_FULL$assignedToRelationship)

# saveRDS(YOQ_imput_FULL, "YOQ/rds/updated_imput_BACKUP.rds")
saveRDS(YOQ_imput_FULL, "YOQ/rds/updated_imput.rds")







# How big is our new sample size??
YOQ_imput_FULL %>% filter(instance == "DOA") %>%  group_by(surveyID) %>% tally()
## 433 Parent cases and 141 Student cases


names(YOQ_imput_FULL)


############ BESTNOTES Diagnosis data ----

yoq <- readRDS("YOQ/rds/updated_imput.rds")
diag <- readRDS("ETC/rds/bestNotes_diag_wide.rds")

yoq_diag1 <- inner_join(yoq, diag, by = "otClientId")
# This is now a full dataset of all the YOQ scores by diagnosis 
# (repeated ID's due to parents.stus etc)

# Go to long
yoq_diag2 <- gather(yoq_diag1, key = "diag_instance", value = "diagnosis", 16:25)   # 4840 obs

# Convert to na and remove Remove NA's
yoq_diag3 <- yoq_diag2 %>% 
  mutate_all(na_if, "") %>% 
  mutate_all(na_if, "NONE") %>% 
  mutate_all(na_if, "R/O")

# Duplicate diagnosis column
yoq_diag3$diag_org <- yoq_diag3$diagnosis

# Change low amounts of data to "other" category in "diagnosis" column
yoq_diag3$diagnosis <- recode(yoq_diag3$diagnosis, life = "other", 
                              neuro = "other", personality = "other", 
                              tech = "other")
# Remove NA's
yoq_diag_cc1 <- yoq_diag3 %>% drop_na(diagnosis)  ## 2488
# Remove duplicates
yoq_diag <- yoq_diag_cc1[!duplicated(yoq_diag_cc1[c(1:14, 17)]),]

# Change to factors
yoq_diag[,c("diagnosis", "diag_instance", "diag_org")] <- lapply(
  yoq_diag[,c("diagnosis", "diag_instance", "diag_org")], factor)

# 
yoq_diag <- yoq_diag_cc

saveRDS(yoq_diag_cc, "YOQ/rds/YOQ_diag.rds")




###### Sample size charts ----

# DOWN TO 2108

# See sample size of imputation with diagnosis (broken out without diagnosis)
## confusing, just look at numbers
imp_sampsize <- yoq_diag_withoutdiag %>% filter(instance == "DOA") %>% group_by(surveyID, instance) %>% tally()
imp_sampsize$diagnosis <- "total"
imp_sampsize$instance <- NULL
## 83 parents, 33 students - this is cases that are not repeated

## 447 parents, 175 students
# Sample size of all diagnosis
## Because we want to know out sample size without diagnosis, make an object that is only based on surveyid, instance, and yoq scores
yoq_diag_withoutdiag <- yoq_diag_cc1[!duplicated(yoq_diag_cc1[c(1:14)]),]
imp_sampsize_diag <- yoq_diag_cc %>% filter(instance == "DOA") %>% group_by(surveyID, diagnosis) %>% tally()
imp_sampsize_diag$instance <- NULL
# imp_sampsize_diag includes cases that ARE repeated due to multiple diagnosis per student
imp_samp <- rbind(imp_sampsize, imp_sampsize_diag)

## Save these rds
saveRDS(imp_samp, "YOQ/rds/YOQ_diag_counts.rds")

write.csv(imp_samp, "YOQ/output/diagnosis with imputation sample sizes.csv")


####  ADDITIONAL MANIPULATION OF RDS ----

YOQ_new <- readRDS("YOQ/rds/updated_imput.rds")
YOQ_diag <- readRDS("YOQ/rds/YOQ_diag.rds")



YOQ_new$surveyID <- recode(YOQ_new$surveyID, YOQparent = "Parent", YOQself = "Student")
YOQ_diag$surveyID <- recode(YOQ_diag$surveyID, YOQparent = "Parent", YOQself = "Student")

saveRDS(YOQ_new, "YOQ/rds/updated_imput.rds")
saveRDS(YOQ_diag, "YOQ/rds/YOQ_diag.rds")

