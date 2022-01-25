###  Wrangling script for diagnosis data from best notes

library(tidyverse)

setwd("/Users/applesaint/Desktop/My Things/ELEMENTS")
# AQD_19.1 <- readRDS("RDS files/AQD_19.rds")

#### Create sample ----

# Create a sample of first 100
#set.seed(1)
#sample <- sample_n(AQD_19.1, 100)
#saveRDS(file = "RDS files/diag_sample.rds", sample)
#write.csv(sample, file = "working csvs/diag_sample.csv")
# Now repeate that sample for entire population (since 2019)
#set.seed(1)
#full <- sample_n(AQD_19.1, 228)
#write.csv(full, file = "working csvs/diag_full.csv")


#### Bring diagnosis csv into script ----

# Bring in the data set from Bestnotes on diagnosis 
diag <- read.csv("ETC/data/diag_full_bestnotes.csv")  # 227




###### LONG DATAFRAME 

# Subset only needed values from data.frame
diag.sel <- diag %>% select(otClientId, Therapist, diag_1_bin, diag_2_bin, diag_3_bin, diag_4_bin, 
                      diag_5_bin, diag_6_bin, diag_7_bin, diag_8_bin, diag_9_bin, , diag_10_bin)
# Convert data to long format
diag.long <- gather(diag.sel, instance, diagnosis, diag_1_bin:diag_10_bin, factor_key = TRUE)    # 2270 obs

# Before removing repeates, save as rds wide and long REPEATING
saveRDS(diag.long, "ETC/rds/bestNotes_diag_long_REPEATING.rds")
saveRDS(diag.sel, "ETC/rds/bestNotes_diag_wide_REPEATING.rds")


# Remove cases that are repeated diagnostic bins within one case (marajuana and alochol, both substance but shouldnt count as two)
diag.long1 <- diag.long[!duplicated(diag.long[c(1,4)]),]            # 1192 obs
# Make diagnostic bins factors
diag.long1$diagnosis <- as.factor(diag.long1$diagnosis)
diag.long1$Therapist <- as.factor(diag.long1$Therapist)

# Rename rows that have value of " " spaces in diagnosis 
diag.long1[diag.long1$diagnosis == "",] <- NA
# And remove these
diag.long2 <- na.omit(diag.long1)                                      # Removed those 220 to bring obs to 972
# Remove NA's, R/O and NONE
# Remove blank spaces
diag.long2[diag.long2$diagnosis == "", ] <- NA
diag.long3 <- diag.long2 %>% filter(diagnosis != "NONE" & diagnosis != "R/O")   # 942 observations


# Make count object of diagnosis without other
diag.full.count <- diag.long3 %>% group_by(diagnosis) %>% tally()
# saveRDS(diag.full.count, "ETC/rds/count of each diagnosis_NO OTHER.rds")
diag.full.count <- readRDS("ETC/rds/count of each diagnosis_NO OTHER.rds")


## REPLACE tech, personality, neuro, life with "other"  :: anything less than 30 individuals
diag.long.other <- diag.long3
diag.long.other$diagnosis <- recode(diag.long3$diagnosis, tech = "other", personality = "other", neuro = "other", life = "other")
# Drop the unused levels
diag.long.other <- droplevels(diag.long.other, "NONE", "R/O")
# Change instance to diag_inst
diag.long.other <- diag.long.other %>% rename(diag_inst = instance)
# And put to Wide
diag.wide.other <- spread(diag.long.other, key = diag_inst, value = "diagnosis")
## 211, missing 16 observations??

####### Save RDS diag.long3, diag.sel----

# Save files withOUT other category
saveRDS(diag.long3, file = "ETC/rds/diag_long_noOther.rds")
saveRDS(diag.sel, file = "ETC/rds/diag_wide_noOther.rds")

# Save files WITH OTHER 
saveRDS(diag.long.other, file = "ETC/rds/bestNotes_diag_long.rds")
saveRDS(diag.wide.other , file = "ETC/rds/bestNotes_diag_wide.rds")


comorbidity_tally_chart.rds <- readRDS("ETC/rds/comorbidity_tally_chart.rds")







#### PLOTS ----

# Run a bar graph

diag.long.other <- diag.long.other %>% group_by(diagnosis) %>% mutate(count_name_occurr = n())  # First count the number of occurances, this adds a column to df with number of occurances

diagnostic_cat1 <- ggplot(diag.long.other) + geom_bar(aes(x = reorder(diagnosis, count_name_occurr), fill = diagnosis))+
  labs(title = "2019-2020 EWP Student Diagnostic Categories", subtitle = "All Categories", 
       caption = "Students may have more than one diagnosis, NONE and R/O not listed")+
  xlab("Diagnostic Categories")
diagnostic_cat1
diagnostic_cat2 <- diagnostic_cat1 + facet_wrap(~Therapist)
diagnostic_cat2


# Save these plots
# ggsave("Reports/graphs/diag_BN_data_newVAR.pdf", plot = diagnostic_cat1, height = 7 , width = 10)
# ggsave("Reports/graphs/diag_BN_data_newVAR_therapists.pdf", plot = diagnostic_cat2, height = 10 , width = 15)



###  Pie chart based on full data set ----

# Produce a count of each factor variable
diag.count <- diag.long.other %>% count(diagnosis)

diag.full.pie <- ggplot(diag.count, aes(x="", y=n, fill=diagnosis))+
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0)+
  geom_col(color="white") +
  theme_void()+
  labs(title="Clients Treated at EWP", vjust=1)+
  labs(subtitle = "Diagnosis at discharge", caption = "Students may have more than one diagnosis, NONE and R/O not listed")+
  theme(plot.title = element_text(hjust = 0))+
  theme(plot.subtitle = element_text(hjust=0)) + 
  theme(plot.caption = element_text(hjust=1)) 
diag.full.pie
  
# ggsave("ETC/fig/diag_BN_data_pie.pdf", diag.full.pie, height = 7, width = 10)





######## COMORBIDITITES ----


# Substance abuse ----

# Filter out substance abuse diagnosis from overall long data and remove count_name_occurr
diag.sub1 <- diag.long.other %>% filter(diagnosis == "substance")
diag.sub1$count_name_occurr <- NULL
# Then merge this dataset to the original file
diag.sub2 <- merge(diag.sub1, diag.wide.other, by = "otClientId", all = FALSE)
# Select the columns you'll want to move to long format
diag.sub3 <- select(diag.sub2, otClientId, Therapist.x, diag_1_bin, diag_2_bin, diag_3_bin, diag_4_bin, 
                    diag_5_bin, diag_6_bin, diag_7_bin, diag_8_bin, diag_9_bin, , diag_10_bin)
# Gather to long format
diag.sub.long <- gather(diag.sub3, key = instance, value = diagnosis, diag_1_bin:diag_10_bin, factor_key = TRUE)
# Rename therapist.x 
diag.sub.long1 <- rename(diag.sub.long, Therapist = Therapist.x, diag_inst = instance)
# Add diag.sub.long1 and diag.sub1 together 
diag.sub.full <- rbind(diag.sub.long1, diag.sub1)
# And remove both cases that are duplicated 
diag.sub.full1 <- diag.sub.full[!(duplicated(diag.sub.full) | duplicated(diag.sub.full, fromLast = TRUE)),]
# Now remove duplicated diagnosises within each case
diag.sub.full2 <- diag.sub.full1[!duplicated(diag.sub.full1[c(1,4)]),] 
# And remove NA's
diag.sub.full3 <- na.omit(diag.sub.full2)           
# Remove NA's, R/O and NONE
sub.comorbidity <- diag.sub.full3 %>% filter(diagnosis != "NONE" & diagnosis != "R/O")   # 


## This gives us a dataframe (substance) that is the comorbidities of substance abuse
##   i.e. any student who has substance abuse listed, here is what else they had listed

## SUBSTANCE ABUSE PLOTS ----

sub.comorbidity1 <- sub.comorbidity %>% group_by(diagnosis) %>% mutate(count_name_occurr = n())  # First count the number of occurances, this adds a column to df with number of occurances

sub.comorbidity_bar <- ggplot(sub.comorbidity1) + geom_bar(aes(x = reorder(diagnosis, count_name_occurr), fill = diagnosis))+
  labs(title = "2019-2020 EWP Substance Abuse Comorbidities",  
       caption = "Students suffering with substance abuse issues were also diagnosed with above (including other substance abuse issues: alcohol AND cannabis for example)")+
  xlab("Diagnostic Categories")
sub.comorbidity_bar
sub.comorbidity_bar2 <- sub.comorbidity_bar + facet_wrap(~Therapist)
sub.comorbidity_bar2
# Save 
#ggsave("Reports/Comorbidities/substance abuse comorbidity bar graph.pdf", sub.comorbidity_bar, height = 7, width = 10)
#ggsave("Reports/Comorbidities/substance abuse comorbidity bar graph_by therapist.pdf", sub.comorbidity_bar2, height = 10, width = 15)
