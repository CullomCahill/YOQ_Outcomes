# Script to merge all datasets downloaded from rrd2 
# containing consent information for clients (minor assent) 
# and parents/guardians (P/G consent)


# Data setup ----------
library(tidyverse)
setwd("C:/Users/cullo/OneDrive/Desktop/One Cloud/ELEMENTS")

con1_org <- read.csv("consent/1 NATSAP-MINOR-ASSENT-19_Completed_Surveys_2021-04-16.csv")   ## In other file
con2_org <- read.csv("consent/2 NATSAP MINOR ASSENT_Completed_Surveys_2021-04-16.csv")
# con3 <- read.csv("consent/3 G CONSENT_Completed_Surveys_2021-04-16__ NATSAP PG CONSENT.csv")  == EMPTY df
con4_org <- read.csv("consent/4 NATSAP-MINOR-ASSENT-19_Completed_Surveys_2021-04-16.csv")
con5_org <- read.csv("consent/5 NATSAP-MINOR-ASSENT-19_Completed_Surveys_2021-04-16.csv")
con6_org <- read.csv("consent/6 NATSAP-PG-CONSENT-19_Completed_Surveys_2021-04-16.csv")     ## In other file
# New file with missing values from outcometools
con7_org <- read.csv("consent/ElementsWildernessProgram-NATSAP-P_G-CONSENT.csv")   #1017 



## Clean newly imported dataset
con1 <- con1_org %>% 
  rename(consent = `Q1..Do.you.CONSENT.or.DECLINE.participation.in.the.NATSAP.research.study.as.described.above.`) %>% 
  select(clientName, otClientId, assignedToRelationship, consent)

con2 <- con2_org %>% 
  rename(consent = `Q1..Do.you.CONSENT.or.DECLINE.participation.in.the.NATSAP.research.study.as.described.above.`) %>% 
  select(clientName, otClientId, assignedToRelationship, consent)

con4 <- con4_org %>% 
  rename(consent = `Q1..Do.you.CONSENT.or.DECLINE.participation.in.the.NATSAP.research.study.as.described.above.`) %>% 
  select(clientName, otClientId, assignedToRelationship, consent)

con5 <- con5_org %>% 
  rename(consent = `Q1..Do.you.CONSENT.or.DECLINE.participation.in.the.NATSAP.research.study.as.described.above.`) %>% 
  select(clientName, otClientId, assignedToRelationship, consent)

con6 <- con6_org %>% 
  rename(consent = `Q1..Do.you.CONSENT.or.DECLINE.participation.in.the.NATSAP.research.study.as.described.above.`) %>% 
  select(clientName, otClientId, assignedToRelationship, consent)


# Bind into master file
con_mast0 <- rbind(con1, con2, con4, con5, con6)  # 2165




## Now add the other file that is linked by CID and not otClientId.----

# merge first and last name to one column
con7_org$clientName <- paste(con7_org$FirstName, con7_org$LastName)

# Rename and pull out needed
con7 <- con7_org %>% 
  rename(consent = Q1, assignedToRelationship = Relationship) %>%    # 1016
  select(CID, clientName, DOD, assignedToRelationship, consent) %>% 
  na.omit()
con7$DOD <- as.Date(con7$DOD, "%m/%d/%Y")


# Merge otClientId numbers based on CID
master_names <- readRDS("ETC/rds/master list of names and id.rds")   # 1002
master_names <- rename(master_names, CID = remoteClientId)
master_names$CID <- as.numeric(master_names$CID)

con7_id <- left_join(con7, master_names, by = "CID")  # 1016
con7_id <- con7_id %>% 
  rename(clientName = clientName.x, DOD = DOD.x) %>% 
  select(clientName, otClientId, assignedToRelationship, consent)


# Add this new object to the consent master object
con_mast0.1 <- rbind(con_mast0, con7_id)  #3181



# Remove duplicates
con_mast1 <- con_mast0.1[!duplicated(con_mast0.1),]  # 1454 // now: 2400
# Try again to make sure no consents are messed up
con_mast2 <- con_mast0.1[!duplicated(con_mast0.1[2:3]),] # 1447 // 2384  go with this one, just a couple weird ones to throw out
# And check number of student id's overall, should be like 815
con_mast3 <- con_mast0.1[!duplicated(con_mast0.1[2]),] # 990  Great!  should merge nicely

# consent master to use is con_mast2
con_mast <- con_mast2

# Check numbers
con_mast %>% group_by( consent) %>% tally()  
# 2149 consent, 235 decline with no NA's


## Save this consent file as a rds
setwd("C:/Users/cullo/OneDrive/Desktop/One Cloud/Resume 2022")
saveRDS(con_mast, "Github Portfolio/rds/consent master list.rds")


