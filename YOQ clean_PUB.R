library(tidyverse)


# YOQ CLEAN FROM ORIGINAL FILES

# Load original files to R
setwd("C:/Users/cullo/OneDrive/Desktop/One Cloud/ELEMENTS")

# 9-25-2020 data
yoq_stu <- read.csv("YOQ/data/YOQ-2.0 Self Report_Completed_Surveys_2020-09-25.csv")   # 1907
yoq_par <- read.csv("YOQ/data/YOQ-2.01 - Parent_Completed_Surveys_2020-09-25.csv")    # 3590

yoq_org <- readRDS("YOQ/rds/YOQ_ver1.rds")  # 4114 obs

# Clean up column names and select only needed

yoq_stu <- yoq_stu %>% 
  select(clientName:sessionId, currentClientProvider:DateOfDischarge, 
         assignedToRelationship:completeDate_instanceId,
         subscale_r2d2_ID:subscale_r2d2_SCORE) %>% 
  rename(Therapist = currentClientProvider, DOB = DateOfBirth, 
         DOA = DateOfAdmit, DOD = DateOfDischarge, ID = subscale_r2d2_ID,
         S = subscale_r2d2_S, IR = subscale_r2d2_IR, SP = subscale_r2d2_SP, 
         BD = subscale_r2d2_BD, CI = subscale_r2d2_CI, 
         TOTAL = subscale_r2d2_SCORE)
yoq_stu$surveyID <- "student"

yoq_par <- yoq_par %>% 
  select(clientName:sessionId, currentClientProvider:DateOfDischarge, 
         assignedToRelationship:completeDate_instanceId,
         subscale_r2d2_ID:subscale_r2d2_SCORE) %>% 
  rename(Therapist = currentClientProvider, DOB = DateOfBirth, 
         DOA = DateOfAdmit, DOD = DateOfDischarge, ID = subscale_r2d2_ID,
         S = subscale_r2d2_S, IR = subscale_r2d2_IR, SP = subscale_r2d2_SP, 
         BD = subscale_r2d2_BD, CI = subscale_r2d2_CI, 
         TOTAL = subscale_r2d2_SCORE)
yoq_par$surveyID <- "parent"


# Bind the two together
yoq_mast <- rbind(yoq_stu, yoq_par)

# Add instance column
yoq_mast$instance <- NA
# Populate with function of complete_sinceDOA
yoq_mast$instance <- yoq_mast$complete_sinceDOD

## Make instance column a function of breaks in the numerical data
# Define what the cut points are and the new category names
cuts <- c(-Inf, -15, 60, 285, Inf)
instances <- c("DOA", "DOD", "MO6", "YR1")
# Copy the numbers needed to instance category
yoq_mast$instance <- yoq_mast$complete_sinceDOD
# Apply the cut() function and denote breaks = and labels =
yoq_mast$instance <- cut(yoq_mast$instance, breaks = cuts, labels = instances)

# Spot check instance to instance_test and remove instance_test
yoq_mast$instance_test <- NULL


##### Check some counts
# yoq_mast %>% group_by(instance) %>% tally()
# DOA = 2106, DOD = 1815, 910 = 630
yoq_mast %>% group_by(surveyID, instance) %>% tally()
nrow(yoq_mast[!duplicated(yoq_mast["otClientId"]),])
# 943 total students with any kind of data


## Save RDS YOQ_ver1.RDS

