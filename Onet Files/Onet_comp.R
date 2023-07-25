
# clear data and close graphs
rm(list=ls())
graphics.off()

# set working directory
setwd("C:/Users/Ibrahim/OneDrive/Desktop/Brookfield 2023/Microcredentials/Onet Files")

library(data.table)
library(stringr)
library(psych)
library(knitr)
library(tidyverse)
library(readxl)


# Read in O*NET data and BII+E crosswalk
tech_skill <- as.data.table(read_excel("Technology Skills.xlsx"))
crosswalk <- as.data.table(read.csv("complete crosswalks.csv"))

names(tech_skill) <- c("onet","title","skill","code","skill_group","hot_tech","indemand")

# Link O*NET data to NOC using crosswalk
setkey(crosswalk,onet)
setkey(tech_skill,onet)
full.crosswalk.skill <- crosswalk[tech_skill,nomatch=0,allow.cartesian=TRUE]

#Clean environment
rm(tech_skill)
rm(crosswalk)


#Import top skills to compare
data_scientists_topskills <- as.data.table(read_excel("topskills_datascientists.xlsx"))

#Fix the names of some skills
data_scientists_topskills$`Common Skill` <- ifelse(data_scientists_topskills$`Common Skill` == "Java", "JavaScript", data_scientists_topskills$`Common Skill`)
data_scientists_topskills$`Common Skill` <- ifelse(data_scientists_topskills$`Common Skill` == "C (Programming Language)", "C", data_scientists_topskills$`Common Skill`)


# Merge the two data tables based on the common column
merged_data <- merge(data_scientists_topskills, full.crosswalk.skill, by.x = "Common Skill", by.y = "skill")

#NOCs in our study
nocs <- c("21211", "21231", "21232") #DS - SE - SE

#Filter for our NOCs
data <- subset(merged_data, noc_2021 %in% nocs)

#Clean environment
rm(data_scientists_topskills)
rm(full.crosswalk.skill)
rm(merged_data)
rm(nocs)

#Filter for needed columns
data <- data[,c("noc_2021","noc_title","Common Skill","Difference","AbsoluteDifference","Group","skill_group","hot_tech","indemand")]









