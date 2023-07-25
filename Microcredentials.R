# clear data and close graphs
rm(list=ls())
graphics.off()

# set working directory
setwd("C:/Users/Ibrahim/OneDrive/Desktop/Brookfield 2023/Microcredentials")

# Load the necessary libraries
library(data.table)
library(stringr)
library(psych)
library(knitr)
library(tidyverse)
library(readxl)
library(dplyr)
library(treemap)
library(extrafont)
library(showtext)
library(ggplot2)
library(DaisTheme)
library(scales)
library(openxlsx)


# Read in data
# Create an empty list to store the data.tables
data_list <- list()

stemvsbhase <- as.data.table(read_excel("Broad Fields of Study.xlsx"))

#################################
#IMPORT FIRST EXCEL FILE 
##################################

sheets <- excel_sheets("data-scientist-21211.xlsx")

# Loop through each sheet and read it into a data.table
for (sheet in sheets) {
  data <- as.data.table(read_excel("data-scientist-21211.xlsx", sheet = sheet))
  setnames(data, make.names(names(data)))
  data_list[[sheet]] <- data
}

# Unpack the data_list with modified names
for (i in seq_along(data_list)) {
  assign(paste0("data_scientist_all_", names(data_list)[i]), data_list[[i]])
}

rm(data)
rm(data_list)
rm(i)
rm(sheet)
rm(sheets)


#################################
#IMPORT SECOND EXCEL FILE 
##################################

# Create an empty list to store the data.tables
data_list <- list()

sheets <- excel_sheets("data-scientist-mc2.xlsx")

# Loop through each sheet and read it into a data.table
for (sheet in sheets) {
  data <- as.data.table(read_excel("data-scientist-mc2.xlsx", sheet = sheet))
  setnames(data, make.names(names(data)))
  data_list[[sheet]] <- data
}

# Unpack the data_list with modified names
for (i in seq_along(data_list)) {
  assign(paste0("data_scientist_mc_", names(data_list)[i]), data_list[[i]])
}

rm(data)
rm(data_list)
rm(i)
rm(sheet)
rm(sheets)


#################################
#IMPORT THIRD EXCEL FILE 
##################################

# Create an empty list to store the data.tables
data_list <- list()

sheets <- excel_sheets("software-engineer-developer-no-m.xlsx")

# Loop through each sheet and read it into a data.table
for (sheet in sheets) {
  data <- as.data.table(read_excel("software-engineer-developer-no-m.xlsx", sheet = sheet))
  setnames(data, make.names(names(data)))
  data_list[[sheet]] <- data
}

# Unpack the data_list with modified names
for (i in seq_along(data_list)) {
  assign(paste0("software_all_", names(data_list)[i]), data_list[[i]])
}

rm(data)
rm(data_list)
rm(i)
rm(sheet)
rm(sheets)

#################################
#IMPORT FOURTH EXCEL FILE 
##################################

# Create an empty list to store the data.tables
data_list <- list()

sheets <- excel_sheets("software-engineer-with-mc2.xlsx")

# Loop through each sheet and read it into a data.table
for (sheet in sheets) {
  data <- as.data.table(read_excel("software-engineer-with-mc2.xlsx", sheet = sheet))
  setnames(data, make.names(names(data)))
  data_list[[sheet]] <- data
}

# Unpack the data_list with modified names
for (i in seq_along(data_list)) {
  assign(paste0("software_mc_", names(data_list)[i]), data_list[[i]])
}

rm(data)
rm(data_list)
rm(i)
rm(sheet)
rm(sheets)



###############################################################
###############################################################
###############################################################
###############################################################


# LOCATION
merged_data <- merge(data_scientist_all_Locations, data_scientist_mc_Locations, by = "Location", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

data_scientists_withoutmc_location <- data.table(Location = merged_data$Location,
                                                 Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)

merged_data <- merge(software_all_Locations, software_mc_Locations, by = "Location", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

software_withoutmc_location <- data.table(Location = merged_data$Location,
                                          Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)

# Companies
merged_data <- merge(data_scientist_all_Companies, data_scientist_mc_Companies, by = "Company", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

data_scientists_withoutmc_company <- data.table(Company = merged_data$Company,
                                                Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)

merged_data <- merge(software_all_Companies, software_mc_Companies, by = "Company", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

software_withoutmc_company <- data.table(Company = merged_data$Company,
                                         Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)

# Common Titles
merged_data <- merge(`data_scientist_all_Common Titles`, `data_scientist_mc_Common Titles`, by = "Titles", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

data_scientists_withoutmc_common_titles <- data.table(`Common Title` = merged_data$`Titles`,
                                                      Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)

merged_data <- merge(`software_all_Common Titles`, `software_mc_Common Titles`, by = "Titles", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

software_withoutmc_common_titles <- data.table(`Common Title` = merged_data$`Titles`,
                                               Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)

# Common Skills
merged_data <- merge(`data_scientist_all_Common Skills`, `data_scientist_mc_Common Skills`, by = "Skills", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

data_scientists_withoutmc_common_skills <- data.table(`Common Skill` = merged_data$`Skills`,
                                                      Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)

merged_data <- merge(`software_all_Common Skills`, `software_mc_Common Skills`, by = "Skills", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

software_withoutmc_common_skills <- data.table(`Common Skill` = merged_data$`Skills`,
                                               Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)


# Industries
merged_data <- merge(data_scientist_all_Industries, data_scientist_mc_Industries, by = "Industry", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

data_scientists_withoutmc_industries <- data.table(Industry = merged_data$Industry,
                                                   Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)

merged_data <- merge(software_all_Industries, software_mc_Industries, by = "Industry", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

software_withoutmc_industries <- data.table(Industry = merged_data$Industry,
                                            Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)

# Schools
merged_data <- merge(data_scientist_all_Schools, data_scientist_mc_Schools, by = "Schools", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

data_scientists_withoutmc_schools <- data.table(School = merged_data$School,
                                                Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)

merged_data <- merge(software_all_Schools, software_mc_Schools, by = "Schools", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

software_withoutmc_schools <- data.table(School = merged_data$School,
                                         Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)

# Fields of Study
merged_data <- merge(`data_scientist_all_Fields of Study`, `data_scientist_mc_Fields of Study`, by = "Fields.of.study", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

data_scientists_withoutmc_fields_of_study <- data.table(`Field of Study` = merged_data$`Fields.of.study`,
                                                        Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)

merged_data <- merge(`software_all_Fields of Study`, `software_mc_Fields of Study`, by = "Fields.of.study", suffixes = c("_all", "_mc"), all.x = TRUE)
merged_data[is.na(Professionals_mc), Professionals_mc := 0]

software_withoutmc_fields_of_study <- data.table(`Field of Study` = merged_data$`Fields.of.study`,
                                                 Professionals = merged_data$Professionals_all - merged_data$Professionals_mc)




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

######## COMPARISONS #########

##### DATA SCIENTISTS ######

# COMMON SKILLS

# create a new datatable that combines the data from both datatables
data <- rbind(
  data_scientists_withoutmc_common_skills[, .(Group = "Without MC", `Common Skill`, Professionals)],
  `data_scientist_mc_Common Skills`[, .(Group = "With MC", `Common Skill` = Skills, Professionals)]
)


# normalize the Professionals variable within each group
data[, NormalizedProfessionals := ifelse(Group == "With MC", Professionals / sum(data_scientist_mc_Locations$Professionals), Professionals / sum(data_scientists_withoutmc_location$Professionals))]


# Calculate the differences in professionals between individuals with and without microcredentials for each common skill
data_diff <- data[, .(Difference = diff(NormalizedProfessionals)), by = .(`Common Skill`)]


# Calculate the absolute differences
data_diff[, AbsoluteDifference := abs(Difference)]

# Sort the data in descending order of the absolute difference
data_diff <- data_diff[order(-AbsoluteDifference)]

# Select the top 10 values of the absolute differences
top_10_diff <- head(data_diff, 10)

###

# Sort the data in descending order of the difference
data_diff <- data_diff[order(-Difference)]

# Select the top 10 values where MC is higher
top_10_diff_mc <- head(data_diff, 10)

###

# Sort the data in ascending order of the difference
data_diff <- data_diff[order(Difference)]

# Select the top 10 values where without MC is higher
top_10_diff_withoutmc <- head(data_diff, 10)

###

# Filter data for common skills in top_10_diff
filtered_data <- data[`Common Skill` %in% top_10_diff$`Common Skill`]

# Filter data for common skills in top_10_diff_mc
filtered_data_mc <- data[`Common Skill` %in% top_10_diff_mc$`Common Skill`]

# Filter data for common skills in top_10_diff_withoutmc
filtered_data_withoutmc <- data[`Common Skill` %in% top_10_diff_withoutmc$`Common Skill`]


# ggplot(filtered_data, aes(x = `Common Skill`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
 # ggplot(filtered_data_mc, aes(x = `Common Skill`, y = NormalizedProfessionals, fill = Group)) +
 #   geom_bar(stat = "identity", position = "dodge") +
 #   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
 #   theme(panel.background=element_blank()) +
 #   dais.base.theme() +
 #   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
 #   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_withoutmc, aes(x = `Common Skill`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))



#Top 5s
data_diff <- data_diff[order(Difference)]
top_5_diff_withoutmc <- head(data_diff, 5)
data_diff <- data_diff[order(-Difference)]
top_5_diff_mc <- head(data_diff, 5)

combined_data <- rbind(transform(top_5_diff_mc, Group = "With MC"),
                       transform(top_5_diff_withoutmc, Group = "Without MC"))


# Graph with bars on the right and left
ggplot(combined_data, aes(x = `Difference`, y = `Common Skill`, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("With MC" = "#eb0072", "Without MC" = "#6bbfae")) +
  labs(title = "Figure 1", 
       subtitle = "Comparison of % Differences in Skills with and without MC", 
       x = "Difference", y = "Common Skill", fill = "Group",
       caption = "Source: Linkedin Insights Data" ) +
  dais.base.theme() +
  theme(panel.background=element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family = "Replica-Regular")) +
  geom_text(aes(label = percent(round(abs(`Difference`), 3))), 
            position = position_dodge(width = 0.9), vjust = 0.5, color = "black", family = "Replica-Regular")


ggsave("Figure_1.pdf", plot = last_plot(), width = 7.25, height = 7.25, units = "in")




#write.xlsx(combined_data, "topskills_datascientists.xlsx", rowNames = FALSE)



# Define some Dais colours 
dais_palette <- c("#DDA5C0", "#6bbfae" , "#ffa300",  "#002d72", "#eb0072" ,"#5bc2f4" )


# Common Skills treemap
treemap(data,
        index = c("Group", "Common Skill"),
        vSize = "NormalizedProfessionals",
        title = "Comparison of Common Skills",
        palette = dais_palette,
        #fontfamily.title = "Rooneysans",
        #fontfamily.labels = "Rooneysans",
        #fontfamily.legend = "Rooneysans",
        fontsize.labels = 12)

######################################################################

# Industry

# create a new datatable that combines the data from both datatables
data <- rbind(
  data_scientists_withoutmc_industries[, .(Group = "Without MC", Industry, Professionals)],
  data_scientist_mc_Industries[, .(Group = "With MC", Industry, Professionals)]
)

# normalize the Professionals variable within each group
data[, NormalizedProfessionals := ifelse(Group == "With MC", Professionals / sum(data_scientist_mc_Locations$Professionals), Professionals / sum(data_scientists_withoutmc_location$Professionals))]

# Calculate the differences in professionals between individuals with and without microcredentials for each industry
data_diff <- data[, .(Difference = diff(NormalizedProfessionals)), by = .(Industry)]

# Calculate the absolute differences
data_diff[, AbsoluteDifference := abs(Difference)]

# Sort the data in descending order of the absolute difference
data_diff <- data_diff[order(-AbsoluteDifference)]

# Select the top 10 values of the absolute differences
top_10_diff <- head(data_diff, 10)

###

# Sort the data in descending order of the difference
data_diff <- data_diff[order(-Difference)]

# Select the top 10 values where MC is higher
top_10_diff_mc <- head(data_diff, 10)

###

# Sort the data in ascending order of the difference
data_diff <- data_diff[order(Difference)]

# Select the top 10 values where without MC is higher
top_10_diff_withoutmc <- head(data_diff, 10)

###

# Filter data in top_10_diff
filtered_data <- data[Industry %in% top_10_diff$Industry]

# Filter data in top_10_diff_mc
filtered_data_mc <- data[Industry %in% top_10_diff_mc$Industry]

# Filter data in top_10_diff_withoutmc
filtered_data_withoutmc <- data[Industry %in% top_10_diff_withoutmc$Industry]


# ggplot(filtered_data, aes(x = Industry, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_mc, aes(x = Industry, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_withoutmc, aes(x = Industry, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))


#Top 5s
data_diff <- data_diff[order(Difference)]
top_5_diff_withoutmc <- head(data_diff, 5)
data_diff <- data_diff[order(-Difference)]
top_5_diff_mc <- head(data_diff, 5)

combined_data <- rbind(transform(top_5_diff_mc, Group = "With MC"),
                       transform(top_5_diff_withoutmc, Group = "Without MC"))


# Graph with bars on the right and left
ggplot(combined_data, aes(x = `Difference`, y = `Industry`, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("With MC" = "#eb0072", "Without MC" = "#6bbfae")) +
  labs(title = "Figure 1", 
       subtitle = "Comparison of % Differences in Industry with and without MC", 
       x = "Difference", y = "Industry", fill = "Group",
       caption = "Source: Linkedin Insights Data" ) +
  dais.base.theme() +
  theme(panel.background=element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family = "Replica-Regular")) +
  geom_text(aes(label = percent(round(abs(`Difference`), 3))), 
            position = position_dodge(width = 0.9), vjust = 0.5, color = "black", family = "Replica-Regular")

ggsave("Figure_2.pdf", plot = last_plot(), width = 7.25, height = 7.25, units = "in")




# Industries treemap

treemap(data,
        index = c("Group", "Industry"),
        vSize = "NormalizedProfessionals",
        title = "Comparison of Industries",
        palette = dais_palette,
        #fontfamily.title = "Rooneysans",
        #fontfamily.labels = "Rooneysans",
        #fontfamily.legend = "Rooneysans",
        fontsize.labels = 12)

######################################################################

# Common Titles

# create a new datatable that combines the data from both datatables
data <- rbind(
  data_scientists_withoutmc_common_titles[, .(Group = "Without MC", `Common Title`, Professionals)],
  `data_scientist_mc_Common Titles`[, .(Group = "With MC", `Common Title` = Titles, Professionals)]
)

# normalize the Professionals variable within each group
data[, NormalizedProfessionals := ifelse(Group == "With MC", Professionals / sum(data_scientist_mc_Locations$Professionals), Professionals / sum(data_scientists_withoutmc_location$Professionals))]

# Calculate the differences in professionals between individuals with and without microcredentials for each common title
data_diff <- data[, .(Difference = diff(NormalizedProfessionals)), by = .(`Common Title`)]

# Calculate the absolute differences
data_diff[, AbsoluteDifference := abs(Difference)]

# Sort the data in descending order of the absolute difference
data_diff <- data_diff[order(-AbsoluteDifference)]

# Select the top 10 values of the absolute differences
top_10_diff <- head(data_diff, 10)

###

# Sort the data in descending order of the difference
data_diff <- data_diff[order(-Difference)]

# Select the top 10 values where MC is higher
top_10_diff_mc <- head(data_diff, 10)

###

# Sort the data in ascending order of the difference
data_diff <- data_diff[order(Difference)]

# Select the top 10 values where without MC is higher
top_10_diff_withoutmc <- head(data_diff, 10)

###

# Filter data in top_10_diff
filtered_data <- data[`Common Title` %in% top_10_diff$`Common Title`]

# Filter data in top_10_diff_mc
filtered_data_mc <- data[`Common Title` %in% top_10_diff_mc$`Common Title`]

# Filter data in top_10_diff_withoutmc
filtered_data_withoutmc <- data[`Common Title` %in% top_10_diff_withoutmc$`Common Title`]


# ggplot(filtered_data, aes(x = `Common Title`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_mc, aes(x = `Common Title`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_withoutmc, aes(x = `Common Title`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))



#Top 5s
data_diff <- data_diff[order(Difference)]
top_5_diff_withoutmc <- head(data_diff, 5)
data_diff <- data_diff[order(-Difference)]
top_5_diff_mc <- head(data_diff, 5)

combined_data <- rbind(transform(top_5_diff_mc, Group = "With MC"),
                       transform(top_5_diff_withoutmc, Group = "Without MC"))


# Graph with bars on the right and left
ggplot(combined_data, aes(x = `Difference`, y = `Common Title`, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("With MC" = "#eb0072", "Without MC" = "#6bbfae")) +
  labs(title = "Figure 1", 
       subtitle = "Comparison of % Differences in Common Titles with and without MC", 
       x = "Difference", y = "Common Title", fill = "Group",
       caption = "Source: Linkedin Insights Data" ) +
  dais.base.theme() +
  theme(panel.background=element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family = "Replica-Regular")) +
  geom_text(aes(label = percent(round(abs(`Difference`), 3))), 
            position = position_dodge(width = 0.9), vjust = 0.5, color = "black", family = "Replica-Regular")

ggsave("Figure_3.pdf", plot = last_plot(), width = 7.25, height = 7.25, units = "in")




# Common Titles treemap

treemap(data,
        index = c("Group", "Common Title"),
        vSize = "NormalizedProfessionals",
        title = "Comparison of Common Titles",
        palette = dais_palette,
        #fontfamily.title = "Rooneysans",
        #fontfamily.labels = "Rooneysans",
        #fontfamily.legend = "Rooneysans",
        fontsize.labels = 12)

######################################################################

# Fields of Study

# create a new datatable that combines the data from both datatables
data <- rbind(
  data_scientists_withoutmc_fields_of_study[, .(Group = "Without MC", `Field of Study`, Professionals)],
  `data_scientist_mc_Fields of Study`[, .(Group = "With MC", `Field of Study` = Fields.of.study, Professionals)]
)

# normalize the Professionals variable within each group
data[, NormalizedProfessionals := ifelse(Group == "With MC", Professionals / sum(data_scientist_mc_Locations$Professionals), Professionals / sum(data_scientists_withoutmc_location$Professionals))]

# Calculate the differences in professionals between individuals with and without microcredentials for each field of study
data_diff <- data[, .(Difference = diff(NormalizedProfessionals)), by = .(`Field of Study`)]

# Calculate the absolute differences
data_diff[, AbsoluteDifference := abs(Difference)]

# Sort the data in descending order of the absolute difference
data_diff <- data_diff[order(-AbsoluteDifference)]

# Select the top 10 values of the absolute differences
top_10_diff <- head(data_diff, 10)

###

# Sort the data in descending order of the difference
data_diff <- data_diff[order(-Difference)]

# Select the top 10 values where MC is higher
top_10_diff_mc <- head(data_diff, 10)

###

# Sort the data in ascending order of the difference
data_diff <- data_diff[order(Difference)]

# Select the top 10 values where without MC is higher
top_10_diff_withoutmc <- head(data_diff, 10)

###

# Filter data in top_10_diff
filtered_data <- data[`Field of Study` %in% top_10_diff$`Field of Study`]

# Filter data in top_10_diff_mc
filtered_data_mc <- data[`Field of Study` %in% top_10_diff_mc$`Field of Study`]

# Filter data in top_10_diff_withoutmc
filtered_data_withoutmc <- data[`Field of Study` %in% top_10_diff_withoutmc$`Field of Study`]


# ggplot(filtered_data, aes(x = `Field of Study`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
 ggplot(filtered_data_mc, aes(x = `Field of Study`, y = NormalizedProfessionals, fill = Group)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Field of Study", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
   dais.base.theme() +
   theme(panel.background=element_blank()) +
   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
   scale_fill_manual(values = c("#eb0072", "#6bbfae"))

# ggplot(filtered_data_withoutmc, aes(x = `Field of Study`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))


#Top 5s
data_diff <- data_diff[order(Difference)]
top_5_diff_withoutmc <- head(data_diff, 5)
data_diff <- data_diff[order(-Difference)]
top_5_diff_mc <- head(data_diff, 5)

combined_data <- rbind(transform(top_5_diff_mc, Group = "With MC"),
                       transform(top_5_diff_withoutmc, Group = "Without MC"))


#Add STEM VS BHASE to fields of Study
combined_data_with_broad_field <- merge(combined_data, stemvsbhase,
                                        by.x = "Field of Study", 
                                        by.y = "Fields of study",all.x = TRUE)

combined_data_with_broad_field <- combined_data_with_broad_field %>%
  mutate(Study_Field_and_Broad = paste(`Field of Study`, "\n(", `Broad Field of Study`, ")", sep = ""))

# Graph with bars on the right and left
ggplot(combined_data_with_broad_field, aes(x = `Difference`, y = `Study_Field_and_Broad`, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("With MC" = "#eb0072", "Without MC" = "#6bbfae")) +
  labs(title = "Figure 1", 
       subtitle = "Comparison of % Differences in Fields of Study with and without MC", 
       x = "Difference", y = "Field of Study", fill = "Group",
       caption = "Source: Linkedin Insights Data" ) +
  dais.base.theme() +
  theme(panel.background=element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family = "Replica-Regular")) +
  geom_text(aes(label = percent(round(abs(`Difference`), 3))), 
            position = position_dodge(width = 0.9), vjust = 0.5, color = "black", family = "Replica-Regular")

ggsave("Figure_4.pdf", plot = last_plot(), width = 7.25, height = 7.25, units = "in")



# Field of Study treemap

treemap(data,
        index = c("Group", "Field of Study"),
        vSize = "NormalizedProfessionals",
        title = "Comparison of Fields of Study",
        palette = dais_palette,
        #fontfamily.title = "Rooneysans",
        #fontfamily.labels = "Rooneysans",
        #fontfamily.legend = "Rooneysans",
        fontsize.labels = 12)


######################################################################
##############################################################
###########################################################

##### Regressions #####








################################################
###################################################
#########################################################
##############################################################
######################################################################
############################################
######################################
################################
#########################
#######



################### SOFTWARE ####################

######## COMPARISONS #########


# COMMON SKILLS

# create a new datatable that combines the data from both datatables
data <- rbind(
  software_withoutmc_common_skills[, .(Group = "Without MC", `Common Skill`, Professionals)],
  `software_mc_Common Skills`[, .(Group = "With MC", `Common Skill` = Skills, Professionals)]
)


# normalize the Professionals variable within each group
data[, NormalizedProfessionals := ifelse(Group == "With MC", Professionals / sum(software_mc_Locations$Professionals), Professionals / sum(software_withoutmc_location$Professionals))]


# Calculate the differences in professionals between individuals with and without microcredentials for each common skill
data_diff <- data[, .(Difference = diff(NormalizedProfessionals)), by = .(`Common Skill`)]


# Calculate the absolute differences
data_diff[, AbsoluteDifference := abs(Difference)]

# Sort the data in descending order of the absolute difference
data_diff <- data_diff[order(-AbsoluteDifference)]

# Select the top 10 values of the absolute differences
top_10_diff <- head(data_diff, 10)

###

# Sort the data in descending order of the difference
data_diff <- data_diff[order(-Difference)]

# Select the top 10 values where MC is higher
top_10_diff_mc <- head(data_diff, 10)

###

# Sort the data in ascending order of the difference
data_diff <- data_diff[order(Difference)]

# Select the top 10 values where without MC is higher
top_10_diff_withoutmc <- head(data_diff, 10)

###

# Filter data for common skills in top_10_diff
filtered_data <- data[`Common Skill` %in% top_10_diff$`Common Skill`]

# Filter data for common skills in top_10_diff_mc
filtered_data_mc <- data[`Common Skill` %in% top_10_diff_mc$`Common Skill`]

# Filter data for common skills in top_10_diff_withoutmc
filtered_data_withoutmc <- data[`Common Skill` %in% top_10_diff_withoutmc$`Common Skill`]


# ggplot(filtered_data, aes(x = `Common Skill`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_mc, aes(x = `Common Skill`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_withoutmc, aes(x = `Common Skill`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))



#Top 5s
data_diff <- data_diff[order(Difference)]
top_5_diff_withoutmc <- head(data_diff, 5)
data_diff <- data_diff[order(-Difference)]
top_5_diff_mc <- head(data_diff, 5)

combined_data <- rbind(transform(top_5_diff_mc, Group = "With MC"),
                       transform(top_5_diff_withoutmc, Group = "Without MC"))


# Graph with bars on the right and left
ggplot(combined_data, aes(x = `Difference`, y = `Common Skill`, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("With MC" = "#eb0072", "Without MC" = "#6bbfae")) +
  labs(title = "Figure 1", 
       subtitle = "Comparison of % Differences in Skills with and without MC", 
       x = "Difference", y = "Common Skill", fill = "Group",
       caption = "Source: Linkedin Insights Data" ) +
  dais.base.theme() +
  theme(panel.background=element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family = "Replica-Regular")) +
  geom_text(aes(label = percent(round(abs(`Difference`), 3))), 
            position = position_dodge(width = 0.9), vjust = 0.5, color = "black", family = "Replica-Regular")

ggsave("Figure_5.pdf", plot = last_plot(), width = 7.25, height = 7.25, units = "in")




# Define some Dais colours 
dais_palette <- c("#DDA5C0", "#6bbfae" , "#ffa300",  "#002d72", "#eb0072" ,"#5bc2f4" )


# Common Skills treemap
treemap(data,
        index = c("Group", "Common Skill"),
        vSize = "NormalizedProfessionals",
        title = "Comparison of Common Skills",
        palette = dais_palette,
        #fontfamily.title = "Rooneysans",
        #fontfamily.labels = "Rooneysans",
        #fontfamily.legend = "Rooneysans",
        fontsize.labels = 12)

######################################################################

# Industry

# create a new datatable that combines the data from both datatables
data <- rbind(
  software_withoutmc_industries[, .(Group = "Without MC", Industry, Professionals)],
  software_mc_Industries[, .(Group = "With MC", Industry, Professionals)]
)

# normalize the Professionals variable within each group
data[, NormalizedProfessionals := ifelse(Group == "With MC", Professionals / sum(software_mc_Locations$Professionals), Professionals / sum(software_withoutmc_location$Professionals))]

# Calculate the differences in professionals between individuals with and without microcredentials for each industry
data_diff <- data[, .(Difference = diff(NormalizedProfessionals)), by = .(Industry)]

# Calculate the absolute differences
data_diff[, AbsoluteDifference := abs(Difference)]

# Sort the data in descending order of the absolute difference
data_diff <- data_diff[order(-AbsoluteDifference)]

# Select the top 10 values of the absolute differences
top_10_diff <- head(data_diff, 10)

###

# Sort the data in descending order of the difference
data_diff <- data_diff[order(-Difference)]

# Select the top 10 values where MC is higher
top_10_diff_mc <- head(data_diff, 10)

###

# Sort the data in ascending order of the difference
data_diff <- data_diff[order(Difference)]

# Select the top 10 values where without MC is higher
top_10_diff_withoutmc <- head(data_diff, 10)

###

# Filter data in top_10_diff
filtered_data <- data[Industry %in% top_10_diff$Industry]

# Filter data in top_10_diff_mc
filtered_data_mc <- data[Industry %in% top_10_diff_mc$Industry]

# Filter data in top_10_diff_withoutmc
filtered_data_withoutmc <- data[Industry %in% top_10_diff_withoutmc$Industry]


# ggplot(filtered_data, aes(x = Industry, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_mc, aes(x = Industry, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_withoutmc, aes(x = Industry, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))


#Top 5s
data_diff <- data_diff[order(Difference)]
top_5_diff_withoutmc <- head(data_diff, 5)
data_diff <- data_diff[order(-Difference)]
top_5_diff_mc <- head(data_diff, 5)

combined_data <- rbind(transform(top_5_diff_mc, Group = "With MC"),
                       transform(top_5_diff_withoutmc, Group = "Without MC"))


# Graph with bars on the right and left
ggplot(combined_data, aes(x = `Difference`, y = `Industry`, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("With MC" = "#eb0072", "Without MC" = "#6bbfae")) +
  labs(title = "Figure 1", 
       subtitle = "Comparison of % Differences in Industry with and without MC", 
       x = "Difference", y = "Industry", fill = "Group",
       caption = "Source: Linkedin Insights Data" ) +
  dais.base.theme() +
  theme(panel.background=element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family = "Replica-Regular")) +
  geom_text(aes(label = percent(round(abs(`Difference`), 3))), 
            position = position_dodge(width = 0.9), vjust = 0.5, color = "black", family = "Replica-Regular")

ggsave("Figure_6.pdf", plot = last_plot(), width = 7.25, height = 7.25, units = "in")




# Industries treemap

treemap(data,
        index = c("Group", "Industry"),
        vSize = "NormalizedProfessionals",
        title = "Comparison of Industries",
        palette = dais_palette,
        #fontfamily.title = "Rooneysans",
        #fontfamily.labels = "Rooneysans",
        #fontfamily.legend = "Rooneysans",
        fontsize.labels = 12)

######################################################################

# Common Titles

# create a new datatable that combines the data from both datatables
data <- rbind(
  software_withoutmc_common_titles[, .(Group = "Without MC", `Common Title`, Professionals)],
  `software_mc_Common Titles`[, .(Group = "With MC", `Common Title` = Titles, Professionals)]
)

# normalize the Professionals variable within each group
data[, NormalizedProfessionals := ifelse(Group == "With MC", Professionals / sum(software_mc_Locations$Professionals), Professionals / sum(software_withoutmc_location$Professionals))]

# Calculate the differences in professionals between individuals with and without microcredentials for each common title
data_diff <- data[, .(Difference = diff(NormalizedProfessionals)), by = .(`Common Title`)]

# Calculate the absolute differences
data_diff[, AbsoluteDifference := abs(Difference)]

# Sort the data in descending order of the absolute difference
data_diff <- data_diff[order(-AbsoluteDifference)]

# Select the top 10 values of the absolute differences
top_10_diff <- head(data_diff, 10)

###

# Sort the data in descending order of the difference
data_diff <- data_diff[order(-Difference)]

# Select the top 10 values where MC is higher
top_10_diff_mc <- head(data_diff, 10)

###

# Sort the data in ascending order of the difference
data_diff <- data_diff[order(Difference)]

# Select the top 10 values where without MC is higher
top_10_diff_withoutmc <- head(data_diff, 10)

###

# Filter data in top_10_diff
filtered_data <- data[`Common Title` %in% top_10_diff$`Common Title`]

# Filter data in top_10_diff_mc
filtered_data_mc <- data[`Common Title` %in% top_10_diff_mc$`Common Title`]

# Filter data in top_10_diff_withoutmc
filtered_data_withoutmc <- data[`Common Title` %in% top_10_diff_withoutmc$`Common Title`]

# 
# ggplot(filtered_data, aes(x = `Common Title`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_mc, aes(x = `Common Title`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_withoutmc, aes(x = `Common Title`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))



#Top 5s
data_diff <- data_diff[order(Difference)]
top_5_diff_withoutmc <- head(data_diff, 5)
data_diff <- data_diff[order(-Difference)]
top_5_diff_mc <- head(data_diff, 5)

combined_data <- rbind(transform(top_5_diff_mc, Group = "With MC"),
                       transform(top_5_diff_withoutmc, Group = "Without MC"))


# Graph with bars on the right and left
ggplot(combined_data, aes(x = `Difference`, y = `Common Title`, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("With MC" = "#eb0072", "Without MC" = "#6bbfae")) +
  labs(title = "Figure 1", 
       subtitle = "Comparison of % Differences in Common Titles with and without MC", 
       x = "Difference", y = "Common Title", fill = "Group",
       caption = "Source: Linkedin Insights Data" ) +
  dais.base.theme() +
  theme(panel.background=element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family = "Replica-Regular")) +
  geom_text(aes(label = percent(round(abs(`Difference`), 3))), 
            position = position_dodge(width = 0.9), vjust = 0.5, color = "black", family = "Replica-Regular")


ggsave("Figure_7.pdf", plot = last_plot(), width = 7.25, height = 7.25, units = "in")



# Common Titles treemap

treemap(data,
        index = c("Group", "Common Title"),
        vSize = "NormalizedProfessionals",
        title = "Comparison of Common Titles",
        palette = dais_palette,
        #fontfamily.title = "Rooneysans",
        #fontfamily.labels = "Rooneysans",
        #fontfamily.legend = "Rooneysans",
        fontsize.labels = 12)

######################################################################

# Fields of Study

# create a new datatable that combines the data from both datatables
data <- rbind(
  software_withoutmc_fields_of_study[, .(Group = "Without MC", `Field of Study`, Professionals)],
  `software_mc_Fields of Study`[, .(Group = "With MC", `Field of Study` = Fields.of.study, Professionals)]
)

# normalize the Professionals variable within each group
data[, NormalizedProfessionals := ifelse(Group == "With MC", Professionals / sum(software_mc_Locations$Professionals), Professionals / sum(software_withoutmc_location$Professionals))]

# Calculate the differences in professionals between individuals with and without microcredentials for each field of study
data_diff <- data[, .(Difference = diff(NormalizedProfessionals)), by = .(`Field of Study`)]

# Calculate the absolute differences
data_diff[, AbsoluteDifference := abs(Difference)]

# Sort the data in descending order of the absolute difference
data_diff <- data_diff[order(-AbsoluteDifference)]

# Select the top 10 values of the absolute differences
top_10_diff <- head(data_diff, 10)

###

# Sort the data in descending order of the difference
data_diff <- data_diff[order(-Difference)]

# Select the top 10 values where MC is higher
top_10_diff_mc <- head(data_diff, 10)

###

# Sort the data in ascending order of the difference
data_diff <- data_diff[order(Difference)]

# Select the top 10 values where without MC is higher
top_10_diff_withoutmc <- head(data_diff, 10)

###

# Filter data in top_10_diff
filtered_data <- data[`Field of Study` %in% top_10_diff$`Field of Study`]

# Filter data in top_10_diff_mc
filtered_data_mc <- data[`Field of Study` %in% top_10_diff_mc$`Field of Study`]

# Filter data in top_10_diff_withoutmc
filtered_data_withoutmc <- data[`Field of Study` %in% top_10_diff_withoutmc$`Field of Study`]


# ggplot(filtered_data, aes(x = `Field of Study`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
 ggplot(filtered_data_mc, aes(x = `Field of Study`, y = NormalizedProfessionals, fill = Group)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Field of Study", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
   dais.base.theme() +
   theme(panel.background=element_blank()) +
   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
  
 ggplot(filtered_data_withoutmc, aes(x = `Field of Study`, y = NormalizedProfessionals, fill = Group)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and without MC for top 10 skill differences") +
   dais.base.theme() +
   theme(panel.background=element_blank()) +
   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
   scale_fill_manual(values = c("#eb0072", "#6bbfae"))


#Top 5s
data_diff <- data_diff[order(Difference)]
top_5_diff_withoutmc <- head(data_diff, 5)
data_diff <- data_diff[order(-Difference)]
top_5_diff_mc <- head(data_diff, 5)

combined_data <- rbind(transform(top_5_diff_mc, Group = "With MC"),
                       transform(top_5_diff_withoutmc, Group = "Without MC"))


#Add STEM VS BHASE to fields of Study
combined_data_with_broad_field <- merge(combined_data, stemvsbhase,
                                        by.x = "Field of Study", 
                                        by.y = "Fields of study",all.x = TRUE)

combined_data_with_broad_field <- combined_data_with_broad_field %>%
  mutate(Study_Field_and_Broad = paste(`Field of Study`, "\n(", `Broad Field of Study`, ")", sep = ""))

# Graph with bars on the right and left
ggplot(combined_data_with_broad_field, aes(x = `Difference`, y = `Study_Field_and_Broad`, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("With MC" = "#eb0072", "Without MC" = "#6bbfae")) +
  labs(title = "Figure 1", 
       subtitle = "Comparison of % Differences in Fields of Study with and without MC", 
       x = "Difference", y = "Field of Study", fill = "Group",
       caption = "Source: Linkedin Insights Data" ) +
  dais.base.theme() +
  theme(panel.background=element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family = "Replica-Regular")) +
  geom_text(aes(label = percent(round(abs(`Difference`), 3))), 
            position = position_dodge(width = 0.9), vjust = 0.5, color = "black", family = "Replica-Regular")


ggsave("Figure_8.pdf", plot = last_plot(), width = 7.25, height = 7.25, units = "in")




# Field of Study treemap

treemap(data,
        index = c("Group", "Field of Study"),
        vSize = "NormalizedProfessionals",
        title = "Comparison of Fields of Study",
        palette = dais_palette,
        #fontfamily.title = "Rooneysans",
        #fontfamily.labels = "Rooneysans",
        #fontfamily.legend = "Rooneysans",
        fontsize.labels = 12)

