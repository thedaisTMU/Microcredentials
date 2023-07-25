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


# Read in data
# Create an empty list to store the data.tables
data_list <- list()

#################################
#IMPORT FIRST EXCEL FILE 
##################################

sheets <- excel_sheets("data-bootcamps-moocs-v2.xlsx")

# Loop through each sheet and read it into a data.table
for (sheet in sheets) {
  data <- as.data.table(read_excel("data-bootcamps-moocs-v2.xlsx", sheet = sheet))
  setnames(data, make.names(names(data)))
  data_list[[sheet]] <- data
}

# Unpack the data_list with modified names
for (i in seq_along(data_list)) {
  assign(paste0("SE_", names(data_list)[i]), data_list[[i]])
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

sheets <- excel_sheets("ds-se-mooc-bootcamp-only.xlsx")

# Loop through each sheet and read it into a data.table
for (sheet in sheets) {
  data <- as.data.table(read_excel("ds-se-mooc-bootcamp-only.xlsx", sheet = sheet))
  setnames(data, make.names(names(data)))
  data_list[[sheet]] <- data
}

# Unpack the data_list with modified names
for (i in seq_along(data_list)) {
  assign(paste0("se_", names(data_list)[i]), data_list[[i]])
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
merged_data <- merge(SE_Locations, se_Locations, by = "Location", suffixes = c("_SE", "_se"), SE.x = TRUE)
merged_data[is.na(Professionals_se), Professionals_se := 0]

withoutse_location <- data.table(Location = merged_data$Location,
                                                 Professionals = merged_data$Professionals_SE - merged_data$Professionals_se)


# Companies
merged_data <- merge(SE_Companies, se_Companies, by = "Company", suffixes = c("_SE", "_se"), SE.x = TRUE)
merged_data[is.na(Professionals_se), Professionals_se := 0]

withoutse_company <- data.table(Company = merged_data$Company,
                                                Professionals = merged_data$Professionals_SE - merged_data$Professionals_se)


# Common Titles
merged_data <- merge(`SE_Common Titles`, `se_Common Titles`, by = "Titles", suffixes = c("_SE", "_se"), SE.x = TRUE)
merged_data[is.na(Professionals_se), Professionals_se := 0]

withoutse_common_titles <- data.table(`Common Title` = merged_data$`Titles`,
                                                      Professionals = merged_data$Professionals_SE - merged_data$Professionals_se)


# Common Skills
merged_data <- merge(`SE_Common Skills`, `se_Common Skills`, by = "Skills", suffixes = c("_SE", "_se"), SE.x = TRUE)
merged_data[is.na(Professionals_se), Professionals_se := 0]

withoutse_common_skills <- data.table(`Common Skill` = merged_data$`Skills`,
                                                      Professionals = merged_data$Professionals_SE - merged_data$Professionals_se)



# Industries
merged_data <- merge(SE_Industries, se_Industries, by = "Industry", suffixes = c("_SE", "_se"), SE.x = TRUE)
merged_data[is.na(Professionals_se), Professionals_se := 0]

withoutse_industries <- data.table(Industry = merged_data$Industry,
                                                   Professionals = merged_data$Professionals_SE - merged_data$Professionals_se)


# Schools
merged_data <- merge(SE_Schools, se_Schools, by = "Schools", suffixes = c("_SE", "_se"), SE.x = TRUE)
merged_data[is.na(Professionals_se), Professionals_se := 0]

withoutse_schools <- data.table(School = merged_data$School,
                                                Professionals = merged_data$Professionals_SE - merged_data$Professionals_se)


# Fields of Study
merged_data <- merge(`SE_Fields of Study`, `se_Fields of Study`, by = "Fields.of.study", suffixes = c("_SE", "_se"), SE.x = TRUE)
merged_data[is.na(Professionals_se), Professionals_se := 0]

withoutse_fields_of_study <- data.table(`Field of Study` = merged_data$`Fields.of.study`,
                                                        Professionals = merged_data$Professionals_SE - merged_data$Professionals_se)





################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

######## COMPARISONS #########


# COMMON SKILLS

# create a new datatable that combines the data from both datatables
data <- rbind(
  withoutse_common_skills[, .(Group = "Without SE", `Common Skill`, Professionals)],
  `se_Common Skills`[, .(Group = "SE", `Common Skill` = Skills, Professionals)]
)


# normalize the Professionals variable within each group
data[, NormalizedProfessionals := ifelse(Group == "SE", Professionals / sum(se_Locations$Professionals), Professionals / sum(withoutse_location$Professionals))]


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
top_10_diff_se <- head(data_diff, 10)

###

# Sort the data in ascending order of the difference
data_diff <- data_diff[order(Difference)]

# Select the top 10 values where Without SE is higher
top_10_diff_withoutse <- head(data_diff, 10)

###

# Filter data for common skills in top_10_diff
filtered_data <- data[`Common Skill` %in% top_10_diff$`Common Skill`]

# Filter data for common skills in top_10_diff_se
filtered_data_se <- data[`Common Skill` %in% top_10_diff_se$`Common Skill`]

# Filter data for common skills in top_10_diff_withoutse
filtered_data_withoutse <- data[`Common Skill` %in% top_10_diff_withoutse$`Common Skill`]


# ggplot(filtered_data, aes(x = `Common Skill`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and Without SE for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_se, aes(x = `Common Skill`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and Without SE for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_withoutse, aes(x = `Common Skill`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and Without SE for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))



#Top 5s
data_diff <- data_diff[order(Difference)]
top_5_diff_withoutse <- head(data_diff, 5)
data_diff <- data_diff[order(-Difference)]
top_5_diff_se <- head(data_diff, 5)

combined_data <- rbind(transform(top_5_diff_se, Group = "SE"),
                       transform(top_5_diff_withoutse, Group = "Without SE"))


# Graph with bars on the right and left
ggplot(combined_data, aes(x = `Difference`, y = `Common Skill`, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("SE" = "#eb0072", "Without SE" = "#6bbfae")) +
  labs(x = "Difference", y = "Common Skill", fill = "Group") +
  dais.base.theme() +
  theme(panel.background=element_blank()) +
  ggtitle("Comparison of % Differences in Skills with and Without SE") +
  geom_text(aes(label = percent(round(abs(`Difference`), 3))), 
            position = position_dodge(width = 0.9), vjust = 0.5, color = "black")




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
  withoutse_industries[, .(Group = "Without SE", Industry, Professionals)],
  se_Industries[, .(Group = "SE", Industry, Professionals)]
)

# normalize the Professionals variable within each group
data[, NormalizedProfessionals := ifelse(Group == "SE", Professionals / sum(se_Locations$Professionals), Professionals / sum(withoutse_location$Professionals))]

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
top_10_diff_se <- head(data_diff, 10)

###

# Sort the data in ascending order of the difference
data_diff <- data_diff[order(Difference)]

# Select the top 10 values where Without SE is higher
top_10_diff_withoutse <- head(data_diff, 10)

###

# Filter data in top_10_diff
filtered_data <- data[Industry %in% top_10_diff$Industry]

# Filter data in top_10_diff_se
filtered_data_se <- data[Industry %in% top_10_diff_se$Industry]

# Filter data in top_10_diff_withoutse
filtered_data_withoutse <- data[Industry %in% top_10_diff_withoutse$Industry]


# ggplot(filtered_data, aes(x = Industry, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and Without SE for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_se, aes(x = Industry, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and Without SE for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_withoutse, aes(x = Industry, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and Without SE for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))


#Top 5s
data_diff <- data_diff[order(Difference)]
top_5_diff_withoutse <- head(data_diff, 5)
data_diff <- data_diff[order(-Difference)]
top_5_diff_se <- head(data_diff, 5)

combined_data <- rbind(transform(top_5_diff_se, Group = "SE"),
                       transform(top_5_diff_withoutse, Group = "Without SE"))


# Graph with bars on the right and left
ggplot(combined_data, aes(x = `Difference`, y = `Industry`, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("SE" = "#eb0072", "Without SE" = "#6bbfae")) +
  labs(x = "Difference", y = "Industry", fill = "Group") +
  dais.base.theme() +
  theme(panel.background=element_blank()) +
  ggtitle("Comparison of % Differences in Industries with and Without SE") +
  geom_text(aes(label = percent(round(abs(`Difference`), 3))), 
            position = position_dodge(width = 0.9), vjust = 0.5, color = "black")




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
  withoutse_common_titles[, .(Group = "Without SE", `Common Title`, Professionals)],
  `se_Common Titles`[, .(Group = "SE", `Common Title` = Titles, Professionals)]
)

# normalize the Professionals variable within each group
data[, NormalizedProfessionals := ifelse(Group == "SE", Professionals / sum(se_Locations$Professionals), Professionals / sum(withoutse_location$Professionals))]

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
top_10_diff_se <- head(data_diff, 10)

###

# Sort the data in ascending order of the difference
data_diff <- data_diff[order(Difference)]

# Select the top 10 values where Without SE is higher
top_10_diff_withoutse <- head(data_diff, 10)

###

# Filter data in top_10_diff
filtered_data <- data[`Common Title` %in% top_10_diff$`Common Title`]

# Filter data in top_10_diff_se
filtered_data_se <- data[`Common Title` %in% top_10_diff_se$`Common Title`]

# Filter data in top_10_diff_withoutse
filtered_data_withoutse <- data[`Common Title` %in% top_10_diff_withoutse$`Common Title`]

# 
# ggplot(filtered_data, aes(x = `Common Title`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and Without SE for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_se, aes(x = `Common Title`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and Without SE for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
# ggplot(filtered_data_withoutse, aes(x = `Common Title`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and Without SE for top 10 skill differences") +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))



#Top 5s
data_diff <- data_diff[order(Difference)]
top_5_diff_withoutse <- head(data_diff, 5)
data_diff <- data_diff[order(-Difference)]
top_5_diff_se <- head(data_diff, 5)

combined_data <- rbind(transform(top_5_diff_se, Group = "SE"),
                       transform(top_5_diff_withoutse, Group = "Without SE"))


# Graph with bars on the right and left
ggplot(combined_data, aes(x = `Difference`, y = `Common Title`, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("SE" = "#eb0072", "Without SE" = "#6bbfae")) +
  labs(x = "Difference", y = "Common Title", fill = "Group") +
  dais.base.theme() +
  theme(panel.background=element_blank()) +
  ggtitle("Comparison of % Differences in Common Titles with and Without SE") +
  geom_text(aes(label = percent(round(abs(`Difference`), 3))), 
            position = position_dodge(width = 0.9), vjust = 0.5, color = "black")




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
  withoutse_fields_of_study[, .(Group = "Without SE", `Field of Study`, Professionals)],
  `se_Fields of Study`[, .(Group = "SE", `Field of Study` = Fields.of.study, Professionals)]
)

# normalize the Professionals variable within each group
data[, NormalizedProfessionals := ifelse(Group == "SE", Professionals / sum(se_Locations$Professionals), Professionals / sum(withoutse_location$Professionals))]

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
top_10_diff_se <- head(data_diff, 10)

###

# Sort the data in ascending order of the difference
data_diff <- data_diff[order(Difference)]

# Select the top 10 values where Without SE is higher
top_10_diff_withoutse <- head(data_diff, 10)

###

# Filter data in top_10_diff
filtered_data <- data[`Field of Study` %in% top_10_diff$`Field of Study`]

# Filter data in top_10_diff_se
filtered_data_se <- data[`Field of Study` %in% top_10_diff_se$`Field of Study`]

# Filter data in top_10_diff_withoutse
filtered_data_withoutse <- data[`Field of Study` %in% top_10_diff_withoutse$`Field of Study`]


# ggplot(filtered_data, aes(x = `Field of Study`, y = NormalizedProfessionals, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and Without SE for top 10 skill differences") +
#   dais.base.theme() +
#   theme(panel.background=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
#   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
# 
 ggplot(filtered_data_se, aes(x = `Field of Study`, y = NormalizedProfessionals, fill = Group)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Field of Study", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and Without SE for top 10 skill differences") +
   dais.base.theme() +
   theme(panel.background=element_blank()) +
   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
   scale_fill_manual(values = c("#eb0072", "#6bbfae"))
  
 ggplot(filtered_data_withoutse, aes(x = `Field of Study`, y = NormalizedProfessionals, fill = Group)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Common Skill", y = "Normalized Number of Professionals", title = "Comparison of Professionals with and Without SE for top 10 skill differences") +
   dais.base.theme() +
   theme(panel.background=element_blank()) +
   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
   scale_fill_manual(values = c("#eb0072", "#6bbfae"))


#Top 5s
data_diff <- data_diff[order(Difference)]
top_5_diff_withoutse <- head(data_diff, 5)
data_diff <- data_diff[order(-Difference)]
top_5_diff_se <- head(data_diff, 5)

combined_data <- rbind(transform(top_5_diff_se, Group = "SE"),
                       transform(top_5_diff_withoutse, Group = "Without SE"))


# Graph with bars on the right and left
ggplot(combined_data, aes(x = `Difference`, y = `Field of Study`, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("SE" = "#eb0072", "Without SE" = "#6bbfae")) +
  labs(x = "Difference", y = "Field of Study", fill = "Group") +
  dais.base.theme() +
  theme(panel.background=element_blank()) +
  ggtitle("Comparison of % Differences in Fields of Study with and Without SE") +
  geom_text(aes(label = percent(round(abs(`Difference`), 3))), 
            position = position_dodge(width = 0.9), vjust = 0.5, color = "black")




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

