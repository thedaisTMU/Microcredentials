
# clear data and close graphs
rm(list=ls())
graphics.off()

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# set working directory
setwd("/Users/ibrahim/Desktop/Project/Results")


#install.packages("devtools")
#library(devtools)
#install_github("BrookfieldIIE/BFTheme",build_vignettes=FALSE)
#ggmap
#install.packages("showtext")
#install.packages("ggtext")

#install.packages('extrafont')
#library(extrafont)

#font_import(paths = "/Users/ibrahim/")

library(ggplot2)
library(data.table)
library(stringr)
library(psych)
library(knitr)
library(tidyverse)
#library(extrafont)
library(BFTheme)
library(readxl)
library(dplyr)
library(tidyr)
library(flextable)

# Read data
results_2006 <- as.data.table(read.csv("tech.sector.def.2006.csv"))
results_2016 <- as.data.table(read.csv("tech.sector.def.2016.csv"))
results_2021 <- as.data.table(read.csv("tech.sector.def.2021.csv"))


crosswalk <- as.data.table(read.csv("noc_2016_2006.csv"))
full_crosswalk <- as.data.table(read.csv("complete crosswalks.csv"))

setkey(full_crosswalk,noc.2016)
setkey(results_2006,noc_2006)
setkey(results_2016,noc_2016)
setkey(results_2021,noc_2021)


#Keep only noc codes and harmonic ranks
short06 <- results_2006[,c("noc_2006","harm.rank")]
names(short06) <- c("noc_2006","harm.rank06")
short16 <- results_2016[,c("noc_2016","harm.rank")]
names(short16) <- c("noc_2016","harm.rank16")
short21 <- results_2021[,c("noc_2021","harm.rank")]
names(short21) <- c("noc_2021","harm.rank21")


short_21 <- unique(merge.data.table(short21,full_crosswalk[,c("noc.2016","noc_2021")],by.x="noc_2021",by.y="noc_2021",all.x=TRUE))

full_results16 <- merge.data.table(short16,short06,by.x="noc_2016",by.y="noc_2006",all.x=TRUE)

full_results <- merge.data.table(full_results16,short_21[,c("noc.2016","harm.rank21")],by.x="noc_2016",by.y="noc.2016",all.x=TRUE)

dup_full = full_results[which(full_results$noc_2016%in%unique(full_results[which(duplicated(full_results$noc_2016)),]$noc_2016)),]

avg.full = aggregate(dup_full$harm.rank21, by = list(dup_full$noc_2016), FUN = "mean")
names(avg.full) <- c("noc_2016","harm.rank21")

avg.full = unique(merge(avg.full,full_results[,-"harm.rank21"],by.x="noc_2016",by.y="noc_2016",all.x=TRUE))
avg.full = avg.full[,c("noc_2016","harm.rank16","harm.rank06","harm.rank21")]

full_results_unique = full_results[-which(full_results$noc_2016%in%dup_full$noc_2016),]

final = rbind(full_results_unique,avg.full)

final = final[which(!is.na(final$harm.rank06)),]
final = final[which(!is.na(final$harm.rank21)),]

final[,diff_21_06:=harm.rank21-harm.rank06]

final[,diff_21_16:=harm.rank21-harm.rank16]

skill_groups = as.numeric(substr(final$noc_2016, 2, 2))

final = cbind(final,skill_groups)

#final[,.(mean(harm.rank21,na.rm=TRUE),mean(harm.rank16,na.rm=TRUE),mean(harm.rank06,na.rm=TRUE)),by=noc_16]


#Clean up the environment
rm(skill_groups)
rm(avg.full)
rm(crosswalk)
rm(dup_full)
rm(full_crosswalk)
rm(full_results)
rm(full_results_unique)
rm(full_results16)
rm(results_2006)
rm(results_2016)
rm(results_2021)
rm(short_21)
rm(short06)
rm(short16)
rm(short21)


#final06 = final[order(harm.rank06)]
#final16 = final[order(harm.rank16)]
#final21 = final[order(harm.rank21)]


setkey(final,harm.rank06)
final[,rank06:=.I]
setkey(final,harm.rank16)
final[,rank16:=.I]
setkey(final,harm.rank21)
final[,rank21:=.I]

#skills1 = final[skill_groups %in% c(0,1)]
#skills2 = final[skill_groups %in% c(2,3)]
#skills3 = final[skill_groups %in% c(4,5)]
#skills4 = final[skill_groups %in% c(6,7)]

final[skill_groups %in% c(0,1),skill_groups_2:="0-1"]
final[skill_groups %in% c(2,3),skill_groups_2:="2-3"]
final[skill_groups %in% c(4,5),skill_groups_2:="4-5"]
final[skill_groups %in% c(6,7),skill_groups_2:="6-7"]


crosswalk <- as.data.table(read.csv("Complete Crosswalks.csv"))
noc_title <- unique(crosswalk[,c("noc.2016", "noc.2016.title")])
names(noc_title) <- c("noc_2016","noc_title")
final = merge(noc_title,final,by="noc_2016")
rm(noc_title)
rm(crosswalk)


#setkey(final,skill_groups_2,harm.rank06)

#final[,rank.06:=.I,by=skill_groups_2]
#final[,rank.06:=rank.06-min(rank.06)+1,by=skill_groups_2]




#cols = which(final06$noc_2016 %in% skills1$noc_2016)


#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#LET'S PLAY

# I - Standardize the ranks

final = final[,standardized06:=((harm.rank06-mean(harm.rank06))/sd(harm.rank06))]
final = final[,standardized16:=((harm.rank06-mean(harm.rank16))/sd(harm.rank16))]
final = final[,standardized21:=((harm.rank06-mean(harm.rank21))/sd(harm.rank21))]

final[,std_diff_21_06:=(standardized21-standardized06)]

setkey(final,std_diff_21_06)
setkey(final,skill_groups_2)
top_1_std = final[1:10,c("noc_title","rank06","rank16","rank21")]



# II - Normalizing with min-max feature scaling
final = final[,normalized06:=((harm.rank06-min(harm.rank06))/(max(harm.rank06)-min(harm.rank06)))]
final = final[,normalized16:=((harm.rank06-min(harm.rank16))/(max(harm.rank16)-min(harm.rank16)))]
final = final[,normalized21:=((harm.rank06-min(harm.rank21))/(max(harm.rank21)-min(harm.rank21)))]

final[,norm_diff_21_06:=normalized21-normalized06]

setkey(final,std_diff_21_06)
setkey(final,skill_groups_2)
top_1_norm = final[1:10,c("noc_title","rank06","rank16","rank21")]




# III - Look at Each of the 6 Rank Categories

results_2006 <- as.data.table(read.csv("tech.sector.def.2006.csv"))
ranks06 <- results_2006[,c("noc_2006", "Rank...Tech.Design","Rank...Programming", "Rank...Comp.and.Elec","Rank...Inter.w.Comp","Rank...Eng.and.Tech","Rank...Telco")]
names(ranks06) = c("noc_2006", "Rank06_Tech_Design","Rank06_Programming", "Rank06_Comp_and_Elec","Rank06_Inter_w_Comp","Rank06_Eng_and_Tech","Rank06_Telco")
rm(results_2006)

results_2016 <- as.data.table(read.csv("tech.sector.def.2016.csv"))
ranks16 <- results_2016[,c("noc_2016", "Rank...Tech.Design","Rank...Programming", "Rank...Comp.and.Elec","Rank...Inter.w.Comp","Rank...Eng.and.Tech","Rank...Telco")]
names(ranks16) = c("noc_2016", "Rank16_Tech_Design","Rank16_Programming", "Rank16_Comp_and_Elec","Rank16_Inter_w_Comp","Rank16_Eng_and_Tech","Rank16_Telco")
rm(results_2016)

results_2021 <- as.data.table(read.csv("tech.sector.def.2021.csv"))
ranks21 <- results_2021[,c("noc_2021", "Rank...Tech.Design","Rank...Programming", "Rank...Comp.and.Elec","Rank...Inter.w.Comp","Rank...Eng.and.Tech","Rank...Telco")]
names(ranks21) = c("noc_2021", "Rank21_Tech_Design","Rank21_Programming", "Rank21_Comp_and_Elec","Rank21_Inter_w_Comp","Rank21_Eng_and_Tech","Rank21_Telco")
rm(results_2021)

setkey(ranks06,noc_2006)
setkey(ranks16,noc_2016)
setkey(ranks21,noc_2021)


crosswalk <- as.data.table(read.csv("Complete Crosswalks.csv"))

hola = merge(crosswalk,ranks21,by.x="noc_2021",by.y="noc_2021",all.x=TRUE)
hola = hola[,c("noc.2016","Rank21_Tech_Design","Rank21_Programming", "Rank21_Comp_and_Elec","Rank21_Inter_w_Comp","Rank21_Eng_and_Tech","Rank21_Telco")]

#hola2 = hola[,aggregate(hola, list("Rank21_Tech_Design","Rank21_Programming","Rank21_Comp_and_Elec","Rank21_Inter_w_Comp","Rank21_Eng_and_Tech","Rank21_Telco"),mean)
hola3 = hola[,.(mean(Rank21_Tech_Design,na.rm=TRUE),mean(Rank21_Programming,na.rm=TRUE),mean(Rank21_Comp_and_Elec,na.rm=TRUE),mean(Rank21_Inter_w_Comp,na.rm=TRUE),mean(Rank21_Eng_and_Tech,na.rm=TRUE),mean(Rank21_Telco,na.rm=TRUE)),by=noc.2016]

hola3 = na.omit(hola3)
names(hola3) = c("noc_2021", "Rank21_Tech_Design","Rank21_Programming", "Rank21_Comp_and_Elec","Rank21_Inter_w_Comp","Rank21_Eng_and_Tech","Rank21_Telco")
rm(hola)

#hola2 = rowMeans(hola)
#hola = hola[which(!is.na(V1)),]


final = merge(final,hola3,by.x="noc_2016",by.y="noc_2021")
final = merge(final,ranks16,by.x="noc_2016",by.y="noc_2016")
final = merge(final,ranks06,by.x="noc_2016",by.y="noc_2006",all.x=TRUE)

rm(crosswalk)
rm(hola3)
rm(ranks21)
rm(ranks06)
rm(ranks16)




# IV - Look at Each of the 6 Score Categories

results_2006 <- as.data.table(read.csv("tech.sector.def.2006.csv"))
scores06 <- results_2006[,c("noc_2006", "X2.C.3.a...Comp.and.Elec","X2.C.3.b...Eng.and.Tech","X2.C.9.a...Telco", "X2.B.3.b...Tech.Design","X2.B.3.e...Programming","X4.A.3.b.1...Inter.w.Comp")]
names(scores06) = c("noc_2006", "Score06_comp_elec","Score06_eng_tech", "Score06_telco","Score06_tech_design","Score06_programming","Score06_inter_comp")
rm(results_2006)


results_2016 <- as.data.table(read.csv("tech.sector.def.2016.csv"))
scores16 <- results_2016[,c("noc_2016", "X2.C.3.a...Comp.and.Elec","X2.C.3.b...Eng.and.Tech","X2.C.9.a...Telco", "X2.B.3.b...Tech.Design","X2.B.3.e...Programming","X4.A.3.b.1...Inter.w.Comp")]
names(scores16) = c("noc_2016", "Score16_comp_elec","Score16_eng_tech", "Score16_telco","Score16_tech_design","Score16_programming","Score16_inter_comp")
rm(results_2016)


results_2021 <- as.data.table(read.csv("tech.sector.def.2021.csv"))
scores21 <- results_2021[,c("noc_2021", "X2.C.3.a...Comp.and.Elec","X2.C.3.b...Eng.and.Tech","X2.C.9.a...Telco", "X2.B.3.b...Tech.Design","X2.B.3.e...Programming","X4.A.3.b.1...Inter.w.Comp")]
names(scores21) = c("noc_2021", "Score21_comp_elec","Score21_eng_tech", "Score21_telco","Score21_tech_design","Score21_programming","Score21_inter_comp")
rm(results_2021)


setkey(scores06,noc_2006)
setkey(scores16,noc_2016)
setkey(scores21,noc_2021)


crosswalk <- as.data.table(read.csv("Complete Crosswalks.csv"))

hola = merge(crosswalk,scores21,by.x="noc_2021",by.y="noc_2021",all.x=TRUE)
hola = hola[,c("noc.2016", "Score21_comp_elec","Score21_eng_tech", "Score21_telco","Score21_tech_design","Score21_programming","Score21_inter_comp")]

hola3 = hola[,.(mean(Score21_comp_elec,na.rm=TRUE),mean(Score21_eng_tech,na.rm=TRUE),mean(Score21_telco,na.rm=TRUE),mean(Score21_tech_design,na.rm=TRUE),mean(Score21_programming,na.rm=TRUE),mean(Score21_inter_comp,na.rm=TRUE)),by=noc.2016]

hola3 = na.omit(hola3)
names(hola3) = c("noc_2021", "Score21_comp_elec","Score21_eng_tech", "Score21_telco","Score21_tech_design","Score21_programming","Score21_inter_comp")
rm(hola)

#hola2 = rowMeans(hola)
#hola = hola[which(!is.na(V1)),]


final = merge(final,hola3,by.x="noc_2016",by.y="noc_2021")
final = merge(final,scores16,by.x="noc_2016",by.y="noc_2016")
final = merge(final,scores06,by.x="noc_2016",by.y="noc_2006",all.x=TRUE)




tech_design = final[,c("noc_2016","Score06_tech_design","Score16_tech_design", "Score21_tech_design")]
programming = final[,c("noc_2016","Score06_programming","Score16_programming", "Score21_programming")]
comp_elec = final[,c("noc_2016","Score06_comp_elec","Score16_comp_elec", "Score21_comp_elec")]
inter_comp = final[,c("noc_2016","Score06_inter_comp","Score16_inter_comp", "Score21_inter_comp")]
eng_tech = final[,c("noc_2016","Score06_eng_tech","Score16_eng_tech", "Score21_eng_tech")]
telco = final[,c("noc_2016","Score06_telco","Score16_telco", "Score21_telco")]


melted_tech_design <- melt(tech_design, id = "noc_2016")
melted_tech_design_sd = melted_tech_design[,sd(value,na.rm=TRUE),by=noc_2016]
setorder(melted_tech_design,-value)
setorder(melted_tech_design_sd,-V1)


melted_programming <- melt(programming, id = "noc_2016")
melted_programming_sd = melted_programming[,sd(value,na.rm=TRUE),by=noc_2016]
setorder(melted_programming,-value)
setorder(melted_programming_sd,-V1)


melted_comp_elec <- melt(comp_elec, id = "noc_2016")
melted_comp_elec_sd = melted_comp_elec[,sd(value,na.rm=TRUE),by=noc_2016]
setorder(melted_comp_elec,-value)
setorder(melted_comp_elec_sd,-V1)


melted_inter_comp <- melt(inter_comp, id = "noc_2016")
melted_inter_comp_sd = melted_inter_comp[,sd(value,na.rm=TRUE),by=noc_2016]
setorder(melted_inter_comp,-value)
setorder(melted_inter_comp_sd,-V1)


melted_eng_tech <- melt(eng_tech, id = "noc_2016")
melted_eng_tech_sd = melted_eng_tech[,sd(value,na.rm=TRUE),by=noc_2016]
setorder(melted_eng_tech,-value)
setorder(melted_eng_tech_sd,-V1)


melted_telco <- melt(telco, id = "noc_2016")
melted_telco_sd = melted_telco[,sd(value,na.rm=TRUE),by=noc_2016]
setorder(melted_telco,-value)
setorder(melted_telco_sd,-V1)





#V - Normalized Scores

final = final[,normalized_tech_design06:=((Score06_tech_design-min(Score06_tech_design))/(max(Score06_tech_design)-min(Score06_tech_design)))]
final = final[,normalized_tech_design16:=((Score06_tech_design-min(Score16_tech_design))/(max(Score16_tech_design)-min(Score16_tech_design)))]
final = final[,normalized_tech_design21:=((Score21_tech_design-min(Score21_tech_design))/(max(Score21_tech_design)-min(Score21_tech_design)))]
avg_norm_tech_design06 = mean(final$normalized_tech_design06)
avg_norm_tech_design16 = mean(final$normalized_tech_design16)
avg_norm_tech_design21 = mean(final$normalized_tech_design21)

final = final[,normalized_programming06:=((Score06_programming-min(Score06_programming))/(max(Score06_programming)-min(Score06_programming)))]
final = final[,normalized_programming16:=((Score16_programming-min(Score16_programming))/(max(Score16_programming)-min(Score16_programming)))]
final = final[,normalized_programming21:=((Score21_programming-min(Score21_programming))/(max(Score21_programming)-min(Score21_programming)))]
avg_norm_programming06 = mean(final$normalized_programming06)
avg_norm_programming16 = mean(final$normalized_programming16)
avg_norm_programming21 = mean(final$normalized_programming21)


final = final[,normalized_comp_elec06:=((Score06_comp_elec-min(Score06_comp_elec))/(max(Score06_comp_elec)-min(Score06_comp_elec)))]
final = final[,normalized_comp_elec16:=((Score16_comp_elec-min(Score16_comp_elec))/(max(Score16_comp_elec)-min(Score16_comp_elec)))]
final = final[,normalized_comp_elec21:=((Score21_comp_elec-min(Score21_comp_elec))/(max(Score21_comp_elec)-min(Score21_comp_elec)))]
avg_norm_comp_elec06 = mean(final$normalized_comp_elec06)
avg_norm_comp_elec16 = mean(final$normalized_comp_elec16)
avg_norm_comp_elec21 = mean(final$normalized_comp_elec21)


final = final[,normalized_inter_comp06:=((Score06_inter_comp-min(Score06_inter_comp))/(max(Score06_inter_comp)-min(Score06_inter_comp)))]
final = final[,normalized_inter_comp16:=((Score16_inter_comp-min(Score16_inter_comp))/(max(Score16_inter_comp)-min(Score16_inter_comp)))]
final = final[,normalized_inter_comp21:=((Score21_inter_comp-min(Score21_inter_comp))/(max(Score21_inter_comp)-min(Score21_inter_comp)))]
avg_norm_inter_comp06 = mean(final$normalized_inter_comp06)
avg_norm_inter_comp16 = mean(final$normalized_inter_comp16)
avg_norm_inter_comp21 = mean(final$normalized_inter_comp21)


final = final[,normalized_eng_tech06:=((Score06_eng_tech-min(Score06_eng_tech))/(max(Score06_eng_tech)-min(Score06_eng_tech)))]
final = final[,normalized_eng_tech16:=((Score16_eng_tech-min(Score16_eng_tech))/(max(Score16_eng_tech)-min(Score16_eng_tech)))]
final = final[,normalized_eng_tech21:=((Score21_eng_tech-min(Score21_eng_tech))/(max(Score21_eng_tech)-min(Score21_eng_tech)))]
avg_norm_eng_tech06 = mean(final$normalized_eng_tech06)
avg_norm_eng_tech16 = mean(final$normalized_eng_tech16)
avg_norm_eng_tech21 = mean(final$normalized_eng_tech21)


final = final[,normalized_telco06:=((Score06_telco-min(Score06_telco))/(max(Score06_telco)-min(Score06_telco)))]
final = final[,normalized_telco16:=((Score16_telco-min(Score16_telco))/(max(Score16_telco)-min(Score16_telco)))]
final = final[,normalized_telco21:=((Score21_telco-min(Score21_telco))/(max(Score21_telco)-min(Score21_telco)))]
avg_norm_telco06 = mean(final$normalized_telco06)
avg_norm_telco16 = mean(final$normalized_telco16)
avg_norm_telco21 = mean(final$normalized_telco21)


average_norm_scores = rbind(avg_norm_tech_design06,avg_norm_tech_design16,avg_norm_tech_design21,avg_norm_programming06,avg_norm_programming16,avg_norm_programming21,avg_norm_comp_elec06,avg_norm_comp_elec16,avg_norm_comp_elec21,avg_norm_inter_comp06,avg_norm_inter_comp16,avg_norm_inter_comp21,avg_norm_eng_tech06,avg_norm_eng_tech16,avg_norm_eng_tech21,avg_norm_telco06,avg_norm_telco16,avg_norm_telco21)

average_norm_scores = data.table(categories=row.names(average_norm_scores),average_scores=average_norm_scores)

years = data.table(c("2006","2016","2021","2006","2016","2021","2006","2016","2021","2006","2016","2021","2006","2016","2021","2006","2016","2021"))
names(years) = "years"

categories_2 = data.table(c("avg_norm_tech_design","avg_norm_tech_design","avg_norm_tech_design","avg_norm_programming","avg_norm_programming","avg_norm_programming","avg_norm_comp_elec","avg_norm_comp_elec","avg_norm_comp_elec","avg_norm_inter_comp","avg_norm_inter_comp","avg_norm_inter_comp","avg_norm_eng_tech","avg_norm_eng_tech","avg_norm_eng_tech","avg_norm_telco","avg_norm_telco","avg_norm_telco"))

average_norm_scores = average_norm_scores[,years:=years]
average_norm_scores = average_norm_scores[,categories:=categories_2]


# Let's look at the top movers between 2021 and 2006, and see how their normlized scores changed

final = final[,normalized_tech_design_diff:=normalized_tech_design21-normalized_tech_design06]
final = final[,normalized_programming_diff:=normalized_programming21-normalized_programming06]
final = final[,normalized_comp_elec_diff:=normalized_comp_elec21-normalized_comp_elec06]
final = final[,normalized_inter_comp_diff:=normalized_inter_comp21-normalized_inter_comp06]
final = final[,normalized_eng_tech_diff:=normalized_eng_tech21-normalized_eng_tech06]
final = final[,normalized_telco_diff:=normalized_telco21-normalized_telco06]

setkey(final,diff_21_16)
changes_norm21_16 = final[1:10,c("noc_2016","noc_title","normalized_tech_design_diff","normalized_programming_diff","normalized_comp_elec_diff","normalized_inter_comp_diff","normalized_eng_tech_diff","normalized_telco_diff")]


# VI - Adding Work Context

work_context <- as.data.table(read_excel("work Context.xlsx"))
work_context = work_context[,c("Title","Element Name","Category","Data Value")]
names(work_context) = c("onet_title", "element_name","category", "data_value")
work_context = work_context[,product:=(category*data_value)]
work_context = work_context[,mean(product,na.rm=TRUE),by=.(element_name,onet_title)]

work_context_06 <- as.data.table(read_excel("work_context_06.xlsx"))
work_context_06 = work_context_06[,c("Title","Element Name","Category","Data Value")]
names(work_context_06) = c("onet_title", "element_name","category", "data_value")
work_context_06 = work_context_06[,product:=(category*data_value)]
work_context_06 = work_context_06[,mean(product,na.rm=TRUE),by=.(element_name,onet_title)]

work_context_21 <- as.data.table(read_excel("work_context_21.xlsx"))
work_context_21 = work_context_21[,c("Title","Element Name","Category","Data Value")]
names(work_context_21) = c("onet_title", "element_name","category", "data_value")
work_context_21 = work_context_21[,product:=(category*data_value)]
work_context_21 = work_context_21[,mean(product,na.rm=TRUE),by=.(element_name,onet_title)]



# VII - Adding Work Activities

work_activities <- as.data.table(read_excel("work Activities.xlsx"))
work_activities = work_activities[,c("Title","Element Name","Scale ID","Data Value")]
names(work_activities) = c("onet_title", "element_name","scale_id", "data_value")
work_activities = work_activities[,prod(data_value),by=.(onet_title,element_name)]

work_activities_06 <- as.data.table(read_excel("work_activities_06.xlsx"))
work_activities_06 = work_activities_06[,c("Title","Element Name","Scale ID","Data Value")]
names(work_activities_06) = c("onet_title", "element_name","scale_id", "data_value")
work_activities_06 = work_activities_06[,prod(data_value),by=.(onet_title,element_name)]

work_activities_21 <- as.data.table(read_excel("work_activities_21.xlsx"))
work_activities_21 = work_activities_21[,c("Title","Element Name","Scale ID","Data Value")]
names(work_activities_21) = c("onet_title", "element_name","scale_id", "data_value")
work_activities_21 = work_activities_21[,prod(data_value),by=.(onet_title,element_name)]



#From Work Context:
#"Importance of Being Exact or Accurate"
#"Importance of Repeating the Same Tasks"
#"Structured versus Unstructured Work"
#"Pace Determined by Speed of Equipment"
#"Spend Time Making Repetitive Motions"

#From Work Activities:
#"Controlling Machines and Processes"

crosswalk_16 <- as.data.table(read.csv("onetnoc.csv"))
crosswalk_21 <- as.data.table(read.csv("Complete Crosswalks.csv"))
crosswalk_06 <- as.data.table(read.csv("onetnoc_10_v1final.csv"))


exact_accurate = work_context[element_name=="Importance of Being Exact or Accurate",]
repeating = work_context[element_name=="Importance of Repeating Same Tasks",]
structured_vsun = work_context[element_name=="Structured versus Unstructured Work",]
pace_equipment = work_context[element_name=="Pace Determined by Speed of Equipment",]
repetitive = work_context[element_name=="Spend Time Making Repetitive Motions",]
controlling_machines = work_activities[element_name=="Controlling Machines and Processes",]

exact_accurate_06 = work_context_06[element_name=="Importance of Being Exact or Accurate",]
repeating_06 = work_context_06[element_name=="Importance of Repeating Same Tasks",]
structured_vsun_06 = work_context_06[element_name=="Structured versus Unstructured Work",]
pace_equipment_06 = work_context_06[element_name=="Pace Determined by Speed of Equipment",]
repetitive_06 = work_context_06[element_name=="Spend Time Making Repetitive Motions",]
controlling_machines_06 = work_activities_06[element_name=="Controlling Machines and Processes",]

exact_accurate_21 = work_context_21[element_name=="Importance of Being Exact or Accurate",]
repeating_21 = work_context_21[element_name=="Importance of Repeating Same Tasks",]
structured_vsun_21 = work_context_21[element_name=="Structured versus Unstructured Work",]
pace_equipment_21 = work_context_21[element_name=="Pace Determined by Speed of Equipment",]
repetitive_21 = work_context_21[element_name=="Spend Time Making Repetitive Motions",]
controlling_machines_21 = work_activities_21[element_name=="Controlling Machines and Processes",]


#DATA FOR 2006 TO 2021

setkey(final,diff_21_06)
top10_21_06 = final[1:10,1:2]
all_others = final[11:447,1:2]

#all in this section will refer to "all others," meaning all other jobs not in the top 10 movers. 

helloo <- merge.data.table(top10_21_06,crosswalk_16,by.x="noc_2016",by.y="noc_2016",all.x=TRUE)
helloo = helloo[,c("onet_title","noc_title_2016")]
helloo_all <- merge.data.table(all_others,crosswalk_16,by.x="noc_2016",by.y="noc_2016",all.x=TRUE)
helloo_all = helloo_all[,c("onet_title","noc_title_2016")]

helloo_06 <- merge.data.table(top10_21_06,crosswalk_06,by.x="noc_2016",by.y="noc_2006",all.x=TRUE)
helloo_06 = helloo_06[,c("Title","noc_title_2006")]
names(helloo_06) = c("onet_title","noc_title_2016")
helloo_all_06 <- merge.data.table(all_others,crosswalk_06,by.x="noc_2016",by.y="noc_2006",all.x=TRUE)
helloo_all_06 = helloo_all_06[,c("Title","noc_title_2006")]
names(helloo_all_06) = c("onet_title","noc_title_2016")

helloo_21 <- merge.data.table(top10_21_06,crosswalk_21,by.x="noc_2016",by.y="noc.2016",all.x=TRUE)
helloo_21 = helloo_21[,c("onet_title","noc.2016.title")]
names(helloo_21) = c("onet_title","noc_title_2016")
helloo_all_21 <- merge.data.table(all_others,crosswalk_21,by.x="noc_2016",by.y="noc.2016",all.x=TRUE)
helloo_all_21 = helloo_all_21[,c("onet_title","noc.2016.title")]
names(helloo_all_21) = c("onet_title","noc_title_2016")


top10_exact_accurate_16 = merge.data.table(helloo,exact_accurate,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_exact_accurate_16 = top10_exact_accurate_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_exact_accurate_06 = merge.data.table(helloo_06,exact_accurate_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_exact_accurate_06 = top10_exact_accurate_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_exact_accurate_21 = merge.data.table(helloo_21,exact_accurate_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_exact_accurate_21 = na.omit(top10_exact_accurate_21)
top10_exact_accurate_21 = top10_exact_accurate_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

all_exact_accurate_16 = merge.data.table(helloo_all,exact_accurate,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_exact_accurate_16 = all_exact_accurate_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
all_exact_accurate_06 = merge.data.table(helloo_all_06,exact_accurate_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_exact_accurate_06 = all_exact_accurate_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
all_exact_accurate_21 = merge.data.table(helloo_all_21,exact_accurate_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_exact_accurate_21 = na.omit(all_exact_accurate_21)
all_exact_accurate_21 = all_exact_accurate_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]


top10_exact_accurate_merge = merge.data.table(top10_exact_accurate_06,top10_exact_accurate_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
top10_exact_accurate = merge.data.table(top10_exact_accurate_merge,top10_exact_accurate_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(top10_exact_accurate) = c("noc_title","2006","2016","2021")

all_exact_accurate_merge = merge.data.table(all_exact_accurate_06,all_exact_accurate_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
all_exact_accurate = merge.data.table(all_exact_accurate_merge,all_exact_accurate_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(all_exact_accurate) = c("noc_title","2006","2016","2021")

top10_exact_accurate_melted = melt(top10_exact_accurate,id="noc_title")
plot.line.bf(top10_exact_accurate_melted,"variable","value",group.by="noc_title",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Top 10 Movers - Importance of Being Exact or Accurate",export=TRUE,export.name="top10_exact_accurate")


top10_repeating_16 = merge.data.table(helloo,repeating,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_repeating_16 = top10_repeating_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_repeating_06 = merge.data.table(helloo_06,repeating_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_repeating_06 = top10_repeating_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_repeating_21 = merge.data.table(helloo_21,repeating_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_repeating_21 = top10_repeating_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

all_repeating_16 = merge.data.table(helloo_all,repeating,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_repeating_16 = all_repeating_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
all_repeating_06 = merge.data.table(helloo_all_06,repeating_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_repeating_06 = all_repeating_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
all_repeating_21 = merge.data.table(helloo_all_21,repeating_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_repeating_21 = all_repeating_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

top10_repeating_merge = merge.data.table(top10_repeating_06,top10_repeating_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
top10_repeating = merge.data.table(top10_repeating_merge,top10_repeating_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(top10_repeating) = c("noc_title","2006","2016","2021")

all_repeating_merge = merge.data.table(all_repeating_06,all_repeating_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
all_repeating = merge.data.table(all_repeating_merge,all_repeating_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(all_repeating) = c("noc_title","2006","2016","2021")


top10_repeating_melted = melt(top10_repeating,id="noc_title")
plot.line.bf(top10_repeating_melted,"variable","value",group.by="noc_title",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Top 10 Movers - Importance of Repeating Same Tasks",export=TRUE,export.name="top10_repeating")




top10_structured_vsun_16 = merge.data.table(helloo,structured_vsun,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_structured_vsun_16 = top10_structured_vsun[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_structured_vsun_06 = merge.data.table(helloo_06,structured_vsun_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_structured_vsun_06 = top10_structured_vsun_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_structured_vsun_21 = merge.data.table(helloo_21,structured_vsun_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_structured_vsun_21 = top10_structured_vsun_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

all_structured_vsun_16 = merge.data.table(helloo_all,structured_vsun,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_structured_vsun_16 = all_structured_vsun_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
all_structured_vsun_06 = merge.data.table(helloo_all_06,structured_vsun_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_structured_vsun_06 = all_structured_vsun_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
all_structured_vsun_21 = merge.data.table(helloo_all_21,structured_vsun_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_structured_vsun_21 = all_structured_vsun_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

top10_structured_vsun_merge = merge.data.table(top10_structured_vsun_06,top10_structured_vsun_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
top10_structured_vsun = merge.data.table(top10_repeating_merge,top10_structured_vsun_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(top10_structured_vsun) = c("noc_title","2006","2016","2021")

all_structured_vsun_merge = merge.data.table(all_structured_vsun_06,all_structured_vsun_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
all_structured_vsun = merge.data.table(all_structured_vsun_merge,all_structured_vsun_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(all_structured_vsun) = c("noc_title","2006","2016","2021")


top10_structured_vsun_melted = melt(top10_structured_vsun,id="noc_title")
plot.line.bf(top10_structured_vsun_melted,"variable","value",group.by="noc_title",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Top 10 Movers - Structured Vs Unstructured Work",export=TRUE,export.name="top10_strctured")





top10_pace_equipment_16 = merge.data.table(helloo,pace_equipment,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_pace_equipment_16 = top10_pace_equipment_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_pace_equipment_06 = merge.data.table(helloo_06,pace_equipment_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_pace_equipment_06 = top10_pace_equipment_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_pace_equipment_21 = merge.data.table(helloo_21,pace_equipment_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_pace_equipment_21 = top10_pace_equipment_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

all_pace_equipment_16 = merge.data.table(helloo_all,pace_equipment,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_pace_equipment_16 = all_pace_equipment_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
all_pace_equipment_06 = merge.data.table(helloo_all_06,pace_equipment_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_pace_equipment_06 = all_pace_equipment_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
all_pace_equipment_21 = merge.data.table(helloo_all_21,pace_equipment_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_pace_equipment_21 = all_pace_equipment_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

top10_pace_equipment_merge = merge.data.table(top10_pace_equipment_06,top10_pace_equipment_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
top10_pace_equipment = merge.data.table(top10_repeating_merge,top10_pace_equipment_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(top10_pace_equipment) = c("noc_title","2006","2016","2021")

all_pace_equipment_merge = merge.data.table(all_pace_equipment_06,all_pace_equipment_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
all_pace_equipment = merge.data.table(all_pace_equipment_merge,all_pace_equipment_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(all_pace_equipment) = c("noc_title","2006","2016","2021")

top10_pace_equipment_melted = melt(top10_pace_equipment,id="noc_title")
plot.line.bf(top10_pace_equipment_melted,"variable","value",group.by="noc_title",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Top 10 Movers - Pace Determined By Speed of Equipment",export=TRUE,export.name="top10_pace_equipment")




top10_repetitive_16 = merge.data.table(helloo,repetitive,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_repetitive_16 = top10_repetitive_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_repetitive_06 = merge.data.table(helloo_06,repetitive_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_repetitive_06 = top10_repetitive_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_repetitive_21 = merge.data.table(helloo_21,repetitive_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_repetitive_21 = top10_repetitive_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

all_repetitive_16 = merge.data.table(helloo_all,repetitive,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_repetitive_16 = all_repetitive_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
all_repetitive_06 = merge.data.table(helloo_all_06,repetitive_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_repetitive_06 = all_repetitive_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
all_repetitive_21 = merge.data.table(helloo_all_21,repetitive_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_repetitive_21 = all_repetitive_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

top10_repetitive_merge = merge.data.table(top10_repetitive_06,top10_repetitive_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
top10_repetitive = merge.data.table(top10_repetitive_merge,top10_repetitive_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(top10_repetitive) = c("noc_title","2006","2016","2021")

all_repetitive_merge = merge.data.table(all_repetitive_06,all_repetitive_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
all_repetitive = merge.data.table(all_repetitive_merge,all_repetitive_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(all_repetitive) = c("noc_title","2006","2016","2021")


top10_repetitive_melted = melt(top10_repetitive,id="noc_title")
plot.line.bf(top10_repetitive_melted,"variable","value",group.by="noc_title",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Top 10 Movers - Spend Time Making Repetitive Motions",export=TRUE,export.name="top10_repetitive")




top10_controlling_machines_16 = merge.data.table(helloo,controlling_machines,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_controlling_machines_16 = top10_controlling_machines_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_controlling_machines_06 = merge.data.table(helloo_06,controlling_machines_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_controlling_machines_06 = top10_controlling_machines_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_controlling_machines_21 = merge.data.table(helloo_21,controlling_machines_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_controlling_machines_21 = top10_controlling_machines_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

all_controlling_machines_16 = merge.data.table(helloo_all,controlling_machines,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_controlling_machines_16 = all_controlling_machines_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
all_controlling_machines_06 = merge.data.table(helloo_all_06,controlling_machines_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_controlling_machines_06 = all_controlling_machines_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
all_controlling_machines_21 = merge.data.table(helloo_all_21,controlling_machines_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
all_controlling_machines_21 = all_controlling_machines_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

top10_controlling_machines_merge = merge.data.table(top10_controlling_machines_06,top10_controlling_machines_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
top10_controlling_machines = merge.data.table(top10_controlling_machines_merge,top10_controlling_machines_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(top10_controlling_machines) = c("noc_title","2006","2016","2021") 

all_controlling_machines_merge = merge.data.table(all_controlling_machines_06,all_controlling_machines_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
all_controlling_machines = merge.data.table(all_controlling_machines_merge,all_controlling_machines_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(all_controlling_machines) = c("noc_title","2006","2016","2021") 

top10_controlling_machines_melted = melt(top10_controlling_machines,id="noc_title")
plot.line.bf(top10_controlling_machines_melted,"variable","value",group.by="noc_title",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Top 10 Movers - Controlling Machines and Processes",export=TRUE,export.name="top10_controlling_machines")



top10_tasks = cbind(top10_exact_accurate,top10_repeating,top10_structured_vsun,top10_pace_equipment,top10_repetitive,top10_controlling_machines)
names(top10_tasks) = c("noc_title","exact_accurate","noc.2016.title","repeating","noc.2016.title","structured_vsun","noc.2016.title","pace_equipment","noc.2016.title","repetitive","noc.2016.title","controlling_machines")
top10_tasks = top10_tasks[,c("noc_title","exact_accurate","repeating","structured_vsun","pace_equipment","repetitive","controlling_machines")]

top10_tasks_06 = cbind(top10_exact_accurate_06,top10_repeating_06,top10_structured_vsun_06,top10_pace_equipment_06,top10_repetitive_06,top10_controlling_machines_06)
names(top10_tasks_06) = c("noc_title","exact_accurate_06","noc.2016.title","repeating_06","noc.2016.title","structured_vsun_06","noc.2016.title","pace_equipment_06","noc.2016.title","repetitive_06","noc.2016.title","controlling_machines_06")
top10_tasks_06 = top10_tasks_06[,c("noc_title","exact_accurate_06","repeating_06","structured_vsun_06","pace_equipment_06","repetitive_06","controlling_machines_06")]

top10_tasks_21 = cbind(top10_exact_accurate_21,top10_repeating_21,top10_structured_vsun_21,top10_pace_equipment_21,top10_repetitive_21,top10_controlling_machines_21)
names(top10_tasks_21) = c("noc_title","exact_accurate_21","noc.2016.title","repeating_21","noc.2016.title","structured_vsun_21","noc.2016.title","pace_equipment_21","noc.2016.title","repetitive_21","noc.2016.title","controlling_machines_21")
top10_tasks_21 = top10_tasks_21[,c("noc_title","exact_accurate_21","repeating_21","structured_vsun_21","pace_equipment_21","repetitive_21","controlling_machines_21")]

setkey(top10_tasks,noc_title)
setkey(top10_tasks_06,noc_title)
setkey(top10_tasks_21,noc_title)

top10_tasks_merge = merge.data.table(top10_tasks_06,top10_tasks,by.x="noc_title",by.y="noc_title",all.x=TRUE)
top10_tasks_all = merge.data.table(top10_tasks_merge,top10_tasks_21,by.x="noc_title",by.y="noc_title",all.x=TRUE)


melted_top10_tasks_all <- melt(top10_tasks_all, id = "noc_title")
melted_top10_tasks <- melt(top10_tasks, id = "noc_title")
melted_top10_tasks_06 <- melt(top10_tasks_06, id = "noc_title")
melted_top10_tasks_21 <- melt(top10_tasks_21, id = "noc_title")


melted_top10_tasks = melted_top10_tasks[,mean(value),by="variable"]
melted_top10_tasks_06 = melted_top10_tasks_06[,mean(value),by="variable"]
melted_top10_tasks_21 = melted_top10_tasks_21[,mean(value),by="variable"]


melted_top10_tasks_all = cbind(melted_top10_tasks_06,melted_top10_tasks,melted_top10_tasks_21)
names(melted_top10_tasks_all) = c("Task","2006","Task","2016","Task","2021")
melted_top10_tasks_all = melted_top10_tasks_all[,c("Task","2006","2016","2021")]
melted_top10_tasks_all = melt(melted_top10_tasks_all, id = "Task")

Tasks = c("Importance of Being Exact or Accurate","Importance of Repeating the Same Tasks","Structured versus Unstructured Work","Pace Determined by Speed of Equipment","Spend Time Making Repetitive Motions","Controlling Machines and Processes")

melted_top10_tasks_all = cbind(melted_top10_tasks_all,Tasks)
melted_top10_tasks_all = melted_top10_tasks_all[,c("Tasks","variable","value")]


test.graph <- plot.line.bf(melted_top10_tasks_all,"variable","value",group.by="Tasks",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Average of the Top 10 Movers in each Task Category Across the 3 NOC years",export=TRUE,export.name="top10_average_context")


melted_all_exact_accurate = melt(all_exact_accurate, id="noc_title")
melted_all_repeating = melt(all_repeating, id="noc_title")
melted_all_pace_equitpment = melt(all_pace_equipment, id="noc_title")
melted_all_controlling_machines = melt(all_controlling_machines, id="noc_title")
melted_all_repetitive = melt(all_repetitive, id="noc_title")
melted_all_structured_vsun = melt(all_structured_vsun, id="noc_title")

mean_all_exact_accuate = melted_all_exact_accurate[,mean(value,na.rm=TRUE),by="variable"]
mean_all_repeating = melted_all_repeating[,mean(value,na.rm=TRUE),by="variable"]
mean_all_pace_equitpment = melted_all_pace_equitpment[,mean(value,na.rm=TRUE),by="variable"]
mean_all_controlling_machines = melted_all_controlling_machines[,mean(value,na.rm=TRUE),by="variable"]
mean_all_repetitive = melted_all_repetitive[,mean(value,na.rm=TRUE),by="variable"]
mean_all_structured_vsun = melted_all_structured_vsun[,mean(value,na.rm=TRUE),by="variable"]





#melted_top10_tasks_merge = merge.data.table(melted_top10_tasks_06,melted_top10_tasks,by.x="variable",by.y="variable",all.x=TRUE)
#melted_top10_tasks_all = merge.data.table(melted_top10_tasks_merge,melted_top10_tasks_21,by.x="variable",by.y="variable",all.x=TRUE)
#names(melted_top10_tasks_all) = c("noc_title","2006","2016","2021")

melted_top10_tasks_all <- melt(melted_top10_tasks_all, id = "noc_title")

plot.line.bf(melted_top10_tasks_all,"noc_title","value",group.by="variable",ingraph.labels=FALSE,plot.title="Property Administrators scores on routine tasks",export=TRUE,export.name="Property Admin_context")




melted_top10_tasks_all = melted_top10_tasks_all[,mean(value),by="noc_title"]


# FOR PROPERTY ADMINISTRATORS
job1_melted_top10_tasks = melted_top10_tasks[noc_title=="Property administrators",]
names(job1_melted_top10_tasks) = c("noc_title","task_16","value_16")
job1_melted_top10_tasks_06 = melted_top10_tasks_06[noc_title=="Property administrators",]
names(job1_melted_top10_tasks_06) = c("noc_title","task_06","value_06")
job1_melted_top10_tasks_21 = melted_top10_tasks_21[noc_title=="Property administrators",]
names(job1_melted_top10_tasks_21) = c("noc_title","task_21","value_21")

job1_melted_top10_tasks = cbind(job1_melted_top10_tasks,job1_melted_top10_tasks_06,job1_melted_top10_tasks_21)
job1_melted_top10_tasks = job1_melted_top10_tasks[,c("task_16","value_06","value_16","value_21")]
names(job1_melted_top10_tasks) = c("task","2006","2016","2021")
job1_melted_top10_tasks = melt(job1_melted_top10_tasks, id = "task")

plot.line.bf(job1_melted_top10_tasks,"variable","value",group.by="task",ingraph.labels=FALSE,plot.title="Property Administrators scores on routine tasks",export=TRUE,export.name="Property Admin_context")

# FOR Health policy researchers, consultants and program officers
job1_melted_top10_tasks = melted_top10_tasks[noc_title=="Health policy researchers, consultants and program officers",]
names(job1_melted_top10_tasks) = c("noc_title","task_16","value_16")
job1_melted_top10_tasks_06 = melted_top10_tasks_06[noc_title=="Health policy researchers, consultants and program officers",]
names(job1_melted_top10_tasks_06) = c("noc_title","task_06","value_06")
job1_melted_top10_tasks_21 = melted_top10_tasks_21[noc_title=="Health policy researchers, consultants and program officers",]
names(job1_melted_top10_tasks_21) = c("noc_title","task_21","value_21")

job1_melted_top10_tasks = cbind(job1_melted_top10_tasks,job1_melted_top10_tasks_06,job1_melted_top10_tasks_21)
job1_melted_top10_tasks = job1_melted_top10_tasks[,c("task_16","value_06","value_16","value_21")]
names(job1_melted_top10_tasks) = c("task","2006","2016","2021")
job1_melted_top10_tasks = melt(job1_melted_top10_tasks, id = "task")

plot.line.bf(job1_melted_top10_tasks,"variable","value",group.by="task",ingraph.labels=FALSE,plot.title="Health Policy Administrators scores on routine tasks",export=TRUE,export.name="Health Policy_context")


write.csv(top10_tasks,"top10_tasks.csv",row.names=FALSE)

plot.line.bf(top10_tasks,exact_accurate,repeating)

final = final[,diff_16_06:=harm.rank16-harm.rank06]

# Average for 2006-2021
top10_exact_accurate_melted_1 = top10_exact_accurate_melted[,mean(value),by=variable]
top10_repeating_melted_1 = top10_repeating_melted[,mean(value),by=variable]
top10_structured_vsun_melted_1 = top10_structured_vsun_melted[,mean(value),by=variable]
top10_pace_equipment_melted_1 = top10_pace_equipment_melted[,mean(value),by=variable]
top10_repetitive_melted_1 = top10_repetitive_melted[,mean(value),by=variable]
top10_controlling_machines_melted_1 = top10_controlling_machines_melted[,mean(value),by=variable]




#--------------------#
#DATA FOR 2016 TO 2021

setkey(final,diff_21_16)
top10_21_16 = final[1:10,1:2]

helloo <- merge.data.table(top10_21_16,crosswalk_16,by.x="noc_2016",by.y="noc_2016",all.x=TRUE)
helloo = helloo[,c("onet_title","noc_title_2016")]

helloo_06 <- merge.data.table(top10_21_16,crosswalk_06,by.x="noc_2016",by.y="noc_2006",all.x=TRUE)
helloo_06 = helloo_06[,c("Title","noc_title_2006")]
names(helloo_06) = c("onet_title","noc_title_2016")

helloo_21 <- merge.data.table(top10_21_16,crosswalk_21,by.x="noc_2016",by.y="noc.2016",all.x=TRUE)
helloo_21 = helloo_21[,c("onet_title","noc.2016.title")]
names(helloo_21) = c("onet_title","noc_title_2016")


top10_exact_accurate_16 = merge.data.table(helloo,exact_accurate,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_exact_accurate_16 = top10_exact_accurate_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_exact_accurate_06 = merge.data.table(helloo_06,exact_accurate_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_exact_accurate_06 = top10_exact_accurate_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_exact_accurate_21 = merge.data.table(helloo_21,exact_accurate_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_exact_accurate_21 = na.omit(top10_exact_accurate_21)
top10_exact_accurate_21 = top10_exact_accurate_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]


top10_exact_accurate_merge = merge.data.table(top10_exact_accurate_06,top10_exact_accurate_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
top10_exact_accurate = merge.data.table(top10_exact_accurate_merge,top10_exact_accurate_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(top10_exact_accurate) = c("noc_title","2006","2016","2021")

top10_exact_accurate_melted = melt(top10_exact_accurate,id="noc_title")
plot.line.bf(top10_exact_accurate_melted,"variable","value",group.by="noc_title",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Top 10 Movers - Importance of Being Exact or Accurate",export=TRUE,export.name="top10_exact_accurate")


top10_repeating_16 = merge.data.table(helloo,repeating,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_repeating_16 = top10_repeating_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_repeating_06 = merge.data.table(helloo_06,repeating_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_repeating_06 = top10_repeating_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_repeating_21 = merge.data.table(helloo_21,repeating_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_repeating_21 = top10_repeating_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

top10_repeating_merge = merge.data.table(top10_repeating_06,top10_repeating_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
top10_repeating = merge.data.table(top10_repeating_merge,top10_repeating_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(top10_repeating) = c("noc_title","2006","2016","2021")

top10_repeating_melted = melt(top10_repeating,id="noc_title")
plot.line.bf(top10_repeating_melted,"variable","value",group.by="noc_title",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Top 10 Movers - Importance of Repeating Same Tasks",export=TRUE,export.name="top10_repeating")

top10_structured_vsun_16 = merge.data.table(helloo,structured_vsun,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_structured_vsun_16 = top10_structured_vsun[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_structured_vsun_06 = merge.data.table(helloo_06,structured_vsun_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_structured_vsun_06 = top10_structured_vsun_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_structured_vsun_21 = merge.data.table(helloo_21,structured_vsun_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_structured_vsun_21 = top10_structured_vsun_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]


top10_structured_vsun_merge = merge.data.table(top10_structured_vsun_06,top10_structured_vsun_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
top10_structured_vsun = merge.data.table(top10_repeating_merge,top10_structured_vsun_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(top10_structured_vsun) = c("noc_title","2006","2016","2021")


top10_structured_vsun_melted = melt(top10_structured_vsun,id="noc_title")
plot.line.bf(top10_structured_vsun_melted,"variable","value",group.by="noc_title",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Top 10 Movers - Structured Vs Unstructured Work",export=TRUE,export.name="top10_strctured")


top10_pace_equipment_16 = merge.data.table(helloo,pace_equipment,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_pace_equipment_16 = top10_pace_equipment_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_pace_equipment_06 = merge.data.table(helloo_06,pace_equipment_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_pace_equipment_06 = top10_pace_equipment_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_pace_equipment_21 = merge.data.table(helloo_21,pace_equipment_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_pace_equipment_21 = top10_pace_equipment_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]


top10_pace_equipment_merge = merge.data.table(top10_pace_equipment_06,top10_pace_equipment_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
top10_pace_equipment = merge.data.table(top10_repeating_merge,top10_pace_equipment_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(top10_pace_equipment) = c("noc_title","2006","2016","2021")


top10_pace_equipment_melted = melt(top10_pace_equipment,id="noc_title")
plot.line.bf(top10_pace_equipment_melted,"variable","value",group.by="noc_title",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Top 10 Movers - Pace Determined By Speed of Equipment",export=TRUE,export.name="top10_pace_equipment")


top10_repetitive_16 = merge.data.table(helloo,repetitive,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_repetitive_16 = top10_repetitive_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_repetitive_06 = merge.data.table(helloo_06,repetitive_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_repetitive_06 = top10_repetitive_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_repetitive_21 = merge.data.table(helloo_21,repetitive_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_repetitive_21 = top10_repetitive_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

top10_repetitive_merge = merge.data.table(top10_repetitive_06,top10_repetitive_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
top10_repetitive = merge.data.table(top10_repetitive_merge,top10_repetitive_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(top10_repetitive) = c("noc_title","2006","2016","2021")


top10_repetitive_melted = melt(top10_repetitive,id="noc_title")
plot.line.bf(top10_repetitive_melted,"variable","value",group.by="noc_title",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Top 10 Movers - Spend Time Making Repetitive Motions",export=TRUE,export.name="top10_repetitive")


top10_controlling_machines_16 = merge.data.table(helloo,controlling_machines,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_controlling_machines_16 = top10_controlling_machines_16[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_controlling_machines_06 = merge.data.table(helloo_06,controlling_machines_06,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_controlling_machines_06 = top10_controlling_machines_06[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]
top10_controlling_machines_21 = merge.data.table(helloo_21,controlling_machines_21,by.x="onet_title",by.y="onet_title",all.x=TRUE)
top10_controlling_machines_21 = top10_controlling_machines_21[,mean(V1,na.rm=TRUE),by=.(noc_title_2016)]

top10_controlling_machines_merge = merge.data.table(top10_controlling_machines_06,top10_controlling_machines_16,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
top10_controlling_machines = merge.data.table(top10_controlling_machines_merge,top10_controlling_machines_21,by.x="noc_title_2016",by.y="noc_title_2016",all.x=TRUE)
names(top10_controlling_machines) = c("noc_title","2006","2016","2021") 


top10_controlling_machines_melted = melt(top10_controlling_machines,id="noc_title")
plot.line.bf(top10_controlling_machines_melted,"variable","value",group.by="noc_title",colours=rep("#14365D",10),ingraph.labels=FALSE,plot.title="Top 10 Movers - Controlling Machines and Processes",export=TRUE,export.name="top10_controlling_machines")


# Average for 2016-2021
top10_exact_accurate_melted_2 = top10_exact_accurate_melted[,mean(value,na.rm=TRUE),by=variable]
top10_repeating_melted_2 = top10_repeating_melted[,mean(value,na.rm=TRUE),by=variable]
top10_structured_vsun_melted_2 = top10_structured_vsun_melted[,mean(value,na.rm=TRUE),by=variable]
top10_pace_equipment_melted_2 = top10_pace_equipment_melted[,mean(value,na.rm=TRUE),by=variable]
top10_repetitive_melted_2 = top10_repetitive_melted[,mean(value,na.rm=TRUE),by=variable]
top10_controlling_machines_melted_2 = top10_controlling_machines_melted[,mean(value,na.rm=TRUE),by=variable]

top10_exact_accurate_plot = cbind(top10_exact_accurate_melted_1,top10_exact_accurate_melted_2)
top10_repeating_plot = cbind(top10_repeating_melted_1,top10_repeating_melted_2)
top10_structured_vsun_plot = cbind(top10_structured_vsun_melted_1,top10_structured_vsun_melted_2)
top10_pace_equipment_plot = cbind(top10_pace_equipment_melted_1,top10_pace_equipment_melted_2)
top10_repetitive_plot = cbind(top10_repetitive_melted_1,top10_repetitive_melted_2)
top10_controlling_machines_plot = cbind(top10_controlling_machines_melted_1,top10_controlling_machines_melted_2)

names(top10_exact_accurate_plot) = c("Years","change_21_06","Years2","change_21_16")
names(top10_repeating_plot) = c("Years","change_21_06","Years2","change_21_16")
names(top10_structured_vsun_plot) = c("Years","change_21_06","Years2","change_21_16")
names(top10_pace_equipment_plot) = c("Years","change_21_06","Years2","change_21_16")
names(top10_repetitive_plot) = c("Years","change_21_06","Years2","change_21_16")
names(top10_controlling_machines_plot) = c("Years","change_21_06","Years2","change_21_16")

top10_exact_accurate_plot = top10_exact_accurate_plot[,Years2:=NULL]
top10_repeating_plot = top10_repeating_plot[,Years2:=NULL]
top10_structured_vsun_plot = top10_structured_vsun_plot[,Years2:=NULL]
top10_pace_equipment_plot = top10_pace_equipment_plot[,Years2:=NULL]
top10_repetitive_plot = top10_repetitive_plot[,Years2:=NULL]
top10_controlling_machines_plot = top10_controlling_machines_plot[,Years2:=NULL]


melted_top10_exact_accurate_plot = melt(top10_exact_accurate_plot,id="Years")
melted_top10_exact_accurate_plot = as.data.frame(melted_top10_exact_accurate_plot)
melted_top10_exact_accurate_plot$value = as.numeric(melted_top10_exact_accurate_plot$value)
melted_top10_exact_accurate_plot$Years = as.numeric(melted_top10_exact_accurate_plot$Years)

melted_top10_repeating_plot = melt(top10_repeating_plot,id="Years")
melted_top10_repeating_plot = as.data.frame(melted_top10_repeating_plot)
melted_top10_repeating_plot$value = as.numeric(melted_top10_repeating_plot$value)
melted_top10_repeating_plot$Years = as.numeric(melted_top10_repeating_plot$Years)

melted_top10_structured_vsun_plot = melt(top10_structured_vsun_plot,id="Years")
melted_top10_structured_vsun_plot = as.data.frame(melted_top10_structured_vsun_plot)
melted_top10_structured_vsun_plot$value = as.numeric(melted_top10_structured_vsun_plot$value)
melted_top10_structured_vsun_plot$Years = as.numeric(melted_top10_structured_vsun_plot$Years)

melted_top10_pace_equipment_plot = melt(top10_pace_equipment_plot,id="Years")
melted_top10_pace_equipment_plot = as.data.frame(melted_top10_pace_equipment_plot)
melted_top10_pace_equipment_plot$value = as.numeric(melted_top10_pace_equipment_plot$value)
melted_top10_pace_equipment_plot$Years = as.numeric(melted_top10_pace_equipment_plot$Years)

melted_top10_repetitive_plot = melt(top10_repetitive_plot,id="Years")
melted_top10_repetitive_plot = as.data.frame(melted_top10_repetitive_plot)
melted_top10_repetitive_plot$value = as.numeric(melted_top10_repetitive_plot$value)
melted_top10_repetitive_plot$Years = as.numeric(melted_top10_repetitive_plot$Years)

melted_top10_controlling_machines_plot = melt(top10_controlling_machines_plot,id="Years")
melted_top10_controlling_machines_plot = as.data.frame(melted_top10_controlling_machines_plot)
melted_top10_controlling_machines_plot$value = as.numeric(melted_top10_controlling_machines_plot$value)
melted_top10_controlling_machines_plot$Years = as.numeric(melted_top10_controlling_machines_plot$Years)


ggplot(melted_top10_exact_accurate_plot,aes(Years,value,colour=variable)) +
  geom_line() +
  theme(panel.background=element_blank(),text = element_text(size = 22)) +
  scale_x_continuous(breaks = c(1,2,3),labels=c("2006","2016","2021")) +
  scale_colour_discrete(name  ="Top 10 Jobs Average",
                        breaks=c("change_21_06", "change_21_16"),
                        labels=c("Top 10 Between 2006 and 2021", "Top 10 Between 2016 and 2021")) +
  ggtitle("Importance of Being Exact or Accurate")


ggplot(melted_top10_repeating_plot,aes(Years,value,colour=variable)) +
  geom_line() +
  theme(panel.background=element_blank(),text = element_text(size = 22)) +
  scale_x_continuous(breaks = c(1,2,3),labels=c("2006","2016","2021")) +
  scale_colour_discrete(name  ="Top 10 Jobs Average",
                        breaks=c("change_21_06", "change_21_16"),
                        labels=c("Top 10 Between 2006 and 2021", "Top 10 Between 2016 and 2021")) +
  ggtitle("Importance of Repeating the Same Tasks")


ggplot(melted_top10_structured_vsun_plot,aes(Years,value,colour=variable)) +
  geom_line() +
  theme(panel.background=element_blank(),text = element_text(size = 22)) +
  scale_x_continuous(breaks = c(1,2,3),labels=c("2006","2016","2021")) +
  scale_colour_discrete(name  ="Top 10 Jobs Average",
                        breaks=c("change_21_06", "change_21_16"),
                        labels=c("Top 10 Between 2006 and 2021", "Top 10 Between 2016 and 2021")) +
  ggtitle("Structured Vs Unstructured Work")


ggplot(melted_top10_pace_equipment_plot,aes(Years,value,colour=variable)) +
  geom_line() +
  theme(panel.background=element_blank(),text = element_text(size = 22)) +
  scale_x_continuous(breaks = c(1,2,3),labels=c("2006","2016","2021")) +
  scale_colour_discrete(name  ="Top 10 Jobs Average",
                        breaks=c("change_21_06", "change_21_16"),
                        labels=c("Top 10 Between 2006 and 2021", "Top 10 Between 2016 and 2021")) +
  ggtitle("Pace Determined by Speed of Equipment")


ggplot(melted_top10_repetitive_plot,aes(Years,value,colour=variable)) +
  geom_line() +
  theme(panel.background=element_blank(),text = element_text(size = 22)) +
  scale_x_continuous(breaks = c(1,2,3),labels=c("2006","2016","2021")) +
  scale_colour_discrete(name  ="Top 10 Jobs Average",
                        breaks=c("change_21_06", "change_21_16"),
                        labels=c("Top 10 Between 2006 and 2021", "Top 10 Between 2016 and 2021")) +
  ggtitle("Spend Time Making Repetitive Motions")


ggplot(melted_top10_controlling_machines_plot,aes(Years,value,colour=variable)) +
  geom_line() +
  theme(panel.background=element_blank(),text = element_text(size = 22)) +
  scale_x_continuous(breaks = c(1,2,3),labels=c("2006","2016","2021")) +
  scale_colour_discrete(name  ="Top 10 Jobs Average",
                        breaks=c("change_21_06", "change_21_16"),
                        labels=c("Top 10 Between 2006 and 2021", "Top 10 Between 2016 and 2021")) +
  ggtitle("Controlling Machines and Processes")




# VIII - Top 10 most intensive jobs each NOC year

setkey(final,harm.rank06)
top10_2006 = final[1:10,2]

setkey(final,harm.rank16)
top10_2016 = final[1:10,2]

setkey(final,harm.rank21)
top10_2021 = final[1:10,2]

top10_combined = cbind(top10_2006,top10_2016,top10_2021)
names(top10_combined) = c("2006","2016","2021")

top10_table = flextable(top10_combined)

top10_table = merge_h(top10_table,i=1)

top10_table = theme_zebra(top10_table)
theme_zebra(top10_table)

top10_combined = top10_combined[,id:=c(1,2,3,4,5,6,7,8,9,10)]

melted_top10_combined = melt(top10_combined,id="id")
names(melted_top10_combined) = c("y","x","group")
setkey(melted_top10_combined,group)

melted_top10_combined = as.data.frame(melted_top10_combined,stringsAsFactors=TRUE)
 
melted_top10_combined$y = as.numeric(melted_top10_combined$y)
melted_top10_combined$x = as.numeric(melted_top10_combined$x)
melted_top10_combined$group = as.character(melted_top10_combined$group)

unique(melted_top10_combined$y)



ggplot(melted_top10_combined, aes(x = x, y = y, colour = "group")) +
  geom_line(aes(color = group), size = 3) +
  geom_point(aes(color = group), size = 8) +
  scale_y_reverse(breaks = 1:nrow(melted_top10_combined))  + 
  xlab("Years") +
  ylab(label="Ranking in the Top 10") +
  theme(panel.background=element_blank(),text = element_text(size = 20)) +
  scale_x_continuous(breaks = c(1,2,3),labels=c("2006","2016","2021")) +
  labs(colour="Top 10 Jobs")
ggsave("Bump_Chart",height=9,width=14)
  




#plot.scatter.bf(eng_tech,"Score06_eng_tech","Score16_eng_tech")
#------------------------------------------------------#
#------------------------------------------------------------#
#---------------------------------------------------------------------#

#Top Movers

setkey(final,diff_21_06)
setkey(final,skill_groups_2)
top_1 = final[1:10,c("noc_title","rank06","rank16","rank21")]


setkey(final,diff_21_06)
setkey(final,skill_groups_2)
#group2 = which(final$skill_groups_2=="2-3")
#group2 = as.numeric(group2[1:1])
#top_2 = final[100:110,c("noc_title","rank.06","rank16","rank21")]
top_2 = final[skill_groups_2 %in% c("2-3"),c("noc_title","rank06","rank16","rank21")]
top_2 = top_2[1:10]

setkey(final,diff_21_06)
setkey(final,skill_groups_2)
top_3 = final[skill_groups_2 %in% c("4-5"),c("noc_title","rank06","rank16","rank21")]
top_3 = top_3[1:10]


setkey(final,diff_21_06)
setkey(final,skill_groups_2)
top_4 = final[skill_groups_2 %in% c("6-7"),c("noc_title","rank06","rank16","rank21")]
top_4 = top_4[1:10]


final = final[order(diff_21_06,decreasing=TRUE)]
setkey(final,skill_groups_2)
topdown_1 = final[skill_groups_2 %in% c("0-1"),c("noc_title","rank06","rank16","rank21")]
topdown_1 = topdown_1[1:10]

final = final[order(diff_21_06,decreasing=TRUE)]
setkey(final,skill_groups_2)
topdown_2 = final[skill_groups_2 %in% c("2-3"),c("noc_title","rank06","rank16","rank21")]
topdown_2 = topdown_2[1:10]

final = final[order(diff_21_06,decreasing=TRUE)]
setkey(final,skill_groups_2)
topdown_3 = final[skill_groups_2 %in% c("4-5"),c("noc_title","rank06","rank16","rank21")]
topdown_3 = topdown_3[1:10]

final = final[order(diff_21_06,decreasing=TRUE)]
setkey(final,skill_groups_2)
topdown_4 = final[skill_groups_2 %in% c("6-7"),c("noc_title","rank06","rank16","rank21")]
topdown_4 = topdown_4[1:10]


write.csv(final,"final_results.csv",row.names=FALSE)





#---------------------------------------_#
#-----------------------------------------#
#---------------------------------------------#
#----------------------------------------------------------------#

#Plot Section


#1 - Reshape and prepare the data

top_1 <- top_1 %>%
  mutate(noc_title=as.factor(noc_title),
         rank06=as.numeric(rank06),rank16=as.numeric(rank16),rank21=as.numeric(rank21))

melted_top_1 <- melt(top_1, id = "noc_title")

melted_top_1 <- melted_top_1 %>%
  mutate(noc_title=as.factor(noc_title), variable=as.factor(variable),
         value=as.numeric(value))

melted_top_1$noc_title = str_wrap(melted_top_1$noc_title, width = 15, indent = 0, exdent = 0)




top_2 <- top_2 %>%
  mutate(noc_title=as.factor(noc_title),
         rank06=as.numeric(rank06),rank16=as.numeric(rank16),rank21=as.numeric(rank21))

melted_top_2 <- melt(top_2, id = "noc_title")

melted_top_2 <- melted_top_2 %>%
  mutate(noc_title=as.factor(noc_title), variable=as.factor(variable),
         value=as.numeric(value))

melted_top_2$noc_title = str_wrap(melted_top_2$noc_title, width = 15, indent = 0, exdent = 0)



top_3 <- top_3 %>%
  mutate(noc_title=as.factor(noc_title),
         rank06=as.numeric(rank06),rank16=as.numeric(rank16),rank21=as.numeric(rank21))

melted_top_3 <- melt(top_3, id = "noc_title")

melted_top_3 <- melted_top_3 %>%
  mutate(noc_title=as.factor(noc_title), variable=as.factor(variable),
         value=as.numeric(value))

melted_top_3$noc_title = str_wrap(melted_top_3$noc_title, width = 15, indent = 0, exdent = 0)




top_4 <- top_4 %>%
  mutate(noc_title=as.factor(noc_title),
         rank06=as.numeric(rank06),rank16=as.numeric(rank16),rank21=as.numeric(rank21))

melted_top_4 <- melt(top_4, id = "noc_title")

melted_top_4 <- melted_top_4 %>%
  mutate(noc_title=as.factor(noc_title), variable=as.factor(variable),
         value=as.numeric(value))

melted_top_4$noc_title = str_wrap(melted_top_4$noc_title, width = 15, indent = 0, exdent = 0)




topdown_1 <- topdown_1 %>%
  mutate(noc_title=as.factor(noc_title),
         rank06=as.numeric(rank06),rank16=as.numeric(rank16),rank21=as.numeric(rank21))

melted_topdown_1 <- melt(topdown_1, id = "noc_title")

melted_topdown_1 <- melted_topdown_1 %>%
  mutate(noc_title=as.factor(noc_title), variable=as.factor(variable),
         value=as.numeric(value))

melted_topdown_1$noc_title = str_wrap(melted_topdown_1$noc_title, width = 15, indent = 0, exdent = 0)




topdown_2 <- topdown_2 %>%
  mutate(noc_title=as.factor(noc_title),
         rank06=as.numeric(rank06),rank16=as.numeric(rank16),rank21=as.numeric(rank21))

melted_topdown_2 <- melt(topdown_2, id = "noc_title")

melted_topdown_2 <- melted_topdown_2 %>%
  mutate(noc_title=as.factor(noc_title), variable=as.factor(variable),
         value=as.numeric(value))

melted_topdown_2$noc_title = str_wrap(melted_topdown_2$noc_title, width = 15, indent = 0, exdent = 0)




topdown_3 <- topdown_3 %>%
  mutate(noc_title=as.factor(noc_title),
         rank06=as.numeric(rank06),rank16=as.numeric(rank16),rank21=as.numeric(rank21))

melted_topdown_3 <- melt(topdown_3, id = "noc_title")

melted_topdown_3 <- melted_topdown_3 %>%
  mutate(noc_title=as.factor(noc_title), variable=as.factor(variable),
         value=as.numeric(value))

melted_topdown_3$noc_title = str_wrap(melted_topdown_3$noc_title, width = 15, indent = 0, exdent = 0)




topdown_4 <- topdown_4 %>%
  mutate(noc_title=as.factor(noc_title),
         rank06=as.numeric(rank06),rank16=as.numeric(rank16),rank21=as.numeric(rank21))

melted_topdown_4 <- melt(topdown_4, id = "noc_title")

melted_topdown_4 <- melted_topdown_4 %>%
  mutate(noc_title=as.factor(noc_title), variable=as.factor(variable),
         value=as.numeric(value))

melted_topdown_4$noc_title = str_wrap(melted_topdown_4$noc_title, width = 15, indent = 0, exdent = 0)




#2 - A - Plots for Top 10 Using Absolute Differences


ggplot(data=melted_top_1,aes(value,noc_title,fill=variable)) +
  geom_bar(stat="identity",position="dodge",alpha=0.9) +
  geom_text(aes(label=value),position = position_dodge(width=1)) +
  scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
  xlab(label="Rank") + 
  ylab(label="NOC Title") +
  theme(panel.background=element_blank()) +
  guides(fill=guide_legend(title="Rank for Each NOC Year")) +
ggtitle("Top 10 Movers (more digital skills) between 2006 and 2021 in Skills 0 and A")
  ggsave("output1.pdf",height=9,width=14)
  
  
  
  ggplot(data=melted_top_2,aes(value,noc_title,fill=variable)) +
    geom_bar(stat="identity",position="dodge",alpha=0.9) +
    geom_text(aes(label=value),position = position_dodge(width=1)) +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Rank") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    ggtitle("Top 10 Movers (more digital skills) between 2006 and 2021 in Skill B")
  ggsave("output2.pdf",height=9,width=14)

  
  ggplot(data=melted_top_3,aes(value,noc_title,fill=variable)) +
    geom_bar(stat="identity",position="dodge",alpha=0.9) +
    geom_text(aes(label=value),position = position_dodge(width=1)) +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Rank") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    ggtitle("Top 10 Movers (more digital skills) between 2006 and 2021 in Skill C")
  ggsave("output3.pdf",height=9,width=14)
  
  
  ggplot(data=melted_top_4,aes(value,noc_title,fill=variable)) +
    geom_bar(stat="identity",position="dodge",alpha=0.9) +
    geom_text(aes(label=value),position = position_dodge(width=1)) +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Rank") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    ggtitle("Top 10 Movers (more digital skills) between 2006 and 2021 in Skill D")
  ggsave("output4.pdf",height=9,width=14)
  
  
  ggplot(data=melted_topdown_1,aes(value,noc_title,fill=variable)) +
    geom_bar(stat="identity",position="dodge",alpha=0.9) +
    geom_text(aes(label=value),position = position_dodge(width=1)) +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Rank") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    ggtitle("Top 10 Movers (less digital skills) between 2006 and 2021 in Skills 0 and A")
  ggsave("outputdown1.pdf",height=9,width=14)
  
  
  ggplot(data=melted_topdown_2,aes(value,noc_title,fill=variable)) +
    geom_bar(stat="identity",position="dodge",alpha=0.9) +
    geom_text(aes(label=value),position = position_dodge(width=1)) +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Rank") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    ggtitle("Top 10 Movers (less digital skills) between 2006 and 2021 in Skill B")
  ggsave("outputdown2.pdf",height=9,width=14)
  
  
  ggplot(data=melted_topdown_3,aes(value,noc_title,fill=variable)) +
    geom_bar(stat="identity",position="dodge",alpha=0.9) +
    geom_text(aes(label=value),position = position_dodge(width=1)) +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Rank") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    ggtitle("Top 10 Movers (less digital skills) between 2006 and 2021 in Skill C")
  ggsave("outputdown3.pdf",height=9,width=14)
  
  
  ggplot(data=melted_topdown_4,aes(value,noc_title,fill=variable)) +
    geom_bar(stat="identity",position="dodge",alpha=0.9) +
    geom_text(aes(label=value),position = position_dodge(width=1)) +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Rank") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    ggtitle("Top 10 Movers (less digital skills) between 2006 and 2021 in Skill D")
  ggsave("outputdown4.pdf",height=9,width=14)

  

  
#2 - B - Plots For Scores
  
  
  # norm_tech_design_avg = average_norm_scores[1:3]
  # norm_programming_avg = average_norm_scores[4:6]
  # norm_comp_elec_avg = average_norm_scores[7:9]
  # norm_inter_comp_avg = average_norm_scores[10:12]
  # norm_eng_tech_avg = average_norm_scores[13:15]
  # norm_telco_avg = average_norm_scores[16:18]
  
  
  ggplot(data=average_norm_scores) +
    geom_point(mapping=aes(x=categories, y=average_scores.V1,color=years)) +
    theme(panel.background=element_blank()) +
    xlab(label="The 6 Technology Categories") + 
    ylab(label="Average Normalized Scores") +
    scale_x_discrete(labels=c("Computer and Electronics", "Engineering and Tech", "Inter W Comp","Programming", "Tech Design", "Telco"))
  
  
  tech_design_sd = melted_tech_design_sd[1:10]
  tech_design_sd = merge(tech_design_sd,final,by.x="noc_2016",by.y="noc_2016",all.x=TRUE)
  tech_design_sd = tech_design_sd[,.(noc_2016,V1,noc_title,normalized_tech_design_diff)] 
  tech_design_sd = tech_design_sd[,Change:=ifelse(normalized_tech_design_diff>0,"Score Increased","Score Decreased")]
  tech_design_sd = tech_design_sd[,normalized_tech_design_diff:=NULL] 
  tech_design_sd$noc_title = str_wrap(tech_design_sd$noc_title, width = 15, indent = 0, exdent = 0)
  
  
  programming_sd = melted_programming_sd[1:10]
  programming_sd = merge(programming_sd,final,by.x="noc_2016",by.y="noc_2016",all.x=TRUE)
  programming_sd = programming_sd[,.(noc_2016,V1,noc_title,normalized_programming_diff)] 
  programming_sd = programming_sd[,Change:=ifelse(normalized_programming_diff>0,"Score Increased","Score Decreased")]
  programming_sd = programming_sd[,normalized_programming_diff:=NULL] 
  programming_sd$noc_title = str_wrap(programming_sd$noc_title, width = 15, indent = 0, exdent = 0)
  
  
  comp_elec_sd = melted_comp_elec_sd[1:10]
  comp_elec_sd = merge(comp_elec_sd,final,by.x="noc_2016",by.y="noc_2016",all.x=TRUE)
  comp_elec_sd = comp_elec_sd[,.(noc_2016,V1,noc_title,normalized_comp_elec_diff)] 
  comp_elec_sd = comp_elec_sd[,Change:=ifelse(normalized_comp_elec_diff>0,"Score Increased","Score Decreased")]
  comp_elec_sd = comp_elec_sd[,normalized_comp_elec_diff:=NULL] 
  comp_elec_sd$noc_title = str_wrap(comp_elec_sd$noc_title, width = 15, indent = 0, exdent = 0)
  
  inter_comp_sd = melted_inter_comp_sd[1:10]
  inter_comp_sd = merge(inter_comp_sd,final,by.x="noc_2016",by.y="noc_2016",all.x=TRUE)
  inter_comp_sd = inter_comp_sd[,.(noc_2016,V1,noc_title,normalized_inter_comp_diff)] 
  inter_comp_sd = inter_comp_sd[,Change:=ifelse(normalized_inter_comp_diff>0,"Score Increased","Score Decreased")]
  inter_comp_sd = inter_comp_sd[,normalized_inter_comp_diff:=NULL] 
  inter_comp_sd$noc_title = str_wrap(inter_comp_sd$noc_title, width = 15, indent = 0, exdent = 0)
  
  eng_tech_sd = melted_eng_tech_sd[1:10]
  eng_tech_sd = merge(eng_tech_sd,final,by.x="noc_2016",by.y="noc_2016",all.x=TRUE)
  eng_tech_sd = eng_tech_sd[,.(noc_2016,V1,noc_title,normalized_eng_tech_diff)] 
  eng_tech_sd = eng_tech_sd[,Change:=ifelse(normalized_eng_tech_diff>0,"Score Increased","Score Decreased")]
  eng_tech_sd = eng_tech_sd[,normalized_eng_tech_diff:=NULL] 
  eng_tech_sd$noc_title = str_wrap(eng_tech_sd$noc_title, width = 15, indent = 0, exdent = 0)
  
  telco_sd = melted_telco_sd[1:10]
  telco_sd = merge(telco_sd,final,by.x="noc_2016",by.y="noc_2016",all.x=TRUE)
  telco_sd = telco_sd[,.(noc_2016,V1,noc_title,normalized_telco_diff)] 
  telco_sd = telco_sd[,Change:=ifelse(normalized_telco_diff>0,"Score Increased","Score Decreased")]
  telco_sd = telco_sd[,normalized_telco_diff:=NULL] 
  telco_sd$noc_title = str_wrap(telco_sd$noc_title, width = 15, indent = 0, exdent = 0)
  
  
  # Highest Standard Deviation in Tech Design Scores
  ggplot(data=tech_design_sd,aes(V1,noc_title, fill="#14365D")) + #Add if you want: ,fill=Change
    geom_bar(stat="identity",position="dodge",alpha=0.9,color="#14365D") +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Standard Deviation") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    #guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    #theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5)) +
    theme(legend.position="none") +
    coord_flip() +
    ggtitle("Highest Standard Deviation in Engineering Design Scores")
  ggsave("tech_design_sd.pdf",height=9,width=15)
  
 
  # Highest Standard Deviation in Programming Scores
  ggplot(data=programming_sd,aes(V1,noc_title,fill="#14365D")) +
    geom_bar(stat="identity",position="dodge",alpha=0.9,color="#14365D") +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Standard Deviation") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    #guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    #theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5)) +
    theme(legend.position="none") +
    coord_flip() +
    ggtitle("Highest Standard Deviation in Programming Scores")
  ggsave("programming_sd.pdf",height=9,width=15)
  
  
  # Highest Standard Deviation in Comp Elec Scores
  ggplot(data=comp_elec_sd,aes(V1,noc_title,fill="#14365D")) +
    geom_bar(stat="identity",position="dodge",alpha=0.9,color="#14365D") +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Standard Deviation") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    #guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    #theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5)) +
    theme(legend.position="none") +
    coord_flip() +
    ggtitle("Highest Standard Deviation in Computers and Electronics Scores")
  ggsave("comp_elec_sd.pdf",height=9,width=15)
  
  

  # Highest Standard Deviation in Inter Comp Scores
  ggplot(data=inter_comp_sd,aes(V1,noc_title,fill="#14365D")) +
    geom_bar(stat="identity",position="dodge",alpha=0.9,color="#14365D") +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Standard Deviation") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    #guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    #theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5)) +
    theme(legend.position="none") +
    coord_flip() +
    ggtitle("Highest Standard Deviation in Interacting With Computers Scores")
  ggsave("inter_comp_sd.pdf",height=9,width=15)
  
  
  # Highest Standard Deviation in Eng Tech Scores
  ggplot(data=eng_tech_sd,aes(V1,noc_title,fill="#14365D")) +
    geom_bar(stat="identity",position="dodge",alpha=0.9,color="#14365D") +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Standard Deviation") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    #guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    #theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5)) +
    theme(legend.position="none") +
    coord_flip() +
    ggtitle("Highest Standard Deviation in Engineering and Technology Scores")
  ggsave("eng_tech_sd.pdf",height=9,width=15)
  
  
  
  # Highest Standard Deviation in Telco Scores
  ggplot(data=telco_sd,aes(V1,noc_title,fill="#14365D")) +
    geom_bar(stat="identity",position="dodge",alpha=0.9,color="#14365D") +
    scale_fill_manual(values = c("#14365D","#DD347A","#8AD4DF")) +
    xlab(label="Standard Deviation") + 
    ylab(label="NOC Title") +
    theme(panel.background=element_blank()) +
    #guides(fill=guide_legend(title="Rank for Each NOC Year")) +
    #theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5)) +
    theme(legend.position="none") +
    coord_flip() +
    ggtitle("Highest Standard Deviation in Telecommunications Scores")
  ggsave("telco_sd.pdf",height=9,width=15)
  
  
  
  
  
  
  
  
#To reverse Bar Graph:
 # + coord_flip()
  
  
  

  
  
  
#----------------------------------------------------------------------------#
  
  
#SCRATCH WORK


#ggplot(data=top_1) +
 # geom_point(mapping=aes(x=noc_title, y=rank06),alpha=1/2,color="red") +
  #geom_point(mapping=aes(x=noc_title, y=rank16),alpha=1/2,color="green") +
  #geom_point(mapping=aes(x=noc_title, y=rank21),alpha=1/2,color="blue") +
  #theme(panel.background=element_blank(),legend.position = c(.95, .95),
   #     legend.justification = c("right", "top"),
    #    legend.box.just = "right",
     #   legend.margin = margin(6, 6, 6, 6)) + 
  #theme(plot.background=element_blank(),axis.text.x=element_text(angle = 90),text = element_text(size = 6)) + 
  #scale_x_discrete(name="NOC Title") + 
  #scale_y_continuous(name="Rank in 2006",limits=c(0,500)) #+
  #labs(x = "NOC Title",
   #    y = "()",
    #   color = "Legend")
  


#ggplot(data=melted_top_1,mapping=aes(x=value)) +
#  geom_bar(alpha=1/2,color="red") +
 # theme(panel.background=element_blank(),legend.position = c(.95, .95),
   #     legend.justification = c("right", "top"),
    #    legend.box.just = "right",
     #   legend.margin = margin(6, 6, 6, 6)) + 
 # theme(plot.background=element_blank(),axis.text.x=element_text(angle = 90),text = element_text(size = 6)) + 
  #scale_x_discrete(name="NOC Title") + 
  #scale_y_continuous(name="Rank in 2006",limits=c(0,10)) #+
#labs(x = "NOC Title",
#    y = "()",
#   color = "Legend")







#ggplot(data=top_1) +
 # geom_point(mapping=aes(x=noc_title, y=rank06),alpha=1/2) +
  #geom_point(mapping=aes(x=noc_title, y=rank16),alpha=1/2) +
  #geom_point(mapping=aes(x=noc_title, y=rank21),alpha=1/2) +
  #theme(panel.background=element_blank()) + 
  #theme(plot.background=element_blank(),axis.text.x=element_text(angle = 90),text = element_text(size = 6)) + 
  #scale_x_discrete(name="NOC Title") + 
  #scale_y_continuous(name="Rank in 2006",limits=c(0,500))  
  
  
#ggplot(data=top_1) +
#  geom_bar(mapping=aes(x=noc_title, y=rank06)) + theme(panel.background=element_blank()) + theme(plot.background=element_blank())

#ggplot(data=top_1) +
 # geom_point(mapping=aes(x=noc_title, y=rank16))






#setkey(final,skill_groups_2)





#Combine the 3 into one table
#results_full <- data.table(short06,short16,short21)
#names(results_full) <- c("noc_title_2006","harm.rank06","noc_title_2016","harm.rank16","noc_title_2021","harm.rank21")

#Keep the same noc titles, show harmonic rank across years
#setkey(short16,noc_code)
#setkey(short06,noc_code)
#setkey(short21,noc_code)

#short06from16 <- full_crosswalk[short06,nomatch=0]
#short16v06 <- short16[short06,nomatch=0]
#short16v21 <- short16[short21,nomatch=0]
#setkey(short16v21,noc_title_2016)

#table <- short16v21[short16v06,nomatch=0]



#short06v16 <- short06[short16,nomatch=0]
#short06v21 <- short06[short21,nomatch=0]
#setkey(short06v16,noc_title_2006)
#table <- short06v16[short06v21,nomatch=0]
#final_table <- subset(table, select = -c(harm.rank16) )
#names(final_table) <- c("noc_title","harm.rank06","harm.rank16","harm.rank21")

#Order the results 
#final_table <- final_table[order(harm.rank06),]

#Clean environment
#rm(results_2006) 
#rm(results_2016) 
#rm(results_2021) 
#rm(short06) 
#rm(short16) 
#rm(short21) 
#rm(table) 




#is.unsorted(final_table)
#is.unsorted(final_table[(harm.rank16)])





