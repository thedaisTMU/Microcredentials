
# clear data and close graphs
rm(list=ls())
graphics.off()

# set working directory
setwd("C:/Users/Ibrahim/OneDrive/Desktop/Brookfield 2023/Microcredentials/RATM Files/2016")



# Install Packages

#install.packages("data.table")
#install.packages("stringr")
#install.packages("psych")
#install.packages("knitr")
#install.packages("tidyverse")
#install.packages("readxl")


library(data.table)
library(stringr)
library(psych)
library(knitr)
library(tidyverse)
library(readxl)


# Read in O*NET data and BII+E crosswalk
knowledge <- as.data.table(read_excel("knowledge.xlsx"))
skill <- as.data.table(read_excel("skills.xlsx"))
work.activity <- as.data.table(read_excel("work Activities.xlsx"))
crosswalk <- as.data.table(read.csv("onetnoc.csv"))


# Align O*NET Skills, Knowledge, and Work Activity Data
full.skill <- rbindlist(list(knowledge,skill,work.activity))
names(full.skill) <- c("onet","title","element.id","element.name","scale.id","scale.name","value","N","stder","lci","uci","sup","nr","date","source")
full.skill[,c("N","date","source","lci","uci"):=NULL]


# Link O*NET data to NOC using crosswalk
setkey(crosswalk,onet)
setkey(full.skill,onet)
full.crosswalk.skill <- crosswalk[full.skill,nomatch=0,allow.cartesian=TRUE]
full.avg.crosswalk.skill <- full.crosswalk.skill[,mean(value),by=.(noc_2016,element.id,element.name,scale.id,scale.name)]

####IBRAHIM COMMENT - Here we filtered by 2016 NOC titles, taking the mean value
#for the different NOC titles under the same skill set, that might have many different O*net Titles

#Clean environment
rm(full.crosswalk.skill) #Remove redundancies
rm(full.skill)
rm(work.activity,skill,knowledge)

### Generate Rankings for NOCs based on O*NET Skills
#This chunk of code selects the technology skills of interest, computes our ranking of technology and digital occupations, and produces our final list of digital and high-tech occupations, as outlined in methodology for the report.

# Select the technology skills of interest for each NOC
tech.skills <- c("2.B.3.b", "2.B.3.e", "2.C.3.a", "4.A.3.b.1", "2.C.3.b", "2.C.9.a")
individual.ranking <- full.avg.crosswalk.skill[element.id %in% tech.skills,prod(V1),by=.(noc_2016,element.id)]
individual.ranking <- reshape(individual.ranking,direction="wide",v.names = c("V1"),timevar="element.id",idvar="noc_2016")
individual.ranking <- individual.ranking[!is.na(noc_2016)] #One O*NET occupation was not matched to NOC in the crosswalk and must be removed
setkey(individual.ranking,noc_2016)
rows <- nrow(individual.ranking)


#------------------------------------------------------------------------
#####IBRAHIM COMMENT -- Here, we basically filtered the full average crosswalk. 
#This was done using the %in% tech.skills, choosing only the tech skills
#in the list. Then, we changed the data from long to wide (will be useful for the loop later)
#Then we  multiplied the values of the importance to those surveyed (1-5), 
#by the level they indicated is in the job (0-7), to produce our measure V1
#We will then use this V1 to rank jobs.
#--------------------------------------------------------------------------


#####The first loop basically creates a place ranking for the jobs in each skill category
#based on the V1 calculated earlier.

# Rank each NOC across each of the selected tech skills
for(n in tech.skills){
  individual.ranking[,str_c("rank.",n):=frankv(get(str_c("V1.",n)),order=-1)]
}


#### The second loop takes the harmonic mean across the 6 categories in order 
#to come up with a harmonic rank.

# Calculate the harmonic means for each NOC under three ranking systems
## Rankings across all tech skills
for(n in seq(1,rows)){
  individual.ranking[n,harm.rank:=harmonic.mean(c(get("rank.2.B.3.b")+1,rank.2.B.3.e+1,
                                                  rank.2.C.3.a+1,rank.4.A.3.b.1+1,rank.2.C.3.b+1,rank.2.C.9.a+1))]
}


## Rankings across digital skills - used to distinguish between digital and high-tech occupations
for(n in seq(1,rows)){
  individual.ranking[n,harm.rank.digital:=harmonic.mean(c(rank.2.B.3.e+1,
                                                          rank.2.C.3.a+1,rank.4.A.3.b.1+1,rank.2.C.9.a+1))]
}

## Rankings excluding knowledge of telecommunications
### Used for sensititivity analysis in report, but not for findings
for(n in seq(1,rows)){
  individual.ranking[n,harm.rank.no.tel:=harmonic.mean(c(rank.2.B.3.b+1,rank.2.B.3.e+1,
                                                         rank.2.C.3.a+1,rank.4.A.3.b.1+1,rank.2.C.3.b+1))]
}


# Select tech, high-tech, and digital occupations based on cut-offs from analyzing rankings
tech.cut.off <- 25 #Define the tech cut off rank
digital.cut.off <- 17 #Define digital cut-off rank
individual.ranking[,tech:=0]
individual.ranking[harm.rank < tech.cut.off, tech:=1]
individual.ranking[harm.rank < tech.cut.off, digital:= "High-Tech"]
individual.ranking[harm.rank < tech.cut.off & harm.rank.digital < digital.cut.off, digital:= "Digital"]
individual.ranking[harm.rank.no.tel < tech.cut.off,tech.no.tel:=1]
# Rename fields to be easier to understand prior to output
individual.ranking <- individual.ranking %>%
  rename("2.C.3.a - Comp and Elec" = "V1.2.C.3.a",
         "2.C.3.b - Eng and Tech" = "V1.2.C.3.b",
         "2.C.9.a - Telco" = "V1.2.C.9.a" ,
         "2.B.3.b - Tech Design" = "V1.2.B.3.b",
         "2.B.3.e - Programming" = "V1.2.B.3.e" ,
         "4.A.3.b.1 - Inter w Comp" =  "V1.4.A.3.b.1",
         "Rank - Comp and Elec" = "rank.2.C.3.a",
         "Rank - Eng and Tech" = "rank.2.C.3.b",
         "Rank - Telco" = "rank.2.C.9.a",
         "Rank - Tech Design" = "rank.2.B.3.b",
         "Rank - Programming" = "rank.2.B.3.e",
         "Rank - Inter w Comp" = "rank.4.A.3.b.1"
  )

#Create a NOC code and name field for convenience
individual.ranking$noc_code <- substr(individual.ranking$noc_2016, 1, 4)
individual.ranking$noc_name <- substr(individual.ranking$noc_2016, 5, nchar(individual.ranking$noc_2016))

#Write the CSV file containing the rankings
write.csv(individual.ranking,"tech.sector.def.2016.csv",row.names=FALSE)

#Clean up the environment
rm(crosswalk, digital.cut.off, n, tech.cut.off, tech.skills)



kable(
  individual.ranking %>%
    filter(tech == 1 | !is.na(digital) | !is.na(tech.no.tel)) %>%
    mutate(tech.no.tel = ifelse(is.na(tech.no.tel) & !is.na(digital), "Exclude", "Remain")) %>%
    mutate(tech.no.tel = ifelse(!is.na(tech.no.tel) & is.na(digital), "Add", tech.no.tel)) %>%
    arrange(digital, desc(tech.no.tel), harm.rank) %>%
    select("Occupation" = noc_2016, "Tech Definition" = digital, "Tech Def w/o Telco" = tech.no.tel)
)


#Load and process skill and occupation data
onet.s <- full.avg.crosswalk.skill
onet.s$element<- paste(substr(onet.s$element.id, 1, 5), onet.s$element.name)

onet.s <- onet.s %>%
  select(noc_2016, element, scale.name, V1) %>%
  filter(!is.na(noc_2016)) %>%
  spread(scale.name, V1) %>%
  mutate(score = Importance * Level) %>% # Multiplied importance by level  of skill per O*NET recommendations
  select(noc_2016, element, score) %>%
  spread(element, score) %>%
  remove_rownames %>% 
  column_to_rownames(var="noc_2016")


#Create principal components
onet.s.pca <- prcomp(onet.s, center=TRUE, scale.=TRUE, rank. = 5) # The last argument retains the first 5 PCs. This can be changed if you desire.


#Analyze principal component skill loadings
onet.loading <- as.data.frame(onet.s.pca$rotation) %>% rownames_to_column(var = "Skill") %>% arrange(desc(PC3))
write.csv(as.data.frame(onet.s.pca$rotation) %>% arrange(PC3), "PCA_skill_loads.csv")


#Analyze occupational scores
onet.pca <- as.data.frame(predict(onet.s.pca, newdata = onet.s))
write.csv(onet.s.pca$rotation, "PCA_occ_scores.csv")

summary(onet.s.pca)
# kable(signif(head(onet.loading[,-1], 10)), digits = 6)
kable(head(cbind(Skill = onet.loading[,1],round(onet.loading[,-1], digits = 2)), 10))


