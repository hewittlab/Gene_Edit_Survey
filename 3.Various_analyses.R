# Need to have run the 1.Data_prep.R script and have the resulting data in this workspace

library(plyr) # Various
library(car) # Recode variables
library(doBy) # Group summary stats

## Need to create a "Results symlink" folder in the WD to save results files to.


#ATTRITION-----------------------------------------------------------------------------------------------

# CHECK ATTRITION as questions progress - Count NA's

# Demographic variables
# "sex","YOB","country","ethnicity","religion","religion_type","edu_level","worked_health","worked_health_type","heard_about","wealth"

# Survey variables
# "genetic_cond","genetic_cond_affected","genetic_cond_type","kids_cure_life","kids_cure_debil","embr_prev_life","embr_prev_debil","edit_for_nondis","deter_phys_appear","deter_intell","deter_strength","other_traits_alter","gen_mod_food"

NA_vec <- c("sex","YOB","country","ethnicity","religion","religion_type","edu_level","worked_health","worked_health_type","heard_about","wealth","genetic_cond","genetic_cond_affected","genetic_cond_type","kids_cure_life","kids_cure_debil","embr_prev_life","embr_prev_debil","edit_for_nondis","deter_phys_appear","deter_intell","deter_strength","gen_mod_food")

NA_count <- colSums(is.na(all[,NA_vec]))
prop_complete <- round(100 - NA_count/nrow(all)*100,1)
total_count <- rep(nrow(all),length(NA_vec))
obs_count <- total_count - NA_count
attrition <- data.frame(total_count, obs_count, NA_count, prop_complete)
attrition
# Write results
write.csv(attrition, "Results symlink/attrition.csv")
write.table(attrition, "Results symlink/attrition.txt", sep="\t")


#DEM_SUMMARY_STATS---------------------------------------------------------------------------------------

# DEMOGRAPHIC SUMMARY STATS

# COUNTRY
country_table <- table(all$country)
country_prop <- round(prop.table(country_table)*100,2)
country_stat <- data.frame(country_table, country_prop)
country_stat <- country_stat[,c(1,2,4)]
country_stat <- rename(country_stat,c("Var1"="country","Freq"="freq","Freq.1"="%"))
# Sort
country_stat <- country_stat[order(-country_stat$freq),] 
country_stat
# Write results
write.csv(country_stat, "Results symlink/country_stat.csv")
write.table(country_stat, "Results symlink/country_stat.txt", sep="\t")


# ETHNICITY
ethnicity_table <- table(all$ethnicity)
ethnicity_prop <- round(prop.table(ethnicity_table)*100,2)
ethnicity_stat <- data.frame(ethnicity_table, ethnicity_prop)
ethnicity_stat <- ethnicity_stat[,c(1,2,4)]
ethnicity_stat <- rename(ethnicity_stat,c("Var1"="ethnicity","Freq"="freq","Freq.1"="%"))
# Label the levels
ethnicity_stat$ethnicity <- factor(ethnicity_stat$ethnicity, levels = c(1,2,3,4,5,6,7,8,9,10,11),labels = c("Mixed Race","African/African American","Asian Indian","Caucasian (European)","Caucasian (Middle East)","Hispanic, Latino or Spanish","Indigenous Australian","Native American","North East Asian (Mongol, Tibetan, Korean, Japanese, etc)","Pacific (Polynesian, micronesian, etc)","South East Asian (Chinese, Thai, Malay, Filipino, etc)"))
# Sort
ethnicity_stat <- ethnicity_stat[order(-ethnicity_stat$freq),] 
ethnicity_stat
# Write results
write.csv(ethnicity_stat, "Results symlink/ethnicity_stat.csv")
write.table(ethnicity_stat, "Results symlink/ethnicity_stat.txt", sep="\t")


# AGE(*SEX)
age_stat <- data.frame(age=all$age, group=all$sex)
age_stat <- summaryBy(age~group, data=age_stat, FUN=c(mean, sd, length))
age.prop <- c(round(length(all$sex[all$sex == "F"])/nrow(all)*100,1), round(length(all$sex[all$sex == "M"])/nrow(all)*100,1))
age_stat <- age_stat[1:2,]
age_stat <- cbind(age_stat, age.prop)
# Calculate and rbind totals
age_total <- c("All", round(mean(all$age),2), round(sd(all$age),2), sum(age_stat[,4]), sum(age_stat[,5]))
age_stat <- rbind(age_stat, age_total)
age_stat$age.mean <-as.numeric(age_stat$age.mean)
age_stat$age.mean <- round(age_stat$age.mean,2)
age_stat$age.sd <-as.numeric(age_stat$age.sd)
age_stat$age.sd <- round(age_stat$age.sd,2)
age_stat
# No. NA's
age_stat$age.length <-as.numeric(age_stat$age.length)
nrow(all)-sum(age_stat[1:2,4])
# Write results
write.csv(age_stat, "Results symlink/age_stat.csv")
write.table(age_stat, "Results symlink/age_stat.txt", sep="\t")


# SEX
sex_table <- table(all$sex)
sex_prop <- round(prop.table(sex_table)*100,2)
sex_stat <- data.frame(sex_table, sex_prop)
sex_stat <- sex_stat[,c(1,2,4)]
sex_stat <- rename(sex_stat,c("Var1"="sex","Freq"="freq","Freq.1"="%"))
sex_stat
# Write results
write.csv(sex_stat, "Results symlink/sex_stat.csv")
write.table(sex_stat, "Results symlink/sex_stat.txt", sep="\t")


# EDUCATION
edu_level_table <- table(all$edu_level)
edu_level_prop <- round(prop.table(edu_level_table)*100,2)
edu_level_stat <- data.frame(edu_level_table, edu_level_prop)
edu_level_stat <- edu_level_stat[,c(1,2,4)]
edu_level_stat <- rename(edu_level_stat,c("Var1"="education","Freq"="freq","Freq.1"="%"))
# Label the levels
edu_level_stat$education <- factor(edu_level_stat$education, levels = c(1,2,3,4,5,6),labels = c("No formal schooling","Finished primary school","Finished high school","Finished a course following school","Finished undergraduate university degree","Finished postgraduate university degree"))
edu_level_stat
# Write results
write.csv(edu_level_stat, "Results symlink/edu_level_stat.csv")
write.table(edu_level_stat, "Results symlink/edu_level_stat.txt", sep="\t")


# WEALTH
wealth_table <- table(all$wealth)
wealth_prop <- round(prop.table(wealth_table)*100,2)
wealth_stat <- data.frame(wealth_table, wealth_prop)
wealth_stat <- wealth_stat[,c(1,2,4)]
wealth_stat <- rename(wealth_stat,c("Var1"="wealth","Freq"="freq","Freq.1"="%"))
# Label the levels
wealth_stat$wealth <- factor(wealth_stat$wealth, levels = c(1,2,3),labels = c("Above average wealth","Average wealth","Below average wealth"))
wealth_stat
# Write results
write.csv(wealth_stat, "Results symlink/wealth_stat.csv")
write.table(wealth_stat, "Results symlink/wealth_stat.txt", sep="\t")


# RELIGION
religion_table <- table(all$religion)
religion_prop <- round(prop.table(religion_table)*100,2)
religion_stat <- data.frame(religion_table, religion_prop)
religion_stat <- religion_stat[,c(1,2,4)]
religion_stat <- rename(religion_stat,c("Var1"="religion","Freq"="freq","Freq.1"="%"))
religion_stat
# Write results
write.csv(religion_stat, "Results symlink/religion_stat.csv")
write.table(religion_stat, "Results symlink/religion_stat.txt", sep="\t")


# RELIGION_TYPE
relig_type_table <- table(all$religion_type)
relig_type_prop <- round(prop.table(relig_type_table)*100,2)
relig_type_stat <- data.frame(relig_type_table, relig_type_prop)
relig_type_stat <- relig_type_stat[,c(1,2,4)]
relig_type_stat <- rename(relig_type_stat,c("Var1"="religion_type","Freq"="freq","Freq.1"="%"))
# Label the levels
relig_type_stat$religion_type <- factor(relig_type_stat$religion_type, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),labels = c("Christian - Protestant/Anglican","Christian - Catholic","Christian - Other","Muslim","Jewish","Buddhist","Hindu","Shinto","Taoism","Confucianism","Sikhism","Jainism","Druze","Other"))
# Sort
relig_type_stat <- relig_type_stat[order(-relig_type_stat$freq),] 
relig_type_stat
# Write results
write.csv(relig_type_stat, "Results symlink/relig_type_stat.csv")
write.table(relig_type_stat, "Results symlink/relig_type_stat.txt", sep="\t")


# WORKED IN HEALTH
worked_health_table <- table(all$worked_health)
worked_health_prop <- round(prop.table(worked_health_table)*100,2)
worked_health_stat <- data.frame(worked_health_table, worked_health_prop)
worked_health_stat <- worked_health_stat[,c(1,2,4)]
worked_health_stat <- rename(worked_health_stat,c("Var1"="worked_health","Freq"="freq","Freq.1"="%"))
worked_health_stat
# Write results
write.csv(worked_health_stat, "Results symlink/worked_health_stat.csv")
write.table(worked_health_stat, "Results symlink/worked_health_stat.txt", sep="\t")


# WORKED_TYPE
worked_type_table <- table(all$worked_health_type)
worked_type_prop <- round(prop.table(worked_type_table)*100,2)
worked_type_stat <- data.frame(worked_type_table, worked_type_prop)
worked_type_stat <- worked_type_stat[,c(1,2,4)]
worked_type_stat <- rename(worked_type_stat,c("Var1"="worked_type","Freq"="freq","Freq.1"="%"))
# Label the levels
worked_type_stat$worked_type <- factor(worked_type_stat$worked_type, levels = c(1,2,3,4,5,6),labels = c("Medical Doctor","Scientific Researcher","Nurse","Allied health worker","Other role at hospital/medical centre","Other"))
# Sort
worked_type_stat <- worked_type_stat[order(-worked_type_stat$freq),] 
worked_type_stat
# Write results
write.csv(worked_type_stat, "Results symlink/worked_type_stat.csv")
write.table(worked_type_stat, "Results symlink/worked_type_stat.txt", sep="\t")


# HEARD ABOUT
heard_table <- table(all$heard_about)
heard_prop <- round(prop.table(heard_table)*100,2)
heard_stat <- data.frame(heard_table, heard_prop)
heard_stat <- heard_stat[,c(1,2,4)]
heard_stat <- rename(heard_stat,c("Var1"="heard_about","Freq"="freq","Freq.1"="%"))
# Label the levels
heard_stat$heard_about <- factor(heard_stat$heard_about, levels = c(1,2,3),labels = c("I have never heard of it","I have heard a little about it","I have heard a lot about it"))
heard_stat
# Write results
write.csv(heard_stat, "Results symlink/heard_stat.csv")
write.table(heard_stat, "Results symlink/heard_stat.txt", sep="\t")


# GENETIC CONDITION
genetic_cond_table <- table(all$genetic_cond)
genetic_cond_prop <- round(prop.table(genetic_cond_table)*100,2)
genetic_cond_stat <- data.frame(genetic_cond_table, genetic_cond_prop)
genetic_cond_stat <- genetic_cond_stat[,c(1,2,4)]
genetic_cond_stat <- rename(genetic_cond_stat,c("Var1"="genetic_cond","Freq"="freq","Freq.1"="%"))
genetic_cond_stat
# Write results
write.csv(genetic_cond_stat, "Results symlink/genetic_cond_stat.csv")
write.table(genetic_cond_stat, "Results symlink/genetic_cond_stat.txt", sep="\t")


# GENETIC CONDITION AFFECTED
genetic_affected_table <- table(all$genetic_cond_affected)
genetic_affected_prop <- round(prop.table(genetic_affected_table)*100,2)
genetic_affected_stat <- data.frame(genetic_affected_table, genetic_affected_prop)
genetic_affected_stat <- genetic_affected_stat[,c(1,2,4)]
genetic_affected_stat <- rename(genetic_affected_stat,c("Var1"="genetic_affected","Freq"="freq","Freq.1"="%"))
# Label the levels
genetic_affected_stat$genetic_affected <- factor(genetic_affected_stat$genetic_affected, levels = c(1,2,3),labels = c("Me","Another family member(s)","Me and a family member(s)"))
genetic_affected_stat
# Write results
write.csv(genetic_affected_stat, "Results symlink/genetic_affected_stat.csv")
write.table(genetic_affected_stat, "Results symlink/genetic_affected_stat.txt", sep="\t")


# GENETIC CONDITION TYPE
genetic_type_table <- table(all$genetic_cond_type)
genetic_type_prop <- round(prop.table(genetic_type_table)*100,2)
genetic_type_stat <- data.frame(genetic_type_table, genetic_type_prop)
genetic_type_stat <- genetic_type_stat[,c(1,2,4)]
genetic_type_stat <- rename(genetic_type_stat,c("Var1"="genetic_type","Freq"="freq","Freq.1"="%"))
# Label the levels
genetic_type_stat$genetic_type <- factor(genetic_type_stat$genetic_type, levels = c(1,2,3,4,5,6,7,8,9,10),labels = c("Cystic Fibrosis","Huntington’s Disease","Muscular Dystrophy","Sickle Cell Anaemia","Beta Thalassemia","Haemophilia","Tay Sachs Disease","Fragile X Syndrome","Down’s Syndrome, Edward’s Syndrome, Patau Syndrome","Other"))
# Sort
genetic_type_stat <- genetic_type_stat[order(-genetic_type_stat$freq),] 
genetic_type_stat
# Write results
write.csv(genetic_type_stat, "Results symlink/genetic_type_stat.csv")
write.table(genetic_type_stat, "Results symlink/genetic_type_stat.txt", sep="\t")



# AGE(*COUNTRY)
age_country_stat <- data.frame(age=all$age, group=all$country)
age_country_stat <- summaryBy(age~group, data=age_country_stat, FUN=c(mean, sd, length))
age_country_stat$age.mean <- round(age_country_stat$age.mean,2)
age_country_stat$age.sd <- round(age_country_stat$age.sd,2)
age_country_stat <- rename(age_country_stat,c("age.length"="age.freq"))
# Sort
age_country_stat <- age_country_stat[order(-age_country_stat[,4]),] 
age_country_stat
# Write results
write.csv(age_country_stat, "Results symlink/age_country_stat.csv")
write.table(age_country_stat, "Results symlink/age_country_stat.txt", sep="\t")


# SEX(*COUNTRY)
sex_country_table <- table(all$country,all$sex)
sex_country_prop <- prop.table(sex_country_table,1)*100 # 1 specifies row margins (2 for columns I think)
sex_country_prop <- sex_country_prop[,1:2]
# Retrieve total country counts from above (need to reorder)
country_stat <- country_stat[order(country_stat$country),] 
sex_country_stat <- cbind(country_stat$freq, sex_country_table, sex_country_prop)
sex_country_stat <- data.frame(sex_country_stat)
sex_country_stat <- rename(sex_country_stat,c("V1"="Total","F"="F.freq","M"="M.freq","F.1"="F.prop","M.1"="M.prop"))
sex_country_stat$F.prop <- round(sex_country_stat$F.prop,2)
sex_country_stat$M.prop <- round(sex_country_stat$M.prop,2)
# Sort
sex_country_stat <- sex_country_stat[order(-country_stat$freq),] 
sex_country_stat
# Write results
write.csv(sex_country_stat, "Results symlink/sex_country_stat.csv")
write.table(sex_country_stat, "Results symlink/sex_country_stat.txt", sep="\t")


# SEX(*ETHNICITY)
ethnic_vec <- all$ethnicity
ethnic_vec <- factor(ethnic_vec, levels = c(1,2,3,4,5,6,7,8,9,10,11),labels = c("Mixed Race","African/African American","Asian Indian","Caucasian (European)","Caucasian (Middle East)","Hispanic, Latino or Spanish","Indigenous Australian","Native American","North East Asian (Mongol, Tibetan, Korean, Japanese, etc)","Pacific (Polynesian, micronesian, etc)","South East Asian (Chinese, Thai, Malay, Filipino, etc)"))
sex_ethnicity_table <- table(ethnic_vec,all$sex)
sex_ethnicity_prop <- prop.table(sex_ethnicity_table,1)*100 # 1 specifies row margins (2 for columns I think)
sex_ethnicity_prop <- sex_ethnicity_prop[,1:2]
# Retrieve total ethnicity counts from above (need to reorder)
ethnicity_stat <- ethnicity_stat[order(ethnicity_stat$ethnicity),] 
sex_ethnicity_stat <- cbind(ethnicity_stat$ethnicity, ethnicity_stat$freq, sex_ethnicity_table, sex_ethnicity_prop)
sex_ethnicity_stat <- data.frame(sex_ethnicity_stat)
sex_ethnicity_stat <- rename(sex_ethnicity_stat,c("V1"="ethnicity", "V2"="Total","F"="F.freq","M"="M.freq","F.1"="F.prop","M.1"="M.prop"))
sex_ethnicity_stat$F.prop <- round(sex_ethnicity_stat$F.prop,2)
sex_ethnicity_stat$M.prop <- round(sex_ethnicity_stat$M.prop,2)
# Sort
sex_ethnicity_stat <- sex_ethnicity_stat[order(-ethnicity_stat$freq),] 
sex_ethnicity_stat
# Write results
write.csv(sex_ethnicity_stat, "Results symlink/sex_ethnicity_stat.csv")
write.table(sex_ethnicity_stat, "Results symlink/sex_ethnicity_stat.txt", sep="\t")





# 4) comparisons 
# a) between ethnicity 
# - Caucasian vs Non-Caucasian 
# - Chinese vs Caucasian 
# b) sex 
# c) age - prob need to also do a subgroup analysis on those over the 
# age of 19 (as this is the estimated readability of the hardest 
#            question). 
# d) Medical Background 
# e) Religion 
# f) Country 
# g) Inherited disease








