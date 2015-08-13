# Need to have run the 1.Data_prep.R script and have the resulting data in this workspace

library(plyr) # Various
library(car) # Recode variables
library(doBy) # Group summary stats
library(MASS) # OLR
library(ordinal) # OLR
library(lmtest) # OLR
library(VGAM) # OLR
library(nnet) # Multinomial LR
library(fifer) # Posthoc chi-square

## Need to create a "Results symlink" folder in the WD to save results files to.

#UNIQUE_IP-----------------------------------------------------------------------------------------------

#Total number of IPs
length(all$ip)

#Number of unique IPs
length(unique(all$ip))

#Multiples
mult_ip <- all$ip[which(duplicated(all$ip))]
table(mult_ip)


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

# KIDS_CURE_LIFE
kids_cure_life_table <- table(all$kids_cure_life)
kids_cure_life_prop <- round(prop.table(kids_cure_life_table)*100,2)
kids_cure_life_stat <- data.frame(kids_cure_life_table, kids_cure_life_prop)
kids_cure_life_stat <- kids_cure_life_stat[,c(1,2,4)]
kids_cure_life_stat <- rename(kids_cure_life_stat,c("Var1"="response","Freq"="freq","Freq.1"="%"))
# Write results
write.csv(kids_cure_life_stat, "Results symlink/kids_cure_life_stat.csv")
write.table(kids_cure_life_stat, "Results symlink/kids_cure_life_stat.txt", sep="\t")


# KIDS_CURE_DEBIL
kids_cure_debil_table <- table(all$kids_cure_debil)
kids_cure_debil_prop <- round(prop.table(kids_cure_debil_table)*100,2)
kids_cure_debil_stat <- data.frame(kids_cure_debil_table, kids_cure_debil_prop)
kids_cure_debil_stat <- kids_cure_debil_stat[,c(1,2,4)]
kids_cure_debil_stat <- rename(kids_cure_debil_stat,c("Var1"="response","Freq"="freq","Freq.1"="%"))
# Write results
write.csv(kids_cure_debil_stat, "Results symlink/kids_cure_debil_stat.csv")
write.table(kids_cure_debil_stat, "Results symlink/kids_cure_debil_stat.txt", sep="\t")


# EMBR_PREV_LIFE
embr_prev_life_table <- table(all$embr_prev_life)
embr_prev_life_prop <- round(prop.table(embr_prev_life_table)*100,2)
embr_prev_life_stat <- data.frame(embr_prev_life_table, embr_prev_life_prop)
embr_prev_life_stat <- embr_prev_life_stat[,c(1,2,4)]
embr_prev_life_stat <- rename(embr_prev_life_stat,c("Var1"="response","Freq"="freq","Freq.1"="%"))
# Write results
write.csv(embr_prev_life_stat, "Results symlink/embr_prev_life_stat.csv")
write.table(embr_prev_life_stat, "Results symlink/embr_prev_life_stat.txt", sep="\t")


# EMBR_PREV_DEBIL
embr_prev_debil_table <- table(all$embr_prev_debil)
embr_prev_debil_prop <- round(prop.table(embr_prev_debil_table)*100,2)
embr_prev_debil_stat <- data.frame(embr_prev_debil_table, embr_prev_debil_prop)
embr_prev_debil_stat <- embr_prev_debil_stat[,c(1,2,4)]
embr_prev_debil_stat <- rename(embr_prev_debil_stat,c("Var1"="response","Freq"="freq","Freq.1"="%"))
# Write results
write.csv(embr_prev_debil_stat, "Results symlink/embr_prev_debil_stat.csv")
write.table(embr_prev_debil_stat, "Results symlink/embr_prev_debil_stat.txt", sep="\t")


# EDIT_FOR_NONDIS
edit_for_nondis_table <- table(all$edit_for_nondis)
edit_for_nondis_prop <- round(prop.table(edit_for_nondis_table)*100,2)
edit_for_nondis_stat <- data.frame(edit_for_nondis_table, edit_for_nondis_prop)
edit_for_nondis_stat <- edit_for_nondis_stat[,c(1,2,4)]
edit_for_nondis_stat <- rename(edit_for_nondis_stat,c("Var1"="response","Freq"="freq","Freq.1"="%"))
# Write results
write.csv(edit_for_nondis_stat, "Results symlink/edit_for_nondis_stat.csv")
write.table(edit_for_nondis_stat, "Results symlink/edit_for_nondis_stat.txt", sep="\t")


# DETER_PHYS_APPEAR
deter_phys_appear_table <- table(all$deter_phys_appear)
deter_phys_appear_prop <- round(prop.table(deter_phys_appear_table)*100,2)
deter_phys_appear_stat <- data.frame(deter_phys_appear_table, deter_phys_appear_prop)
deter_phys_appear_stat <- deter_phys_appear_stat[,c(1,2,4)]
deter_phys_appear_stat <- rename(deter_phys_appear_stat,c("Var1"="response","Freq"="freq","Freq.1"="%"))
# Write results
write.csv(deter_phys_appear_stat, "Results symlink/deter_phys_appear_stat.csv")
write.table(deter_phys_appear_stat, "Results symlink/deter_phys_appear_stat.txt", sep="\t")


# DETER_INTELL
deter_intell_table <- table(all$deter_intell)
deter_intell_prop <- round(prop.table(deter_intell_table)*100,2)
deter_intell_stat <- data.frame(deter_intell_table, deter_intell_prop)
deter_intell_stat <- deter_intell_stat[,c(1,2,4)]
deter_intell_stat <- rename(deter_intell_stat,c("Var1"="response","Freq"="freq","Freq.1"="%"))
# Write results
write.csv(deter_intell_stat, "Results symlink/deter_intell_stat.csv")
write.table(deter_intell_stat, "Results symlink/deter_intell_stat.txt", sep="\t")


# DETER_STRENGTH
deter_strength_table <- table(all$deter_strength)
deter_strength_prop <- round(prop.table(deter_strength_table)*100,2)
deter_strength_stat <- data.frame(deter_strength_table, deter_strength_prop)
deter_strength_stat <- deter_strength_stat[,c(1,2,4)]
deter_strength_stat <- rename(deter_strength_stat,c("Var1"="response","Freq"="freq","Freq.1"="%"))
# Write results
write.csv(deter_strength_stat, "Results symlink/deter_strength_stat.csv")
write.table(deter_strength_stat, "Results symlink/deter_strength_stat.txt", sep="\t")


# GEN_MOD_FOOD
gen_mod_food_table <- table(all$gen_mod_food)
gen_mod_food_prop <- round(prop.table(gen_mod_food_table)*100,2)
gen_mod_food_stat <- data.frame(gen_mod_food_table, gen_mod_food_prop)
gen_mod_food_stat <- gen_mod_food_stat[,c(1,2,4)]
gen_mod_food_stat <- rename(gen_mod_food_stat,c("Var1"="response","Freq"="freq","Freq.1"="%"))
# Write results
write.csv(gen_mod_food_stat, "Results symlink/gen_mod_food_stat.csv")
write.table(gen_mod_food_stat, "Results symlink/gen_mod_food_stat.txt", sep="\t")




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


#EDUCATION(*COUNTRY)
edu_vec <- all$edu_level
edu_vec <- factor(edu_vec, levels = c(1,2,3,4,5,6),labels = c("None","Primary","High","Course","Undergrad","Postgrad"))
edu_country_table <- table(all$country, edu_vec)
edu_country_prop <- prop.table(edu_country_table,1)*100 # 1 specifies row margins (2 for columns I think)
edu_country_stat <- cbind(edu_country_table, edu_country_prop)
edu_country_stat[,7] <- round(edu_country_stat[,7],2)
edu_country_stat[,8] <- round(edu_country_stat[,8],2)
edu_country_stat[,9] <- round(edu_country_stat[,9],2)
edu_country_stat[,10] <- round(edu_country_stat[,10],2)
edu_country_stat[,11] <- round(edu_country_stat[,11],2)
edu_country_stat[,12] <- round(edu_country_stat[,12],2)
edu_country_stat
# Write results
write.csv(edu_country_stat, "Results symlink/edu_country_stat.csv")
write.table(edu_country_stat, "Results symlink/edu_country_stat.txt", sep="\t")


# GENETIC CONDITION(*COUNTRY)
gc_country_table <- table(all$country,all$genetic_cond)
gc_country_prop <- prop.table(gc_country_table,1)*100 # 1 specifies row margins (2 for columns I think)
gc_country_prop <- gc_country_prop[,1:2]
# Retrieve total country counts from above (need to reorder)
country_stat <- country_stat[order(country_stat$country),] 
gc_country_stat <- cbind(country_stat$freq, gc_country_table, gc_country_prop)
gc_country_stat <- data.frame(gc_country_stat)
gc_country_stat <- rename(gc_country_stat,c("V1"="Total","N"="N.freq","Y"="Y.freq","N.1"="N.prop","Y.1"="Y.prop"))
gc_country_stat$N.prop <- round(gc_country_stat$N.prop,2)
gc_country_stat$Y.prop <- round(gc_country_stat$Y.prop,2)
gc_country_stat
# Write results
write.csv(gc_country_stat, "Results symlink/gc_country_stat.csv")
write.table(gc_country_stat, "Results symlink/gc_country_stat.txt", sep="\t")


#COMPARISONS---------------------------------------------------------------------------------------

# Can do chi-square
# chisq.test(sex_ethnicity_mat)
# Or a proportions test - Prop.test function calculates the value of chi-square, given the values of success (in the vector x) and total attempts (in the vector n).
# F    M
# 616 1160
# 427  293
# prop.test(x=c(616,427),n=c(1776,720),correct=F)


# SEX DIFFS CAUCASIAN AND ASIAN
ethnic_vec <- all$ethnicity
ethnic_vec <- factor(ethnic_vec, levels = c(1,2,3,4,5,6,7,8,9,10,11),labels = c("Mixed Race","African/African American","Asian Indian","Caucasian (European)","Caucasian (Middle East)","Hispanic, Latino or Spanish","Indigenous Australian","Native American","North East Asian (Mongol, Tibetan, Korean, Japanese, etc)","Pacific (Polynesian, micronesian, etc)","South East Asian (Chinese, Thai, Malay, Filipino, etc)"))
sex_ethnicity_table <- table(ethnic_vec,all$sex)
# Save table rownames in a vector
sex_ethnicity_rn <- rownames(sex_ethnicity_table)
# Now set to NULL so they aren't duplicated when the table is converted to a df.
rownames(sex_ethnicity_table) <- NULL
# Convert table to df
sex_ethnicity_df <- as.data.frame.matrix(sex_ethnicity_table)
# Add rownames back in
sex_ethnicity_df <- cbind(sex_ethnicity_rn, sex_ethnicity_df)
# Rename first column (i.e. rownames)
colnames(sex_ethnicity_df)[1]="ethnicity"
sex_ethnicity_df
# Create separate dataframes for Caucasian and Asian
sex_ethnicity_df1 <- sex_ethnicity_df[which(sex_ethnicity_df$ethnicity == "Caucasian (European)" | sex_ethnicity_df$ethnicity == "Caucasian (Middle East)"), ]
sex_ethnicity_df1
sex_ethnicity_df2 <- sex_ethnicity_df[which(sex_ethnicity_df$ethnicity == "South East Asian (Chinese, Thai, Malay, Filipino, etc)" | sex_ethnicity_df$ethnicity == "North East Asian (Mongol, Tibetan, Korean, Japanese, etc)"), ]
sex_ethnicity_df2
# Sum columns and rbind two aggregate rows
sex_ethnicity_df <- rbind(colSums(sex_ethnicity_df1[,2:3]), colSums(sex_ethnicity_df2[,2:3]))
sex_ethnicity_df
# Convert to matrix
sex_ethnicity_mat <- as.matrix(sex_ethnicity_df)
prop.test(sex_ethnicity_mat,correct=F)
sex_ethnicity_prop.test <- prop.test(sex_ethnicity_mat,correct=F)
# Remove old file
file.remove("Results symlink/sex_ethnicity_prop.test_CaucasianVsAsian.txt")
# Write results
descrip <- "Proportions test comparing Males and Females in combined Caucasian (4)+(5) and Asian (9)+(11) samples. Sample estimates represent the proportions of females. Row 1 = Caucasian, Row 2 = Asian"
txt <-capture.output(descrip,file=NULL) # Print description
mat <-capture.output(sex_ethnicity_mat,file=NULL) # Print contingency table
out <-capture.output(sex_ethnicity_prop.test) # Print proportions test result
cat(txt,file="Results symlink/sex_ethnicity_prop.test_CaucasianVsAsian.txt",sep="\n",append=T)
cat(mat,file="Results symlink/sex_ethnicity_prop.test_CaucasianVsAsian.txt",sep="\n",append=T)
cat(out,file="Results symlink/sex_ethnicity_prop.test_CaucasianVsAsian.txt",sep="\n", append=T)


# SEX DIFFS CAUCASIAN AND CHINESE
ethnic_vec <- all$ethnicity
ethnic_vec <- factor(ethnic_vec, levels = c(1,2,3,4,5,6,7,8,9,10,11),labels = c("Mixed Race","African/African American","Asian Indian","Caucasian (European)","Caucasian (Middle East)","Hispanic, Latino or Spanish","Indigenous Australian","Native American","North East Asian (Mongol, Tibetan, Korean, Japanese, etc)","Pacific (Polynesian, micronesian, etc)","South East Asian (Chinese, Thai, Malay, Filipino, etc)"))
sex_ethnicity_table <- table(ethnic_vec,all$sex)
# Save table rownames in a vector
sex_ethnicity_rn <- rownames(sex_ethnicity_table)
# Now set to NULL so they aren't duplicated when the table is converted to a df.
rownames(sex_ethnicity_table) <- NULL
# Convert table to df
sex_ethnicity_df <- as.data.frame.matrix(sex_ethnicity_table)
# Add rownames back in
sex_ethnicity_df <- cbind(sex_ethnicity_rn, sex_ethnicity_df)
# Rename first column (i.e. rownames)
colnames(sex_ethnicity_df)[1]="ethnicity"
sex_ethnicity_df
# Create separate dataframes for Caucasian and Chinese
sex_ethnicity_df1 <- sex_ethnicity_df[which(sex_ethnicity_df$ethnicity == "Caucasian (European)" | sex_ethnicity_df$ethnicity == "Caucasian (Middle East)"), ]
sex_ethnicity_df1
sex_ethnicity_df2 <- sex_ethnicity_df[which(sex_ethnicity_df$ethnicity == "South East Asian (Chinese, Thai, Malay, Filipino, etc)"), ]
sex_ethnicity_df2
# Sum columns and rbind two aggregate rows
sex_ethnicity_df <- rbind(colSums(sex_ethnicity_df1[,2:3]), colSums(sex_ethnicity_df2[,2:3]))
sex_ethnicity_df
# Convert to matrix
sex_ethnicity_mat <- as.matrix(sex_ethnicity_df)
prop.test(sex_ethnicity_mat,correct=F)
sex_ethnicity_prop.test <- prop.test(sex_ethnicity_mat,correct=F)
# Remove old file
file.remove("Results symlink/sex_ethnicity_prop.test_CaucasianVsChinese.txt")
# Write results
descrip <- "Proportions test comparing Males and Females in combined Caucasian (4)+(5) and Chinese (11) samples. Sample estimates represent the proportions of females. Row 1 = Caucasian, Row 2 = Chinese"
txt <-capture.output(descrip,file=NULL) # Print description
mat <-capture.output(sex_ethnicity_mat,file=NULL) # Print contingency table
out <-capture.output(sex_ethnicity_prop.test) # Print proportions test result
cat(txt,file="Results symlink/sex_ethnicity_prop.test_CaucasianVsChinese.txt",sep="\n",append=T)
cat(mat,file="Results symlink/sex_ethnicity_prop.test_CaucasianVsChinese.txt",sep="\n",append=T)
cat(out,file="Results symlink/sex_ethnicity_prop.test_CaucasianVsChinese.txt",sep="\n", append=T)


# SEX DIFFS BY MOST FREQUENT 8 COUNTRIES
country_table <- table(all$country)
country_prop <- round(prop.table(country_table)*100,2)
country_stat <- data.frame(country_table, country_prop)
country_stat <- country_stat[,c(1,2,4)]
country_stat <- rename(country_stat,c("Var1"="country","Freq"="freq","Freq.1"="%"))
# Sort
country_stat <- country_stat[order(-country_stat$freq),] 
country_stat
top_countries <- as.character(country_stat$country[1:8]) #Selects top 8
country_vec <- all$country
sex_country_table <- table(country_vec,all$sex)
# Save table rownames in a vector
sex_country_rn <- rownames(sex_country_table)
# Now set to NULL so they aren't duplicated when the table is converted to a df.
rownames(sex_country_table) <- NULL
# Convert table to df
sex_country_df <- as.data.frame.matrix(sex_country_table)
# Add rownames back in
sex_country_df <- cbind(sex_country_rn, sex_country_df)
# Rename first column (i.e. rownames)
colnames(sex_country_df)[1]="country"
sex_country_df
# Create dataframe of most frequent countries
sex_country_df <- sex_country_df[which(sex_country_df$country == top_countries[1] | sex_country_df$country == top_countries[2] | sex_country_df$country == top_countries[3] | sex_country_df$country == top_countries[4] | sex_country_df$country == top_countries[5] | sex_country_df$country == top_countries[6] | sex_country_df$country == top_countries[7] | sex_country_df$country == top_countries[8]), ]
sex_country_df
# Convert to matrix
sex_country_mat <- cbind(sex_country_df[,2], sex_country_df[,3])
rnames <- as.character(sex_country_df[,1])
cnames <- c("F", "M")
colnames(sex_country_mat) <- cnames
rownames(sex_country_mat) <- rnames
(sex_country_chi <- chisq.test(sex_country_mat))
# Post hoc pairwise comparisons
(sex_country_chi_pairwise <- chisq.post.hoc(sex_country_mat))
# Remove old file
file.remove("Results symlink/sex_country_chi.test.txt")
# Write results
descrip <- "Chi-square test comparing males and females in the most responsive countries. Pairwise comparisons follow main result."
txt <-capture.output(descrip,file=NULL) # Print description
mat <-capture.output(sex_country_mat,file=NULL) # Print contingency table
out <-capture.output(sex_country_chi) # Print chi test result
out2 <-capture.output(sex_country_chi_pairwise) # Print pairwise results
cat(txt,file="Results symlink/sex_country_chi.test.txt",sep="\n",append=T)
cat(mat,file="Results symlink/sex_country_chi.test.txt",sep="\n",append=T)
cat(out,file="Results symlink/sex_country_chi.test.txt",sep="\n", append=T)
cat(out2,file="Results symlink/sex_country_chi.test.txt",sep="\n", append=T)


# AGE DIFFS CAUCASIAN AND ASIAN
age_stat <- data.frame(age=all$age, group=all$ethnicity)
age_stat <- subset(age_stat, group == 4 | group == 5 | group == 9 | group == 11)
# Recode
age_stat$group <- recode(age_stat$group,
'"4" = "Caucasian";
"5" = "Caucasian";
"9" = "Asian";
"11" = "Asian"')
levels(age_stat$group)
# t-test for diffs in age between groups
age_ethnicity_ttest_CaucasianVsAsian <- t.test(age ~ group, data = age_stat)
# Exclude those below 19 years
age_stat <- subset(age_stat, age > 18)
age_ethnicity_ttest_CaucasianVsAsian19 <- t.test(age ~ group, data = age_stat)
# Remove old file
file.remove("Results symlink/age_ethnicity_t.test_CaucasianVsAsian.txt")
# Write results
descrip <- "t-test comparing age in combined Caucasian (4)+(5) and Asian (9)+(11) samples."
txt <-capture.output(descrip,file=NULL) # Print description
out <-capture.output(age_ethnicity_ttest_CaucasianVsAsian) # Print test result
cat(txt,file="Results symlink/age_ethnicity_t.test_CaucasianVsAsian.txt",sep="\n",append=T)
cat(out,file="Results symlink/age_ethnicity_t.test_CaucasianVsAsian.txt",sep="\n", append=T)
descrip2 <- "t-test comparing age (those aged 19+) in combined Caucasian (4)+(5) and Asian (9)+(11) samples."
txt2 <-capture.output(descrip2,file=NULL) # Print description
out2 <-capture.output(age_ethnicity_ttest_CaucasianVsAsian19) # Print test result
cat(txt2,file="Results symlink/age_ethnicity_t.test_CaucasianVsAsian.txt",sep="\n",append=T)
cat(out2,file="Results symlink/age_ethnicity_t.test_CaucasianVsAsian.txt",sep="\n", append=T)


# AGE DIFFS CAUCASIAN AND CHINESE
age_stat <- data.frame(age=all$age, group=all$ethnicity)
age_stat <- subset(age_stat, group == 4 | group == 5 | group == 11)
# Recode
age_stat$group <- recode(age_stat$group,
'"4" = "Caucasian";
"5" = "Caucasian";
"11" = "Chinese"')
levels(age_stat$group)
# t-test for diffs in age between groups
age_ethnicity_ttest_CaucasianVsChinese <- t.test(age ~ group, data = age_stat)
# Exclude those below 19 years
age_stat <- subset(age_stat, age > 18)
age_ethnicity_ttest_CaucasianVsChinese19 <- t.test(age ~ group, data = age_stat)
# Remove old file
file.remove("Results symlink/age_ethnicity_t.test_CaucasianVsChinese.txt")
# Write results
descrip <- "t-test comparing age in combined Caucasian (4)+(5) and Chinese (11) samples."
txt <-capture.output(descrip,file=NULL) # Print description
out <-capture.output(age_ethnicity_ttest_CaucasianVsChinese) # Print test result
cat(txt,file="Results symlink/age_ethnicity_t.test_CaucasianVsChinese.txt",sep="\n",append=T)
cat(out,file="Results symlink/age_ethnicity_t.test_CaucasianVsChinese.txt",sep="\n", append=T)
descrip2 <- "t-test comparing age (those aged 19+) in combined Caucasian (4)+(5) and Chinese (11) samples."
txt2 <-capture.output(descrip2,file=NULL) # Print description
out2 <-capture.output(age_ethnicity_ttest_CaucasianVsChinese19) # Print test result
cat(txt2,file="Results symlink/age_ethnicity_t.test_CaucasianVsChinese.txt",sep="\n",append=T)
cat(out2,file="Results symlink/age_ethnicity_t.test_CaucasianVsChinese.txt",sep="\n", append=T)


# AGE DIFFS BY MOST FREQUENT 8 COUNTRIES
country_table <- table(all$country)
country_prop <- round(prop.table(country_table)*100,2)
country_stat <- data.frame(country_table, country_prop)
country_stat <- country_stat[,c(1,2,4)]
country_stat <- rename(country_stat,c("Var1"="country","Freq"="freq","Freq.1"="%"))
# Sort
country_stat <- country_stat[order(-country_stat$freq),] 
country_stat
top_countries <- as.character(country_stat$country[1:8]) #Selects top 8
# Create dataframe of most frequent countries
age_country <- data.frame(age=all$age, group=all$country)
(age_country <- subset(age_country, group == top_countries[1] | group == top_countries[2] | group == top_countries[3] | group == top_countries[4] | group == top_countries[5] | group == top_countries[6] | group == top_countries[7] | group == top_countries[8]))
# Change group to character to remove redundant factor levels
age_country$group <- as.character(age_country$group)
# Mean and SDs
(age_country_mean <- tapply(age_country$age, age_country$group, mean))
(age_country_sd <- tapply(age_country$age, age_country$group, sd))
# Anova
age_country_anova <- aov(age_country$age ~ age_country$group)
(age_country_anova_sum <- summary(age_country_anova))
# Post hoc pairwise comparisons
(age_country_anova_pairwise <- pairwise.t.test(age_country$age, age_country$group, p.adj = "bonferroni"))
# Remove old file
file.remove("Results symlink/age_country_anova.txt")
# Write results
descrip <- "Anova comparing males and females in the most responsive countries. Pairwise comparisons follow main result."
txt <-capture.output(descrip,file=NULL) # Print description
out <-capture.output(age_country_anova_sum) # Print test result
out2 <-capture.output(age_country_anova_pairwise) # Print pairwise results
cat(txt,file="Results symlink/age_country_anova.txt",sep="\n",append=T)
cat(out,file="Results symlink/age_country_anova.txt",sep="\n", append=T)
cat(out2,file="Results symlink/age_country_anova.txt",sep="\n", append=T)
descrip2 <- "Means"
txt2 <-capture.output(descrip2,file=NULL)
out3 <-capture.output(age_country_mean) 
descrip3 <- "Standard Deviations"
txt3 <-capture.output(descrip3,file=NULL)
out4 <-capture.output(age_country_sd)
cat(txt2,file="Results symlink/age_country_anova.txt",sep="\n",append=T)
cat(out3,file="Results symlink/age_country_anova.txt",sep="\n", append=T)
cat(txt3,file="Results symlink/age_country_anova.txt",sep="\n",append=T)
cat(out4,file="Results symlink/age_country_anova.txt",sep="\n", append=T)



#MULTINOMIAL_REGRESSION---------------------------------------------------------------------------------------

# Rescale GDP (a 1 unit change in GDP = $1000)
all$GDP <- all$GDP/1000


# KIDS_CURE_LIFE
# Duplicate main dataset before collapsing likert levels
all_LR <- all
# Remove NAs from dataset (keep relevant variables only to minimise data loss), otherwise can't do LR test of different models
all_LR <- all_LR[c(8,10:14,16:17,20:22,31)]
# Remove NAs from "religion" - now all NAs in "religion_type" indicate that person has no religion.
all_LR<-subset(all_LR,!(is.na(all_LR["religion"])))
# Recode religion and set Christian as the comparison category
all_LR$religion_type <- recode(all_LR$religion_type,
'"1" = "Christian";
"2" = "Christian";
"3" = "Christian";
"4" = "Muslim";
NA = "None";
else = "Other"')
levels(all_LR$religion_type)
all_LR$religion_type <- relevel(all_LR$religion_type, "None")
all_LR <- na.omit(all_LR)
# Recode dependent variable (collapse 6 levels to 3)
all_LR$kids_cure_life <- recode(all_LR$kids_cure_life,
'"1" = "1(agree)";
"2" = "1(agree)";
"3" = "2(neutral)";
"4" = "3(disagree)";
"5" = "3(disagree)";
"6" = "2(neutral)";')
all_LR$kids_cure_life <- ordered(all_LR$kids_cure_life)
levels(all_LR$kids_cure_life)
# Recode ethnicity and set Caucasian as the comparison category
all_LR$ethnicity <- recode(all_LR$ethnicity,
'"4" = "Caucasian";
"5" = "Caucasian";
"9" = "Asian";
"11" = "Asian";
NA = NA;
else = "Other"')
levels(all_LR$ethnicity)
all_LR$ethnicity <- relevel(all_LR$ethnicity, "Caucasian")
# Recode education and set None as the comparison category
all_LR$edu_level <- recode(all_LR$edu_level,
'"1" = "None";
"2" = "School";
"3" = "School";
"4" = "School";
NA = NA;
else = "Tertiary"')
levels(all_LR$edu_level)
all_LR$edu_level <- relevel(all_LR$edu_level, "None")
# Recode wealth and set Below as the comparison category
all_LR$wealth <- recode(all_LR$wealth,
'"1" = "Below";
"2" = "Average";
"3" = "Above"')
levels(all_LR$wealth)
all_LR$wealth <- relevel(all_LR$wealth, "Below")
# Recode heard_about and set Never as the comparison category
all_LR$heard_about <- recode(all_LR$heard_about,
'"1" = "Never";
"2" = "Little";
"3" = "Lot"')
levels(all_LR$heard_about)
all_LR$heard_about <- relevel(all_LR$heard_about, "Never")

# Multinomial Model
# Fit and compare models
fit <- multinom(kids_cure_life ~ 1, data = all_LR) # Intercept only
fit2 <- multinom(kids_cure_life ~ sex, data = all_LR)
fit3 <- multinom(kids_cure_life ~ sex + age, data = all_LR)
fit4 <- multinom(kids_cure_life ~ sex + age + ethnicity, data = all_LR)
fit5 <- multinom(kids_cure_life ~ sex + age + ethnicity + GDP, data = all_LR)
fit6 <- multinom(kids_cure_life ~ sex + age + ethnicity + GDP + heard_about, data = all_LR)
fit7 <- multinom(kids_cure_life ~ sex + age + ethnicity + GDP + heard_about + edu_level, data = all_LR)
fit8 <- multinom(kids_cure_life ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type, data = all_LR)
fit9 <- multinom(kids_cure_life ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth, data = all_LR)
fit10 <- multinom(kids_cure_life ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth + worked_health, data = all_LR)
fit11 <- multinom(kids_cure_life ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth + worked_health + genetic_cond, data = all_LR)

# Compare
best_mod <- anova(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11)

# Create vector of AIC values
AIC <- c(fit$AIC,fit2$AIC,fit3$AIC,fit4$AIC,fit5$AIC,fit6$AIC,fit7$AIC,fit8$AIC,fit9$AIC,fit10$AIC,fit11$AIC)
best_mod <- cbind(best_mod, AIC)
(best_mod <- best_mod[c("Model", "Resid. df", "Resid. Dev", "AIC", "Test", "   Df", "LR stat.", "Pr(Chi)")])

# Find model with lowest AIC and automatically assign
nametmp <- paste("fit",which.min(AIC),sep="")
nametmp <- eval(parse(text = nametmp))
best_mod_kids_cure_life <- nametmp
# CIs and ORs of model with lowest AIC
best_mod_sum <- summary(best_mod_kids_cure_life)
coef <- coef(best_mod_kids_cure_life)
coef_ci <- confint(best_mod_kids_cure_life)
OR <- exp(coef)
OR_ci <- exp(coef_ci)
z <- summary(best_mod_kids_cure_life)$coefficients/summary(best_mod_kids_cure_life)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# Sequence vectors to recreate coef and OR tables
# Coefficients
coef_neutral_seq <- seq(1,length(coef),2)
coef_neutral <- coef[coef_neutral_seq]
coef_disagree_seq <- seq(2,length(coef),2)
coef_disagree <- coef[coef_disagree_seq]
coef_neutral_025 <- coef_ci[1:(length(coef_ci)/4)]
coef_neutral_975 <- coef_ci[(length(coef_ci)/4)+1:(length(coef_ci)/4)]
coef_disagree_025 <- coef_ci[(length(coef_ci)/2)+1:(length(coef_ci)/4)]
coef_disagree_975 <- coef_ci[((length(coef_ci)/4)*3)+1:(length(coef_ci)/4)]
# Odds Ratios
OR_neutral_seq <- seq(1,length(OR),2)
OR_neutral <- OR[OR_neutral_seq]
OR_disagree_seq <- seq(2,length(OR),2)
OR_disagree <- OR[OR_disagree_seq]
OR_neutral_025 <- OR_ci[1:(length(OR_ci)/4)]
OR_neutral_975 <- OR_ci[(length(OR_ci)/4)+1:(length(OR_ci)/4)]
OR_disagree_025 <- OR_ci[(length(OR_ci)/2)+1:(length(OR_ci)/4)]
OR_disagree_975 <- OR_ci[((length(OR_ci)/4)*3)+1:(length(OR_ci)/4)]
# p values
p_neutral <- p[coef_neutral_seq]
p_disagree <- p[coef_disagree_seq]

# Assemble dataframes of coefficients and odds ratios for neutral and disagree categories
pred_names <- colnames(coef)
best_mod_neutral <- cbind(coef_neutral, coef_neutral_025, coef_neutral_975, OR_neutral, OR_neutral_025, OR_neutral_975, p_neutral)
best_mod_neutral <- as.data.frame(best_mod_neutral)
best_mod_neutral <- cbind(pred_names, best_mod_neutral)
(best_mod_neutral_coef <- best_mod_neutral[,c(1:4,8)])
(best_mod_neutral_OR <- best_mod_neutral[,c(1,5:8)])
best_mod_disagree <- cbind(coef_disagree, coef_disagree_025, coef_disagree_975, OR_disagree, OR_disagree_025, OR_disagree_975, p_disagree)
best_mod_disagree <- as.data.frame(best_mod_disagree)
best_mod_disagree <- cbind(pred_names, best_mod_disagree)
(best_mod_disagree_coef <- best_mod_disagree[,c(1:4,8)])
(best_mod_disagree_OR <- best_mod_disagree[,c(1,5:8)])

# Remove old file
file.remove("Results symlink/MultReg_kidscurelife.txt")
# Write results
newline <- "----------------------------------------------------------------------------------------"
sep <-capture.output(newline,file=NULL) # Print description

descrip <- "Likelihood ratio tests comparing different models"
txt <-capture.output(descrip,file=NULL) # Print description
out <-capture.output(best_mod) # Print test result
cat(txt,file="Results symlink/MultReg_kidscurelife.txt",sep="\n",append=T)
cat(out,file="Results symlink/MultReg_kidscurelife.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_kidscurelife.txt",sep="\n",append=T)

descrip2 <- "Coefficients - Summary of model with lowest AIC - Agree -> Neutral"
txt2 <-capture.output(descrip2,file=NULL) # Print description
out2 <-capture.output(best_mod_neutral_coef) # Print test result
cat(txt2,file="Results symlink/MultReg_kidscurelife.txt",sep="\n",append=T)
cat(out2,file="Results symlink/MultReg_kidscurelife.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_kidscurelife.txt",sep="\n",append=T)

descrip3 <- "Odds Ratios - Summary of model with lowest AIC - Agree -> Neutral"
txt3 <-capture.output(descrip3,file=NULL) # Print description
out3 <-capture.output(best_mod_neutral_OR) # Print test result
cat(txt3,file="Results symlink/MultReg_kidscurelife.txt",sep="\n",append=T)
cat(out3,file="Results symlink/MultReg_kidscurelife.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_kidscurelife.txt",sep="\n",append=T)

descrip4 <- "Coefficients - Summary of model with lowest AIC - Agree -> Disagree"
txt4 <-capture.output(descrip4,file=NULL) # Print description
out4 <-capture.output(best_mod_disagree_coef) # Print test result
cat(txt4,file="Results symlink/MultReg_kidscurelife.txt",sep="\n",append=T)
cat(out4,file="Results symlink/MultReg_kidscurelife.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_kidscurelife.txt",sep="\n",append=T)

descrip5 <- "Odds Ratios - Summary of model with lowest AIC - Agree -> Disagree"
txt5 <-capture.output(descrip5,file=NULL) # Print description
out5 <-capture.output(best_mod_disagree_OR) # Print test result
cat(txt5,file="Results symlink/MultReg_kidscurelife.txt",sep="\n",append=T)
cat(out5,file="Results symlink/MultReg_kidscurelife.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_kidscurelife.txt",sep="\n",append=T)

# # Ordinal Model
# # Fit and compare models
# fit <- clm(kids_cure_life ~ 1, data = all_LR) # Intercept only
# fit2 <- clm(kids_cure_life ~ sex, data = all_LR)
# fit3 <- clm(kids_cure_life ~ sex + age, data = all_LR)
# fit4 <- clm(kids_cure_life ~ sex + age + ethnicity, data = all_LR)
# fit5 <- clm(kids_cure_life ~ sex + age + ethnicity + heard_about, data = all_LR)
# fit6 <- clm(kids_cure_life ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR)
# fit7 <- clm(kids_cure_life ~ sex + age + ethnicity + heard_about + edu_level + religion_type, data = all_LR)
# fit8 <- clm(kids_cure_life ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth, data = all_LR)
# fit9 <- clm(kids_cure_life ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth + worked_health, data = all_LR)
# fit10 <- clm(kids_cure_life ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth + worked_health + genetic_cond, data = all_LR)
# # Compare
# (best_mod <- anova(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10))
# 
# # NEED TO CHANGE MODEL NUMBER
# best_mod_kids_cure_life <- fit10
# # CIs and ORs of model with lowest AIC
# (best_mod_sum <- summary(best_mod_kids_cure_life))
# (ci <- confint(best_mod_kids_cure_life))
# OR <- coef(best_mod_kids_cure_life)
# OR <- OR[-1:-2] # Remove first 2 values which are exponentiates thresholds (i.e. agree -> neutral and neutral -> disagree)
# (best_mod_sumORs <- exp(cbind(OR, ci)))
# 
# # # NEED TO CHANGE MODEL NUMBER
# # # Proportional Odds Assumption
# # # Comparison of all IVs assumed to have parallel slopes vs all allowed to vary
# # fit6a <- vglm(kids_cure_life ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR, family=cumulative(parallel=TRUE))
# # fit6b <- vglm(kids_cure_life ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR, family=cumulative(parallel=FALSE))
# # lrtest(fit6a,fit6b)
# # 
# # # Comparison of all IVs assumed to have parallel slopes vs only heard_about allowed to vary
# # fit6a <- vglm(kids_cure_life ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR, family=cumulative(parallel=TRUE))
# # fit6b <- vglm(kids_cure_life ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR, family=cumulative(parallel=FALSE~heard_about))
# # lrtest(fit6a,fit6b)
# # 
# # # Same thing using the clm package - only heard_about allowed to vary
# # fit6a <- clm(kids_cure_life ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR)
# # fit6b <- clm(kids_cure_life ~ sex + age + ethnicity + edu_level, nominal=~heard_about, data = all_LR)
# # anova(fit6a,fit6b)


# KIDS_CURE_DEBIL
# Duplicate main dataset before collapsing likert levels
all_LR <- all
# Remove NAs from dataset (keep relevant variables only to minimise data loss), otherwise can't do LR test of different models
all_LR <- all_LR[c(8,10:14,16:17,20:21,23,31)]
# Remove NAs from "religion" - now all NAs in "religion_type" indicate that person has no religion.
all_LR<-subset(all_LR,!(is.na(all_LR["religion"])))
# Recode religion and set Christian as the comparison category
all_LR$religion_type <- recode(all_LR$religion_type,
'"1" = "Christian";
"2" = "Christian";
"3" = "Christian";
"4" = "Muslim";
NA = "None";
else = "Other"')
levels(all_LR$religion_type)
all_LR$religion_type <- relevel(all_LR$religion_type, "None")
all_LR <- na.omit(all_LR)
# Recode dependent variable (collapse 6 levels to 3)
all_LR$kids_cure_debil <- recode(all_LR$kids_cure_debil,
'"1" = "1(agree)";
"2" = "1(agree)";
"3" = "2(neutral)";
"4" = "3(disagree)";
"5" = "3(disagree)";
"6" = "2(neutral)";')
all_LR$kids_cure_debil <- ordered(all_LR$kids_cure_debil)
levels(all_LR$kids_cure_debil)
# Recode ethnicity and set Caucasian as the comparison category
all_LR$ethnicity <- recode(all_LR$ethnicity,
'"4" = "Caucasian";
"5" = "Caucasian";
"9" = "Asian";
"11" = "Asian";
NA = NA;
else = "Other"')
levels(all_LR$ethnicity)
all_LR$ethnicity <- relevel(all_LR$ethnicity, "Caucasian")
# Recode education and set None as the comparison category
all_LR$edu_level <- recode(all_LR$edu_level,
'"1" = "None";
"2" = "School";
"3" = "School";
"4" = "School";
NA = NA;
else = "Tertiary"')
levels(all_LR$edu_level)
all_LR$edu_level <- relevel(all_LR$edu_level, "None")
# Recode wealth and set Below as the comparison category
all_LR$wealth <- recode(all_LR$wealth,
'"1" = "Below";
"2" = "Average";
"3" = "Above"')
levels(all_LR$wealth)
all_LR$wealth <- relevel(all_LR$wealth, "Below")
# Recode heard_about and set Never as the comparison category
all_LR$heard_about <- recode(all_LR$heard_about,
'"1" = "Never";
"2" = "Little";
"3" = "Lot"')
levels(all_LR$heard_about)
all_LR$heard_about <- relevel(all_LR$heard_about, "Never")

# Multinomial Model
# Fit and compare models
fit <- multinom(kids_cure_debil ~ 1, data = all_LR) # Intercept only
fit2 <- multinom(kids_cure_debil ~ sex, data = all_LR)
fit3 <- multinom(kids_cure_debil ~ sex + age, data = all_LR)
fit4 <- multinom(kids_cure_debil ~ sex + age + ethnicity, data = all_LR)
fit5 <- multinom(kids_cure_debil ~ sex + age + ethnicity + GDP, data = all_LR)
fit6 <- multinom(kids_cure_debil ~ sex + age + ethnicity + GDP + heard_about, data = all_LR)
fit7 <- multinom(kids_cure_debil ~ sex + age + ethnicity + GDP + heard_about + edu_level, data = all_LR)
fit8 <- multinom(kids_cure_debil ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type, data = all_LR)
fit9 <- multinom(kids_cure_debil ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth, data = all_LR)
fit10 <- multinom(kids_cure_debil ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth + worked_health, data = all_LR)
fit11 <- multinom(kids_cure_debil ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth + worked_health + genetic_cond, data = all_LR)

# Compare
best_mod <- anova(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11)

# Create vector of AIC values
AIC <- c(fit$AIC,fit2$AIC,fit3$AIC,fit4$AIC,fit5$AIC,fit6$AIC,fit7$AIC,fit8$AIC,fit9$AIC,fit10$AIC,fit11$AIC)
best_mod <- cbind(best_mod, AIC)
(best_mod <- best_mod[c("Model", "Resid. df", "Resid. Dev", "AIC", "Test", "   Df", "LR stat.", "Pr(Chi)")])

# Find model with lowest AIC and automatically assign
nametmp <- paste("fit",which.min(AIC),sep="")
nametmp <- eval(parse(text = nametmp))
best_mod_kids_cure_debil <- nametmp
# CIs and ORs of model with lowest AIC
best_mod_sum <- summary(best_mod_kids_cure_debil)
coef <- coef(best_mod_kids_cure_debil)
coef_ci <- confint(best_mod_kids_cure_debil)
OR <- exp(coef)
OR_ci <- exp(coef_ci)
z <- summary(best_mod_kids_cure_debil)$coefficients/summary(best_mod_kids_cure_debil)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# Sequence vectors to recreate coef and OR tables
# Coefficients
coef_neutral_seq <- seq(1,length(coef),2)
coef_neutral <- coef[coef_neutral_seq]
coef_disagree_seq <- seq(2,length(coef),2)
coef_disagree <- coef[coef_disagree_seq]
coef_neutral_025 <- coef_ci[1:(length(coef_ci)/4)]
coef_neutral_975 <- coef_ci[(length(coef_ci)/4)+1:(length(coef_ci)/4)]
coef_disagree_025 <- coef_ci[(length(coef_ci)/2)+1:(length(coef_ci)/4)]
coef_disagree_975 <- coef_ci[((length(coef_ci)/4)*3)+1:(length(coef_ci)/4)]
# Odds Ratios
OR_neutral_seq <- seq(1,length(OR),2)
OR_neutral <- OR[OR_neutral_seq]
OR_disagree_seq <- seq(2,length(OR),2)
OR_disagree <- OR[OR_disagree_seq]
OR_neutral_025 <- OR_ci[1:(length(OR_ci)/4)]
OR_neutral_975 <- OR_ci[(length(OR_ci)/4)+1:(length(OR_ci)/4)]
OR_disagree_025 <- OR_ci[(length(OR_ci)/2)+1:(length(OR_ci)/4)]
OR_disagree_975 <- OR_ci[((length(OR_ci)/4)*3)+1:(length(OR_ci)/4)]
# p values
p_neutral <- p[coef_neutral_seq]
p_disagree <- p[coef_disagree_seq]

# Assemble dataframes of coefficients and odds ratios for neutral and disagree categories
pred_names <- colnames(coef)
best_mod_neutral <- cbind(coef_neutral, coef_neutral_025, coef_neutral_975, OR_neutral, OR_neutral_025, OR_neutral_975, p_neutral)
best_mod_neutral <- as.data.frame(best_mod_neutral)
best_mod_neutral <- cbind(pred_names, best_mod_neutral)
(best_mod_neutral_coef <- best_mod_neutral[,c(1:4,8)])
(best_mod_neutral_OR <- best_mod_neutral[,c(1,5:8)])
best_mod_disagree <- cbind(coef_disagree, coef_disagree_025, coef_disagree_975, OR_disagree, OR_disagree_025, OR_disagree_975, p_disagree)
best_mod_disagree <- as.data.frame(best_mod_disagree)
best_mod_disagree <- cbind(pred_names, best_mod_disagree)
(best_mod_disagree_coef <- best_mod_disagree[,c(1:4,8)])
(best_mod_disagree_OR <- best_mod_disagree[,c(1,5:8)])

# Remove old file
file.remove("Results symlink/MultReg_kidscuredebil.txt")
# Write results
newline <- "----------------------------------------------------------------------------------------"
sep <-capture.output(newline,file=NULL) # Print description

descrip <- "Likelihood ratio tests comparing different models"
txt <-capture.output(descrip,file=NULL) # Print description
out <-capture.output(best_mod) # Print test result
cat(txt,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n",append=T)
cat(out,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n",append=T)

descrip2 <- "Coefficients - Summary of model with lowest AIC - Agree -> Neutral"
txt2 <-capture.output(descrip2,file=NULL) # Print description
out2 <-capture.output(best_mod_neutral_coef) # Print test result
cat(txt2,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n",append=T)
cat(out2,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n",append=T)

descrip3 <- "Odds Ratios - Summary of model with lowest AIC - Agree -> Neutral"
txt3 <-capture.output(descrip3,file=NULL) # Print description
out3 <-capture.output(best_mod_neutral_OR) # Print test result
cat(txt3,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n",append=T)
cat(out3,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n",append=T)

descrip4 <- "Coefficients - Summary of model with lowest AIC - Agree -> Disagree"
txt4 <-capture.output(descrip4,file=NULL) # Print description
out4 <-capture.output(best_mod_disagree_coef) # Print test result
cat(txt4,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n",append=T)
cat(out4,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n",append=T)

descrip5 <- "Odds Ratios - Summary of model with lowest AIC - Agree -> Disagree"
txt5 <-capture.output(descrip5,file=NULL) # Print description
out5 <-capture.output(best_mod_disagree_OR) # Print test result
cat(txt5,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n",append=T)
cat(out5,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_kidscuredebil.txt",sep="\n",append=T)

# # Ordinal Model
# # Fit and compare models
# fit <- clm(kids_cure_debil ~ 1, data = all_LR) # Intercept only
# fit2 <- clm(kids_cure_debil ~ sex, data = all_LR)
# fit3 <- clm(kids_cure_debil ~ sex + age, data = all_LR)
# fit4 <- clm(kids_cure_debil ~ sex + age + ethnicity, data = all_LR)
# fit5 <- clm(kids_cure_debil ~ sex + age + ethnicity + heard_about, data = all_LR)
# fit6 <- clm(kids_cure_debil ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR)
# fit7 <- clm(kids_cure_debil ~ sex + age + ethnicity + heard_about + edu_level + religion_type, data = all_LR)
# fit8 <- clm(kids_cure_debil ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth, data = all_LR)
# fit9 <- clm(kids_cure_debil ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth + worked_health, data = all_LR)
# fit10 <- clm(kids_cure_debil ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth + worked_health + genetic_cond, data = all_LR)
# # Compare
# (best_mod <- anova(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10))
# 
# # NEED TO CHANGE MODEL NUMBER
# best_mod_kids_cure_debil <- fit7
# # CIs and ORs of model with lowest AIC
# (best_mod_sum <- summary(best_mod_kids_cure_debil))
# (ci <- confint(best_mod_kids_cure_debil))
# OR <- coef(best_mod_kids_cure_debil)
# OR <- OR[-1:-2] # Remove first 2 values which are exponentiates thresholds (i.e. agree -> neutral and neutral -> disagree)
# (best_mod_sumORs <- exp(cbind(OR, ci)))
# 
# # # Proportional Odds Assumption
# # fit6a <- vglm(kids_cure_debil ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR, family=cumulative(parallel=TRUE))
# # fit6b <- vglm(kids_cure_debil ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR, family=cumulative(parallel=FALSE))
# # lrtest(fit6a,fit6b)


# EMBR_PREV_LIFE
# Duplicate main dataset before collapsing likert levels
all_LR <- all
# Remove NAs from dataset (keep relevant variables only to minimise data loss), otherwise can't do LR test of different models
all_LR <- all_LR[c(8,10:14,16:17,20:21,24,31)]
# Remove NAs from "religion" - now all NAs in "religion_type" indicate that person has no religion.
all_LR<-subset(all_LR,!(is.na(all_LR["religion"])))
# Recode religion and set Christian as the comparison category
all_LR$religion_type <- recode(all_LR$religion_type,
'"1" = "Christian";
"2" = "Christian";
"3" = "Christian";
"4" = "Muslim";
NA = "None";
else = "Other"')
levels(all_LR$religion_type)
all_LR$religion_type <- relevel(all_LR$religion_type, "None")
all_LR <- na.omit(all_LR)
# Recode dependent variable (collapse 6 levels to 3)
all_LR$embr_prev_life <- recode(all_LR$embr_prev_life,
'"1" = "1(agree)";
"2" = "1(agree)";
"3" = "2(neutral)";
"4" = "3(disagree)";
"5" = "3(disagree)";
"6" = "2(neutral)";')
all_LR$embr_prev_life <- ordered(all_LR$embr_prev_life)
levels(all_LR$embr_prev_life)
# Recode ethnicity and set Caucasian as the comparison category
all_LR$ethnicity <- recode(all_LR$ethnicity,
'"4" = "Caucasian";
"5" = "Caucasian";
"9" = "Asian";
"11" = "Asian";
NA = NA;
else = "Other"')
levels(all_LR$ethnicity)
all_LR$ethnicity <- relevel(all_LR$ethnicity, "Caucasian")
# Recode education and set None as the comparison category
all_LR$edu_level <- recode(all_LR$edu_level,
'"1" = "None";
"2" = "School";
"3" = "School";
"4" = "School";
NA = NA;
else = "Tertiary"')
levels(all_LR$edu_level)
all_LR$edu_level <- relevel(all_LR$edu_level, "None")
# Recode wealth and set Below as the comparison category
all_LR$wealth <- recode(all_LR$wealth,
'"1" = "Below";
"2" = "Average";
"3" = "Above"')
levels(all_LR$wealth)
all_LR$wealth <- relevel(all_LR$wealth, "Below")
# Recode heard_about and set Never as the comparison category
all_LR$heard_about <- recode(all_LR$heard_about,
'"1" = "Never";
"2" = "Little";
"3" = "Lot"')
levels(all_LR$heard_about)
all_LR$heard_about <- relevel(all_LR$heard_about, "Never")

# Multinomial Model
# Fit and compare models
fit <- multinom(embr_prev_life ~ 1, data = all_LR) # Intercept only
fit2 <- multinom(embr_prev_life ~ sex, data = all_LR)
fit3 <- multinom(embr_prev_life ~ sex + age, data = all_LR)
fit4 <- multinom(embr_prev_life ~ sex + age + ethnicity, data = all_LR)
fit5 <- multinom(embr_prev_life ~ sex + age + ethnicity + GDP, data = all_LR)
fit6 <- multinom(embr_prev_life ~ sex + age + ethnicity + GDP + heard_about, data = all_LR)
fit7 <- multinom(embr_prev_life ~ sex + age + ethnicity + GDP + heard_about + edu_level, data = all_LR)
fit8 <- multinom(embr_prev_life ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type, data = all_LR)
fit9 <- multinom(embr_prev_life ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth, data = all_LR)
fit10 <- multinom(embr_prev_life ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth + worked_health, data = all_LR)
fit11 <- multinom(embr_prev_life ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth + worked_health + genetic_cond, data = all_LR)

# Compare
best_mod <- anova(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11)

# Create vector of AIC values
AIC <- c(fit$AIC,fit2$AIC,fit3$AIC,fit4$AIC,fit5$AIC,fit6$AIC,fit7$AIC,fit8$AIC,fit9$AIC,fit10$AIC,fit11$AIC)
best_mod <- cbind(best_mod, AIC)
(best_mod <- best_mod[c("Model", "Resid. df", "Resid. Dev", "AIC", "Test", "   Df", "LR stat.", "Pr(Chi)")])

# Find model with lowest AIC and automatically assign
nametmp <- paste("fit",which.min(AIC),sep="")
nametmp <- eval(parse(text = nametmp))
best_mod_embr_prev_life <- nametmp
# CIs and ORs of model with lowest AIC
best_mod_sum <- summary(best_mod_embr_prev_life)
coef <- coef(best_mod_embr_prev_life)
coef_ci <- confint(best_mod_embr_prev_life)
OR <- exp(coef)
OR_ci <- exp(coef_ci)
z <- summary(best_mod_embr_prev_life)$coefficients/summary(best_mod_embr_prev_life)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# Sequence vectors to recreate coef and OR tables
# Coefficients
coef_neutral_seq <- seq(1,length(coef),2)
coef_neutral <- coef[coef_neutral_seq]
coef_disagree_seq <- seq(2,length(coef),2)
coef_disagree <- coef[coef_disagree_seq]
coef_neutral_025 <- coef_ci[1:(length(coef_ci)/4)]
coef_neutral_975 <- coef_ci[(length(coef_ci)/4)+1:(length(coef_ci)/4)]
coef_disagree_025 <- coef_ci[(length(coef_ci)/2)+1:(length(coef_ci)/4)]
coef_disagree_975 <- coef_ci[((length(coef_ci)/4)*3)+1:(length(coef_ci)/4)]
# Odds Ratios
OR_neutral_seq <- seq(1,length(OR),2)
OR_neutral <- OR[OR_neutral_seq]
OR_disagree_seq <- seq(2,length(OR),2)
OR_disagree <- OR[OR_disagree_seq]
OR_neutral_025 <- OR_ci[1:(length(OR_ci)/4)]
OR_neutral_975 <- OR_ci[(length(OR_ci)/4)+1:(length(OR_ci)/4)]
OR_disagree_025 <- OR_ci[(length(OR_ci)/2)+1:(length(OR_ci)/4)]
OR_disagree_975 <- OR_ci[((length(OR_ci)/4)*3)+1:(length(OR_ci)/4)]
# p values
p_neutral <- p[coef_neutral_seq]
p_disagree <- p[coef_disagree_seq]

# Assemble dataframes of coefficients and odds ratios for neutral and disagree categories
pred_names <- colnames(coef)
best_mod_neutral <- cbind(coef_neutral, coef_neutral_025, coef_neutral_975, OR_neutral, OR_neutral_025, OR_neutral_975, p_neutral)
best_mod_neutral <- as.data.frame(best_mod_neutral)
best_mod_neutral <- cbind(pred_names, best_mod_neutral)
(best_mod_neutral_coef <- best_mod_neutral[,c(1:4,8)])
(best_mod_neutral_OR <- best_mod_neutral[,c(1,5:8)])
best_mod_disagree <- cbind(coef_disagree, coef_disagree_025, coef_disagree_975, OR_disagree, OR_disagree_025, OR_disagree_975, p_disagree)
best_mod_disagree <- as.data.frame(best_mod_disagree)
best_mod_disagree <- cbind(pred_names, best_mod_disagree)
(best_mod_disagree_coef <- best_mod_disagree[,c(1:4,8)])
(best_mod_disagree_OR <- best_mod_disagree[,c(1,5:8)])

# Remove old file
file.remove("Results symlink/MultReg_embrprevlife.txt")
# Write results
newline <- "----------------------------------------------------------------------------------------"
sep <-capture.output(newline,file=NULL) # Print description

descrip <- "Likelihood ratio tests comparing different models"
txt <-capture.output(descrip,file=NULL) # Print description
out <-capture.output(best_mod) # Print test result
cat(txt,file="Results symlink/MultReg_embrprevlife.txt",sep="\n",append=T)
cat(out,file="Results symlink/MultReg_embrprevlife.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_embrprevlife.txt",sep="\n",append=T)

descrip2 <- "Coefficients - Summary of model with lowest AIC - Agree -> Neutral"
txt2 <-capture.output(descrip2,file=NULL) # Print description
out2 <-capture.output(best_mod_neutral_coef) # Print test result
cat(txt2,file="Results symlink/MultReg_embrprevlife.txt",sep="\n",append=T)
cat(out2,file="Results symlink/MultReg_embrprevlife.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_embrprevlife.txt",sep="\n",append=T)

descrip3 <- "Odds Ratios - Summary of model with lowest AIC - Agree -> Neutral"
txt3 <-capture.output(descrip3,file=NULL) # Print description
out3 <-capture.output(best_mod_neutral_OR) # Print test result
cat(txt3,file="Results symlink/MultReg_embrprevlife.txt",sep="\n",append=T)
cat(out3,file="Results symlink/MultReg_embrprevlife.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_embrprevlife.txt",sep="\n",append=T)

descrip4 <- "Coefficients - Summary of model with lowest AIC - Agree -> Disagree"
txt4 <-capture.output(descrip4,file=NULL) # Print description
out4 <-capture.output(best_mod_disagree_coef) # Print test result
cat(txt4,file="Results symlink/MultReg_embrprevlife.txt",sep="\n",append=T)
cat(out4,file="Results symlink/MultReg_embrprevlife.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_embrprevlife.txt",sep="\n",append=T)

descrip5 <- "Odds Ratios - Summary of model with lowest AIC - Agree -> Disagree"
txt5 <-capture.output(descrip5,file=NULL) # Print description
out5 <-capture.output(best_mod_disagree_OR) # Print test result
cat(txt5,file="Results symlink/MultReg_embrprevlife.txt",sep="\n",append=T)
cat(out5,file="Results symlink/MultReg_embrprevlife.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_embrprevlife.txt",sep="\n",append=T)

# # Ordinal Model
# # Fit and compare models
# fit <- clm(embr_prev_life ~ 1, data = all_LR) # Intercept only
# fit2 <- clm(embr_prev_life ~ sex, data = all_LR)
# fit3 <- clm(embr_prev_life ~ sex + age, data = all_LR)
# fit4 <- clm(embr_prev_life ~ sex + age + ethnicity, data = all_LR)
# fit5 <- clm(embr_prev_life ~ sex + age + ethnicity + heard_about, data = all_LR)
# fit6 <- clm(embr_prev_life ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR)
# fit7 <- clm(embr_prev_life ~ sex + age + ethnicity + heard_about + edu_level + religion_type, data = all_LR)
# fit8 <- clm(embr_prev_life ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth, data = all_LR)
# fit9 <- clm(embr_prev_life ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth + worked_health, data = all_LR)
# fit10 <- clm(embr_prev_life ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth + worked_health + genetic_cond, data = all_LR)
# # Compare
# (best_mod <- anova(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10))
# 
# # NEED TO CHANGE MODEL NUMBER
# best_mod_embr_prev_life <- fit10
# # CIs and ORs of model with lowest AIC
# (best_mod_sum <- summary(best_mod_embr_prev_life))
# (ci <- confint(best_mod_embr_prev_life))
# OR <- coef(best_mod_embr_prev_life)
# OR <- OR[-1:-2] # Remove first 2 values which are exponentiates thresholds (i.e. agree -> neutral and neutral -> disagree)
# (best_mod_sumORs <- exp(cbind(OR, ci)))
# 
# # # Proportional Odds Assumption
# # fit5a <- vglm(embr_prev_life ~ sex + age + ethnicity + heard_about, data = all_LR, family=cumulative(parallel=TRUE))
# # fit5b <- vglm(embr_prev_life ~ sex + age + ethnicity + heard_about, data = all_LR, family=cumulative(parallel=FALSE))
# # lrtest(fit5a,fit5b)


# EMBR_PREV_DEBIL
# Duplicate main dataset before collapsing likert levels
all_LR <- all
# Remove NAs from dataset (keep relevant variables only to minimise data loss), otherwise can't do LR test of different models
all_LR <- all_LR[c(8,10:14,16:17,20:21,25,31)]
# Remove NAs from "religion" - now all NAs in "religion_type" indicate that person has no religion.
all_LR<-subset(all_LR,!(is.na(all_LR["religion"])))
# Recode religion and set Christian as the comparison category
all_LR$religion_type <- recode(all_LR$religion_type,
'"1" = "Christian";
"2" = "Christian";
"3" = "Christian";
"4" = "Muslim";
NA = "None";
else = "Other"')
levels(all_LR$religion_type)
all_LR$religion_type <- relevel(all_LR$religion_type, "None")
all_LR <- na.omit(all_LR)
# Recode dependent variable (collapse 6 levels to 3)
all_LR$embr_prev_debil <- recode(all_LR$embr_prev_debil,
'"1" = "1(agree)";
"2" = "1(agree)";
"3" = "2(neutral)";
"4" = "3(disagree)";
"5" = "3(disagree)";
"6" = "2(neutral)";')
all_LR$embr_prev_debil <- ordered(all_LR$embr_prev_debil)
levels(all_LR$embr_prev_debil)
# Recode ethnicity and set Caucasian as the comparison category
all_LR$ethnicity <- recode(all_LR$ethnicity,
'"4" = "Caucasian";
"5" = "Caucasian";
"9" = "Asian";
"11" = "Asian";
NA = NA;
else = "Other"')
levels(all_LR$ethnicity)
all_LR$ethnicity <- relevel(all_LR$ethnicity, "Caucasian")
# Recode education and set None as the comparison category
all_LR$edu_level <- recode(all_LR$edu_level,
'"1" = "None";
"2" = "School";
"3" = "School";
"4" = "School";
NA = NA;
else = "Tertiary"')
levels(all_LR$edu_level)
all_LR$edu_level <- relevel(all_LR$edu_level, "None")
# Recode wealth and set Below as the comparison category
all_LR$wealth <- recode(all_LR$wealth,
'"1" = "Below";
"2" = "Average";
"3" = "Above"')
levels(all_LR$wealth)
all_LR$wealth <- relevel(all_LR$wealth, "Below")
# Recode heard_about and set Never as the comparison category
all_LR$heard_about <- recode(all_LR$heard_about,
'"1" = "Never";
"2" = "Little";
"3" = "Lot"')
levels(all_LR$heard_about)
all_LR$heard_about <- relevel(all_LR$heard_about, "Never")

# Multinomial Model
# Fit and compare models
fit <- multinom(embr_prev_debil ~ 1, data = all_LR) # Intercept only
fit2 <- multinom(embr_prev_debil ~ sex, data = all_LR)
fit3 <- multinom(embr_prev_debil ~ sex + age, data = all_LR)
fit4 <- multinom(embr_prev_debil ~ sex + age + ethnicity, data = all_LR)
fit5 <- multinom(embr_prev_debil ~ sex + age + ethnicity + GDP, data = all_LR)
fit6 <- multinom(embr_prev_debil ~ sex + age + ethnicity + GDP + heard_about, data = all_LR)
fit7 <- multinom(embr_prev_debil ~ sex + age + ethnicity + GDP + heard_about + edu_level, data = all_LR)
fit8 <- multinom(embr_prev_debil ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type, data = all_LR)
fit9 <- multinom(embr_prev_debil ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth, data = all_LR)
fit10 <- multinom(embr_prev_debil ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth + worked_health, data = all_LR)
fit11 <- multinom(embr_prev_debil ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth + worked_health + genetic_cond, data = all_LR)

# Compare
best_mod <- anova(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11)

# Create vector of AIC values
AIC <- c(fit$AIC,fit2$AIC,fit3$AIC,fit4$AIC,fit5$AIC,fit6$AIC,fit7$AIC,fit8$AIC,fit9$AIC,fit10$AIC,fit11$AIC)
best_mod <- cbind(best_mod, AIC)
(best_mod <- best_mod[c("Model", "Resid. df", "Resid. Dev", "AIC", "Test", "   Df", "LR stat.", "Pr(Chi)")])

# Find model with lowest AIC and automatically assign
nametmp <- paste("fit",which.min(AIC),sep="")
nametmp <- eval(parse(text = nametmp))
best_mod_embr_prev_debil <- nametmp
# CIs and ORs of model with lowest AIC
best_mod_sum <- summary(best_mod_embr_prev_debil)
coef <- coef(best_mod_embr_prev_debil)
coef_ci <- confint(best_mod_embr_prev_debil)
OR <- exp(coef)
OR_ci <- exp(coef_ci)
z <- summary(best_mod_embr_prev_debil)$coefficients/summary(best_mod_embr_prev_debil)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# Sequence vectors to recreate coef and OR tables
# Coefficients
coef_neutral_seq <- seq(1,length(coef),2)
coef_neutral <- coef[coef_neutral_seq]
coef_disagree_seq <- seq(2,length(coef),2)
coef_disagree <- coef[coef_disagree_seq]
coef_neutral_025 <- coef_ci[1:(length(coef_ci)/4)]
coef_neutral_975 <- coef_ci[(length(coef_ci)/4)+1:(length(coef_ci)/4)]
coef_disagree_025 <- coef_ci[(length(coef_ci)/2)+1:(length(coef_ci)/4)]
coef_disagree_975 <- coef_ci[((length(coef_ci)/4)*3)+1:(length(coef_ci)/4)]
# Odds Ratios
OR_neutral_seq <- seq(1,length(OR),2)
OR_neutral <- OR[OR_neutral_seq]
OR_disagree_seq <- seq(2,length(OR),2)
OR_disagree <- OR[OR_disagree_seq]
OR_neutral_025 <- OR_ci[1:(length(OR_ci)/4)]
OR_neutral_975 <- OR_ci[(length(OR_ci)/4)+1:(length(OR_ci)/4)]
OR_disagree_025 <- OR_ci[(length(OR_ci)/2)+1:(length(OR_ci)/4)]
OR_disagree_975 <- OR_ci[((length(OR_ci)/4)*3)+1:(length(OR_ci)/4)]
# p values
p_neutral <- p[coef_neutral_seq]
p_disagree <- p[coef_disagree_seq]

# Assemble dataframes of coefficients and odds ratios for neutral and disagree categories
pred_names <- colnames(coef)
best_mod_neutral <- cbind(coef_neutral, coef_neutral_025, coef_neutral_975, OR_neutral, OR_neutral_025, OR_neutral_975, p_neutral)
best_mod_neutral <- as.data.frame(best_mod_neutral)
best_mod_neutral <- cbind(pred_names, best_mod_neutral)
(best_mod_neutral_coef <- best_mod_neutral[,c(1:4,8)])
(best_mod_neutral_OR <- best_mod_neutral[,c(1,5:8)])
best_mod_disagree <- cbind(coef_disagree, coef_disagree_025, coef_disagree_975, OR_disagree, OR_disagree_025, OR_disagree_975, p_disagree)
best_mod_disagree <- as.data.frame(best_mod_disagree)
best_mod_disagree <- cbind(pred_names, best_mod_disagree)
(best_mod_disagree_coef <- best_mod_disagree[,c(1:4,8)])
(best_mod_disagree_OR <- best_mod_disagree[,c(1,5:8)])

# Remove old file
file.remove("Results symlink/MultReg_embrprevdebil.txt")
# Write results
newline <- "----------------------------------------------------------------------------------------"
sep <-capture.output(newline,file=NULL) # Print description

descrip <- "Likelihood ratio tests comparing different models"
txt <-capture.output(descrip,file=NULL) # Print description
out <-capture.output(best_mod) # Print test result
cat(txt,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n",append=T)
cat(out,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n",append=T)

descrip2 <- "Coefficients - Summary of model with lowest AIC - Agree -> Neutral"
txt2 <-capture.output(descrip2,file=NULL) # Print description
out2 <-capture.output(best_mod_neutral_coef) # Print test result
cat(txt2,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n",append=T)
cat(out2,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n",append=T)

descrip3 <- "Odds Ratios - Summary of model with lowest AIC - Agree -> Neutral"
txt3 <-capture.output(descrip3,file=NULL) # Print description
out3 <-capture.output(best_mod_neutral_OR) # Print test result
cat(txt3,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n",append=T)
cat(out3,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n",append=T)

descrip4 <- "Coefficients - Summary of model with lowest AIC - Agree -> Disagree"
txt4 <-capture.output(descrip4,file=NULL) # Print description
out4 <-capture.output(best_mod_disagree_coef) # Print test result
cat(txt4,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n",append=T)
cat(out4,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n",append=T)

descrip5 <- "Odds Ratios - Summary of model with lowest AIC - Agree -> Disagree"
txt5 <-capture.output(descrip5,file=NULL) # Print description
out5 <-capture.output(best_mod_disagree_OR) # Print test result
cat(txt5,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n",append=T)
cat(out5,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_embrprevdebil.txt",sep="\n",append=T)

# # Ordinal Model
# # Fit and compare models
# fit <- clm(embr_prev_debil ~ 1, data = all_LR) # Intercept only
# fit2 <- clm(embr_prev_debil ~ sex, data = all_LR)
# fit3 <- clm(embr_prev_debil ~ sex + age, data = all_LR)
# fit4 <- clm(embr_prev_debil ~ sex + age + ethnicity, data = all_LR)
# fit5 <- clm(embr_prev_debil ~ sex + age + ethnicity + heard_about, data = all_LR)
# fit6 <- clm(embr_prev_debil ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR)
# fit7 <- clm(embr_prev_debil ~ sex + age + ethnicity + heard_about + edu_level + religion_type, data = all_LR)
# fit8 <- clm(embr_prev_debil ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth, data = all_LR)
# fit9 <- clm(embr_prev_debil ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth + worked_health, data = all_LR)
# fit10 <- clm(embr_prev_debil ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth + worked_health + genetic_cond, data = all_LR)
# # Compare
# (best_mod <- anova(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10))
# 
# # NEED TO CHANGE MODEL NUMBER
# best_mod_embr_prev_debil <- fit7
# # CIs and ORs of model with lowest AIC
# (best_mod_sum <- summary(best_mod_embr_prev_debil))
# (ci <- confint(best_mod_embr_prev_debil))
# OR <- coef(best_mod_embr_prev_debil)
# OR <- OR[-1:-2] # Remove first 2 values which are exponentiates thresholds (i.e. agree -> neutral and neutral -> disagree)
# (best_mod_sumORs <- exp(cbind(OR, ci)))
# 
# # # Proportional Odds Assumption
# # fit5a <- vglm(embr_prev_debil ~ sex + age + ethnicity + heard_about, data = all_LR, family=cumulative(parallel=TRUE))
# # fit5b <- vglm(embr_prev_debil ~ sex + age + ethnicity + heard_about, data = all_LR, family=cumulative(parallel=FALSE))
# # lrtest(fit5a,fit5b)


# EDIT_FOR_NONDIS
# Duplicate main dataset before collapsing likert levels
all_LR <- all
# Remove NAs from dataset (keep relevant variables only to minimise data loss), otherwise can't do LR test of different models
all_LR <- all_LR[c(8,10:14,16:17,20:21,26,31)]
# Remove NAs from "religion" - now all NAs in "religion_type" indicate that person has no religion.
all_LR<-subset(all_LR,!(is.na(all_LR["religion"])))
# Recode religion and set Christian as the comparison category
all_LR$religion_type <- recode(all_LR$religion_type,
'"1" = "Christian";
"2" = "Christian";
"3" = "Christian";
"4" = "Muslim";
NA = "None";
else = "Other"')
levels(all_LR$religion_type)
all_LR$religion_type <- relevel(all_LR$religion_type, "None")
all_LR <- na.omit(all_LR)
# Recode dependent variable (collapse 6 levels to 3)
all_LR$edit_for_nondis <- recode(all_LR$edit_for_nondis,
'"1" = "1(agree)";
"2" = "1(agree)";
"3" = "2(neutral)";
"4" = "3(disagree)";
"5" = "3(disagree)";
"6" = "2(neutral)";')
all_LR$edit_for_nondis <- ordered(all_LR$edit_for_nondis)
levels(all_LR$edit_for_nondis)
# Recode ethnicity and set Caucasian as the comparison category
all_LR$ethnicity <- recode(all_LR$ethnicity,
'"4" = "Caucasian";
"5" = "Caucasian";
"9" = "Asian";
"11" = "Asian";
NA = NA;
else = "Other"')
levels(all_LR$ethnicity)
all_LR$ethnicity <- relevel(all_LR$ethnicity, "Caucasian")
# Recode education and set None as the comparison category
all_LR$edu_level <- recode(all_LR$edu_level,
'"1" = "None";
"2" = "School";
"3" = "School";
"4" = "School";
NA = NA;
else = "Tertiary"')
levels(all_LR$edu_level)
all_LR$edu_level <- relevel(all_LR$edu_level, "None")
# Recode wealth and set Below as the comparison category
all_LR$wealth <- recode(all_LR$wealth,
'"1" = "Below";
"2" = "Average";
"3" = "Above"')
levels(all_LR$wealth)
all_LR$wealth <- relevel(all_LR$wealth, "Below")
# Recode heard_about and set Never as the comparison category
all_LR$heard_about <- recode(all_LR$heard_about,
'"1" = "Never";
"2" = "Little";
"3" = "Lot"')
levels(all_LR$heard_about)
all_LR$heard_about <- relevel(all_LR$heard_about, "Never")

# Multinomial Model
# Fit and compare models
fit <- multinom(edit_for_nondis ~ 1, data = all_LR) # Intercept only
fit2 <- multinom(edit_for_nondis ~ sex, data = all_LR)
fit3 <- multinom(edit_for_nondis ~ sex + age, data = all_LR)
fit4 <- multinom(edit_for_nondis ~ sex + age + ethnicity, data = all_LR)
fit5 <- multinom(edit_for_nondis ~ sex + age + ethnicity + GDP, data = all_LR)
fit6 <- multinom(edit_for_nondis ~ sex + age + ethnicity + GDP + heard_about, data = all_LR)
fit7 <- multinom(edit_for_nondis ~ sex + age + ethnicity + GDP + heard_about + edu_level, data = all_LR)
fit8 <- multinom(edit_for_nondis ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type, data = all_LR)
fit9 <- multinom(edit_for_nondis ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth, data = all_LR)
fit10 <- multinom(edit_for_nondis ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth + worked_health, data = all_LR)
fit11 <- multinom(edit_for_nondis ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth + worked_health + genetic_cond, data = all_LR)

# Compare
best_mod <- anova(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11)

# Create vector of AIC values
AIC <- c(fit$AIC,fit2$AIC,fit3$AIC,fit4$AIC,fit5$AIC,fit6$AIC,fit7$AIC,fit8$AIC,fit9$AIC,fit10$AIC,fit11$AIC)
best_mod <- cbind(best_mod, AIC)
(best_mod <- best_mod[c("Model", "Resid. df", "Resid. Dev", "AIC", "Test", "   Df", "LR stat.", "Pr(Chi)")])

# Find model with lowest AIC and automatically assign
nametmp <- paste("fit",which.min(AIC),sep="")
nametmp <- eval(parse(text = nametmp))
best_mod_edit_for_nondis <- nametmp
# CIs and ORs of model with lowest AIC
best_mod_sum <- summary(best_mod_edit_for_nondis)
coef <- coef(best_mod_edit_for_nondis)
coef_ci <- confint(best_mod_edit_for_nondis)
OR <- exp(coef)
OR_ci <- exp(coef_ci)
z <- summary(best_mod_edit_for_nondis)$coefficients/summary(best_mod_edit_for_nondis)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# Sequence vectors to recreate coef and OR tables
# Coefficients
coef_neutral_seq <- seq(1,length(coef),2)
coef_neutral <- coef[coef_neutral_seq]
coef_disagree_seq <- seq(2,length(coef),2)
coef_disagree <- coef[coef_disagree_seq]
coef_neutral_025 <- coef_ci[1:(length(coef_ci)/4)]
coef_neutral_975 <- coef_ci[(length(coef_ci)/4)+1:(length(coef_ci)/4)]
coef_disagree_025 <- coef_ci[(length(coef_ci)/2)+1:(length(coef_ci)/4)]
coef_disagree_975 <- coef_ci[((length(coef_ci)/4)*3)+1:(length(coef_ci)/4)]
# Odds Ratios
OR_neutral_seq <- seq(1,length(OR),2)
OR_neutral <- OR[OR_neutral_seq]
OR_disagree_seq <- seq(2,length(OR),2)
OR_disagree <- OR[OR_disagree_seq]
OR_neutral_025 <- OR_ci[1:(length(OR_ci)/4)]
OR_neutral_975 <- OR_ci[(length(OR_ci)/4)+1:(length(OR_ci)/4)]
OR_disagree_025 <- OR_ci[(length(OR_ci)/2)+1:(length(OR_ci)/4)]
OR_disagree_975 <- OR_ci[((length(OR_ci)/4)*3)+1:(length(OR_ci)/4)]
# p values
p_neutral <- p[coef_neutral_seq]
p_disagree <- p[coef_disagree_seq]

# Assemble dataframes of coefficients and odds ratios for neutral and disagree categories
pred_names <- colnames(coef)
best_mod_neutral <- cbind(coef_neutral, coef_neutral_025, coef_neutral_975, OR_neutral, OR_neutral_025, OR_neutral_975, p_neutral)
best_mod_neutral <- as.data.frame(best_mod_neutral)
best_mod_neutral <- cbind(pred_names, best_mod_neutral)
(best_mod_neutral_coef <- best_mod_neutral[,c(1:4,8)])
(best_mod_neutral_OR <- best_mod_neutral[,c(1,5:8)])
best_mod_disagree <- cbind(coef_disagree, coef_disagree_025, coef_disagree_975, OR_disagree, OR_disagree_025, OR_disagree_975, p_disagree)
best_mod_disagree <- as.data.frame(best_mod_disagree)
best_mod_disagree <- cbind(pred_names, best_mod_disagree)
(best_mod_disagree_coef <- best_mod_disagree[,c(1:4,8)])
(best_mod_disagree_OR <- best_mod_disagree[,c(1,5:8)])

# Remove old file
file.remove("Results symlink/MultReg_editfornondis.txt")
# Write results
newline <- "----------------------------------------------------------------------------------------"
sep <-capture.output(newline,file=NULL) # Print description

descrip <- "Likelihood ratio tests comparing different models"
txt <-capture.output(descrip,file=NULL) # Print description
out <-capture.output(best_mod) # Print test result
cat(txt,file="Results symlink/MultReg_editfornondis.txt",sep="\n",append=T)
cat(out,file="Results symlink/MultReg_editfornondis.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_editfornondis.txt",sep="\n",append=T)

descrip2 <- "Coefficients - Summary of model with lowest AIC - Agree -> Neutral"
txt2 <-capture.output(descrip2,file=NULL) # Print description
out2 <-capture.output(best_mod_neutral_coef) # Print test result
cat(txt2,file="Results symlink/MultReg_editfornondis.txt",sep="\n",append=T)
cat(out2,file="Results symlink/MultReg_editfornondis.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_editfornondis.txt",sep="\n",append=T)

descrip3 <- "Odds Ratios - Summary of model with lowest AIC - Agree -> Neutral"
txt3 <-capture.output(descrip3,file=NULL) # Print description
out3 <-capture.output(best_mod_neutral_OR) # Print test result
cat(txt3,file="Results symlink/MultReg_editfornondis.txt",sep="\n",append=T)
cat(out3,file="Results symlink/MultReg_editfornondis.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_editfornondis.txt",sep="\n",append=T)

descrip4 <- "Coefficients - Summary of model with lowest AIC - Agree -> Disagree"
txt4 <-capture.output(descrip4,file=NULL) # Print description
out4 <-capture.output(best_mod_disagree_coef) # Print test result
cat(txt4,file="Results symlink/MultReg_editfornondis.txt",sep="\n",append=T)
cat(out4,file="Results symlink/MultReg_editfornondis.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_editfornondis.txt",sep="\n",append=T)

descrip5 <- "Odds Ratios - Summary of model with lowest AIC - Agree -> Disagree"
txt5 <-capture.output(descrip5,file=NULL) # Print description
out5 <-capture.output(best_mod_disagree_OR) # Print test result
cat(txt5,file="Results symlink/MultReg_editfornondis.txt",sep="\n",append=T)
cat(out5,file="Results symlink/MultReg_editfornondis.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_editfornondis.txt",sep="\n",append=T)

# # Ordinal Model
# # Fit and compare models
# fit <- clm(edit_for_nondis ~ 1, data = all_LR) # Intercept only
# fit2 <- clm(edit_for_nondis ~ sex, data = all_LR)
# fit3 <- clm(edit_for_nondis ~ sex + age, data = all_LR)
# fit4 <- clm(edit_for_nondis ~ sex + age + ethnicity, data = all_LR)
# fit5 <- clm(edit_for_nondis ~ sex + age + ethnicity + heard_about, data = all_LR)
# fit6 <- clm(edit_for_nondis ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR)
# fit7 <- clm(edit_for_nondis ~ sex + age + ethnicity + heard_about + edu_level + religion_type, data = all_LR)
# fit8 <- clm(edit_for_nondis ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth, data = all_LR)
# fit9 <- clm(edit_for_nondis ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth + worked_health, data = all_LR)
# fit10 <- clm(edit_for_nondis ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth + worked_health + genetic_cond, data = all_LR)
# # Compare
# (best_mod <- anova(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10))
# 
# # NEED TO CHANGE MODEL NUMBER
# best_mod_edit_for_nondis <- fit7
# # CIs and ORs of model with lowest AIC
# (best_mod_sum <- summary(best_mod_edit_for_nondis))
# (ci <- confint(best_mod_edit_for_nondis))
# OR <- coef(best_mod_edit_for_nondis)
# OR <- OR[-1:-2] # Remove first 2 values which are exponentiates thresholds (i.e. agree -> neutral and neutral -> disagree)
# (best_mod_sumORs <- exp(cbind(OR, ci)))
# 
# # # Proportional Odds Assumption
# # fit7a <- vglm(edit_for_nondis ~ sex + age + ethnicity + heard_about + edu_level + religion_type, data = all_LR, family=cumulative(parallel=TRUE))
# # fit7b <- vglm(edit_for_nondis ~ sex + age + ethnicity + heard_about + edu_level + religion_type, data = all_LR, family=cumulative(parallel=FALSE))
# # lrtest(fit7a,fit7b)



# GEN_MOD_FOOD
# Duplicate main dataset before collapsing likert levels
all_LR <- all
# Remove NAs from dataset (keep relevant variables only to minimise data loss), otherwise can't do LR test of different models
all_LR <- all_LR[c(8,10:14,16:17,20:21,30,31)]
# Remove NAs from "religion" - now all NAs in "religion_type" indicate that person has no religion.
all_LR<-subset(all_LR,!(is.na(all_LR["religion"])))
# Recode religion and set Christian as the comparison category
all_LR$religion_type <- recode(all_LR$religion_type,
'"1" = "Christian";
"2" = "Christian";
"3" = "Christian";
"4" = "Muslim";
NA = "None";
else = "Other"')
levels(all_LR$religion_type)
all_LR$religion_type <- relevel(all_LR$religion_type, "None")
all_LR <- na.omit(all_LR)
# Recode dependent variable (collapse 6 levels to 3)
all_LR$gen_mod_food <- recode(all_LR$gen_mod_food,
'"1" = "1(agree)";
"2" = "1(agree)";
"3" = "2(neutral)";
"4" = "3(disagree)";
"5" = "3(disagree)";
"6" = "2(neutral)";')
all_LR$gen_mod_food <- ordered(all_LR$gen_mod_food)
levels(all_LR$gen_mod_food)
# Recode ethnicity and set Caucasian as the comparison category
all_LR$ethnicity <- recode(all_LR$ethnicity,
'"4" = "Caucasian";
"5" = "Caucasian";
"9" = "Asian";
"11" = "Asian";
NA = NA;
else = "Other"')
levels(all_LR$ethnicity)
all_LR$ethnicity <- relevel(all_LR$ethnicity, "Caucasian")
# Recode education and set None as the comparison category
all_LR$edu_level <- recode(all_LR$edu_level,
'"1" = "None";
"2" = "School";
"3" = "School";
"4" = "School";
NA = NA;
else = "Tertiary"')
levels(all_LR$edu_level)
all_LR$edu_level <- relevel(all_LR$edu_level, "None")
# Recode wealth and set Below as the comparison category
all_LR$wealth <- recode(all_LR$wealth,
'"1" = "Below";
"2" = "Average";
"3" = "Above"')
levels(all_LR$wealth)
all_LR$wealth <- relevel(all_LR$wealth, "Below")
# Recode heard_about and set Never as the comparison category
all_LR$heard_about <- recode(all_LR$heard_about,
'"1" = "Never";
"2" = "Little";
"3" = "Lot"')
levels(all_LR$heard_about)
all_LR$heard_about <- relevel(all_LR$heard_about, "Never")


# Multinomial Model
# Fit and compare models
fit <- multinom(gen_mod_food ~ 1, data = all_LR) # Intercept only
fit2 <- multinom(gen_mod_food ~ sex, data = all_LR)
fit3 <- multinom(gen_mod_food ~ sex + age, data = all_LR)
fit4 <- multinom(gen_mod_food ~ sex + age + ethnicity, data = all_LR)
fit5 <- multinom(gen_mod_food ~ sex + age + ethnicity + GDP, data = all_LR)
fit6 <- multinom(gen_mod_food ~ sex + age + ethnicity + GDP + heard_about, data = all_LR)
fit7 <- multinom(gen_mod_food ~ sex + age + ethnicity + GDP + heard_about + edu_level, data = all_LR)
fit8 <- multinom(gen_mod_food ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type, data = all_LR)
fit9 <- multinom(gen_mod_food ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth, data = all_LR)
fit10 <- multinom(gen_mod_food ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth + worked_health, data = all_LR)
fit11 <- multinom(gen_mod_food ~ sex + age + ethnicity + GDP + heard_about + edu_level + religion_type + wealth + worked_health + genetic_cond, data = all_LR)

# Compare
best_mod <- anova(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11)

# Create vector of AIC values
AIC <- c(fit$AIC,fit2$AIC,fit3$AIC,fit4$AIC,fit5$AIC,fit6$AIC,fit7$AIC,fit8$AIC,fit9$AIC,fit10$AIC,fit11$AIC)
best_mod <- cbind(best_mod, AIC)
(best_mod <- best_mod[c("Model", "Resid. df", "Resid. Dev", "AIC", "Test", "   Df", "LR stat.", "Pr(Chi)")])

# Find model with lowest AIC and automatically assign
nametmp <- paste("fit",which.min(AIC),sep="")
nametmp <- eval(parse(text = nametmp))
best_mod_gen_mod_food <- nametmp
# CIs and ORs of model with lowest AIC
best_mod_sum <- summary(best_mod_gen_mod_food)
coef <- coef(best_mod_gen_mod_food)
coef_ci <- confint(best_mod_gen_mod_food)
OR <- exp(coef)
OR_ci <- exp(coef_ci)
z <- summary(best_mod_gen_mod_food)$coefficients/summary(best_mod_gen_mod_food)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# Sequence vectors to recreate coef and OR tables
# Coefficients
coef_neutral_seq <- seq(1,length(coef),2)
coef_neutral <- coef[coef_neutral_seq]
coef_disagree_seq <- seq(2,length(coef),2)
coef_disagree <- coef[coef_disagree_seq]
coef_neutral_025 <- coef_ci[1:(length(coef_ci)/4)]
coef_neutral_975 <- coef_ci[(length(coef_ci)/4)+1:(length(coef_ci)/4)]
coef_disagree_025 <- coef_ci[(length(coef_ci)/2)+1:(length(coef_ci)/4)]
coef_disagree_975 <- coef_ci[((length(coef_ci)/4)*3)+1:(length(coef_ci)/4)]
# Odds Ratios
OR_neutral_seq <- seq(1,length(OR),2)
OR_neutral <- OR[OR_neutral_seq]
OR_disagree_seq <- seq(2,length(OR),2)
OR_disagree <- OR[OR_disagree_seq]
OR_neutral_025 <- OR_ci[1:(length(OR_ci)/4)]
OR_neutral_975 <- OR_ci[(length(OR_ci)/4)+1:(length(OR_ci)/4)]
OR_disagree_025 <- OR_ci[(length(OR_ci)/2)+1:(length(OR_ci)/4)]
OR_disagree_975 <- OR_ci[((length(OR_ci)/4)*3)+1:(length(OR_ci)/4)]
# p values
p_neutral <- p[coef_neutral_seq]
p_disagree <- p[coef_disagree_seq]

# Assemble dataframes of coefficients and odds ratios for neutral and disagree categories
pred_names <- colnames(coef)
best_mod_neutral <- cbind(coef_neutral, coef_neutral_025, coef_neutral_975, OR_neutral, OR_neutral_025, OR_neutral_975, p_neutral)
best_mod_neutral <- as.data.frame(best_mod_neutral)
best_mod_neutral <- cbind(pred_names, best_mod_neutral)
(best_mod_neutral_coef <- best_mod_neutral[,c(1:4,8)])
(best_mod_neutral_OR <- best_mod_neutral[,c(1,5:8)])
best_mod_disagree <- cbind(coef_disagree, coef_disagree_025, coef_disagree_975, OR_disagree, OR_disagree_025, OR_disagree_975, p_disagree)
best_mod_disagree <- as.data.frame(best_mod_disagree)
best_mod_disagree <- cbind(pred_names, best_mod_disagree)
(best_mod_disagree_coef <- best_mod_disagree[,c(1:4,8)])
(best_mod_disagree_OR <- best_mod_disagree[,c(1,5:8)])

# Remove old file
file.remove("Results symlink/MultReg_genmodfood.txt")
# Write results
newline <- "----------------------------------------------------------------------------------------"
sep <-capture.output(newline,file=NULL) # Print description

descrip <- "Likelihood ratio tests comparing different models"
txt <-capture.output(descrip,file=NULL) # Print description
out <-capture.output(best_mod) # Print test result
cat(txt,file="Results symlink/MultReg_genmodfood.txt",sep="\n",append=T)
cat(out,file="Results symlink/MultReg_genmodfood.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_genmodfood.txt",sep="\n",append=T)

descrip2 <- "Coefficients - Summary of model with lowest AIC - Agree -> Neutral"
txt2 <-capture.output(descrip2,file=NULL) # Print description
out2 <-capture.output(best_mod_neutral_coef) # Print test result
cat(txt2,file="Results symlink/MultReg_genmodfood.txt",sep="\n",append=T)
cat(out2,file="Results symlink/MultReg_genmodfood.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_genmodfood.txt",sep="\n",append=T)

descrip3 <- "Odds Ratios - Summary of model with lowest AIC - Agree -> Neutral"
txt3 <-capture.output(descrip3,file=NULL) # Print description
out3 <-capture.output(best_mod_neutral_OR) # Print test result
cat(txt3,file="Results symlink/MultReg_genmodfood.txt",sep="\n",append=T)
cat(out3,file="Results symlink/MultReg_genmodfood.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_genmodfood.txt",sep="\n",append=T)

descrip4 <- "Coefficients - Summary of model with lowest AIC - Agree -> Disagree"
txt4 <-capture.output(descrip4,file=NULL) # Print description
out4 <-capture.output(best_mod_disagree_coef) # Print test result
cat(txt4,file="Results symlink/MultReg_genmodfood.txt",sep="\n",append=T)
cat(out4,file="Results symlink/MultReg_genmodfood.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_genmodfood.txt",sep="\n",append=T)

descrip5 <- "Odds Ratios - Summary of model with lowest AIC - Agree -> Disagree"
txt5 <-capture.output(descrip5,file=NULL) # Print description
out5 <-capture.output(best_mod_disagree_OR) # Print test result
cat(txt5,file="Results symlink/MultReg_genmodfood.txt",sep="\n",append=T)
cat(out5,file="Results symlink/MultReg_genmodfood.txt",sep="\n", append=T)
cat(sep,file="Results symlink/MultReg_genmodfood.txt",sep="\n",append=T)


# # Ordinal Model
# # Fit and compare models
# fit <- clm(gen_mod_food ~ 1, data = all_LR) # Intercept only
# fit2 <- clm(gen_mod_food ~ sex, data = all_LR)
# fit3 <- clm(gen_mod_food ~ sex + age, data = all_LR)
# fit4 <- clm(gen_mod_food ~ sex + age + ethnicity, data = all_LR)
# fit5 <- clm(gen_mod_food ~ sex + age + ethnicity + heard_about, data = all_LR)
# fit6 <- clm(gen_mod_food ~ sex + age + ethnicity + heard_about + edu_level, data = all_LR)
# fit7 <- clm(gen_mod_food ~ sex + age + ethnicity + heard_about + edu_level + religion_type, data = all_LR)
# fit8 <- clm(gen_mod_food ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth, data = all_LR)
# fit9 <- clm(gen_mod_food ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth + worked_health, data = all_LR)
# fit10 <- clm(gen_mod_food ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth + worked_health + genetic_cond, data = all_LR)
# # Compare
# (best_mod <- anova(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10))
# 
# # NEED TO CHANGE MODEL NUMBER
# best_mod_gen_mod_food <- fit8
# # CIs and ORs of model with lowest AIC
# (best_mod_sum <- summary(best_mod_gen_mod_food))
# (ci <- confint(best_mod_gen_mod_food))
# OR <- coef(best_mod_gen_mod_food)
# OR <- OR[-1:-2] # Remove first 2 values which are exponentiates thresholds (i.e. agree -> neutral and neutral -> disagree)
# (best_mod_sumORs <- exp(cbind(OR, ci)))

# # Proportional Odds Assumption
# fit8a <- vglm(gen_mod_food ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth, data = all_LR, family=cumulative(parallel=TRUE))
# fit8b <- vglm(gen_mod_food ~ sex + age + ethnicity + heard_about + edu_level + religion_type + wealth, data = all_LR, family=cumulative(parallel=FALSE))
# lrtest(fit8a,fit8b)




# Predict probabilities
predict_fit <- clm(kids_cure_life ~ sex + ethnicity + heard_about + edu_level + religion_type, data = all_LR)
newData <- expand.grid(sex=levels(all_LR$sex), ethnicity=levels(all_LR$ethnicity), heard_about=levels(all_LR$heard_about), edu_level=levels(all_LR$edu_level), religion_type=levels(all_LR$religion_type))
cbind(newData, predict(predict_fit, newdata=newData)$fit)


# Actual proportions from the data
kids_table <- table(all_LR$kids_cure_life)
(kids_prop <- round(prop.table(kids_table)*100,2))





# Check Variance Inflation Factor of Predictors
# https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/

vif_func<-function(in_frame,thresh=10,trace=T,...){
  require(fmsb)
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  for(val in names(in_frame)){
    form_in<-formula(paste(val,' ~ .'))
    vif_init<-rbind(vif_init,c(val,VIF(lm(form_in,data=in_frame,...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(names(in_frame))
  }
  else{
    in_dat<-in_frame
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      vif_vals<-NULL
      for(val in names(in_dat)){
        form_in<-formula(paste(val,' ~ .'))
        vif_add<-VIF(lm(form_in,data=in_dat,...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      vif_max<-as.numeric(vif_vals[max_row,2])
      if(vif_max<thresh) break
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
    }
    return(names(in_dat))
  }
}


vif_test <- all[c(8,10:14,16:17,20:21,31)]
vif_test <- na.omit(test)
vif_test$sex <- recode(vif_test$sex,
'"M" = 1;
"F" = 2')
vif_test$worked_health <- recode(vif_test$worked_health,
'"N" = 1;
"Y" = 2')
vif_test$genetic_cond <- recode(vif_test$genetic_cond,
'"N" = 1;
"Y" = 2')
vif_test$religion <- recode(vif_test$religion,
'"N" = 1;
"Y" = 2')

vif_test$sex <- as.numeric(vif_test$sex)
vif_test$ethnicity <- as.numeric(vif_test$ethnicity)
vif_test$wealth <- as.numeric(vif_test$wealth)
vif_test$edu_level <- as.numeric(vif_test$edu_level)
vif_test$worked_health <- as.numeric(vif_test$worked_health)
vif_test$heard_about <- as.numeric(vif_test$heard_about)
vif_test$genetic_cond <- as.numeric(vif_test$genetic_cond)
vif_test$religion <- as.numeric(vif_test$religion)
vif_test$religion_type <- as.numeric(vif_test$religion_type)

vif_func(in_frame=vif_test,thresh=5,trace=T)



