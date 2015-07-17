#IMPORT----

library(jsonlite) # Import .json file
library(stringr) # Trim whitespace 
library(plyr) # Various
library(car) # Recode variables
library(doBy) # Group summary stats

# PREPARE JSON FILE FOR IMPORT INTO R

#Edit .json file in text editor before importing - Remove 'Results' and outer curly brackets. Replicate:
# [
# { "name":"Amy" , "grade1": 35 , "grade2": 41 , "grade3": 53},
# { "name":"Bob" , "grade1": 44 , "grade2": 37 , "grade3": 28},
# ]

# CHANGE WORKING DIRECTORY HERE
# setwd('~/Desktop/Research/CRISPR/PublicPercpeption/Analysis/Active/')

setwd('~/Dropbox/Research Projects/2015/Gene Edit Survey')

# Need 2 files in the working directory: 'Answers.json' and 'wechat.csv'
wechat <- read.csv(file="wechat.csv", header=T) #wechat data file
data <- fromJSON('Answers.json') #main data file

# Variable Names
# 1.createdAt - date/time survey started
# 2.updatedAt - date/time survey ended
# 3.time_to_do - time in minutes to complete survey
# 4.cohort - responses from main survey (1) or wechat (2)
# 5.ip - ip address
# 6.language - as is
# 7.country - as is
# 8.sex - Factor variable (levels - M/F)
# 9.YOB - year of birth
# 10.age - age in years
# 11.ethnicity - Factor variable (levels - 1:11)
# 12.wealth - Factor variable (levels - 1:3)
# 13.edu_level - Factor variable (levels - 1:6)
# 14.worked_health - have you ever worked in a health or medical related field? Factor variable (levels - N/Y)
# 15.worked_health_type - type of work. Factor variable (levels - 1:6)
# 16.heard_about - have you ever heard of human genetic engineering or gene editing? Factor variable (levels - 1:3)
# 17.genetic_cond - do you or anyone in your family have an inherited or genetic condition? Factor variable (levels - N/Y)
# 18.genetic_cond_affected - who is affected? Factor variable (levels 1:3)
# 19.genetic_cond_type - what is the disease? Factor variable (levels 1:10)
# 20.religion - Factor variable (levels - N/Y)
# 21.religion_type - type of religion. Factor variable (levels 1:14)
# 22.kids_cure_life - how much do you agree with the use of genetic editing of cells in children or adults to cure a life threatening disease? Factor variable (levels 1:6)
# 23.kids_cure_debil - how much do you agree with the use of genetic editing of cells in children or adults to cure a debilitating disease? Factor variable (levels 1:6)
# 24.embr_prev_life - how much do you agree with the use of genetic editing of cells in embryos to prevent a life threatening disease? Factor variable (levels 1:6)
# 25.embr_prev_debil - how much do you agree with the use of genetic editing of cells in embryos to prevent a debilitating disease? Factor variable (levels 1:6)
# 26.edit_for_nondis - how much do you agree with the use of genetic editing of cells in embryos to alter any non-disease characteristic? Factor variable (levels 1:6)
# 27.deter_phys_appear - would you use this technology to determine Physical appearance? Factor variable (levels - N/Y)
# 28.deter_intell - would you use this technology to determine Intelligence? Factor variable (levels - N/Y)
# 29.deter_strength - would you use this technology to determine Strength or Sporting ability? Factor variable (levels - N/Y)
# 30.gen_mod_food - how much do you agree with the use of genetically modified food? Factor variable (levels 1:6)

#WECHAT----------------------------------------------------------------------------------------

# IMPORT AND PREPARE WECHAT DATA

# RENAME AND REORDER VARIABLES

wechat<-rename(wechat, c("IP地址"="ip", "开始时间"="createdAt", "结束时间"="updatedAt", "Q1_您的性别."="sex", "Q2_您的年龄."="age", "Q3_您所居住的国家."="country", "Q5_有无宗教信仰."="religion", "Q6_您所信仰的宗教是."="religion_type", "Q8_您的受教育水平."="edu_level", "Q9_您之前在保健医疗行业工作过吗."="worked_health", "Q10_您在保健医疗行业所从事的职业是."="worked_health_type", "Q12_您听说过基因工程或基因编辑吗."="heard_about", "Q13_您的经济状况如何."="wealth", "Q14_您家庭里有人有遗传或基因现象吗."="genetic_cond", "Q15_哪些人受到影响."="genetic_cond_affected", "Q16_症状为哪些."="genetic_cond_type", "Q18_..仅治愈儿童或成人自身危及生命的遗传病症.但疾病仍会遗传给下一代."="kids_cure_life", "Q19_.仅治愈儿童或成人使人衰弱的遗传病症.但疾病仍会遗传给下一代."="kids_cure_debil", "Q20_.从胚胎开始根治危及生命的疾病.这就意味着疾病不会被传给下一代."="embr_prev_life", "Q21_......从胚胎开始根治使人衰弱的疾病.这就意味着他们的后代不会患上这种疾病."="embr_prev_debil", "Q22_.利用胚胎改变任何非疾病的特征.例如记忆.瞳色或者身高.这就意味着下一代可能会有同样的基因特征"="edit_for_nondis","Q23_如果您可以安全的进行胚胎基因改造.您会使用这项技术改变什么._外貌.瞳色.发色.肤色."="deter_phys_appear", "Q23_如果您可以安全的进行胚胎基因改造.您会使用这项技术改变什么._智力"="deter_intell", "Q23_如果您可以安全的进行胚胎基因改造.您会使用这项技术改变什么._力量或者体育能力"="deter_strength", "Q25_您是否同意食用转基因食物._您是否同意食用转基因食物."="gen_mod_food"))

# Clean time variables and generate time_to_do variable (minutes)
time_start <- strptime(wechat$createdAt, format='%Y-%m-%d %H:%M:%S')
time_end <- strptime(wechat$updatedAt, format='%Y-%m-%d %H:%M:%S')
wechat$createdAt <- time_start
wechat$updatedAt <- time_end
wechat$time_to_do <- round((time_end - time_start)/60,digits=2)
rm(time_start)
rm(time_end)

# Generate matching variables to main dataframe
wechat$language <- as.factor("CHINESE")
wechat$ethnicity <- as.factor(11)
wechat$cohort <- as.factor(2)
wechat$ip <- as.character(wechat$ip)

# Need to remove # from some entried of age variable
wechat$age <- gsub("#", "", wechat$age)
wechat$age <- as.numeric(as.character(wechat$age))
wechat$YOB <- 2015-wechat$age

# Delete unnecessary variables
wechat$答卷编号 <- wechat$来源 <- wechat$昵称.微信. <- wechat$性别.微信. <- wechat$城市.微信. <- wechat$状态 <- wechat$Q4_请填写您所居住的国家 <- wechat$Q7_请填写您所信仰的宗教名称 <- wechat$Q11_请填写您在保健医疗行业所从事的职业 <- wechat$Q23_如果您可以安全的进行胚胎基因改造.您会使用这项技术改变什么._其他跟健康无关的特征 <- wechat$Q23_如果您可以安全的进行胚胎基因改造.您会使用这项技术改变什么._不做任何改变 <- wechat$Q24_请填写您会改变的其他跟健康无关的特征 <- wechat$Q26_我们对您回答这些的理由很感兴趣.您是否愿意描述一下影响您这些回答的理由或者影响您对于人类基因工程的态度的因素.谢谢 <- wechat$Q27_请填写您的理由 <- NULL

# Reorder
wechat <- wechat[c("createdAt", "updatedAt", "time_to_do", "cohort", "ip", "language", "country", "sex", "YOB", "age", "ethnicity", "wealth", "edu_level", "worked_health", "worked_health_type", "heard_about", "genetic_cond", "genetic_cond_affected", "genetic_cond_type", "religion", "religion_type", "kids_cure_life", "kids_cure_debil", "embr_prev_life", "embr_prev_debil", "edit_for_nondis", "deter_phys_appear", "deter_intell", "deter_strength", "gen_mod_food")]


# RECODE VARIABLES AS NOMINAL/ORDINAL

# COUNTRY
# Trim Whitespace
wechat$country <- str_trim(wechat$country, side = "both")
# Make as Factor
wechat$country <- as.factor(wechat$country)
levels(wechat$country)
# Recode
wechat$country <- recode(wechat$country,
'"中国" = "China";
"澳大利亚" = "Australia";
"德国" = "Germany";
"美国" = "United States of America";
"英国" = "United Kingdom";
c("其他", "") = "Other"')
levels(wechat$country)


# SEX
# Trim Whitespace
wechat$sex <- str_trim(wechat$sex, side = "both")
# Make as Factor
wechat$sex <- as.factor(wechat$sex)
levels(wechat$sex)
# Recode
wechat$sex <- recode(wechat$sex,
'"男" = "M";
"女" = "F";
"" = NA')
levels(wechat$sex)


# WEALTH
# Trim Whitespace
wechat$wealth <- str_trim(wechat$wealth, side = "both")
# Make as Factor
wechat$wealth <- as.factor(wechat$wealth)
levels(wechat$wealth)
# Recode
wechat$wealth <- recode(wechat$wealth,
'"平均水平之上" = 1;
"平均水平" = 2;
"低于平均水平" = 3;
"" = NA')
levels(wechat$wealth)


# EDU_LEVEL
# Trim Whitespace
wechat$edu_level <- str_trim(wechat$edu_level, side = "both")
# Make as Factor
wechat$edu_level <- as.factor(wechat$edu_level)
levels(wechat$edu_level)
# Recode
wechat$edu_level <- recode(wechat$edu_level,
'"未上过正规学校" = 1;
"小学毕业" = 2;
"高中毕业" = 3;
"仅完成大学学业（结业）" = 4;
"大学毕业" = 5;
"研究生毕业" = 6;
"" = NA')
levels(wechat$edu_level)
wechat$edu_level <- ordered(wechat$edu_level)


# WORKED_HEALTH
# Trim Whitespace
wechat$worked_health <- str_trim(wechat$worked_health, side = "both")
# Make as Factor
wechat$worked_health <- as.factor(wechat$worked_health)
levels(wechat$worked_health)
# Recode
wechat$worked_health <- recode(wechat$worked_health,
'"无" = "N";
"有"= "Y";
"" = NA')
levels(wechat$worked_health)


# WORKED_HEALTH_TYPE
# Trim Whitespace
wechat$worked_health_type <- str_trim(wechat$worked_health_type, side = "both")
# Make as Factor
wechat$worked_health_type <- as.factor(wechat$worked_health_type)
levels(wechat$worked_health_type)
# Recode
wechat$worked_health_type <- recode(wechat$worked_health_type,
'"医生" = 1;
"科研工作者"  = 2;
"护士" = 3;
"医学相关专业" = 4;                   
"医院或其他医疗中心工作者" = 5;
"其他" = 6;
"" = NA')
levels(wechat$worked_health_type)


# HEARD_ABOUT
# Trim Whitespace
wechat$heard_about <- str_trim(wechat$heard_about, side = "both")
# Make as Factor
wechat$heard_about <- as.factor(wechat$heard_about)
levels(wechat$heard_about)
# Recode
wechat$heard_about <- recode(wechat$heard_about,
'"从未听说过" = 1;
"听说过一点点" = 2;
"很了解" =  3') 
levels(wechat$heard_about)
wechat$heard_about <- ordered(wechat$heard_about)


# GENETIC_COND
# Trim Whitespace
wechat$genetic_cond <- str_trim(wechat$genetic_cond, side = "both")
# Make as Factor
wechat$genetic_cond <- as.factor(wechat$genetic_cond)
levels(wechat$genetic_cond)
# Recode
wechat$genetic_cond <- recode(wechat$genetic_cond,
'"无" = "N";
"有"= "Y";
"" = NA')
levels(wechat$genetic_cond)


# GENETIC_COND_AFFECTED
# Trim Whitespace
wechat$genetic_cond_affected <- str_trim(wechat$genetic_cond_affected, side = "both")
# Make as Factor
wechat$genetic_cond_affected <- as.factor(wechat$genetic_cond_affected)
levels(wechat$genetic_cond_affected)
# Recode
wechat$genetic_cond_affected <- recode(wechat$genetic_cond_affected,
'"我" = 1;
"其他家庭成员"  = 2;
"我和另外的家庭成员" = 3;
"" = NA')
levels(wechat$genetic_cond_affected)


# GENETIC_COND_TYPE
# Trim Whitespace
wechat$genetic_cond_type <- str_trim(wechat$genetic_cond_type, side = "both")
# Make as Factor
wechat$genetic_cond_type <- as.factor(wechat$genetic_cond_type)
levels(wechat$genetic_cond_type)
# Recode
wechat$genetic_cond_type <- recode(wechat$genetic_cond_type,
'"囊胞性纤维症" = 1;
"肌肉萎缩症" = 3;
"地中海贫血" = 5;
"唐氏综合症，爱德华综合症，三体综合征" = 9;
"其他" = 10;
"" = NA')
levels(wechat$genetic_cond_type)


# RELIGION
# Trim Whitespace
wechat$religion <- str_trim(wechat$religion, side = "both")
# Make as Factor
wechat$religion <- as.factor(wechat$religion)
levels(wechat$religion)
# Recode
wechat$religion <- recode(wechat$religion,
'"无" = "N";
"有"= "Y";
"" = NA')
levels(wechat$religion)  


# RELIGION_TYPE
# Trim Whitespace
wechat$religion_type <- str_trim(wechat$religion_type, side = "both")
# Make as Factor
wechat$religion_type <- as.factor(wechat$religion_type)
levels(wechat$religion_type)
# Recode
wechat$religion_type <- recode(wechat$religion_type,
'"基督教-新教/英国国教" = 1;
"基督教-天主教" = 2;
"基督教-其他" = 3;
"穆斯林" = 4;
"佛教" = 6;
"道教" = 9;
"儒家" = 10;
"其他" = 14;
"" = NA')
levels(wechat$religion_type)


# KIDS_CURE_LIFE
# Trim Whitespace
wechat$kids_cure_life <- str_trim(wechat$kids_cure_life, side = "both")
# Make as Factor
wechat$kids_cure_life <- as.factor(wechat$kids_cure_life)
levels(wechat$kids_cure_life)
# Recode
wechat$kids_cure_life <- recode(wechat$kids_cure_life,
'"非常同意" = 1;
"同意" = 2;
"中立" = 3;
"不同意" = 4;
"坚决反对" = 5;
"不知道" = 6')
levels(wechat$kids_cure_life)
wechat$kids_cure_life <- ordered(wechat$kids_cure_life)


# KIDS_CURE_DEBIL
# Trim Whitespace
wechat$kids_cure_debil <- str_trim(wechat$kids_cure_debil, side = "both")
# Make as Factor
wechat$kids_cure_debil <- as.factor(wechat$kids_cure_debil)
levels(wechat$kids_cure_debil)
# Recode
wechat$kids_cure_debil <- recode(wechat$kids_cure_debil,
'"非常同意" = 1;
"同意" = 2;
"中立" = 3;
"不同意" = 4;
"坚决反对" = 5;
"不知道" = 6')
levels(wechat$kids_cure_debil)
wechat$kids_cure_debil <- ordered(wechat$kids_cure_debil)


# EMBR_PREV_LIFE
# Trim Whitespace
wechat$embr_prev_life <- str_trim(wechat$embr_prev_life, side = "both")
# Make as Factor
wechat$embr_prev_life <- as.factor(wechat$embr_prev_life)
levels(wechat$embr_prev_life)
# Recode
wechat$embr_prev_life <- recode(wechat$embr_prev_life,
'"非常同意" = 1;
"同意" = 2;
"中立" = 3;
"不同意" = 4;
"坚决反对" = 5;
"不知道" = 6')
levels(wechat$embr_prev_life)
wechat$embr_prev_life <- ordered(wechat$embr_prev_life)


# EMBR_PREV_DEBIL
# Trim Whitespace
wechat$embr_prev_debil <- str_trim(wechat$embr_prev_debil, side = "both")
# Make as Factor
wechat$embr_prev_debil <- as.factor(wechat$embr_prev_debil)
levels(wechat$embr_prev_debil)
# Recode
wechat$embr_prev_debil <- recode(wechat$embr_prev_debil,
'"非常同意" = 1;
"同意" = 2;
"中立" = 3;
"不同意" = 4;
"坚决反对" = 5;
"不知道" = 6')
levels(wechat$embr_prev_debil)
wechat$embr_prev_debil <- ordered(wechat$embr_prev_debil)


# EDIT_FOR_NONDIS
# Trim Whitespace
wechat$edit_for_nondis <- str_trim(wechat$edit_for_nondis, side = "both")
# Make as Factor
wechat$edit_for_nondis <- as.factor(wechat$edit_for_nondis)
levels(wechat$edit_for_nondis)
# Recode
wechat$edit_for_nondis <- recode(wechat$edit_for_nondis,
'"非常同意" = 1;
"同意" = 2;
"中立" = 3;
"不同意" = 4;
"坚决反对" = 5;
"不知道" = 6')
levels(wechat$edit_for_nondis)
wechat$edit_for_nondis <- ordered(wechat$edit_for_nondis)


# GEN_MOD_FOOD
# Trim Whitespace
wechat$gen_mod_food <- str_trim(wechat$gen_mod_food, side = "both")
# Make as Factor
wechat$gen_mod_food <- as.factor(wechat$gen_mod_food)
levels(wechat$gen_mod_food)
# Recode
wechat$gen_mod_food <- recode(wechat$gen_mod_food,
'"非常同意" = 1;
"同意" = 2;
"中立" = 3;
"不同意" = 4;
"坚决反对" = 5;
"不知道" = 6')
levels(wechat$gen_mod_food)
wechat$gen_mod_food <- ordered(wechat$gen_mod_food)


# DETER_PYHS_APPEAR
# Trim Whitespace
wechat$deter_phys_appear <- str_trim(wechat$deter_phys_appear, side = "both")
# Make as Factor
wechat$deter_phys_appear <- as.factor(wechat$deter_phys_appear)
levels(wechat$deter_phys_appear)
# Recode
wechat$deter_phys_appear <- recode(wechat$deter_phys_appear,
'"" = "N";
"外貌（瞳色，发色，肤色）" = "Y"')
levels(wechat$deter_phys_appear)


# DETER_INTELL
# Trim Whitespace
wechat$deter_intell <- str_trim(wechat$deter_intell, side = "both")
# Make as Factor
wechat$deter_intell <- as.factor(wechat$deter_intell)
levels(wechat$deter_intell)
# Recode
wechat$deter_intell <- recode(wechat$deter_intell,
'"" = "N";
"智力" = "Y"')
levels(wechat$deter_intell)


# DETER_STRENGTH
# Trim Whitespace
wechat$deter_strength <- str_trim(wechat$deter_strength, side = "both")
# Make as Factor
wechat$deter_strength <- as.factor(wechat$deter_strength)
levels(wechat$deter_strength)
# Recode
wechat$deter_strength <- recode(wechat$deter_strength,
'"" = "N";
"力量或者体育能力" = "Y"')
levels(wechat$deter_strength)

#MAIN----------------------------------------------------------------------------------------

# IMPORT AND PREPARE MAIN DATA

# Delete observations that we filled in as a test
# Creates dataframe containing observations with the string "delete" - identify which to delete and add below
#del <- data[grep("delete", c(data$question_25,data$question_23, data$question_101)),]
# Delete specific observations that contain the following string matches - too difficult to remove all in one go that contain "delete' as some people have used this word in their responses
data <- data[- grep("Test test delete", c(data$question_25)),]
data <- data[- grep("delete - Paul test", c(data$question_25)),]
data <- data[- grep("delete", c(data$question_23)),]

# RENAME AND REORDER VARIABLES

data<-rename(data, c("question_1"="sex", "question_2"="YOB", "question_3"="country", "question_4"="ethnicity", "question_5"="religion", "question_6"="religion_type", "question_7"="edu_level", "question_8"="worked_health", "question_9"="worked_health_type", "question_10"="heard_about", "question_11"="wealth", "question_12"="genetic_cond", "question_13"="genetic_cond_affected", "question_14"="genetic_cond_type", "question_15"="kids_cure_life", "question_16"="kids_cure_debil", "question_17"="embr_prev_life", "question_18"="embr_prev_debil", "question_19"="edit_for_nondis", "question_20"="deter_phys_appear", "question_21"="deter_intell", "question_22"="deter_strength", "question_23"="other_traits_alter","question_24"="gen_mod_food"))

# Fix Arabic and Russian country translations
Countries_combined <- read.csv("Countries_combined.csv", header=FALSE, stringsAsFactors = FALSE)

for(i in 1:nrow(data)) {
  if(data$country[i] %in% Countries_combined$V2) {
    data$tmp[i]<-as.character(Countries_combined$V1[match(data$country[i], Countries_combined$V2)[1]])
  } 
  if(data$country[i] %in% Countries_combined$V3) {
    data$tmp[i]<-as.character(Countries_combined$V1[match(data$country[i], Countries_combined$V3)[1]])
  } 
  if(data$country[i] %in% Countries_combined$V1) {
    data$tmp[i]<-data$country[i]
  }
}
data$country <- data$tmp
data$tmp <- NULL
rm(Countries_combined)
rm(i)

# Generate age and cohort variables
data$YOB <- as.numeric(data$YOB)
data$age <- 2015-data$YOB
data$cohort <- as.factor(1)
data$country <- as.factor(data$country)
data$language <- as.factor(data$language)

# Clean time variables and generate time_to_do variable (minutes)
time_start <- strptime(data$createdAt, format='%Y-%m-%dT%H:%M:%S')
time_end <- strptime(data$updatedAt, format='%Y-%m-%dT%H:%M:%S')
data$createdAt <- time_start
data$updatedAt <- time_end
data$time_to_do <- round((time_end - time_start)/60,digits=2)
rm(time_start)
rm(time_end)

# Delete unnecessary variables
data$client <- data$objectId <- NULL

# Delete free text variables
# data$other_traits_alter <- data$question_25 <- data$question_100 <- data$question_101 <- data$question_102 <- NULL

# Reorder
data <- data[c("createdAt", "updatedAt", "time_to_do", "cohort", "ip", "language", "country", "sex", "YOB", "age", "ethnicity", "wealth", "edu_level", "worked_health", "worked_health_type", "heard_about", "genetic_cond", "genetic_cond_affected", "genetic_cond_type", "religion", "religion_type", "kids_cure_life", "kids_cure_debil", "embr_prev_life", "embr_prev_debil", "edit_for_nondis", "deter_phys_appear", "deter_intell", "deter_strength", "gen_mod_food", "other_traits_alter", "question_25", "question_100", "question_101", "question_102")]

# RECODE VARIABLES AS NOMINAL/ORDINAL

# SEX
# Trim Whitespace
data$sex <- str_trim(data$sex, side = "both")
# Make as Factor
data$sex <- as.factor(data$sex)
levels(data$sex)
# Recode
data$sex <- recode(data$sex,
'c("Male", "Homme", "ذكر", "男", "Männlich", "पुरुषों", "男性", "Masculino", "Мужской", "Erkek") = "M";
c("Female", "Femme", "أنثى", "女", "Weiblich", "मिहला", "女性", "Feminino", "Женский", "Femenino", "Kadın") = "F"')
levels(data$sex)


# ETHNICITY
# Trim Whitespace
data$ethnicity <- str_trim(data$ethnicity, side = "both")
# Make as Factor
data$ethnicity <- as.factor(data$ethnicity)
levels(data$ethnicity)
# Recode
data$ethnicity <- recode(data$ethnicity,
'c("Mixed Race", "Karışık (Melez)", "Raza mixta", "Смешанная раса", "Raça mista", "Gemischte Rasse", "Métis", "混血", "عرقية مختلطة", "मिश्र जाति") = 1; 

c("African/African American", "Afroamericano/Africano", "Africana/afro-americana", "Africain/Afro-américain", "Afro/Afro-Amerikanisch", "Африканец/Афроамериканец", "Afrikalı", "非裔美籍 ", "アフリカ人／アフリカ系アメリカ人 ", "أفريقى/ أمريكى أفريقى", "अफ्रीकी / अफ्रीकी अमेरिकी") = 2; 

c("Asian Indian", "Indio asiático", "Asiatische Inder", "Asiático Índico", "Indien d’Asie", "印度裔 ", "भारतीय/एशियाई", "هندى أسيوى", "Индиец", "Hint Asyalı", "アジア系インド人") = 3; 

c("Caucasian (European)", "قوقازى (أوروبى)", "白种人（欧洲）", "Caucasien (Européen)", "Kaukasic/Europäer", "श्वेतजाति  (यूरोपीय)", "श्वेतजाति (मध्यपूर्व)", "白人（ヨーロッパ系）", "Caucasiano (Europeu)", "Европеоид (Европа)", "Caucásico (europeo)", "Avrupalı") = 4; 

c("Caucasian (Middle East)", "قوقازى (شرق أوسطى)", "白种人（中东）", "Caucasien (Moyen-Orient)", "Kaukasisch ( Middle East )", "श्वेतजाति  (मध्यपूर्व)", "白人（中東系)", "Caucasiano (Oriente Médio)", "Европеоид (Ближний Восток)", "Caucásico (medio oriente)", "Orta Doğulu") = 5; 

c("Hispanic, Latino or Spanish", "هسبانى أو لاتينى أو اسبانى", "西班牙，拉美裔 ", "Hispanique, Latino", "Hispanic, Latino oder Spanish", "हिस्पैनिकलातीनोयास्पेनिश", "ヒスパニック、ラテン系またはスペイン人", "Hispânico, latino ou espanhol", "Латиноамериканец или испанец", "Hispano, latinoamericano o español", "Hispanik, Latin veya İspanyol") = 6; 

c("Indigenous Australian", "أسترالى أصلى", "澳大利亚土著人 ", "Indigène australien", "Einheimischer Australier", "मूलऑस्ट्रेलियाई", "原住民オーストラリア人", "Indígena Australiano", "Австралийский абориген", "Indígena australiano", "Avusturalya Yerlisi") = 7;

c("Native American", "أمريكى أصلى", "美国人 ", "Indien d’Amérique", "Einheimischer Amerikaner (Indianer)", "मूलअमेरिकी", "アメリカ先住民 ", "Americano Nativo", "Коренной американец", "Nativo americano", "Amerika Yerlisi") = 8;

c("North East Asian (Mongol, Tibetan, Korean, Japanese, etc)", "شمال شرق آسيا (منغولى، تبتى، كورى، يابانى، إلخ)", "东北亚裔（蒙古，西藏，韩国，日本等）", "Asiatique du nord-est (Mongol, Tibétaine, Coréen, Japonais, etc...)", "Nordöstliche Asiaten (mongolisch, tibetisch, koreanisch, japanisch etc.)", "उत्तर-पूर्वएशियाई (मंगोल, तिब्बती, कोरियाई, जापानी, आदि)", "北東アジア（モンゴル人、チベット族、韓国人、日本人、など）", "Norte Leste Asiático (mongol, tibetano, coreano, japonês, etc.)", "Северо-восточный азиат (монгол, тибетец, кореец, японец и т.п.)", "Del noreste asiático (mongol, tibetano, coreano, japonés, etc.)", "Kuzey Doğu Asyalı (Moğol, Tibet, Koreli, Japon, vs)") = 9;

c("Pacific (Polynesian, micronesian, etc)", "المحيط الهادى (بولونيزى، ميكرونيزى، إلخ)", "太平洋(波利尼西亚, 密克罗尼西亚等) ", "Pacifique (Polynésien, Micronésien, etc...)", "Pazifische (Polynesisch etc.)", "प्रशांत (पॉलिनीषियन, माइक्रोनेशियन, आदि)", "大西洋（ポリネシア人、ミクロネシア人、など）", "Pacífico (etc. da Micronésia, Polinésia,)", "Тихоокеанец (полинезиец, микронезиец и т.п.)", "Del Pacífico (polinesio, micronesio, etc.)", "Pasifik (Polinezyalı, Mikronezyalı, vs)") = 10;

c("South East Asian (Chinese, Thai, Malay, Filipino, etc)", "جنوب شرق آسيا (صينى، تايلاندى، ماليزى، فليبينى، إلخ)", "东南亚裔（中国，泰国，马来，菲律宾等）", "Asiatique du sud-est -Chinois, Thaï, Malaisienne, Philippine, etc...)", "Südöstliche Asiaten (chinesisch, thai,  malaysisch, Filipino etc.)", "दक्षिणपूर्वएशियाई (चीनी, थाई, मलय, फिलिपिनो,आदि)", "南東アジア（中国人、タイ人、マレーシア人、フィリピン人、など）", "Do Sudeste Asiático (chinês, tailandês, malaio, filipino, etc)", "Юго-восточный азиат (китаец, таец, малаец, филиппинец и т.п.)", "Del sudeste asiático (chino, tailandés, malayo, filipino, etc.)", "Güney Doğu Asyalı (Çinli, Tay, Malezyalı, Filipinli, vs)") = 11')
levels(data$ethnicity) 


# WEALTH
# Trim Whitespace
data$wealth <- str_trim(data$wealth, side = "both")
# Make as Factor
data$wealth <- as.factor(data$wealth)
levels(data$wealth)
# Recode
data$wealth <- recode(data$wealth,
'c("Above average wealth", "فوق المتوسط", "平均水平之上", "Au dessus de la moyenne financièree", "überdurchschnittlicher Wohlstand", "औसतधनराशिकेऊपर", "平均よりも上", "Classe Alta", "Выше среднего", "Por encima del salario promedio", "Genel refah düzeyinin üstünde") = 1;

c("Average wealth", "متوسط", "平均水平", "Dans la moyenne financière", "mittlerer Wohlstand", "औसतधनराशि", "平均", "Classe Média", "Среднее", "Salario promedio", "Genel refah düzeyinde") = 2;

c("Below average wealth", "تحت المتوسط", "低于平均水平", "En dessous de la moyenne financièr", "unterdurchschnittlich Wohlstand", "औसतधनराशि सेनीचे", "平均よりも下", "Classe Baixa", "Ниже среднего", "Por debajo del salario promedio", "Genel refah düzeyinin altında") = 3')
levels(data$wealth)
data$wealth <- ordered(data$wealth)


# EDU_LEVEL
# Trim Whitespace
data$edu_level <- str_trim(data$edu_level, side = "both")
# Make as Factor
data$edu_level <- as.factor(data$edu_level)
levels(data$edu_level)
# Recode
data$edu_level <- recode(data$edu_level,
'c("No formal schooling", "تعليم غير رسمى", "未上过正规学校", "Aucune éducation scolaire", "keine Schulausbildung", "कोईऔपचारिकस्कूलीशिक्षानहीं", "学校教育なし", "Sem escolaridade formal", "Нет официального образования", "Sin escolaridad formal", "Herhangi bir resmi eğitimim yok") = 1;

c("Finished primary school", "أنهيت المرحلة الابتدائية", "小学毕业", "École primaire", "Grunds hulabschluss", "प्राथमिकविद्यालयपूरीकी", "小学校を卒業", "Primário completo", "Начальное", "Escuela primaria concluida", "İlkokul veya ortaokul mezunuyum") = 2;

c("Finished high school", "أنهيت المرحلة الثانوية", "高中毕业", "Lycée", "Abitur", "उच्चविद्यालयपूरीकी", "高校を卒業", "Ensino médio completo", "Среднее", "Escuela secundaria concluida", "Lise veya dengi mezunuyum") = 3;

c("Finished a course following school", "أنهيت دورة بعد المرحلة المدرسة", "仅完成学校学业", "Cours en dehors de l’école", "Kurs nach der Schule", "स्कूलकेबादएककोर्ससमाप्तकिया", "学校卒業に続くコースを終了", "Completou um curso após a escola", "Среднее специальное", "Concluí un curso después de la educación básica", "Okulu müteakip bir kurs bitirdim") = 4;

c("Finished undergraduate university degree", "أنهيت التعليم الجامعى", "大学毕业", "Licence", "Bachelor‐Abschluss", "विश्वविद्यालयकेस्नातक(undergraguate)कीडिग्रीली", "年制大学を卒業", "Formação universitária de graduação completa", "Бакалавр", "Escolaridad a nivel licenciatura concluida", "Yüksekokul ya da fakülte mezunuyum") = 5;

c("Finished postgraduate university degree", "أنهيت الراسات العليا", "研究生毕业", "Master", "Doctorat", "Diplom‐Abschluss", "स्नातकोत्तर(postgraduate)विश्वविद्यालयकीडिग्रीली", "大学院を卒業", "Formação universitária de pós-graduação completa", "Магистр", "Escolaridad a nivel de posgrado concluida", "Yüksek lisans mezunuyum") = 6;

"" = NA')
levels(data$edu_level)
data$edu_level <- ordered(data$edu_level)


# WORKED_HEALTH
# Trim Whitespace
data$worked_health <- str_trim(data$worked_health, side = "both")
# Make as Factor
data$worked_health <- as.factor(data$worked_health)
levels(data$worked_health)
# Recode
data$worked_health <- recode(data$worked_health,
'c("No", "لا", "无", "Non", "Nein", "नहीं", "いいえ", "Não", "Нет", "No", "Hayır") = "N";
c("Yes", "نعم", "有", "Oui", "Ja", "हां", "はい", "Sim", "Да", "Sí", "Evet") = "Y"')
levels(data$worked_health)
                      

# WORKED_HEALTH_TYPE
# Trim Whitespace
data$worked_health_type <- str_trim(data$worked_health_type, side = "both")
# Make as Factor
data$worked_health_type <- as.factor(data$worked_health_type)
levels(data$worked_health_type)
# Recode
data$worked_health_type <- recode(data$worked_health_type,
'c("Medical Doctor", "طبيب", "医生", "Docteur / Doctoresse", "Arzt", "चिकित्सक", "医師", "Médico(a)", "Врач", "Doctorado en medicina", "Tıbbi Doktor") = 1;

c("Scientific Researcher", "باحث علمى", "科研工作者", "Scientifique", "wissenschaftlicher Mitarbeiter", "वैज्ञानिकशोधकर्ता", "科学研究者", "Pesquisador(a) Científico(a)", "Научный сотрудник", "Investigación científica", "Bilimsel Araştırmacı") = 2;

c("Nurse", "ممرض", "护士", "Infirmier/ infirmière", "Krankenschwester", "नर्स", "看護師", "Enfermeiro(a)", "Медсестра (медбрат)", "Enfermería", "Hemşire") = 3;

c("Allied health worker", "عامل صحى", "合作医疗工作者", "医学相关专业","Auxiliaire de médecine", "Krankenpfleger", "अन्यस्वास्थ्यकार्यकर्ता", "保険ワーカー", "Agente de Saúde Conveniado", "Похожие медицинские специальности", "Trabajador aliado de la salud", "Yardımcı sağlık görevlisi") = 4;

c("Other role at hospital/medical centre", "دور آخر فى مستشفى/مركز طبى", "医院或其他医疗中心工 作者", "Autre rôle dans un hôpital / centre médical", "andere Rolle im Krankenhaus/Klinikum", "अस्पताल/मेडिकलसेंटरमेंअन्यभूमिका", "病院／医療センターでの他の仕事", "Outro cargo no centro médico/hospital", "Прочие должности при больнице/медицинском центре", "Otra función en un hospital/centro médico", "Hastanede/sağlık merkezinde diğer bir rol") = 5;

c("Other", "أخرى", "其他", "Autre", "andere", "अन्य", "その他", "Outros", "Другое", "Otro", "Diğer") = 6')
levels(data$worked_health_type)


# HEARD_ABOUT
# Trim Whitespace
data$heard_about <- str_trim(data$heard_about, side = "both")
# Make as Factor
data$heard_about <- as.factor(data$heard_about)
levels(data$heard_about)
# Recode
# There is a problem here with the string matching for the Hindi response = 2. In the raw text ouput there are 2 levels displayed that appear exactly the same - one of them will match using recode and the other won't (I can't figure this out - a Boolean even says they're the same). So I have needed to recode twice, coercing the non-matching string in an 'else' statement. 
data$heard_about <- recode(data$heard_about,
'c("I have never heard of it", "لم أسمع بها من قبل", "从未听说过", "Je n’en ai jamais entendu parler", "Ich habe nie etwas darüber gehört", "मैंनेइसकेबारेमेंकभीनहींसुना|", "聞いたことがない", "Nunca ouvi falar sobre isso", "никогда не слышал об этом", "Jamás he escuchado al respecto", "Hiç duymadım") = 1;

c("I have heard a little about it", "سمعت القليل عنها", "听说过一点点", "J’en ai un peu entendu parler", "Ich eine ein wenig darüber gehört", "मैंने  इसकेबारेमेंथोड़ासासुनाहै|", "少しだけなら聞いたことがある", "Ouvi um pouco sobre isso", "немного слышал об этом", "He escuchado un poco al respecto", "Biraz fikrim var") = 2;

c("I have heard a lot about it", "سمعت الكثير عنها", "很了解", "J’en ai beaucoup entendu parler", "Ich habe viel davon gehört", "मैंनेइसकेबारेमेंकाफीसुनाहै|", "たくさん聞いたことがある", "Ouvi muito sobre isso", "много слышал об этом", "He escuchado mucho al respecto", "Hakkında çok şey duydum") = 3') 
levels(data$heard_about)
####
#Run to here and check that only "मैंने  इसकेबारेमेंथोड़ासासुनाहै|" isn't recoded, before forcing this string to be recoded as 2.
####
data$heard_about <- recode(data$heard_about, '1 = 1; 2 = 2; 3 = 3; NA = NA; else = 2')
levels(data$heard_about)
data$heard_about <- ordered(data$heard_about)


# GENETIC_COND
# Trim Whitespace
data$genetic_cond <- str_trim(data$genetic_cond, side = "both")
# Make as Factor
data$genetic_cond <- as.factor(data$genetic_cond)
levels(data$genetic_cond)
# Recode
data$genetic_cond <- recode(data$genetic_cond,
'c("No", "لا", "无", "Non", "Nein", "नहीं", "いいえ", "Não", "Нет", "No", "Hayır") = "N";
c("Yes", "نعم", "有", "Oui", "Ja", "हां", "はい", "Sim", "Да", "Sí", "Evet") = "Y"')
levels(data$genetic_cond)


# GENETIC_COND_AFFECTED
# Trim Whitespace
data$genetic_cond_affected <- str_trim(data$genetic_cond_affected, side = "both")
# Make as Factor
data$genetic_cond_affected <- as.factor(data$genetic_cond_affected)
levels(data$genetic_cond_affected)
# Recode
data$genetic_cond_affected <- recode(data$genetic_cond_affected,
'c("Me", "أنا", "我", "Moi", "Ich", "मैं", "自分", "Eu", "Я", "Yo", "Ben") = 1;

c("Another family member(s)", "فرد)أفراد( آخر فى الأسرة)", "其他家庭成员", "Un autre membre de ma famille", "Ein Familienzugehöriger", "परिवार का अन्य सदस्य", "自分以外の家族メンバー（複数)", "Outro membro (s) da família", "Другой(-ие) член(-ы) семьи", "Otro(s) miembro(s) de mi familia", "Diğer aile üyesi veya üyeleri") = 2;

c("Me and a family member(s)", "أنا وفرد )أفراد( آخر فى الأسرة)", "我和另外的家庭成员", "Moi et un membre de ma famille", "Ich und meine Familie", "मैं और परिवार का एक सदस्य", "自分と家族（複数）　 病症は何ですか", "自分と家族（複数）　 病", "Eu e um membro (s) da família ", "u e um membro (s) da família", "Я и член(-ы) семьи", "Yo y un (otros) miembro(s) de mi familia", "Ben ve diğer bir aile üyesi/üyeleri") = 3')
levels(data$genetic_cond_affected)


# GENETIC_COND_TYPE
# Trim Whitespace
data$genetic_cond_type <- str_trim(data$genetic_cond_type, side = "both")
# Make as Factor
data$genetic_cond_type <- as.factor(data$genetic_cond_type)
levels(data$genetic_cond_type)
# Recode
data$genetic_cond_type <- recode(data$genetic_cond_type,
'c("Cystic Fibrosis", "تليف كيسى", "囊胞性纤维症", "Mucoviscidose", "Mukoviszidose", "सिस्टिकफाइब्रोसिस", "嚢胞性繊維証", "Fibrose Cística", "Муковисцидоз", "Fibrosis quística", "Kistik Fibroz") = 1;

c("Huntington’s Disease", "مرض هونتيغتون", "亨廷顿病", "Maladie de Huntington", "Huntigton‐Kranhkheit", "हनटिंग्टनरोग", "ハンティントン病", "Doença de Huntington", "Enfermedad de Huntington", "Huntington Hastalığı") = 2;

c("Muscular Dystrophy", "ضمور العضلات", "肌肉萎缩症", "Dystrophie musculaire", "Muskeldystrophie", "मांसपेशीयदुर्विकास (Muscular Dystrophy)", "筋ジストロフィー", "Distrofia", "Мышечная дистрофия", "Distrofia muscular", "Kas Distrofisi") = 3;

c("Sickle Cell Anaemia", "أنيميا الخلايا المنجلية", "镰状细胞贫血", "Hydroxyurée", "Sichzellenänemie", "रक्तकीलालकोशिकाओंकीकमी (Sickle Cell Ananemia)", "鎌状赤血球貧血", "Anemia Falciforme", "Серповидно-клеточная анемия", "Anemia de células falciformes", "Orak Hücre Anemisi") = 4;

c("Beta Thalassemia", "ثلاسيمية بيتا", "地中海贫血", "Cardiomyopathie par surcharge en fer", "Beta Thalässamie", "बीटाथैलेसीमिया", "地中海性貧血", "Beta Talassemia", "Бета-талассемия ", "Talasemia beta", "Akdeniz Anemisi") = 5;

c("Haemophilia", "سيولة الدم", "血友病", "Hémophilie", "Hämophilie", "हीमोफीलिया", "血友病", "Hemofilia", "Гемофилия", "Hemofilia", "Hemofili") = 6;

c("Tay Sachs Disease", "شحام سفينغولى )تاى ساكس(", "泰-萨克斯病", "Hyperacousie", "Tay-Sachs-Syndrom", "टेसेक्सबीमारी", "テイ•サックス病", "Doença de Tay Sachs", "Болезнь Тея-Сакса", "Enfermedad de Tay Sachs", "Tay Sachs Hastalığı") = 7;

c("Fragile X Syndrome", "متلازمة كروموزوم اكس الهش", "X染色体易损综合征", "Syndrome de l’X fragile", "Fragiles-X-Syndrom", "कमजोरएक्सलक्षण ( Fragile X Syndrome)", "脆弱X症候群", "Síndrome Frágil X", "Синдром Мартина-Белл", "Síndrome X frágil", "Frajil X Sendromu") = 8;

c("Down’s Syndrome, Edward’s Syndrome, Patau Syndrome", "متلازمة داون، متلازمة إدوارد، متلازمة باتو", "唐氏综合症,爱德华综 合症,三体综合征", "Syndrome de Down, Syndrome d’Edward, Syndrome de Patau", "Down Syndrom,  Edwards Syndrom,  Pätau Syndrom", "डाउनसिंड्रोम, एडवर्ड\'ससिंड्रोम, पतौसिंड्रोम", "ダウン症候群、エドワード症候群、パトー症候群", "Síndrome de Down, Síndrome de Edward, Síndrome de Patau", "Синдром Дауна, Синдром Эдвардса, Синдром Патау", "Síndrome de Down, síndrome de Edward, síndrome de Patau", "Down Sendromu, Edward Sendromu, Patau Sendromu") = 9;

c("Other", "أخرى", "其他", "Autre", "andere", "अन्य", "その他", "Outras", "Другое", "Otro", "Diğer") = 10')
levels(data$genetic_cond_type)
                                 

# RELIGION
# Trim Whitespace
data$religion <- str_trim(data$religion, side = "both")
# Make as Factor
data$religion <- as.factor(data$religion)
levels(data$religion)
# Recode
data$religion <- recode(data$religion,
'c("No", "لا", "无", "Non", "Nein", "नहीं", "いいえ", "Não", "Нет", "No", "Hayır") = "N";
c("Yes", "نعم", "有", "Oui", "Ja", "हां", "はい", "Sim", "Да", "Sí", "Evet") = "Y"')
levels(data$religion)                   
                        

# RELIGION_TYPE
# Trim Whitespace
data$religion_type <- str_trim(data$religion_type, side = "both")
# Make as Factor
data$religion_type <- as.factor(data$religion_type)
levels(data$religion_type)
# Recode
data$religion_type <- recode(data$religion_type,
'c("Christian - Protestant/Anglican", "مسيحى – بروتوستانتى/إنجيلى", "基督教-新教/英国 国教", "Chrétien - Protestant/Anglican", "Christlich – evangelische/anglikanisch", "ईसाई- प्रोटेस्टेंट / अँग्रेज़ी", "キリスト教—プロテスタント／英国国教会派", "Cristão Protestante/Anglicano", "Христианство - протестантство/англиканство", "Cristianismo (protestante/anglicano)", "Hristiyan Protestan/Anglikan") = 1;

c("Christian - Catholic", "مسيحى – كاثوليكى", "基督教-天主教", "Chrétien - Catholique", "Christlich – Katholisch", "ईसाई- कैथोलिक", "ईसाई- कैथोिलक", "キリスト教—カトリック教会", "Cristão – Católico", "Христианство - католичество", "Cristianismo (católico)", "Hristiyan Katolik") = 2;

c("Christian - Other", "مسيحى – أخرى", "基督教-其他", "Chrétien - Autre", "Christlich – andere", "ईसाई- अन्य", "キリスト教—その他", "Cristão - Outro", "Христианство - прочее", "Cristianismo (otra)", "Hristiyan Diğer") = 3;

c("Muslim", "مسلم", "穆斯林", "Musulman", "Muslim", "मुस्लिम", "イスラム教", "Muçulmano", "Ислам", "Islamismo", "Müslüman") = 4;

c("Jewish", "يهودى", "犹太教", "Juif", "Jüdisch", "यहूदी", "ユダヤ教", "Judeu", "Иудейство", "Judaísmo", "Musevi") = 5;

c("Buddhist", "بوذى", "佛教", "Bouddhiste", "Buddhismus", "बौद्ध", "仏教", "Budista", "Буддизм", "Budismo", "Budist") = 6;

c("Hindu", "هندوسى", "印度教", "Hindouiste", "Hindu", "हिन्दू", "ヒンズー教", "Hindu", "Индуизм", "Hindú", "Hindu") = 7;

c("Shinto", "شنتو", "日本神道教", "Shintoïste", "Shinto", "शिंटो", "神道", "Xintoísmo", "Синтоизм", "Shinto", "Şinto") = 8;

c("Taoism", "طاوى", "道教", "Taoïste", "Taoismus", "ताओधर्म", "道教", "Taoísmo", "Даосизм", "Taoísmo", "Taoizm") = 9;

c("Confucianism", "كنفوشيوسى", "儒家", "Confucianiste", "Konfuzianismus", "कन्फ्यूशीवाद", "儒教", "Confucionismo", "Конфуцианство", "Confucianismo", "Konfüçyüsçülük") = 10;

c("Sikhism", "سيخى", "锡克教", "Sikh", "Sihismus", "सिखधर्म", "シーク教", "Sikhismo", "Сикхизм", "Sijismo", "Sihizim") = 11;

c("Jainism", "يانى", "耆那教", "Djaïniste", "Janis", "जैनधर्म", "ジャイナ教", "Jainismo", "Джайнизм", "Jainismo", "Jainizm") = 12;

c("Druze", "درزى", "德鲁兹教派", "Druze", "Drusen", "ड्रूज़", "ドルーズ教", "Druso", "Друзы", "Druso", "Dürzi")
= 13;

c("Other", "أخرى", "其他", "Autre", "andere", "अन्य", "その他", "Outras", "Другое ", "Otra", "Diğer") = 14;

"मुि\u1b6dलम" = NA')
levels(data$religion_type)


# KIDS_CURE_LIFE
# Trim Whitespace
data$kids_cure_life <- str_trim(data$kids_cure_life, side = "both")
# Make as Factor
data$kids_cure_life <- as.factor(data$kids_cure_life)
levels(data$kids_cure_life)
# Recode
data$kids_cure_life <- recode(data$kids_cure_life,
'c("Strongly Agree", "أوافق بشدة", "非常同意", "Entièrement d’accord", "Stimme sehr zu", "दृढ़तापूर्वकसहमत", "強く同意", "Concordo totalmente", "Полностью согласен", "Estoy completamente de acuerdo", "Kesinlikle Katılıyorum") = 1;

c("Agree", "أوافق", "同意", "D’accord", "Stimme zu", "सहमत", "同意", "Concordo", "Согласен", "Estoy de acuerdo", "Katılıyorum") = 2;

c("Neutral", "محايد", "中立", "Neutre", "Neutral", "तटस्थ", "中立", "Neutro", "Равнодушен", "Mi opinión es neutral", "Kararsızım") = 3;

c("Disagree", "لا أوافق", "不同意", "En désaccord", "Stimme nicht zu", "असहमत", "反対", "Discordo", "Несогласен", "Estoy en desacuerdo", "Katılmıyorum") = 4;

c("Strongly Disagree", "لا أوافق بشدة", "坚决反对", "Particulièrement en désaccord", "Stimme überhaupt nicht zu", "दृढ़तापूर्वकअसहमत", "強く反対", "Discordo totalmente", "Совершенно несогласен", "Estoy completamente en desacuerdo", "Kesinlikle Katılmıyorum") = 5;

c("I don’t Know", "لا أعلم", "不知道", "Je ne sais pas", "Ich weiß nicht", "मुझेनहींपता", "分からない", "Não sei", "Не знаю", "No lo sé", "Bilmiyorum") = 6')
levels(data$kids_cure_life)
data$kids_cure_life <- ordered(data$kids_cure_life)


# KIDS_CURE_DEBIL
# Trim Whitespace
data$kids_cure_debil <- str_trim(data$kids_cure_debil, side = "both")
# Make as Factor
data$kids_cure_debil <- as.factor(data$kids_cure_debil)
levels(data$kids_cure_debil)
# Recode
data$kids_cure_debil <- recode(data$kids_cure_debil,
'c("Strongly Agree", "أوافق بشدة", "非常同意", "Entièrement d’accord", "Stimme sehr zu", "दृढ़तापूर्वकसहमत", "強く同意", "Concordo totalmente", "Полностью согласен", "Estoy completamente de acuerdo", "Kesinlikle Katılıyorum") = 1;

c("Agree", "أوافق", "同意", "D’accord", "Stimme zu", "सहमत", "同意", "Concordo", "Согласен", "Estoy de acuerdo", "Katılıyorum") = 2;

c("Neutral", "محايد", "中立", "Neutre", "Neutral", "तटस्थ", "中立", "Neutro", "Равнодушен", "Mi opinión es neutral", "Kararsızım") = 3;

c("Disagree", "لا أوافق", "不同意", "En désaccord", "Stimme nicht zu", "असहमत", "反対", "Discordo", "Несогласен", "Estoy en desacuerdo", "Katılmıyorum") = 4;

c("Strongly Disagree", "لا أوافق بشدة", "坚决反对", "Particulièrement en désaccord", "Stimme überhaupt nicht zu", "दृढ़तापूर्वकअसहमत", "強く反対", "Discordo totalmente", "Совершенно несогласен", "Estoy completamente en desacuerdo", "Kesinlikle Katılmıyorum") = 5;

c("I don’t Know", "لا أعلم", "不知道", "Je ne sais pas", "Ich weiß nicht", "मुझेनहींपता", "分からない", "Não sei", "Не знаю", "No lo sé", "Bilmiyorum") = 6')
levels(data$kids_cure_debil)
data$kids_cure_debil <- ordered(data$kids_cure_debil)


# EMBR_PREV_LIVE
# Trim Whitespace
data$embr_prev_life <- str_trim(data$embr_prev_life, side = "both")
# Make as Factor
data$embr_prev_life <- as.factor(data$embr_prev_life)
levels(data$embr_prev_life)
# Recode
data$embr_prev_life <- recode(data$embr_prev_life,
'c("Strongly Agree", "أوافق بشدة", "非常同意", "Entièrement d’accord", "Stimme sehr zu", "दृढ़तापूर्वकसहमत", "強く同意", "Concordo totalmente", "Полностью согласен", "Estoy completamente de acuerdo", "Kesinlikle Katılıyorum") = 1;

c("Agree", "أوافق", "同意", "D’accord", "Stimme zu", "सहमत", "同意", "Concordo", "Согласен", "Estoy de acuerdo", "Katılıyorum") = 2;

c("Neutral", "محايد", "中立", "Neutre", "Neutral", "तटस्थ", "中立", "Neutro", "Равнодушен", "Mi opinión es neutral", "Kararsızım") = 3;

c("Disagree", "لا أوافق", "不同意", "En désaccord", "Stimme nicht zu", "असहमत", "反対", "Discordo", "Несогласен", "Estoy en desacuerdo", "Katılmıyorum") = 4;

c("Strongly Disagree", "لا أوافق بشدة", "坚决反对", "Particulièrement en désaccord", "Stimme überhaupt nicht zu", "दृढ़तापूर्वकअसहमत", "強く反対", "Discordo totalmente", "Совершенно несогласен", "Estoy completamente en desacuerdo", "Kesinlikle Katılmıyorum") = 5;

c("I don’t Know", "لا أعلم", "不知道", "Je ne sais pas", "Ich weiß nicht", "मुझेनहींपता", "分からない", "Não sei", "Не знаю", "No lo sé", "Bilmiyorum") = 6')
levels(data$embr_prev_life)
data$embr_prev_life <- ordered(data$embr_prev_life)


# EMBR_PREV_DEBIL
# Trim Whitespace
data$embr_prev_debil <- str_trim(data$embr_prev_debil, side = "both")
# Make as Factor
data$embr_prev_debil <- as.factor(data$embr_prev_debil)
levels(data$embr_prev_debil)
# Recode
data$embr_prev_debil <- recode(data$embr_prev_debil,
'c("Strongly Agree", "أوافق بشدة", "非常同意", "Entièrement d’accord", "Stimme sehr zu", "दृढ़तापूर्वकसहमत", "強く同意", "Concordo totalmente", "Полностью согласен", "Estoy completamente de acuerdo", "Kesinlikle Katılıyorum") = 1;

c("Agree", "أوافق", "同意", "D’accord", "Stimme zu", "सहमत", "同意", "Concordo", "Согласен", "Estoy de acuerdo", "Katılıyorum") = 2;

c("Neutral", "محايد", "中立", "Neutre", "Neutral", "तटस्थ", "中立", "Neutro", "Равнодушен", "Mi opinión es neutral", "Kararsızım") = 3;

c("Disagree", "لا أوافق", "不同意", "En désaccord", "Stimme nicht zu", "असहमत", "反対", "Discordo", "Несогласен", "Estoy en desacuerdo", "Katılmıyorum") = 4;

c("Strongly Disagree", "لا أوافق بشدة", "坚决反对", "Particulièrement en désaccord", "Stimme überhaupt nicht zu", "दृढ़तापूर्वकअसहमत", "強く反対", "Discordo totalmente", "Совершенно несогласен", "Estoy completamente en desacuerdo", "Kesinlikle Katılmıyorum") = 5;

c("I don’t Know", "لا أعلم", "不知道", "Je ne sais pas", "Ich weiß nicht", "मुझेनहींपता", "分からない", "Não sei", "Не знаю", "No lo sé", "Bilmiyorum") = 6')
levels(data$embr_prev_debil)
data$embr_prev_debil <- ordered(data$embr_prev_debil)


# EMBR_FOR_NONDIS
# Trim Whitespace
data$edit_for_nondis <- str_trim(data$edit_for_nondis, side = "both")
# Make as Factor
data$edit_for_nondis <- as.factor(data$edit_for_nondis)
levels(data$edit_for_nondis)
# Recode
data$edit_for_nondis <- recode(data$edit_for_nondis,
'c("Strongly Agree", "أوافق بشدة", "非常同意", "Entièrement d’accord", "Stimme sehr zu", "दृढ़तापूर्वकसहमत", "強く同意", "Concordo totalmente", "Полностью согласен", "Estoy completamente de acuerdo", "Kesinlikle Katılıyorum") = 1;

c("Agree", "أوافق", "同意", "D’accord", "Stimme zu", "सहमत", "同意", "Concordo", "Согласен", "Estoy de acuerdo", "Katılıyorum") = 2;

c("Neutral", "محايد", "中立", "Neutre", "Neutral", "तटस्थ", "中立", "Neutro", "Равнодушен", "Mi opinión es neutral", "Kararsızım") = 3;

c("Disagree", "لا أوافق", "不同意", "En désaccord", "Stimme nicht zu", "असहमत", "反対", "Discordo", "Несогласен", "Estoy en desacuerdo", "Katılmıyorum") = 4;

c("Strongly Disagree", "لا أوافق بشدة", "坚决反对", "Particulièrement en désaccord", "Stimme überhaupt nicht zu", "दृढ़तापूर्वकअसहमत", "強く反対", "Discordo totalmente", "Совершенно несогласен", "Estoy completamente en desacuerdo", "Kesinlikle Katılmıyorum") = 5;

c("I don’t Know", "لا أعلم", "不知道", "Je ne sais pas", "Ich weiß nicht", "मुझेनहींपता", "分からない", "Não sei", "Не знаю", "No lo sé", "Bilmiyorum") = 6')
levels(data$edit_for_nondis)
data$edit_for_nondis <- ordered(data$edit_for_nondis)


# GEN_MOD_FOOD
# Trim Whitespace
data$gen_mod_food <- str_trim(data$gen_mod_food, side = "both")
# Make as Factor
data$gen_mod_food <- as.factor(data$gen_mod_food)
levels(data$gen_mod_food)
# Recode
data$gen_mod_food <- recode(data$gen_mod_food,
'c("Strongly Agree", "أوافق بشدة", "非常同意", "Entièrement d’accord", "Stimme sehr zu", "दृढ़तापूर्वकसहमत", "強く同意", "Concordo totalmente", "Полностью согласен", "Estoy completamente de acuerdo", "Kesinlikle Katılıyorum") = 1;

c("Agree", "أوافق", "同意", "D’accord", "Stimme zu", "सहमत", "同意", "Concordo", "Согласен", "Estoy de acuerdo", "Katılıyorum") = 2;

c("Neutral", "محايد", "中立", "Neutre", "Neutral", "तटस्थ", "中立", "Neutro", "Равнодушен", "Mi opinión es neutral", "Kararsızım") = 3;

c("Disagree", "لا أوافق", "不同意", "En désaccord", "Stimme nicht zu", "असहमत", "反対", "Discordo", "Несогласен", "Estoy en desacuerdo", "Katılmıyorum") = 4;

c("Strongly Disagree", "لا أوافق بشدة", "坚决反对", "Particulièrement en désaccord", "Stimme überhaupt nicht zu", "दृढ़तापूर्वकअसहमत", "強く反対", "Discordo totalmente", "Совершенно несогласен", "Estoy completamente en desacuerdo", "Kesinlikle Katılmıyorum") = 5;

c("I don’t Know", "لا أعلم", "不知道", "Je ne sais pas", "Ich weiß nicht", "मुझेनहींपता", "分からない", "Não sei", "Не знаю", "No lo sé", "Bilmiyorum") = 6')
levels(data$gen_mod_food)
data$gen_mod_food <- ordered(data$gen_mod_food)


# DETER_PYHS_APPEAR
# Trim Whitespace
data$deter_phys_appear <- str_trim(data$deter_phys_appear, side = "both")
# Make as Factor
data$deter_phys_appear <- as.factor(data$deter_phys_appear)
levels(data$deter_phys_appear)
# Recode
data$deter_phys_appear <- recode(data$deter_phys_appear,
'c("No", "لا", "无", "否", "Non", "Nein", "नहीं", "いいえ", "Não", "Нет", "No", "Hayır") = "N";
c("Yes", "نعم", "有", "是", "Oui", "Ja", "हां", "はい", "Sim", "Да", "Sí", "Evet") = "Y"')
levels(data$deter_phys_appear)


# DETER_INTELL
# Trim Whitespace
data$deter_intell <- str_trim(data$deter_intell, side = "both")
# Make as Factor
data$deter_intell <- as.factor(data$deter_intell)
levels(data$deter_intell)
# Recode
data$deter_intell <- recode(data$deter_intell,
'c("No", "لا", "无", "否", "Non", "Nein", "नहीं", "いいえ", "Não", "Нет", "No", "Hayır") = "N";
c("Yes", "نعم", "有", "是", "Oui", "Ja", "हां", "はい", "Sim", "Да", "Sí", "Evet") = "Y"')
levels(data$deter_intell)


# DETER_STRENGTH
# Trim Whitespace
data$deter_strength <- str_trim(data$deter_strength, side = "both")
# Make as Factor
data$deter_strength <- as.factor(data$deter_strength)
levels(data$deter_strength)
# Recode
data$deter_strength <- recode(data$deter_strength,
'c("No", "لا", "无", "否", "Non", "Nein", "नहीं", "いいえ", "Não", "Нет", "No", "Hayır") = "N";
c("Yes", "نعم", "有", "是", "Oui", "Ja", "हां", "はい", "Sim", "Да", "Sí", "Evet") = "Y"')
levels(data$deter_strength)


# Print Factors and their levels as check
PrintLvls <- function(x) 
{print(data.frame(Lvls=sapply(x[sapply(x, is.factor)], nlevels), 
  Names=sapply(x[sapply(x, is.factor)], 
  function(y) paste0(levels(y), collapse=", "))), 
  right=FALSE) 
} 

PrintLvls(wechat)
PrintLvls(data)

data_with_free_text <- data

#remove free text fields from data
data$question_25 <- data$question_100 <- data$question_101 <- data$question_102 <- data$other_traits_alter <- NULL

#MERGE----------------------------------------------------------------------------------------

all <- rbind(data, wechat)

# Remove observations with ages < 10 and > 90

all <- all[which(all$age > 9 & all$age < 91), ]



#----------------------------------------------------------------------------------------



