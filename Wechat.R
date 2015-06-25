#IMPORT----

library(stringr) # Trim whitespace 
library(plyr) # Various
library(car) # Recode variables


# CHANGE WORKING DIRECTORY HERE

setwd('~/Dropbox/Research Projects/2015/Gene Edit Survey')
wechat <- read.csv(file="wechat.csv", header=T)

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
wechat$language <- "CHINESE"
wechat$ethnicity <-

# Need to remove # from some entried of age variable
wechat$age <- gsub("#", "", wechat$age)
wechat$age <- as.numeric(as.character(wechat$age))
wechat$YOB <- 2015-wechat$age

# Delete unnecessary variables
wechat$答卷编号 <- wechat$来源 <- wechat$昵称.微信. <- wechat$性别.微信. <- wechat$城市.微信. <- wechat$状态 <- wechat$Q4_请填写您所居住的国家 <- wechat$Q7_请填写您所信仰的宗教名称 <- wechat$Q11_请填写您在保健医疗行业所从事的职业 <- wechat$Q23_如果您可以安全的进行胚胎基因改造.您会使用这项技术改变什么._其他跟健康无关的特征 <- wechat$Q23_如果您可以安全的进行胚胎基因改造.您会使用这项技术改变什么._不做任何改变 <- wechat$Q24_请填写您会改变的其他跟健康无关的特征 <- wechat$Q26_我们对您回答这些的理由很感兴趣.您是否愿意描述一下影响您这些回答的理由或者影响您对于人类基因工程的态度的因素.谢谢 <- wechat$Q27_请填写您的理由 <- NULL

# Reorder
wechat <- wechat[c("createdAt", "updatedAt", "time_to_do", "ip", "language", "country", "sex", "YOB", "age", "ethnicity", "wealth", "edu_level", "worked_health", "worked_health_type", "heard_about", "genetic_cond", "genetic_cond_affected", "genetic_cond_type", "religion", "religion_type", "kids_cure_life", "kids_cure_debil", "embr_prev_life", "embr_prev_debil", "edit_for_nondis", "deter_phys_appear", "deter_intell", "deter_strength", "gen_mod_food")]


