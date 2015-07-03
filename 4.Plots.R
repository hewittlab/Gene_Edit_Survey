# Need to have run the 1.Data_prep.R script and have the resulting data in this workspace

library(plyr)
library(likert)
library(ggplot2) # Plots

## Need to create a "Figures symlink" folder in the WD to save plots to.

#LIKERT----------------------------------------------------------------------------------------

# LIKERT PLOT

likert_vec <- c("kids_cure_life","kids_cure_debil","embr_prev_life","embr_prev_debil","edit_for_nondis","gen_mod_food")

likert_df <- all[,likert_vec]

levels(likert_df$kids_cure_life)[levels(likert_df$kids_cure_life)=="1"] <- "Strongly Agree"
levels(likert_df$kids_cure_life)[levels(likert_df$kids_cure_life)=="2"] <- "Agree"
levels(likert_df$kids_cure_life)[levels(likert_df$kids_cure_life)=="3"] <- "Neutral"
levels(likert_df$kids_cure_life)[levels(likert_df$kids_cure_life)=="4"] <- "Disagree"
levels(likert_df$kids_cure_life)[levels(likert_df$kids_cure_life)=="5"] <- "Strongly Disagree"
levels(likert_df$kids_cure_life)[levels(likert_df$kids_cure_life)=="6"] <- "Don't Know"

likert_df<-rename(likert_df, c("kids_cure_life"="How much do you agree with the use of genetic editing of cells in children or adults to cure a life threatening disease?","kids_cure_debil"="How much do you agree with the use of genetic editing of cells in children or adults to cure a debilitating disease?","embr_prev_life"="How much do you agree with the use of genetic editing of cells in embryos to prevent a life threatening disease?","embr_prev_debil"="How much do you agree with the use of genetic editing of cells in embryos to prevent a debilitating disease?","edit_for_nondis"="How much do you agree with the use of genetic editing of cells in embryos to alter any non-disease characteristic - such as memory, eye colour or height?","gen_mod_food"="How much do you agree with the use of genetically modified food?"))

likert_df <-likert(likert_df)

summary(likert_df)

# plot(likert_df, type="density")
# ggsave(file="Figures symlink/Likert_Density_Faceted.eps", width=12, height=10)
# 
# plot(likert_df, type="density", facet=F)
# ggsave(file="Figures symlink/Likert_Density.eps", width=12, height=10)
# 
# plot(likert_df, type="heat", ordered=T)
# ggsave(file="Figures symlink/Likert_Heat.eps", width=12, height=10)

plot(likert_df, ordered=F)
ggsave(file="Figures symlink/Likert_Bar.eps", width=12, height=10)

#POPN----------------------------------------------------------------------------------------

# POPULATION PYRAMID

ggplot(data=all,aes(x=age,fill=sex)) + 
  geom_bar(subset=.(sex=="F")) + 
  geom_bar(subset=.(sex=="M"),aes(y=..count..*(-1))) + 
  scale_fill_grey(start = 0.1, end = 0.4, na.value = "grey50") +
  scale_x_continuous(breaks=seq(0,100,10),labels=abs(seq(0,100,10))) +
  scale_y_continuous(breaks=seq(-440,200,80),labels=abs(seq(-440,200,80))) + 
  coord_flip()
ggsave(file="Figures symlink/Pyramid_Plot.eps", width=12, height=10)

#----------------------------------------------------------------------------------------


