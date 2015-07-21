packages <- c("jsonlite", "stringr", "plyr", "car", "doBy", "maps", "ggplot2", "MASS", "ordinal", "lmtest", "VGAM", "likert")
print(packages)
for(i in 1:length(packages)) {
  install.packages(packages[i])
}
rm(list=ls())
