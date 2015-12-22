#colors-based analysis 

#import color data
colordata <- read.csv("~/R/flag project/colordata.csv")
flagcolors <- data.frame(colordata)
colors <- flagcolors[ ,2:8]

#import dataset
alldata <- read.csv("~/R/flag project/flag.data_edited.csv")
alldata <- data.frame(alldata)

#regression with color data
#area & population
col_area <- cbind(colors, alldata$area)
area_lm_fit <- lm(alldata$area ~ ., data = col_area) 
area_lm_preds <- predict(area_lm_fit, col_area)

