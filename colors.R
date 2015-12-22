#colors-based analysis 

#import color data
colordata <- read.csv("~/R/flag project/colordata.csv")
flagcolors <- data.frame(colordata)
colors <- flagcolors[ ,2:8]

#import dataset
alldata <- read.csv("~/R/flag project/flag.data_edited.csv")
alldata <- data.frame(alldata)

#regression with color data
#area
col_area <- cbind(colors, alldata$area)
area_lm_fit <- lm(col_area[ ,8] ~ red + green + blue + gold + white + black + orange, 
                  data = col_area) 
area_lm_preds <- predict(area_lm_fit, col_area)
#population
col_pop <- cbind(colors, alldata$pop)
pop_lm_fit <- lm(col_pop[ ,8] ~ red + green + blue + gold + white + black + orange, 
                 data = col_pop) 
pop_lm_preds <- predict(area_lm_fit, col_pop)
#need to group colors

