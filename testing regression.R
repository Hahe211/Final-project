###how to remove coeff from a regression 

lm0$coefficients <- lm0$coefficients[names(lm0$coefficients) != "pop75"]
#And you're good to go. To remove a lot of coefficients:

lm0$coefficients <- lm0$coefficients[!names(lm0$coefficients) %in% c("pop75","pop15")]


#exploring panel data
library(foreign)
library(tidyverse)
library(car)

panel <- read_rds("D1_interest.rds")


#print nice graph showing dips over Ramadan across countries by date in 5 small graphs

coplot(sum ~ date|geo,  type="l", data=panel) 





#graph with fun different displays for each country, somewhat hard to read
scatterplot(sum~date|geo, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=panel)


library(gplots)
#shows the differences in total values of porn searching for country- there are 261 data points in each but AE has the most and SA has the fewest
#not super useful
plotmeans(sum ~ geo, main="Heterogeineity across countries", data=panel) 

#Fixed effects using Least squares dummy variable model
#controlling for year and country significantly reduces the standard error

#first without controls
summary(lm(sum ~ in_ram + in_eid, data=panel))


#second with controls  - reduced standard error
# The coefficient of x1 indicates how much Y changes overtime, 
#controlling by differences in countries, when X increases by one unit. 
# Notice x1is significant in the LSDV model
summary(lm(sum ~ in_ram + in_eid+ factor(geo) -1 + factor(year) - 1, data=panel))

save <- lm(sum ~ in_ram + in_eid+ factor(geo) -1 + factor(year) - 1, data=panel)

stargazer(save, "text")




#using plm
library(plm)

