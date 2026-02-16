library(reshape2)
data(airquality)
aq_melt <- melt(airquality, id.vars="Month")
print(aq_melt)
aq_cast <- dcast(aq_melt, Month ~ variable,
                 mean, na.rm=TRUE)
print(aq_cast)
