data(ChickWeight)
sorted_cw <- ChickWeight[order(ChickWeight$weight), ]
print(sorted_cw)
library(reshape2)
cw_melt <- melt(ChickWeight, id=c("Time","Diet"))
cw_cast <- dcast(cw_melt, Diet ~ variable, mean)
print(cw_cast)
