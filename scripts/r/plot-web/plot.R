
library(ggplot2)
dat <- read.csv('aggregate-csv')
png('mighty-perfs.png')
print(qplot(x=rps, data=dat, fill=factor(gc.area.size), xlab="Server speed (Req per sec.).", ylab="Number of experiments.") + scale_fill_discrete("GC Area-Size (MB)"))
dev.off()
