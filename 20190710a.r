install.packages(c("VIM", "mice"))

par(ask=TRUE)

data(sleep, package="VIM")

sleep[complete.cases(sleep),]

sleep[!complete.cases(sleep),]
