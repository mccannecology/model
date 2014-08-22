dataold <- read.csv("output30.csv")
data01 <- read.csv("output31a.csv")
data02 <- read.csv("output31b.csv")
data03 <- read.csv("output31c.csv")

head(data01)
class(data01)

temp<-rbind(dataold,data01,data02,data03)

write.csv(temp,"output31.csv")
