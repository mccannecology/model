getwd()
list.files()

# output30.csv should be in the /model folder and in /shiny_FP_SAV folder 
# output30.csv has 54,000 rows 

######################################################
# combine files from most recent simulation into one #
######################################################
data01 <- read.csv("output32a.csv")
data02 <- read.csv("output32b.csv")
data03 <- read.csv("output32c.csv")
data04 <- read.csv("output32d.csv")
data05 <- read.csv("output32e.csv")
data06 <- read.csv("output32f.csv")
data07 <- read.csv("output32g.csv")
data08 <- read.csv("output32h.csv")
data09 <- read.csv("output32i.csv")
data10 <- read.csv("output32j.csv")

# combine into single data frame 
temp<-rbind(data01,data02,data03,
            data04,data05,data06,
            data07,data08,data09,
            data10)

# check that it has the right # of rows (should be 18,000)
nrow(temp)

# write to file 
write.csv(temp,"output32.csv")

###########################################
# combine most recent simul w/ older ones #
###########################################
# read in the old data 
dataold <- read.csv("output30.csv")
colnames(dataold)

# the first column is the row # 
# this does not match up with the output32.csv which does not have row numbers as a col
# remove the row number from the old data 
dataold["X"]<-NULL

# combine with most recent simul
temp2 <- rbind(dataold,temp)

# check the # of rows (should be 72,000)
nrow(temp2)

# write to file 
write.csv(temp2,"output30.csv")
