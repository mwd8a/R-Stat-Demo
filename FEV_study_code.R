library(ggplot2)

# load the data
load(url('http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/FEV.sav')) 

# make sure that "FEV" loaded
ls()

# fyi: to make a csv version of FEV, run the following code
# write.table(FEV, file='FEV.csv', sep=',', col.names=NA)

#### Exploration of FEV ####

# the first six rows of FEV
head(FEV) 

# get summary statistics for all variables
summary(FEV)

# scatter plot of fev by age with smoke indicated
plot1 <- ggplot(FEV, aes(x=age, y=fev, colour=smoke)) + geom_point()
plot1 <- plot1 + labs(x="Age", y="Forced Expiratory Volume", 
                      title="FEV by Age with Indicated Smoking Status")
plot1

# find out the age of the youngest male smokers
summary(
        subset(FEV,
               subset = smoke=="current smoker" & sex=="male",
               select = age )
        )

# find out the age of the youngest female smokers
summary(
        subset(FEV,
               subset = smoke=="current smoker" & sex=="female",
               select = age )
        )

# create a contingency table for smoke by sex
cats <- subset(FEV, select = c(sex,smoke))
tcats <- table(cats)
tcats

prop.table(tcats, margin=1) # gives proporitons by sex

# use a chi-square test for proportions without Yates correction
chisq.test(tcats, correct=FALSE)

# boxplot of fev by smoke and sex
plot2 <- ggplot(FEV, aes(smoke, fev)) + geom_boxplot(aes(fill = sex)) 
plot2 <- plot2 + labs(title="Boxplots of fev by smoke for each gender")
plot2

# linear regression model for response FEV and explanatory variables AGE, HEIGHT, SEX, and SMOKE 
fit <- lm(fev ~ age + height + sex + smoke, data=FEV)
summary(fit)

#### Restriction of FEV to teenagers ####

# restrict FEV to age 13+
FEV2 <- subset(FEV, subset = age>= 13)

# scatterplot of fev by age with smoke coding (age 13+)
plot3 <- ggplot(FEV2, aes(x=age, y=fev, colour=smoke)) + geom_point()
plot3 <- plot3 + labs(x="Age", y="Forced Expiratory Volume", 
                      title="FEV by Age with Indicated Smoking Status")
plot3

# check out smoke status by sex (age 13+)
cats2 <- subset(FEV2, select = c(sex,smoke))
tcats2 <- table(cats2)
tcats2

prop.table(tcats2, 1) # gives proporitons by sex (age 13+)

# use a chi-square test for proportions without Yates correction (age 13+)
chisq.test(tcats2, correct=FALSE)

# boxplot of FEV by smoke and sex (age 13+)
plot4 <- ggplot(FEV2, aes(smoke, fev)) + geom_boxplot(aes(fill = sex)) 
plot4 <- plot4 + labs(title="boxplot of FEV by smoke and sex (age 13+)")
plot4

# histogram of fev for males (age 13+)
plot5 <- ggplot(subset(FEV2, subset = sex == "male"), 
                aes(fev)) + geom_histogram()
plot5 <- plot5 + labs(x="Forced Expiratory Volume", 
                      title="fev for Males (age 13+)")
plot5

# histogram of fev for females (age 13+)
plot6 <- ggplot(subset(FEV2, subset = sex == "female"),
                aes(fev)) + geom_histogram()
plot6 <- plot6 + labs(x="Forced Expiratory Volume", 
                      title="fev for Females (age 13+)")
plot6

# t-test for fev by sex (age 13+)
t.test(fev ~ sex, data = FEV2)

# split-up the data set FEV2 by sex
FEV2m <- subset(FEV2, subset = sex == "male")
FEV2f <- subset(FEV2, subset = sex == "female")

# t-test for fev by smoke (males, age 13+)
t.test(fev ~ smoke, data = FEV2m)

# t-test for fev by smoke (females, age 13+)
t.test(fev ~ smoke, data = FEV2f)

# scatterplot of fev by age with smoke coding (males, age 13+)
plot7 <- ggplot(FEV2m, aes(x=age, y=fev, colour=smoke)) + geom_point()
plot7 <- plot7 + labs(x="Age", y="Forced Expiratory Volume", 
                      title="FEV by Age with Indicated Smoking Status, Males Age 13+")
plot7

# scatterplot of fev by age with smoke coding (females, age 13+)
plot8 <- ggplot(FEV2f, aes(x=age, y=fev, colour=smoke)) + geom_point()
plot8 <- plot8 + labs(x="Age", y="Forced Expiratory Volume", 
                      title="FEV by Age with Indicated Smoking Status, Females Age 13+")
plot8