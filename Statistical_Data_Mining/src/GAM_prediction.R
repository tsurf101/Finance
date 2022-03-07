install.packages("ROSE")
library(ROSE)
library(splines)
library(gam)

daily_counts_all = read.table("Cleaned_Data/daily_counts_all.csv", header=TRUE, sep=",") # Loading data 

table(daily_counts_all$SNOW)
prop.table(table(daily_counts_all$SNOW))
data_balanced_under <- ovun.sample(count ~ ., data = daily_counts_all, method = "under", N = 160, seed = 1)$data
table(data_balanced_under$count)
#data <- read.csv("~/Desktop/orie 5270/4740 lab/data.txt")
sapply(daily_counts_all,class)
#daily_counts_all$count <- as.numeric(daily_counts_all$count)
#daily_counts_all <- read.csv("~/Desktop/daily_counts_all.txt") #uncomment when using your local, crashes script  
cols <- c("WT01", "WT04", "WT09")

# Lapply here takes each column as part of list cols and converts it into a factor 
daily_counts_all[cols] <- lapply(daily_counts_all[cols], factor)
daily_counts_all$time_delta_sq = daily_counts_all$time_delta^2
daily_counts_all$day_sq = daily_counts_all$day^2

#sapply(daily_counts_all,class) check all the classes of the data
gam1 <- gam(count~weekday+s(time_delta_sq,2)+TMAX+WT09+PRCP+SNOW,data=daily_counts_all)
gam2 <- gam(count~weekday+s(time_delta_sq,2)+TMAX+WT09+WT04+PRCP+SNOW,data=daily_counts_all)
gam2 <- gam(count~weekday+s(time_delta_sq,2)+TMAX+WT09+WT04+WT01+PRCP+SNOW,data=daily_counts_all)


#all x's seem to have linear relationships with crimes,CI is very large when the smample size 
#is relatively small. Because most crimes happened in normal weather, so when 
#prcp and snow are 0 (CI is very tight) doesn't mean much things. Wind speed has almost no
#impact on number of crime happened in a day. shooting has positive impact, but as
#number of shootings increase, sample size decreases, the CI widens, 
#Anova analysis
anova(gam1, gam2, test="F")

#plot the gam with best p-value
par(mfrow=c(3,2))
plot(gam2, se=TRUE, col="red")

#May change shootings to a factor variable based on the level
#Level_of_shootings <- cut(daily_counts_all$shootings, breaks=c(-1, 0, 4, 10, 15, Inf), labels=paste("Level", 1:5, sep=""))
#daily_counts_all$shootings <- Level_of_shootings 

#Level_of_snow <- cut(daily_counts_all$SNOW, breaks=c(-1, 0, 0.5, 1, 2, Inf), labels=paste("Level", 1:5, sep=""))
#daily_counts_all$SNOW <- Level_of_snow