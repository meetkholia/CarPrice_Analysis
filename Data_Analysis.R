#===============================================================================
#  USED GM CAR PRICE PREDICTIONS      
#     	MEET KHOLIA
#	INFORMATION TECHNOLOGY MANAGEMENT 
#	  SCHOOL OF APPLIED TECHNOLOGY
#	 ILLINOIS INTITUTE OF TECHNOLOGY
#===============================================================================

#===============================================================================
# USED GM CAR PRICE PREDICTOR
#===============================================================================

# Reading the file into R object(GM)
GM <- read.csv(file="G:/GM_Car_Data.csv",header=TRUE, sep=",");

# Libraries used 
library(xlsx) 
library(effects) 
library(lattice) 
library(ggplot2) 
library(devtools)
#install_github("dgrtwo/broom") library(broom)

#=============================================================================== 
## EDA
#===============================================================================
EDA<-summary(GM); write.csv(EDA,"G:/EDA.csv")
# Analysing data types of the variable names(GM) #Gives all the variable names head(GM) # gives first few instances of data str(GM) # gives structure of the data
# deleting unnecessary blank columns 
GM$X<-NULL
GM$X.1<-NULL
GM$X.2<-NULL
GM$X.3<-NULL
GM$X.4<-NULL
GM$X.5<-NULL
GM$X.6<-NULL
GM$X.7<-NULL
GM$X.8<-NULL
GM$X.9<-NULL
GM$X.10<-NULL GM$X.11<-NULL str(GM)
# converting Cylinder, Liter,Doors, Cruise,Sound and Leather..
# ..into categorical variables
GM$Cylinder <- as.factor(GM$Cylinder)
GM$Liter <- as.factor(GM$Liter)
GM$Doors <- as.factor(GM$Doors)
GM$Sound <- as.factor(GM$Sound)
GM$Cruise <- as.factor(GM$Cruise)
GM$Leather <- as.factor(GM$Leather)
# Final data structure review
str(GM)



## SCATTER PLOT
# Scatter plot of Price and Mileage
plot(GM$Mileage, GM$Price, xlab="Mileage", ylab="Price", main="Price v/s Mileage"
     ,pch=20)
ggplot(GM,aes(x=Mileage,y=Price)) +geom_point() + ggtitle("Scatter plot of Price/Mileage")

# further exploration
#colors <-c("Grey","Blue","Red","Green","Yellow","Violet","Orange")

#which type has the highest freq 
#barplot(table(GM$Type),xlab="Type",ylab="Freqency",col=colors) ggplot(GM,aes(x=Type,fill=Type))+ geom_bar()+
ggtitle("Barplot of Type")

#which make has the highest freq
#barplot(table(GM$Make),xlab="Make",ylab="Freqency",col=colors) ggplot(GM,aes(x=Make,fill=Make))+ geom_bar()+
ggtitle("Barplot of Make")

#which trim(sedan) has the highest freq
barplot(table(GM$Trim),xlab="Trim",ylab="Freqency",col=colors)
#we can't figure out anything out of this barplot(table(GM$Model),col=colors)


#statistical summary of Price Variable summary(GM$Price)
hist(GM$Price, xlab="Price", main="Histogram of Price",col= colors,breaks=500) ggplot(GM,aes(x=Price))+ geom_histogram(binwidth=500)+ ggtitle("Histogram of Price") quantile(GM$Price,c(.985))


#Analysis of Make and Type
#mean by Make
tapply(GM$Price,GM$Make,mean)

#aggregate(GM$Price,by=list(GM$Make),FUN=mean)
#boxplot(GM$Price ~ GM$Make,main="Boxplot Price/Make",xlab="Make",ylab="Price",col=colors) qplot(Make,Price,data=GM,main="Boxplot Price/Make",xlab="Make",ylab="Price",geom="boxplot")+ stat_summary(fun.y=mean,shape=1,col="red",geom="point")
#mean by type
tapply(GM$Price,GM$Type,mean)

#aggregate(GM$Price,by=list(GM$Type),FUN=mean)
#boxplot(GM$Price ~ GM$Type,main="Boxplot Price/Type",xlab="Type",ylab="Price",col=colors) qplot(Type,Price,data=GM,main="Boxplot Price/Type",xlab="Type",ylab="Price",geom="boxplot") + stat_summary(fun.y=mean,shape=1,col="red",geom="point")
qplot(Cylinder,Price,data=GM,main="Boxplot 
      Price/Cylinder",xlab="Cylinder",ylab="Price",geom="boxplot")


#=============================================================================== 
## FINDING OUTLIERS 
#===============================================================================
# Boxplot pf Price Variable
boxplot(GM$Price,horizontal=TRUE,col="Grey",xlab="Price",main="Boxplot for Price") boxplot.stats(sort(GM$Price))
new_GM <- GM[GM$Price >50000,]
# we will consider Price >50000 as outliers
# finding which make has price >50000
GM[GM$Price > 50000,"Make"]
which.max(table(GM[GM$Price >50000,"Make"]))
barplot(table(GM[GM$Price >50000,"Make"]),col="Red",xlab="Make",ylab="Count"         ,main="Histogram of Make for outliers")
#finding which type of Cadillac Made car has price >50000 GM[GM$Price > 50000 & GM$Make == "Cadillac","Type"] which.max(table(GM[GM$Price >50000 & GM$Make == "Cadillac","Type"])) barplot(table(GM[GM$Price >50000 & GM$Make == "Cadillac","Type"]), col=c("red","blue"),xlab="Type",ylab="Count",main="Histogram of Type for outliers")
#finding which trim of Caddilac made Convertible car has price >50000  GM[GM$Price >50000 & GM$Make == "Cadillac" & GM$Type == "Convertible","Trim"]
which.max(table(GM[GM$Price > 50000& GM$Make == "Cadillac" & GM$Type == "Convertible","Trim"]))
#no significance of the following barplot because Trim has 47 levels
# barplot(table(GM[GM$Price > 50000& GM$Make == "Cadillac" & GM$Type == "Convertible","Trim"])) #       ,col=colors,xlab="Trim",ylab="Count")

#=============================================================================== 
##MORE EXPLORATION
#===============================================================================

# Boxplot of Price by Make and Type ggplot(GM,aes(x=Type,y=Price),fill=Make) + geom_boxplot() + facet_wrap(~ Make) + xlab("Make") + ylab("Price") + labs(fill="Type")+
ggtitle("Exploration Boxplot") stat_summary(fun.y=mean,shape=1,col="red",geom="point")
#Histogram of Price by Make and Type ggplot(GM,aes(x=Price,fill=Make))+ geom_histogram(binwidth=5)+ facet_wrap(~Type)+ ggtitle("Histogram of Price")

#=============================================================================== 
## CORRELATION (PREARSON'S CORRELATION) 
#===============================================================================
# Correlation between Price and Mileage cor(GM$Price,GM$Mileage,method="pearson") cor.test(GM$Price,GM$Mileage,method="kendall")
t.test(GM$Price, GM$Mileage)

#=============================================================================== 
## LINEAR REGRESSION MODEL
#=============================================================================== 
GM_lm <- lm(Price ~ Mileage, data=GM)

# To write the results into csv x<-GM_lm$coefficients
# this will only write estimate, it won't include std.erro,statistic and p-value write.csv(x,file="G:/Reg1.csv")
# using tidy function of broom lib to write everything into csv tidy_gm <- tidy(GM_lm)
tidy_gm
write.csv(tidy_gm,"G:/reg.csv")
#Model Summary summary(GM_lm) attributes(GM_lm) plot(GM_lm)
# Fit Plot for Price with 95% Confidence interval and regression equation windows();
lm <- ggplot(GM,aes(x=Mileage,y=Price)) +geom_point() print(lm)
lm + stat_smooth(method="lm",formula= y ~ x,size=1) + annotate("text",x=40000,y=68000,label="Y = -0.1725*X + 24764.55899",col="blue",size=5)+ annotate("text",x=40000,y=65000,label="R- Squared Value = 0.02046",col="blue",size=5)+ ggtitle("Fit Plot for Price")
# annotate("rect",xmin=0,xmax=45000,ymin=51500,ymax=72000,alpha=0.2)
# {
#   plot(GM$Mileage, GM$Price, xlab="Mileage", ylab="Price", main="Fit Plot for Price",pch=20)
#   fitted <- predict(GM_lm,interval="confidence")
#   lines(GM$Mileage,fitted[,"fit"],col="Blue")
#   lines(GM$Mileage,fitted[,"lwr"],lty="dotted",col="Red")
#   lines(GM$Mileage,fitted[,"upr"],lty="dotted", col="Red")
#   abline(coef(GM_lm)[1:2],col="Blue")
#   text(40000, 70000, adj=c(0,0),labels="Price = -0.17*Mileage + 24764.55",col="Blue")
#   text(40000,68000,adj=c(0,0),labels="R-squared value : 0.02046",col="Blue")
# }

##PREDICTIONS
# Prediction with Mileage = 30000
#sample.data <- data.frame(Mileage=30000)
#predict(GM_lm,sample.data)
#predict(GM_lm,sample.data,interval="confidence")
#predict(GM_lm,sample.data,interval="predict")

#=============================================================================== 
## ANOVA
#===============================================================================
# considering all the categorical variables in ANOVA Model
GM_anova<- aov(Price ~ Make+Model+Type+Cylinder+Trim+
                 Cruise+Sound,data=GM)
summary(GM_anova) anova_gm <- summary(GM_anova)
#saving the summary in file
capture.output(anova_gm,file="G:/anova_sum.csv")
#Same procedure with highly significant variables GM_anova1<-aov(Price ~ Make+Model+Type+Cylinder,data=GM) summary.lm(GM_anova1)
# ANOVA with factors
GM_anova_fact <- aov(Price ~ Make+Type+Cylinder+Leather+Cruise+Sound+Liter, data=GM)
GM_anova_fact1 <- aov(Price ~ Model+Trim,data=GM)
GM_anova_fact2 <- aov(Price ~ Doors,data=GM)
#to see factor significance we use summary.lm or we  can also input factor level # argumetns in the aov function eg. instead of Make we type factors(Make) summary.lm(GM_anova_fact) summary.lm(GM_anova_fact1) summary.lm(GM_anova_fact2)
#Interactions between Make,Model and Type GM_anova2<-aov(Price ~ Make*Model*Type,data=GM) summary(GM_anova2)

#=============================================================================== 
##TUKEY_KRAMER TEST
#===============================================================================
#to analize the variance between different levels of data
#if p-adj is 1, it means statistically both levels are the same(not much variance)
GM_anova3 <- aov(Price ~ Make+Model+Type+Cylinder,data=GM) summary(GM_anova3) a<-TukeyHSD(GM_anova3)
# str(a)
# attributes(a$Model)
# a$Model[,"p adj"]
