setwd("C:/Users/HP/Desktop/Business Analytics program/Business analytics session")
##Data Visualization

 library(readxl)
Weekly_Gasoline_prices <- read_excel("Weekly Gasoline prices.xls")

head(Weekly_Gasoline_prices )

plot(Weekly_Gasoline_prices, type="l", main="Time-series",
     col="red", lwd=2)

library(zoo)
WGP=zoo(Weekly_Gasoline_prices$`Gasoline Prices  (Dollars per Gallon)`, 
        Weekly_Gasoline_prices$Date)

WGP1=aggregate(WGP, as.yearqtr)

par(mfrow=c(2,1))
plot(Weekly_Gasoline_prices, type="l", main="Time-series Weekly",
     col="red", lwd=2)


plot(WGP1, type="l", main="Time-series Qtrly",
     col="green", lwd=2)

par(mfrow=c(1,1))

barplot(WGP1, ylab="Qtrly Prices", xlab="Date",
        col="Blue", main="Gasoline", border="red")

#################################################

library(readxl)
 Nations <- read_excel("Nations.xlsx")

 head(Nations)
Nations$Outlook=as.factor(Nations$Outlook)
 levels(Nations$Outlook)
 
 table(Nations$Outlook)
barplot(table(Nations$Outlook), main="Barplot") 
barplot(table(Nations$Outlook), main="Barplot", horiz = T) 
barplot(table(Nations$Outlook), main="Rating Outlook", col=rainbow(3),
        ylab="Freq", xlab="Outlook", border="Green") 

legend("topleft", c("Negative", "Positive", "Stable"),
       fill=rainbow(3), cex=1)
####################################################### 
 
library(readxl)
Soft_Drink <- read_excel("Cold drink data.xlsx")
summary(Soft_Drink)
head(Soft_Drink) 
table(Soft_Drink)
Soft_Drink=cbind.data.frame(table(Soft_Drink)) 
Soft_Drink
Soft_Drink$Relative_Freq= Soft_Drink$Freq/sum(Soft_Drink$Freq)
barplot(Soft_Drink$Relative_Freq, names.arg= Soft_Drink$Soft_Drink,
        col=c(1,2,3,4,5)) 
barplot(Soft_Drink$Relative_Freq, names.arg= Soft_Drink$Soft_Drink,
        col=rainbow(5)) 

barplot(Soft_Drink$Relative_Freq, names.arg= Soft_Drink$Soft_Drink,
        col=rainbow(5), horiz=T)

barplot(Soft_Drink$Relative_Freq, names.arg= Soft_Drink$Soft_Drink,
        col=rainbow(5)) 

legend("topright", legend=c("Ck", "Dck", "Pep", "Psi", "Spr"),
       fill=rainbow(5), cex=1)



pie(Soft_Drink$Relative_Freq, labels=Soft_Drink$Soft_Drink, 
    main="Soft_Drink", col=rainbow(5))


paste0(round(100*Soft_Drink$Relative_Freq,2), "%")

pie(Soft_Drink$Relative_Freq, 
    labels=paste0(round(100*Soft_Drink$Relative_Freq,2), "%"), 
    main="Soft_Drink", col=rainbow(5))

legend("topright", c("Ck", "Dck", "Pep", "Psi", "Spr"), cex=0.8,
       fill=rainbow(5))



library(plotrix)
pie3D(Soft_Drink$Relative_Freq, 
      labels=paste0(round(100*Soft_Drink$Relative_Freq,2), "%"), 
      main="Soft_Drink", col=rainbow(5))
legend("topleft", c("Ck", "Dck", "Pep", "Psi", "Spr"), cex=1,
       fill=rainbow(5))


###############################Plot Histogram
Var= sample(50:100, 10000, replace=T)
summary(Var)

hist(Var, xlab="Frequency", ylab="Variable", col=rainbow(20), border="green", 
     breaks=100)

par(mfrow=c(1,3))

hist(Var, xlab="Frequency", ylab="Variable", col=rainbow(20), border="green", 
     breaks=100)

hist(Var, xlab="Frequency", ylab="Variable", col=rainbow(20), border="green", 
     breaks=10)

hist(Var, xlab="Frequency", ylab="Variable", col=rainbow(20), border="green", 
     breaks=1)


par(mfrow=c(1,1))


hist(Var, xlab="Frequency", ylab="Variable", col=rainbow(20), border="green", 
     breaks=100, main="Histogram", cex.main=2, font.main=4, cex.axis=1,
     col.lab="green", col.main="red", col.axis="black", cex.lab=2)


m=hist(Var, xlab="Frequency", ylab="Variable", col=rainbow(20), border="green", 
     breaks=10)

text(m$mids, m$counts, labels = m$counts, adj=c(0.5, -0.5))


##########
par(mar=c(5,4,4,6)+0.1)
m=hist(Var, xlab="Frequency", ylab="Variable", col=rainbow(20), border="green", 
       breaks=10, main="Histogram")


par(new=T)

plot(density(Var), lwd=4, col="black", xlab="", ylab="",
     axes=F, main="")

axis(4, col="black", col.axis="black")

mtext("Cell Density", side=4, col="black", line=4)
######################################################
############Side by side/grouped and stacked
> library(readxl)
> Meal <- read_excel("Meal.xlsx")
head(Meal)
#####Grouped/stacked charg

barplot(as.matrix(Metal[c(1:3), c(2:6)]), col=rainbow(3),
        border="white", xlab="Price", ylab="Number_of_Rest")

legend("topleft", Meal$`Quality Rating`[1:3], cex=1,
       fill=rainbow(3))


barplot(as.matrix(Metal[c(1:3), c(2:6)]), col=rainbow(3),
        border="white", xlab="Price", ylab="Number_of_Rest", beside = T)

legend("topleft", Meal$`Quality Rating`[1:3], cex=1,
       fill=rainbow(3))

barplot(as.matrix(Metal[c(1:3), c(2:6)]), col=rainbow(3),
        border="white", xlab="Price", ylab="Number_of_Rest", beside = T, horiz=T)
















