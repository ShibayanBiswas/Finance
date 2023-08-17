#Data Handling and Data Cleaning with R
setwd("C:/Users/HP/Desktop/Simply learn/R_Simplilearn_official (1)/R_Simplilearn_official/Module 3 Exploratory Data Analysis and Visualisation using R/Data Cleaning Handling and Visualizaiton")
Data= read.csv("Salary.csv")
dim(Data)

summary(Data)
brief(Data)
head(Data,8)
str(Data)
colnames(Data)

###Changings the names
colnames(Data)[1]="Name"
colnames(Data)[2:4]=c("Title", "ID", "AgencyName")
colnames(Data)
head(Data)
#####
#############################Cleaning the dataframe
x= c(0, NA, 2, 3, 4, -0.5, 0.2)
x
x>2

!is.na(NA)

x>2 & !is.na(x)

(x==0 | x==2)

(x==0 | x==2) & !is.na(x)

is.nan(0/0)
is.infinite(1/0)

head(Data)
Data_1=Data

Data_1[1000, 5]=NA
Data_1[3000, 2]=NA
Data_1[4000, 3]=NA


sum(is.na(Data))
sum(is.na(Data_1))
all(!is.na(Data_1))
all(!is.na(Data))

Data_1[is.na(Data_1)]=0
sum(is.na(Data_1))
all(!is.na(Data_1))


######################
DF= data.frame(a=c(NA, 1, 2)), b=c("One", NA, "Three")
DF
subset(DF, !is.na(a))
subset(DF, !is.na(b))

subset(DF, complete.cases(DF))
na.omit(DF)


######
library(car)
head(Freedman)
str(Freedman)
summary(Freedman)
rm(Freedman)

median(Freedman$density)
median(Freedman$density, na.rm=T)

mean(Freedman$density)
mean(Freedman$density, na.rm=T)

Freedman.good=na.omit(Freedman)
summary(Freedman.good)

Freedman_notav= Freedman[!complete.cases(Freedman),]

Freedman_notav


############
library(UsingR)

x=babies$dwt
summary(x)
x[x==999]=NA
range(x)
summary(x)
range(x, na.rm=T)


######Removal of non-unique columns
head(Data)
Data_2=Data

dim(Data_2)

Data_3= rbind.data.frame(Data_2, Data_2[1:500, ])
dim(Data_3)

Data_4= unique(Data_3)
dim(Data_4)

##########################Data Handling
#####Selection of columns and rows in R
head(iris)
iris[,3]
head(iris[,3])

iris[, c(3,5)]
iris[, c(3:5)]
head(iris[, c(3:5)])
iris[c(4:10), c(3:5)]

iris[, c("Species", "Petal.Width")]
head(iris[, c("Species", "Petal.Width")])



#######Creation of new variables
iris$Petal.Ratio=  iris$Petal.Length/iris$Petal.Width
iris$Sepal.Ratio=  iris$Sepal.Length/iris$Sepal.Width
head(iris)

data(iris)
head(iris)
#####Extracting observations
iris[iris$Petal.Width>0.5 & iris$Species=="setosa",]

subset(iris, Petal.Width>0.5  &  Species=="setosa")
#########


#####Summarize the observations

summary(iris)
str(iris)
brief(iris)

summarise(iris,
          Petal.lenght.mean= mean(Petal.Length),
         Sepal.lenght.mean= mean(Sepal.Length),
         Petal.lenght.sd= sd(Petal.Length),
         Sepal.lenght.sd= sd(Sepal.Length),
         
         
         )


library(car)

dim(Davis)
head(Davis)

output=data.frame(matrix(nrow=dim(Davis)[1], ncol= dim(Davis)[2]))

dim(output)

head(output)
colnames(output)=c("Gender", "Wt", "Ht", "Repwt", "Repht")
head(output)

output$Gender= Davis$Gender
output$Wt= Davis$weight
output$Ht= Davis$height
output$Repwt= Davis$repwt
output$Repht= Davis$repht

head(output)
################Working with factor variables
library(UsingR)

head(Cars93)

d= Cars93[1:3, 1:4]
str(d)
summary(d)


d[3,4]=NA
d[1,1]=NA
d

d[3, c(2,4)]= list("A3", 40)
d
class(d$Model)

levels(d$Model)
d$Model= droplevels(d$Model)

levels(d$Model)

levels(d$Model)= c(levels(d$Model), c("A3", "A4", "A6"))
levels(d$Model)

d[3, c(2,4)]= list("A3", 40)
d[4,]=list("Audi", "A4", "Midsize", 35)
d
d=rbind(d,list("Audi", "A4", "Midsize", 35) )
d
d[,5]=d$Min.Price*1.3
d
colnames(d)[5]="modprice"
d
d$modprice=d$Min.Price*1.3
d
d= within(d, {modprice= Min.Price*1.3})
d



#####Transform the dataframes across long and wide formats

Speed.1= c(850, 740, 900, 1070, 930, 850)
Speed.2= c(930, 650, 760, 810, 1000, 1000)
Speed.3= c(800, 790, 760, 800, 880, 880)
Speed.4= c(800, 790, 760, 800, 880, 880)
Speed.5= c(800, 790, 760, 800, 880, 880)
id= c(1,2,3,4,5,6)
Run= c("A", "B", "C", "D", "E", "F")
Speed= cbind.data.frame(id, Run, Speed.1, Speed.2,Speed.3, Speed.4, Speed.5)
head(Speed)
summary(Speed)
str(Speed)
library(reshape2)

long= melt(Speed, id.vars= names(Speed)[1:2], variable.name= "Speed")
head(long, 10)

wide= dcast(long, id+Run ~Speed)
head(wide)

####Merging dataframes

V1= c("The Avengers", "Dark Knight", "Hunger Games", "Skyfall", "Hobbit")
V2= c(6233357910, 448139099, 408010692, 304360277, 303003568)
domestic=cbind.data.frame(V1, V2)
head(domestic)
colnames(domestic)=c ("Name", "Domestic")
head(domestic)

V3= c("The Avengers", "Dark Knight", "Ice age", "Skyfall", "Hobbit")
V4=c(6233357910, 448139099, 408010692, 304360277, 303003568)

foreign= cbind.data.frame(V3, V4)
head(foreign)
colnames(foreign)= c("name", "foreign")
head(foreign)

Final=merge(domestic, foreign, by.x="Name", by.y="name")
head(Final)

colnames(foreign)= c("Name", "foreign")
Final=merge(domestic, foreign, by="Name")
head(Final)

Final=merge(domestic, foreign, by="Name", all=T)

Final=merge(domestic, foreign, by="Name", all.x=T)
Final


Final=merge(domestic, foreign, by="Name", all.y=T)
Final


