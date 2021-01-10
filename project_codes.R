#Importing the dataset
rm(list=ls())
data=read.csv("cardio_train(project).csv",header=TRUE)
data

#Data cleaning:

#Eliminating patient ID column
Mydata=data[,-1]

#Null value detection
sapply(Mydata,function(x) sum(is.na(x)))
#So there are no null values in the dataset

head(Mydata)
summary(Mydata)
dim(Mydata)

#Identifying duplicate rows
df=unique(Mydata)
dim(df)
#So, there were 24 duplicate rows in the dataset

#Outliers detection
a=boxplot(df$age)$out
b=boxplot(df$height)$out
c=boxplot(df$weight)$out
d=boxplot(df$ap_hi)$out
e=boxplot(df$ap_lo)$out

length(a)
length(b)
length(c)
length(d)
length(e)

#So, there are 4 outliers in the variable 'Age', 519 outliers in variable 'Height', 1819 outliers in 'Weight', 1435 and 4632 outliers in variables 'ap_hi' and 'ap_lo' respectively which are to be treated

x=c()
for(i in 1:length(a)){
  x=c(x,which(df$age==a[i]))
}
y=c()
for(i in 1:length(b)){
  y=c(y,which(df$height==b[i]))
}
z=c()
for(i in 1:length(c)){
  z=c(z,which(df$weight==c[i]))
}
p=c()
for(i in 1:length(d)){
  p=c(p,which(df$ap_hi==d[i]))
}
q=c()
for(i in 1:length(e)){
  q=c(q,which(df$ap_lo==e[i]))
}

data_cardio=df

data_cardio$age[x]=min(data_cardio$age[-x])

data_cardio$height[data_cardio$height>(quantile(data_cardio$height,0.75)+(1.5*IQR(data_cardio$height)))]=max(data_cardio$height[-y])
data_cardio$height[data_cardio$height<(quantile(data_cardio$height,0.25)-(1.5*IQR(data_cardio$height)))]=min(data_cardio$height[-y])

data_cardio$weight[data_cardio$weight>(quantile(data_cardio$weight,0.75)+(1.5*IQR(data_cardio$weight)))]=max(data_cardio$weight[-z])
data_cardio$weight[data_cardio$weight<(quantile(data_cardio$weight,0.25)-(1.5*IQR(data_cardio$weight)))]=min(data_cardio$weight[-z])

data_cardio$ap_hi[data_cardio$ap_hi>(quantile(data_cardio$ap_hi,0.75)+(1.5*IQR(data_cardio$ap_hi)))]=max(data_cardio$ap_hi[-p])
data_cardio$ap_hi[data_cardio$ap_hi<(quantile(data_cardio$ap_hi,0.25)-(1.5*IQR(data_cardio$ap_hi)))]=min(data_cardio$ap_hi[-p])

data_cardio$ap_lo[data_cardio$ap_lo>(quantile(data_cardio$ap_lo,0.75)+(1.5*IQR(data_cardio$ap_lo)))]=max(data_cardio$ap_lo[-q])
data_cardio$ap_lo[data_cardio$ap_lo<(quantile(data_cardio$ap_lo,0.25)-(1.5*IQR(data_cardio$ap_lo)))]=min(data_cardio$ap_lo[-q])

#For removal of outliers, for each variable, the values greater than Q3+1.5IQR are replaced by the maximum of the values other than the outliers and the values less than Q1-1.5IQR are replaced by the minimum of the values other than the outliers.

par(mfrow=c(3,2))
boxplot(data_cardio$age,main="Boxplot of Age")
boxplot(data_cardio$height,main="Boxplot of Height")
boxplot(data_cardio$weight,main="Boxplot of Weight")
boxplot(data_cardio$ap_hi,main="Boxplot of Systolic b.p.")
boxplot(data_cardio$ap_lo,main="Boxplot of Diastolic b.p.")

#From the boxplots of each variable we can ensure that there are no more outliers in the dataset

dim(data_cardio)
#Thus we have our cleaned dataset 'data_cardio' which is free from missing values, duplicate values & outliers

#Univariate Analysis

par(mfrow=c(3,2))
hist(data_cardio$age,main="Histogram of Age",prob=TRUE)
hist(data_cardio$height,main="Histogram of Height",prob=TRUE)
hist(data_cardio$weight,main="Histogram of Weight",prob=TRUE)
hist(data_cardio$ap_hi,main="Histogram of Systolic b.p.",prob=TRUE)
hist(data_cardio$ap_lo,main="Histogram of Diastolic b.p.",prob=TRUE)

#Height and weight are more or less symmetrically distributed and age is slightly negatively skewed

par(mfrow=c(3,2))
plot(density(data_cardio$age),main="Density Plot of Age")
plot(density(data_cardio$height),main="Density Plot of Height")
plot(density(data_cardio$weight),main="Density Plot of Weight")
plot(density(data_cardio$ap_hi),main="Density Plot of Systolic b.p.")
plot(density(data_cardio$ap_lo),main="Density Plot of Diastolic b.p.")
plot(density(data_cardio$ap_hi-data_cardio$ap_lo),main="Density Plot of pulse pressure")

#Most of the patients we have are between the age of 18000 to 22000 days, are of height 160 to 170 cm, weight 60 to 80 kg. Most of the patients have systolic bp 120 and diastolic bp 80 i.e. b.p. is in normal range.

#Bivariate Analysis

summary(data_cardio)
round((cor(data_cardio[,-c(12)])),4)
cor.test(data_cardio$ap_hi,data_cardio$ap_lo)$p.value

#So there is significant correlation between Systolic and Diastolic b.p.

par(mfrow=c(1,1))
boxplot(data_cardio$age~data_cardio$cardio)
plot(density(data_cardio$age[data_cardio$cardio==1]),main="Age distribution of patients who suffer from heart disease")

#Thus on an average, people older in age suffer more heart disease than those who are younger. More precisely patients having age between 20,000-22,000 days i.e. between 55 to 60 years approximately suffer mostly with heart disease


