library(car)
setwd("~/Pulpit/Moje/STUDIA II/Lead in IT/new/Cechy/Product")
Brutto<-read.csv("Brutto.csv", sep=",", header=TRUE)
Brutto1<-Brutto[,2]
Brutto2<-Brutto[,3]
Brutto3<-Brutto[,4]
Brutto4<-Brutto[,5]

Count<-read.csv("Count.csv", sep=",", header=TRUE)
Count1<-Count[,2]
Count2<-Count[,3]
Count3<-Count[,4]
Count4<-Count[,5]

Hour<-read.csv("Hour.csv", sep=",", header=TRUE)
Hour1<-Hour[,2]
Hour2<-Hour[,3]
Hour3<-Hour[,4]

ReceiptPrice<-read.csv("ReceiptPrice.csv", sep=",", header=TRUE)
ReceiptPrice1<-ReceiptPrice[,2]
ReceiptPrice2<-ReceiptPrice[,3]
ReceiptPrice3<-ReceiptPrice[,4]
ReceiptPrice4<-ReceiptPrice[,5]

UniqueProductsCount<-read.csv("UniqueProductsCount.csv", sep=",", header=TRUE)
UniqueProductsCount1<-UniqueProductsCount[,2]
UniqueProductsCount2<-UniqueProductsCount[,3]
UniqueProductsCount3<-UniqueProductsCount[,4]
UniqueProductsCount4<-UniqueProductsCount[,5]

stat_Day_of_month<-read.csv("stat_Day_of_month.csv", sep=",", header=FALSE)
stat_Day_of_month1<-stat_Day_of_month[,2]
stat_Day_of_month2<-stat_Day_of_month[,3]
stat_Day_of_month3<-stat_Day_of_month[,4]

stat_Day_of_week<-read.csv("stat_Day_of_week.csv", sep=",", header=FALSE)
stat_Day_of_week1<-stat_Day_of_week[,2]
stat_Day_of_week2<-stat_Day_of_week[,3]
stat_Day_of_week3<-stat_Day_of_week[,4]

#1,2 ceche
#gamma
hist(Brutto1)
hist(Brutto1[Brutto1<30],breaks=100)
#gamma
hist(Brutto2[Brutto2<30],breaks=40) 
hist(Brutto3[Brutto3<4],breaks=40)
table(Brutto3)
hist(Brutto4[Brutto4<30],breaks=400)
cor(Brutto1,Brutto2)
cor(Brutto1,Brutto3)
cor(Brutto1,Brutto4)
cor(Brutto2,Brutto3)
cor(Brutto2,Brutto4)
cor(Brutto3,Brutto4)


#dwie klasy czy cos bylo = 1 i wieksze 
hist(Count1[Count1<2],breaks=50)
table(Count1)
#dwie klasy czy cos bylo = 1 i wieksze 
hist(Count2[Count2<30],breaks=40) 
table(Count2)
hist(Count3)
hist(Count3[Count3<2],breaks=400)
table(Count3)
hist(Count4[Count4<2],breaks=400)
cor(Count1,Count2)
cor(Count1,Count3)
cor(Count1,Count4)
cor(Count2,Count3)
cor(Count2,Count4)
cor(Count3,Count4)


#dwie klasy czy cos bylo = 1 i wieksze 
hist(Hour1,breaks=20)
table(Hour1)
#dwie klasy czy cos bylo = 1 i wieksze 
hist(Hour2,breaks=40) 
hist(Hour2[Hour2<30],breaks=40) 
Hour21<-round(Hour2)-1
table(Hour2)
hist(Hour21,breaks=50)
hist(Hour3,breaks=300)
hist(Hour3[Hour3<1],breaks=40)
table(Hour3)
hist(Hour4[Hour4<2],breaks=400)
cor(Hour1,Hour2)
cor(Hour1,Hour3)
cor(Hour1,Hour4)
cor(Hour2,Hour3)
cor(Hour2,Hour4)
cor(Hour3,Hour4)


#1,2,3,4
#gamma skosny
hist(ReceiptPrice1,breaks=500)
table(ReceiptPrice1)
#gamma skosny okolice 0 
hist(ReceiptPrice2,breaks=500)
#tweedy <- masa w 0 i normalny potem
hist(ReceiptPrice3)
hist(ReceiptPrice3[ReceiptPrice3<1500 & ReceiptPrice3>0],breaks=400)
table(ReceiptPrice3)
#normalny
hist(ReceiptPrice4,breaks=400)
hist(ReceiptPrice4[ReceiptPrice4<2],breaks=400)
cor(ReceiptPrice1,ReceiptPrice2)
cor(ReceiptPrice1,ReceiptPrice3)
cor(ReceiptPrice1,ReceiptPrice4)
cor(ReceiptPrice2,ReceiptPrice3)
cor(ReceiptPrice2,ReceiptPrice4)
cor(ReceiptPrice3,ReceiptPrice4)


hist(stat_Day_of_month1,breaks=30)
table(stat_Day_of_month1)
#ciekawie
hist(stat_Day_of_month2,breaks=30) 
table(stat_Day_of_month2)
hist(stat_Day_of_month2[stat_Day_of_month2<30],breaks=40) 
hist(stat_Day_of_month3,breaks=300)
hist(stat_Day_of_month3[stat_Day_of_month3<1],breaks=40)
table(stat_Day_of_month3)
hist(stat_Day_of_month4[stat_Day_of_month4<2],breaks=400)
cor(stat_Day_of_month1stat_Day_of_month2)
cor(stat_Day_of_month1,stat_Day_of_month3)
cor(stat_Day_of_month1,stat_Day_of_month4)
cor(stat_Day_of_month2,stat_Day_of_month3)
cor(stat_Day_of_month2,stat_Day_of_month4)
cor(stat_Day_of_month3,stat_Day_of_month4)


#stad biezemy kolumne 5 srednia
#gamma dyskretny mocno
hist(UniqueProductsCount1,breaks=60)
table(UniqueProductsCount1)
#jw
hist(UniqueProductsCount2,breaks=60)
#tweedy <- masa w 0 i normalny potem
hist(UniqueProductsCount3,breaks=600)
#normalny
hist(UniqueProductsCount4,breaks=400)
hist(UniqueProductsCount4[UniqueProductsCount4<2],breaks=400)
cor(UniqueProductsCount1,UniqueProductsCount2)
cor(UniqueProductsCount1,UniqueProductsCount3)
cor(UniqueProductsCount1,UniqueProductsCount4)
cor(UniqueProductsCount2,UniqueProductsCount3)
cor(UniqueProductsCount2,UniqueProductsCount4)
cor(UniqueProductsCount3,UniqueProductsCount4)

cor(UniqueProductsCount4,Brutto1)
cor(UniqueProductsCount4,Brutto2)
cor(UniqueProductsCount4,ReceiptPrice1)
cor(UniqueProductsCount4,ReceiptPrice2)
cor(UniqueProductsCount4,ReceiptPrice3)
cor(UniqueProductsCount4,ReceiptPrice4)
cor(Brutto1,ReceiptPrice1)
cor(Brutto1,ReceiptPrice2)
cor(Brutto1,ReceiptPrice3)
cor(Brutto1,ReceiptPrice4)

Count_Hour<-ifelse(Count1==1 & Hour3==0,1,0)
Count_Hour2<-ifelse(Count1>1 & Hour1 %in% c(11,12,13,14,15,16,17,18,19),1,0)

cor(Brutto1,Count_Hour)
cor(UniqueProductsCount4,Count_Hour)
cor(ReceiptPrice3,Count_Hour)

cor(Brutto1,Count_Hour2)
cor(UniqueProductsCount4,Count_Hour2)
cor(ReceiptPrice3,Count_Hour2)
cor(Count_Hour,Count_Hour2)



summary(Brutto1)
summary(UniqueProductsCount4)
summary(ReceiptPrice3)
summary(Count_Hour)
summary(Count_Hour2)

hist(Brutto1,breaks=300)
Brutto1<-ifelse(Brutto1<=50,Brutto1,50)
summary(Brutto1)
hist(UniqueProductsCount4,breaks=200)
UniqueProductsCount4<-ifelse(UniqueProductsCount4<=15,UniqueProductsCount4,15)
summary(UniqueProductsCount4)
hist(ReceiptPrice3[ReceiptPrice3<600],breaks=300)
ReceiptPrice3<-ifelse(ReceiptPrice3<=600,ReceiptPrice3,600)
summary(ReceiptPrice3)
hist(Count_Hour)
hist(Count_Hour2)


maxBrutto1<-max(Brutto1)
maxUniqueProductsCount4<-max(UniqueProductsCount4)
maxReceiptPrice3<-max(ReceiptPrice3)
Brutto1<-Brutto1/maxBrutto1
UniqueProductsCount4<-UniqueProductsCount4/maxUniqueProductsCount4
ReceiptPrice3<-ReceiptPrice3/maxReceiptPrice3
summary(Brutto1)
summary(UniqueProductsCount4)
summary(ReceiptPrice3)
hist(Brutto1,breaks=100)
hist(UniqueProductsCount4,breaks=100)
hist(ReceiptPrice3,breaks=100)

##bierzemy UniqueProductsCount4,Brutto1,ReceiptPrice3,Count_Hour,Count_Hour2)
Products_Features_Nice<-as.data.frame(ReceiptPrice$Product_id)
Products_Features_Nice[,2]<-Brutto1
Products_Features_Nice[,3]<-UniqueProductsCount4
Products_Features_Nice[,4]<-ReceiptPrice3
Products_Features_Nice[,5]<-Count_Hour
Products_Features_Nice[,6]<-Count_Hour2
names(Products_Features_Nice)<-c("Product_id","Brutto_mediana","UniqueProductsCount_mean",
                                 "ReceiptPrice_var","Count_Hour","Count_Hour2")





write.csv(Products_Features_Nice,file="Products_Features_Nice.csv")
##product id csv cechach  
product_on_receipt<-read.csv("how_many_times_product_on_receipt.csv", sep=",", header=FALSE)
View(as.data.frame(product_on_receipt))
options(scipen = 999)
product_on_receipt<-product_on_receipt[,-10337]
product_on_receipt<-product_on_receipt[!(product_on_receipt==0)]
hist(product_on_receipt,breaks=100)
hist(product_on_receipt[product_on_receipt<5],breaks=)
hist(log(product_on_receipt),breaks=100)
quantile(product_on_receipt,0.95)
#product_on_receipt<-product_on_receipt-0.99999
#product_on_receipt1<-1/(product_on_receipt)
#hist(product_on_receipt1,breaks=1000)
#summary(product_on_receipt)
#summary(product_on_receipt1)
#View(product_on_receipt)
#summary(product_on_receipt)
#hist(product_on_receipt)length(product_on_receipt[(product_on_receipt==1)])
length(product_on_receipt[(product_on_receipt==2)])
length(product_on_receipt[(product_on_receipt==3)])
length(product_on_receipt[(product_on_receipt==4)])
length(product_on_receipt[(product_on_receipt==5)])
length(product_on_receipt[(product_on_receipt==6)])
length(product_on_receipt[(product_on_receipt==7)])
length(product_on_receipt[(product_on_receipt==8)])
length(product_on_receipt[(product_on_receipt==9)])
length(product_on_receipt[(product_on_receipt==10)])
length(product_on_receipt[(product_on_receipt==11)])
length(product_on_receipt[(product_on_receipt==12)])
length(product_on_receipt[(product_on_receipt==13)])
length(product_on_receipt[(product_on_receipt==14)])
length(product_on_receipt[(product_on_receipt==15)])
length(product_on_receipt[(product_on_receipt==16)])
product_on_receipt1234<-product_on_receipt[product_on_receipt %in% c(1,2,3,4)]
hist(product_on_receipt1234)
product_on_receipt1234<-product_on_receipt1234/5
product_on_receipt1<-as.data.frame(product_on_receipt[product_on_receipt %in% c(1,2,3,4)])
product_on_receipt1[,2]<-product_on_receipt1/5
names(product_on_receipt1)<-c("col1","col2")
names(product_on_receipt)<-c("col1")
product_on_receipt<-as.data.frame(product_on_receipt)
product_on_receipt1<-as.data.frame(product_on_receipt1)
#library(plyr)
#l <- list(,)
product_on_receipt2<-merge(product_on_receipt,product_on_receipt1[,2],by="col1",all.x = TRUE)
install.packages("sqldf")
library(sqldf)
#product_on_receipt2<-do.call(rbind.fill, l)
#product_on_receipt2 <- data.frame()
#for(i in seq(along=l)) for(j in names(l[[i]]))
#  product_on_receipt2[i,j] <- l[[i]][j]
#product_on_receipt2
#product_on_receipt2<-do.call(rbind, lapply(lapply(l, unlist), "[",
                      #unique(unlist(c(sapply(l,names))))))
product_on_receipt2 <- sqldf("SELECT product_on_receipt.col1, product_on_receipt1.col2  
              FROM product_on_receipt
              LEFT JOIN product_on_receipt1 USING(col1)")

library(data.table)

dt1 <- data.table(product_on_receipt, key = "col1") 
dt2 <- data.table(product_on_receipt1, key = "col1")

joined.dt1.dt.2 <- dt1[dt2]

product_on_receipt5678910<-product_on_receipt[product_on_receipt %in% c(5,6,7,8,9,10)]
product_on_receipt5678910<-product_on_receipt5678910-4
hist(product_on_receipt5678910)
product_on_receipt5678910<-product_on_receipt5678910/7
product_on_receiptrest<-as.data.frame(product_on_receipt[product_on_receipt>=10])    
View(product_on_receiptrest)
hist(product_on_receiptrest,breaks=1000)
m<-mean(product_on_receiptrest)
l<-1/m
bc_product_on_receiptrest<-bcPower(product_on_receiptrest,l)
bc_product_on_receiptrest<-ifelse(bc_product_on_receiptrest>12,12,bc_product_on_receiptrest)
bc_product_on_receiptrest<-bc_product_on_receiptrest-min(bc_product_on_receiptrest)+1/max(bc_product_on_receiptrest)
bc_product_on_receiptrest<-bc_product_on_receiptrest/max(bc_product_on_receiptrest)
hist(bc_product_on_receiptrest,breaks=100)

stat_category_first<-read.csv("stat_category_first2.csv", sep=",", header=FALSE)
stat_category_first1<-stat_category_first[,2]
stat_category_first2<-stat_category_first[,3]
stat_category_first3<-stat_category_first[,4]
stat_category_first4<-stat_category_first[,5]
hist(stat_category_first1,breaks=16)
plot(stat_category_first1,log="y")
summary(stat_category_first1)
unique(stat_category_first1)
##gamma,dwa rozkłady
hist(stat_category_first2,breaks=45) 
##normalny
hist(stat_category_first3,breaks=1000)
##normalny z masą w 0
hist(stat_category_first4,breaks=400)
#normalny z jakimis masami
cor(stat_category_first1,stat_category_first2)
cor(stat_category_first1,stat_category_first3)
cor(stat_category_first1,stat_category_first4)
cor(stat_category_first2,stat_category_first3)
cor(stat_category_first2,stat_category_first4)
cor(stat_category_first3,stat_category_first4)




stat_category_second<-read.csv("stat_category_second2.csv", sep=",", header=FALSE)
stat_category_second1<-stat_category_second[,2]
stat_category_second2<-stat_category_second[,3]
stat_category_second3<-stat_category_second[,4]
stat_category_second4<-stat_category_second[,5]
hist(stat_category_second1,breaks=60)
plot(stat_category_second1,log="y")
summary(stat_category_second1)
unique(stat_category_second1)
#gamma dwa rozkłady
hist(stat_category_second2,breaks=60) 
##normalny
hist(stat_category_second3,breaks=500)
##normalny z masą w 0
hist(stat_category_second4,breaks=400)
#normalny z jakimis masami

cor(stat_category_second1,stat_category_second2)
cor(stat_category_second1,stat_category_second3)
cor(stat_category_second1,stat_category_second4)
cor(stat_category_second2,stat_category_second3)
cor(stat_category_second2,stat_category_second4)
cor(stat_category_second3,stat_category_second4)


stat_category_third<-read.csv("stat_category_third2.csv", sep=",", header=FALSE)
stat_category_third1<-stat_category_third[,2]
stat_category_third2<-stat_category_third[,3]
stat_category_third3<-stat_category_third[,4]
stat_category_third4<-stat_category_third[,5]
hist(stat_category_third1,breaks=60)
plot(stat_category_third1,log="y")
summary(stat_category_third1)
unique(stat_category_third1)
#gamma dwa rozkłady
hist(stat_category_third2,breaks=60) 
##normalny
hist(stat_category_third3,breaks=500)
##normalny z masą w 0
hist(stat_category_third4,breaks=400)
#normalny z jakimis masami



stat_category_forth<-read.csv("stat_category_forth2.csv", sep=",", header=FALSE)
stat_category_forth1<-stat_category_forth[,2]
stat_category_forth2<-stat_category_forth[,3]
stat_category_forth3<-stat_category_forth[,4]
stat_category_forth4<-stat_category_forth[,5]
hist(stat_category_forth1,breaks=60)
plot(stat_category_forth1,log="y")
summary(stat_category_forth1)
unique(stat_category_forth1)
#gamma dwa rozkłady
hist(stat_category_forth2,breaks=60) 
##normalny
hist(stat_category_forth3,breaks=500)
##normalny z masą w 0
hist(stat_category_forth4,breaks=400)
#normalny z jakimis masami

###dziwne to, gamma
hist(stat_category_first1[stat_category_first1<8],breaks=20)
hist(stat_category_second1[stat_category_second1<8],breaks=20)
hist(stat_category_third1[stat_category_third1<8],breaks=20)
hist(stat_category_forth1[stat_category_forth1<8],breaks=20)
table(stat_category_first1[stat_category_first1<8])



#inverse gaussian
hist(stat_category_first2[stat_category_first2<8],breaks=10)
hist(stat_category_second2[stat_category_second2<8],breaks=10)
hist(stat_category_third2[stat_category_third2<8],breaks=10)
hist(stat_category_forth2[stat_category_forth2<8],breaks=10)

#rozkłady z masami
#normalny
hist(stat_category_first3[stat_category_first3<8],breaks=400)
#gamma
hist(stat_category_second3[stat_category_second3<8],breaks=400)
#gamma
hist(stat_category_third3[stat_category_third3<15],breaks=400)
#gamma
hist(stat_category_forth3[stat_category_forth3<15],breaks=400)

##piki w całkowitych
hist(stat_category_first4,breaks=400)
hist(stat_category_second4,breaks=400)
hist(stat_category_third4,breaks=600)
hist(stat_category_forth4,breaks=600)


cor(stat_category_first1,stat_category_second1)
cor(stat_category_first1,stat_category_third1)
cor(stat_category_first1,stat_category_forth1)
cor(stat_category_second1,stat_category_third1)
cor(stat_category_second1,stat_category_forth1)
cor(stat_category_third1,stat_category_forth1)


stat_category_first_sta<-stat_category_first[,c(1,2)]
table(stat_category_first1)
stat_category_first_sta[,3]<-stat_category_first_sta[,2]/15
table(stat_category_first_sta[,3])


stat_category_second_sta<-stat_category_second[,c(1,2)]
table(stat_category_second1)
stat_category_second_sta[,3]<-stat_category_second_sta[,2]/15
table(stat_category_first_second[,3])


stat_category_first_sta<-stat_category_first[,c(1,2)]
table(stat_category_first1)
stat_category_first_sta[,3]<-stat_category_first_sta[,2]/15
table(stat_category_first_sta[,3])


stat_category_first_sta<-stat_category_first[,c(1,2)]
table(stat_category_first1)
stat_category_first_sta[,3]<-stat_category_first_sta[,2]/15
table(stat_category_first_sta[,3])


stat_Count_of_unique_products_in_receipt2<-read.csv("stat_Count_of_unique_products_in_receipt2.csv", sep=",", header=FALSE)
View(stat_Count_of_unique_products_in_receipt2)
stat_Count_of_unique_products_in_receipt21<-stat_Count_of_unique_products_in_receipt2[,2]
stat_Count_of_unique_products_in_receipt22<-stat_Count_of_unique_products_in_receipt2[,3]
stat_Count_of_unique_products_in_receipt23<-stat_Count_of_unique_products_in_receipt2[,4]
stat_Count_of_unique_products_in_receipt24<-stat_Count_of_unique_products_in_receipt2[,5]

hist(stat_Count_of_unique_products_in_receipt21,breaks=60)
hist(stat_Count_of_unique_products_in_receipt22[stat_Count_of_unique_products_in_receipt22<9],breaks=30)
hist(stat_Count_of_unique_products_in_receipt23[stat_Count_of_unique_products_in_receipt23<20],breaks=500)
hist(stat_Count_of_unique_products_in_receipt24,breaks=800)



##brak dokumentacji by doogarnac
stat_Day_of_month_from_receipt2<-read.csv("stat_Day_of_month_from_receipt2.csv", sep=",", header=FALSE)
View(stat_Day_of_month_from_receipt2)
stat_Day_of_month_from_receipt21<-stat_Day_of_month_from_receipt2[,1]
stat_Day_of_month_from_receipt22<-stat_Day_of_month_from_receipt2[,2]
stat_Day_of_month_from_receipt23<-stat_Day_of_month_from_receipt2[,3]
stat_Day_of_month_from_receipt24<-stat_Day_of_month_from_receipt2[,4]
hist(stat_Day_of_month_from_receipt21)




stat_Day_of_week_from_receipt2<-read.csv("stat_Day_of_week_from_receipt2.csv", sep=",", header=FALSE)
View(stat_Day_of_week_from_receipt2)
stat_Day_of_week_from_receipt21<-stat_Day_of_week_from_receipt2[,1]
stat_Day_of_week_from_receipt22<-stat_Day_of_week_from_receipt2[,2]
stat_Day_of_week_from_receipt23<-stat_Day_of_week_from_receipt2[,3]
stat_Day_of_week_from_receipt24<-stat_Day_of_week_from_receipt2[,4]

hist(stat_Day_of_week_from_receipt21,breaks=60)
hist(stat_Day_of_week_from_receipt22,breaks=10)
table(stat_Day_of_week_from_receipt22)
hist(stat_Day_of_week_from_receipt22[stat_Day_of_week_from_receipt22<9],breaks=30)
hist(stat_Day_of_week_from_receipt23[stat_Day_of_week_from_receipt23<20],breaks=500)
#jednostajny
hist(stat_Day_of_week_from_receipt23,breaks=10)
#gamma z masą w 0
hist(stat_Day_of_week_from_receipt24[stat_Day_of_week_from_receipt24<9],breaks=400)


stat_how_many_times_product_was_bought2<-read.csv("stat_how_many_times_product_was_bought2.csv", sep=",", header=FALSE)
View(stat_how_many_times_product_was_bought2)
stat_how_many_times_product_was_bought22<-stat_how_many_times_product_was_bought2[,2]
#wykladniczy
hist(stat_how_many_times_product_was_bought22,breaks=100)
table(stat_how_many_times_product_was_bought22)
hist(stat_how_many_times_product_was_bought22[stat_how_many_times_product_was_bought22<9],breaks=30)

