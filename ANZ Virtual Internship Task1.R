library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)

df<-read_excel("Data/ANZ synthesised transaction dataset.xlsx")
summary(df)
#Duration: from 2018-08-01 to 2018-10-31, No. of Records=12043, average transaction amount= AUD 187.93  

#Missing Value:
#It is noted that there are some missing value 
table(df$merchant_state,useNA = "ifany")  #With Missing Value


#Pie Chart Using ggplot
merchant_state<-table(df$merchant_state)
merchant_state<-as.data.frame(merchant_state)
names(merchant_state)[names(merchant_state) =='Var1']<-"State"
merchant_state<-merchant_state %>% 
                mutate(Percentage =round((Freq/sum(Freq)*100),1))
ggplot(merchant_state, aes(x = "", y = Freq, fill = State)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()

#Histogram
hist(df$amount[!df$amount %in% boxplot.stats(df$amount)$out], breaks=50, xlab='Tranaction Amount', main="Histogram of transaction amount")

#Date & Time setting
df$date<- as.Date(df$date,format = "%d/%m/%Y") #Change the "date" column as date format

# derive weekday and hour data of each transaction
df$extraction = as.character(df$extraction)
df$hour = hour(as.POSIXct(substr(df$extraction,12,19),format="%H:%M:%S"))
df$weekday = weekdays(df$date)

#df3: Average Transaction Amount by weekday
df3 <- df %>%
  select(date,weekday) %>%
  group_by(date,weekday) %>%
  summarise(daily_avg_vol = n()) %>%
  group_by(weekday) %>%
  summarise(avg_vol=mean(daily_avg_vol,na.rm=TRUE ))
df3$weekday <- factor(df3$weekday, levels=c( "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
df3<-df3[order(df3$weekday),]
barplot(df3$avg_vol ,col=rainbow(20),names.arg = df3$weekday ,xlab="Weekday" ,ylab="Transaction Volume", main="Average Transaction volume by weekday")


#df4: Average Transaction Amount by weekday
df4 <- df %>%
  select(weekday,amount) %>%
  group_by(weekday) %>%
  summarise(avg_am=mean(amount, na.rm=TRUE))
df4$weekday <- factor(df4$weekday, levels=c( "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
df4<-df4[order(df4$weekday),]

barplot(df4$avg_am ,col=rainbow(20),names.arg = df4$weekday ,xlab="Weekday" ,ylab="Transaction Amount", main="Average Transaction Amount by weekday")

