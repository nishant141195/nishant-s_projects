library(ggplot2)
library(data.table)
library(tidyr)
df = read.csv(file.choose())
head(df)
df$Region = as.factor(df$Region)



#Question - 1 Compare sales by region for 2016 with 2015 using bar chart
df1 = data.frame(df$Region,df$Sales2015,df$Sales2016)
summary(df1)
Region = unique(df$Region)

Region
df1.dt = data.table(df1)
df2 = df1.dt[,list(Sales2015 = sum(df.Sales2015), Sales2016 = sum(df.Sales2016)), by = 'df.Region']
df2


Sales <- c(df2$Sales2015,df2$Sales2016) 
ggplot(df2 %>% gather(Sales2015, Sales2016, -df.Region), 
       aes(x=df.Region,y = Sales, fill = Sales2015,Sales2016)) + 
  geom_bar(stat = 'identity', position = 'dodge')+geom_text(aes(label = signif(Sales)))




#Question - 2 Pie charts for sales  for each region in 2016
ggplot(data = df2,aes(x = "",y = Sales2016, fill = df.Region)) + 
  geom_col() + geom_text(aes(label = round((df2$Sales2016/sum(df2$Sales2016))*100,2)), position = position_stack(vjust = 0.5))  + 
  coord_polar(theta = 'y')




#Question - 3 Compare sales of 2015 and 2016 with regions and tiers
df3 = data.frame(df$Region,df$Sales2015,df$Sales2016,df$Tier)
df3.dt = data.table(df3)
df4 = df3.dt[,list(Sales2015 = sum(df.Sales2015), Sales2016 = sum(df.Sales2016)), by = c('df.Region','df.Tier')]
df4

Sales <- c(df4$Sales2015,df4$Sales2016)
ggplot(df4 %>% gather(Sales2015, Sales2016, -c(df.Region,df.Tier)), 
       aes(x=df.Tier,y = Sales, fill = Sales2015,Sales2016)) + 
  geom_bar(stat = 'identity', position = 'dodge')+
facet_wrap(facets = vars(df.Region))




#Question - 4 In East Region, Which state registered decline 2016 sales as compared with 2015
df5 = data.frame(df$Region,df$State,df$Sales2015,df$Sales2016)
df5.dt = data.table(df5)
df5.dt
df6 = df5.dt[,list(Sales2015 = sum(df.Sales2015), Sales2016 = sum(df.Sales2016)), by = c('df.Region','df.State')]
df6 = df6[df6$df.Region == 'East']
df6
Sales <- c(df6$Sales2015,df6$Sales2016) 
ggplot(df6 %>% gather(Sales2015, Sales2016, -c(df.Region,df.State)), 
       aes(x=df.State,y = Sales, fill = Sales2015,Sales2016)) + 
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_wrap(facets = vars(df.Region))




#Question - 5 In all the high tier, which division saw a decline in number of units sold in 2016 compared to 2015
df7 = data.frame(df$Tier,df$Division,df$Units2015,df$Units2016)
df7.dt = data.table(df7)
df8 = df7.dt[,list(Units2015 = sum(df.Units2015), Units2016 = sum(df.Units2016)), by = c('df.Tier','df.Division')]
df8
df8 = df8[df8$df.Tier == 'High']

Units = c(df8$Units2015,df8$Units2016)
ggplot(df8 %>% gather(Units2015, Units2016, -c(df.Tier,df.Division)), 
       aes(x=df.Division,y = Units, fill = Units2015,Units2016)) + 
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_wrap(facets = vars(df.Tier))




#Question - 6 Create a new column Quarter.
df10 = df
df10$Quarter = df10$Month
df10$Quarter[df10$Quarter == 'Jan'] <- 'Q1'
df10$Quarter[df10$Quarter == 'Feb'] <- 'Q1'
df10$Quarter[df10$Quarter == 'Mar'] <- 'Q1'
df10$Quarter[df10$Quarter == 'Apr'] <- 'Q2'
df10$Quarter[df10$Quarter == 'May'] <- 'Q2'
df10$Quarter[df10$Quarter == 'Jun'] <- 'Q2'
df10$Quarter[df10$Quarter == 'Jul'] <- 'Q3'
df10$Quarter[df10$Quarter == 'Aug'] <- 'Q3'
df10$Quarter[df10$Quarter == 'Sep'] <- 'Q3'
df10$Quarter[df10$Quarter == 'Oct'] <- 'Q4'
df10$Quarter[df10$Quarter == 'Nov'] <- 'Q4'
df10$Quarter[df10$Quarter == 'Dec'] <- 'Q4'

unique(df10$Quarter)




#Question - 7 Compare Quarter wise sales for 2015 and 2016
df10$Quarter = as.factor(df10$Quarter)

df11 = data.frame(df10$Quarter,df10$Sales2015,df10$Sales2016)
df11.dt = data.table(df11)
df11.dt
df12 = df11.dt[,list(Sales2015 = sum(df10.Sales2015), Sales2016 = sum(df10.Sales2016)), by = 'df10.Quarter']

Sales <- c(df12$Sales2015,df12$Sales2016) 
ggplot(df12 %>% gather(Sales2015, Sales2016, -df10.Quarter), 
       aes(x=df10.Quarter,y = Sales, fill = Sales2015,Sales2016)) + 
  geom_bar(stat = 'identity', position = 'dodge')

df12

#Question - 8 Determine the composition of qtr wise sales in 2015 with regards to all the tiers
df11 = data.frame(df10$Quarter,df10$Tier,df10$Sales2015)
df11.dt = data.table(df11)
df11.dt
df12 = df11.dt[,list(Sales2015 = sum(df10.Sales2015)), by = c('df10.Quarter','df10.Tier')]

df12
ggplot(data = df12,aes(x = "",y = Sales2015, fill = df10.Tier)) + 
  geom_col()  + 
  geom_text(aes(label = df10.Tier), position = position_identity())+
  coord_polar(theta = 'y', start = 2)+
  facet_wrap(facets = vars(df10.Quarter))

