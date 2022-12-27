customer = read.csv(file.choose())
spend = read.csv(file.choose())
repay = read.csv(file.choose())

#Question - 1
#(a)
customer$Age[customer$Age < 18] = round(mean(customer$Age))


#(b)
library(data.table)
df1 = merge.data.frame(customer,spend,by = "Customer")
head(df1)
df1.dt = data.table(df1)
df2 = df1.dt[,list(Amount), by = c('Customer','Limit')]
df2
library(dplyr)
df2 <- df2 %>%
  mutate(
    Amount = ifelse(Amount > Limit, Limit/2, Amount)
  )

spend$Amount <- df2$Amount

#(c)
df1 = merge.data.frame(customer, repay, by = 'Customer')
df1.dt = as.data.table(df1)
df2 = df1.dt[,list(Amount), by = c('Customer','Limit')]

df2 <- df2 %>%
  mutate(
    Amount = ifelse(Amount > Limit, Limit, Amount)
  )
repay$Amount = df2$Amount

#Question - 2 
#(a)
length(unique(customer$Customer))

#(b)
length(unique(customer$Product))

#(c)
spend$Month = as.Date(spend$Month, format = "%d-%b-%y")

Months = months(spend$Month)
Years = format(spend$Month, format = '%y')
Years  

aggregate( Amount ~ Months + Years , spend , mean )

#(d)
repay$Month = as.Date(repay$Month, format = "%d-%b-%y")
Months = months(repay$Month)
Years = format(repay$Month, format = '%y')
Years

aggregate( Amount ~ Months + Years , repay , mean )

#(e)
df = merge.data.frame(customer, spend, by = 'Customer')
df1 = data.frame(df$Customer, month = spend$Month ,Spent = df$Amount, repay = repay$Amount)
head(df1)
df1$profit = df1$repay - df1$Spent

df1 <- df1 %>%
  mutate(
    profit = ifelse(profit > 0, profit, 0)
  )
df1$profit = df1$profit*1.029
head(df1)
class(df1$month)
Months = months(df1$month)
Years = format(df1$month, format = '%y')
aggregate( profit ~ Months + Years , df1 , sum )


#(f)
df2 = aggregate( Amount ~ Type , spend , sum )
df2 = df2[order(df2$Amount, decreasing = TRUE),]

df2 = head(df2,5)
df2

#(g)
df = merge.data.frame(customer, spend, by = 'Customer')

df1 = aggregate(Amount ~ City, df, sum)
df1

df1 = df1[order(df1$Amount, decreasing = TRUE),]
df1 = head(df1,3)
df1

#(h)
df = merge.data.frame(customer, spend, by ="Customer")
df = df %>% 
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      Age <= 20            ~ "0-20",
      Age > 20 & Age <= 45 ~ "21-45",
      Age > 45             ~ "45+"
    )
)

head(df)

df1 = aggregate(Amount ~ age_group, df, sum)
df1 = df1[order(df1$Amount,decreasing = TRUE),]
head(df1,1)

#(i)
df1 = aggregate( Amount~Customer, repay, sum)
df1
df1 = df1[order(df1$Amount,decreasing = TRUE),]
df1 = head(df1,10)
df1


#Question - 3
df = merge.data.frame(customer, spend, by = "Customer")
df1 = data.frame(City = df$City,Product = df$Product,Month = df$Month,Amount = df$Amount)
df2 = aggregate(Amount ~ City + Product + Month, df1, sum)
df2

library(ggplot2)
Years = format(df2$Month, format = '%y')
df2$Years = Years
ggplot(data = df2, aes(x = City, y = Amount, fill = Product)) + 
  geom_bar(stat = 'identity' , position = 'dodge') + 
  facet_wrap(facets = vars(Years))


#Question - 4
#(a)
df = merge.data.frame(customer, spend, by =  "Customer")
df1 = aggregate(Amount~ City+ Month, df, sum)
df1

Month = months(df1$Month)
df1$Month = Month

ggplot(data = df1,aes(x = Month, y = Amount, fill = City)) + geom_bar(stat = 'identity', position = 'dodge')

#(b)
df = merge.data.frame(customer, spend, by = "Customer")
Year = format(df$Month, format = '%y')
df$Year = Year

df1 = aggregate(Amount ~ Year, df, sum)
df1
ggplot(df1, aes(x = Year, y = Amount, fill = Year)) + geom_bar(stat = 'identity') + geom_text(aes(label = signif(Amount)))

#(c)
df = merge.data.frame(customer, spend, by = "Customer")

df1 = aggregate(Amount~ Month + Product, df, sum)
df1


ggplot(data = df1, aes(x = Month, y = Amount)) + geom_line() + facet_wrap(facets = vars(Product))

#Question - 5
df = merge.data.frame(customer, repay, by = "Customer")
df$Years = format(df$Month, format = '%y')
df$Month = month(df$Month)


udf <- function(product, time_period) {
  if (product == "gold" & time_period == "monthly")
    print(df %>% group_by(product = "Gold", City, Customer, Month) %>% summarise(repayment = sum(Amount)) %>% top_n(10))
  else if (product == "gold" & time_period == "yearly")
    print(df %>% group_by(product = "Gold", City, Customer, Years) %>% summarise(repayment = sum(Amount)) %>% top_n(10))
  else if (product == "silver" & time_period == "monthly")
    print(df %>% group_by(product = "Gold", City, Customer, Month) %>% summarise(repayment = sum(Amount)) %>% top_n(10))
  else if (product == "silver" & time_period == "yearly")
    print(df %>% group_by(product = "Gold", City, Customer, Years) %>% summarise(repayment = sum(Amount)) %>% top_n(10))
  else if (product == "platinum" & time_period == "monthly")
    print(df %>% group_by(product = "Gold", City, Customer, Month) %>% summarise(repayment = sum(Amount)) %>% top_n(10))
  else
    print(df %>% group_by(product = "Gold", City, Customer, Years) %>% summarise(repayment = sum(Amount)) %>% top_n(10))
}
udf("Gold","monthly")
