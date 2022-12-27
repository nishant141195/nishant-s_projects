customer = read.csv(file.choose())
transaction = read.csv(file.choose())
product = read.csv(file.choose())

#Question - 1
#(a) Merge using base merge
df = merge.data.frame(x = customer, y = transaction, by.x ="customer_Id", by.y = "cust_id")
names(transaction)[2]
names(customer)[1]

df1 = merge.data.frame(x = df, y = product, by.x = c("prod_cat_code","prod_subcat_code"), by.y = c("prod_cat_code","prod_sub_cat_code"))

#(b) Merge using dplyr
library(dplyr)
df2 = inner_join(x = customer,y = transaction, by = c("customer_Id" = "cust_id"))
df3 = inner_join(df2,product, by = c("prod_cat_code"="prod_cat_code", "prod_subcat_code" = "prod_sub_cat_code"))


#Question - 2 Summary report for new merged dataset
#(a) Column names and respective data types
str(df1)

#(b) Top and bottom 10 entries
head(df1,10) #Top 10
tail(df1,10) #Bottom 10 

#(c) Five point summary 
Rate = fivenum(df1$Rate)
Tax = fivenum(df1$Tax)
Total_Amount = fivenum(df1$total_amt)
data.frame(Rate,Tax,Total_Amount, row.names = c("Min","1st Quartile","Median","3rd Quartile","Max"))

#(d) Frequency distribution
table1 <- table(df1$prod_cat_code)
table1
table2 <- table(df1$prod_subcat_code)
table2
table3 = table(df1$Gender)
table3
table4 = table(df1$city_code)
table4
table5 = table(df1$Qty)
table5
table6 = table(df1$Store_type)
table6
table7 = table(df1$prod_cat)
table7
table8 = table(df1$prod_subcat)
table8
structure(list(table1,table2,table3,table4,table5,table6,table7,table8), 
          names = list("Prod_cat_code",
                       "prod_subcat_code",
                       "Gender",
                       "City Code",
                       "Quantity",
                       "Store Type",
                       "Product Category",
                       "Product Sub Category"))


#Question - 3 
#(a)Histogram for continuous variable
hist(df1$Rate, labels = TRUE)
hist(df1$Tax, labels = TRUE)
hist(df1$total_amt, labels = TRUE)

#(b)Barplot for Categorical Variable
barplot(table1, xlab = "Product Category Code")
barplot(table2, xlab = "Product Sub-Category Code")
barplot(table3,xlab = "Gender")
barplot(table4,xlab = "city Code")
barplot(table5,xlab = "Quantity")
barplot(table6,xlab = "Store type")
barplot(table7,xlab = "Product Category")
barplot(table8,xlab = "Product Sub-Category")

#Question - 4
#(a)
#install.packages("lubridate")
library(lubridate)
transaction$tran_date = parse_date_time(transaction$tran_date, orders = "dmy")

difftime(max(transaction$tran_date), min(transaction$tran_date))

#(b)
length(unique(transaction$transaction_id[transaction$total_amt<0]))

#Question- 5
table(df1$prod_cat[df1$Gender == "F" & df1$total_amt >0])
table(df1$prod_cat[df1$Gender == "M" & df1$total_amt >0])

#books, Electronics and home and kitchen are the top three categories with most transactions for male and female


#Question - 6
table1 = data.frame(table(customer$city_code))
table1
table1$Var1[table1$Freq == max(table1$Freq)]

cat("Percentage: ",table1$Freq[table1$Freq == max(table1$Freq)]/sum(table1$Freq)*100,"%")


#Question - 7
#maximum product by value
x = ((aggregate(total_amt~Store_type,df1,sum)))
x$Store_type[x$total_amt == max(x$total_amt)]
#maximumn product by Quantity
y = aggregate(Qty~Store_type, df1, sum)
y$Store_type[y$Qty == max(y$Qty)]


#Question - 8
unique(df1$Store_type)
k = aggregate(total_amt~Store_type+prod_cat, df1, sum)
k
k$total_amt[k$Store_type == "Flagship store" & k$prod_cat == "Electronics"]+
  k$total_amt[k$Store_type == "Flagship store" & k$prod_cat == "Clothing"]

sum(df1$total_amt[df1$Store_type == "Flagship store" & df1$prod_cat == "Electronics"])+
  sum(df1$total_amt[df1$Store_type == "Flagship store" & df1$prod_cat == "Clothing"])

#Question - 9
sum(df1$total_amt[df1$Gender == "M" & df1$prod_cat == "Electronics"])


#Question - 10
length(unique(df1$customer_Id[df1$total_amt>0]))

aggregate(transaction_id~customer_Id,df1, length)

c = data.frame(df1$customer_Id,df1$transaction_id,df1$total_amt)          
c
c = c[c$df1.total_amt > 0,]
d = aggregate(df1.transaction_id~df1.customer_Id, c, length)
length(d$df1.customer_Id[d$df1.transaction_id>10])


#Question - 11
df1$DOB = parse_date_time(df1$DOB, orders = "dmy")
df1$tran_date = parse_date_time(df1$tran_date, orders = "dmy")

age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

df1$Age = age(df1$DOB)

#(a)
df2= df1[df1$Age >=25 & df1$Age<=35,]

x = aggregate(total_amt~ prod_cat,df2, sum)
x[ x$prod_cat == "Electronics" | x$prod_cat == "Books" ,]
# Total Amount
x$total_amt[x$prod_cat == 'Books'] +
x$total_amt[x$prod_cat == 'Electronics']

#(b)
x = aggregate(total_amt~ tran_date,df2, sum)
sum(x$total_amt[ x$tran_date >= "2014-01-01" & x$tran_date <= "2014-03-01" ])
