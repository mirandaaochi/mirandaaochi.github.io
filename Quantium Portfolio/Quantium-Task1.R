# Solution template for Task 1
" This file is a solution template for the Task 1 of the Quantium Virtual Internship.
It will walk you through the analysis, providing the scaffolding for your solution
with gaps left for you to fill in yourself. 

Look for comments that say 'over to you' for places where you need to add your own
code!

  Often, there will be hints about what to do or what function to use in the text
leading up to a code block - if you need a bit of extra help on how to use a
function, the internet has many excellent resources on R coding, which you can find
using your favourite search engine."

## Load required libraries and datasets

#### Example code to install packages
#install.packages("data.table")
install.packages("data.table")
install.packages("ggplot2")
install.packages("ggmosaic")
install.packages("readr")

#### Load required libraries
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)

#### Point the filePath to where you have downloaded the datasets to and
#### assign the data files to data.tables
# over to you! fill in the path to your working directory. 
filePath <- "~/Desktop/Forage/Quantium/"
transactionData <- fread(paste0(filePath,"QVI_transaction_data.csv"))
customerData <- fread(paste0(filePath,"QVI_purchase_behaviour.csv"))

## Exploratory data analysis
"The first step in any analysis is to first understand the data. Let's take a look
at each of the datasets provided"

### Examining transaction data
"We can use `str()` to look at the format of each column and see a sample of the
data. As we have read in the dataset as a `data.table` object, we can also run
`transactionData` in the console to see a sample of the data or use
`head(transactionData)` to look at the first 10 rows.

Let's check if columns we would expect to be numeric are in numeric form and date
columns are in date format."

# Over to you! Examine the data using one or more of the methods described above.
head(transactionData)

"We can see that the date column is in an integer format. Let's change this to a
date format."

#### Convert DATE column to a date format
#### A quick search online tells us that CSV and Excel integer dates begin on 30 Dec 1899
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

"We should check that we are looking at the right products by examining PROD_NAME."

#### Examine PROD_NAME
# Over to you! Generate a summary of the PROD_NAME column.
table(transactionData$PROD_NAME) # We can use table to count.
transactionData[, .N, PROD_NAME] # N is the number of rows

"Looks like we are definitely looking at potato chips but how can we check that
these are all chips? We can do some basic text analysis by summarising the
individual words in the product name."

#### Examine the words in PROD_NAME to see if there are any incorrect entries
#### such as products that are not chips
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), " ")))
setnames(productWords, 'words')

"As we are only interested in words that will tell us if the product is chips or
not, let's remove all words with digits and special characters such as '&' from our
set of product words. We can do this using `grepl()`."

"grep(), grepl() – return the indices of strings containing a match (grep()) or 
a logical vector showing which strings contain a match (grepl())."

# Over to you! Remove digits, and special characters, and then sort the distinct words by frequency of occurrence.
#### Removing digits
productWords <- productWords[grepl("\\d", words) == FALSE, ] 
# this function says keep words that DO NOT have numbers
# \d is equivalent to [0-9] meaning it matches any number 
# \\d is saying match any digits

#### Removing special characters
productWords <- productWords[grepl("[:alpha:]", words), ]
# [:alpha:] : alphabetic characters, equivalent to [[:lower:][:upper:]] or [A-z]
# this function says keep words that have alphabetic characters

#### Let's look at the most common words by counting the number of times a word appears and
#### sorting them by this frequency in order of highest to lowest frequency
productWords[, .N, words][order(N, decreasing = TRUE)]
# productWords[, .N, words] select rows from column words
# productWords[, .N] returns 323 (number of rows)

"There are salsa products in the dataset but we are only interested in the chips
category, so let's remove these."

#### Remove salsa products
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]

"Next, we can use `summary()` to check summary statistics such as mean, min and max
values for each feature to see if there are any obvious outliers in the data and if
there are any nulls in any of the columns (`NA's : number of nulls` will appear in
the output if there are any nulls)."

#### Summarise the data to check for nulls and possible outliers
# Over to you!
summary(transactionData)

"There are no nulls in the columns but product quantity appears to have an outlier
which we should investigate further. Let's investigate further the case where 200
packets of chips are bought in one transaction."

#### Filter the dataset to find the outlier
# Over to you! Use a filter to examine the transactions in question.
transactionData[PROD_QTY == 200, ]

"There are two transactions where 200 packets of chips are bought in one transaction
and both of these transactions were by the same customer."

#### Let's see if the customer has had other transactions
# Over to you! Use a filter to see what other transactions that customer made.
# customer Loyalty Number is 226000
transactionData[LYLTY_CARD_NBR == 226000]

"It looks like this customer has only had the two transactions over the year and is
not an ordinary retail customer. The customer might be buying chips for commercial
purposes instead. We'll remove this loyalty card number from further analysis."

#### Filter out the customer based on the loyalty card number
# Over to you!
# Show rows that DO NOT match the loyalty card number.
transactionData <- transactionData[LYLTY_CARD_NBR != 226000]

#### Re-examine transaction data
# Over to you!
summary(transactionData)
# the new max for PROD_QTY is 5, which is much closer to the other data points!

"That's better. Now, let's look at the number of transaction lines over time to see
if there are any obvious data issues such as missing data."

#### Count the number of transactions by date
# Over to you! Create a summary of transaction count by date.
transaction_by_day <- transactionData[, .N, DATE]

"There's only 364 rows, meaning only 364 dates which indicates a missing date. Let's
create a sequence of dates from 1 Jul 2018 to 30 Jun 2019 and use this to create a
chart of number of transactions over time to find the missing date."

#### Create a sequence of dates and join this the count of transactions by date
# Over to you - create a column of dates that includes every day from 1 Jul 2018 to
# 30 Jun 2019, and join it onto the data to fill in the missing day.
# seq(from, to, by)
dates <- data.table(seq(as.Date("2018/07/01"), as.Date("2019/06/30"), by = "day"))
# We need to change the column name to match what's in our table.
# Otherwise we'll see "Error: A non-empty vector of column names for `by` is required."
dates <- setNames(dates, "DATE") # change from V1 to DATE
# Keep all rows from X, but only those from Y that match ("left join")
# Z <- merge(X, Y, all.x = T)
# In our case, we want to keep all rows from daily sequence and only those that match in the table.
transactions_by_day <- merge(dates, transaction_by_day, all.x = T)

#### Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

#### Plot transactions over time
ggplot(transactions_by_day, aes(x = DATE, y = N)) +
 geom_line() +
 labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
 scale_x_date(breaks = "1 month") +
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

"We can see that there is an increase in purchases in December and a break in late
December. Let's zoom in on this."

#### Filter to December and look at individual days
# Over to you - recreate the chart above zoomed in to the relevant dates.
ggplot(transactions_by_day[month(DATE) == 12, ], aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions in December") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

"We can see that the increase in sales occurs in the lead-up to Christmas and that
there are zero sales on Christmas day itself. This is due to shops being closed on
Christmas day.

Now that we are satisfied that the data no longer has outliers, we can move on to
creating other features such as brand of chips or pack size from PROD_NAME. We will
start with pack size."

#### Pack size
#### We can work this out by taking the digits that are in PROD_NAME
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]

#### Always check your output
#### Let's check if the pack sizes look sensible
transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]

"The largest size is 380g and the smallest size is 70g - seems sensible!"

#### Let's plot a histogram of PACK_SIZE since we know that it is a categorical
### variable and not a continuous variable even though it is numeric.
# Over to you! Plot a histogram showing the number of transactions by pack size.

options(scipen=999) # remove scientific notations for 10000s
hist(transactionData$PACK_SIZE, xlab = "Pack Size", ylab = "Total Purchased", 
     main = "Chip Purchases by Pack Size")

"Pack sizes created look reasonable.

Now to create brands, we can use the first word in PROD_NAME to work out the brand
name."

#### Brands
# Over to you! Create a column which contains the brand of the product, by
# extracting it from the product name.
head(transactionData$PROD_NAME)
# This shows us that the brand is usually contained within the first word.
# Brands like Natural ChipCo or Red Rock Deli can also be identified by their first words.
# We want to extract the first word in PROD_NAME.
# You can use a regex ("([A-Za-z]+)" or "([[:alpha:]]+)"or "(\\w+)") to grab the first word
# Dataframe1$COL2 <- gsub("([A-Za-z]+).*", "\\1", Dataframe1$COL1)
transactionData$BRAND <- gsub("([A-Za-z]+).*", "\\1", transactionData$PROD_NAME)

#### Checking brands
# Over to you! Check the results look reasonable.
unique(transactionData$BRAND)

"Some of the brand names look like they are of the same brands - such as RED and
RRD, which are both Red Rock Deli chips. Let's combine these together."

#### Clean brand names
transactionData[BRAND == "RED", BRAND := "RRD"]

# Over to you! Add any additional brand adjustments you think may be required.
unique(transactionData$BRAND)

# Snbts and Sunbites
transactionData[BRAND == "Snbts", BRAND := "Sunbites"]

# NCC and Natural Chip Company
transactionData[BRAND == "NCC", BRAND := "Natural"]

# Infzns and Infuzions
transactionData[BRAND == "Infzns", BRAND := "Infuzions"]

# Smith and Smiths
transactionData[BRAND == "Smith", BRAND := "Smiths"]

# WW and Woolworths
transactionData[BRAND == "WW", BRAND := "Woolworths"]

# Dorito and Doritos
transactionData[BRAND == "Dorito", BRAND := "Doritos"]

# Grain and GRNWVES
transactionData[BRAND == "Grain", BRAND := "GrnWves"]

#### Check again
# Over to you! Check the results look reasonable.
unique(transactionData$BRAND)

###############################################################################

"Now that we are happy with the transaction dataset, let's have a look at the
customer dataset."

#### Examining customer data
# Over to you! Do some basic summaries of the dataset, including distributions of
# any key columns.
head(customerData) # Loyalty Number, Lifestage, Premium/Mainstream/Budget Customer

customerData[, .N, by = LYLTY_CARD_NBR][order(-N)]
# This shows us that each number is used once, which makes sense.

customerData[, .N, by = LIFESTAGE][order(-N)]
# This shows that RETIREES have the most numbers, followed by OLDER SINGES/COUPLES
# and YOUNG SINGLES/COUPLES. Families seem to have less numbers.

customerData[, .N, by = PREMIUM_CUSTOMER][order(-N)]
# Mainstream customers are the most common, followed by Budget.

#### Merge transaction data to customer data
data <- merge(transactionData, customerData, all.x = TRUE)

"As the number of rows in `data` is the same as that of `transactionData`, we can be
sure that no duplicates were created. This is because we created `data` by setting
`all.x = TRUE` (in other words, a left join) which means take all the rows in
`transactionData` and find rows with matching values in shared columns and then
joining the details in these rows to the `x` or the first mentioned table.

Let's also check if some customers were not matched on by checking for nulls."

# Over to you! See if any transactions did not have a matched customer.
# sum(is.na(data)) checks if the entire df has any nulls
# colSums: Form row and column sums and means for numeric arrays (or data frames).
colSums(is.na(data)) # checks all individual columns

"Great, there are no nulls! So all our customers in the transaction data has been
accounted for in the customer dataset.

Note that if you are continuing with Task 2, you may want to retain this dataset
which you can write out as a csv."

fwrite(data, paste0(filePath,"QVI_data.csv"))

"Data exploration is now complete!"

## Data analysis on customer segments
"Now that the data is ready for analysis, we can define some metrics of interest to
the client:
- Who spends the most on chips (total sales), describing customers by lifestage and
how premium their general purchasing behaviour is
- How many customers are in each segment
- How many chips are bought per customer by segment
- What's the average chip price by customer segment

We could also ask our data team for more information. Examples are:
  - The customer's total spend over the period and total spend for each transaction
to understand what proportion of their grocery spend is on chips
- Proportion of customers in each customer segment overall to compare against the
mix of customers who purchase chips

Let's start with calculating total sales by LIFESTAGE and PREMIUM_CUSTOMER and
plotting the split by these segments to describe which customer segment contribute
most to chip sales."

#### Total sales by LIFESTAGE and PREMIUM_CUSTOMER
# Over to you! Calculate the summary of sales by those dimensions and create a
# plot.
head(data)
# data[rows, columns, by]
sales <- data[, .(SALES = sum(TOT_SALES)), .(LIFESTAGE, PREMIUM_CUSTOMER)]
sales

"Sales are coming mainly from Budget - older families, Mainstream - young
singles/couples, and Mainstream - retirees.

Let's see if the higher sales are due to there being more customers who buy chips."

#### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
# Over to you! Calculate the summary of number of customers by those dimensions and
# create a plot.
customers <- data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)]
customers

# Stacked Bar Chart: ggplot(df,aes(x = cat2,fill = cat1)) + geom_bar(position = "fill")
# https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html 
# It makes more sense to see what % of lifestage is each type of customer.
customer_proportions <- ggplot(data = customers) +
  geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Lifestage", y = "Customer Type", title = "Proportion of Customers") +
  geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))))
customer_proportions

"There are more Mainstream - young singles/couples (11.1%) and 
Mainstream - retirees (9%) who buy chips. This contributes to there being more 
sales to these customer segments but this is not a major driver for the 
Budget - Older families segment (6.4%). Higher sales may also be driven by more 
units of chips being bought per customer. Let's have a look at this next."

#### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
# Over to you! Calculate and plot the average number of units per customer by those two dimensions.
# data[rows, columns, by]
# [order(-AVERAGE)] would sort avg_units in decreasing order
avg_units <- data[, .(AVERAGE = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)] 
avg_units

# geom_bar: https://ggplot2.tidyverse.org/reference/geom_bar.html
ggplot(data = avg_units, aes(weight = AVERAGE, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
         geom_bar(position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Lifestage", y = "Average Units per Customer", title = "Units Per Customer") 

"Older families and young families in general buy more chips per customer
Let's also investigate the average price per unit chips bought for each customer
segment as this is also a driver of total sales."

#### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
# Over to you! Calculate and plot the average price per unit sold (average sale
# price) by those two customer dimensions.
# price is TOT_SALES
avg_price <- data[, .(AVERAGE = sum(TOT_SALES)/sum(PROD_QTY)), .(LIFESTAGE, PREMIUM_CUSTOMER)]
avg_price

ggplot(data = avg_price, aes(weight = AVERAGE, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Lifestage", y = "Average Price per Unit", title = "Price per Unit") 

"Mainstream midage and young singles and couples are more willing to pay more per
packet of chips compared to their budget and premium counterparts. This may be due
to premium shoppers being more likely to buy healthy snacks and when they buy
chips, this is mainly for entertainment purposes rather than their own consumption.
This is also supported by there being fewer premium midage and young singles and
couples buying chips compared to their mainstream counterparts.

As the difference in average price per unit isn't large, we can check if this
difference is statistically different."

#### Perform an independent t-test between mainstream vs premium and budget midage
#### and young singles and couples
# Over to you! Perform a t-test to see if the difference is significant.

# t.test(x, y, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE,
# conf.level = 0.95, ...)

# https://sparkbyexamples.com/r-programming/r-select-rows-based-on-column-value/
# Select Rows by list of column Values: df[df$state %in% c('CA','AZ','PH'),]
# We're looking at the prices that they're willing to pay.
pricePerUnit = data[, price := TOT_SALES/PROD_QTY]
mainstream_young_mid <- data[data$LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER == "Mainstream", price]
premium_budget_young_mid <- data[data$LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER != "Mainstream", price]

t.test(mainstream_young_mid, premium_budget_young_mid, alternative = "greater")

"Welch Two Sample t-test

data:  mainstream_young_mid and premium_budget_young_mid
t = 37.624, df = 54791, p-value < 2.2e-16
alternative hypothesis: true difference in means is greater than 0
95 percent confidence interval:
 0.3187234       Inf
sample estimates:
mean of x mean of y 
 4.039786  3.706491 "

"The t-test results in a p-value that is < 2.2e-16, i.e. the unit price for mainstream,
young and mid-age singles and couples ARE significantly higher than
that of budget or premium, young and midage singles and couples.
## Deep dive into specific customer segments for insights
We have found quite a few interesting insights that we can dive deeper into.
We might want to target customer segments that contribute the most to sales to
retain them or further increase sales. Let's look at Mainstream - young
singles/couples. For instance, let's find out if they tend to buy a particular
brand of chips."

#### Deep dive into Mainstream, young singles/couples
# Over to you! Work out if there are brands that these two customer segments prefer
# more than others. You could use a technique called affinity analysis or a-priori
# analysis (or any other method if you prefer)

# arules (apriori) analysis requires our data to be in "transaction" format
# see data(Groceries)

mainstream_ysc <- data[data$PREMIUM_CUSTOMER == "Mainstream" & LIFESTAGE == "YOUNG SINGLES/COUPLES"]
mainstream_ysc

## Apriori Algorithm ##

# load libraries -- uploaded through Tools > Install
library(arules)
library(arulesViz)
library(RColorBrewer)

# Data Frame: card number & brand
"The part that I struggled to understand was that I wanted to have a DF with a
transaction ID and item, rather than a DF with a list of items."
main_t <- mainstream_ysc[, c("LYLTY_CARD_NBR", "BRAND")]

# convert the data to transaction class
transactions_main <- as(split(main_t$BRAND, main_t$LYLTY_CARD_NBR), "transactions")

# using apriori() function
rules_main <- apriori(transactions_main, parameter = list(supp = 0.01, conf = 0.2))

# inspect() the first 10 strong associations
inspect(rules_main[1:10])

#itemFrequencyPlot()
arules::itemFrequencyPlot(transactions_main, topN = 20, 
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative",
                          ylab = "Item Frequency (Relative)")

# What's the brand with the most units purchased?
mainstream_ysc[, .(SUM = sum(PROD_QTY)), .(BRAND)][order(-SUM)]

"
Of all of the brands, Kettle is purchased most often.

We can see that for Mainstream, young singles/couples, some association rules are:
• If Red is bought, Smiths is also bought. In addition, if Red is bough, Kettle is also bought.
• If CCs is bought, Smiths is also bought. Likewise, if CCs is bought, Kettle is also bought.
• If Cheezels is bought, Pringles is also bought. Similarly, if Cheezels is bought, Doritos is also bought.
"

# For safety, let's also do what we did above for everyone else.
others <- data[!(data$PREMIUM_CUSTOMER == "Mainstream" & LIFESTAGE == "YOUNG SINGLES/COUPLES")]
others

others_t <- others[, c("LYLTY_CARD_NBR", "BRAND")]

transactions_others <- as(split(others_t$BRAND, others_t$LYLTY_CARD_NBR), "transactions")

rules_others <- apriori(transactions_others, parameter = list(supp = 0.01, conf = 0.2))

inspect(rules_others[1:10])

arules::itemFrequencyPlot(transactions_others, topN = 20, 
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative",
                          ylab = "Item Frequency (Relative)")

# What's the brand with the most units purchased?
others[, .(SUM = sum(PROD_QTY)), .(BRAND)][order(-SUM)]

"
Once again, Kettle is the brand purchased most often.

For everyone else, some association rules are:
• If French is bought, Smiths are bought.
• If Burger is bought, Smiths are bought. Kettle is also bought.
• If Cheetos are bought, RRD, Woolworths, and Pringles are also bought.
"

"Let's also find out if our target segment tends to buy larger packs of chips."

#### Preferred pack size compared to the rest of the population
# Over to you! Do the same for pack size.

main_size <- mainstream_ysc[, c("LYLTY_CARD_NBR", "PACK_SIZE")]
main_size

transactions_msize <- as(split(main_size$PACK_SIZE, main_t$LYLTY_CARD_NBR), "transactions")

rules_msize <- apriori(transactions_msize, parameter = list(supp = 0.01, conf = 0.2))

inspect(rules_msize[1:10])

arules::itemFrequencyPlot(transactions_msize, topN = 20, 
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative",
                          ylab = "Item Frequency (Relative)")

# What's the commonly purchased pack size?
mainstream_ysc[, .(COUNT = sum(PROD_QTY)), .(PACK_SIZE)][order(-COUNT)]
mainstream_ysc[PACK_SIZE == 175, unique(PROD_NAME)]

"
This tells us that for Mainstream, young singles/couples, 175 is the most common
pack size. 

Our affinity analysis tells us that some association rules are:
• If 200 g bags are purchased, 150 g and 175 g bags are also purchased.
• If 250 g bags are purhcased, 150 g and 175 g bags are also purhcased.
• If 135 g bags are purchased, 150 g and 175 g bags are also purchased.

One limitation of this affinity analysis was that we didn't factor in the number 
of packs per brand purchased in a single transaction.
"

"Now, let's check this information for everyone else."
others_size <- others[, c("LYLTY_CARD_NBR", "PACK_SIZE")]
others_size

transactions_osize <- as(split(others_size$PACK_SIZE, others_size$LYLTY_CARD_NBR), "transactions")

rules_osize <- apriori(transactions_osize, parameter = list(supp = 0.01, conf = 0.2))

inspect(rules_osize[1:10])

arules::itemFrequencyPlot(transactions_osize, topN = 20, 
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative",
                          ylab = "Item Frequency (Relative)")

# What's the commonly purchased pack size?
others[, .(COUNT = sum(PROD_QTY)), .(PACK_SIZE)][order(-COUNT)]
others[PACK_SIZE == 175, unique(PROD_NAME)]

"
For everyone else, 175 is also the most common pack size. 

Our affinity analysis tells us that some association rules are:
• If 125 g bags are purchased, 150 g and 175 g bags are also purchased.
• If 180 g bags are purhcased, 150 g and 175 g bags are also purhcased.
• If 70 g bags are purchased, 150 g bags are also purchased.

One limitation of this affinity analysis was that we didn't factor in the number 
of packs per brand purchased in a single transaction. However, note that it appears
that Mainstream young singles/couples are more likely to buy larger pack sizes
compared to the rest of the population.
"