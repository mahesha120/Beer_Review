
library(knitr)
knitr::opts_chunk$set(fig.width = 8, fig.height = 8, echo = FALSE, warning = FALSE, message = FALSE)

#set the working directory
setwd("C:/Users/mbgsu/Desktop/Documents/Guildford County Exercise/beer_reviews")

#import dataset
beer<-read.csv('beer_reviews.csv')

# Load the caret package
#library(caret)


# Structure of the dataframe
str(beer)

# See top 3 rows and all columns
head(beer[1:3,])

#Descriptive stastics
#Before moving to missing value imputation and feature preprocessing, 
#let’s observe the descriptive statistics of each column in the training dataset.

#The skimr package provides a nice solution to show key descriptive stats for each column.

#The skimr::skim_to_wide() produces a nice dataframe containing the descriptive stats of 
#each of the columns. The dataframe output includes a nice histogram drawn without any 
#plotting help.

library(skimr)
skimmed <- skim_to_wide(beer)
skimmed[, c(1:5, 9:11, 13, 15:16)]

#Notice the number of missing values for each feature, mean, median, proportion split of 
#categories in the factor variables, percentiles and the histogram in the last column.

#only beer_abv has missing values of 67785

#Checking for duplicates where both item and user are duplicated or same user gave multiple ratings to same beer 
beer[(duplicated(beer[c("beer_beerid","review_profilename")]) | duplicated(beer[c("beer_beerid","review_profilename")], fromLast = TRUE)), ] %>% nrow()

library(dplyr)


#Removing duplicates having both beer_id & user duplicated, this will ensure that no 2 reviews from single user to same beer are counted & cumulate
beer<-distinct(beer,beer_beerid,review_profilename,.keep_all = TRUE)

dim(beer)


library(sqldf)
#lets remove all the records that corresponds to missing values of ABV% and assign it to beer
beer = sqldf('select *  from beer where beer_abv is not null ')

#Q1 now lets answer the question 01
#Which brewery produces the strongest beers by ABV%?
sqldf('select distinct brewery_name, max(beer_abv), count(distinct beer_beerid)  from beer  group by brewery_name order by max(beer_abv) desc limit 3')
#1. SchorschbrÃ¤u               57.70    10
#2. BrewDog                     41.00    89
#3. De Struise Brouwers         39.00    46

#Answer
#This shows that SchorschbrÃ¤u  produces the strongest beers by 57.70 ABV%. Followed by BrewDog and De Struise Brouwers

# list of strongest beers from SchorschbrÃ¤u
sqldf("select distinct beer_name, beer_abv  from beer where brewery_name like 'SchorschbrÃ¤u' order by beer_abv desc limit 10")



#Q2:
# If you had to pick 3 beers to recommend using only this data, which would you pick?

#before recommending top beers lets find out how many total unique reviewers participated
#in this database and whats the statisitically significant number of reviewers required for
#reviewing a single beer_beerid.

#view the top 5 records from beer
View(beer[1:5,])
#get the total count of  distinct number of review_profilename 

sqldf('select count(distinct review_profilename) from beer') 

#There is a total of 32909 distinct profile names used to rate the beer products.

# This implies that with 95% confidence interval and with 5% margin of error minimum of 380 reviewers
# are required to rate single beer_beerid in order to consider that rating as statistically significant

# get the beer_beerids that have only statistically significant reviews

beer_1 = sqldf('select beer_beerid, count(distinct review_profilename) from beer 
                group by beer_beerid having count(distinct review_profilename) >=380
                order by count(distinct review_profilename) desc')

tail(beer_1)

#select the all records corresponding the beer_1 beer_beerids from beer and store them in beer_2

beer_2 = sqldf('select * from beer where (select beer_beerid from beer_1)')

# it seems that originally cleaned up beer dataset is identical to  beer_2. In other words all beers
#have recieved a statistically significant number of reviews.
#so now we can proceed to Q2.

sqldf('select distinct beer_name,review_overall, count(*)  from beer where review_overall = 5 group by beer_name order by count(*) desc limit 3')
# This shows that the 3 top beers that received review_overall 5 is 
#1. Pliny The Elder
#2. Weihenstephaner Hefeweissbier
#3. Bell's Hopslam Ale

# All these above 3 rated with a overall_review score of 5 by a  statistically significant number of reviewers given as the count 
#in the sql above.

#Q3.
# Which of the factors (aroma, taste, appearance, palette) are most important in 
#determining the overall quality of a beer?

# This can be answered by looking at the correlation plots of overall quality vs factors() graphs
# select the following columns from the beer dataset for plotting correlation plots
df = beer[c("review_overall", "review_aroma", "review_appearance", "review_palate", "review_taste")]

View(df[1:10,])

# load library
library(corrplot)
correlations <- cor(df[,1:5])
# create correlation plot
corrplot(correlations, method="circle")
#plot shows that the largest dark circle is that correlates with review_overall is review_taste
#so the most important variable is review_taste column


#Q4.Lastly, if I typically enjoy a beer due to its aroma and appearance, which beer style should I try?


# Compute the frequency

df1 = sqldf('select beer_style, review_aroma, count(review_aroma) from beer group by beer_style, review_aroma')

library(ggplot2)
###################################
#dataframe for aroma

df1_1 = df1 %>% group_by(beer_style)   %>%  summarize ( median_value = median(`count(review_aroma)`)) %>% arrange(desc(median_value)) %>% top_n(5) 

df1_2=  df1 %>% filter(beer_style %in% df1_1$beer_style )

head(df1_2)


#plot for Aroma
plot1 <- ggplot(df1_2, aes(x = reorder(df1_2$beer_style,df1_2$`count(review_aroma)` , FUN = median), y = df1_2$`count(review_aroma)`, fill= df1_2$beer_style)) + geom_boxplot() + labs(fill = "Beer Style", title = "Top 5 - Aroma vs Beer Style", y= "Number of reviewers for Aroma", x = "Beer style")

mynamestheme <- theme(plot.title = element_text(hjust = 0.5, family = "Times", face = "bold", size = (15), colour = 'blue'), 
                      legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                      legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                      axis.title = element_text(family = "Times", size = (12), colour = "black"),
                      axis.text = element_text(family = "Times", colour = "red", size = (10)))


print(plot1 +mynamestheme )

#################################
#dataframe for appearance 

df2 = sqldf('select beer_style, review_appearance, count(review_appearance) from beer group by beer_style, review_appearance')

head(df2)

df2_1 = df2 %>% group_by(beer_style)   %>%  summarize ( median_value = median(`count(review_appearance)`)) %>% arrange(desc(median_value)) %>% top_n(5) 

df2_2 =  df2 %>% filter(beer_style %in% df2_1$beer_style )

head(df2_2)




#plot for Appearance
plot2 <- ggplot(df2_2, aes(x = reorder(df2_2$beer_style,df2_2$`count(review_appearance)` , FUN = median), y = df2_2$`count(review_appearance)`, fill= df2_2$beer_style)) + geom_boxplot() + labs(fill = "Beer Style", title = "Top 5 - Appearance vs Beer Style", y= "Number of reviewers for Appearance", x = "Beer style")


print(plot2 +mynamestheme )

# american IPA shows the highest median by reviews for aroma and appearance



