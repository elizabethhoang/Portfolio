---
title: "Case Study Bellabeat"
author: "Elizabeth Hoang"
date: "2023-06-25"
output:
  word_document: default
  pdf_document: default
  html_document: default
---
---
For this case study I'm a Junior Marketing Analyst for Bellabeat. Our business task is to find how the current trends for smart device usage will help our Bellabeat Marketing team’s strategy and apply these trends to better target our products to Bellabeat’s customers, in this case focusing on the Bellabeat App and trends we can use to improve it for users. We will then present our findings to the Marketing team and executive team which include the co-founders Urska Srsen and Sando Mur.
These are the packages that I will need to download in order to clean, sort and analyze the data-sets.

## Step 1: Load Packages

```{r}
install.packages("tidyverse") 
install.packages("readr") 
install.packages("tidyr") 
install.packages("dplyr") 
install.packages("skimr") 
install.packages("janitor")
```


```{r}
library("tidyverse")
library("readr")
library("tidyr")
library("dplyr")
library("skimr")
library("janitor")
```

The Fitbit data was retrieved from Kaggle a public database, the data set “FitBit Fitness Tracker Data” was created by Robert Furber, Julia Brinton, Michael Keating, and Alexa Ortiz and made available on Kaggle by Mobius. is a data-set collected of 30 participants who responded via a survey on Amazon Mechanical Turk from March 12, 2016 to May 12, 2016. It contains information that tracks data from physical activity, heart rate, and sleep monitoring.
The files are in a downloaded zip folder with 18 CSV. files each including different categories of calories, daily activity, heart rate, steps and sleep measured by minutes and hour; including a data=set with weight changes and updates. Some files are long format and other files are wide format. 

## ROCCC- Validating Data

Reliability of the data provided is not reliable since there are only 30 participants. 

Originality would be questionable since it was done via survey though Amazon Mechanical Turk, there could be bias or duplicate enters, or participants answers might not be accurate. 

Comprehensiveness is good since most of the data sets are identical to Bellabeat’s products and measurements each track for users, making it easy to understand,

Data is not current or up to date it is data focused specifically from March 12, 2016 to May 12, 2016; today’s smart device usage trends are different than 2016.

Data is cited correctly with the information of the creators/authors of the database. The data base is cited as (Furberg, R., Brinton, J., Keating, M., & Ortiz, A. (2016). Crowd-sourced Fitbit datasets 03.12.2016-05.12.2016 [Data set]. Zenodo. https://doi.org/10.5281/zenodo.53894). 

There’s information on 30 participants that are FitBit users, it’s only a small portion of the population, there could be some bias or limitations since it’s not a good sample of the whole fitness population. The dataset provided is licensed by the Creative Commons Attriubtion 4.0 International Public License, making it public and accessible to anyone. 


For these data sets we are only downloading three tables:
dailyActivity_merged.csv
sleepDay_merged.csv
weightLogInfo_merged.csv

The other data-sets contain data that is already recorded in one of these three data-sets. 

## Step 2: Importing Data
```{r}
activity_daily<- read_csv("dailyActivity_merged.csv")
daily_sleep<- read_csv("sleepDay_merged.csv")
weight<- read_csv("weightLogInfo_merged.csv")
```

## Step 3: Understanding our Data

Now we will take a look at each data-set to check for any errors that that the data was imported correctly. 

```{r}
head(activity_daily)
head(daily_sleep) 
head(weight) 
```

We'll run the str() function to take a quick look at the data frames for each data-set. 
```{r}
str(activity_daily) 
str(daily_sleep) 
str(weight) 
```

Now the column names
```{r}
colnames(activity_daily)
```
The Activity table is made up of 15 columns, two of them being the data for Total Steps and Calories, which are contained on two different CSV. files that we don't have to upload, since the information is also copied onto this data-set.
```{r}
colnames(daily_sleep)
```
The Daily Sleep table is made of 5 columns containing the ID of each participant, the day, total sleep records, total minutes asleep, and total time spend in bed in minute form. 

```{r}
colnames(weight)
```
The Weight table, contains information of each participants ID, weight in Kg, fat, BMI, weight in pounds 

Now we will view the data-sets to see how each one looks and some data we can analyze at first glance. 

```{r}
view(activity_daily)
view(daily_sleep)
view(weight)
```

At first glance, there a values with zeros for columns like Total Steps and Total Distance. This can mean that there are days when participants did not recorded or logged their steps and activity. This can cause the data to be skewed and not accurate. 

## Step 4: Analyzing some of the data

Now we will run a clean name function to make sure there are only number, letters and underscores in the names of each column for each table.

```{r}
clean_names(activity_daily) 
clean_names(daily_sleep) 
clean_names(weight)
```

We will take a look at the Total Steps column to see how the steps are recorded and in order to see if the zeros will be consistent and an issue for our analysis. 
First we will arrange it by ascending order of total steps.
Then we will do a descending order of total steps to see the difference.
```{r}
activity_daily %>%  arrange(TotalSteps) 
activity_daily %>% arrange(-TotalSteps)
```

Running these functions, we can see the smallest number of steps tracked are 0 in ascending order, then when we run a function to descend the number of steps we can see max number of steps taken by a participants was 36,019. There are to count so we need to get rid of all of the zeros to get an accurate analysis.

I will create data frames for each one, so it'll be easier to view later on in the data analysis. 
```{r}
stepsasc <- activity_daily %>%  arrange(TotalSteps)
stepsdes <- activity_daily %>% arrange(-TotalSteps)
view(stepsasc)
view(stepsdes)
```

We will run a function to get summarized information about of participants and the average steps they took.When we run this we get a data frame of all of the participants and the average mean steps they took across every day.

```{r}
activity_daily %>% group_by(Id) %>% drop_na() %>% summarize(mean_totalsteps=mean(TotalSteps))
```

When we run this we get a data frame of all of the participants and the average mean each individual took across every day. 
```{r}
activity_daily %>% group_by(Id) %>% drop_na() %>% summarize(max_totalsteps=max(TotalSteps))
```

We ran a function to see the highest amount of steps taken by a participants, which we see was done by ID(1624580081) for the amount of 36019 steps. 

Next we'll check if there's any correlation between Total Steps taken and calories with the participants. 
```{r}
activity_daily %>%  group_by(Id, TotalSteps) %>% drop_na() %>% summarize(mean_calories=mean(Calories))
activity_daily %>%  group_by(Id, TotalSteps) %>% drop_na() %>%
summarize(mean_calories=mean(Calories), max_calories=max(Calories))  
```

I kept getting an errors every time I ran this function. This information can be wrong so I will remove all the zeros now and work with a new set of dataframes made for each data-set.

## Step 5: Cleaning our Data

```{r}
activity_daily2<-activity_daily %>% filter(TotalSteps !=0) 
view(activity_daily2) 
```

By creating a new data frame for Activity Daily, are data is much cleaner and easier to make analysis from it. 

Next we see that the weight and daily sleep tables contain dates and time in the same column, we need to separate these in order to be able to have a clear reading of the table. 

```{r}
daily_sleep2<-daily_sleep %>% 
  separate(SleepDay, c("Date","Time"), " ") 
weight2<-weight %>% separate(Date, c("Date","Time")," ") 
view(daily_sleep2) 
view(weight2)
```

Next we need to distinguish how many Ids or participants are unique for each data-set in order to make there are no duplicates or missing values. 
```{r}
n_distinct(activity_daily2$Id) 
n_distinct(daily_sleep2$Id) 
n_distinct(weight2$Id)
```

In distinguishing how many IDs there are present for each table, we can see three different outcomes for the data that was tracked and logged. There's suppose to be only 30 participants in the project from the data collect through, yet when we run the function, it shows there's 33 participants, this puts in the question the credibility and accuracy of the data-set. Further only 24 people recorded their sleep scheduled and only 8 people of the 30 recorded their weights. 

Now I will check the uniqueness of the amount of rows, I'll check to see if there are any duplicated rows, since there are more than 30 users showing up, we want to make sure we get rid of these duplicated rows and numbers that can skew our analysis. 
```{r}
nrow(activity_daily2) 
nrow(daily_sleep2) 
nrow(weight2)
```

After running our function we see that the daily sleep table has 3 more duplicated rows, therefore we will get rid of these to create a new data frame.

```{r}
daily_sleep3<-unique(daily_sleep2)
```

Now that are data tables are finally clean and filtered we can begin to analyze it. 
We will start by looking a detailed summary of each data-set. 
 
```{r}
skim_without_charts(activity_daily2) 
skim_without_charts(daily_sleep3) 
skim_without_charts(weight2)
```

I will make these data-sets easier to read by focusing only the columns I want to see for each. I will make new dataframes for each containing only the important information or data I will use to analyze FitBits trends.
```{r}
activity_daily_new<-activity_daily2 %>%  
  select(Id, ActivityDate, TotalSteps, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories) %>%
  rename(Date=ActivityDate)
```
```{r}
daily_sleep_new<-daily_sleep3 %>%  
  select(Id, Date, TotalMinutesAsleep, TotalTimeInBed)
```
```{r}
weight_new<-weight2 %>%  
  select(Id, Date, BMI, WeightPounds, IsManualReport)
```

Now let us get a quick detailed summary of each new table.

```{r}
summary(activity_daily_new) 
summary(daily_sleep_new) 
summary(weight_new) 
```
Right away we can see the Activity_Daily table that minimum total of steps taken was 4, this is impossible unless the individual was in bed all day. Again, this is a concern for error in the data. The max number of step was 36,019. The average number of steps taken was 8,319. In regards to calories, the minimum calories burned was 52, the max was 2,220 and the average overall was 2,361. 

According to the article ["How Many Steps Should I Take a Day?" by Zoe Weiner](https://www.wellandgood.com/how-many-steps-should-i-take-a-day/), released on the Well and Good website, the average steps someone should take to be "considered active" is 7,500 steps a day, but to be healthy, boost metabolism and weight management, 15,000 steps are day would need to be met. With that in mind, most of the FitBit users are reaching the average steps they should take but not the necessary steps to stay healthy and have a healthy weight management. 

The Daily_Sleep table shows that the minimum minutes of sleep were 58 minutes, the max was 796 minutes and on average it was 419.2 minutes. While the total time in bed, the minimum was 61 minutes, maximum was 961 minutes and on average 458 minutes. 

The Centers for Disease Control and Prevention in the article ["How Much Sleep Do I need?"](https://www.cdc.gov/sleep/about_sleep/how_much_sleep.html) states that the average adult needs to sleeps between 7-8 hours a night, that's between 420 to 480 minutes a night. Based on the data gather for FitBit users, the average participant is sleeping the right amount of sleep, but there are also users that are sleeping only an hour and other users that sleep more than the average time they should be sleeping, this can cause lack of activity. 

The Weight table shows that the BMI minimum was 21.45, the maximum was 47.54 and on average it was 25.19. While for the weight in pounds, the minimum was 116 pounds, maximum was 294.3 pounds and on average the weight was 158.8 pounds. 

Also according to the CDC for ["Accessing Your Weight,"](https://www.cdc.gov/healthyweight/assessing/index.html) the healthy weight BMI falls between 18.5 and 24.9. A BMI between 25.0 and 29.9 is considered to be overweight. With this in mind from the Weight data-set analysis, most of the FitBit users fall in the overweight category. The minimum BMI is 21.45 this means that even the users with the minimum BMI are closer to being overweight. 

## Step 6: Visualizations & Findings

The first graph is a pie chart out of 100 percent that shows how much the participants tended to workout, as you can see 79% of them were in Sedentary, meaning that they were never active or didn't record how active they had been. Only 20% of the participants were actively from lightly, fairy, to very active.

```{r}
VeryActiveMin<-sum(activity_daily_new$VeryActiveMinutes) 
FairlyActiveMin<-sum(activity_daily_new$FairlyActiveMinutes) 
LightlyActiveMin<-sum(activity_daily_new$LightlyActiveMinutes) 
SedentaryMin<-sum(activity_daily_new$SedentaryMinutes) 
TotalMin<-VeryActiveMin + FairlyActiveMin + LightlyActiveMin + SedentaryMin
```
```{r}
slices<-c(VeryActiveMin, FairlyActiveMin, LightlyActiveMin, SedentaryMin) 
lbls<-c("VeryActive", "FairlyActive", "LightlyActive", "Sedentary") 
pct <-round(slices/sum(slices)*100)
lbls <-paste(lbls, pct)
lbls<-paste(lbls, "%", sep = "") 
pie(slices, labels = lbls, col = rainbow(length(lbls)),main ="Percentage of Activity")
```

This scatter plot shows how the more steps people took the more calories they would burn. Again as we can wee most of the participants didn't take as much steps or record most of their steps. Therefore the data can be inaccurate or there is missing information.'
```{r}
ggplot(data=activity_daily_new) + 
  geom_point(mapping=aes(x=TotalSteps, y=Calories), color="yellow") + 
  geom_smooth(mapping=aes(x=TotalSteps, y=Calories)) + 
  labs(title="Relationship Between Total Steps and Calories Burned", x="Total Steps", y="Calories Burned (kcal)")
```

Now lets combine the data of Activity Daily and Daily Sleep to see if there is any correlation between both factors.

```{r}
combined_data<- merge(activity_daily_new, daily_sleep_new, by="Id")
```
```{r}
ggplot(data=combined_data) + 
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=VeryActiveMinutes, color="VeryActiveMinutes")) + 
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=VeryActiveMinutes, regLineColor="blue"))+ 
  labs(title="Relationship Between Activity Levels and Total Minutes Asleep", x="Total Minutes Asleep", y="Minutes of Activity") 
```

This table shows that the less minutes very active participants slept, the more minutes of activity they would have. 
```{r}
ggplot(data=combined_data) + 
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=FairlyActiveMinutes, color="FairlyActiveMinutes")) + 
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=FairlyActiveMinutes, regLineColor="blue")) + 
  labs(title="Relationship Between Activity Levels and Total Minutes Asleep", x="Total Minutes Asleep", y="Minutes of Activity") 
```

While when participants were Fairly Active their total minutes of sleep shows that the more minutes they slept, the less active they would be. 
```{r}
ggplot(data=combined_data) + 
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=LightlyActiveMinutes, color="LightlyActiveMinutes")) + 
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=LightlyActiveMinutes, regLineColor="blue")) + 
  labs(title="Relationship Between Activity Levels and Total Minutes Asleep", x="Total Minutes Asleep", y="Minutes of Activity") 
```

The table for Lightly Active participants stayed almost constant, they more minutes they sleep the more their activity stayed the same as their total minutes. 

```{r}
ggplot(data=combined_data) + 
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=SedentaryMinutes, color="SedentaryMinutes")) + 
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=SedentaryMinutes, regLineColor="blue")) + 
  labs(title="The Relationship Between Activity Levels and Total Minutes Asleep", x="Total Minutes Asleep", y="Minutes of Activity")
```

Now I'll see if there's any correlation between the activity daily and weight by each participant. I will merge both tables to get to see the correlation between them.
```{r}
weight_activitydaily<- merge(activity_daily_new, weight_new, by="Id")
```

The sedentary participants table shows that the more they slept, the more sedentary they would be. 

```{r}
ggplot(data=weight_activitydaily)+ 
  geom_point(mapping=aes(x=TotalSteps, y=Calories))+
  labs(title="Relationship Between Steps and Calories Burned")
```
As expected like the previous tables, the more steps participants took, the more calories they would burn. 

```{r}
ggplot(data=weight_activitydaily) + 
  geom_point(mapping=aes(x=Calories, y=VeryActiveMinutes, color="VeryActiveMinutes")) + 
  geom_smooth(mapping=aes(x=Calories, y=VeryActiveMinutes, regLineColor="blue"))+ 
  labs(title="Relationship Between Activity Levels and Calories", x="Calories", y="Minutes of Activity")
```

This table shows the relationship between Activity Level for Very Active participants and the calories burned, as we can see, the more minutes they were active the more calories were burned over time. 
```{r}
ggplot(data=weight_activitydaily) + 
  geom_point(mapping=aes(x=Calories, y=FairlyActiveMinutes, color="FairlyActiveMinutes")) + 
  geom_smooth(mapping=aes(x=Calories, y=FairlyActiveMinutes, regLineColor="blue"))+
  labs(title="Relationship Between Activity Levels and Calories", x="Calories", y="Minutes of Activity") 
```

This table shows the relationship between the participants that were fairly Active and the calories that they burned. As we can see they mostly worked on average 30 to 40 minutes a day so their calories burned stayed around the same amount, expect for those few that worked out more and burned more calories.

```{r}
ggplot(data=weight_activitydaily) + 
  geom_point(mapping=aes(x=Calories, y=LightlyActiveMinutes, color="LightlyActiveMinutes")) + 
  geom_smooth(mapping=aes(x=Calories, y=LightlyActiveMinutes, regLineColor="blue"))+ 
  labs(title="Relationship Between Activity Levels and Calories", x="Calories", y="Minutes of Activity") 
```

This table shows the relationship between the participants that were Lightly Active and the calories burned, it was a positive correlation in two points. Most calories were burned when they worked out between 200 to 400 minutes. The calories stayed consistent around those minutes. 
```{r}
ggplot(data=weight_activitydaily) + 
  geom_point(mapping=aes(x=Calories, y=SedentaryMinutes, color="SedentaryMinutes"))+
  geom_smooth(mapping=aes(x=Calories, y=SedentaryMinutes, regLineColor="blue"))+ 
  labs(title="Relationship Between Activity Levels and Calories", x="Calories", y="Minutes of Activity")
```

This table shows the relationship between participants who were Sedentary and the calories burned, it starts with light positive correlation and begins to go downward. This means that the less activity they began to have the less calories they would burn. They were mostly stagnant and didn't lose much calories as the other participants did.

Next we I ran this function with facet wrap to see more in detail each participant and the calories and steps they took. 
```{r}
ggplot(data=weight_activitydaily, aes(x=TotalSteps, y=Calories))+ 
  geom_point(aes(color=Id))+ facet_wrap(~Id)+ 
  labs(title = "Relationship between Steps and Calories Burned")
```

We only got back 8 tables, reminder that only 8 participants recorded their calories and weight change, this is why instead of getting all 30 participants and a table for each, we only receive back 8. This further proves that the data collected is not accurate and a good data set to make analysis. The lack of information from the remaining participants shows that the information is already skewed and will lack the necessary information that Bellabeat will need to answer its question of the marketing trends.

From the analysis and tables created, we can see that there is a lack of information and data that is still needed, we can try to answer the questions of the device usage current trends, but our analysis will be skewed and not current. Many participants didn't record their data or logged in steps, weight and activity for some days. 
When they did have their FitBits on, we noticed that the more active they were the more calories they would burn and lose weight. The same can be said for when they were barely active, the less active they were the less calories they would burn. 
Reasons why data might not have collected or logged by participants can be for various reasons, the Fitbit could have run out of battery, or they simply could have forgotten to put it on.

## Step 7: Act
That being said, based on the analysis of FitBit, Bellabeat, can try to implement ways to have it's consumers log and track their information via the app or devices by giving incentives or reminders.
Having daily or hourly reminders can be the perfect way to have consumers be able to record their steps, weight, and calories. 
Nowadays, our devices are able to detect when we are moving, based on this, Bellabeat can try to find a way to utilize this new trend by recognizing when consumers are moving or being active and sending a reminder to them to record their time or steps. 
This would create consistency and reliability in the data that can be collected from Bellabeat consumers in order to find ways to make our devices easier to use and exciting for consumers. 

In regards to the data sets, one recommendation I would have that would make this data more accurate, reliable, and unique, would be to collect data for the exact 2 months or 60 days. Along with having a bigger sample size of the population; 30 participants isn't enough to create an analysis, the data is inaccurate, not consistent and creates issues when trying to figure out the business task. There's also the potential of it being biased. When there's a small sample size there's limitations to the data and chance of error can occur. 
The data should also be more current and up to date. This data-sets are from 2016, that is 8 years ago, the device usage trends have definitely changed along with user's habits. This means that the data provided was inaccurate and not current, leading to potential errors or analysis in the final finding of the data-sets. 

## Bibliography:

CDC. “Assessing Your Weight.” Centers for Disease Control and Prevention, 9 June 2023, https://www.cdc.gov/healthyweight/assessing/index.html.

---. “How Much Sleep Do I Need?” Centers for Disease Control and Prevention, 14 Sept. 2022, https://www.cdc.gov/sleep/about_sleep/how_much_sleep.html.

Furberg, R., Brinton, J., Keating, M., & Ortiz, A. (2016). Crowd-sourced Fitbit datasets 03.12.2016-05.12.2016 [Data set]. Zenodo., https://doi.org/10.5281/zenodo.53894

Weiner, Zoe. “How Many Steps Should I Take a Day?” Well+Good, 22 June 2022, https://www.wellandgood.com/how-many-steps-should-i-take-a-day/.

