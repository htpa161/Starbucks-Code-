#Clean Data
install.packages("ivreg")
library(ivreg)
library(readxl)
library(dplyr)
library(lessR)
library(readxl)
library(dplyr)
library(ggplot2)

Transcript_Data <- read_excel('Transactions.xlsx', sheet = 1, guess_max = 1000000)
Portfolio_Data <- read_excel('Starbucks_Offer_Portfolio.xlsx', sheet = 1, guess_max = 1000000)
Profile_Data <- read_excel('Starbucks_Member_Profiles.xlsx', sheet = 1, guess_max = 1000000)
Profile_Data$income <- as.numeric(ifelse(Profile_Data$income=="null",NA, Profile_Data$income))

#merge all datasets
Transcript_Profile <- merge(Transcript_Data, Profile_Data, by.x = "person", by.y = "id", all = TRUE)
Transcript_Profile_Portfolio <- merge(Transcript_Profile, Portfolio_Data, by.x = "offer id", by.y = "id", all = TRUE)
unique(Portfolio_Data$id)

summary(Transcript_Profile_Portfolio) ##Main Data Set
summary(Profile_Data)
summary(Portfolio_Data)

nrow(Transcript_Profile_Portfolio)
nrow(Transcript_Profile)
nrow(Transcript_Data)

#Clean Data
Transcript_Profile_Portfolio$amount[is.na(Transcript_Profile_Portfolio$amount)]<-0
Transcript_Profile_Portfolio$reward.x[is.na(Transcript_Profile_Portfolio$reward.x)]<-0
Transcript_Profile_Portfolio<-na.omit(Transcript_Profile_Portfolio)

x_date<-data.frame(strptime(Transcript_Profile_Portfolio$became_member_on, format = "%Y%m%d"))
Transcript_Profile_Portfolio<-cbind(Transcript_Profile_Portfolio, x_date)
names(Transcript_Profile_Portfolio)[names(Transcript_Profile_Portfolio) == "strptime.Transcript_Profile_Portfolio.became_member_on..format.....Y.m.d.."] <- 'Member_date'
Mem_duration<-data.frame(as.integer(difftime("2022-10-15", Transcript_Profile_Portfolio$Member_date, units = "days")))
Transcript_Profile_Portfolio<-cbind(Transcript_Profile_Portfolio, Mem_duration)
names(Transcript_Profile_Portfolio)[names(Transcript_Profile_Portfolio) == "as.integer.difftime..2022.10.15...Transcript_Profile_Portfolio.Member_date.."] <- 'Mem_duration'

### Check value 
unique(Transcript_Profile_Portfolio$offer_type)
unique(Transcript_Profile_Portfolio$person)

### EDA

#Event
summary(Transcript_Profile_Portfolio$event) #306648 - characters
count(Transcript_Profile_Portfolio,duplicated((Transcript_Profile_Portfolio$event)))
#plot((Transcript_Profile_Portfolio$event)) #right-skew
count(Transcript_Profile_Portfolio,is.na(Transcript_Profile_Portfolio$event)) #114 NA's
table(Transcript_Profile_Portfolio$event) #Majority movies
ggplot(Transcript_Profile_Portfolio, aes(Transcript_Profile_Portfolio$event)) + geom_bar() + xlab("Offer Type")


#Reward
summary(Transcript_Profile_Portfolio$reward.y) #Min 0, max 10
count(Transcript_Profile_Portfolio,duplicated((Transcript_Profile_Portfolio$reward.y)))
hist(Transcript_Profile_Portfolio$reward.y) #right-skew
count(Transcript_Profile_Portfolio,is.na(Transcript_Profile_Portfolio$reward.y)) #NAs
table(Transcript_Profile_Portfolio$reward.y) #Majority movies
ggplot(Transcript_Profile_Portfolio, aes(Transcript_Profile_Portfolio$reward.y)) + geom_bar() + xlab("Reward Amount")

#Time
summary(Transcript_Profile_Portfolio$time) #Min 0, max 714
count(Transcript_Profile_Portfolio,duplicated((Transcript_Profile_Portfolio$time)))
hist(Transcript_Profile_Portfolio$time) #right-skew
count(Transcript_Profile_Portfolio,is.na(Transcript_Profile_Portfolio$time)) #NAs
table(Transcript_Profile_Portfolio$time) 
ggplot(Transcript_Profile_Portfolio, aes(Transcript_Profile_Portfolio$time)) + geom_bar() + xlab("Length of Time")

#Gender
summary(Transcript_Profile_Portfolio$gender) #Min 0, max 714
count(Transcript_Profile_Portfolio,duplicated((Transcript_Profile_Portfolio$gender)))
count(Transcript_Profile_Portfolio,is.na(Transcript_Profile_Portfolio$gender)) #NAs
table(Transcript_Profile_Portfolio$gender) #Majority movies
ggplot(Transcript_Profile_Portfolio, aes(Transcript_Profile_Portfolio$gender)) + geom_bar() + xlab("Gender Type")

#Age
summary(Transcript_Profile_Portfolio$age) #Min 18, max 118
count(Transcript_Profile_Portfolio,duplicated((Transcript_Profile_Portfolio$age)))
hist(Transcript_Profile_Portfolio$age) #right-skew
count(Transcript_Profile_Portfolio,is.na(Transcript_Profile_Portfolio$age)) #NAs
table(Transcript_Profile_Portfolio$age) 
ggplot(Transcript_Profile_Portfolio, aes(Transcript_Profile_Portfolio$age)) + geom_bar() + xlab("Age")

#Income
summary(Transcript_Profile_Portfolio$income) #Min 30000, max 120000
count(Transcript_Profile_Portfolio,duplicated((Transcript_Profile_Portfolio$income)))
hist(na.omit(as.numeric(Transcript_Profile_Portfolio$income))) #right-skew
count(Transcript_Profile_Portfolio,is.na(Transcript_Profile_Portfolio$income)) #NAs
table(Transcript_Profile_Portfolio$income) 
ggplot(Transcript_Profile_Portfolio, aes(Transcript_Profile_Portfolio$income)) + geom_bar() + xlab("Income")

#Channels
summary(Transcript_Profile_Portfolio$channels) 
count(Transcript_Profile_Portfolio,duplicated((Transcript_Profile_Portfolio$channels)))
count(Transcript_Profile_Portfolio,is.na(Transcript_Profile_Portfolio$channels)) #NAs
table(Transcript_Profile_Portfolio$channels) 
ggplot(Transcript_Profile_Portfolio, aes(Transcript_Profile_Portfolio$channels)) + geom_bar() + xlab("Mix of Channels")

#Difficulty
summary(Transcript_Profile_Portfolio$duration) #Min 30000, max 120000
count(Transcript_Profile_Portfolio,duplicated((Transcript_Profile_Portfolio$duration)))
hist(Transcript_Profile_Portfolio$duration) 
count(Transcript_Profile_Portfolio,is.na(Transcript_Profile_Portfolio$duration)) #NAs
table(Transcript_Profile_Portfolio$duration) 
ggplot(Transcript_Profile_Portfolio, aes(Transcript_Profile_Portfolio$duration)) + geom_bar() + xlab("Duration")


#Duration
summary(Transcript_Profile_Portfolio$difficulty) #Min 30000, max 120000
count(Transcript_Profile_Portfolio,duplicated((Transcript_Profile_Portfolio$difficulty)))
hist(Transcript_Profile_Portfolio$difficulty)
count(Transcript_Profile_Portfolio,is.na(Transcript_Profile_Portfolio$difficulty)) #NAs
table(Transcript_Profile_Portfolio$difficulty) 
ggplot(Transcript_Profile_Portfolio, aes(Transcript_Profile_Portfolio$difficulty)) + geom_bar() + xlab("Difficulty")


#Offer_type
summary(Transcript_Profile_Portfolio$offer_type) #Min 30000, max 120000
count(Transcript_Profile_Portfolio,duplicated((Transcript_Profile_Portfolio$offer_type)))
table(Transcript_Profile_Portfolio$offer_type)
count(Transcript_Profile_Portfolio,is.na(Transcript_Profile_Portfolio$offer_type)) #NAs
table(Transcript_Profile_Portfolio$offer_type) 
ggplot(Transcript_Profile_Portfolio, aes(Transcript_Profile_Portfolio$offer_type)) + geom_bar() + xlab("Offer Type")



#install.packages("lessR")

#EDA
#The missing values in ‘gender’ and ‘income’ variables which are related solely and specifically
#with the 2175 customers registered at age 118.
#In other words, customers at age 118 have no registered ‘gender’ and ‘income’.
table(Profile_Data$gender)

#Distribution of offers
PieChart(offer_type, hole = 0, values = "%", data = Transcript_Profile_Portfolio, main = "Offer Distribution")

#based on age - metric offers viewed / offers completed

Age_Banding <-  Transcript_Profile_Portfolio %>% mutate(age_bin = cut(Transcript_Profile_Portfolio$age, breaks=c(0, 20, 40, 60, 80, 100, 120)))
unique(Age_Banding$event)
summary(Age_Banding)

Age_Banding$offer_viewed <- ifelse(Age_Banding$event=="offer viewed",1,0)
Age_Banding$offer_received <- ifelse(Age_Banding$event=="offer received",1,0)
Age_Banding$offer_completed <- ifelse(Age_Banding$event=="offer completed",1,0)
Age_Banding$age_bin <- as.character(Age_Banding$age_bin)
Age_Banding$Num_age_bin <-
  Age_Banding %>%
  mutate(Num_age_bin = case_when(
    Age_Banding$age_bin == "(0,20]" ~ 20,
    Age_Banding$age_bin == "(20,40]" ~ 40,
    Age_Banding$age_bin == "(40,60]" ~ 60,
    Age_Banding$age_bin == "(60,80]" ~ 80,
    Age_Banding$age_bin == "(80,100]" ~ 100,
    Age_Banding$age_bin == "(100,120]" ~ 120))

str(Age_Banding)
Age_Offers<- Age_Banding %>%
  group_by(age_bin)%>%
  summarize(Offers_received = sum(offer_received,na.rm=FALSE),
            Offers_viewed = sum(offer_viewed,na.rm=F),
            Offers_completed = sum(offer_completed,na.rm=F))

Age_offer_received <- data.frame(cbind(Offer_Type = rep("Offer Received",7),Age_Bin = Age_Offers$age_bin, Value = as.numeric(Age_Offers$Offers_received)))
Age_offer_received$Value <- as.numeric(ifelse(Age_offer_received$Value=="NA",NA, Age_offer_received$Value))

Age_offer_viewed <- data.frame(cbind(Offer_Type = rep("Offer Viewed",7),Age_Bin = Age_Offers$age_bin, Value = as.numeric(Age_Offers$Offers_viewed)))
Age_offer_viewed$Value <- as.numeric(ifelse(Age_offer_viewed$Value=="NA",NA, Age_offer_viewed$Value))

Age_offer_completed <- data.frame(cbind(Offer_Type = rep("Offer Completed",7),Age_Bin = Age_Offers$age_bin, Value = as.numeric(Age_Offers$Offers_completed)))
Age_offer_completed$Value <- as.numeric(ifelse(Age_offer_completed$Value=="NA",NA, Age_offer_completed$Value))

Age_Offer_Union <- rbind(Age_offer_viewed,Age_offer_completed)
Age_Offer_Union<- filter(Age_Offer_Union,is.na(Value)!= T)

# Stacked Barplot
ggplot(Age_Offer_Union, aes(fill=Offer_Type, y=Value, x=Age_Offer_Union$Age_Bin)) +
  geom_bar(position="stack", stat="identity")+
  ggtitle("Offer Response over all age groups")+
  xlab("Age Groups")+
  ylab("#Offers")

#Membership trends over time
Transcript_Profile_Portfolio$Membership_start <- as.Date(as.character(Transcript_Profile_Portfolio$became_member_on), format = '%Y%m%d')
Transcript_Profile_Portfolio$Membership_start_year <- format(Transcript_Profile_Portfolio$Membership_start,"%Y")

par(mar=c(3,3,2,2))
plot((table(Transcript_Profile_Portfolio$Membership_start_year)), las = 2, cex.axis = 0.5, type = "o", main = "Customer Acquisition over time", xlab = "Year", ylab = "Counts")

#Membership year vs Offer Response

Transcript_Profile_Portfolio$offer_viewed <- ifelse(Transcript_Profile_Portfolio$event=="offer viewed",1,0)
Transcript_Profile_Portfolio$offer_received <- ifelse(Transcript_Profile_Portfolio$event=="offer received",1,0)
Transcript_Profile_Portfolio$offer_completed <- ifelse(Transcript_Profile_Portfolio$event=="offer completed",1,0)

time_Offers<- Transcript_Profile_Portfolio %>%
  group_by(Membership_start_year)%>%
  summarize(Offers_received = sum(offer_received,na.rm=FALSE),
            Offers_viewed = sum(offer_viewed,na.rm=F),
            Offers_completed = sum(offer_completed,na.rm=F))

time_Offers<- filter(time_Offers,is.na(Membership_start_year)!= T)
time_Offers_viewed <- data.frame(cbind(Offer_Type = rep("Offer Received",6),Membership_Start_Year = time_Offers$Membership_start_year, Value = as.numeric(time_Offers$Offers_received)))
time_Offers_completed <- data.frame(cbind(Offer_Type = rep("Offer Completed",6),Membership_Start_Year = time_Offers$Membership_start_year, Value = as.numeric(time_Offers$Offers_completed)))
time_Offers_viz <- rbind(time_Offers_viewed,time_Offers_completed)
time_Offers_viz$Value <- as.numeric(time_Offers_viz$Value)

# Stacked Viz
ggplot(time_Offers_viz, aes(fill=Offer_Type, y=Value, x=time_Offers_viz$Membership_Start_Year)) +
  geom_bar(position="stack", stat="identity")+
  ggtitle("Offer Response based on length of membership")+
  xlab("Member Since")+
  ylab("#Offers")

str(Transcript_Profile_Portfolio)

##Membership vs offer type
time_Offers_Event<- Transcript_Profile_Portfolio %>%
  group_by(Membership_start_year,offer_type)%>%
  summarize(Offers_received = sum(offer_received,na.rm=FALSE),
            Offers_viewed = sum(offer_viewed,na.rm=F),
            Offers_completed = sum(offer_completed,na.rm=F))

time_Offers_Event<- filter(time_Offers_Event,is.na(offer_type)!= T)
time_Offers_Event_completed<- filter(time_Offers_Event, offer_type != "informational")
time_Offers_Event_viewed <- data.frame(cbind(Offer_Type = rep("Offer Received",18),Membership_Start_Year = time_Offers_Event$Membership_start_year, Offer_Type = time_Offers_Event$offer_type, Value = as.numeric(time_Offers_Event$Offers_received)))
time_Offers_Event_completed_updated <- data.frame(cbind(Offer_Type = rep("Offer Completed",12),Membership_Start_Year = time_Offers_Event_completed$Membership_start_year,Offer_Type = time_Offers_Event_completed$offer_type, Value = as.numeric(time_Offers_Event_completed$Offers_completed)))
time_Offers_Event_viz <- rbind(time_Offers_Event_viewed,time_Offers_Event_completed_updated)
time_Offers_Event_viz$Value <- as.numeric(time_Offers_Event_viz$Value)

ggplot(filter(time_Offers_Event_viz, Offer_Type.1=="bogo"), aes(fill=Offer_Type, y=Value, x=Membership_Start_Year)) +
  geom_bar(position="stack", stat="identity")+
  ggtitle("BOGO Offer Response based on length of membership and Offer Type")+
  xlab("Member Since")+
  ylab("#Offers")

ggplot(filter(time_Offers_Event_viz, Offer_Type.1=="discount"), aes(fill=Offer_Type, y=Value, x=Membership_Start_Year)) +
  geom_bar(position="stack", stat="identity")+
  ggtitle("Discount Offer Response based on length of membership and Offer Type")+
  xlab("Member Since")+
  ylab("#Offers")

ggplot(filter(time_Offers_Event_viz, Offer_Type.1=="informational"), aes(fill=Offer_Type, y=Value, x=Membership_Start_Year)) +
  geom_bar(position="stack", stat="identity")+
  ggtitle("Informational Offer Response based on length of membership and Offer Type")+
  xlab("Member Since")+
  ylab("#Offers")

#Income and Age Distribution for male and female
str(Transcript_Profile_Portfolio$income)

plot(density(na.omit(Transcript_Profile_Portfolio$income)), main = "Income Distribution Across Genders", xlab = "Annual Income($)", ylab = "Density")
lines(density(na.omit(Transcript_Profile_Portfolio$income[Transcript_Profile_Portfolio$gender=="F"])), col = "red")                     
lines(density(na.omit(Transcript_Profile_Portfolio$income[Transcript_Profile_Portfolio$gender!="F"])), col = "green") 
#legend(0, 0.000020, legend=c("Female", "Male"),
#       fill = c("red","green"), cex=0.009)

#reward vs difficulty
plot(Portfolio_Data$difficulty,Portfolio_Data$reward, main="Reward vs Difficulty", xlab="Difficulty", ylab = "Reward",col="Blue", fill = "Blue", pch = 15)

#Offer Rankings & Time they received the offer
Time_In_Days <-
  Transcript_Data %>%
  mutate(Time_In_Days = case_when(
    Transcript_Data$time >= 0 & Transcript_Data$time <= 24 ~ 1,
    Transcript_Data$time > 24 & Transcript_Data$time <= 2*24 ~ 2,
    Transcript_Data$time >2*24 & Transcript_Data$time <= 3*24 ~ 3,
    Transcript_Data$time >3*24 & Transcript_Data$time <= 4*24 ~ 4,
    Transcript_Data$time >4*24 & Transcript_Data$time <= 5*24 ~ 5,
    Transcript_Data$time >5*24 & Transcript_Data$time <= 6*24 ~ 6,
    Transcript_Data$time >6*24 & Transcript_Data$time <= 7*24 ~ 7,
    Transcript_Data$time >7*24 & Transcript_Data$time <= 8*24 ~ 8,
    Transcript_Data$time >8*24 & Transcript_Data$time <= 9*24 ~ 9,
    Transcript_Data$time >9*24 & Transcript_Data$time <= 10*24 ~ 10,
    Transcript_Data$time >10*24 & Transcript_Data$time <= 11*24 ~ 11,
    Transcript_Data$time > 11*24 & Transcript_Data$time <= 12*24 ~ 12,
    Transcript_Data$time >12*24 & Transcript_Data$time <= 13*24 ~ 13,
    Transcript_Data$time >13*24 & Transcript_Data$time <= 14*24 ~ 14,
    Transcript_Data$time >14*24 & Transcript_Data$time <= 15*24 ~ 15,
    Transcript_Data$time >15*24 & Transcript_Data$time <= 16*24 ~ 16,
    Transcript_Data$time >16*24 & Transcript_Data$time <= 17*24 ~ 17,
    Transcript_Data$time >17*24 & Transcript_Data$time <= 18*24 ~ 18,
    Transcript_Data$time >18*24 & Transcript_Data$time <= 19*24 ~ 19,
    Transcript_Data$time >19*24 & Transcript_Data$time <= 20*24 ~ 20,
    Transcript_Data$time >20*24 & Transcript_Data$time <= 21*24 ~ 21,
    Transcript_Data$time > 21*24 & Transcript_Data$time <= 22*24 ~ 22,
    Transcript_Data$time >22*24 & Transcript_Data$time <= 23*24 ~ 23,
    Transcript_Data$time >23*24 & Transcript_Data$time <= 24*24 ~ 24,
    Transcript_Data$time >24*24 & Transcript_Data$time <= 25*24 ~ 25,
    Transcript_Data$time >25*24 & Transcript_Data$time <= 26*24 ~ 26,
    Transcript_Data$time >26*24 & Transcript_Data$time <= 27*24 ~ 27,
    Transcript_Data$time >27*24 & Transcript_Data$time <= 28*24 ~ 28,
    Transcript_Data$time >28*24 & Transcript_Data$time <= 29*24 ~ 29,
    Transcript_Data$time >29*24 & Transcript_Data$time <= 30*24 ~ 30,
    Transcript_Data$time >30*24 & Transcript_Data$time <= 31*24 ~ 31))

unique(Time_In_Days$Time_In_Days)
#Amount spent since start of the offer
Amount_Time <- Time_In_Days %>% group_by(Time_In_Days)%>%summarise(Amount_Spent = sum(na.omit(amount)))
str(Amount_Time)
plot(Amount_Time, pch = 15, col = "red", main = "Total Amount Spent in the duration of the ")
lines(Amount_Time)

#Offer Rankings
Offer_Rankings <- filter(Time_In_Days, event == "offer received") %>%
  group_by(person) %>%
  mutate(offer_rank = rank(time, ties.method = "first"))

Offer_Ranking_Viz <- data.frame(table(Offer_Rankings$offer_rank))

par(mar = c(2, 2, 2, 2))
plot(Offer_Ranking_Viz$Var1, Offer_Ranking_Viz$Freq, main = "#Offers rolled out to customers", xlab = "Number of offers received", ylab="Count")
plot(Offer_Ranking_Viz)

#Volume of total number of offers received by customers
ggplot(Offer_Ranking_Viz, aes( y=Freq, x=Var1))+
  geom_bar(stat = "identity", fill = "#007042")+
  ggtitle("Number of Offers Rolled Out to Customers")+
  xlab("Number of offers received")+
  ylab("Count")

#Volume of total number of offers viewed by customers
Offer_Rankings_Viewed <- filter(Time_In_Days, event == "offer viewed") %>%
  group_by(person) %>%
  mutate(offer_rank = rank(time, ties.method = "first"))
Offer_Ranking_Viewed_Viz <- data.frame(table(Offer_Rankings_Viewed$offer_rank))

ggplot(Offer_Ranking_Viewed_Viz, aes( y=Freq, x=Var1))+
  geom_bar(stat = "identity", fill = "#007042")+
  ggtitle("Number of Offers Viewed by Customers")+
  xlab("Number of offers viewed")+
  ylab("Count")

#Volume of total number of offers completed by customers
Offer_Rankings_Completed <- filter(Time_In_Days, event == "offer completed") %>%
  group_by(person) %>%
  mutate(offer_rank = rank(time, ties.method = "first"))
Offer_Ranking_Completed_Viz <- data.frame(table(Offer_Rankings_Completed$offer_rank))

ggplot(Offer_Ranking_Completed_Viz, aes( y=Freq, x=Var1))+
  geom_bar(stat = "identity", fill = "#007042")+
  ggtitle("Number of Offers completed by Customers")+
  xlab("Number of offers completed")+
  ylab("Count")

#amount spent vs income
plot(Transcript_Profile_Portfolio$income, Transcript_Profile_Portfolio$amount, main = "Amount Spent vs Income", xlab="Income",ylab = "Amount Spent")




### Data Prep & Modelling

Transcript_Profile_Portfolio$amount[is.na(Transcript_Profile_Portfolio$amount)]<-0
Transcript_Profile_Portfolio$reward.x[is.na(Transcript_Profile_Portfolio$reward.x)]<-0
Transcript_Profile_Portfolio<-na.omit(Transcript_Profile_Portfolio)

x_date<-data.frame(strptime(Transcript_Profile_Portfolio$became_member_on, format = "%Y%m%d"))
Transcript_Profile_Portfolio<-cbind(Transcript_Profile_Portfolio, x_date)
names(Transcript_Profile_Portfolio)[names(Transcript_Profile_Portfolio) == "strptime.Transcript_Profile_Portfolio.became_member_on..format.....Y.m.d.."] <- 'Member_date'
Mem_duration<-data.frame(as.integer(difftime("2022-10-15", Transcript_Profile_Portfolio$Member_date, units = "days")))
Transcript_Profile_Portfolio<-cbind(Transcript_Profile_Portfolio, Mem_duration)
names(Transcript_Profile_Portfolio)[names(Transcript_Profile_Portfolio) == "as.integer.difftime..2022.10.15...Transcript_Profile_Portfolio.Member_date.."] <- 'Mem_duration'

### Check value 
unique(Transcript_Profile_Portfolio$offer_type)
unique(Transcript_Profile_Portfolio$person)

### Data Prep 

### Remove irrevelant columns 
dropcolumns <- c("amount","channels","duration","became_member_on","reward.y","offer id","Member_date", "person","time")
Transcript_Profile_Portfolio <- Transcript_Profile_Portfolio[,!(names(Transcript_Profile_Portfolio) %in% dropcolumns)]

### Remove offer received 
Transcript_Profile_Portfolio <- Transcript_Profile_Portfolio[!(Transcript_Profile_Portfolio$event == "offer received"),]

DATA1 <- Transcript_Profile_Portfolio                
### Binary: Offer Type, Gender, event 
DATA1$offer_type <- ifelse(DATA1$offer_type == 'informational', 0, 1)
DATA1$event <- ifelse(DATA1$event == 'offer completed', 1, 0)
DATA1$gender <- ifelse(DATA1$gender == 'F', 1, 0)


## IV Model 

cor(DATA1)
Model <- glm(event ~.-reward.x-offer_type-age,data = DATA1, family = "binomial")
summary(Model)

Model <- glm(event ~ Mem_duration + income + difficulty + gender,data = DATA1, family = "binomial")
summary(Model)

Model2 <- lm(event ~.-offer_type-gender,data = DATA1)
summary(Model2)

##Data Modeling  
## 1st Stage for Age as IV --> Refute possibility
First_Stage <- lm(DATA1$Mem_duration~DATA1$age)
summary(First_Stage)


