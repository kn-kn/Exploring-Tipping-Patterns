# Prepared by: Kevin Nguyen
# For: Tamer Abdou
# Course: CKME 136
# Session: Spring 2016

# Load libraries
library(car) # Rename State names and plots
library(ggplot2) # Visualizations
library(reshape2) # Melt function to assist in visualizating a map of the USA
library(leaps) # Subsets regression
library(maps) # For USA map

##### Import data into R #####

#setwd("C:/Users/Kevin/Desktop/Dropbox/Dropbox/CKME136")
setwd("C:/Users/Kevin.Nguyen/Dropbox/CKME136")
raw_df <- read.csv(file="server-survey.csv", head=TRUE, na.strings=c(" ", "NA"))

# Subset variables to be used in analysis
myvars <- c("State", "asian_prop", "black_prop", "hispanic_prop", "white_prop", "ppbill", 
            "pcttip", "flair", "intro", "selling", "repeat.", "customer_name", "smile",
            "thanks", "Extraverted_enthusiastic", "Critical_quarrelsome", 
            "Dependable_selfdisciplined","Anxious_easily_upset", "Reserved_quiet", 
            "Sympathetic_warm", "Disorganized_careless","Conventional_uncreative", 
            "birth_yr", "sex", "race")
df <- raw_df[myvars]

##### Clean variables #####

# Clean variable: State
# Set variable "State" to all lowercase letters to make cleaning easier to deal with
df$State <- tolower(df$State)

# Rename state names in the USA using the following reference: http://www.stateabbreviations.us/
df$State <- recode(df$State, '"alabama"="al"; "alaska"="ak"; "arizona"="az"; "arkansas"="ar"; 
                   "california"="ca"; "colorado"="co"; "connecticut"="ct"; "delware"="de";
                   "florida"="fl"; "georgia"="ga"; "hawaii"="hi"; "idaho"="id";"illinois"="il"; 
                   "indiana"="in"; "iowa"="ia"; "kansas"="ks"; "kentucky"="ky"; "louisiana"="la";
                   "maine"="me"; "maryland"="md"; "massachusetts"="ma"; "michigan"="mi"; 
                   "minnesota"="mn"; "mississippi"="mn"; "missouri"="mo"; 
                   "montana"="mt"; "nebraska"="ne"; "nevada"="nv"; "new hampshire"="nh"; 
                   "new jersey"="nj"; "new mexico"="nm"; "new york"="ny"; "north carolina"="nc"; 
                   "north dakota"="nd"; "ohio"="oh"; "oklahoma"="ok"; "oregon"="or"; 
                   "pennsylvania"="pa"; "rhode island"="ri"; "south carolina"="sc"; 
                   "south dakota"="sc"; "tennessee"="tn"; "texas"="tx"; "utah"="ut"; 
                  "vermont"="vt"; "virginia"="va"; "washington"="wa";
                   "west virginia"="wv"; "wisconsin"="wi"; "wyoming"="wy"')

# Rename State (Province) names in Canada
df$State <- recode(df$State, '"alberta"="ab"; "british columbia"="bc"; "manitoba"="mb"; 
                   "new brunswick"="nb"; "newfoundland"="nl"; "northwest territories"="nt"; 
                   "nova scotia"="ns"; "nunavut"="nu"; "ontario"="on";
                   "prince edward island"="pe"; "quebec"="qc"; "saskatchewan"="sk"; "yukon"="yt"')

# Change all provinces and observations that have the text "canada" in them to simply "canada" 
for (province in c("ab", "bc", "mb", "nb", "nl", "nt", "ns", "nu", "on", "pe", "qc", 
                   "sk", "yt", "canada")) {
  df$State[grepl(province, df$State)] <- "canada"
}

# Clean variable: Proportion
df$total_prop <- df$asian_prop + df$black_prop + df$hispanic_prop + df$white_prop

# If total proportion is equal to or less than 1, multiply by 100 
# Why? The user inputted in percentages rather than whole numbers
dfprop <- df[2:5]
for (col in names(dfprop)) {
  df[[col]] <- ifelse(df$total_prop <= 1, df[[col]]*100, df[[col]])
}

# If total proportion is equal to or over 150, remove observation from analysis
for (col in names(dfprop)) {
  df[[col]] <- ifelse(df$total_prop >= 150, NA, df[[col]])
}

# Remove total_prop column as it is no longer needed
df$total_prop <- NULL 

# Clean variable: ppbill
# Remove observations where the bill is equal to or under 5 dollars 
# as well as equal to or over 200 dollars
df$ppbill <- ifelse(df$ppbill <= 5 | df$ppbill >= 200, NA, df$ppbill)

# If tip percentage is under 1, multiply by 100 to properly fix percentage
df$pcttip <- ifelse(df$pcttip <= 1, df$pcttip*100, df$pcttip)

# Remove observations where the tip percentage is over 50 percent
df$pcttip <- ifelse(df$pcttip >= 50, NA, df$pcttip)

# Clean variable: Birth Year
# Remove observations where the birth year is between 100 to 1900 OR over 2000
df$birth_yr <- ifelse(df$birth_yr >= 100 & df$birth_yr <= 1900 | df$birth_yr >= 2000, NA, df$birth_yr)

# If birth year is under 100, then add 1900 as the user was inputting the last two digits 
# of their birth year only (e.g. 48 -> 1948)
df$birth_yr <- ifelse(df$birth_yr < 100, df$birth_yr + 1900, df$birth_yr)

# Clean variable: Customer Interaction Ratings
# Use ordered instead of factor for easier visualization
# By using ordered, R can properly order the x-axis of plots automatically
dfrating <- df[8:14]
for (col in names(dfrating)) {
  df[[col]] <- ordered(df[[col]], levels=c(1,2,3,4), labels=c("Never", "Sometimes", "Often", "Always"))
}

# Clean variable: Waiter traits
# Use ordered instead of factor here as well
dftrait <- df[15:22]
for (col in names(dftrait)) {
  df[[col]] <- ordered(df[[col]], levels=c(1,2,3,4,5,6,7))
}

# Clean variable: Sex
df$sex <- factor(df$sex, levels=c(0,1), labels=c("Male", "Female"))

# Clean variable: Race
df$race <- factor(df$race, levels=c(1,2,3,4,5), labels=c("Asian", "Black", "Hispanic", 
                                                         "White", "Other"))

# Keep only the rows that do not have any missing data
df <- df[complete.cases(df),]

# Export data to csv
write.csv(df, "cleaned_data.csv")

##### Visualizations #####

# Use the dataframe, df, that was created earlier for this section

# Plot distribution of sex of waiters/ waitresses
gender <- ggplot(df, aes(x=sex, fill=sex)) + geom_bar()
gender + guides(fill=FALSE) + ggtitle("Distribution of Workers' Gender in Dataset")

# Plot boxplot to show distribution of tips for each gender
gendertip <- ggplot(df, aes(sex, pcttip, fill=sex))
gendertip + geom_boxplot() + guides(fill=FALSE) + ggtitle("Tip percentage by Gender")

# Test if there is a significant difference between tips for genders
t.test(df$pcttip~df$sex, conf.level = 0.95)

# Distribution of Tip Percentages
qplot(pcttip, data=df, geom="histogram", binwidth=5) + ggtitle("Distribution of Tip Percentages")

# Plot scatterplots of proportion of race (customers) by tip %
# Asian customers
ggplot(df, aes(x=asian_prop, y=pcttip)) +
  geom_point(colour="dark red") +
  geom_smooth(method = "lm", aes(fill = "confidence"), alpha = 0.5)

# Black customers
ggplot(df, aes(x=black_prop, y=pcttip)) +
  geom_point(colour="black") +
  geom_smooth(method = "lm", aes(fill = "confidence"), alpha = 0.5)

# Hispanic customers
ggplot(df, aes(x=hispanic_prop, y=pcttip)) +
  geom_point(colour="blue") +
  geom_smooth(method = "lm", aes(fill = "confidence"), alpha = 0.5)

# White customers
ggplot(df, aes(x=white_prop, y=pcttip)) +
  geom_point(colour="green") +
  geom_smooth(method = "lm", aes(fill = "confidence"), alpha = 0.5)

# Plot distribution of ethnical backgrounds of waiters/ waitresses
background <- ggplot(df, aes(x=race, fill=race)) + geom_bar()
background + guides(fill=FALSE) + ggtitle("Distribution of Workers' Race in Dataset")

# Plot average tip percentage received by worker by worker race
p <- ggplot(df, aes(x=factor(race), y=pcttip, fill=race)) + stat_summary(fun.y="mean", geom="bar")
p + xlab("Race of Worker") + ylab("Tip Percentage") + 
  ggtitle("Tip percentage by Race of Worker") + coord_flip()

### Relationship: Customer Background VS. Server Background ###

# Asian Servers
cols <- c("pcttip", "asian_prop", "black_prop", "hispanic_prop", "white_prop")
dfasian <- subset(df, df$race == "Asian")
dfasian <- dfasian[cols]
dfasian2 = melt(dfasian, id.vars="pcttip")

ggplot(dfasian2) + geom_jitter(aes(value, pcttip, colour=variable)) + 
  geom_smooth(aes(value,pcttip, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x") +
  labs(x = "Proportion of Customers by Race", y = "Tip %", 
       title = "Tip % for Asian Workers by Customer Race")

# Black Servers
dfblack <- subset(df, df$race == "Black")
dfblack <- dfblack[cols]
dfblack2 <- melt(dfblack, id.vars="pcttip")

ggplot(dfblack2) + geom_jitter(aes(value, pcttip, colour=variable)) + 
  geom_smooth(aes(value,pcttip, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x") +
  labs(x = "Proportion of Customers by Race", y = "Tip %", 
       title = "Tip % for Black Workers by Customer Race")

# Hispanic Servers
dfhisp <- subset(df, df$race == "Hispanic")
dfhisp <- dfhisp[cols]
dfhisp2 <- melt(dfhisp, id.vars="pcttip")

ggplot(dfhisp2) + geom_jitter(aes(value, pcttip, colour=variable)) + 
  geom_smooth(aes(value,pcttip, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x") +
  labs(x = "Proportion of Customers by Race", y = "Tip %", 
       title = "Tip % for Hispanic Workers by Customer Race")

# White Servers
dfwhite <- subset(df, df$race == "White")
dfwhite <- dfwhite[cols]
dfwhite2 <- melt(dfwhite, id.vars="pcttip")

ggplot(dfwhite2) + geom_jitter(aes(value, pcttip, colour=variable)) + 
  geom_smooth(aes(value,pcttip, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x") +
  labs(x = "Proportion of Customers by Race", y = "Tip %", 
       title = "Tip % for White Workers by Customer Race")

# Tipping habits by location
# Create vector for US state names
states <- c("al"="al","ak"="ak","az"="az","ar"="ar","ca"="ca","co"="co",
            "ct"="ct","de"="de","dc"="dc","fl"="fl","ga"="ga","hi"="hi","id"="id","il"="il","in"="in",
            "ia"="ia","ks"="ks","ky"="ky","la"="la","me"="me","md"="md","ma"="ma","mi"="mi","mn"="mn",
            "ms"="ms","mo"="mo","mt"="mt","ne"="ne","nv"="nv","nh"="nh","nj"="nj","nm"="nm","ny"="ny",
            "nc"="nc","nd"="nd","oh"="oh","ok"="ok","or"="or","pw"="pw","pa"="pa","ri"="ri","sc"="sc",
            "sd"="sd","tn"="tn","tx"="tx","ut"="ut","vt"="vt","va"="va","wa"="wa","wv"="wv","wi"="wi",
            "wy"="wy")

# Subset data for US workers only into a different data frame
tableUS <- subset(df, State %in% names(states), select=State:race)
tableCAD <- subset(df, State %in% "canada", select=State:race)
tableOTHER <- df[-which(df$State %in% names(states) | df$State %in% "canada"),]

# See how much observations there are for US workers, Canadian workers, and others
length(tableUS$State)
length(tableCAD$State)
length(tableOTHER$State)

# Calculate average mean tip percentage
meanUS <- mean(tableUS$pcttip, na.rm=TRUE)
meanCAD <- mean(tableCAD$pcttip, na.rm=TRUE)
meanOTHER <- mean(tableOTHER$pcttip, na.rm=TRUE)
meanWorld <- mean(df$pcttip, na.rm=TRUE)

means <- c(meanUS, meanCAD, meanOTHER, meanWorld)
names <- c("Mean USA", "Mean Canada", "Mean Other", "Mean Worldwide")
names <- factor(names, level=c("Mean USA", "Mean Canada", "Mean Other", "Mean Worldwide"))

world_means <- data.frame(names, means)

ggplot(data = world_means, aes(x=names, y=means)) + geom_bar(stat="identity") + 
  xlab("") + ylab("Average Tip %")

# Calculate mean tipping percentage by state
tip <- aggregate(pcttip~State, tableUS, mean)
tip$region <- tip$State # ensure column names match to merge later on

# Utilize the maps library to plot a map of the US
states_name <- map_data("state")

# Recode the state names received from the maps library in order to merge
states_name$region <- recode(states_name$region, 
  '"alabama"="al"; "alaska"="ak"; "arizona"="az"; "arkansas"="ar"; "california"="ca"; "colorado"="co";
  "connecticut"="ct"; "delware"="de"; "florida"="fl"; "georgia"="ga"; "hawaii"="hi"; "idaho"="id";
  "illinois"="il"; "indiana"="in"; "iowa"="ia"; "kansas"="ks"; "kentucky"="ky"; "louisiana"="la";
  "maine"="me"; "maryland"="md"; "massachusetts"="ma"; "michigan"="mi"; "minnesota"="mn"; 
  "mississippi"="mn"; "missouri"="mo"; "montana"="mt"; "nebraska"="ne"; "nevada"="nv"; 
  "new hampshire"="nh"; "new jersey"="nj"; "new mexico"="nm"; "new york"="ny"; "north carolina"="nc"; 
  "north dakota"="nd"; "ohio"="oh"; "oklahoma"="ok"; "oregon"="or"; "pennsylvania"="pa"; 
  "rhode island"="ri"; "south carolina"="sc"; "south dakota"="sc"; "tennessee"="tn"; "texas"="tx"; 
  "utah"="ut"; "vermont"="vt"; "virginia"="va"; "washington"="wa"; "west virginia"="wv"; 
  "wisconsin"="wi"; "wyoming"="wy"')

# Merge tables together
choro <- merge(states_name, tip, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]

# Plot average tip percentage by US state
map <- ggplot(choro, aes(long, lat)) + geom_polygon(aes(group = group, fill = pcttip))
map + scale_fill_gradient(low="red", high="green") + ggtitle("Average Tip Percentage by US State") +
  guides(fill=guide_legend(title="Tip %"))

# View top 3 states
tip2 <- tip[with(tip, order(-pcttip)),] # sort descending by tip %
head(tip2, 3)

# View bottom 3 states
tail(tip2, 3)

# Distribution of Bill Sizes
qplot(ppbill, data=df, geom="histogram", binwidth=5) + ggtitle("Distribution of Bill Sizes")

# Plot bill size against tip percentage
ggplot(df, aes(x=ppbill, y=pcttip)) + geom_point(shape=1) + geom_smooth(method=lm) +
  xlab("Bill per Person ($)") + ylab("Tip %") + ggtitle("Tip Percentage by Bill Size per Person")

# Customer Interactions
# See http://www.r-bloggers.com/grouped-means-again/
a <- aggregate(df$pcttip, by=list(Rating=df$flair), mean)
a1 <- aggregate(df$pcttip, by=list(Rating=df$intro), mean)
a2 <- aggregate(df$pcttip, by=list(Rating=df$selling), mean)
a3 <- aggregate(df$pcttip, by=list(Rating=df$repeat.), mean)
a4 <- aggregate(df$pcttip, by=list(Rating=df$customer_name), mean)
a5 <- aggregate(df$pcttip, by=list(Rating=df$smile), mean)
a6 <- aggregate(df$pcttip, by=list(Rating=df$thanks), mean)

ggplot(data=a, aes(x=Rating, y=x, group=1, size=0.1)) + geom_line(aes(colour="Flair"))  +
  geom_line(aes(y=a1$x, colour="Intro")) +
  geom_line(aes(y=a2$x, colour="Selling")) +
  geom_line(aes(y=a3$x, colour="Repeat")) +
  geom_line(aes(y=a4$x, colour="Customer Name")) +
  geom_line(aes(y=a5$x, colour="Smile")) +
  geom_line(aes(y=a6$x, colour="Thanks")) +
  ylab("Average Tipping %") + ggtitle("Average Tipping % by Customer/ Worker Interaction")

# Server Traits
b <- aggregate(df$pcttip, by=list(Rating=df$Extraverted_enthusiastic), mean)
b1 <- aggregate(df$pcttip, by=list(Rating=df$Critical_quarrelsome), mean)
b2 <- aggregate(df$pcttip, by=list(Rating=df$Dependable_selfdisciplined), mean)
b3 <- aggregate(df$pcttip, by=list(Rating=df$Anxious_easily_upset), mean)
b4 <- aggregate(df$pcttip, by=list(Rating=df$Reserved_quiet), mean)
b5 <- aggregate(df$pcttip, by=list(Rating=df$Sympathetic_warm), mean)
b6 <- aggregate(df$pcttip, by=list(Rating=df$Disorganized_careless), mean)
b7 <- aggregate(df$pcttip, by=list(Rating=df$Conventional_uncreative), mean)

ggplot(data=b, aes(x=Rating, y=x, group=1, size=0.1)) + 
  geom_line(aes(colour="Extraverted Enthusiastic")) +
  geom_line(aes(y=b1$x, colour="Critical Quarrelsome")) +
  geom_line(aes(y=b2$x, colour="Dependable Selfdisciplined")) +
  geom_line(aes(y=b3$x, colour="Anxious Easily Upset")) +
  geom_line(aes(y=b4$x, colour="Reserved Quiet")) +
  geom_line(aes(y=b5$x, colour="Sympathetic Warm")) +
  geom_line(aes(y=b6$x, colour="Disorganized Careless")) +
  geom_line(aes(y=b7$x, colour="Conventional Uncreative")) +
  ylab("Average Tipping %") + ggtitle("Average Tipping % by Worker Traits")

# Distribution of Server Age
qplot(birth_yr, data=df, geom="histogram", binwidth=1) + ggtitle("Distribution of Server Age")

# Server Age
ggplot(df, aes(x=birth_yr, y=pcttip)) + geom_point(shape=3) + geom_smooth(method=lm) +
  xlab("Birth Year of Worker") + ylab("Tip %") + ggtitle("Tip Percentage by Birth Year of Worker")

# Server Age vs Server Gender
ggplot(df, aes(x=birth_yr, y=pcttip, colour = sex)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE) +
  xlab("Birth Year") + ylab("Tip %") + ggtitle("Tip % by Worker Gender and Age")

# Customer Interaction vs. Gender
# Flair
ggplot(data=df, aes(x=flair, y=pcttip, group=sex, colour=sex)) + 
  stat_summary(fun.y="mean", geom="line") + 
  xlab("") + ylab("Tip %") + ggtitle("Tip % by Gender and Customer Interaction: Worker Flair")

# Intro
ggplot(data=df, aes(x=intro, y=pcttip, group=sex, colour=sex)) + 
  stat_summary(fun.y="mean", geom="line") + 
  xlab("") + ylab("Tip %") + ggtitle("Tip % by Gender and Customer Interaction: Introductions")

# Upselling
ggplot(data=df, aes(x=selling, y=pcttip, group=sex, colour=sex)) + 
  stat_summary(fun.y="mean", geom="line") + 
  xlab("") + ylab("Tip %") + ggtitle("Tip % by Gender and Customer Interaction: Upselling")

# See relationship between upselling and per person bill
ggplot(data=df, aes(x=selling, y=ppbill, fill=selling)) + geom_boxplot() + guides(fill=FALSE) +
  xlab("") + ylab("Per person bill") + ggtitle("Total Bill Size by Customer Interaction: Upselling")

# Repeat order  
ggplot(data=df, aes(x=repeat., y=pcttip, group=sex, colour=sex)) + 
  stat_summary(fun.y="mean", geom="line") +
  xlab("") + ylab("Tip %") + ggtitle("Tip % by Gender and Customer Interaction: Repeating Orders")

# Customer Name
ggplot(data=df, aes(x=customer_name, y=pcttip, group=sex, colour=sex)) + 
  stat_summary(fun.y="mean", geom="line") + 
  xlab("") + ylab("Tip %") + ggtitle("Tip % by Gender and Customer Interaction: Customer Name")

# Smile
ggplot(data=df, aes(x=smile, y=pcttip, group=sex, colour=sex)) + 
  stat_summary(fun.y="mean", geom="line") + 
  xlab("") + ylab("Tip %") + ggtitle("Tip % by Gender and Customer Interaction: Smiling")

# Thanking
ggplot(data=df, aes(x=thanks, y=pcttip, group=sex, colour=sex)) + 
  stat_summary(fun.y="mean", geom="line") + 
  xlab("") + ylab("Tip %") + ggtitle("Tip % by Gender and Customer Interaction: Thanking")

### Note: The survey fillers may have mistakenly 
### understood 1 as always and 4 as never (as vice versa)
### This may explain the unrational results given

##### Regression Model Analysis / Setting Up #####

# Remove work environment to start fresh
rm(list = ls())

setwd("C:/Users/Kevin.Nguyen/Dropbox/CKME136")
#setwd("C:/Users/Kevin/Desktop/Dropbox/Dropbox/CKME136")
df <- read.csv(file="clean_string_data.csv", head=TRUE)

# Categorize the State variable
states <- c("al","ak","az","ar","ca","co",
            "ct","de","dc","fl","ga","hi","id","il","in",
            "ia","ks","ky","la","me","md","ma","mi","mn",
            "ms","mo","mt","ne","nv","nh","nj","nm","ny",
            "nc","nd","oh","ok","or","pw","pa","ri","sc",
            "sd","tn","tx","ut","vt","va","wa","wv","wi",
            "wy")

# Change some fields for Tableau purposes
# This section is entirely for Tableau only, the file I used for Tableau
# is exported below as "Tableau_data.csv"
df_tab <- df

df_tab$State_iden <- ifelse(df_tab$State %in% states, 1,
                        ifelse(df_tab$State %in% "canada", 2, 3))

df_tab$State_iden2 <- ifelse(df_tab$State_iden == 1, "USA",
                         ifelse(df_tab$State_iden == 2, "Canada", "Other"))

df_tabrating <- df_tab[9:15]
for (col in names(df_tabrating)) {
  df_tab[[col]] <- ifelse(df_tab[[col]] == "Never", 1,
                      ifelse(df_tab[[col]] == "Sometimes", 2,
                             ifelse(df_tab[[col]] == "Often", 3, 4)))
}  

df_tab$selling_name <- ifelse(df_tab$selling == 1, "Never",
                              ifelse(df_tab$selling == 2, "Sometimes",
                                     ifelse(df_tab$selling == 3, "Often", "Always")))

write.csv(df_tab, "Tableau_data.csv")
  
# USA = 1, Canada = 2, Other = 3
df$State <- ifelse(df$State %in% states, 1,
                   ifelse(df$State %in% "canada", 2, 3))

df$State <- factor(df$State, levels=c(1,2,3))

# Change data type from order back to default as the ratings are not categorical
dfrating <- df[9:15]
for (col in names(dfrating)) {
  df[[col]] <- ifelse(df[[col]] == "Never", 1,
                      ifelse(df[[col]] == "Sometimes", 2,
                             ifelse(df[[col]] == "Often", 3, 4)))
}

# Sex
df$sex <- factor(df$sex, levels=c("Male", "Female"), labels=c(0,1))

# Race of Worker
df$race <- factor(df$race, levels=c("Asian", "Black", "Hispanic", "White"), labels=c(1,2,3,4))

# Create formula
formula_text <- paste(names(df)[8], "~",
                      paste(names(df[c(2:7,9:26)]), collapse="+"))
formula <- as.formula(formula_text)
formula

##### Model Building #####

# Build first multiple linear regression model
set.seed(12) # for reproducibility
fit <- lm(formula, data=df)
summary(fit)

# Check multicollinearity
vif(fit)

# Outliers
outlierTest(fit)

# Influential Observations
# Cook's D plot
cutoff <- 4/((nrow(df)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)

# Influence plot
influencePlot(fit, id.method="noteworthy", main="Influence Plot", 
              sub="Circle Size is proportional to Cook's Distance")

# Lets check the influential observations and decide to remove them or not
# The following are mentioned: 53, 72, 214, 321, 545, 589, 1472

df[53,] # large bill with very large tip
df[72,] # above average tip
df[214,] # proportions don't add up properly, large tip - [DELETE THIS]
df[321,] # large Asian proportion, large bill with NO tip, worker has asian background
df[545,] # proportions don't add up properly, extremely large bill with no tip - [DELETE THIS]
df[589,] # proportions don't make sense - [DELETE THIS]
df[1472,] # No tip and very old worker age

# Delete observations: 214, 545, 589
df <- df[-c(214,545,589),]

# Our VIF check resulted in the 4 proportions having multicollinearity issues
# Lets remove these 4 variables from our analysis
props <- c("asian_prop", "black_prop", "hispanic_prop", "white_prop")
for (col in props) {
  df[[col]] <- NULL
}

# Rerun tests
set.seed(12)
formula_text <- paste(names(df)[4], "~",
                      paste(names(df[c(2:3,5:22)]), collapse="+"))
formula <- as.formula(formula_text)
formula

fit <- lm(formula, data=df)
summary(fit)

# There is no real difference between R^2 values!
par(mfrow=c(2,2)) # change panel layout to 2x2
plot(fit)
par(mfrow=c(1,1)) # change layout back to 1x1

# Evaluate homoscedasticity / Breusch-Pagan test
ncvTest(fit)

# Transformation on PCTTIP (dependent variable) - square root
df2 <- cbind(df, sqrt(df$pcttip))
df2[[4]] <- NULL

# Change column name back to "pcttip"
colnames(df2)[22] <- "pcttip"

formula_text2 <- paste(names(df2)[22], "~",
                      paste(names(df2[c(2:21)]), collapse="+"))
formula2 <- as.formula(formula_text2)
formula2

set.seed(12)
fit <- lm(formula2, data=df2)
summary(fit)

# By transforming the y-variable, the change in R^2 is once again very small

# Recall most of the data are part of the US AND nearly all white workers
# What if we remove these variables?
df3 <- df

df3$State <- NULL
df3$race <- NULL

formula_text3 <- paste(names(df3)[3], "~",
                      paste(names(df3[c(2,4:20)]), collapse="+"))
formula3 <- as.formula(formula_text3)
formula3

set.seed(12)
fit <- lm(formula3, data=df3)
summary(fit)

# The adjusted R^2 fell even further

# Subsets Regression
leaps <- regsubsets(formula, data=df, nbest=1)
plot(leaps, scale="r2")

# The best model is the one on the top column, with the highest R^2
# This includes:
# State, ppbill, flair, repeat, customer_name, dependable_ selfdisciplined, reserved_quiet

formula_textsub <- paste(names(df)[4], "~",
                        paste(names(df[c(2,3,5,8,9,14,16)]), collapse="+"))
formula4 <- as.formula(formula_textsub)
formula4
fit <- lm(formula4, data=df)
summary(fit)

# See IPython Notebook version of this code for more detailed conclusions
# Direct link: https://github.com/kn-kn/Exploring-Tipping-Patterns/blob/master/Tip-Project.ipynb