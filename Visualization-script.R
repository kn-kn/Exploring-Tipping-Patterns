
#setwd("C:/Users/Kevin/Desktop/Dropbox/Dropbox/CKME136")
#setwd("C:/Users/Kevin.Nguyen/Dropbox/CKME136")
#df <- read.csv(file="cleaned_data.csv", head=TRUE) # use same df as before
library(ggplot2)

##### Simple Visualizations (single variable) #####

# Plot distribution of sex of waiters/ waitresses
gender <- ggplot(df, aes(x=sex, fill=sex)) + geom_bar()
gender + guides(fill=FALSE) + ggtitle("Distribution of Workers' Gender in Dataset")

# Plot boxplot to show distribution of tips for each gender
gendertip <- ggplot(df, aes(sex, pcttip, fill=sex))
gendertip + geom_boxplot() + guides(fill=FALSE) + ggtitle("Tip percentage by Gender")

# Test if there is a significant difference between tips for genders
t.test(df$pcttip~df$sex, conf.level = 0.95)

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
p + xlab("Race of Worker") + ylab("Tip Percentage") + ggtitle("Tip percentage by Race of Worker") + coord_flip()

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

# See how much observations there are for US workers and Canadian workers
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
names <- factor(names, level=c("Mean USA", "Mean Canada", "Mean Other", "Mean Worldwide")) # ensures ggplot understands

world_means <- data.frame(names, means)

ggplot(data = world_means, aes(x=names, y=means)) + geom_bar(stat="identity") + xlab("") + ylab("Average Tip %")

# Calculate mean tipping percentage by state
tip <- aggregate(pcttip~State, tableUS, mean)
tip$region <- tip$State # ensure column names match to merge later on

# Utilize the maps library to plot a map of the US
library(maps)
states_name <- map_data("state")

# Recode the state names received from the maps library in order to merge
states_name$region <- recode(states_name$region, '"alabama"="al"; "alaska"="ak"; "arizona"="az"; "arkansas"="ar"; "california"="ca"; "colorado"="co";
                   "connecticut"="ct"; "delware"="de"; "florida"="fl"; "georgia"="ga"; "hawaii"="hi"; "idaho"="id";
                   "illinois"="il"; "indiana"="in"; "iowa"="ia"; "kansas"="ks"; "kentucky"="ky"; "louisiana"="la";
                   "maine"="me"; "maryland"="md"; "massachusetts"="ma"; "michigan"="mi"; "minnesota"="mn"; "mississippi"="mn";
                   "missouri"="mo"; "montana"="mt"; "nebraska"="ne"; "nevada"="nv"; "new hampshire"="nh"; "new jersey"="nj";
                   "new mexico"="nm"; "new york"="ny"; "north carolina"="nc"; "north dakota"="nd"; "ohio"="oh"; "oklahoma"="ok";
                   "oregon"="or"; "pennsylvania"="pa"; "rhode island"="ri"; "south carolina"="sc"; "south dakota"="sc";
                   "tennessee"="tn"; "texas"="tx"; "utah"="ut"; "vermont"="vt"; "virginia"="va"; "washington"="wa";
                   "west virginia"="wv"; "wisconsin"="wi"; "wyoming"="wy"')

# Merge tables together
choro <- merge(states_name, tip, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]

# Plot average tip percentage by US state
map <- ggplot(choro, aes(long, lat)) + geom_polygon(aes(group = group, fill = pcttip))
map + scale_fill_gradient(low="red", high="green") + ggtitle("Average Tip Percentage by US State") +
  guides(fill=guide_legend(title="Tip %"))

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

ggplot(data=a, aes(x=Rating, y=x, group=1)) + geom_line(aes(colour="Flair"))  +
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

ggplot(data=b, aes(x=Rating, y=x, group=1)) + geom_line(aes(colour="Extraverted Enthusiastic")) +
  geom_line(aes(y=b1$x, colour="Critical Quarrelsome")) +
  geom_line(aes(y=b2$x, colour="Dependable Selfdisciplined")) +
  geom_line(aes(y=b3$x, colour="Anxious Easily Upset")) +
  geom_line(aes(y=b4$x, colour="Reserved Quiet")) +
  geom_line(aes(y=b5$x, colour="Sympathetic Warm")) +
  geom_line(aes(y=b6$x, colour="Disorganized Careless")) +
  geom_line(aes(y=b7$x, colour="Conventional Uncreative")) +
  ylab("Average Tipping %") + ggtitle("Average Tipping % by Worker Traits")

# Server Age
ggplot(df, aes(x=birth_yr, y=pcttip)) + geom_point(shape=3) + geom_smooth(method=lm) +
  xlab("Birth Year of Worker") + ylab("Tip %") + ggtitle("Tip Percentage by Birth Year of Worker")
