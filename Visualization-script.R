library(ggplot2)

#setwd("C:/Users/Kevin/Desktop/Dropbox/Dropbox/CKME136")
setwd("C:/Users/Kevin.Nguyen/Dropbox/CKME136")
df <- read.csv(file="cleaned_data.csv", head=TRUE)

##### Simple Visualizations (single variable) #####

# Plot distribution of sex of waiters/ waitresses
gender <- ggplot(na.omit(df), aes(x=sex, fill=sex)) + geom_bar()
gender + guides(fill=FALSE) + ggtitle("Distribution of Workers' Gender in Dataset")

# Plot boxplot to show distribution of tips for each gender
gendertip <- ggplot(na.omit(df), aes(sex, pcttip, fill=sex))
gendertip + geom_boxplot() + guides(fill=FALSE) + ggtitle("Tip percentage by Gender")

# Test if there is a significant difference between tips for genders
t.test(df$pcttip~df$sex, conf.level = 0.95)

# Plot scatterplots of proportion of race (customers) by tip %
# Asian customers
ggplot(na.omit(df), aes(x=asian_prop, y=pcttip)) +
  geom_point(colour="yellow") +
  geom_smooth(method = "lm", aes(fill = "confidence"), alpha = 0.5)

# Black customers
ggplot(na.omit(df), aes(x=black_prop, y=pcttip)) +
  geom_point(colour="black") +
  geom_smooth(method = "lm", aes(fill = "confidence"), alpha = 0.5)

# Hispanic customers
ggplot(na.omit(df), aes(x=hispanic_prop, y=pcttip)) +
  geom_point(colour="blue") +
  geom_smooth(method = "lm", aes(fill = "confidence"), alpha = 0.5)

# White customers
ggplot(na.omit(df), aes(x=white_prop, y=pcttip)) +
  geom_point(colour="green") +
  geom_smooth(method = "lm", aes(fill = "confidence"), alpha = 0.5)

# Plot distribution of ethnical backgrounds of waiters/ waitresses
background <- ggplot(na.omit(df), aes(x=race, fill=race)) + geom_bar()
background + guides(fill=FALSE) + ggtitle("Distribution of Workers' Race in Dataset")

# Plot average tip percentage received by worker by worker race
p <- ggplot(na.omit(df), aes(x=factor(race), y=pcttip, fill=race)) + stat_summary(fun.y="mean", geom="bar")
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

provinces <- map_data("province")
# Merge tables together
choro <- merge(states_name, tip, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]

# Plot average tip percentage by US state
map <- ggplot(choro, aes(long, lat)) + geom_polygon(aes(group = group, fill = pcttip))
map + scale_fill_gradient(low="red", high="green") + ggtitle("Average Tip Percentage by US State") +
  guides(fill=guide_legend(title="Tip %"))

