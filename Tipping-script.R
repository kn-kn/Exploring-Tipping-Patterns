##### Import data into R #####
#setwd("C:/Users/Kevin/Desktop/Dropbox/Dropbox/CKME136")
setwd("C:/Users/Kevin.Nguyen/Dropbox/CKME136")
raw_df <- read.csv(file="server-survey.csv", head=TRUE, na.strings=c(" ", "NA"))

# Subset variables to be used in analysis
myvars <- c("State", "asian_prop", "black_prop", "hispanic_prop", "white_prop", "ppbill", 
            "pcttip", "flair", "intro", "selling", "repeat.", "customer_name", "smile",
            "thanks", "Extraverted_enthusiastic", "Critical_quarrelsome", "Dependable_selfdisciplined",
            "Anxious_easily_upset", "Reserved_quiet", "Sympathetic_warm", "Disorganized_careless",
            "Conventional_uncreative", "birth_yr", "sex", "race")
df <- raw_df[myvars]

##### Clean variables #####

# Clean variable: State
# Set variable "State" to all lowercase letters to easier deal with cleaning
df$State <- tolower(df$State)

# Rename state names in the US using the following reference: http://www.stateabbreviations.us/
library(car)
df$State <- recode(df$State, '"alabama"="al"; "alaska"="ak"; "arizona"="az"; "arkansas"="ar"; "california"="ca"; "colorado"="co";
                   "connecticut"="ct"; "delware"="de"; "florida"="fl"; "georgia"="ga"; "hawaii"="hi"; "idaho"="id";
                   "illinois"="il"; "indiana"="in"; "iowa"="ia"; "kansas"="ks"; "kentucky"="ky"; "louisiana"="la";
                   "maine"="me"; "maryland"="md"; "massachusetts"="ma"; "michigan"="mi"; "minnesota"="mn"; "mississippi"="mn";
                   "missouri"="mo"; "montana"="mt"; "nebraska"="ne"; "nevada"="nv"; "new hampshire"="nh"; "new jersey"="nj";
                   "new mexico"="nm"; "new york"="ny"; "north carolina"="nc"; "north dakota"="nd"; "ohio"="oh"; "oklahoma"="ok";
                   "oregon"="or"; "pennsylvania"="pa"; "rhode island"="ri"; "south carolina"="sc"; "south dakota"="sc";
                   "tennessee"="tn"; "texas"="tx"; "utah"="ut"; "vermont"="vt"; "virginia"="va"; "washington"="wa";
                   "west virginia"="wv"; "wisconsin"="wi"; "wyoming"="wy"')

# Rename state (province) names in Canada
df$State <- recode(df$State, '"alberta"="ab"; "british columbia"="bc"; "manitoba"="mb"; "new brunswick"="nb";
                   "newfoundland"="nl"; "northwest territories"="nt"; "nova scotia"="ns"; "nunavut"="nu"; "ontario"="on";
                   "prince edward island"="pe"; "quebec"="qc"; "saskatchewan"="sk"; "yukon"="yt"')

# Change all provinces and observations that have the text "canada" in them to simply "canada" 
for (province in c("ab", "bc", "mb", "nb", "nl", "nt", "ns", "nu", "on", "pe", "qc", "sk", "yt", "canada")) {
  df$State[grepl(province, df$State)] <- "canada"
}

# Clean variable: Proportion
df$total_prop <- df$asian_prop + df$black_prop + df$hispanic_prop + df$white_prop

# If total proportion is equal to or less than 1, multiply by 100 (the user inputted in percentages rather than whole numbers)
# See http://stackoverflow.com/questions/30774096/based-on-the-value-in-one-column-change-the-value-in-another-column
dfprop <- df[2:5]
for (col in names(dfprop)) {
  df[[col]] <- ifelse(df$total_prop <= 1, df[[col]]*100, df[[col]])
}

# If total proportion is equal to or over 150, remove occurences from analysis
for (col in names(dfprop)) {
  df[[col]] <- ifelse(df$total_prop >= 150, NA, df[[col]])
}

# Remove total_prop column as it is no longer needed
df$total_prop <- NULL 

# Clean variable: ppbill
# Remove observations where the bill is equal to or under 5 dollars as well as equal to or over 200 dollars
df$ppbill <- ifelse(df$ppbill <= 5 | df$ppbill >= 200, NA, df$ppbill)

# If tip percentage is under 1, multiply by 100 to properly fix percentage
df$pcttip <- ifelse(df$pcttip <= 1, df$pcttip*100, df$pcttip)

# Remove observations where the tip percentage is over 50 percent
df$pcttip <- ifelse(df$pcttip >= 50, NA, df$pcttip)

# Clean variable: Customer Interaction Ratings
# Properly label the integers with their ordered level names
dfrating <- df[8:14]
for (col in names(dfrating)) {
  df[[col]] <- ordered(df[[col]], levels=c(1,2,3,4), labels=c("Never", "Sometimes", "Often", "Always"))
}

# Clean variable: Waiter traits
# Ensure R understands the numbers are ordered ~ 1 (low) to 7 (high)
dftrait <- df[15:22]
for (col in names(dftrait)) {
  df[[col]] <- ordered(df[[col]], levels=c(1,2,3,4,5,6,7))
}

# Clean variable: Birth Year
# Remove observations where the birth year is between 100 to 1900 OR over 2000
df$birth_yr <- ifelse(df$birth_yr >= 100 & df$birth_yr <= 1900 | df$birth_yr >= 2000, NA, df$birth_yr)

# If birth year is under 100, then add 1900 as the user inputting the last two digits of their birth year only (e.g. 48 -> 1948)
df$birth_yr <- ifelse(df$birth_yr < 100, df$birth_yr + 1900, df$birth_yr)

# Clean variable: Sex
df$sex <- factor(df$sex, levels=c(0,1), labels=c("Male", "Female"))

# Clean variable: Race
df$race <- factor(df$race, levels=c(1,2,3,4,5), labels=c("Asian", "Black", "Hispanic", "White", "Other"))

# Keep only the rows that do not have any missing data
df <- df[complete.cases(df),]

#write.csv(df, "cleaned_data.csv")