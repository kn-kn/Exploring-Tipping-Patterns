##### Import data into R #####
setwd("C:/Users/Kevin.Nguyen/Dropbox/CKME136")
raw_df <- read.csv(file="server-survey.csv", head=TRUE)

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

# Find occurences where the above did not renamed and name it to Canada (e.g. B.C. (Canada) -> canada)
# See http://stackoverflow.com/questions/22418864/r-replace-entire-strings-based-on-partial-match
df$State[grepl("canada", df$State)] <- "canada"

# Change all provinces and observations that have the text "canada" in them to simply "canada" 
for (province in c("ab", "bc", "mb", "nb", "nl", "nt", "ns", "nu", "on", "pe", "qc", "sk", "yt")) {
  df$State[grepl(province, df$State)] <- "canada"
}

# Clean variable: Proportion
df$total_prop <- df$asian_prop + df$black_prop + df$hispanic_prop + df$white_prop

# If total proportion is equal to or less than 1, multiply by 100 (the user inputted in percentages rather than whole numbers)
# See http://stackoverflow.com/questions/30774096/based-on-the-value-in-one-column-change-the-value-in-another-column

df$asian_prop <- ifelse(df$total_prop <= 1, df$asian_prop*100, df$asian_prop)
df$black_prop <- ifelse(df$total_prop <= 1, df$black_prop*100, df$black_prop)
df$hispanic_prop <- ifelse(df$total_prop <= 1, df$hispanic_prop*100, df$hispanic_prop)
df$white_prop <- ifelse(df$total_prop <= 1, df$white_prop*100, df$white_prop)

# If total proportion is equal to or over 150, remove occurences from analysis

df$asian_prop <- ifelse(df$total_prop >= 150, NA, df$asian_prop)
df$black_prop <- ifelse(df$total_prop >= 150, NA, df$black_prop)
df$hispanic_prop <- ifelse(df$total_prop >= 150, NA, df$hispanic_prop)
df$white_prop <- ifelse(df$total_prop >= 150, NA, df$white_prop)

# Clean variable: ppbill
# Remove observations where the bill is equal to or under 5 dollars
df$ppbill <- ifelse(df$ppbill <= 5, NA, df$ppbill)

# Remove observations where the tip percentage is equal to 100 or over
df$pcttip <- ifelse(df$pcttip >= 100, NA, df$ppbill)

# If tip percentage is under 1, multiply by 100 to properly fix percentage
df$pcttip <- ifelse(df$pcttip <= 1, df$pcttip*100, df$ppbill)

# Clean variable: Customer Interaction Ratings
# Properly label the integers with their factor level names


write.csv(df, "test_data.csv")
