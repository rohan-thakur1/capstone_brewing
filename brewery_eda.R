# Clean up the workspace before beginning
rm(list = ls())

# Load libraries
library(car)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(lattice)
library(plm)
library(lme4)
library(foreign)
library(gplots)
library(stargazer)
library(ggplot2)

# Set working directory
wd = "~/Desktop/Brewery Data/wrk_tables"
setwd(wd)

# Beers and Beer Styles
beer = read.csv("beer.csv")
beer_style = read.csv("beer_style.csv")
names(beer)
names(beer_style)

beer_eda = beer[,c("id","abv","brewery_id","calories", "ibu", "name",
                   "num_ratings","overall_rating","beer_style_id")]
beer_style = beer_style[,c("id","name")]
names(beer_style) = c("beer_style_id", "beer_style_name")

# Joining to add in the style names
beer_eda = left_join(beer_eda, beer_style, by = "beer_style_id")
summary(beer_eda$beer_style_name)

# Cleaning up the beer style category
beer_eda$beer_style_norm = ifelse(beer_eda$beer_style_name %in% c("Stout", "Sweet Stout", "Imperial Stout", "Dry Stout", "Foreign Stout"),"Stout",
                                  ifelse(beer_eda$beer_style_name %in% c("Porter", "Smoked", "Imperial Porter", "Baltic Porter"),"Porter",
                                         ifelse(beer_eda$beer_style_name %in% c("Traditional Ale", "Dunkel/Tmav??", "Altbier", "Brown Ale", "Oktoberfest/M??rzen", "Mild Ale", "Old Ale", "Scottish Ale", "Irish Ale", "Amber Ale"),"Brown/Amber Ale",
                                                ifelse(beer_eda$beer_style_name %in% c("Doppelbock", "Heller Bock", "Eisbock", "Dunkler Bock", "Schwarzbier"),"Bock",
                                                       ifelse(beer_eda$beer_style_name %in% c("Premium Bitter/ESB", "American Pale Ale", "English Pale Ale", "Bi??re de Garde", "Golden Ale/Blond Ale", "Bitter"),"Pale Ale",
                                                              ifelse(beer_eda$beer_style_name %in% c("Imperial IPA", "India Pale Ale (IPA)", "Session IPA", "Black IPA"),"IPA",
                                                                     ifelse(beer_eda$beer_style_name %in% c("K??lsch", "Cream Ale", "California Common", "Imperial Pils/Strong Pale Lager", "Amber Lager/Vienna", "Pilsener", "India Style Lager", "Zwickel/Keller/Landbier", "Premium Lager", "Pale Lager", "Dortmunder/Helles", "Czech Pilsner (Sv??tl??)"),"Pils/Lager",
                                                                            ifelse(beer_eda$beer_style_name %in% c("Wheat Ale", "Witbier", "German Hefeweizen", "Weizen Bock", "Dunkelweizen", "Berliner Weisse", "German Kristallweizen"),"Wheat Beer",
                                                                                   ifelse(beer_eda$beer_style_name %in% c("Abbey Dubbel", "Abbey Tripel", "Abt/Quadrupel", "Belgian Strong Ale", "American Strong Ale", "Scotch Ale", "English Strong Ale"),"Strong Ale",
                                                                                          ifelse(beer_eda$beer_style_name %in% c("Grodziskie/Gose/Lichtenhainer", "Lambic Style - Gueuze", "Sour/Wild Ale", "Sour Red/Brown", "Lambic Style - Fruit", "Lambic Style - Unblended", "Lambic Style - Faro"),"Sour",
                                                                                                 ifelse(beer_eda$beer_style_name %in% c("Sak?? - Junmai", "Sak?? - Genshu", "Sak?? - Nigori", "Sak?? - Daiginjo", "Sak?? - Ginjo", "Sak?? - Namasak??", "Sak?? - Futsu-shu", "Sak?? - Infused", "Sak?? - Koshu", "Sak?? - Honzojo", "Sak?? - Taru", "Sak?? - Tokubetsu"),"Sake",
                                                                                                        ifelse(beer_eda$beer_style_name %in% c("Saison","Belgian Ale"),"Saison","Other (Cider, Mead, etc.)"))))))))))))

beer_eda$beer_style_norm = as.factor(beer_eda$beer_style_norm)
summary(beer_eda$beer_style_norm)
summary(beer_eda)

# Beer tags could be useful later, but not so much for preliminary EDA
# beer_tag = read.csv("beer_tag.csv")
# tag = read.csv("tag.csv")
# summary(tag$name)

# Add in brewery data and link to EDA df
brewery = read.csv("brewery.csv")
brewery_type = read.csv("brewery_type.csv")
head(brewery_type)

brewery_eda = brewery[,c("id","name","location_id","postal_code","brewery_type_id")]
names(brewery_type) = c("brewery_type_id","brewery_type")
names(brewery_eda) = c("brewery_id", "brewery_name","location_id","post_code","brewery_type_id")

brewery_eda = left_join(brewery_eda, brewery_type, by = "brewery_type_id")
head(brewery_eda, 10)

beer_eda$brewery_id = as.character(beer_eda$brewery_id)
brewery_eda$brewery_id = as.character(brewery_eda$brewery_id)

beer_eda = inner_join(beer_eda, brewery_eda, by = "brewery_id")
names(beer_eda)
beer_eda = beer_eda[,c("id","name","ibu","abv","calories","beer_style_name","beer_style_norm",
                       "num_ratings","overall_rating","brewery_name","brewery_type","location_id","post_code")]

# Convert num_ratings factor into numeric
beer_eda$num_ratings = as.numeric(gsub(",","",as.character(beer_eda$num_ratings)))
summary(beer_eda)

# Clean up commas in location_id
beer_eda$location_id = gsub(",","",as.character(beer_eda$location_id))
beer_eda$post_code = as.character(beer_eda$post_code)


# Location-based data
loc = read.csv("location.csv")
summary(loc)
loc = loc[,c("id","city","state_code")]
names(loc) = c("location_id","city","state")
loc$location_id = gsub(",","",as.character(loc$location_id))

post = read.csv("postal_code.csv")
summary(post)
post$id = as.character(post$id)

# Trouble finding link between biz_pattern and post
# biz_pat = read.csv("business_pattern.csv")
# summary(biz_pat)
# names(biz_pat) = c("id","zip","year","num_employees","num_breweries")
# biz_pat$zip = as.character(biz_pat$zip)
# post = left_join(post, biz_pat, by = "zip")

post = post[,c("location_id","zip","latitude","longitude")]
post$location_id = gsub(",","",as.character(post$location_id))
post$zip = as.character(post$zip)

post_eda = left_join(post,loc, by = "location_id")
summary(post_eda)

names(post_eda) = c("loc_id","post_code","lat","long","city","state")

# Add zeros to short post codes
for(i in 1:length(post_eda$post_code)) {
  post_eda$post_code[i] = ifelse(nchar(post_eda$post_code[i]) == 3, paste0("00",post_eda$post_code[i]),
                                     ifelse(nchar(post_eda$post_code[i]) == 4, paste0("0",post_eda$post_code[i]),
                                                  post_eda$post_code[i]))
}

# Join the location info with the eda_df based on postal code
eda_df = left_join(beer_eda, post_eda, by = "post_code")
summary(eda_df)


eda_df[is.na(eda_df$city)==T,]
# there are some breweries missing cities, even though we have them in the location list

names(eda_df)

for(i in c("4881","13678","10674","21509","29","6239")) {
  eda_df[eda_df$location_id == i, c("loc_id","lat","long","city","state")] = post_eda[post_eda$loc_id == i, c("loc_id","lat","long","city","state")]
}

summary(eda_df)
str(eda_df$location_id)
str(eda_df$loc_id)

eda_df[eda_df$location_id != eda_df$loc_id & eda_df$location != "", ]
# There are some cases where location_id doesn't match loc_id
# However, differences minor and not too many occurrences
# Will just keep loc_id

names(eda_df)
eda_df = eda_df[,c(1:11,17:18,13:16)]

# Population dataframe
pop = read.csv("population.csv", stringsAsFactors = F)
names(pop)
summary(pop)
pop$postal_code_id = gsub(",","",pop$postal_code_id)

col.names = names(pop)
col.names = col.names[c(4:51)]

pop[col.names] = sapply(pop[col.names],as.numeric)
sapply(pop, class)
summary(pop)

names(pop)
# Creating the population bins
pop$under_18_m = rowSums(pop[,c(5:8)], na.rm = T)
pop$x18_to_29_m = rowSums(pop[,c(9:13)], na.rm = T)
pop$x30_to_49_m = rowSums(pop[,c(14:17)], na.rm = T)
pop$x50_to_64_m = rowSums(pop[,c(18:21)], na.rm = T)
pop$x65_plus_m = rowSums(pop[,c(22:27)], na.rm = T)

pop$under_18_f = rowSums(pop[,c(29:32)], na.rm = T)
pop$x18_to_29_f = rowSums(pop[,c(33:37)], na.rm = T)
pop$x30_to_49_f = rowSums(pop[,c(38:41)], na.rm = T)
pop$x50_to_64_f = rowSums(pop[,c(42:45)], na.rm = T)
pop$x65_plus_f = rowSums(pop[,c(46:51)], na.rm = T)

names(pop)
pop_eda = pop[,c(1:4,28,52:62)]
names(pop_eda)
names(pop_eda) = c("id","name","year","total_m","total_f","zcta","under_18_m","x18_to_29_m",
                   "x30_to_49_m","x50_to_64_m","x65_plus_m","under_18_f","x18_to_29_f","x30_to_49_f",
                   "x50_to_64_f","x65_plus_f")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

pop_eda$zcta = substrRight(pop_eda$name, 5)
head(pop_eda)

#for(i in 1:length(pop_eda$zcta)) {
#  pop_eda$zcta[i] = ifelse(nchar(pop_eda$zcta[i]) == 3, paste0("00",pop_eda$zcta[i]),
#                                     ifelse(nchar(pop_eda$zcta[i]) == 4, paste0("0",pop_eda$zcta[i]),
#                                            pop_eda$zcta[i]))
#}

head(pop_eda)
pop_eda$year = gsub(",","",pop_eda$year)

# Convert ZCTA to ZIP Codes
zip_conv = read.csv("zip_to_zcta_2016.csv")
head(zip_conv)
zip_conv = zip_conv[,c(1,8)]
zip_conv$ZIP = as.character(zip_conv$ZIP)
zip_conv$ZCTA_USE = as.character(zip_conv$ZCTA_USE)
names(zip_conv) = c("post_code", "zcta")

for(i in 1:length(zip_conv$zcta)) {
  zip_conv$zcta[i] = ifelse(nchar(zip_conv$zcta[i]) == 3, paste0("00",zip_conv$zcta[i]),
                           ifelse(nchar(zip_conv$zcta[i]) == 4, paste0("0",zip_conv$zcta[i]),
                                  zip_conv$zcta[i]))
}

for(i in 1:length(zip_conv$post_code)) {
  zip_conv$post_code[i] = ifelse(nchar(zip_conv$post_code[i]) == 3, paste0("00",zip_conv$post_code[i]),
                            ifelse(nchar(zip_conv$post_code[i]) == 4, paste0("0",zip_conv$post_code[i]),
                                   zip_conv$post_code[i]))
}

head(zip_conv)
class(pop_eda$zcta)
head(pop_eda)

pop_eda2 = left_join(pop_eda, zip_conv, by = "zcta")
names(pop_eda2)
head(pop_eda2)
pop_eda2 = pop_eda2[,c(3:5,17,7:16)]

eda_df = left_join(eda_df, pop_eda2, by = "post_code")
names(eda_df)

# Reorder and drop unnecessary pop id and name
eda_df = eda_df[,c(1,18,2:17,19:30)]
sapply(eda_df, class)

write.csv(eda_df, file = paste0(wd,"/EDA_df.csv"))

# Materialized Views
beer_by_city = read.csv("beer_data_by_city_state.csv")
pop_by_city = read.csv("pop_by_city_state_year.csv")
pop_bin = read.csv("population_bin.csv")

head(pop_by_city)
head(pop_bin)

summary(pop_bin)
## Don't think I'll need this df
# rev = read.csv("review.csv")

# Examine the dataframes
