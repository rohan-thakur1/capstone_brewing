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

beer_sub = beer[,c("id","brewery_id","beer_style_id","abv","calories", "ibu", 
                   "name", "description","num_ratings","overall_rating","beer_style_rating","mean_rating")]
beer_style_sub = beer_style[,c("id","name")]
names(beer_style_sub) = c("beer_style_id", "beer_style_name")

# Joining to add in the style names
beer_merged = left_join(beer_sub, beer_style_sub, by = "beer_style_id")
summary(beer_merged$beer_style_name)

# Cleaning up the beer style category
beer_merged$beer_style_norm = ifelse(beer_merged$beer_style_name %in% c("Stout", "Sweet Stout", "Imperial Stout", "Dry Stout", "Foreign Stout"),"Stout",
                                  ifelse(beer_merged$beer_style_name %in% c("Porter", "Smoked", "Imperial Porter", "Baltic Porter"),"Porter",
                                         ifelse(beer_merged$beer_style_name %in% c("Traditional Ale", "Dunkel/Tmav??", "Altbier", "Brown Ale", "Oktoberfest/M??rzen", "Mild Ale", "Old Ale", "Scottish Ale", "Irish Ale", "Amber Ale"),"Brown/Amber Ale",
                                                ifelse(beer_merged$beer_style_name %in% c("Doppelbock", "Heller Bock", "Eisbock", "Dunkler Bock", "Schwarzbier"),"Bock",
                                                       ifelse(beer_merged$beer_style_name %in% c("Premium Bitter/ESB", "American Pale Ale", "English Pale Ale", "Bi??re de Garde", "Golden Ale/Blond Ale", "Bitter"),"Pale Ale",
                                                              ifelse(beer_merged$beer_style_name %in% c("Imperial IPA", "India Pale Ale (IPA)", "Session IPA", "Black IPA"),"IPA",
                                                                     ifelse(beer_merged$beer_style_name %in% c("K??lsch", "Cream Ale", "California Common", "Imperial Pils/Strong Pale Lager", "Amber Lager/Vienna", "Pilsener", "India Style Lager", "Zwickel/Keller/Landbier", "Premium Lager", "Pale Lager", "Dortmunder/Helles", "Czech Pilsner (Sv??tl??)"),"Pils/Lager",
                                                                            ifelse(beer_merged$beer_style_name %in% c("Wheat Ale", "Witbier", "German Hefeweizen", "Weizen Bock", "Dunkelweizen", "Berliner Weisse", "German Kristallweizen"),"Wheat Beer",
                                                                                   ifelse(beer_merged$beer_style_name %in% c("Abbey Dubbel", "Abbey Tripel", "Abt/Quadrupel", "Belgian Strong Ale", "American Strong Ale", "Scotch Ale", "English Strong Ale"),"Strong Ale",
                                                                                          ifelse(beer_merged$beer_style_name %in% c("Grodziskie/Gose/Lichtenhainer", "Lambic Style - Gueuze", "Sour/Wild Ale", "Sour Red/Brown", "Lambic Style - Fruit", "Lambic Style - Unblended", "Lambic Style - Faro"),"Sour",
                                                                                                 ifelse(beer_merged$beer_style_name %in% c("Sak?? - Junmai", "Sak?? - Genshu", "Sak?? - Nigori", "Sak?? - Daiginjo", "Sak?? - Ginjo", "Sak?? - Namasak??", "Sak?? - Futsu-shu", "Sak?? - Infused", "Sak?? - Koshu", "Sak?? - Honzojo", "Sak?? - Taru", "Sak?? - Tokubetsu"),"Sake",
                                                                                                        ifelse(beer_merged$beer_style_name %in% c("Saison","Belgian Ale"),"Saison","Other (Cider, Mead, etc.)"))))))))))))

beer_merged$beer_style_norm = as.factor(beer_merged$beer_style_norm)
summary(beer_merged$beer_style_norm)
summary(beer_merged)

# Beer tags could be useful later, but not so much for preliminary EDA
# beer_tag = read.csv("beer_tag.csv")
# tag = read.csv("tag.csv")
# summary(tag$name)

# Add in brewery data and link to EDA df
brewery = read.csv("brewery.csv")
brewery_type = read.csv("brewery_type.csv")
head(brewery_type)

names(brewery)

brewery_sub = brewery[,c("id","name","location_id","postal_code","country","brewery_type_id")]
names(brewery_type) = c("brewery_type_id","brewery_type")
names(brewery_sub) = c("brewery_id", "brewery_name","location_id","zip_code","country","brewery_type_id")

brewery_merged = left_join(brewery_sub, brewery_type, by = "brewery_type_id")
head(brewery_merged, 10)

beer_merged$brewery_id = as.character(beer_merged$brewery_id)
brewery_merged$brewery_id = as.character(brewery_merged$brewery_id)

beer_merged2 = inner_join(beer_merged, brewery_merged, by = "brewery_id")
names(beer_merged2)
beer_eda = beer_merged2[,c(1:3,19,7:8,4:6,9:15,20,16:18)]
names(beer_eda)

# Convert num_ratings factor into numeric
beer_eda$num_ratings = as.numeric(gsub(",","",as.character(beer_eda$num_ratings)))
summary(beer_eda)

# Clean up commas in location_id
beer_eda$location_id = gsub(",","",as.character(beer_eda$location_id))
beer_eda$zip_code = as.character(beer_eda$zip_code)


# Location-based data
loc = read.csv("location.csv")
summary(loc)
loc_sub = loc[,c("id","city","state_code")]
names(loc_sub) = c("location_id","city","state")
loc_sub$location_id = gsub(",","",as.character(loc_sub$location_id))

post = read.csv("postal_code.csv")
summary(post)
post$id = as.character(post$id)

post_sub = post[,c("id","location_id","zip","latitude","longitude")]
post_sub$location_id = gsub(",","",as.character(post_sub$location_id))
post_sub$zip = as.character(post_sub$zip)

post_merged = left_join(post_sub,loc_sub, by = "location_id")
summary(post_merged)

names(post_merged) = c("post_code","loc_id","zip_code","lat","long","city","state")

# Add zeros to shorter zip codes
for(i in 1:length(post_merged$zip_code)) {
  post_merged$zip_code[i] = ifelse(nchar(post_merged$zip_code[i]) == 3, paste0("00",post_merged$zip_code[i]),
                                     ifelse(nchar(post_merged$zip_code[i]) == 4, paste0("0",post_merged$zip_code[i]),
                                                  post_merged$zip_code[i]))
}

# Join the location info with the eda_df based on zip code
beer_eda2 = left_join(beer_eda, post_merged, by = "zip_code")
summary(beer_eda2)

beer_eda2[is.na(beer_eda2$city)==T,]
# there are some breweries missing cities, even though we have them in the location list

names(beer_eda2)

for(i in c("4881","13678","10674","21509","29","6239")) {
  beer_eda2[beer_eda2$location_id == i, c("loc_id","lat","long","city","state")] = post_merged[post_merged$loc_id == i, c("loc_id","lat","long","city","state")]
}

summary(beer_eda2)
str(beer_eda2$location_id)
str(beer_eda2$loc_id)

beer_eda2[beer_eda2$location_id != beer_eda2$loc_id & beer_eda2$location != "", ]
# There are some cases where location_id doesn't match loc_id
# However, differences minor and not too many occurrences
# Will just keep loc_id

names(beer_eda2)
beer_eda3 = beer_eda2[,c(1:4,21:22,5:17,25:26,19:20,23:24)]
names(beer_eda3)


write.csv(beer_eda3, file = paste0(wd,"/beer_eda.csv"))


# Population dataframe
pop = read.csv("population.csv")
names(pop)
pop = as_data_frame(apply(pop,2,function(y) as.character(gsub(",","",y))))

pop$year = as.factor(gsub(",","",pop$year))

col.names = names(pop)
col.names = col.names[c(4:51)]
col.names
pop[col.names] = sapply(pop[col.names],as.numeric)
sapply(pop, class)
head(pop, 30)
summary(pop)
names(pop)

pop2 = pop

# Creating the population bins
pop2$under_18_m = rowSums(pop2[,c(5:8)], na.rm = T)
pop2$x18_to_29_m = rowSums(pop2[,c(9:13)], na.rm = T)
pop2$x30_to_49_m = rowSums(pop2[,c(14:17)], na.rm = T)
pop2$x50_to_64_m = rowSums(pop2[,c(18:21)], na.rm = T)
pop2$x65_plus_m = rowSums(pop2[,c(22:27)], na.rm = T)

pop2$under_18_f = rowSums(pop2[,c(29:32)], na.rm = T)
pop2$x18_to_29_f = rowSums(pop2[,c(33:37)], na.rm = T)
pop2$x30_to_49_f = rowSums(pop2[,c(38:41)], na.rm = T)
pop2$x50_to_64_f = rowSums(pop2[,c(42:45)], na.rm = T)
pop2$x65_plus_f = rowSums(pop2[,c(46:51)], na.rm = T)

names(pop2)
pop_sub = pop2[,c(1:4,28,52:62)]
names(pop_sub)
head(pop_sub)
names(pop_sub) = c("id","name","year","total_m","total_f","post_code","under_18_m","x18_to_29_m",
                   "x30_to_49_m","x50_to_64_m","x65_plus_m","under_18_f","x18_to_29_f","x30_to_49_f",
                   "x50_to_64_f","x65_plus_f")

head(pop_sub)

# Use ZCTA and convert to zip code
substrRight <- function(x, n){
 substr(x, nchar(x)-n+1, nchar(x))
}

pop_sub$zcta = substrRight(pop_sub$name, 5)

# Convert ZCTA to ZIP Codes
zip_conv = read.csv("zip_to_zcta_2016.csv")
head(zip_conv)
zip_conv = zip_conv[,c(1,8)]
zip_conv$ZIP = as.character(zip_conv$ZIP)
zip_conv$ZCTA_USE = as.character(zip_conv$ZCTA_USE)
names(zip_conv) = c("zip_code", "zcta")

for(i in 1:length(zip_conv$zcta)) {
  zip_conv$zcta[i] = ifelse(nchar(zip_conv$zcta[i]) == 3, paste0("00",zip_conv$zcta[i]),
                           ifelse(nchar(zip_conv$zcta[i]) == 4, paste0("0",zip_conv$zcta[i]),
                                  zip_conv$zcta[i]))
}

for(i in 1:length(zip_conv$zip_code)) {
  zip_conv$zip_code[i] = ifelse(nchar(zip_conv$zip_code[i]) == 3, paste0("00",zip_conv$zip_code[i]),
                            ifelse(nchar(zip_conv$zip_code[i]) == 4, paste0("0",zip_conv$zip_code[i]),
                                   zip_conv$zip_code[i]))
}

pop_sub = left_join(pop_sub, zip_conv, by = "zcta")

names(pop_sub)
pop_sub2 = pop_sub[,c(6,18,3:5,7:16)]

head(post_merged)
head(pop_sub2)
pop_merged = left_join(pop_sub2, post_merged, by="zip_code")
pop_merged$year = as.character(pop_merged$year)

names(pop_merged)

pop_merged2 = pop_merged[,c(3:15,1,17:21,2)]

names(pop_merged2)
colnames(pop_merged2)[14] = "post_code"

biz_pat = read.csv("business_pattern.csv")
summary(biz_pat)
biz_pat_sub = biz_pat[,c("postal_code_id","year","num_employees_bin","num_breweries")]
names(biz_pat_sub) = c("post_code","year","num_employees","num_breweries")

biz_pat_sub$post_code = as.character(biz_pat_sub$post_code)
biz_pat_sub$year = as.character(gsub(",","",biz_pat_sub$year))

pop_eda = left_join(pop_merged2, biz_pat_sub, by = c("post_code", "year"))
pop_eda$year = as.factor(pop_eda$year)

write.csv(pop_eda, file = paste0(wd,"/pop_eda.csv"))

# EDA and Graphics [TBU]
beer_eda = beer_eda3
summary(beer_eda)
summary(pop_eda)

