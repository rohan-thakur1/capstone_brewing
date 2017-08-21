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
wd = "~/Desktop/Brewery Data/src_tables"
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

beer_merged2 = left_join(beer_merged, brewery_merged, by = "brewery_id")
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
post_merged[post_merged$zip_code == '94240',]
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

unique(beer_eda3$beer_style_norm)

beer_eda3$style_score = ifelse(beer_eda3$beer_style_norm == "Other (Cider, Mead, etc.)",1,
                               ifelse(beer_eda3$beer_style_norm == "Sour",2,
                                      ifelse(beer_eda3$beer_style_norm == "Wheat Beer",3,
                                             ifelse(beer_eda3$beer_style_norm == "Saison",4,
                                                    ifelse(beer_eda3$beer_style_norm == "Pale Ale",5,
                                                           ifelse(beer_eda3$beer_style_norm == "IPA",6,
                                                                  ifelse(beer_eda3$beer_style_norm == "Strong Ale",7,
                                                                         ifelse(beer_eda3$beer_style_norm == "Brown/Amber Ale",8,
                                                                                ifelse(beer_eda3$beer_style_norm == "Porter",9,
                                                                                       ifelse(beer_eda3$beer_style_norm == "Stout",10,
                                                                                              ifelse(beer_eda3$beer_style_norm == "Bock",11,
                                                                                                     ifelse(beer_eda3$beer_style_norm == "Pils/Lager",12,13))))))))))))
head(beer_eda3[is.na(beer_eda3$brewery_name) == T,c("id","name","brewery_id","brewery_name","post_code","loc_id")], 20)

# 1) Other (Cider, Mead, etc.)
# 2) Sour
# 3) Wheat Beer
# 4) Saison
# 5) Pale Ale
# 6) IPA
# 7) Strong Ale
# 8) Brown/Amber Ale
# 9) Porter
# 10) Stout
# 11) Bock
# 12) Pils/Lager
# 13) Sake

write.csv(beer_eda3, file = paste0(wd,"/../wrk_tables/beer_eda_all.csv"))


## Population dataframe ##
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

# There are some zip codes missing postal codes, which will be troublesome for business pattern merging
x = pop_merged2$zip_code[pop_merged2$post_code == ""]
x = unique(x)
y = post_merged[post_merged$zip_code %in% x, c("zip_code","post_code")]
y$post_code = as.character(gsub(",","",y$post_code))

for (i in y$zip_code) {
    pop_merged2$post_code[pop_merged2$zip_code == i] = y$post_code[y$zip_code == i]
}
# Still 227 zip codes missing post codes; alternative is to match biz_pattern post codes to zip codes

biz_pat = read.csv("business_pattern.csv")
summary(biz_pat)
biz_pat_sub = biz_pat[biz_pat$postal_code_id != "",c("postal_code_id","year","num_employees_bin","num_breweries")]
names(biz_pat_sub) = c("post_code","year","num_employees","num_breweries")

biz_pat_sub$post_code = as.character(biz_pat_sub$post_code)
biz_pat_sub$post_code = as.character(gsub(",","",biz_pat_sub$post_code))
biz_pat_sub$year = as.character(gsub(",","",biz_pat_sub$year))

biz_pat_sub = reshape(biz_pat_sub, idvar = c("post_code", "year"), timevar = "num_employees", direction = "wide")
head(biz_pat_sub)
names(biz_pat_sub) = c("post_code","year","total_breweries","Thou_plus", "Hun_to_249","Ten_to_19","One_to_4","Twenty_to_49",
                       "TwoFifty_to_499","FiveHun_to_999","Fifty_to_99","Five_to_9")

biz_pat_sub["small_ct"] = (biz_pat_sub$One_to_4 + biz_pat_sub$Five_to_9 + biz_pat_sub$Ten_to_19)
biz_pat_sub["medium_ct"] = (biz_pat_sub$Twenty_to_49 + biz_pat_sub$Fifty_to_99)
biz_pat_sub["large_ct"] = (biz_pat_sub$Hun_to_249 + biz_pat_sub$TwoFifty_to_499 + biz_pat_sub$FiveHun_to_999 + biz_pat_sub$Thou_plus)

biz_sum = biz_pat_sub[,c("post_code","year","total_breweries","small_ct","medium_ct","large_ct")]

head(biz_sum)

biz_sum_zip = left_join(biz_sum, post_merged[,c("post_code","zip_code")], by = "post_code")

pop_eda = left_join(pop_merged2, biz_sum, by = c("post_code", "year"))
pop_eda$year = as.factor(pop_eda$year)

# Check if we can find matches for missing post codes
x = pop_eda$zip_code[pop_eda$post_code == ""]
x = unique(x)
y = biz_sum_zip[biz_sum_zip$zip_code %in% x, ]
y
# No zip codes matching to the business pattern file

# Replace NAs with 0 (i.e., NA zip codes have no breweries within)
pop_eda$total_breweries[is.na(pop_eda$total_breweries)] = 0
pop_eda$small_ct[is.na(pop_eda$small_ct)] = 0
pop_eda$medium_ct[is.na(pop_eda$medium_ct)] = 0
pop_eda$large_ct[is.na(pop_eda$large_ct)] = 0

write.csv(pop_eda, file = paste0(wd,"/../wrk_tables/pop_eda.csv"))

## Load in socioeconomic data

# Education Data
edu_12 = read.csv("~/Desktop/Brewery Data/src_tables/education-data/edu_12.csv")
names(edu_12) = as.matrix(edu_12[1,])
edu_12 = edu_12[-1,]
edu_12['year'] = "2012"
edu_13 = read.csv("~/Desktop/Brewery Data/src_tables/education-data/edu_13.csv")
names(edu_13) = as.matrix(edu_13[1,])
edu_13 = edu_13[-1,]
edu_13['year'] = "2013"
edu_14 = read.csv("~/Desktop/Brewery Data/src_tables/education-data/edu_14.csv")
names(edu_14) = as.matrix(edu_14[1,])
edu_14 = edu_14[-1,]
edu_14['year'] = "2014"
edu_15 = read.csv("~/Desktop/Brewery Data/src_tables/education-data/edu_15.csv")
names(edu_15) = as.matrix(edu_15[1,])
edu_15 = edu_15[-1,]
edu_15['year'] = "2015"

edu_df = rbind(edu_12, edu_13, edu_14, edu_15)

names(edu_df)

cols = c(4:53)    
edu_df[,cols] = apply(edu_df[,cols], 2, function(x) as.numeric(as.character(x)))
                      
edu_df['hs_diploma'] = edu_df[,36] + edu_df[,38] + edu_df[,40] + edu_df[,42] + edu_df[,44]
edu_df['bachelors'] = edu_df[,46]
edu_df['postgrad'] = edu_df[,48] + edu_df[,50] + edu_df[,52]
edu_df['no_diploma'] = edu_df[,4] - edu_df$hs_diploma - edu_df$bachelors - edu_df$postgrad

names(edu_df)
edu_df = edu_df[,c(2,54,4,55:58)]
names(edu_df) = c("zcta","year","total_pop","hs_diploma","bachelors","postgrad","no_diploma")

# May need to try semi_join to deal with duplicate zip codes under same ZCTA 
edu_df = left_join(edu_df, zip_conv, by = "zcta")
edu_df = edu_df[,c(2:8)]

# Income Data 
inc_11 = read.csv("~/Desktop/Brewery Data/src_tables/income-data/income_11.csv")
names(inc_11) = as.matrix(inc_11[1,])
inc_11 = inc_11[-1,]
inc_11['year'] = "2011"
inc_12 = read.csv("~/Desktop/Brewery Data/src_tables/income-data/income_12.csv")
names(inc_12) = as.matrix(inc_12[1,])
inc_12 = inc_12[-1,]
inc_12['year'] = "2012"
inc_13 = read.csv("~/Desktop/Brewery Data/src_tables/income-data/income_13.csv")
names(inc_13) = as.matrix(inc_13[1,])
inc_13 = inc_13[-1,]
inc_13['year'] = "2013"
inc_14 = read.csv("~/Desktop/Brewery Data/src_tables/income-data/income_14.csv")
names(inc_14) = as.matrix(inc_14[1,])
inc_14 = inc_14[-1,]
inc_14['year'] = "2014"
inc_15 = read.csv("~/Desktop/Brewery Data/src_tables/income-data/income_15.csv")
names(inc_15) = as.matrix(inc_15[1,])
inc_15 = inc_15[-1,]
inc_15['year'] = "2015"

names(inc_11) = names(inc_15)
names(inc_12) = names(inc_15)
names(inc_13) = names(inc_15)
names(inc_14) = names(inc_15)

inc_df = rbind(inc_11, inc_12, inc_13, inc_14, inc_15)
names(inc_df)

inc_df = inc_df[,c(2,124,4,6,8,10,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74)]

names(inc_df) = c("zcta","year","total_hh","med_inc","pct_white_hh","med_inc_white","pct_15to24","med_inc_15to24","pct_25to44",
                  "med_inc_25to44","pct_45to64","med_inc_45to64","pct_65plus","med_inc_65plus","pct_families","med_inc_fam",
                  "pct_fam_w_kids","med_inc_kids","pct_fam_no_kids","med_inc_nokids","pct_married","med_inc_married")

cols = c(3:22)    
inc_df[,cols] = apply(inc_df[,cols], 2, function(x) as.numeric(as.character(x)))

inc_df = left_join(inc_df, zip_conv, by = "zcta")
inc_df = inc_df[,c(2:23)]

# Merge with pop_eda
pop_eda2 = left_join(pop_eda, edu_df, by = c("zip_code", "year"))
pop_eda2 = left_join(pop_eda2, inc_df, by = c("zip_code","year"))

pop_eda2$year = as.factor(pop_eda2$year)
head(pop_eda2)

write.csv(pop_eda2, file = paste0(wd,"/../wrk_tables/pop_eda2.csv"))

summary(pop_eda2)
summary(inc_df)

## Regression analysis
colnames(pop_eda2)[25] = "pop_over25"

# Transforming population statistics into percentages
pop_eda2['total_pop'] = pop_eda2$total_m + pop_eda2$total_f
pop_eda2$total_m = pop_eda2$total_m/pop_eda2$total_pop
pop_eda2$total_f = pop_eda2$total_f/pop_eda2$total_pop

pop_eda2$under_18_m = pop_eda2$under_18_m/pop_eda2$total_pop
pop_eda2$under_18_f = pop_eda2$under_18_f/pop_eda2$total_pop
pop_eda2$x18_to_29_m = pop_eda2$x18_to_29_m/pop_eda2$total_pop
pop_eda2$x18_to_29_f = pop_eda2$x18_to_29_f/pop_eda2$total_pop
pop_eda2$x30_to_49_m = pop_eda2$x30_to_49_m/pop_eda2$total_pop
pop_eda2$x30_to_49_f = pop_eda2$x30_to_49_f/pop_eda2$total_pop
pop_eda2$x50_to_64_m = pop_eda2$x50_to_64_m/pop_eda2$total_pop
pop_eda2$x50_to_64_f = pop_eda2$x50_to_64_f/pop_eda2$total_pop
pop_eda2$x65_plus_m = pop_eda2$x65_plus_m/pop_eda2$total_pop
pop_eda2$x65_plus_f = pop_eda2$x65_plus_f/pop_eda2$total_pop

pop_eda2 = pop_eda2[,c(1:13,16:50)]
pop_eda2 = pop_eda2[,c(1,48,23,2:13,24:47,14:22)]
names(pop_eda2)

pop_eda2$hs_diploma = (pop_eda2$hs_diploma + pop_eda2$bachelors + pop_eda2$postgrad)/pop_eda2$pop_over25
pop_eda2$bachelors = (pop_eda2$bachelors + pop_eda2$postgrad)/pop_eda2$pop_over25
pop_eda2$postgrad = (pop_eda2$postgrad)/pop_eda2$pop_over25
pop_eda2$no_diploma = (pop_eda2$no_diploma)/pop_eda2$pop_over25

pop_eda2$pct_white_hh = pop_eda2$pct_white_hh/100
pop_eda2$pct_15to24 = pop_eda2$pct_15to24/100
pop_eda2$pct_25to44 = pop_eda2$pct_25to44/100
pop_eda2$pct_45to64 = pop_eda2$pct_45to64/100
pop_eda2$pct_65plus = pop_eda2$pct_65plus/100

# pop_eda2$pct_families = pop_eda2$pct_families/100
pop_eda2$pct_fam_w_kids = pop_eda2$pct_fam_w_kids/100
pop_eda2$pct_fam_no_kids = pop_eda2$pct_fam_no_kids/100

pop_eda2$pct_married = pop_eda2$pct_married/100
pop_eda2['pop_growth'] = pop_eda2$total_pop


pop_df = read.csv('~/Desktop/Brewery Data/wrk_tables/pop_eda3.csv')
summary(pop_df)
names(pop_df)
pop_df = pop_df[,c(2:3,50,4:49)]
pop_df = pop_df[with(pop_df, order(pop_df$zip_code, pop_df$year)),]
head(pop_df,5)

for(i in 1:length(pop_df$year)) {
  pop_df$zip_code[i] = ifelse(nchar(pop_df$zip_code[i]) == 3, paste0("00",pop_df$zip_code[i]),
                              ifelse(nchar(pop_df$zip_code[i]) == 4, paste0("0",pop_df$zip_code[i]),
                                     pop_df$zip_code[i]))
}


for (i in 2:length(pop_df$year)){
  pop_df$pop_growth[i] = ifelse(pop_df$zip_code[i] == pop_df$zip_code[i-1], 
                             (pop_df$total_pop[i]/pop_df$total_pop[i-1])-1,NA)
}

pop_df$total_breweries = (pop_df$total_breweries/pop_df$total_pop)*100000
names(pop_df)
pop_df = pop_df[,c(1:46)]

colnames(pop_df)[46]= "brew_per_100k"

do.call(data.frame,lapply(pop_df, function(x) replace(x, is.infinite(x),NA)))
# pop_df$pct_families = pop_df$pct_families*100
pop_df$pct_families = pop_df$pct_families/pop_df$total_hh

pop_df$pop_growth = ifelse(pop_df$pop_growth == Inf, NA, pop_df$pop_growth)

write.csv(pop_df, file = paste0('~/Desktop/Brewery Data/wrk_tables/pop_eda3.csv'))

# Population subsets by year
pop_12 = pop_df[pop_df$year == '2012',]
pop_13 = pop_df[pop_df$year == '2013',]
pop_14 = pop_df[pop_df$year == '2014',]
pop_15 = pop_df[pop_df$year == '2015',]

# Add number of breweries by zip code for future years
brew_13 = pop_13[,c("zip_code","brew_per_100k")]
names(brew_13) = c("zip_code","brew_13")
brew_14 = pop_14[,c("zip_code","brew_per_100k")]
names(brew_14) = c("zip_code","brew_14")
brew_15 = pop_15[,c("zip_code","brew_per_100k")]
names(brew_15) = c("zip_code","brew_15")

pop_12 = left_join(pop_12, brew_13, by = "zip_code")
pop_12 = left_join(pop_12, brew_14, by = "zip_code")
pop_12 = left_join(pop_12, brew_15, by = "zip_code")
pop_13 = left_join(pop_13, brew_14, by = "zip_code")
pop_13 = left_join(pop_13, brew_15, by = "zip_code")
pop_14 = left_join(pop_14, brew_15, by = "zip_code")

# Get rid of NAs
pop_12$brew_13[is.na(pop_12$brew_13)] = 0
pop_12$brew_14[is.na(pop_12$brew_14)] = 0
pop_12$brew_15[is.na(pop_12$brew_15)] = 0
pop_13$brew_14[is.na(pop_13$brew_14)] = 0
pop_13$brew_15[is.na(pop_13$brew_15)] = 0
pop_14$brew_15[is.na(pop_14$brew_15)] = 0

# Scale population by 100k
pop_12$total_pop = pop_12$total_pop/100000
pop_13$total_pop = pop_13$total_pop/100000
pop_14$total_pop = pop_14$total_pop/100000
pop_15$total_pop = pop_15$total_pop/100000

write.csv(pop_12, file = paste0('~/Desktop/Brewery Data/wrk_tables/pop_12.csv'))
write.csv(pop_13, file = paste0('~/Desktop/Brewery Data/wrk_tables/pop_13.csv'))
write.csv(pop_14, file = paste0('~/Desktop/Brewery Data/wrk_tables/pop_14.csv'))
write.csv(pop_15, file = paste0('~/Desktop/Brewery Data/wrk_tables/pop_15.csv'))


# Linear Regression Modeling
names(pop_df)

mod_12_pred_13 = lm(brew_13 ~ total_pop + pop_growth + total_m + x18_to_29_m + x18_to_29_f + x30_to_49_m  + x30_to_49_f +
                      x50_to_64_m + x50_to_64_f + x65_plus_m + x65_plus_f + hs_diploma + bachelors + postgrad + med_inc +
                      med_inc_white + med_inc_15to24 + med_inc_25to44 + med_inc_45to64 + med_inc_65plus + med_inc_fam + med_inc_kids +
                      pct_white_hh + pct_families + pct_fam_w_kids + pct_married + brew_per_100k, data = pop_12, na.action = na.omit)
mod_12_pred_14 = lm(brew_14 ~ total_pop + pop_growth + total_m + x18_to_29_m + x18_to_29_f + x30_to_49_m  + x30_to_49_f +
                      x50_to_64_m + x50_to_64_f + x65_plus_m + x65_plus_f + hs_diploma + bachelors + postgrad + med_inc +
                      med_inc_white + med_inc_15to24 + med_inc_25to44 + med_inc_45to64 + med_inc_65plus + med_inc_fam + med_inc_kids +
                      pct_white_hh + pct_families + pct_fam_w_kids + pct_married + brew_per_100k, data = pop_12, na.action = na.omit)
mod_12_pred_15 = lm(brew_15 ~ total_pop + pop_growth + total_m + x18_to_29_m + x18_to_29_f + x30_to_49_m  + x30_to_49_f +
                      x50_to_64_m + x50_to_64_f + x65_plus_m + x65_plus_f + hs_diploma + bachelors + postgrad + med_inc +
                      med_inc_white + med_inc_15to24 + med_inc_25to44 + med_inc_45to64 + med_inc_65plus + med_inc_fam + med_inc_kids +
                      pct_white_hh + pct_families + pct_fam_w_kids + pct_married + brew_per_100k, data = pop_12, na.action = na.omit)

mod_13_pred_14 = lm(brew_14 ~ total_pop + pop_growth + total_m + x18_to_29_m + x18_to_29_f + x30_to_49_m  + x30_to_49_f +
                      x50_to_64_m + x50_to_64_f + x65_plus_m + x65_plus_f + hs_diploma + bachelors + postgrad + med_inc +
                      med_inc_white + med_inc_15to24 + med_inc_25to44 + med_inc_45to64 + med_inc_65plus + med_inc_fam + med_inc_kids +
                      pct_white_hh + pct_families + pct_fam_w_kids + pct_married + brew_per_100k, data = pop_13, na.action = na.omit)
mod_13_pred_15 = lm(brew_15 ~ total_pop + pop_growth + total_m + x18_to_29_m + x18_to_29_f + x30_to_49_m  + x30_to_49_f +
                      x50_to_64_m + x50_to_64_f + x65_plus_m + x65_plus_f + hs_diploma + bachelors + postgrad + med_inc +
                      med_inc_white + med_inc_15to24 + med_inc_25to44 + med_inc_45to64 + med_inc_65plus + med_inc_fam + med_inc_kids +
                      pct_white_hh + pct_families + pct_fam_w_kids + pct_married + brew_per_100k, data = pop_13, na.action = na.omit)

mod_14_pred_15 = lm(brew_15 ~ total_pop + pop_growth + total_m + x18_to_29_m + x18_to_29_f + x30_to_49_m  + x30_to_49_f +
                      x50_to_64_m + x50_to_64_f + x65_plus_m + x65_plus_f + hs_diploma + bachelors + postgrad + med_inc +
                      med_inc_white + med_inc_15to24 + med_inc_25to44 + med_inc_45to64 + med_inc_65plus + med_inc_fam + med_inc_kids +
                      pct_white_hh + pct_families + pct_fam_w_kids + pct_married + brew_per_100k, data = pop_14, na.action = na.omit)


summary(mod_12_pred_13)
summary(mod_12_pred_14)
summary(mod_12_pred_15)

summary(mod_13_pred_14)
summary(mod_13_pred_15)
summary(mod_14_pred_15)

# Comparing our ranking of the best beer cities to the US's largest cities

beer_cities = c("San Francisco","Berkeley","Oakland","Glendale","Scottsdale","San Diego",
                "Portland","Anaheim","Denver","Tampa","St. Petersburg","Austin","Seattle",
                "St. Louis","Boston","Cincinnati","Santa Ana","Pittsburgh","Reno","Anchorage",
                "Honolulu","Milwaukee","Birmingham","Nashville-Davidson","Jersey City","Durham")

biggest_cities = c("New York","Los Angeles","Chicago","Houston","Philadelphia","Phoenix","San Antonio",
              "San Diego","Dallas","San Jose","Austin","Indianapolis","Jacksonville","San Francisco",
              "Columbus","Charlotte","Fort Worth","Detroit","El Paso","Memphis","Seattle","Denver",
              "Washington","Boston","Nashville-Davidson")

mean(pop_12[pop_12$city %in% beer_cities,c("bachelors")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities,c("bachelors")], na.rm = TRUE)

mean(pop_12[pop_12$city %in% beer_cities,c("hs_diploma")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities,c("hs_diploma")], na.rm = TRUE)

mean(pop_12[pop_12$city %in% beer_cities,c("postgrad")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities,c("postgrad")], na.rm = TRUE)

mean(pop_12[pop_12$city %in% beer_cities,c("total_m")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities,c("total_m")], na.rm = TRUE)

mean(pop_12[pop_12$city %in% beer_cities,c("x18_to_29_m")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities,c("x18_to_29_m")], na.rm = TRUE)

mean(pop_12[pop_12$city %in% beer_cities,c("x18_to_29_f")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities,c("x18_to_29_f")], na.rm = TRUE)

mean(pop_12[pop_12$city %in% beer_cities,c("x30_to_49_m")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities,c("x30_to_49_m")], na.rm = TRUE)

mean(pop_12[pop_12$city %in% beer_cities,c("x30_to_49_f")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities,c("x30_to_49_f")], na.rm = TRUE)

mean(pop_12[pop_12$city %in% beer_cities,c("pct_white_hh")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities,c("pct_white_hh")], na.rm = TRUE)

mean(pop_12[pop_12$city %in% beer_cities,c("pct_families")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities,c("pct_families")], na.rm = TRUE)

mean(pop_12[pop_12$city %in% beer_cities,c("pct_married")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities,c("pct_married")], na.rm = TRUE)

mean(pop_12[pop_12$city %in% beer_cities,c("med_inc")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities,c("med_inc")], na.rm = TRUE)

mean(pop_12[pop_12$city %in% beer_cities, c("pop_growth")], na.rm = TRUE)
mean(pop_12[pop_12$city %in% biggest_cities, c("pop_growth")], na.rm = TRUE)

summary(mod_full)
## Loading Yelp Data
yelp_data = read.csv("yelp_data.csv")

summary(yelp_data)
names(yelp_data)
yelp_data = yelp_data[,c(2,5:8,1,9,10,3:4)]

# Limit to just US states
state_list = c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY",
              "LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK",
              "OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY")

yelp = yelp_data[yelp_data$state %in% state_list, ]
unique(yelp$state)

summary(yelp)
yelp[yelp$zip_code == "", ]

head(yelp$latitude)

yelp[yelp$id == 'bright-ideas-brewing-north-adams', c("latitude","longitude")] = c(42.701934, -73.113943)
yelp[yelp$id == 'coin-haus-la-mesa', c("latitude","longitude")] = c(32.765961, -117.017677)

# Cleaning up Zip Code Data
yelp$zip_code = as.character(yelp$zip_code)

for(i in 1:length(yelp$zip_code)) {
  yelp$zip_code[i] = ifelse(nchar(yelp$zip_code[i]) == 3, paste0("00",yelp$zip_code[i]),
                            ifelse(nchar(yelp$zip_code[i]) == 4, paste0("0",yelp$zip_code[i]),
                                   yelp$zip_code[i]))
}



yelp[yelp$id == 'mispillion-river-brewing-milford', c("zip_code")] = c("19963")
yelp[yelp$id == 'pizza-port-jamul', c("zip_code")] = c("91935")
yelp[yelp$id == 'cisco-brew-pub-boston-2', c("zip_code")] = c("02128")

summary(yelp)

# Cleaning up Pricing info
'%!in%' <- function(x,y)!('%in%'(x,y))

yelp = yelp[yelp$price %in% c("$","$$","$$$","$$$$",""),]
yelp$price = factor(yelp$price)
summary(yelp$price)

yelp$price[yelp$price == ""] = NA
yelp$price = factor(yelp$price)

yelp$state = factor(yelp$state)

summary(yelp)

# Getting rid of entry in South America
yelp = yelp[yelp$latitude > 0,]

summary(yelp)

write.csv(yelp, file = paste0(wd,"/yelp_data.csv"))


# Working with review data
review_data = read.csv("review.csv")

