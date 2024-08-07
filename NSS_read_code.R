# Note for user: If you don't have any packages from below install them using :
# For example to install package readr: 
# install.packages("readr")

library(readxl)
library(readr)
library(tidyverse)
library(plyr)
library(dplyr)
library(tidyr)


dict_test<- read_excel("dict_test.xls")
#This file is the dictionary to read columns

# Read level 1 file
nss_widths_l1 = c(dict_test$Length[dict_test$Level==1]) #Change level value for the data as required
nss_columns_l1 = c(dict_test$Item[dict_test$Level==1]) #Change level value for the data as required

level1 <- read.fwf(file="r77182v1L01.txt",
                 widths=nss_widths_l1,
                 col.names = nss_columns_l1,
                 as.is=TRUE,
                 skip = 0,
                 comment.char="",
                 colClasses = "character" # To read all columns as strings - in order to make commonid accurately later
                 )

# The last 5 characters of the file names are very important.
# For example, in the above file v1L101 represents Visit1 (v1) and Level 01 (L01)
# You can use this to see which visit and level data you need. 
# A test to see if the data is read correctly, is to check the number of rows with No. of Records in the readme PDF 

# And now create the commonid which is used later for merging with other files
level1$Common.ID <- do.call(paste0, level1[1:15])
# The above code is to create commonid is needed to merge data from other levels.
# All other levels of data except 1 have common ID which is nothing but the first 15 columns of level 1 combined into one. 
# You can use this to merge meta data columns such as state, district, date, visit number, etc. which are only present in Level 1. 
# The above meta data is for visit 1. If you want meta data for visit 2 use file "r77182v2L01.txt"

# Let's get rid of the columns we don't want/need in level 1
level1 <- subset (level1, select = -c(Schedule.ID , FSU.Serial.No., Round, Schedule, Sample, Stratum, Sub.Stratum, Sub.Round, 
                  FOD.Sub.Region, Second.stage.stratum.no., Sample.hhld..No., Visit.number, Level, Filler, 
                  Informant.Sl.No., Response.Code, Survey.Code, Substitution.Code..Casualty.code, Employee.code, 
                  Employee.code.1, Employee.code.2, Date.of.Despatch, Time.to.canvass.minutes., No..of.investigators.FI.ASO..in.the.team, 
                  Remarks.in.block.15.16, Remarks.in.block.15.16.1, Remarks.elsewhere.in.Sch., Remarks.elsewhere.in.Sch..1, 
                  Blank, NSC))

# We can reconvert the Multiplier and other columns into numeric format
level1$Multiplier <- as.numeric(level1$Multiplier)
level1$Sector <- as.numeric(level1$Sector)
level1$Total.number.of.insurance.policies.in.the.name.of.household.member.s. <- as.numeric(level1$Total.number.of.insurance.policies.in.the.name.of.household.member.s.)

# Remove the NAs
level1[is.na(level1)] <- 0

# Let's rename the columns to shorter and clearer names
detach("package:dplyr")
# detaching dplyr because 'rename' works with plyr
level1 <- level1 %>%
  rename(no_insurance_policy = Total.number.of.insurance.policies.in.the.name.of.household.member.s.,
         nss_region = NSS.Region,
         survey_date = Date.of.Survey
  )

# NOW WE READ IN ALL THE OTHER FILES WE NEED (since this code is for understanding asset holding and mpce of urban households,
# with a focus on residential real-estate, we choose all the files related to mpce, and different types of assets).
# Note that there may be different data structures in different asset valuation data blocks - some have total assets as a simple entry,
# while others have 'serial numbers' for different asset sub-classes and totals in asset classes.

# But first, MPCE
#Creating level 4 data - mpce as per block 4
nss_widths_l4 = c(dict_test$Length[dict_test$Level==4]) #Changed level==n to read level n data
nss_columns_l4 = c(dict_test$Item[dict_test$Level==4])

level4 <- read.fwf(file="r77182v1L04.txt", 
                 widths=nss_widths_l4,
                 col.names = nss_columns_l4,
                 as.is=TRUE,
                 skip = 0,
                 comment.char=""
)

# Let's get rid of all the columns we don't really want/need in level 4
level4 <- subset(level4, select = -c(Level, Filler, usual.consumer.expenditure.in.a.month.for.household.purposes.out.of.purchase..A., 
         imputed.value.of.usual.consumption.in.a.month.from.home.grown.stock..B., 
         imputed.value.of.usual.consumption.in.a.month.from.wages.in.kind..free.collection..gifts..etc..C.., 
         expenditure.on.purchase.of.household.durable.during.last.365.days..D., Blank, NSC, Multiplier))

# Rename the unwieldy column name
level4 <- level4 %>%
  rename(total_mpce = usual.monthly.consumer.expenditure.E...A.B.C..D.12..)

# REPEAT THE STEPS FOR ALL OTHER REQUIRED DATA FILES (in this case, we want 
# level 5 - block 5.1, total value of rural land owned (s. no. 99)
# level 6 - block 5.2, total value of urban land owned (s. no. 99),
# level 7 - block 6, values of all types of real-estate (all s. nos.) -- since this is the main inquiry in this case
# level 8 - block 7, total value of livestock and poultry (s. no. 17)
# level 9 - block 8, total value of all transport equipment (s. no. 8)
# level 10 - block 9, total value of agricultural machinery (s. no. 13)
# level 11 - block 10, total value of non-farm machinery (s. no. 20)
# level 12 - block 11a, total value of financial assets (s. no. 19), value of gold assets (20) and value of paintings (21) on 30.06.18
# level 13 - block 11b, total value of shares, etc. (s. no. 5) as on 30.06.18

# Let's do this!! 
# Basic steps are, 1. Define widths and columns, 2. Read file, 3. Delete unwanted columns, rename if required, AND
# 4. filter and transpose rows to collate info we need and make the file usable (see below)
# In some cases, we will filter in only the s. no. that we need
# In other cases, transposing will be required in order to rearrange rows into columns (eg - level 7 where we want all the s. nos.)
# And level 12, where we want 3 s. nos.

# level 5 - s. no. 99
nss_widths_l5 = c(dict_test$Length[dict_test$Level==5]) #Changed level==n to read level n data
nss_columns_l5 = c(dict_test$Item[dict_test$Level==5])

level5 <- read.fwf(file="r77182v1L05.txt", 
                   widths=nss_widths_l5,
                   col.names = nss_columns_l5,
                   as.is=TRUE,
                   skip = 0,
                   comment.char=""
)

# We have read the file, now to filter in only those rows with s. no. 99
level5 <- level5 %>% filter(Serial.no.of.plot == 99)

# Delete columns we don't want
level5 <- subset(level5, select = -c(2:5,8:12)) # another way of doing it using column numbers (useful for files with a few columns)

# Renaming
level5 <- level5 %>%
  rename(rural_land_acre = Land.owned...area..acres.0.00.,
         rural_land_value = Land.owned...value.Rs..)

# Level 5 vector is ready!!

# level 6 - s. no. 99
nss_widths_l6 = c(dict_test$Length[dict_test$Level==6]) #Changed level==n to read level n data
nss_columns_l6 = c(dict_test$Item[dict_test$Level==6])

level6 <- read.fwf(file="r77182v1L06.txt", 
                   widths=nss_widths_l6,
                   col.names = nss_columns_l6,
                   as.is=TRUE,
                   skip = 0,
                   comment.char=""
)

# We have read the file, now to filter in only those rows with s. no. 99
level6 <- level6 %>% filter(Serial.no.of.plot == 99)

# Delete columns we don't want
level6 <- subset(level6, select = -c(2:5,8:12))

# Renaming
level6 <- level6 %>%
  rename(urban_land_acre = Land.owned...area..acres.0.00.,
         urban_land_value = Land.owned...value.Rs..)



# level7 - more complicated because we want all the data from all the s. nos.
nss_widths_l7 = c(dict_test$Length[dict_test$Level==7]) #Changed level==n to read level n data
nss_columns_l7 = c(dict_test$Item[dict_test$Level==7])

#Create level 7 vector
level7 <- read.fwf(file="r77182v1L07.txt", 
                 widths=nss_widths_l7,
                 col.names = nss_columns_l7,
                 as.is=TRUE,
                 skip = 0,
                 comment.char=""
                 )

# Getting rid of columns we don't want
level7 <- subset(level7, select = -c(2,3,5,7:9))

# Now to transpose the s. no. data into columns
level7 <- spread(level7, Serial.no., Owned.Value..Rs..in.whole.no..)

# Renaming - notice here the need to add '' to the existing names as they were (probably) text/character
level7 <- level7 %>%
  rename(res_dwelling = '1',
         res_other_in = '2',
         res_other_out = '3',
         bldg_ani_shed = '4',
         bldg_barn = '5',
         bldg_non_farm = '6',
         bldg_other = '7',
         bldg_in_constr = '8',
         constr_other = '9',
         total_bldgs = '10'
         )


# level 8 - s. no. 17
nss_widths_l8 = c(dict_test$Length[dict_test$Level==8]) #Changed level==n to read level n data
nss_columns_l8 = c(dict_test$Item[dict_test$Level==8])

level8 <- read.fwf(file="r77182v1L08.txt", 
                   widths=nss_widths_l8,
                   col.names = nss_columns_l8,
                   as.is=TRUE,
                   skip = 0,
                   comment.char=""
)

# We have read the file, now to filter in only those rows with s. no. 17
level8 <- level8 %>% filter(Serial.no. == 17)

# Delete columns we don't want
level8 <- subset(level8, select = -c(2:5,7:9))

# Renaming
level8 <- level8 %>%
  rename(livestock_value = Owned.Value..Rs..)


# level 9 - s. no. 8
nss_widths_l9 = c(dict_test$Length[dict_test$Level==9]) #Changed level==n to read level n data
nss_columns_l9 = c(dict_test$Item[dict_test$Level==9])

level9 <- read.fwf(file="r77182v1L09.txt", 
                   widths=nss_widths_l9,
                   col.names = nss_columns_l9,
                   as.is=TRUE,
                   skip = 0,
                   comment.char=""
)

# We have read the file, now to filter in only those rows with s. no. 8
level9 <- level9 %>% filter(Serial.no. == 8)

# Delete columns we don't want
level9 <- subset(level9, select = -c(2:5,7:10))

# Renaming
level9 <- level9 %>%
  rename(transport_value = Owned.Value..Rs..)

# level 10 - s. no. 13
nss_widths_l10 = c(dict_test$Length[dict_test$Level==10]) #Changed level==n to read level n data
nss_columns_l10 = c(dict_test$Item[dict_test$Level==10])

level10 <- read.fwf(file="r77182v1L10.txt", 
                   widths=nss_widths_l10,
                   col.names = nss_columns_l10,
                   as.is=TRUE,
                   skip = 0,
                   comment.char=""
)

# We have read the file, now to filter in only those rows with s. no. 13
level10 <- level10 %>% filter(Serial.no. == 13)

# Delete columns we don't want
level10 <- subset(level10, select = -c(2:5,7:9))

# Renaming
level10 <- level10 %>%
  rename(agri_mach_value = Owned.Value..Rs..)

# level 11 - s. no. 20
nss_widths_l11 = c(dict_test$Length[dict_test$Level==11]) #Changed level==n to read level n data
nss_columns_l11 = c(dict_test$Item[dict_test$Level==11])

level11 <- read.fwf(file="r77182v1L11.txt", 
                    widths=nss_widths_l11,
                    col.names = nss_columns_l11,
                    as.is=TRUE,
                    skip = 0,
                    comment.char=""
)

# We have read the file, now to filter in only those rows with s. no. 20
level11 <- level11 %>% filter(Serial.no. == 20)

# Delete columns we don't want
level11 <- subset(level11, select = -c(2:4,6:8))

# Renaming
level11 <- level11 %>%
  rename(non_agri_mach_value = Value..Rs..)


# level 12 - s. no. 19, 20 and 21
nss_widths_l12 = c(dict_test$Length[dict_test$Level==12]) #Changed level==n to read level n data
nss_columns_l12 = c(dict_test$Item[dict_test$Level==12])

#Create level 12 vector
level12 <- read.fwf(file="r77182v1L12.txt", 
                   widths=nss_widths_l12,
                   col.names = nss_columns_l12,
                   as.is=TRUE,
                   skip = 0,
                   comment.char=""
)

# Since we want financial assets data in detail, we are not filtering out any Serial.nos.

# Getting rid of columns we don't want
level12 <- subset(level12, select = -c(2:3,5:7,9:11))

# Now to transpose the s. no. data into columns
level12 <- spread(level12, Serial.no., Value..Rs....Col.3..Col.5...Col.4.)

# Get rid of the NAs
level12[is.na(level12)] <- 0

# Renaming
level12 <- level12 %>%
  rename(cash = '1',
         current_bank_account = '2',
         savings_bank_account = '3',
         fd_account = '4',
         po_bank_account = '5',
         other_deposits = '6',
         coop_bank_account = '7',
         nbfc_account = '8',
         credit_mfi_shg_account = '9',
         pf_account = '10',
         pension_fund = '11',
         sum_assured = '13',
         money_back = '14',
         other_fin_savings = '15',
         receivable_interest_free = '16',
         receivable_business_loan = '17',
         receivable_personal_loan = '18',
         total_fin_asset_value = '19',
         gold_bullion_value = '20',
         paintings_value = '21')


# level 13 - s. no. 5
nss_widths_l13 = c(dict_test$Length[dict_test$Level==13]) #Changed level==n to read level n data
nss_columns_l13 = c(dict_test$Item[dict_test$Level==13])

level13 <- read.fwf(file="r77182v1L13.txt", 
                    widths=nss_widths_l13,
                    col.names = nss_columns_l13,
                    as.is=TRUE,
                    skip = 0,
                    comment.char=""
)

# We have read the file, now to filter in only those rows with s. no. 5
level13 <- level13 %>% filter(Serial.no. == 5)

# Delete columns we don't want
level13 <- subset(level13, select = -c(2:7,9:11))

# Renaming
level13 <- level13 %>%
  rename(shares_value = Value..Rs....Col.3..Col.5...Col.4.)


# ALL THE DATA IS COLLECTED #

# Let's merge the data frames
# First, combine the data frames into a list
merge_test <- list(level1, level4, level5, level6, level7, level8, level9, level10, level11, level12, level13)
# Do the join by Common.ID
library(plyr) # Need to add package plyr here - but please note, use for the next command only and then detach as it conflicts with dplyr
merge_test <- join_all(merge_test, by = "Common.ID", type = 'left')
detach(package:plyr)

# Remove NAs
merge_test[is.na(merge_test)] <- 0

# Filter merged dataframe by urban (Sector == "2")
merge_test <- merge_test %>% 
  filter(Sector == "2")


## WE'RE DONE ##
# Write the csv now

write.csv(merge_test, "aidis_2019_assets.csv")


# Code to do the basic quantile analysis
# Need Hmisc library; install package if required with install.packages(Hmisc)
library(Hmisc)
# read the csv created earlier (if needed) and assign it to a dataframe
df1 <- read.csv('aidis_2019_assets.csv')

# Set the probabilities (quantiles we want the data at - uncomment lines below as required - for declies use 'seq' and for 
# the drill down use the 'c' array)
aidis_probs <- c(0.01, 0.1, 0.25, 0.4, 0.5, 0.75, 0.85, 0.9, 0.99)
# aidis_probs <- seq(0,1,0.01)

# Set the weights as equal to the Multiplier column
aidis_weights <- as.numeric(df1$Multiplier)

# total financial assets in required quantiles
total_fin_assets_quantiles <- Hmisc::wtd.quantile(df1$total_fin_asset_value, w=aidis_weights, probs = aidis_probs)
# total pension and provident funds
df1$pension_provident_funds <- rowSums(df1 [ , c(37,38)])
# pension and provident funds quantiles
pension_provident_funds_quantiles <- Hmisc::wtd.quantile(df1$pension_provident_funds, w = aidis_weights, probs = aidis_probs)

# Non-financial assets
# code to sum res_dwelling and urban_land_value columns
df1$urban_real_estate <- rowSums(df1[ , c(13,23)], na.rm = TRUE)
# code to give us urban_real_estate values at required probabilities
urban_real_estate_quantiles <- Hmisc::wtd.quantile(df1$urban_real_estate, w=aidis_weights, probs = aidis_probs)
# also in case one wants res_dwelling and urban_land_value separately (the whole is greater than the sum of the parts....)
# Hmisc::wtd.quantile(df1$res_dwelling, w=aidis_weights, probs = aidis_probs)
# Hmisc::wtd.quantile(df1$urban_land_value, w=aidis_weights, probs = aidis_probs)
# durable good assets
df1$durable_goods <- rowSums(df1[ , c(24:27)], na.rm = TRUE)
# total durable goods in required quantiles
durable_goods_quantiles <- Hmisc::wtd.quantile(df1$durable_goods, w=aidis_weights, probs = aidis_probs)
# gold/bullion in required quantiles
gold_bullion_quantiles <- Hmisc::wtd.quantile(df1$gold_bullion_value, w=aidis_weights, probs = aidis_probs)
# total art and stock value
df1$stocks_art_value <- rowSums(df1[ , c(47:48)], na.rm = TRUE)
# stocks and art value in required quantiles
stocks_art_quantiles <- Hmisc::wtd.quantile(df1$stocks_art_value, w=aidis_weights, probs = aidis_probs)
# total non-financial assets
df1$total_nonfin_assets_value <- rowSums(df1[ , c("rural_land_value", "urban_real_estate","durable_goods","gold_bullion_value","stocks_art_value")], na.rm = TRUE)
# total nonfin assets in required quantiles
nonfin_assets_quantiles <- Hmisc::wtd.quantile(df1$total_nonfin_assets_value, w=aidis_weights, probs = aidis_probs)

# Total assets
df1$total_assets <- rowSums(df1[ ,c ("total_fin_asset_value", "total_nonfin_assets_value")], na.rm = TRUE)
# total assets in required quantiles
total_assets_quantiles <- Hmisc::wtd.quantile(df1$total_assets, w=aidis_weights, probs = aidis_probs)

# CHECK AGAINST NSSO REPORT No. 588 77th ROUND #
# You can check all the weighted means of each category of asset with the mean reported by NSSO
# For example, Hmisc::wtd.mean(df1$total_fin_asset_value, w = aidis_weights)
# Will give you a result of Rs. 2,48,016 which is the same as 'deposits, etc.' column, row All-All in Table A11U
# Only our mean total assets won't match because for some reason, the report table doesn't include gold/bullion and paintings values
# You can generate quantiles (or deciles) as required using the Hmisc::wtd.quantile function for any set of assets


# number of insurance policies
num_insurance_policies <- Hmisc::wtd.quantile(df1$no_insurance_policy, w = aidis_weights, probs = aidis_probs)

# Join the quantiles dfs together using list command and write csv
# label the columns in the csv in the same order as they appear in the list command

# You can also calculate the Gini coefficient for various assets
# Here is the Gini coefficient calculated for urban_real_estate (includes all land, buildings held in urban areas) using a weighted formula
install.packages("DescTools")
library(DescTools)
Gini(df1$urban_real_estate, w = df1$Multiplier, unbiased = FALSE)
# And for total assets, land assets, building assets and financial assets
Gini(df1$total_assets, w = df1$Multiplier, unbiased = FALSE)
Gini(df1$urban_land_value, w = df1$Multiplier, unbiased = FALSE)
Gini(df1$total_bldgs, w = df1$Multiplier, unbiased = FALSE)
Gini(df1$total_fin_asset_value, w = df1$Multiplier, unbiased = FALSE)

# Going to also try and get Gini by state
# First, generate the state code by deleting the last digit of the nss_region column
df1$state_code <- as.string(substr(df1$nss_region, 1, nchar(df1$nss_region)-1))
# The lets group our rows by state_code and make a new df with weighted Gini
df1_state_gini <- df1 %>%
  group_by(df1$state_code) %>%
  summarize(Gini(total_assets, w = Multiplier, unbiased = FALSE)) %>%
  ungroup() # Have to ungroup after each group_by operation else other functions will behave weirdly

# Can write this to a csv
write.csv(df1_state_gini, "gini-by-state_csv")

# Feel free to run the above again for a different variable such as urban_real_estate
# We will also do the Gini of MPCE by NSS regions
df1_nss_region_urbanrealestate_gini <- df1 %>% 
  group_by(df1$nss_region) %>% 
  summarize(Gini(urban_real_estate, w = Multiplier, unbiased = FALSE)) %>% 
  ungroup()

write.csv(df1_nss_region_urbanrealestate_gini, "gini-urbanrealestate-region.csv")

# Getting total urban real estate Gini by state as well
df1_state_urbanrealestate_gini <- df1 %>% 
  group_by(df1$state_code) %>% 
  summarize(Gini(urban_real_estate, w = Multiplier, unbiased = FALSE)) %>% 
  ungroup()

write.csv(df1_state_urbanrealestate_gini, "gini_urban-real-estate-assets_state.csv")

# Thereafter, unload DescTools because it masks some key functions from Hmisc
unloadNamespace("DescTools")


# Now, for the liabilities side #
# All of the loans are recorded in level 14
# Each loan of a household is given a serial no., and then details about the loan are filled up - who from and when taken, amount, interest regime, purpose, etc.
# We want to focus on whether the loans are secured or not, their broad heads of purpose, and how much debt is from non-institutional sources

# Creating level 14 data
nss_widths_l14 = c(dict_test$Length[dict_test$Level==14]) #Changed level==n to read level n data
nss_columns_l14 = c(dict_test$Item[dict_test$Level==14])

level14 <- read.fwf(file="r77182v1L14.txt", 
                   widths=nss_widths_l14,
                   col.names = nss_columns_l14,
                   as.is=TRUE,
                   skip = 0,
                   comment.char=""
)

# Lets get the total loan amount (Serial.no. 99) first
level14_total_loan <- level14 %>%
  filter(Serial.no. == "99")

# Let's get rid of all the columns we don't really want/need in level 14
level14_total_loan <- subset(level14_total_loan, select = -c(Level, Filler, year.of.borrowing, whether.loan.remained.unpaid.on.30.6.2018, amount.borrowed.originally..Rs..,
                                       credit.agency, scheme.of.lending, tenure.of.loan, nature.of.interest, annual.rate.of.interest..0.00., Serial.no.,
                                       amount..Rs...repaid..including.interest..during.01.07.2018.to.date.of.survey, purpose.of.loan,
                                       amount..Rs...written.off..including.interest..during.01.07.2018.to.date.of.survey, whether.the.loan.is.secured,
                                       amount..Rs...outstanding..including.interest..as.on.date.of.survey, Blank, NSC, Multiplier))

# Rename the unwieldy column name
level14_total_loan <- level14_total_loan %>%
  rename(total_debt = amount..Rs...outstanding..including.interest..as.on.30.06.2018
         )

# Merge with level 1
merge_1_14_total <- list(level1, level14_total_loan)
library(plyr)
merge_1_14_total <- join_all(merge_1_14_total, by = "Common.ID", type = "left")
detach("package:plyr")
merge_1_14_total <- merge_1_14_total %>%
  filter(Sector == "2")
# remove NAs
# merge_1_14_total <- merge_1_14_total[!is.na(merge_1_14_total$total_debt)!=0]
# merge_1_14_total <- merge_1_14_total[!is.na(merge_1_14_total$total_debt)!=0]
merge_1_14_total <- merge_1_14_total %>% filter(!is.na(total_debt) & total_debt!=0)
merge_1_14_total[is.na(merge_1_14_total$total_debt)] != 0
Hmisc::wtd.quantile(merge_1_14_total$total_debt, w = merge_1_14_total$Multiplier, probs = aidis_probs)
# The result from the above transformation can be checked by comparing the weighted mean with the NSSO report and other sources
# So, Hmisc::wtd.mean(merge_1_14_total$total_debt, w = merge_1_14_total$Multiplier)
# Gives a result of Rs. 120,336 which is the same as reported by 
# Ashoka CEDA (https://ceda.ashoka.edu.in/aidis-2019-lower-assets-and-higher-indebtedness-for-indian-households/)

# Filtering in only those Common.IDs with total_debt > 0 to get the debt of only indebted HH
merge_1_14_total <- merge_1_14_total %>%
  filter(merge_1_14_total$total_debt > 0)
Hmisc::wtd.quantile(merge_1_14_total$total_debt, w = merge_1_14_total$Multiplier, probs = aidis_probs)
# The total observations in this df (20,842) match the number reported by NSSO report no. 588 77th round in table A35U on page A-1613
# The weighted mean of the loan in this df is
# Rs. 5,36,860 which matches Ashoka CEDA report and NSSO report, the latter if you divide total loan by total indebted HH in the same table mentioned above






# Lets get total secured loans from level 14
level14_secured <- level14 %>%
  filter(whether.the.loan.is.secured == "1")
# Get rid of unnecessary columns
level14_secured <- subset(level14_secured, select = -c(Level, Filler, year.of.borrowing, whether.loan.remained.unpaid.on.30.6.2018, amount.borrowed.originally..Rs..,
                                                             credit.agency, scheme.of.lending, tenure.of.loan, nature.of.interest, annual.rate.of.interest..0.00.,
                                                             whether.the.loan.is.secured,
                                                             amount..Rs...repaid..including.interest..during.01.07.2018.to.date.of.survey,
                                                             amount..Rs...written.off..including.interest..during.01.07.2018.to.date.of.survey,
                                                             amount..Rs...outstanding..including.interest..as.on.date.of.survey, Blank, NSC, Multiplier))
# Rename the remaining colums
level14_secured <- level14_secured %>%
  rename(loan_purpose = purpose.of.loan,
         loan_outstanding_amount_secured = amount..Rs...outstanding..including.interest..as.on.30.06.2018)

# Lets spread this by purpose of loan
level14_secured <- spread(level14_secured, loan_purpose, loan_outstanding_amount_secured)
# Rename the loan purpose columns
level14_secured <- level14_secured %>%
  rename(capex_farm = '1',
         revex_farm = '2',
         capex_nonfarm = '3',
         revex_nonfarm = '4',
         litigation = '5',
         debt_service = '6',
         investment = '7',
         education = '8',
         medical = '10',
         housing = '11',
         other_hh_exp = '12',
         other = '9'
  )

# We will use the pivot_wider function to pivot by Serial.no.
level14_secured <- level14_secured %>%
  pivot_wider(names_from = Serial.no., values_from = c(capex_farm, revex_farm, capex_nonfarm, revex_nonfarm,
                                                       litigation, debt_service, investment, education, medical,
                                                       housing, other_hh_exp, other))
# Merge with level 1
merge_1_14_secured <- list(level1, level14_secured)
library(plyr)
merge_1_14_secured <- join_all(merge_1_14_secured, by = "Common.ID", type = 'left')
detach("package:plyr")
# filter in urban only
merge_1_14_secured <- merge_1_14_secured %>%
  filter(Sector == "2")


# Make total loan column
merge_1_14_secured$total_sec_debt <- rowSums(merge_1_14_secured[ , c(8:331)], na.rm = TRUE)
# merge and join with merge_1_14_total so we can filter by HH that are indebted (n = 20842)
merge_1_14_secured <- merge_1_14_secured %>%
  left_join(merge_1_14_total %>%
              select(Common.ID, total_debt), by = "Common.ID"
            )
# remove NAs in the total debt column and filter out when total debt != 0
merge_1_14_secured <- merge_1_14_secured %>% filter(!is.na(total_debt) & total_debt!=0)

# and total secured housing and other loans columns
merge_1_14_secured$total_sec_debt_housing <- rowSums(merge_1_14_secured[ , c(251:277)], na.rm = TRUE)
# get quantiles
Hmisc::wtd.quantile(merge_1_14_secured$total_sec_debt_housing, w = merge_1_14_secured$Multiplier, probs = aidis_probs)
# now for total other secured debts
merge_1_14_secured$total_sec_debt_others <- rowSums(merge_1_14_secured[ , c(8:250, 278:331)], na.rm = TRUE)
# get quantiles
Hmisc::wtd.quantile(merge_1_14_secured$total_sec_debt_others, w = merge_1_14_secured$Multiplier, probs = aidis_probs)





# Lets get total unsecured loans from level 14
level14_unsecured <- level14 %>%
  filter(whether.the.loan.is.secured == "2")
# Get rid of unnecessary columns
level14_unsecured <- subset(level14_unsecured, select = -c(Level, Filler, year.of.borrowing, whether.loan.remained.unpaid.on.30.6.2018, amount.borrowed.originally..Rs..,
                                                       credit.agency, scheme.of.lending, tenure.of.loan, nature.of.interest, annual.rate.of.interest..0.00.,
                                                       whether.the.loan.is.secured, amount..Rs...repaid..including.interest..during.01.07.2018.to.date.of.survey,
                                                       amount..Rs...written.off..including.interest..during.01.07.2018.to.date.of.survey,
                                                       amount..Rs...outstanding..including.interest..as.on.date.of.survey, Blank, NSC, Multiplier))
# Rename the remaining columns
level14_unsecured <- level14_unsecured %>%
  rename(loan_purpose = purpose.of.loan,
         loan_outstanding_amount_unsecured = amount..Rs...outstanding..including.interest..as.on.30.06.2018)

# Lets spread this by purpose of loan
level14_unsecured <- spread(level14_unsecured, loan_purpose, loan_outstanding_amount_unsecured)
# Rename the loan purpose columns
level14_unsecured <- level14_unsecured %>%
  rename(capex_farm = '1',
         revex_farm = '2',
         capex_nonfarm = '3',
         revex_nonfarm = '4',
         litigation = '5',
         debt_service = '6',
         investment = '7',
         education = '8',
         medical = '10',
         housing = '11',
         other_hh_exp = '12',
         other = '9'
  )

# We will use the pivot_wider function to pivot by Serial.no.
level14_unsecured <- level14_unsecured %>%
  pivot_wider(names_from = Serial.no., values_from = c(capex_farm, revex_farm, capex_nonfarm, revex_nonfarm,
                                                       litigation, debt_service, investment, education, medical,
                                                       housing, other_hh_exp, other))

# Merge with level 1
merge_1_14_unsecured <- list(level1, level14_unsecured)
library(plyr)
merge_1_14_unsecured <- join_all(merge_1_14_unsecured, by = "Common.ID", type = 'left')
detach("package:plyr")
# filter in urban only
merge_1_14_unsecured <- merge_1_14_unsecured %>%
  filter(Sector == "2")
# Make the total column
merge_1_14_unsecured$total_unsec_debt <- rowSums(merge_1_14_unsecured[ , c(8:331)], na.rm = TRUE)

# merge with total debt df 
merge_1_14_unsecured <- merge_1_14_unsecured %>%
  left_join(merge_1_14_total %>%
              select(Common.ID, total_debt), by = "Common.ID"
  )
# remove NAs in the total debt column and filter out when total debt != 0
merge_1_14_unsecured <- merge_1_14_unsecured %>% filter(!is.na(total_debt) & total_debt!=0)

# Get the quantiles we want only for indebted HH
Hmisc::wtd.quantile(merge_1_14_unsecured$total_unsec_debt, w = merge_1_14_unsecured$Multiplier, probs = aidis_probs)
# and total unsecured housings columns
merge_1_14_unsecured$total_unsec_debt_housing <- rowSums(merge_1_14_unsecured[ , c(251:277)], na.rm = TRUE)
# get quantiles
Hmisc::wtd.quantile(merge_1_14_unsecured$total_unsec_debt_housing, w = merge_1_14_unsecured$Multiplier, probs = aidis_probs)




# Non-institutional debt
# Lets get total non-institutional loans from level 14
level14_non_inst <- level14 %>%
  filter(Serial.no. > 50 & Serial.no. < 99)
# Get rid of unnecessary columns
level14_non_inst <- subset(level14_non_inst, select = -c(Level, Filler, year.of.borrowing, whether.loan.remained.unpaid.on.30.6.2018, amount.borrowed.originally..Rs..,
                                                           credit.agency, scheme.of.lending, tenure.of.loan, nature.of.interest, annual.rate.of.interest..0.00.,
                                                           whether.the.loan.is.secured, amount..Rs...repaid..including.interest..during.01.07.2018.to.date.of.survey,
                                                           amount..Rs...written.off..including.interest..during.01.07.2018.to.date.of.survey,
                                                           amount..Rs...outstanding..including.interest..as.on.date.of.survey, Blank, NSC, Multiplier))
# Rename the remaining columns
level14_non_inst <- level14_non_inst %>%
  rename(loan_purpose = purpose.of.loan,
         loan_outstanding_amount_unsecured = amount..Rs...outstanding..including.interest..as.on.30.06.2018)

# Lets spread this by purpose of loan
level14_non_inst <- spread(level14_non_inst, loan_purpose, loan_outstanding_amount_unsecured)
# Rename the loan purpose columns
level14_non_inst <- level14_non_inst %>%
  rename(capex_farm = '1',
         revex_farm = '2',
         capex_nonfarm = '3',
         revex_nonfarm = '4',
         litigation = '5',
         debt_service = '6',
         investment = '7',
         education = '8',
         medical = '10',
         housing = '11',
         other_hh_exp = '12',
         other = '9'
  )

# We will use the pivot_wider function to pivot by Serial.no.
level14_non_inst <- level14_non_inst %>%
  pivot_wider(names_from = Serial.no., values_from = c(capex_farm, revex_farm, capex_nonfarm, revex_nonfarm,
                                                       litigation, debt_service, investment, education, medical,
                                                       housing, other_hh_exp, other))

# Merge with level 1
merge_1_14_non_inst <- list(level1, level14_non_inst)
library(plyr)
merge_1_14_non_inst <- join_all(merge_1_14_non_inst, by = "Common.ID", type = 'left')
detach("package:plyr")
# filter in urban only
merge_1_14_non_inst <- merge_1_14_non_inst %>%
  filter(Sector == "2")
# Make the total column
merge_1_14_non_inst$total_noninst_debt <- rowSums(merge_1_14_non_inst[ , c(8:199)], na.rm = TRUE)
# merge with total debt df 
merge_1_14_non_inst <- merge_1_14_non_inst %>%
  left_join(merge_1_14_total %>%
              select(Common.ID, total_debt), by = "Common.ID"
  )
# remove NAs in the total debt column and filter out when total debt != 0
merge_1_14_non_inst <- merge_1_14_non_inst %>% filter(!is.na(total_debt) & total_debt!=0)

# Get the quantiles we want only for indebted HH
Hmisc::wtd.quantile(merge_1_14_non_inst$total_noninst_debt, w = merge_1_14_non_inst$Multiplier, probs = aidis_probs)
# Lets also get the total of non-inst housing loans
merge_1_14_non_inst$noninst_housing_loan <- rowSums(merge_1_14_non_inst[ , c(152:167)], na.rm = TRUE)
# get quantiles
Hmisc::wtd.quantile(merge_1_14_non_inst$noninst_housing_loan, w = merge_1_14_non_inst$Multiplier, probs = aidis_probs)




# Lastly, lets get the number of mortgages by our quantiles
merge_1_14_secured$num_mortgage <- apply(X = merge_1_14_secured[ , c(251:277)], 1, FUN = function(x) length(which(x > 0)))
Hmisc::wtd.quantile(merge_1_14_secured$num_mortgage, w = merge_1_14_secured$Multiplier, probs = aidis_probs)




# further work
# trying to see if it makes sense to have a plot of the sum of frequencies (Multipliers) 
# with all urban_real_estate values, grouped by specific real-estate asset holding classes
# First, set the specific categories (Rs. value of urban real estate) we want using ifelse 
# a cut function could be used if we wanted equal buckets of frequency distribution
df1$urban_real_estate_value_category <- as.factor(
    ifelse(df1$urban_real_estate == 0, 'No real-estate asset reported',
    ifelse(df1$urban_real_estate > 0 & df1$urban_real_estate < 250000, '<2.5',
    ifelse(df1$urban_real_estate >= 250000 & df1$urban_real_estate < 500000, '2.5-5.0',
    ifelse(df1$urban_real_estate >= 500000 & df1$urban_real_estate < 1000000, '5.0-10.0',
    ifelse(df1$urban_real_estate >= 1000000 & df1$urban_real_estate < 5000000, '10.0-50.0',
    ifelse(df1$urban_real_estate > 5000000 & df1$urban_real_estate < 10000000, '50.0-100.0',
    ifelse(df1$urban_real_estate >= 10000000 & df1$urban_real_estate < 25000000, '100.0-250.0', '>250.0'))))))))
# now, to sum the frequencies (Multiplier) by grouping the categories (aggregate), and chuck it into a dataframe
df1_grouped_by_urban_real_estate <- aggregate(df1$Multiplier, list(df1$urban_real_estate_value_category), FUN = sum)
# write the csv
write.csv(df1_grouped_by_urban_real_estate, "urban-real-estate-class-frequency.csv")

# Also doing another closer cut of the data
df1$urban_real_estate_value_category <- as.factor(
  ifelse(df1$urban_real_estate < 500000, '<5.0',
  ifelse(df1$urban_real_estate >= 500000 & df1$urban_real_estate < 1000000, '5.0-10.0',
  ifelse(df1$urban_real_estate >= 1000000 & df1$urban_real_estate < 2000000, '10.0-20.0',
  ifelse(df1$urban_real_estate >= 2000000 & df1$urban_real_estate < 3000000, '20.0-30.0',
  ifelse(df1$urban_real_estate >= 3000000 & df1$urban_real_estate < 4000000, '30.0-40.0',
  ifelse(df1$urban_real_estate > 4000000 & df1$urban_real_estate < 5000000, '40.0-50.0', '>50.0')))))))
# run the aggregate (sum of Multipliers) again - note that I am overwriting the previous dataframe as I have already copied it into another file
df1_grouped_by_urban_real_estate <- aggregate(df1$Multiplier, list(df1$urban_real_estate_value_category), FUN = sum)
# write the csv again - to the same csv as I've already copied previous data out of it
write.csv(df1_grouped_by_urban_real_estate, "urban-real-estate-class-frequency.csv")

## END ##