#setwd("C:/Users/Dbrimmer/Desktop/phdyear2/Quarter3/Development")
#setwd("C:/Users/joem/Documents/Fertility/Fertility")
ptm <- proc.time()

library(tidyverse)
library(stringr)
library(readstata13)
library(zoo)
library(broom)
library(mfx)
library(parallel)
library(lme4)
library(data.table)
library(countrycode)
library(readxl)
library(stargazer)
library(multiwayvcov)
library(sandwich)


br <- BR_data[[1]]
ir <- IR_data[[1]]

###This function reads in two different forms of the dhs data (birth and individual recodes), and merges them. Then it panelizes the 
###cross-sections and adds in some variables we would like to compute.
clean_data <- function (br,ir) {
  ###This reads in the stata files. I have already limited the variables in the files so that I can fit the data in github.
  br_data <- read.dta13(br)
  
  ir_data <- read.dta13(ir)
  
  ### From the individual recode, we want all of the women who do not have any children (so do not show up in birth recode)
  ir_data <-  ir_data %>% filter(ir_data$v201=="0")
  
  ###From the birth recode, we want to eliminate all of the multiple births in a single year, as this will not work with our specification
  br_data <- br_data %>%
    filter(str_detect(b0,paste(c('single birth', '1st of multiple'), collapse="|")))
  
  ###compute the mother's age when she gives birth, create the year variable which is the year of the observation
  br_data$m_ageatbirth <- (br_data$b2-br_data$v010)
  br_data$year <- br_data$b2
  
  ### generate ones for all of the datapoints in br, as every row corresponds to a child's birth
  ### generate zeroes for all of the datapoints in ir, as these are the women who did not give birth
  br_data$birth <- 1
  ir_data$birth <- 0
  
  ### Make sure we completely get rid of women who have kids at < 13 (small proportion)
  br_data <- br_data[br_data$m_ageatbirth >12,]
  
  ### generate all of the variables that are in the birth recode but left out of individual recode
  ### so that I can easily push them together
  ir_data <- ir_data %>%
    mutate("b4" = as.character(NA),"b0"=as.factor(NA),"b1"=NA_integer_,"b3"=NA_integer_,"m10"=NA,"b5"=as.factor(NA),"b6"=NA_integer_,
           "b7"=NA_integer_,"b8"=NA_integer_,"b11"=NA_integer_,"bidx"=NA_integer_,"bord"=NA_integer_,"b2"=NA_integer_,
           "m_ageatbirth"=as.numeric(NA), "year" = v007)
  ### Pushes them together
  allwomen <- rbind(br_data,ir_data)

  ###Now I want to create a new dataframe so that I can panelize the data (turn the cross-section into a panel)
  ### I will start by selecting only the columnns we care about
  panelize <- dplyr::select(allwomen,caseid,v010,v007)
  
  ### need to properly identify unique caseids (women) and shrink the data so each woman has one row
  panelize <- panelize[!duplicated(panelize[-c(2,3)]),]
  ### using the columns that tell us the birth year of the woman and the date of her interview, calculate how old she is currently in years
  ### Then subtract 12 so that we know how many years of observations to create (one for the year she turns 13 - current year)

  panelize$freq <-  panelize$v007- panelize$v010 -12
  
  ### replicate that mother's information across the proper number of rows
  panelize <- panelize[rep(row.names(panelize),panelize$freq),1:3]
  
  
  ### create new column that indicates the year for each observation
  panelize <-  panelize %>% 
    group_by(caseid)%>%
    mutate(year = row_number()+12+ v010)
  
  ### Create the column indicating the individual's age (actually is the maximum age they will be in a given year)
  panelize$age <- panelize$year - panelize$v010
  
  ### Now bring in all of the information from the bigger dataset in the appropriate rows

  panelize_merged <- left_join(panelize, allwomen, by = c("year","caseid"))

  
  ### We need to  remove the second births in a given year from the data, as this will lead to problems 
  ###(level of observation is a mother in a year, can't have two rows with same mother and year)
  panelize_merged <-  panelize_merged %>%
    group_by(caseid,year) %>%
    mutate(doubleyear = row_number())%>%
    filter(doubleyear==1)
  
  ### Drop the column that is not needed any more
  panelize_merged <-  panelize_merged %>%
                      dplyr::select(-doubleyear)
  
  ### Create 0's for all of the years where a woman did not give birth
  panelize_merged$birth <- replace(panelize_merged$birth,is.na(panelize_merged$birth),0)

  ### First a test function that will allow me to delete the women that don't have information ( may be redundant but I will leave it in)
  testing <- panelize_merged %>% 
    group_by(caseid) %>%
    summarise(no_rows = unique(v005)[1])
  
  testing1 <- panelize_merged %>% 
    group_by(caseid) %>%
    summarise(no_rows1 = unique(v005)[2])

  names(testing1)[1] <- "case"
  
  tttttest <- cbind(testing,testing1)
  
  `%!in%` = Negate(`%in%`)
  
  ### Some of the women do not have information in their name so I will leave them out (very few as a proportion)
  panelize_merged <- panelize_merged[panelize_merged$caseid %!in% tttttest[is.na(tttttest$no_rows1) & is.na(tttttest$no_rows),1],]
  
  panelize_merged$v704 <- as.character(panelize_merged$v704)
  
  
  ### Fill in all of the new created rows with the values form the original data (expanding the data rows, did not fill in the values.
  ### This is because only some of thee data should be replicated over rows.)
  panelize_merged %<>%
    group_by(caseid) %>% 
    mutate(v000 = unique(na.omit(v000)),v102 = unique(na.omit(v102)),v106 = unique(na.omit(v106)),v130 = unique(na.omit(v130)),
           v201 = unique(na.omit(v201)),v190 = unique(na.omit(v190)),v191 = unique(na.omit(v191)),v627 = ifelse(length(unique(na.omit(v627)))==0,NA,unique(na.omit(v627))),
           v628 = ifelse(length(unique(na.omit(v628)))==0,NA,unique(na.omit(v628))),v702 = unique(na.omit(v702)),v701 = unique(na.omit(v701)),v705 = unique(na.omit(v705)),v716 = unique(na.omit(v716)),
           v717 = unique(na.omit(v717)),v508 = ifelse(v701 == "NA",NA,unique(na.omit(v508))),v704 = ifelse(v701 == "NA",NA,unique(na.omit(v704))))

  ### Create some age polynomials (don't end up using in final analysis, but I left it in just in case)
  panelize_merged <- panelize_merged %>%
    mutate(age_sq = age^2)
  
  panelize_merged <- panelize_merged %>%
    mutate(age_cu = age^3)
  
  panelize_merged <- panelize_merged %>%
    mutate(age_qu = age^4)
  
  ### Determine the current number of kids an individual has in a given year. Always starts at 0, as we removed women who had 
  ### children when they were under 12
  panelize_merged <- panelize_merged %>%
    mutate(current_numkids = ifelse(age == 13 & is.na(bidx),0,bord))
  
  panelize_merged %<>%
    group_by(caseid) %>%
    mutate(current_numkids = na.locf(current_numkids,na.rm=F))
  
  ### Convert some variables to lower case so they match across all datasets
  panelize_merged$b5 <- tolower(panelize_merged$b5)
  panelize_merged$v102 <- tolower(panelize_merged$v102)
  
  ### Create dataset that allows us to determine which kids die so that I can match them to the main dataset in the year they died
  ### We want this to construct the current number of living children a parent has
  deadkids <- panelize_merged[panelize_merged$b5 =='no' & !is.na(panelize_merged$b5),] %>% ### THIS MAY BE ADDING EXTRA ROWS FOR YEARS WHERE MULTIPLE CHILDREN DIED 
    dplyr::select(caseid,year,b5,b7) %>%
    mutate(year_ofdeath = floor(b7/12 + year),kid_died = 1) %>%
    dplyr::select(-b5,-b7,-year)
  
  names(deadkids)[2] <- "year"
  
  deadkids <- deadkids %>% 
    group_by(caseid,year) %>%
    summarise(kid_died = sum(kid_died))
  
  panelize_merged <- left_join(panelize_merged,deadkids)
  
  ### Indicates that a kid dies in this year (PROBLEM, multiple children could die in the same year so I need to rethink this)
  panelize_merged <- panelize_merged %>%
    mutate(kid_died = ifelse(is.na(kid_died),0,kid_died))
  
  panelize_merged %<>%
    group_by(caseid) %>%
    mutate(total_deadkids = cumsum(kid_died))
  
  panelize_merged <- panelize_merged %>%
    mutate(living_kids = current_numkids - total_deadkids)
  
  panelize_merged %<>% 
    group_by(caseid) %>%
    mutate(livingkids_atsurvey = last(living_kids))
  panelize_merged$b4 <- tolower(panelize_merged$b4)
  return(panelize_merged)
}

### Function to define for when an object is not in another object for use later

`%!in%` = Negate(`%in%`)

### Read in all of the files (38 total)
BR_data <- list.files("C:/Users/Dbrimmer/Desktop/phdyear2/Quarter3/Development/Fertility2/fertdata/BR_data", full.names = T)
IR_data <- list.files("C:/Users/Dbrimmer/Desktop/phdyear2/Quarter3/Development/Fertility2/fertdata/IR_data", full.names = T)

### This will push together multiple files after formatting by using the clean data function from above 
allcountries_data <- mapply(clean_data,BR_data,IR_data)

allcountries_merged <- rbindlist(allcountries_data,fill=T)
rm(allcountries_data)
### Now to create a column that will allow for matching with our GDP data
allcountries_merged <- allcountries_merged %>%
  mutate(CountryName = gsub("[0-9*]","",allcountries_merged$v000))

###3 countries don't match with the generic countrycodes so I will do them manually
allcountries_merged <- allcountries_merged %>%
  mutate(CountryName = ifelse(CountryName=="NM","NA",CountryName))
allcountries_merged <- allcountries_merged %>%
  mutate(CountryName = ifelse(CountryName=="BU","BI",CountryName))
allcountries_merged <- allcountries_merged %>%
  mutate(CountryName = ifelse(CountryName=="NI","NE",CountryName))

allcountries_merged$CountryName <- allcountries_merged$CountryName %>%
  countrycode("genc2c","genc.name")

### Some of our observations had no info, delete them
allcountries_merged <- allcountries_merged[!is.na(allcountries_merged$v000),]

### Convert variables into factors
allcountries_merged$v130 = as.factor(allcountries_merged$v130)
allcountries_merged$v102 = as.factor(allcountries_merged$v102)

 

### Drop those who don't report schooling (NA, not zero education)
allcountries_merged <- allcountries_merged[allcountries_merged$v106 %!in% c("NA","9"),]

### Now to fix schooling (some have numbers, some say no or primary or secondary etc.)
allcountries_merged$v106 <- tolower(allcountries_merged$v106)

allcountries_merged <- allcountries_merged %>%
  mutate(school= if_else(allcountries_merged$v106 == "no education", "0", as.character(v106))) %>%
  mutate(school= if_else(allcountries_merged$v106 == "primary", "1", school)) %>%
  mutate(school= if_else(allcountries_merged$v106 == "secondary", "2", school)) %>%
  mutate(school= if_else(allcountries_merged$v106 == "higher", "3", school))

#Now we create age bins
allcountries_merged$agebin <- factor(findInterval(allcountries_merged$age, c(15, 20, 25, 30, 35, 40, 45)))

### Read in gdp file, manipulate it to fit with our data
gdp <- read_excel("C:/Users/Dbrimmer/Desktop/phdyear2/Quarter3/Development/Fertility2/fertdata/Data_Extract_From_World_Development_Indicators (2).xlsx")
gdp <- gdp[-c(2,4)]

gdp <- gather(gdp,key = year, value = indicator, `1960 [YR1960]`:`2017 [YR2017]`) %>%
  spread(gdp,key = `Series Name`, value = `indicator`)

gdp <- unnest(gdp)
gdp$year <- gdp$year %>%
  gsub("\\s(.*)","",.)
allcountries_merged$year <- as.integer(allcountries_merged$year)

gdp$`Country Name` <- toupper(gdp$`Country Name`)

#Now we need to change a name to match
colnames(gdp)[colnames(gdp)=="Country Name"] <- "CountryName"
gdp[gdp$CountryName=="COTE D'IVOIRE",1] <- "CÔTE D’IVOIRE"

gdp$year <- as.integer(gdp$year)

names(gdp) <- gsub("\\s\\(.*\\)","",names(gdp))

gdp$`GDP growth` <- as.numeric(gdp$`GDP growth`)

gdp <- dplyr::select(gdp,c(CountryName,year,`GDP growth`))


### This function will create new dataframe with all of the years moved forward 1 to 6 years. This is equivalent to lagging 
### when we left join the dataframes
lagger <- function(time) {
  gdplag <- gdp[1:3]
  gdplag$year <- as.numeric(gdplag$year) +time
  lagname <- paste0(names(gdplag)[3],"_lag",time)
  names(gdplag)[3] <- lagname
  return(gdplag)
}

lags <- c(1:6)

gdplags <- lapply(lags,lagger)

gdp <- left_join(gdp,gdplags[[1]],by = c("CountryName", "year")) %>%
  left_join(gdplags[[2]],by = c("CountryName", "year")) %>%
  left_join(gdplags[[3]],by = c("CountryName", "year")) %>%
  left_join(gdplags[[4]],by = c("CountryName", "year")) %>%
  left_join(gdplags[[5]],by = c("CountryName", "year")) %>%
  left_join(gdplags[[6]],by = c("CountryName", "year"))


#### To look at consecutiveness of recessions
gdp <- gdp %>%
  mutate(rec = ifelse(`GDP growth` <0 , 1 , 0 ), rec_l1 = ifelse(`GDP growth_lag1` <0 , 1 , 0 ), recl2 = ifelse(`GDP growth_lag2` <0 , 1 , 0 ),
         recl3 = ifelse(`GDP growth_lag3` <0 , 1 , 0 ), recl4 = ifelse(`GDP growth_lag4` <0 , 1 , 0 ),recl5 = ifelse(`GDP growth_lag5` <0 , 1 , 0 ),
         recl6 = ifelse(`GDP growth_lag6` <0 , 1 , 0 ))

### Tells us the consecutive number of recessions that have occured in a given observation
gdp$countrec <- sequence(rle(as.character(gdp$rec))$lengths)

gdp <- gdp %>%
  mutate(countrec = ifelse(rec == 0 , 0 , countrec ))


### Merge gdp information with the main DHS dataset
gdp$year <- as.integer(gdp$year)
acgdp_merged <- left_join(allcountries_merged,gdp)
rm(allcountries_merged)
acgdp_merged$CountryName <- factor(acgdp_merged$CountryName)

##We should drop all observations where there is no gdp growth data
acgdp_merged <- acgdp_merged[!is.na(acgdp_merged$rec_l1),]


acgdp_merged$year <- as.integer(acgdp_merged$year)

### Tells us how many years an obervation is from the year of the interview so that we can censor
acgdp_merged <- mutate(acgdp_merged, yearssince = as.numeric(as.character(acgdp_merged$v007.x)) - as.numeric(as.character(acgdp_merged$year)))

acgdp_merged$v508 <- as.integer(acgdp_merged$v508)

### Create variable that tell us if a woman has ever been married by the year of an observation, and if she has ever had a 
### child by the year of an observation (0 in the year of a birth, as before the birth they had no children)
acgdp_merged <- acgdp_merged %>%
  mutate(motheryet = ifelse(current_numkids > 0,1,0)) %>%
  mutate(motheryet = ifelse(bord ==1 & !is.na(bord),0,motheryet)) %>%
  mutate(evermarried = ifelse(year >= v508,1,0)) 

acgdp_merged$evermarried <- replace(acgdp_merged$evermarried,is.na(acgdp_merged$evermarried),0)


### Look at only those observations that were in the last ten years (we decided to exclude the year of the interview)
recent <- acgdp_merged[acgdp_merged$yearssince <=10,]
recent <- recent[recent$yearssince >0,]
rm(acgdp_merged)

recent$year <- as.factor(recent$year)

acgdp_merged$year <- factor(acgdp_merged$year)


### We will run regressions on only countries that saw a recession now, line is likely redundant, as we don't read in 
### data for excluded countries
recessionladen <- recent[recent$CountryName %in% c("BURUNDI","CAMEROON","CHAD","CÔTE D’IVOIRE","ETHIOPIA","GUINEA","NIGER","TOGO","ZAMBIA","ZIMBABWE"),]

### Don't all women to desire more than 20 boys or girls
recessionladen$v627 <- replace(recessionladen$v627, is.na(recessionladen$v627),0)
recessionladen$v628 <- replace(recessionladen$v628, is.na(recessionladen$v628),0)
recessionladen$v629 <- replace(recessionladen$v629, is.na(recessionladen$v629),0)


recessionladen$v627 <- replace(recessionladen$v627, recessionladen$v627 >20,NA)
recessionladen$v628 <- replace(recessionladen$v628, recessionladen$v628 >20,NA)
recessionladen$v629 <- replace(recessionladen$v629, recessionladen$v629 >20,NA)

### lets us look at the gender preference by country
ratios <- recessionladen %>%
       group_by(CountryName) %>%
   summarize(mean_boys = sum(v627,na.rm=T),mean_girls = sum(v628,na.rm=T)) 

ratios$ratio <- ratios$mean_boys/ratios$mean_girls

### To calculate number of women who state a son preference and for calibration stuff (at the individual level)
recessionladen %<>%
  group_by(caseid) %>%
  mutate(sonpref = ifelse(v627-v628 >0,1,0)) %>%
  mutate(daughtpref = ifelse(v627-v628 <0,1,0))

sonny <- recessionladen[!duplicated(recessionladen[,1]),]
sum(sonny$sonpref,na.rm=T)

sum(sonny$daughtpref,na.rm=T)

### More gender preference stuff
countryratio <- sonny %>%
  group_by(CountryName) %>%
  summarize(sum = sum(sonpref,na.rm=T),count = n())
countryratio$ratio <- countryratio$sum/countryratio$count


recessionladen$living_kids <- as.factor(recessionladen$living_kids)

### Indicator telling us if a a woman gets married in a given year, also creates observation column by caseid 
recessionladen <- recessionladen %>%
  group_by(caseid,v000) %>%
  mutate(wedding = ifelse(v508 == year,1,0),observation = row_number())  ### Observation may be flawed here

recessionladen$wedding <- replace(recessionladen$wedding,is.na(recessionladen$wedding),0)
  
recessionladen$v508 <- as.integer(recessionladen$v508)
recessionladen$year <- as.integer(as.character(recessionladen$year))

recessionladen$year <- as.factor(recessionladen$year)


### We now need to create a dataframe that has only the first 5 years of every child's life, 
### which is a fundamentally different structure. First shrink recessionladen to have one observation per birth

babies <- recessionladen[recessionladen$birth==1,]
babies$caseid <- gsub("\\s+","",babies$caseid)

babies$year <- as.integer(as.character(babies$year))
babies$v007.x <- as.integer(babies$v007.x)

### Now we want observations for the first five years of each child's life, limiting it if the kid died during this time
### if b7 (how long the child lived) is NA then I don't need to calculate, if b7 is an amount of months, I can calculate the number of years to make
babies <- babies %>%
  mutate(kidage_atsurvey = v007.x-year)
babies <- babies %>%
  mutate(reps = ifelse(kidage_atsurvey >5,5,kidage_atsurvey))
babies$freq <-  ifelse(is.na(babies$b7),babies$reps,ifelse(ceiling(babies$b7/12)>5,5,ceiling(babies$b7/12)))
babies$freq <-  replace(babies$freq, babies$freq==0,1) 

### replicate that mother's information across the proper number of rows
babies <- babies[rep(row.names(babies),babies$freq),]


babies %<>% 
  group_by(caseid,bord,v010.x,v007.x,v000,b2,b3) %>% ######### RED ALERT, grouping by CASEID is not perfect, as sometimes they use similar numbers in different surveys
  mutate(year = year + row_number() -1)

babies <- babies %>%
  mutate(died = ifelse(b2 + floor(b7/12) != year | is.na(b7) ,0,1))

babies <- babies %>%
  mutate(kidage = year- b2)


babies <- babies[babies$year < babies$v007.x,] ### This is only giving me 10,000 dead kids, but there should probably be more.

babies$b4 <- tolower(babies$b4)
babies$year <- as.factor(babies$year)
### ONLY USE THIS FOR DIAGNOSTICS
#babies$caseid <- gsub("\\s+","",babies$caseid)
#recessionladen$caseid <- gsub("\\s+","",recessionladen$caseid)
gc()
### Regressions!!!
breg_r <- lm(formula = birth ~ rec_l1 + CountryName + year, data=recessionladen)
clustered <- cluster.vcov(breg_r, recessionladen$v000,df_correction =T)
spec1 <- coeftest(breg_r,clustered)

h <- tidy(breg_r)
reg_nor <- lm(formula = birth ~ rec_l1 + CountryName + year + agebin + v102 + school, data=recessionladen)
#reg_nor <- lm(formula = birth ~ rec_lag + CountryName + year + agebin + v102 + school, data=recent)
clustered1 <- cluster.vcov(reg_nor, recessionladen$v000,df_correction =T)
baseline <- coeftest(reg_nor,clustered1)
base <- tidy(reg_nor)

n <- tidy(reg_nor)
reg_norm <- lm(formula = birth ~ rec + CountryName + year + agebin + v102 + school, data=recessionladen)
#reg_norm <- lm(formula = birth ~ rec + CountryName + year + agebin + v102 + school, data=recent)
m <- tidy(reg_norm)
clustered2 <- cluster.vcov(reg_norm, recessionladen$v000,df_correction =T)
spec2 <- coeftest(reg_norm,clustered2)

stargazer(spec1,baseline,spec2,title = "Results", align= T,omit = c("year","CountryName"),no.space=T)

### These are our three specifications that look at the effect of recessions decomposed by various factors (mother's schooling, mother's age, urban/rural)
reg_ager <- lm(formula = birth ~ agebin*rec_l1 + v102 + school + year + CountryName, data=recessionladen)
clustered3 <- cluster.vcov(reg_ager, recessionladen$v000,df_correction =T)
spec3 <- coeftest(reg_ager,clustered3)

stargazer(spec3,title = "Heterogenous Effects: Age", align= T,omit = c("year","CountryName"),no.space=T)

reg_urbanr <- lm(formula = birth ~ agebin + v102*rec_l1 + school + year + CountryName, data=recessionladen)
clustered4 <- cluster.vcov(reg_urbanr, recessionladen$v000,df_correction =T)
spec4 <- coeftest(reg_urbanr,clustered4)

stargazer(spec4,title = "Heterogenous Effects: Urban/Rural", align= T,omit = c("year","CountryName"),no.space=T)
rm(clustered2,clustered3,clustered4,clustered5,reg_ager,reg_nor,reg_norm)
gc()
reg_schoolr <- lm(formula = birth ~ agebin + v102 + school*rec_l1 + year + CountryName, data=recessionladen)
clustered5 <- cluster.vcov(reg_schoolr, recessionladen$v000,df_correction =T)
spec5 <- coeftest(reg_schoolr,clustered5)

stargazer(spec5,title = "Heterogenous Effects: Schooling", align= T,omit = c("year","CountryName"),no.space=T)

### These are looking at the difference in effect based on whether the woman has ever been married, and whether she has had a child before
reg_married <- lm(formula = birth ~  rec_l1*evermarried + CountryName +  year + agebin + v102 + school, data=recessionladen)
clustermarried <- cluster.vcov(reg_married, recessionladen$v000,df_correction =T)
baseline_married <- coeftest(reg_married,clustermarried)

reg_kids <- lm(formula = birth ~  rec_l1*motheryet + CountryName +  year + agebin + v102 + school, data=recessionladen)
clusterkids <- cluster.vcov(reg_kids, recessionladen$v000,df_correction =T)
baseline_kids <- coeftest(reg_kids,clusterkids)

stargazer(baseline_married,baseline_kids,title = "Heterogenous Effects: Married and Motherss", align= T,omit = c("year","CountryName"),no.space=T)

### Now to lo0k at sex selection through mortality, birth spacing, and sex ratios by age

births <- recessionladen[recessionladen$birth==1,]
births <- births %>%
  mutate(siblings = livingkids_atsurvey -1)

sexreg <- function(Country) {
  datta <- births %>%
    filter(CountryName == Country)
  if (length(levels(factor(datta$v000))) >1) {
    sexreg <- lm(formula = siblings ~ b4, data = datta)
    sexy <- cluster.vcov(sexreg, datta$v000,df_correction =T)
    sext <- coeftest(sexreg,sexy) 
  } else {
    sexreg <- lm(formula = siblings ~ b4, data = datta)
    sext <- coeftest(sexreg, vcov = vcovHC(sexreg, "HC3"))  
  }
  return(sext)
}

Countrylist <- levels(factor(babies$CountryName))
sexum <- lapply(Countrylist,sexreg)

stargazer(sexum[1],sexum[2],sexum[3],sexum[4],sexum[5],sexum[6],sexum[7],sexum[8],sexum[9],sexum[10])
gc()
spacingreg <- function(Country) {
  datta <- births %>%
    filter(CountryName == Country)
  if (length(levels(factor(datta$v000))) >1) {
    sexreg <- lm(formula = b11 ~ b4, data = datta)
    sexy <- cluster.vcov(sexreg, datta$v000,df_correction =T)
    spaced <- coeftest(sexreg,sexy) 
  } else {
    sexreg <- lm(formula = b11 ~ b4, data = datta)
    spaced <- coeftest(sexreg, vcov = vcovHC(sexreg, "HC3"))  
  }
  return(spaced)
}

spaceman <- lapply(Countrylist,spacingreg)

stargazer(spaceman[1],spaceman[2],spaceman[3],spaceman[4],spaceman[5],spaceman[6],spaceman[7],spaceman[8],spaceman[9],spaceman[10])


mortreg <- function(Country) {
  data <- babies %>%
    filter(CountryName == Country)
  if (length(levels(factor(data$v000))) >1) {
    mortality <- lm(formula = died ~ kidage + rec_l1*b4 + agebin +  school + v102 + year, data = data)
    morty <- cluster.vcov(mortality, data$v000,df_correction =T)
    more <- coeftest(mortality,morty)  
  } else {
    mortality <- lm(formula = died ~ kidage + rec_l1*b4 + agebin +  school + v102 + year, data = data)
    more <- coeftest(mortality, vcov = vcovHC(mortality, "HC3"))  
  }
  return(more)
}

statedpref <- recessionladen[order(recessionladen$caseid),]
statedpref <- statedpref[!duplicated(statedpref$caseid),]
statedpref <- statedpref[!is.na(statedpref$v627) & !is.na(statedpref$v628) & !is.na(statedpref$v629),]

statedpref <- statedpref %>%
  group_by(CountryName) %>%
  summarize(sons = sum(v627), daughters = sum(v628), ambig = sum(v629))
  
statedpref <- statedpref %>%
  mutate(dsrb = (sons + .5* ambig)/(daughters + .5*ambig))

#write.csv(statedpref, "statedpreferences.csv")
morts <- mclapply(Countrylist,mortreg)

#lapply(morts, function(x) write.table( data.frame(x), 'test.csv'  , append= T, sep=',' ))

babies$b4 <- as.integer(as.factor(babies$b4)) -1
mm <- babies %>%
  group_by(CountryName,kidage) %>%
  summarize(boys = sum(b4),count = length(b4),proportion = sum(b4)/length(b4))
write.csv(mm,"sex_ratiobyage.csv")

### Regressions for extensions

reg_marriage <- lm(formula = wedding ~ agebin + school + v102 + year + CountryName + rec + rec_l1 + recl2 , data=notyetmarried)
clusteredm <- cluster.vcov(reg_marriage, notyetmarried$v000,df_correction =T)
specm3 <- coeftest(reg_marriage,clusteredm)


reg_marriage1 <- lm(formula = wedding ~ agebin + school + v102 + year + CountryName + rec , data=notyetmarried)
clusteredm1 <- cluster.vcov(reg_marriage1, notyetmarried$v000,df_correction =T)
specm1 <- coeftest(reg_marriage1,clusteredm1)

reg_marriage2 <- lm(formula = wedding ~ agebin + school + v102 + year + CountryName + rec + rec_l1, data=notyetmarried)
clusteredm2 <- cluster.vcov(reg_marriage2, notyetmarried$v000,df_correction =T)
specm2 <- coeftest(reg_marriage2,clusteredm2)

stargazer(specm1,specm2,specm3,title = "Marriage", align= T,omit = c("year","CountryName"),no.space=T)

proc.time() - ptm
