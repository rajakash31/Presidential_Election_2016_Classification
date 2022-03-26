modelData<- mergedElectionData2012And2016 %>% dplyr::select(year, c_fips,	pd,	lforce,	emp,	unemp,	unrate,	imig,	dmig,	nmig,	population,	population.female,	population.male,	`American Indian or Alaska Native`,	`Asian or Pacific Islander`,	`Black or African American`,	White,	`< 1 year`,	`10-14 years`,	`15-19 years`,	`20-24 years`,	`25-29 years`,	`30-34 years`,	`35-39 years`,	`40-44 years`,	`45-49 years`,	`50-54 years`,	`55-59 years`,	`60-64 years`,	`65-69 years`,	`70-74 years`,	`75-79 years`,	`80-84 years`,	`85+ years`)

modelDataNew <- modelData %>% mutate(lforce_perc = lforce/population, emp_perc = emp/population, unemp_perc = unemp/population,
                                     unrate_perc = unrate/population, imig_perc = imig/population, dmig_perc = dmig/population,
                                     nmig_perc = nmig/population, popFemale_perc = population.female/population, popMale_perc = population.male/population,
                                     AmInd_perc = `American Indian or Alaska Native`/population, Asian_perc = `Asian or Pacific Islander`/population,
                                     AfAm_perc = `Black or African American`/population, White_perc = White/population, Age_0_14_perc = (`< 1 year` + `10-14 years`)/population,
                                     Age_15_24_perc = (`15-19 years` + `20-24 years`)/population, Age_25_34_perc = (`25-29 years` + `30-34 years`)/population,
                                     Age_35_44_perc = (`35-39 years` + `40-44 years`)/population, Age_45_54_perc = (`45-49 years` + `50-54 years`)/population,
                                     Age_55_64_perc = (`55-59 years` + `60-64 years`)/population, Age_65_74_perc = (`65-69 years` + `70-74 years`)/population,
                                     Age_74_84_perc = (`75-79 years` + `80-84 years`)/population, Age_85_perc = (`85+ years`)/population) %>%
  dplyr::select(year, c_fips, pd, lforce_perc, emp_perc, unemp_perc, unrate_perc, imig_perc, dmig_perc, nmig_perc, popFemale_perc, 
                popMale_perc, AmInd_perc, Asian_perc, AfAm_perc, White_perc, Age_0_14_perc, Age_15_24_perc, Age_25_34_perc, Age_35_44_perc, 
                Age_45_54_perc, Age_55_64_perc, Age_65_74_perc, Age_74_84_perc, Age_85_perc)

modelDataNew2 <- modelDataNew %>% dplyr::select(-unemp_perc, -popMale_perc, -White_perc, -lforce_perc, -nmig_perc, -Age_15_24_perc)

fun <- function(x){
  quantiles <- quantile( x, c(.01, .99 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

for (name in names(modelDataNew2)){
  if(!(name %in% c('year', 'pd', 'c_fips'))){
    modelDataNew2[, name] <- fun( modelDataNew2[, name] ) 
  }
}

crime2011 <- read_excel(paste0("C:/Users/rajak/OneDrive/Desktop/Study/University/ALY6015 - Intermediate Analytics/Final Project/Presidential_Election_2016_Classification/Data/Additional Data Crime.xlsx"), sheet = "CRIME 2011")
crime2011$year <- 2012
crime2015 <- read_excel(paste0("C:/Users/rajak/OneDrive/Desktop/Study/University/ALY6015 - Intermediate Analytics/Final Project/Presidential_Election_2016_Classification/Data/Additional Data Crime.xlsx"), sheet = "CRIME 2015")
crime2015$year <- 2016

crime <- rbind(crime2011, crime2015)

fun2 <- function(x){
  x <- (x - mean(x))/sd(x)
  x
}


for (name in names(crime)){
  if(!(name %in% c('year', 'FIPS_COUNTY'))){
    crime[, name] <- fun2( crime[, name][[1]] ) 
  }
}

modelDataNew2$c_fips <- as.factor(modelDataNew2$c_fips)
crime$FIPS_COUNTY <- as.factor(crime$FIPS_COUNTY)

modelDataNew2$year <- as.factor(modelDataNew2$year)
crime$year <- as.factor(crime$year)

modelDataNew3 <- modelDataNew2 %>% inner_join(crime, by = c("c_fips" = "FIPS_COUNTY", "year" = "year"))

ggplot(data = modelDataNew2) +
  geom_density(mapping = aes(x = emp_perc))

ggplot(data = modelDataNew2) +
  geom_boxplot(mapping = aes(x = emp_perc))

modelTrain <- modelDataNew3 %>% filter(year == 2012) %>% dplyr::select(-year, -c_fips)
modelTest <- modelDataNew3 %>% filter(year == 2016) %>% dplyr::select(-year, -pd, -c_fips)

lm1 <- lm(pd ~ ., data = modelTrain)
summary(lm1)

vif(lm1)
alias(lm1)

pred <- predict(lm1, newdata = modelTest)

