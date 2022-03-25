#----------------------------------- ALY6015 FinalProject Initial Analysis -------------------------------#

# Declaring the names of packages to be imported
packageList <- c("tidyverse", "vtable", "RColorBrewer", "corrplot", "car", "psych", "stargazer", "scales", "glmnet",
                 "Metrics", "caret", "leaps", "MASS", "flextable")

for (package in packageList) {
  if (!package %in% rownames(installed.packages())) 
  { install.packages(package) }
  
  # Import the package
  library(package, character.only = TRUE)
}

# Import/Load the data set
location_harshit <- "~/Documents/Northeastern University/MPS Analytics/ALY 6015/Final Project/Presidential_Election_2016_Classification/"
location_akash <- "C:/Users/rajak/OneDrive/Desktop/Study/University/ALY6015 - Intermediate Analytics/Final Project/Presidential_Election_2016_Classification/"

location <- ifelse(!dir.exists(paths = location_harshit), location_akash, location_harshit)
ElectionData <- read.csv(paste0(location, "Data/2016Election.csv"))

# Get a Glimpse/View of the data set
glimpse(ElectionData)

# Function to save table as 3 line format
threeLineTable <- function(df, title, footer, file_name){
  ft <- flextable(df)
  ft <- theme_booktabs(ft)
  ft <- add_footer_lines(ft, footer)
  ft <- flextable::color(ft, part = "footer", color = "#666666")
  ft <- set_caption(ft, caption = title)
  save_as_docx(ft, path =  paste0(location, "/Tables/", file_name, ".docx")) 
}

threeLineTable(ElectionData[1:5, 4:11], "Table 1: Snapshot of Data", "Snapshot of Election Data Sample", "Data Snapshot")

# ------------------------------------- Merging External Data sets ----------------------------------- #
# Reading new external data set
population <- read.csv(paste0(location, "Data/USPopulationByCounty.csv"))
election2020 <- read.csv(paste0(location, "Data/2020ElectionResults.csv"))
region <- read.csv(paste0(location, "Data/Regions.csv"))

threeLineTable(population[1:5, 2:8], "Table 2: Snapshot of external population data", "Snapshot of population data sample", "Population Snapshot")
threeLineTable(election2020[1:5, ], "Table 3: Snapshot of external 2020 election results data", "Snapshot of 2020 election results data", "Election 2020 Snapshot")
threeLineTable(region[1:5, ], "Table 4: Sample of additional region variable created", "", "Region Snapshot")

# Combining the external data sets with the original election data sets
ElectionData <- ElectionData %>% inner_join(population, by = c("county", "state"))

election2020$county_fips <- as.factor(election2020$county_fips)
ElectionData$c_fips <- as.factor(ElectionData$c_fips)
ElectionData <- ElectionData %>% inner_join(election2020, by = c("c_fips" = "county_fips"))

threeLineTable(ElectionData[1:5, 157:165], "Table 2: New variables added to the master data", "", "Appended data")

ElectionData <- ElectionData %>% inner_join(region, by = c("state" = "state.code"))


################################################################
# Checking the records with missing/NA values
################################################################
# Since, libertarian feature has almost all the data points as NA. We're removing it for now to check other NA cases
ElectionData <- ElectionData %>% dplyr::select(-libertarian)
missingRecords <- ElectionData %>% filter(!complete.cases(ElectionData))

threeLineTable(missingRecords[, 4:11], "Table 5: Records with missing data", "", "Missing Data")

# Retrieving the names of features with missing values.
missingValuesCols <- names(which(colSums(is.na(ElectionData)) > 0))

# Imputing missing values with their respective features' mean value
for(i in 1:ncol(ElectionData)) {
  ifelse(is.numeric(ElectionData[,i]), 
    ElectionData[is.na(ElectionData[,i]), i] <- mean(ElectionData[,i], na.rm = TRUE),
    ifelse(is.character(ElectionData[,i]), "NULL", 0)
  )
}

# Factorize these 'Quality Assessment Texts' in the data set
ElectionData[sapply(ElectionData, is.character)] <- lapply(ElectionData[sapply(ElectionData, is.character)], as.factor)


###########################################################################################
#                               Creating Variables for 2020                               #
###########################################################################################
# Creating variables for 2020 Predictions
ElectionData$pd2020 <- round(ElectionData$democrats / (ElectionData$democrats + ElectionData$green + ElectionData$other + ElectionData$republican), 2)
ElectionData$pg2020 <- round(ElectionData$republican / (ElectionData$democrats + ElectionData$green + ElectionData$other + ElectionData$republican), 2)


###########################################################################################
#                                     Exploratory Data Analysis                           #
###########################################################################################

ElectionDataEDA <- ElectionData %>% 
  dplyr::select(v2016, pd2016, pg2016, ppd2016, v2012, pd2012, pg2012, ppd2012, v2008, pd2008, pg2008, ppd2012, population.2016, geograhic.region)


percDemocraticVotesStats <- ElectionDataEDA %>% 
  dplyr::select(pd2008, pd2012, pd2016) %>% 
  describe(quant = c(.25, .75), IQR = TRUE) %>% 
  mutate(year = c(2008, 2012, 2016)) %>% 
  relocate(year)

threeLineTable(percDemocraticVotesStats, "Table 6: Descriptive statistics of Democratic party %votes by year", "", "Democratic votes")

percRepublicanVotesStats <- ElectionDataEDA %>% 
  dplyr::select(pg2008, pg2012, pg2016) %>% 
  describe(quant = c(.25, .75), IQR = TRUE) %>% 
  mutate(year = c(2008, 2012, 2016)) %>% 
  relocate(year)

threeLineTable(percRepublicanVotesStats, "Table 7: Descriptive statistics of Republican party %votes by year", "", "Republican votes")

percRepublicanVotesStatsSouth <- ElectionDataEDA %>% 
  dplyr::select(geograhic.region, pg2016) %>% filter(geograhic.region == "South") %>% 
  dplyr::select(-geograhic.region) %>% describe(quant = c(.25, .75), IQR = TRUE) %>% mutate(vars = "South")

percRepublicanVotesStatsWest <- ElectionDataEDA %>% 
  dplyr::select(geograhic.region, pg2016) %>% filter(geograhic.region == "West") %>% 
  dplyr::select(-geograhic.region) %>% describe(quant = c(.25, .75), IQR = TRUE) %>% mutate(vars = "West")

percRepublicanVotesStatsNE <- ElectionDataEDA %>% 
  dplyr::select(geograhic.region, pg2016) %>% filter(geograhic.region == "Northeast") %>% 
  dplyr::select(-geograhic.region) %>% describe(quant = c(.25, .75), IQR = TRUE) %>% mutate(vars = "Northeast")

percRepublicanVotesStatsMid <- ElectionDataEDA %>% 
  dplyr::select(geograhic.region, pg2016) %>% filter(geograhic.region == "Midwest") %>% 
  dplyr::select(-geograhic.region) %>% describe(quant = c(.25, .75), IQR = TRUE) %>% mutate(vars = "Midwest")


percRepublicanVotesStatReg <- rbind(percRepublicanVotesStatsSouth, percRepublicanVotesStatsWest, percRepublicanVotesStatsNE,
                                    percRepublicanVotesStatsMid)

threeLineTable(percRepublicanVotesStatReg, "Table 7: Descriptive statistics of Republican party %votes by region", "", "Republican votes region")

# Plot of outcome variable 'Total Democratic Votes' by state and year
percDVotesL <- ElectionDataEDA %>% 
  dplyr::select(geograhic.region, pd2008, pd2012, pd2016) %>% group_by(geograhic.region) %>% summarise('2008' = mean(pd2008, na.rm = TRUE),
                                                                          '2012' = mean(pd2012, na.rm = TRUE),
                                                                          '2016' = mean(pd2016, na.rm = TRUE)) %>%
  gather(year, pdVotes, c('2008', '2012', '2016'))

ggplot(data = percDVotesL, mapping = aes(x = reorder(factor(geograhic.region), pdVotes, function(x) -1*sum(x)), y = pdVotes, fill = year)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Percentage Democrat Votes by Region & Year") + 
  scale_x_discrete(name ="Region") + 
  scale_y_continuous(name = "Percent Democrat Votes") +
  theme_bw()


# Plot of outcome variable 'Total Republican Votes' by state and year
percRVotesL <- ElectionDataEDA %>% 
  dplyr::select(geograhic.region, pd2008, pd2012, pd2016) %>% group_by(geograhic.region) %>% summarise('2008' = mean(pd2008, na.rm = TRUE),
                                                                                                       '2012' = mean(pd2012, na.rm = TRUE),
                                                                                                       '2016' = mean(pd2016, na.rm = TRUE)) %>%
  gather(year, pgVotes, c('2008', '2012', '2016'))

ggplot(data = percRVotesL, mapping = aes(x = reorder(factor(geograhic.region), pgVotes, function(x) -1*sum(x)), y = pgVotes, fill = year)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Percent Republican Votes by Region & Year") + 
  scale_x_discrete(name ="Region") + 
  scale_y_continuous(name = "Percent Republican Votes") +
  theme_bw()




###########################################################################################
#                                     Correlation                                         #
###########################################################################################
# numIntFeatures_ElectionData <- ElectionData[sapply(ElectionData, is.numeric)]
corrFeatures_ElectionData <- ElectionData %>% 
  dplyr::select(v2016, pd2016, pg2016, ppd2016, v2012, pd2012, pg2012, ppd2012, v2008, pd2008, pg2008, ppd2012, population.2016)

correlationTable <- round(cor(corrFeatures_ElectionData, use = "pairwise"), 5)

threeLineTable(as.data.frame.matrix(correlationTable), "Table 8: Correlation Table of Numerical Features", "", "Correlation Table")

corrplot(cor(corrFeatures_ElectionData, use = "pairwise"), tl.cex = 1.2, type = "upper",
         col = brewer.pal(n = ncol(corrFeatures_ElectionData), name = "RdYlBu"))



###########################################################################################
#                             LASSO Regularization                                        #
###########################################################################################

################################################################
# Split data into train and test data
################################################################
lassoFittingFeatures <-  ElectionData[sapply(ElectionData, is.numeric)]

set.seed(454)
trainIndex <- createDataPartition(lassoFittingFeatures$pd2016, p = 0.80, list = FALSE)
train <- lassoFittingFeatures[trainIndex,]
test <- lassoFittingFeatures[-trainIndex,]

train_x <- model.matrix(pd2016 ~ ., train)[, -1]
test_x <- model.matrix(pd2016 ~ ., test)[, -1]

train_y <- train$pd2016
test_y <- test$pd2016


################################################################
# Find best value of Lambda using Cross-Validation
################################################################
set.seed(454)
cv.lasso <- cv.glmnet(train_x, train_y)
plot(cv.lasso)

################################################################
# Optimal Value of Lambda; Minimizes the Prediction Error
# Lambda Min - Minimizes out of sample loss
# Lambda 1SE - Largest value of Lambda within 1 Standard Error of Lambda Min.
################################################################
log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)


# Fit the model on training set using lambda.min 
model.lasso.min <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso$lambda.min)

# Display Regression Coefficients
coef(model.lasso.min)

threeLineTable(cbind(rownames(coef(model.lasso.min)), as.data.frame.matrix(round(coef(model.lasso.min), 4))), "Table 9: Lasso Regression on Training Data set using Lambda at 1 Standard Error", "", "Lasso_Regression_Table")

# Fit the model on training set using lambda.1se
model.lasso.1se <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso$lambda.1se)

# Display Regression Coefficients
coef(model.lasso.1se)


################################################################
# Make Prediction on the Training Data
################################################################
predict.lasso.train.1se <- predict(model.lasso.1se, newx = train_x)
lasso.train.rmse <- rmse(train_y, predict.lasso.train.1se)


################################################################
# Make Prediction on the Testing Data
################################################################
predict.lasso.test.1se <- predict(model.lasso.1se, newx = test_x)
lasso.test.rmse <- rmse(test_y, predict.lasso.test.1se)

lasso.train.rmse
lasso.test.rmse


################################################################
# Regression Fit - Model the data
################################################################
regressionFittingFeatures <- ElectionData[sapply(ElectionData, is.numeric)]

fit <- lm(formula = pd2016 ~ ., data = regressionFittingFeatures)
lm.summary <- summary(fit)

# Check 'Perfect Multi-Collinearity' because of "NA" values in summary of the fit model.
vif(fit)
alias(fit)

threeLineTable(cbind(rownames(lm.summary$coefficients), as.data.frame(round(lm.summary$coefficients, 4))), "Table 10: Linear Regression Model on Training Data set", "", "Linear_Regression_Table")

# RMSE of Linear Regression Model
rmse_stepwise_train <- sqrt(mean(lm.summary$residuals^2))

######### First Batch of Variables Removed #########
regressionFittingFeatures <-  regressionFittingFeatures %>% 
  dplyr::select(-unemp15, -unemp14, -unemp13, -unemp12, -unemp11, -nmig11)
fit <- lm(formula = pd2016 ~ ., data = regressionFittingFeatures)
summary(fit)

# Check 'Perfect Multi-Collinearity' because of "NA" values in summary of the fit model.
vif(fit)
alias(fit)


######### Second Batch of Variables Removed #########
regressionFittingFeatures <-  regressionFittingFeatures %>% 
  dplyr::select(-nmig12, -nmig13, -nmig14, -nmig15, -adkle025, -adkle026)
fit <- lm(formula = pd2016 ~ ., data = regressionFittingFeatures)
summary(fit)

# Check 'Perfect Multi-Collinearity' because of "NA" values in summary of the fit model.
vif(fit)
alias(fit)


######### Third Batch of Variables Removed #########
regressionFittingFeatures <-  regressionFittingFeatures %>% 
  dplyr::select(-adkle049, -adkxe008, -adkxe010, -year)
fit <- lm(formula = pd2016 ~ ., data = regressionFittingFeatures)
summary(fit)

# Check 'Perfect Multi-Collinearity' because of "NA" values in summary of the fit model.
vif(fit)
alias(fit)


######### Third Batch of Variables Removed #########
regressionFittingFeatures <-  regressionFittingFeatures %>% 
  dplyr::select(pd2016, v2016, vd2016, pg2016, ppd2016, pd2012, pg2012)
fit <- lm(formula = pd2016 ~ ., data = regressionFittingFeatures)
summary(fit)

# Check 'Perfect Multi-Collinearity' because of "NA" values in summary of the fit model.
vif(fit)
alias(fit)


################################################################
# Feature Selection
################################################################

# Stepwise Stepwise Selection
stepAIC(fit, direction = "both")

# Regression Model Fit using Features from Stepwise Stepwise Selection
fit <- lm(formula = pd2016 ~ v2016 + vd2016 + vg2016 + pg2016 + ppd2016 + 
            v2012 + vd2012 + vg2012 + pd2012 + pg2012 + diff2012 + vd2008 + 
            emp15 + lforce12 + unrate12 + lforce11 + pcpv15 + pp51715 + 
            plhs + phsd + pbdh + imig12 + imig15 + dmig14 + adkle001 + 
            adkle002 + adkle006 + adkle015 + adkle019 + adkle028 + adkle030 + 
            adkle033 + adkle036 + adkle039 + adkle041 + adkle042 + adkle043 + 
            adkle045 + adkxe002 + adkxe006 + adple030 + adple034 + adple039 + 
            adple055, data = regressionFittingFeatures)
summary(fit)


