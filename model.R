library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(ggvis)

# Read in and clean data
#1 export
export <- read_excel("TW_EX.xlsx")[ , c(1, 7:21)]
names(export) <- c("country", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                   "2009", "2010", "2011", "2012", "2013", "2014", "2015")
export$country <- as.factor(export$country)

#2 GDP_change/GDP_per_value
gdp <- read_delim("weoreptc2.txt", delim = "\t",
                  col_types = "c____ccccccccccccccc_______", n_max = 382)
for (i in 2:16) {
  gdp[[i]]<- as.numeric(gsub(",", "", gdp[[i]]))
}
gdp_nation <- gdp[seq(1, 381, 2), ]
names(gdp_nation) <- c("country", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                       "2009", "2010", "2011", "2012", "2013", "2014", "2015")
gdp_capita <- gdp[seq(2, 382, 2), ]
names(gdp_capita) <- c("country", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                       "2009", "2010", "2011", "2012", "2013", "2014", "2015")

#3 Import value
import <- read_excel("2001-2015 import value.xlsx", col_types = c("text", "text", "numeric"))

#4 Import vol change
import_vol_change <- read_delim("vol_import_goods.txt", delim = "\t",
                                col_types = "c____ccccccccccccccc_______",n_max = 191)
for (i in 2:16) {
  import_vol_change[[i]]<- as.numeric(import_vol_change[[i]])
}

names(import_vol_change) <- c("country", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                   "2009", "2010", "2011", "2012", "2013", "2014", "2015")

import_vol_change$country <- as.factor(import_vol_change$country)

#5 exchange rate_US/national currency

exchange_rate <- read.csv("Exchange rate_US_Dollar_per_National_Currency.csv", header=T, fileEncoding = "UCS-2LE")

# Gather year columns
export <- gather(export, year, export, 2:16)
gdp_nation <- gather(gdp_nation, year, gdp_nation, 2:16)
gdp_capita <- gather(gdp_capita, year, gdp_capita, 2:16)
import_vol_change <- gather(import_vol_change,year,import_vol_change, 2:16)

# Join all four tables
data_temp <- inner_join(export, gdp_nation, by = c("country", "year")) %>%
  inner_join(inner_join(gdp_capita, import, by = c("country", "year")), by = c("country", "year"))

data <- inner_join(data_temp, import_vol_change, by= c("country", "year"))

# Handle zero entries
data[which(data$export == 0), ]$export <- 1

# Remove rows containing NA's
data <- data[complete.cases(data), ]

# Take logarithm
data$export <- log(data$export)
data$gdp_capita <- log(data$gdp_capita)
data$import <- log(data$import)

data[[1]] <- as.factor(data[[1]])
data[[2]] <- as.factor(data[[2]])

# Method #1: Split by year
train_year <- data[grepl(paste(2001:2013, collapse = '|'), data$year), ]
test_year <- data[grepl(paste(2014:2015, collapse = '|'), data$year), ]
lm_year <- lm(export ~ gdp_nation + gdp_capita + import + import_vol_change, data = train_year)

rmse_year_train <- sqrt(sum((train_year$export - predict(lm_year)) ^ 2) / nrow(train_year))
rmse_year_test <- sqrt(sum((test_year$export - predict(lm_year, test_year)) ^ 2) / nrow(test_year))

plot(lm_year$fitted.values, lm_year$residuals)
qqnorm(lm_year$residuals)


# Method #2: Split randomly into 10-fold
set.seed(1)
n <- nrow(data)
shuffled_data <- data[sample(n), ][-(2261:2265), ]

rmse_nfold_train <- rep(NA, 10)
rmse_nfold_test <- rep(NA, 10)
r_squared_nfold <- rep(NA, 10)
pvalue_gdp_nation_nfold <- rep(NA, 10)
pvalue_gdp_capita_nfold <- rep(NA, 10)
pvalue_import_nfold <- rep(NA, 10)
pvalue_import_vol_change_nfold <- rep(NA, 10)

for (i in 1:10) {
  indices <- (((i - 1) * round(1/10 * n)) + 1):(i * round((1/10 * n)))
  train <- shuffled_data[-indices, ]
  test <- shuffled_data[indices, ]
  lm_nfold <- lm(export ~ gdp_nation + gdp_capita + import+ import_vol_change, data = train)
  
  rmse_nfold_train[i] <- sqrt(sum((train$export - predict(lm_nfold)) ^ 2) / nrow(train))
  rmse_nfold_test[i] <- sqrt(sum((test$export - predict(lm_nfold, test)) ^ 2) / nrow(test))
  r_squared_nfold[i] <- summary(lm_nfold)$r.squared
  pvalue_gdp_nation_nfold[i] <- summary(lm_nfold)$coefficients[2, 4]
  pvalue_gdp_capita_nfold[i] <- summary(lm_nfold)$coefficients[3, 4]
  pvalue_import_nfold[i] <- summary(lm_nfold)$coefficients[4, 4]
  pvalue_import_vol_change_nfold[i] <- summary(lm_nfold)$coefficients[5, 4]
}

rmse_nfold_train_cv <- mean(rmse_nfold_train)
rmse_nfold_test_cv <- mean(rmse_nfold_test)
r_squared_nfold_cv <- mean(r_squared_nfold)
pvalue_gdp_nation_nfold_cv <- mean(pvalue_gdp_nation_nfold)
pvalue_gdp_capita_nfold_cv <- mean(pvalue_gdp_capita_nfold)
pvalue_import_nfold_cv <- mean(pvalue_import_nfold)
pvalue_import_vol_change_nfold_cv <- mean(pvalue_import_vol_change_nfold)

# Produce summary
method <- c('year', '10-fold', subgroup_names)
n_train <- c(nrow(train_year),
             nrow(shuffled_data) * 0.9,
             rep(NA, 7))
n_test <- c(nrow(test_year),
            nrow(shuffled_data) * 0.1,
            rep(NA, 7))
rmse_train <- c(rmse_year_train,
                rmse_nfold_train_cv,
                rep(NA, 7))
rmse_test <- c(rmse_year_test,
               rmse_nfold_test_cv,
               rep(NA, 7))
r_squared <- c(summary(lm_year)$r.squared,
               r_squared_nfold_cv,
               rep(NA, 7))
pvalue_gdp_nation <- c(summary(lm_year)$coefficients[2, 4],
                       pvalue_gdp_nation_nfold_cv,
                       rep(NA, 7))
pvalue_gdp_capita <- c(summary(lm_year)$coefficients[3, 4],
                       pvalue_gdp_capita_nfold_cv,
                       rep(NA, 7))
pvalue_import <- c(summary(lm_year)$coefficients[4, 4],
                   pvalue_import_nfold_cv,
                   rep(NA, 7))
pvalue_import_vol_change <- c(summary(lm_year)$coefficients[5, 4],
                   pvalue_import_vol_change_nfold_cv,
                   rep(NA, 7))
lm_summary <- data.frame(method, n_train, n_test, rmse_train, rmse_test, r_squared, pvalue_gdp_nation, pvalue_gdp_capita, pvalue_import, pvalue_import_vol_change)

# Method #3: Split by subgroups of countries
advanced <- strsplit("Australia, Austria, Belgium, Canada, Cyprus, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece, Hong Kong SAR, Iceland, Ireland, Israel, Italy, Japan, Korea, Latvia, Lithuania, Luxembourg, Macao SAR, Malta, Netherlands, New Zealand, Norway, Portugal, Puerto Rico, San Marino, Singapore, Slovak Republic, Slovenia, Spain, Sweden, Switzerland, Taiwan Province of China, United Kingdom, United States", ", ")[[1]]
eu <- strsplit("Austria, Belgium, Bulgaria, Croatia, Cyprus, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Ireland, Italy, Latvia, Lithuania, Luxembourg, Malta, Netherlands, Poland, Portugal, Slovak Republic, Slovenia, Spain, Sweden, Romania, United Kingdom", ", ")[[1]]
emerging <- strsplit("Afghanistan, Albania, Algeria, Angola, Antigua and Barbuda, Argentina, Armenia, Azerbaijan, The Bahamas, Bahrain, Bangladesh, Barbados, Belarus, Belize, Benin, Bhutan, Bolivia, Bosnia and Herzegovina, Botswana, Brazil, Brunei Darussalam, Bulgaria, Burkina Faso, Burundi, Cabo Verde, Cambodia, Cameroon, Central African Republic, Chad, Chile, China, Colombia, Comoros, Democratic Republic of the Congo, Republic of Congo, Costa Rica, C繫te d'Ivoire, Croatia, Djibouti, Dominica, Dominican Republic, Ecuador, Egypt, El Salvador, Equatorial Guinea, Eritrea, Ethiopia, Fiji, Gabon, The Gambia, Georgia, Ghana, Grenada, Guatemala, Guinea, Guinea-Bissau, Guyana, Haiti, Honduras, Hungary, India, Indonesia, Iran, Iraq, Jamaica, Jordan, Kazakhstan, Kenya, Kiribati, Kosovo, Kuwait, Kyrgyz Republic, Lao P.D.R., Lebanon, Lesotho, Liberia, Libya, FYR Macedonia, Madagascar, Malawi, Malaysia, Maldives, Mali, Marshall Islands, Mauritania, Mauritius, Mexico, Micronesia, Moldova, Mongolia, Montenegro, Morocco, Mozambique, Myanmar, Namibia, Nepal, Nicaragua, Niger, Nigeria, Oman, Pakistan, Palau, Panama, Papua New Guinea, Paraguay, Peru, Philippines, Poland, Qatar, Romania, Russia, Rwanda, Samoa, S瓊o Tom矇 and Pr穩ncipe, Saudi Arabia, Senegal, Serbia, Seychelles, Sierra Leone, Solomon Islands, South Africa, South Sudan, Sri Lanka, St. Kitts and Nevis, St. Lucia, St. Vincent and the Grenadines, Sudan, Suriname, Swaziland, Syria, Tajikistan, Tanzania, Thailand, Timor-Leste, Togo, Tonga, Trinidad and Tobago, Tunisia, Turkey, Turkmenistan, Tuvalu, Uganda, Ukraine, United Arab Emirates, Uruguay, Uzbekistan, Vanuatu, Venezuela, Vietnam, Yemen, Zambia, Zimbabwe", ", ")[[1]]
emerging_asia <- strsplit("Bangladesh, Bhutan, Brunei Darussalam, Cambodia, China, Fiji, India, Indonesia, Kiribati, Lao P.D.R., Malaysia, Maldives, Marshall Islands, Micronesia, Mongolia, Myanmar, Nepal, Palau, Papua New Guinea, Philippines, Samoa, Solomon Islands, Sri Lanka, Thailand, Timor-Leste, Tonga, Tuvalu, Vanuatu, Vietnam", ", ")[[1]]
latin_america <- strsplit("Antigua and Barbuda, Argentina, The Bahamas, Barbados, Belize, Bolivia, Brazil, Chile, Colombia, Costa Rica, Dominica, Dominican Republic, Ecuador, El Salvador, Grenada, Guatemala, Guyana, Haiti, Honduras, Jamaica, Mexico, Nicaragua, Panama, Paraguay, Peru, St. Kitts and Nevis, St. Lucia, St. Vincent and the Grenadines, Suriname, Trinidad and Tobago, Uruguay, Venezuela", ", ")[[1]]
middle_east <- strsplit("Algeria, Bahrain, Djibouti, Egypt, Iran, Iraq, Jordan, Kuwait, Lebanon, Libya, Mauritania, Morocco, Oman, Qatar, Saudi Arabia, Sudan, Syria, Tunisia, United Arab Emirates, Yemen", ", ")[[1]]
sub_saharan_africa <- strsplit("Bangladesh, Bhutan, Brunei Darussalam, Cambodia, China, Fiji, India, Indonesia, Kiribati, Lao P.D.R., Malaysia, Maldives, Marshall Islands, Micronesia, Mongolia, Myanmar, Nepal, Palau, Papua New Guinea, Philippines, Samoa, Solomon Islands, Sri Lanka, Thailand, Timor-Leste, Tonga, Tuvalu, Vanuatu, Vietnam", ", ")[[1]]

subgroups <- list(advanced, eu, emerging, emerging_asia, latin_america, middle_east, sub_saharan_africa)
subgroup_names <- c("advanced", "eu", "emerging", "emerging_asia", "latin_america", "middle_east", "sub_saharan_africa")
names(subgroups) <- subgroup_names

for (i in 1:length(subgroups)) {
  members <- data[data$country %in% subgroups[[i]], ]
  n <- nrow(members)
  shuffled <- members[sample(n), ]
  train <- shuffled[1:round(0.6 * n), ]
  test <- shuffled[(round(0.6 * n) + 1):n, ]
  lm_subgroup <- lm(export ~ gdp_nation + gdp_capita + import + import_vol_change, data = train)
  
  lm_summary[lm_summary$method == names(subgroups)[i],
             -1] <- c(nrow(train),
                      nrow(test),
                      sqrt(sum((train$export - predict(lm_subgroup)) ^ 2) / nrow(train)),
                      sqrt(sum((test$export - predict(lm_subgroup, test)) ^ 2) / nrow(test)),
                      summary(lm_subgroup)$r.squared,
                      summary(lm_subgroup)$coefficients[2, 4],
                      summary(lm_subgroup)$coefficients[3, 4],
                      summary(lm_subgroup)$coefficients[4, 4],
                      summary(lm_subgroup)$coefficients[5, 4])
}


#graph
plot_r_squared <- {
  lm_summary %>%
    ggvis(~method, ~r_squared) %>%
    layer_points(size := 100, fill := 'blue', opacity := 0.5)
}

plot_rmse <- {
  lm_summary %>%
    mutate(rmse_ratio = rmse_test / rmse_train) %>%
    ggvis(~rmse_train, ~rmse_ratio) %>%
    layer_points(fill = ~method, size := 500) %>%
    add_axis('x', title = 'RMSE within Training Set') %>%
    add_axis('y', title = 'RMSE_test Divided by RMSE_training') %>%
    add_legend('fill', title = 'Splitting Method')
}

plot_pvalue <- {
  lm_summary %>%
    gather(pred, pvalue, 7:9) %>%
    ggvis(~method, ~pvalue, stroke = ~pred,
          strokeWidth := 10,
          strokeOpacity := 0.5) %>%
    layer_lines() %>%
    add_axis('x', title = 'Method') %>%
    add_axis('y', title = 'p-value') %>%
    add_legend('stroke', title = 'Predictors')
}
