library(readr)
library(readxl)
library(tidyr)
library(dplyr)

# Read in and clean data
export <- read_excel("TW_EX.xlsx")[ , c(1, 7:21)]
names(export) <- c("country", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                   "2009", "2010", "2011", "2012", "2013", "2014", "2015")
export$country <- as.factor(export$country)

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

import <- read_excel("2001-2015 import value.xlsx", col_types = c("text", "text", "numeric"))

# Gather year columns
export <- gather(export, year, export, 2:16)
gdp_nation <- gather(gdp_nation, year, gdp_nation, 2:16)
gdp_capita <- gather(gdp_capita, year, gdp_capita, 2:16)

# Join all four tables
data <- inner_join(export, gdp_nation, by = c("country", "year")) %>%
  inner_join(inner_join(gdp_capita, import, by = c("country", "year")), by = c("country", "year"))

# Handle zero entries
data[which(data$export == 0), ]$export <- 1

# Take logarithm
data$export <- log(data$export)
data$gdp_capita <- log(data$gdp_capita)
data$import <- log(data$import)

data[[1]] <- as.factor(data[[1]])
data[[2]] <- as.factor(data[[2]])

# Method #1: Split by year
train <- data[grepl(paste(2001:2013, collapse = '|'), data$year), ]
test <- data[grepl(paste(2014:2015, collapse = '|'), data$year), ]
lm_year <- lm(export ~ gdp_nation + gdp_capita + import, data = train)

# Method #2: Split randomly into 10-fold

# Method #3: Split by country groups

#separate county group
Advanced <- c('Australia', 'Austria', 'Belgium', 'Canada, Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hong Kong SAR', 'Iceland', 'Ireland', 'Israel', 'Italy', 'Japan', 'Korea', 'Latvia', 'Lithuania', 'Luxembourg', 'Macao SAR', 'Malta', 'Netherlands', 'New Zealand', 'Norway', 'Portugal', 'Puerto Rico', 'San Marino', 'Singapore', 'Slovak Republic', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Taiwan Province of China', 'United Kingdom', 'United States')
EU <-strsplit("Austria, Belgium, Bulgaria, Croatia, Cyprus, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Ireland, Italy, Latvia, Lithuania, Luxembourg, Malta, Netherlands, Poland, Portugal, Slovak Republic, Slovenia, Spain, Sweden, Romania, United Kingdom", ", ")[[1]]
Emerging <- strsplit("Afghanistan, Albania, Algeria, Angola, Antigua and Barbuda, Argentina, Armenia, Azerbaijan, The Bahamas, Bahrain, Bangladesh, Barbados, Belarus, Belize, Benin, Bhutan, Bolivia, Bosnia and Herzegovina, Botswana, Brazil, Brunei Darussalam, Bulgaria, Burkina Faso, Burundi, Cabo Verde, Cambodia, Cameroon, Central African Republic, Chad, Chile, China, Colombia, Comoros, Democratic Republic of the Congo, Republic of Congo, Costa Rica, Côte d'Ivoire, Croatia, Djibouti, Dominica, Dominican Republic, Ecuador, Egypt, El Salvador, Equatorial Guinea, Eritrea, Ethiopia, Fiji, Gabon, The Gambia, Georgia, Ghana, Grenada, Guatemala, Guinea, Guinea-Bissau, Guyana, Haiti, Honduras, Hungary, India, Indonesia, Iran, Iraq, Jamaica, Jordan, Kazakhstan, Kenya, Kiribati, Kosovo, Kuwait, Kyrgyz Republic, Lao P.D.R., Lebanon, Lesotho, Liberia, Libya, FYR Macedonia, Madagascar, Malawi, Malaysia, Maldives, Mali, Marshall Islands, Mauritania, Mauritius, Mexico, Micronesia, Moldova, Mongolia, Montenegro, Morocco, Mozambique, Myanmar, Namibia, Nepal, Nicaragua, Niger, Nigeria, Oman, Pakistan, Palau, Panama, Papua New Guinea, Paraguay, Peru, Philippines, Poland, Qatar, Romania, Russia, Rwanda, Samoa, São Tomé and Príncipe, Saudi Arabia, Senegal, Serbia, Seychelles, Sierra Leone, Solomon Islands, South Africa, South Sudan, Sri Lanka, St. Kitts and Nevis, St. Lucia, St. Vincent and the Grenadines, Sudan, Suriname, Swaziland, Syria, Tajikistan, Tanzania, Thailand, Timor-Leste, Togo, Tonga, Trinidad and Tobago, Tunisia, Turkey, Turkmenistan, Tuvalu, Uganda, Ukraine, United Arab Emirates, Uruguay, Uzbekistan, Vanuatu, Venezuela, Vietnam, Yemen, Zambia, Zimbabwe", ", ")[[1]]
EmergingAsia <- strsplit("Bangladesh, Bhutan, Brunei Darussalam, Cambodia, China, Fiji, India, Indonesia, Kiribati, Lao P.D.R., Malaysia, Maldives, Marshall Islands, Micronesia, Mongolia, Myanmar, Nepal, Palau, Papua New Guinea, Philippines, Samoa, Solomon Islands, Sri Lanka, Thailand, Timor-Leste, Tonga, Tuvalu, Vanuatu, Vietnam", ", ")[[1]]
LatinAmerica <- strsplit("Antigua and Barbuda, Argentina, The Bahamas, Barbados, Belize, Bolivia, Brazil, Chile, Colombia, Costa Rica, Dominica, Dominican Republic, Ecuador, El Salvador, Grenada, Guatemala, Guyana, Haiti, Honduras, Jamaica, Mexico, Nicaragua, Panama, Paraguay, Peru, St. Kitts and Nevis, St. Lucia, St. Vincent and the Grenadines, Suriname, Trinidad and Tobago, Uruguay, Venezuela", ", ")[[1]]
MiddleEast <- strsplit("Algeria, Bahrain, Djibouti, Egypt, Iran, Iraq, Jordan, Kuwait, Lebanon, Libya, Mauritania, Morocco, Oman, Qatar, Saudi Arabia, Sudan, Syria, Tunisia, United Arab Emirates, Yemen", ", ")[[1]]
SubSaharanAfrica <- strsplit("Bangladesh, Bhutan, Brunei Darussalam, Cambodia, China, Fiji, India, Indonesia, Kiribati, Lao P.D.R., Malaysia, Maldives, Marshall Islands, Micronesia, Mongolia, Myanmar, Nepal, Palau, Papua New Guinea, Philippines, Samoa, Solomon Islands, Sri Lanka, Thailand, Timor-Leste, Tonga, Tuvalu, Vanuatu, Vietnam", ", ")[[1]]
