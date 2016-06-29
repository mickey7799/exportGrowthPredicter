library(readr)
library(readxl)
library(tidyr)
library(dplyr)
#test
df <- read_excel("TW_EX.xlsx")[,c(1,7:21)]
names(df) <- c("country","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")
df$country <- as.factor(df$country)

#gdp_growth
gdp <- read_delim("weoreptc2.txt",delim ="\t",
                  col_types = 'c____ccccccccccccccc_______', n_max = 382)
for (i in 2:16){
gdp[[i]]<- as.numeric(gsub(",","",gdp[[i]]))
}

gdp1 <- gdp[seq(1,381,2),]
names(gdp1) <- c("country","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")


gdp2 <- gdp[seq(2,382,2),]
names(gdp2) <- c("country","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")


im <- read_excel("2001-2015 import value.xlsx",col_types = c("text","text","numeric"))


df2 <- gather(df,year,export, 2:16)
gdp1_1<- gather(gdp1,year,gdp, 2:16)
gdp2_1<- gather(gdp2,year,gdp_per, 2:16)
im2 <- im
#im2<- gather(im,year,import, 2:6)

f1 <- inner_join(df2,gdp1_1,by=c("country","year"))
f2 <- inner_join(gdp2_1, im2, by=c("country","year"))
f <- inner_join(f1,f2,by=c("country","year"))

f[which(f$export==0),]$export <- 1


f$export <- log(f$export)
f$gdp_per <- log(f$gdp_per)
f$import <- log(f$import)
#f$gdp <- log(f$gdp*1000000000)


f[[1]]<- as.factor(f[[1]])
f[[2]]<- as.factor(f[[2]])

train <- f[f$year=="2001"| f$year=="2002"|f$year=="2003"|f$year=="2004"| f$year=="2005"|f$year=="2006"|f$year=="2007"| f$year=="2008"|f$year=="2009"|f$year=="2010"|f$year=="2011"| f$year=="2012"|f$year=="2013",]
test <- f[f$year=="2014"|f$year=="2015",]
lm <- lm(export~import+gdp+gdp_per,data=train)

#separate county group
Advanced <- c('Australia', 'Austria', 'Belgium', 'Canada, Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hong Kong SAR', 'Iceland', 'Ireland', 'Israel', 'Italy', 'Japan', 'Korea', 'Latvia', 'Lithuania', 'Luxembourg', 'Macao SAR', 'Malta', 'Netherlands', 'New Zealand', 'Norway', 'Portugal', 'Puerto Rico', 'San Marino', 'Singapore', 'Slovak Republic', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Taiwan Province of China', 'United Kingdom', 'United States')
EU <-strsplit("Austria, Belgium, Bulgaria, Croatia, Cyprus, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Ireland, Italy, Latvia, Lithuania, Luxembourg, Malta, Netherlands, Poland, Portugal, Slovak Republic, Slovenia, Spain, Sweden, Romania, United Kingdom", ", ")[[1]]
Emerging <- strsplit("Afghanistan, Albania, Algeria, Angola, Antigua and Barbuda, Argentina, Armenia, Azerbaijan, The Bahamas, Bahrain, Bangladesh, Barbados, Belarus, Belize, Benin, Bhutan, Bolivia, Bosnia and Herzegovina, Botswana, Brazil, Brunei Darussalam, Bulgaria, Burkina Faso, Burundi, Cabo Verde, Cambodia, Cameroon, Central African Republic, Chad, Chile, China, Colombia, Comoros, Democratic Republic of the Congo, Republic of Congo, Costa Rica, Côte d'Ivoire, Croatia, Djibouti, Dominica, Dominican Republic, Ecuador, Egypt, El Salvador, Equatorial Guinea, Eritrea, Ethiopia, Fiji, Gabon, The Gambia, Georgia, Ghana, Grenada, Guatemala, Guinea, Guinea-Bissau, Guyana, Haiti, Honduras, Hungary, India, Indonesia, Iran, Iraq, Jamaica, Jordan, Kazakhstan, Kenya, Kiribati, Kosovo, Kuwait, Kyrgyz Republic, Lao P.D.R., Lebanon, Lesotho, Liberia, Libya, FYR Macedonia, Madagascar, Malawi, Malaysia, Maldives, Mali, Marshall Islands, Mauritania, Mauritius, Mexico, Micronesia, Moldova, Mongolia, Montenegro, Morocco, Mozambique, Myanmar, Namibia, Nepal, Nicaragua, Niger, Nigeria, Oman, Pakistan, Palau, Panama, Papua New Guinea, Paraguay, Peru, Philippines, Poland, Qatar, Romania, Russia, Rwanda, Samoa, São Tomé and Príncipe, Saudi Arabia, Senegal, Serbia, Seychelles, Sierra Leone, Solomon Islands, South Africa, South Sudan, Sri Lanka, St. Kitts and Nevis, St. Lucia, St. Vincent and the Grenadines, Sudan, Suriname, Swaziland, Syria, Tajikistan, Tanzania, Thailand, Timor-Leste, Togo, Tonga, Trinidad and Tobago, Tunisia, Turkey, Turkmenistan, Tuvalu, Uganda, Ukraine, United Arab Emirates, Uruguay, Uzbekistan, Vanuatu, Venezuela, Vietnam, Yemen, Zambia, Zimbabwe", ", ")[[1]]
EmergingAsia <- strsplit("Bangladesh, Bhutan, Brunei Darussalam, Cambodia, China, Fiji, India, Indonesia, Kiribati, Lao P.D.R., Malaysia, Maldives, Marshall Islands, Micronesia, Mongolia, Myanmar, Nepal, Palau, Papua New Guinea, Philippines, Samoa, Solomon Islands, Sri Lanka, Thailand, Timor-Leste, Tonga, Tuvalu, Vanuatu, Vietnam", ", ")[[1]]
LatinAmerica <- strsplit("Antigua and Barbuda, Argentina, The Bahamas, Barbados, Belize, Bolivia, Brazil, Chile, Colombia, Costa Rica, Dominica, Dominican Republic, Ecuador, El Salvador, Grenada, Guatemala, Guyana, Haiti, Honduras, Jamaica, Mexico, Nicaragua, Panama, Paraguay, Peru, St. Kitts and Nevis, St. Lucia, St. Vincent and the Grenadines, Suriname, Trinidad and Tobago, Uruguay, Venezuela", ", ")[[1]]
MiddleEast <- strsplit("Algeria, Bahrain, Djibouti, Egypt, Iran, Iraq, Jordan, Kuwait, Lebanon, Libya, Mauritania, Morocco, Oman, Qatar, Saudi Arabia, Sudan, Syria, Tunisia, United Arab Emirates, Yemen", ", ")[[1]]
SubSaharanAfrica <- strsplit("Bangladesh, Bhutan, Brunei Darussalam, Cambodia, China, Fiji, India, Indonesia, Kiribati, Lao P.D.R., Malaysia, Maldives, Marshall Islands, Micronesia, Mongolia, Myanmar, Nepal, Palau, Papua New Guinea, Philippines, Samoa, Solomon Islands, Sri Lanka, Thailand, Timor-Leste, Tonga, Tuvalu, Vanuatu, Vietnam", ", ")[[1]]

