# Load the packages
library(plm)
library(lmtest)
library(sandwich)
library(imf.data)
library(dplyr)
library(tidyr)
library(wbstats)
library(ggplot2)
library(readxl)
library(multiwayvcov)
library(webshot2)
library(stargazer)
library(car)
library(clubSandwich)
library(moments)

################################ Equity Home Bias ################################

# Load CPIS dataset
CPIS <- load_datasets("CPIS")

# Create a mapping between country abbreviations and full names
country_codes <- CPIS$dimensions$ref_area[[1]]
country_names <- CPIS$dimensions$ref_area[[2]]
country_mapping <- setNames(country_names, country_codes)

# Create an empty results dataframe
FEH <- data.frame(Country = character(), Year = numeric(), FEH = numeric(), stringsAsFactors = FALSE)

# Function to fetch and add data
add_country_data <- function(country_code) {
  CPISdata <- CPIS$get_series(freq = "A", ref_area = country_code, 
                              indicator = "I_A_E_T_T_BP6_USD", ref_sector = "T", 
                              counterpart_sector = "T", counterpart_area = "W00")
  
  if (!is.null(CPISdata) && nrow(CPISdata) > 0) {
    new_data <- data.frame(Country = country_code, 
                           Year = CPISdata[[1]], 
                           FEH = CPISdata[[2]])
    FEH <<- rbind(FEH, new_data)
  }
}

# Loop through all countries and add their data
for (country in country_codes) {
  add_country_data(country)
  Sys.sleep(1)
}

# Replace country abbreviations with full names
FEH$Country <- country_mapping[FEH$Country]

# Load OECD data
oecd.liab <- read.csv("Thesis/Data/OECD.LIAB.csv") 
oecd.assets <- read.csv("Thesis/Data/OECD.ASSETS.csv") 

# Process OECD liability and asset data
oecd.liab <- oecd.liab %>%
  select(Reference.area, TIME_PERIOD, OBS_VALUE) %>%
  arrange(Reference.area) %>%
  rename(Country = Reference.area, Year = TIME_PERIOD, oecd.l = OBS_VALUE) %>%
  mutate(oecd.l = oecd.l * 10^6)

oecd.assets <- oecd.assets %>%
  select(Reference.area, TIME_PERIOD, OBS_VALUE) %>%
  arrange(Reference.area) %>%
  rename(Country = Reference.area, Year = TIME_PERIOD, oecd.a = OBS_VALUE) %>%
  mutate(oecd.a = oecd.a * 10^6)

# Compute Domestic Equity Holdings (DEH)
DEH <- oecd.liab %>%
  left_join(oecd.assets, by = c("Country", "Year")) %>%
  mutate(DEH = oecd.l - oecd.a) %>%
  select(Country, Year, DEH)

# Merge DEH and FEH to create TVEH
data <- merge(DEH, FEH, by = c("Country", "Year"), all = FALSE)

# Compute TVEH and Foreign Equity Share (FES)
data$TVEH <- as.numeric(data$DEH) + as.numeric(data$FEH)
data$FES <- as.numeric(data$FEH) / as.numeric(data$TVEH)

# Load World Bank Data for Market Capitalization
marketcap <- wb_data(indicator = "CM.MKT.LCAP.CD", start_date = 1997, end_date = 2022)
marketcap[[5]][is.na(marketcap[[5]])] <- 0

# Prepare market capitalization data
marketcap <- marketcap[, c(3, 4, 5)]
colnames(marketcap) <- c("Country", "Year", "MarketCap")

# Summarize global market capitalization by year
global_market_cap <- marketcap %>%
  group_by(Year) %>%
  summarise(TotalMarketCap = sum(MarketCap))

# Merge market capitalization data into the main dataset
data <- merge(data, marketcap, by = c("Country", "Year"), all = FALSE)
data <- data %>%
  left_join(global_market_cap, by = "Year")

# Compute optimal foreign equity allocation
data$Optimal <- 1 - (data$MarketCap / data$TotalMarketCap)

# Filter rows where MarketCap > 0
data <- data %>%
  filter(MarketCap > 0)

# Compute Home Bias (HB)
data$HB <- 1 - (data$FES / data$Optimal)

# Remove duplicate rows based on Country-Year pairs
data <- data %>%
  distinct(Country, Year, .keep_all = TRUE)

################################ Cultural Variables ################################

# Load and clean Religious Groups dataset
HIRF <- read.csv("Thesis/Data/ReligiousGroupsLong_v1.02.csv") %>%
  select(Country, Year, Group.Name, Group.Estimate) %>%   # Select relevant columns
  mutate(Group.Estimate = Group.Estimate / 100) %>%       # Transform Group.Estimate to proportion
  distinct() %>%                                          # Remove duplicates
  drop_na()                                               # Remove rows with NAs

# Function to calculate Religious Fractionalization (RF)
calculate_RF <- function(group_proportions) {
  1 - sum(group_proportions^2, na.rm = TRUE)  # RF = 1 - Sum(Si^2)
}

# Calculate RF for each Country and Year
HIRF_RF <- HIRF %>%
  group_by(Country, Year) %>%
  summarise(RFindex = calculate_RF(Group.Estimate)) %>%
  ungroup()

# Merge RF index with the main dataset
data <- data %>%
  left_join(HIRF_RF, by = c("Country", "Year")) %>%
  filter(!is.na(RFindex))  # Remove rows with missing RF index

# Load and clean Ethnic Fractionalization dataset
HIEF <- read_excel("Thesis/Data/HIEF.xlsx") %>%
  select(cname, year, hief_efindex) %>%
  rename(Country = cname, Year = year, EthnicFractionalisation = hief_efindex)  # Rename columns

# Merge Ethnic Fractionalization data with the main dataset
data <- data %>%
  left_join(HIEF, by = c("Country", "Year")) %>%
  filter(!is.na(EthnicFractionalisation))  # Remove rows with missing Ethnic Fractionalization data

################################ Control Variables ################################

# GDP per capita (current US$) relative to US
gdp_per_capita <- wb_data(indicator = "NY.GDP.PCAP.CD", start_date = 1997, end_date = 2022) %>%
  select(country, date, NY.GDP.PCAP.CD) %>%
  rename(Country = country, Year = date, GDPperCapita = NY.GDP.PCAP.CD)

# Merge US GDP per capita into the dataset
gdp_us <- gdp_per_capita %>%
  filter(Country == "United States") %>%
  select(Year, US_GDPperCapita = GDPperCapita)

gdp_per_capita <- gdp_per_capita %>%
  left_join(gdp_us, by = "Year") %>%
  mutate(GDPperCapitaRelUS = GDPperCapita / US_GDPperCapita)

# Exports as % of GDP (converted to decimal)
exports_gdp <- wb_data(indicator = "NE.EXP.GNFS.ZS", start_date = 1997, end_date = 2022) %>%
  select(country, date, NE.EXP.GNFS.ZS) %>%
  rename(Country = country, Year = date, ExportsGDP = NE.EXP.GNFS.ZS) %>%
  mutate(ExportsGDP = ExportsGDP / 100)

# Domestic market capitalisation as % of GDP
domestic_mktcap_gdp <- wb_data(indicator = "CM.MKT.LCAP.GD.ZS", start_date = 1997, end_date = 2022) %>%
  select(country, date, CM.MKT.LCAP.GD.ZS) %>%
  rename(Country = country, Year = date, DomesticMktCapGDP = CM.MKT.LCAP.GD.ZS) %>%
  mutate(DomesticMktCapGDP = DomesticMktCapGDP / 100)

# Merge all control variables into the main dataset
data <- data %>%
  left_join(gdp_per_capita, by = c("Country", "Year")) %>%
  left_join(exports_gdp, by = c("Country", "Year")) %>%
  left_join(domestic_mktcap_gdp, by = c("Country", "Year")) %>%
  filter(!is.na(GDPperCapita) & !is.na(ExportsGDP)) %>%
  distinct(Country, Year, .keep_all = TRUE)

# Foreign Market Capitalisation/GDP
gdp <- wb_data(indicator = "NY.GDP.MKTP.CD", start_date = 1997, end_date = 2022) %>%
  select(country, date, NY.GDP.MKTP.CD) %>%
  rename(Country = country, Year = date, GDP = NY.GDP.MKTP.CD)

data <- data %>%
  left_join(gdp, by = c("Country", "Year")) %>%
  mutate(ForeignMarketCap = TotalMarketCap - MarketCap, 
         ForeignMarketCapGDP = ForeignMarketCap / GDP)

# Financial Development Index
FDIndex <- load_datasets("FDI")
country_codes_FDIndex <- FDIndex$dimensions$ref_area[[1]]
country_names_FDIndex <- FDIndex$dimensions$ref_area[[2]]
country_mapping_FDIndex <- setNames(country_names_FDIndex, country_codes_FDIndex)

# Function to fetch FDIndex data for all countries
add_country_FDIndex_data <- function(country_code) {
  FDIndex_series <- FDIndex$get_series(freq = "A", ref_area = country_code, 
                                       indicator = "FD_FD_IX")
  if (!is.null(FDIndex_series) && nrow(FDIndex_series) > 0) {
    data.frame(Country = country_code, 
               Year = FDIndex_series[[1]], 
               FDIndex = FDIndex_series[[2]], 
               stringsAsFactors = FALSE)
  } else {
    NULL
  }
}

# Fetch FDIndex data for all countries
FDIndex_data <- lapply(country_codes_FDIndex, add_country_FDIndex_data) %>%
  bind_rows()

FDIndex_data$Country <- country_mapping_FDIndex[FDIndex_data$Country]

# Convert Year column to numeric in both dataframes before joining
FDIndex_data$Year <- as.numeric(FDIndex_data$Year)
data$Year <- as.numeric(data$Year)

# Merge FDIndex data into the main dataset
data <- data %>%
  left_join(FDIndex_data, by = c("Country", "Year")) %>%
  distinct(Country, Year, .keep_all = TRUE)

###################################################### DESCRIPTIVE STATISTICS #######################################################################

# Load the all.csv file
all_data <- read.csv("Thesis/Data/all.csv") %>%
  select(name, region) %>%
  rename(Country = name)

# Merge region column from all_data into the data df
data <- data %>%
  left_join(all_data, by = "Country")

# Define regions for specific countries
region_mapping <- c(
  "United States" = "Americas",
  "United Kingdom" = "Europe",
  "Netherlands" = "Europe",
  "Bolivia" = "Americas",
  "Bosnia-Herzegovina" = "Europe",
  "Cape Verde" = "Africa",
  "Cote d'Ivoire" = "Africa",
  "Czech Republic" = "Europe",
  "Czechoslovakia" = "Europe",
  "Democratic Republic of Congo" = "Africa",
  "Democratic Republic of Vietnam" = "Asia",
  "East Timor" = "Asia",
  "German Democratic Republic" = "Europe",
  "German Federal Republic" = "Europe",
  "Iran" = "Asia",
  "Kyrgyz Republic" = "Asia",
  "Laos" = "Asia",
  "Macedonia" = "Europe",
  "Moldova" = "Europe",
  "Republic of Korea" = "Asia",
  "Rumania" = "Europe",
  "Russia" = "Europe",
  "Swaziland" = "Africa",
  "Syria" = "Asia",
  "Taiwan" = "Asia",
  "Tanzania" = "Africa",
  "Turkey" = "Asia",
  "USSR" = "Europe",
  "Venezuela" = "Americas",
  "Yemen Arab Republic" = "Asia",
  "Yemen PDR" = "Asia",
  "Yugoslavia" = "Europe",
  "Vietnam, South" = "Asia",
  "Taiwan (Province of China)" = "Asia",
  "Yemen Democratic" = "Asia"
)

# Update region column based on the mapping
data <- data %>%
  mutate(region = coalesce(region, region_mapping[Country]))

# Remove rows with missing 'region'
data <- data %>%
  filter(!is.na(region))

# Relabel EthnicFractionalisation column for aesthetics and generate descriptive tables
table1::label(data$EthnicFractionalisation) <- "Ethnic Fractionalisation Index"
table1::table1(~ EthnicFractionalisation, data = data)
table1::label(data$region) <- "Region"
table1::table1(~ EthnicFractionalisation | region, data = data)

# Relabel RFindex column for aesthetics and generate descriptive tables
table1::label(data$RFindex) <- "Religious Fractionalisation Index"
table1::table1(~ RFindex, data = data)
table1::table1(~ RFindex | region, data = data)

# Relabel HB column for aesthetics and generate descriptive tables
table1::label(data$HB) <- "Equity Home Bias"
table1::table1(~ HB, data = data)
table1::table1(~ HB | region, data = data)

# Plot Equity Home Bias by Country in 2013
ggplot(subset(data, Year == 2013), aes(x = reorder(Country, HB), y = HB, fill = HB)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal(base_size = 12) +  # Smaller base size for a more formal appearance
  labs(title = "Equity Home Bias by Country in 2013", x = "Country", y = "Equity Home Bias") +
  scale_fill_gradient(low = "skyblue", high = "darkred") +  # Softer color gradient
  theme(
    axis.text.x = element_text(size = 10, angle = 0, hjust = 1),  # Smaller, aligned text
    axis.text.y = element_text(size = 10),  # Ensure clarity for longer country names
    axis.title.x = element_text(size = 12),  # Professional axis title size
    axis.title.y = element_text(size = 12),  # Professional axis title size
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  # Bold title
    plot.margin = margin(10, 10, 10, 10),  # Add some space around the plot
    legend.position = "none",  # No legend for clarity
    panel.grid.major = element_line(size = 0.5, color = "grey80"),  # Light grid for readability
    panel.grid.minor = element_blank(),  # Remove minor grid lines for a cleaner look
    axis.line = element_line(size = 0.5)  # Thin axis lines for a professional appearance
  )

###################################################### REGRESSIONS #######################################################################

# Convert the data to a panel data structure using plm package
pdata <- pdata.frame(data, index = c("Country", "Year"))

# Ensure that FDIndex is treated as a continuous variable
pdata$FDIndex <- as.numeric(pdata$FDIndex)

# Check for missing values and panel dimensions
table(is.na(pdata$HB))  # Ensure no missing dependent var
pdim(pdata)             # Check panel dimensions

####################### Fixed Effects Model #######################
fixed_model <- plm(HB ~ EthnicFractionalisation + RFindex + GDPperCapitaRelUS + 
                     FDIndex + ExportsGDP + DomesticMktCapGDP + ForeignMarketCapGDP,
                   data = pdata, model = "within")

# Extract panel index for countries and years
panel_index <- index(fixed_model)
countries <- panel_index[[1]]  # Country names
years <- panel_index[[2]]      # Years

# Country-clustered Standard Errors
vcov_cr_country <- vcovCR(fixed_model, cluster = countries, type = "CR2")
cat("Country-clustered SEs:\n")
print(coeftest(fixed_model, vcov. = vcov_cr_country))

# Time-clustered Standard Errors
vcov_cr_time <- vcovCR(fixed_model, cluster = years, type = "CR2")
cat("\nTime-clustered SEs:\n")
print(coeftest(fixed_model, vcov. = vcov_cr_time))

# Two-way Clustered Standard Errors (Thompson 2011)
vcov_white <- vcovHC(fixed_model, method = "arellano", type = "HC1")  # White robust VCOV
vcov_two_way <- vcov_cr_country + vcov_cr_time - vcov_white  # Apply Thompson's formula

cat("\nTwo-way clustered SEs:\n")
print(coeftest(fixed_model, vcov. = vcov_two_way))

####################### Robustness Checks for Base Fixed Effects Model #######################

######## 1. Cut-off Sensitivity: High Financial Development ########
# Define the threshold for high financial development (75th percentile)
fd_cutoff <- quantile(pdata$FDIndex, 0.75, na.rm = TRUE)

# Filter for top 25% most financially developed countries
pdata_highFD <- subset(pdata, FDIndex > fd_cutoff)

# Run the fixed effects regression on this subsample
fe_highFD <- plm(HB ~ EthnicFractionalisation + RFindex + GDPperCapitaRelUS + 
                   FDIndex + ExportsGDP + DomesticMktCapGDP + ForeignMarketCapGDP,
                 data = pdata_highFD, model = "within")

# Clustered SEs by Year for High FD sample
vcov_cr_highFD <- vcovCR(fe_highFD, cluster = years, type = "CR2")
cat("\nHigh FD: Time-clustered SEs:\n")
print(coeftest(fe_highFD, vcov. = vcov_cr_highFD))

# Cut-off Sensitivity: Low Financial Development
# Define the threshold for low financial development (25th percentile)
fd_cutoff_low <- quantile(pdata$FDIndex, 0.25, na.rm = TRUE)

# Filter for bottom 25% most financially underdeveloped countries
pdata_lowFD <- subset(pdata, FDIndex <= fd_cutoff_low)

# Run the fixed effects regression on this low FD subsample
fe_lowFD <- plm(HB ~ EthnicFractionalisation + RFindex + GDPperCapitaRelUS + 
                  FDIndex + ExportsGDP + DomesticMktCapGDP + ForeignMarketCapGDP,
                data = pdata_lowFD, model = "within")

# Clustered SEs by Year for Low FD sample
vcov_cr_lowFD <- vcovCR(fe_lowFD, cluster = years, type = "CR2")
cat("\nLow FD: Time-clustered SEs:\n")
print(coeftest(fe_lowFD, vcov. = vcov_cr_lowFD))

# Generate stargazer comparison table in LaTeX for Low vs High FD
stargazer(fe_lowFD, fe_highFD,
          type = "latex",
          se = list(sqrt(diag(vcov_cr_lowFD)), sqrt(diag(vcov_cr_highFD))),
          title = "Cut-off Sensitivity: Fixed Effects on Low vs High FD Countries",
          column.labels = c("Low FD", "High FD"),
          model.names = FALSE,
          keep.stat = c("n", "rsq", "adj.rsq"),
          dep.var.labels = "Equity Home Bias (HB)",
          no.space = TRUE)

# Print unique countries in each FD subsample
cat("Countries in the top 25% (High FD):\n")
print(unique(pdata_highFD$Country))

cat("\nCountries in the bottom 25% (Low FD):\n")
print(unique(pdata_lowFD$Country))

#### Double-clustered Standard Errors for Appendix ####

# ----- High FD Sample -----
vcov_country_highFD <- vcovCR(fe_highFD, cluster = countries, type = "CR2")
vcov_time_highFD <- vcovCR(fe_highFD, cluster = years, type = "CR2")
vcov_white_highFD <- vcovHC(fe_highFD, method = "arellano", type = "HC1")

# Double-clustered SE (Thompson 2011)
vcov_tw_highFD <- vcov_country_highFD + vcov_time_highFD - vcov_white_highFD
se_highFD <- sqrt(diag(vcov_tw_highFD))

# ----- Low FD Sample -----
vcov_country_lowFD <- vcovCR(fe_lowFD, cluster = countries, type = "CR2")
vcov_time_lowFD <- vcovCR(fe_lowFD, cluster = years, type = "CR2")
vcov_white_lowFD <- vcovHC(fe_lowFD, method = "arellano", type = "HC1")

# Double-clustered SE (Thompson 2011)
vcov_tw_lowFD <- vcov_country_lowFD + vcov_time_lowFD - vcov_white_lowFD
se_lowFD <- sqrt(diag(vcov_tw_lowFD))

# ----- Stargazer Table -----
stargazer(fe_lowFD, fe_highFD,
          type = "latex",
          se = list(se_lowFD, se_highFD),
          title = "Cut-off Sensitivity: Fixed Effects on Low vs High FD Countries",
          column.labels = c("Low FD", "High FD"),
          model.names = FALSE,
          keep.stat = c("n", "rsq", "adj.rsq"),
          dep.var.labels = "Equity Home Bias (HB)",
          no.space = TRUE)

######## 2. Alternative Clustering: Double Clustering (Thompson 2011-style) ########

# Clustered SEs by Country
vcov_cr_country <- vcovCR(fixed_model, cluster = countries, type = "CR2")
cat("Country-clustered SEs:\n")
print(coeftest(fixed_model, vcov. = vcov_cr_country))

# Clustered SEs by Year
vcov_cr_time <- vcovCR(fixed_model, cluster = years, type = "CR2")
cat("\nTime-clustered SEs:\n")
print(coeftest(fixed_model, vcov. = vcov_cr_time))

# Two-way Clustered SEs (Thompson 2011)
# Get the "white" robust VCOV matrix (heteroskedasticity-consistent)
vcov_white <- vcovHC(fixed_model, method = "arellano", type = "HC1")

# Apply Thompson's formula: V = V_country + V_time - V_white
vcov_two_way <- vcov_cr_country + vcov_cr_time - vcov_white

cat("\nTwo-way clustered SEs:\n")
print(coeftest(fixed_model, vcov. = vcov_two_way))

######## 3. Check Model Assumptions ########

#### i.i.d. across units (cross-sectional independence)
# Run the Pesaran CD test on your fixed effects model
pcdtest(fixed_model, test = "cd")

## Multicollinearity
# Run linear regression to check for multicollinearity using VIF (Variance Inflation Factor)
lm_model <- lm(HB ~ EthnicFractionalisation + RFindex + GDPperCapitaRelUS + 
                 as.numeric(FDIndex) + ExportsGDP + DomesticMktCapGDP + ForeignMarketCapGDP, 
               data = data)
vif(lm_model)

### Residuals being normally distributed
# Extract residuals and fitted values from the fixed effects model
fitted_values <- fitted(fixed_model)
residuals_fe <- residuals(fixed_model)

# Create a data frame for residuals vs fitted values plot
resid_plot_data <- data.frame(Fitted = fitted_values, Residuals = residuals_fe)

######## 4. Model Specifications: Non-linearity ######## 

# Add squared and cubic terms for EthnicFractionalisation and RFindex
pdata$EthnicFractionalisation_sq <- pdata$EthnicFractionalisation^2
pdata$EthnicFractionalisation_cube <- pdata$EthnicFractionalisation^3
pdata$RFindex_sq <- pdata$RFindex^2
pdata$RFindex_cube <- pdata$RFindex^3

# Fit the new model with non-linear terms
extended_model <- plm(HB ~ EthnicFractionalisation + EthnicFractionalisation_sq + EthnicFractionalisation_cube +
                        RFindex + RFindex_sq + RFindex_cube + GDPperCapitaRelUS + 
                        FDIndex + ExportsGDP + DomesticMktCapGDP + ForeignMarketCapGDP,
                      data = pdata, model = "within")

# Compare the models using RSS (Residual Sum of Squares)
RSS_original <- sum(residuals(fixed_model)^2)
RSS_extended <- sum(residuals(extended_model)^2)

# Print RSS to compare model fit
cat("RSS for Original Model: ", RSS_original, "\n")
cat("RSS for Extended Model: ", RSS_extended, "\n")

# Use stargazer to display both models in a table (to compare Rsquared and adj. Rsquared)
stargazer(fixed_model, extended_model, type = "text", title = "Model Comparison")

############## EXTENDED MODEL: Clustered SEs by Year ##############

# Clustered standard errors by year for the extended model
vcov_cr_time <- vcovCR(extended_model, cluster = years, type = "CR2")
cat("\nTime-clustered SEs:\n")
print(coeftest(extended_model, vcov. = vcov_cr_time))

# Stargazer table comparing the original model, the extended model, and the extended model with fixed time effects
# Fit the extended model with fixed time effects
extended_time_model <- plm(HB ~ EthnicFractionalisation + EthnicFractionalisation_sq + EthnicFractionalisation_cube +
                             RFindex + RFindex_sq + RFindex_cube + GDPperCapitaRelUS + 
                             FDIndex + ExportsGDP + DomesticMktCapGDP + ForeignMarketCapGDP + factor(Year),
                           data = pdata, model = "within")

# Calculate clustered standard errors for the extended model by time
vcov_cr_time_FE <- vcovCR(fixed_model, cluster = years, type = "CR2")
vcov_cr_time_EX <- vcovCR(extended_model, cluster = years, type = "CR2")
vcov_cr_time_EXTIME <- vcovCR(extended_time_model, cluster = years, type = "CR2")

# Compare the models using RSS (Residual Sum of Squares)
RSS_extended_time <- sum(residuals(extended_time_model)^2)

# Print RSS to compare model fit
cat("RSS for Extended Model with Time Effects: ", RSS_extended_time, "\n")

# Use stargazer to display a comparison table with time-clustered SEs in LaTeX
stargazer(fixed_model, extended_model, extended_time_model, 
          type = "latex", 
          se = list(sqrt(diag(vcov_cr_time_FE)), 
                    sqrt(diag(vcov_cr_time_EX)), 
                    sqrt(diag(vcov_cr_time_EXTIME))),  
          column.labels = c("Original Model", "Extended Model", "Extended Model with Time Effects"),
          title = "Fixed Effects Models with Time-Clustered Standard Errors",
          dep.var.labels = "Equity Home Bias (HB)",
          keep.stat = c("n", "rsq", "adj.rsq"),
          no.space = TRUE)

# Stargazer to display a comparison table with double-clustered SEs in LaTeX

# Fixed model TIME (White robust and clustered by country)
vcov_white_fixed <- vcovHC(fixed_model, method = "arellano", type = "HC1")
vcov_cr_country_fixed <- vcovCR(fixed_model, cluster = countries, type = "CR2")
vcov_two_way_fixed <- vcov_cr_country_fixed + vcov_cr_time_FE - vcov_white_fixed

# Extended model TIME (White robust and clustered by country)
vcov_white_extended <- vcovHC(extended_model, method = "arellano", type = "HC1")
vcov_cr_country_extended <- vcovCR(extended_model, cluster = countries, type = "CR2")
vcov_two_way_extended <- vcov_cr_country_extended + vcov_cr_time_EX - vcov_white_extended 

# Extended model with time effects TIME (White robust and clustered by country)
vcov_white_extended_time <- vcovHC(extended_time_model, method = "arellano", type = "HC1")
vcov_cr_country_extended_time <- vcovCR(extended_time_model, cluster = countries, type = "CR2")
vcov_two_way_extended_time <- vcov_cr_country_extended_time + vcov_cr_time_EXTIME - vcov_white_extended_time

# Stargazer to display comparison table with double-clustered SEs in LaTeX
stargazer(fixed_model, extended_model, extended_time_model, 
          type = "latex", 
          se = list(sqrt(diag(vcov_two_way_fixed)), 
                    sqrt(diag(vcov_two_way_extended)), 
                    sqrt(diag(vcov_two_way_extended_time))),  
          column.labels = c("Original Model", "Extended Model", "Extended Model with Time Effects"),
          title = "Fixed Effects Models with Time-Clustered Standard Errors",
          dep.var.labels = "Equity Home Bias (HB)",
          keep.stat = c("n", "rsq", "adj.rsq"),
          no.space = TRUE)

# Define list of models and their names
models <- list(fixed_model, extended_model, extended_time_model)
model_names <- c("Original Model", "Extended Model", "Extended + Time FE")

# Perform an F-test to compare the fixed_model and extended_model
f_test_1 <- phtest(fixed_model, extended_model)

# Perform an F-test to compare the extended_model and extended_time_model
f_test_2 <- phtest(extended_model, extended_time_model)

# Perform an F-test to compare the fixed_model (Original) and extended_time_model
f_test_original_vs_extended_time <- phtest(fixed_model, extended_time_model)

cat("\nF-test: Standard Fixed Model vs Extended Model\n")
print(f_test_1)
cat("\nF-test: Extended Model vs Extended Model with Time Effects\n")
print(f_test_2)
cat("\nF-test: Original Model vs Extended Model with Time Effects\n")
print(f_test_original_vs_extended_time)

# GRAPH RELATIONSHIPS IN EXTENDED MODELS

# Function to create a plot for different models
create_plot <- function(seq, model_coefs, x_label, title) {
  model_df <- data.frame(
    X = rep(seq, 2),
    Model = rep(c("Model 2", "Model 3"), each = length(seq))
  )
  
  model_df$Predicted_HB <- ifelse(
    model_df$Model == "Model 2",
    model_coefs[1] * model_df$X + model_coefs[2] * model_df$X^2 + model_coefs[3] * model_df$X^3,
    model_coefs[4] * model_df$X + model_coefs[5] * model_df$X^2 + model_coefs[6] * model_df$X^3
  )
  
  ggplot(model_df, aes(x = X, y = Predicted_HB, linetype = Model, color = Model)) +
    geom_line(size = 1.2) +
    labs(
      x = x_label,
      y = expression("Equity Home Bias (HB)"),
      title = title
    ) +
    scale_linetype_manual(
      values = c("solid", "twodash"),
      breaks = c("Model 2", "Model 3"),
      labels = c("Model 2: Extended Model", "Model 3: Extended Model + Time Effects")
    ) +
    scale_color_manual(
      values = c("blue", "red"),
      breaks = c("Model 2", "Model 3"),
      labels = c("Model 2: Extended Model", "Model 3: Extended Model + Time Effects")
    ) +
    theme_classic(base_size = 14, base_family = "serif") +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 15, hjust = 0.5),
      panel.grid = element_blank(),
      panel.border = element_blank()
    )
}

# Coefficients for Ethnic Fractionalisation (Model 2 and Model 3)
ethnic_coeffs_m2 <- c(-1.289, 4.496, -2.309)
ethnic_coeffs_m3 <- c(-0.916, 4.255, -1.996)

# Create plot for Ethnic Fractionalisation
eth_seq <- seq(0, 1, length.out = 200)
create_plot(eth_seq, c(ethnic_coeffs_m2, ethnic_coeffs_m3), "Ethnic Fractionalisation", "")

# Coefficients for Religious Fractionalisation (Model 2 and Model 3)
rf_coeffs_m2 <- c(0.0003, -2.994, 3.916)
rf_coeffs_m3 <- c(0.994, -5.164, 6.141)

# Create plot for Religious Fractionalisation
rf_seq <- seq(0, 1, length.out = 200)
create_plot(rf_seq, c(rf_coeffs_m2, rf_coeffs_m3), "Religious Fractionalisation", "")

# Residuals vs fitted values for the: fixed_model, extended_model and extended_time_model.
# Create a list of models
models <- list(
  "Fixed Effects Model" = fixed_model,
  "Extended Model" = extended_model,
  "Extended Model + Time Effects" = extended_time_model
)

# Create a data frame with residuals and fitted values for all models
residuals_df <- bind_rows(lapply(names(models), function(name) {
  model <- models[[name]]
  data.frame(
    Fitted = fitted(model),
    Residuals = residuals(model),
    Model = name
  )
}))

# Plot residuals vs fitted values with best fit lines
ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~Model, scales = "free") +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residuals vs Fitted Values with LOESS Fit"
  ) +
  theme_classic(base_size = 14, base_family = "serif") +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 15, hjust = 0.5),
    axis.text = element_text(size = 11)
  )

# EXTENDED TIME MODEL WITH LOGS

# Fit the extended time model with logs
extended_time_model_log <- plm(
  HB ~ EthnicFractionalisation + EthnicFractionalisation_sq + EthnicFractionalisation_cube +
    RFindex + RFindex_sq + RFindex_cube + GDPperCapitaRelUS + FDIndex + 
    log(ExportsGDP) + log(DomesticMktCapGDP) + ForeignMarketCapGDP + factor(Year),
  data = pdata, model = "within"
)

# Summarize the model
summary(extended_time_model_log)

# Extract fitted values and residuals
fitted_vals <- fitted(extended_time_model_log)
residual_vals <- residuals(extended_time_model_log)

# Calculate RSS
RSS <- sum(residual_vals^2)
cat("RSS: ", RSS, "\n")

# Create a data frame for residuals vs fitted values
resid_df <- data.frame(
  Fitted = fitted_vals,
  Residuals = residual_vals
)

# Plot residuals vs fitted values with LOESS best fit line
ggplot(resid_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residuals vs Fitted Values\nExtended Time Model with Logs"
  ) +
  theme_classic(base_size = 14, base_family = "serif") +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    axis.text = element_text(size = 11)
  )

# Compute time-clustered standard errors
vcov_cr_time_extended_time_log <- vcovCR(extended_time_model_log, cluster = years, type = "CR2")

# Print clustered standard errors
cat("\nTime-clustered SEs:\n")
print(coeftest(extended_time_model_log, vcov. = vcov_cr_time_extended_time_log))

# Stargazer table for extended_time_model_log with time-clustered SEs
stargazer(
  extended_time_model_log,
  type = "latex",
  se = list(sqrt(diag(vcov_cr_time_extended_time_log))),
  title = "Extended Time Model with Time-Clustered Standard Errors",
  dep.var.labels = "Equity Home Bias (HB)",
  covariate.labels = c(
    "Ethnic Fractionalisation", "Ethnic Frac.$^2$", "Ethnic Frac.$^3$",
    "Religious Frac.", "Religious Frac.$^2$", "Religious Frac.$^3$",
    "GDP per Capita (Rel. to US)", "Financial Development Index",
    "log(Exports/GDP)", "log(Domestic Mkt Cap/GDP)", "Foreign Mkt Cap/GDP"
  ),
  omit = "factor\\(Year\\)",
  omit.labels = "Year Fixed Effects",
  no.space = TRUE,
  model.numbers = FALSE,
  digits = 3,
  font.size = "small",
  label = "tab:extended_time_model_log",
  column.sep.width = "1pt"
)

# Compute double-clustered standard errors (by country and time)
vcov_white_log <- vcovHC(extended_time_model_log, method = "arellano", type = "HC1")
vcov_cr_country_log <- vcovCR(extended_time_model_log, cluster = countries, type = "CR2")
vcov_two_way_log <- vcov_cr_country_log + vcov_cr_time_extended_time_log - vcov_white_log

# Stargazer table for extended_time_model_log with double-clustered SEs
stargazer(
  extended_time_model_log,
  type = "latex",
  se = list(sqrt(diag(vcov_two_way_log))),
  title = "Extended Time Model with Double-Clustered Standard Errors",
  dep.var.labels = "HB",
  covariate.labels = c(
    "Ethnic Fractionalisation", "Ethnic Frac.$^2$", "Ethnic Frac.$^3$",
    "Religious Frac.", "Religious Frac.$^2$", "Religious Frac.$^3$",
    "GDP per Capita (Rel. to US)", "Financial Development Index",
    "log(Exports/GDP)", "log(Domestic Mkt Cap/GDP)", "Foreign Mkt Cap/GDP"
  ),
  omit = "factor\\(Year\\)",
  omit.labels = "Year Fixed Effects",
  no.space = TRUE,
  model.numbers = FALSE,
  digits = 3,
  font.size = "small",
  label = "tab:extended_time_model_log",
  column.sep.width = "1pt"
)

################################################################################################################################################################
#################### APPENDIX: OLD TESTS ####################

###################### Model Comparison with and without Migration ######################
#################### Conclusion: Migration does not significantly improve the standard fixed model nor the two-way FD model (p ≥ 0.05).

# 1. Load Population Data
pop <- wb_data(indicator = "SP.POP.TOTL", start_date = 1997, end_date = 2022) %>%
  select(country, date, SP.POP.TOTL) %>%
  rename(Country = country, Year = date, pop = SP.POP.TOTL)

data <- data %>%
  left_join(pop, by = c("Country", "Year"))

# 2. Load Foreign-Born Population Data
foreign_born <- read.csv("Thesis/Data/Foreign_Born.csv") %>%
  select(Country, TIME_PERIOD, OBS_VALUE) %>%
  rename(Year = TIME_PERIOD, foreignborn = OBS_VALUE)

data <- data %>%
  left_join(foreign_born, by = c("Country", "Year")) %>%
  mutate(foreignborn_percentage = foreignborn / pop)

# 3. Clean Migration Data from Excel
file_path <- "Thesis/Data/Stocks of foreign-born population.xlsx"
df <- read_excel(file_path, sheet = "A4_EN_2023", col_names = FALSE)

# Clean the dataset
df <- df[-c(1:3), ]  # Remove first three rows
df <- df[-c(73:74), ]  # Remove last two rows

# Rename columns
colnames(df) <- c("Country", as.character(2012:2022))  # Ensure column names are characters

# Replace the first column value in even rows with the value from the odd row above
df$Country[seq(2, nrow(df), by = 2)] <- df$Country[seq(1, nrow(df) - 1, by = 2)]

# Keep only even rows (corresponding to % of total population)
df <- df[seq(2, nrow(df), by = 2), ]

# Convert all columns except "Country" to character first, then pivot
df <- df %>%
  mutate(across(-Country, as.character)) %>%  # Force all year columns to character
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Migration") %>%
  mutate(Year = as.numeric(Year),  # Convert Year to numeric
         Migration = as.numeric(Migration) / 100)

df <- df %>% drop_na()

# Ensure Year is numeric in both datasets
df <- df %>%
  mutate(Year = as.numeric(Year))

data <- data %>%
  mutate(Year = as.numeric(Year))

# Merge df with data to fill in missing foreignborn_percentage
data <- data %>%
  left_join(df, by = c("Country", "Year")) %>%
  mutate(foreignborn_percentage = ifelse(is.na(foreignborn_percentage), Migration, foreignborn_percentage)) %>%
  select(-Migration)  # Remove temporary column

# Check if all NAs are handled
sum(is.na(data$foreignborn_percentage))  # Should be 0 or reduced significantly

data <- data %>% select(-foreignborn)

# Load data, skipping the first 4 rows
missing_foreignborn <- read.csv("Thesis/Data/Foreign missing 2.csv", skip = 4)

# Remove columns 2, 3, and 4
missing_foreignborn <- missing_foreignborn[, -c(2, 3, 4)]

# Rename remaining columns
colnames(missing_foreignborn) <- c("Country", 1960:2023)

# Remove columns that contain only NA values
missing_foreignborn <- missing_foreignborn %>%
  select(where(~ any(!is.na(.))))  # Keeps only columns with data

# Pivot dataset to long format
missing_foreignborn <- missing_foreignborn %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "foreignborn_percentage") %>%
  mutate(Year = as.numeric(Year),
         foreignborn_percentage = as.numeric(foreignborn_percentage) / 100)  # Convert to decimal if needed

# Replace missing foreignborn_percentage in data with corresponding values from missing_foreignborn
data <- data %>%
  mutate(foreignborn_percentage = ifelse(
    is.na(foreignborn_percentage), 
    missing_foreignborn$foreignborn_percentage[match(paste(data$Country, data$Year), paste(missing_foreignborn$Country, missing_foreignborn$Year))],
    foreignborn_percentage
  ))

# Remove rows with NA in foreignborn_percentage
data <- data %>%
  filter(!is.na(foreignborn_percentage))

pdata <- pdata.frame(data, index = c("Country", "Year"))
pdata$FDIndex <- as.numeric(pdata$FDIndex)

# Standard FE model without migration
fixed_model_base <- plm(HB ~ EthnicFractionalisation + RFindex + GDPperCapitaRelUS + 
                          FDIndex + ExportsGDP + DomesticMktCapGDP + ForeignMarketCapGDP,
                        data = pdata, model = "within")

# Standard FE model with migration
fixed_model_mig <- plm(HB ~ EthnicFractionalisation + RFindex + GDPperCapitaRelUS + 
                         FDIndex + ExportsGDP + DomesticMktCapGDP + ForeignMarketCapGDP +
                         foreignborn_percentage,
                       data = pdata, model = "within")

# F-test using pFtest()
f_test_result <- pFtest(fixed_model_mig, fixed_model_base)

# Extract p-value
p_value <- f_test_result$p.value

# Print result and interpretation
cat("F-test p-value:", round(p_value, 4), "\n")

if (p_value < 0.05) {
  cat("Conclusion: Migration significantly improves the model (p < 0.05).\n")
} else {
  cat("Conclusion: Migration does not significantly improve the model (p ≥ 0.05).\n")
}

# Two-Way FD model without migration
fd_model_base <- plm(HB ~ EthnicFractionalisation + RFindex + GDPperCapitaRelUS + 
                       FDIndex + ExportsGDP + DomesticMktCapGDP + ForeignMarketCapGDP,
                     data = pdata, model = "fd")

# Two-Way FD model with migration
fd_model_mig <- plm(HB ~ EthnicFractionalisation + RFindex + GDPperCapitaRelUS + 
                      FDIndex + ExportsGDP + DomesticMktCapGDP + ForeignMarketCapGDP +
                      foreignborn_percentage,
                    data = pdata, model = "fd")

# F-test using pFtest()
f_test_result <- pFtest(fd_model_mig, fd_model_base)

# Extract p-value
p_value <- f_test_result$p.value

# Print result and interpretation
cat("F-test p-value:", round(p_value, 4), "\n")

if (p_value < 0.05) {
  cat("Conclusion: Migration significantly improves the model (p < 0.05).\n")
} else {
  cat("Conclusion: Migration does not significantly improve the model (p ≥ 0.05).\n")
}

