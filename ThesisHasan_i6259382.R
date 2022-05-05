#Activate Packages
library(outliers)
library(lmtest)
library(readxl)
library(dplyr)
library(WriteXLS)
library(stringr)
library(lubridate)
library(AER)
library(lmtest)
library(olsrr)
library(outliers)
library(readr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(stargazer)
library(robustHD)
library(psych)
library(summarytools)
library(modelsummary)
library(fixest)


#Load STOXX EUROPE 600 Bonds
data_STOXXEUROPE600 <- read_excel('STOXXEUROPE600BONDS.xlsx', sheet = 'Tab1')


#Split Dataframe In Subdataframes of Quarters
create_base_data_set <- function(total_data_set){ # Create Dataset Without Quarter Columns
  df <- as.data.frame(total_data_set) #Create Data Frame Structure For Easier Processing
  df[, -grep("20", colnames(df))] #Clear Strings That Include "20" In Columnnames
}

#Create Dataset With Function create_base_data_set
base_data_set <- create_base_data_set(data_STOXXEUROPE600) #Create Base Dataset With All Columns That Do Not Include "20"


create_quarter_data_set <- function(data, Quarter){ # Create Dataset For Each Quarter
  df <- as.data.frame(data) #Create Data Frame Structure For Easier Processing
  df <- df[, grep(Quarter, colnames(df))] #Include Strings That Include "Quarter" In Columnnames
  # Rename Columnames By Removing  Quarter Identifier From The Names 
  old_col_names <- colnames(df) #Copy Old Columnnames To Keep Original Dataset 
  new_col_names <- stringr::str_remove(old_col_names, paste0("_", Quarter)) #Remove Everything After "_"
  
  #Unique Identifier: Use The Issue Symbol
  unique_identifier_issueSymbol <- data$Issue_Symbol 
  
  colnames(df) <- new_col_names
  df$Quarter <- Quarter #Create New Column With The Respective Quarters From Line 39 (Function Argument 2)
  df <- cbind(df, unique_identifier_issueSymbol) #Merge Unique Identifier To The Quarter Datasets For Merging Purposes
  df # Return The Final Dataset
  
}


#Create Quarter Datasets
dataset_Q12014 <- create_quarter_data_set(data_STOXXEUROPE600, '2014Q1') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q22014 <- create_quarter_data_set(data_STOXXEUROPE600, '2014Q2') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q32014 <- create_quarter_data_set(data_STOXXEUROPE600, '2014Q3') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q42014 <- create_quarter_data_set(data_STOXXEUROPE600, '2014Q4') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)

dataset_Q12015 <- create_quarter_data_set(data_STOXXEUROPE600, '2015Q1') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q22015 <- create_quarter_data_set(data_STOXXEUROPE600, '2015Q2') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q32015 <- create_quarter_data_set(data_STOXXEUROPE600, '2015Q3') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q42015 <- create_quarter_data_set(data_STOXXEUROPE600, '2015Q4') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)

dataset_Q12016 <- create_quarter_data_set(data_STOXXEUROPE600, '2016Q1') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q22016 <- create_quarter_data_set(data_STOXXEUROPE600, '2016Q2') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q32016 <- create_quarter_data_set(data_STOXXEUROPE600, '2016Q3') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q42016 <- create_quarter_data_set(data_STOXXEUROPE600, '2016Q4') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)

dataset_Q12017 <- create_quarter_data_set(data_STOXXEUROPE600, '2017Q1') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q22017 <- create_quarter_data_set(data_STOXXEUROPE600, '2017Q2') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q32017 <- create_quarter_data_set(data_STOXXEUROPE600, '2017Q3') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q42017 <- create_quarter_data_set(data_STOXXEUROPE600, '2017Q4') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)

dataset_Q12018 <- create_quarter_data_set(data_STOXXEUROPE600, '2018Q1') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q22018 <- create_quarter_data_set(data_STOXXEUROPE600, '2018Q2') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q32018 <- create_quarter_data_set(data_STOXXEUROPE600, '2018Q3') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q42018 <- create_quarter_data_set(data_STOXXEUROPE600, '2018Q4') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)

dataset_Q12019 <- create_quarter_data_set(data_STOXXEUROPE600, '2019Q1') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q22019 <- create_quarter_data_set(data_STOXXEUROPE600, '2019Q2') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q32019 <- create_quarter_data_set(data_STOXXEUROPE600, '2019Q3') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q42019 <- create_quarter_data_set(data_STOXXEUROPE600, '2019Q4') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)

dataset_Q12020 <- create_quarter_data_set(data_STOXXEUROPE600, '2020Q1') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q22020 <- create_quarter_data_set(data_STOXXEUROPE600, '2020Q2') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q32020 <- create_quarter_data_set(data_STOXXEUROPE600, '2020Q3') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q42020 <- create_quarter_data_set(data_STOXXEUROPE600, '2020Q4') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)

dataset_Q12021 <- create_quarter_data_set(data_STOXXEUROPE600, '2021Q1') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q22021 <- create_quarter_data_set(data_STOXXEUROPE600, '2021Q2') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q32021 <- create_quarter_data_set(data_STOXXEUROPE600, '2021Q3') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)
dataset_Q42021 <- create_quarter_data_set(data_STOXXEUROPE600, '2021Q4') #Use Function (Argument 1 = Dataset; Argument 2 = Year Quarter I Want To Extract)


#Bind Quarter Datasets Into Cleaned Dataset With Quarter Stacked
cleaned_total_dataset_STOXXEUROPE600 <- rbind(dataset_Q12014, dataset_Q22014, dataset_Q32014,dataset_Q42014,
                                           dataset_Q12015, dataset_Q22015, dataset_Q32015,dataset_Q42015,
                                           dataset_Q12016, dataset_Q22016, dataset_Q32016,dataset_Q42016,
                                           dataset_Q12017, dataset_Q22017, dataset_Q32017,dataset_Q42017,
                                           dataset_Q12018, dataset_Q22018, dataset_Q32018,dataset_Q42018,
                                           dataset_Q12019, dataset_Q22019, dataset_Q32019,dataset_Q42019,
                                           dataset_Q12020, dataset_Q22020, dataset_Q32020,dataset_Q42020,
                                           dataset_Q12021, dataset_Q22021, dataset_Q32021,dataset_Q42021
)


#Final Dataset With Quarters Stacked
cleaned_total_dataset_STOXXEUROPE600 <- cbind(
  cleaned_total_dataset_STOXXEUROPE600, base_data_set)


#Load S&P BONDS
sp500 <- read_excel('SP500BONDS.xlsx', sheet = 'Summary')


#Create Base Dataset (Function)
base_data_set_sp500 <- create_base_data_set(sp500)


#Create Quarter Dataset
dataset_sp500_Q12014 <- create_quarter_data_set(sp500, '2014Q1')
dataset_sp500_Q22014 <- create_quarter_data_set(sp500, '2014Q2')
dataset_sp500_Q32014 <- create_quarter_data_set(sp500, '2014Q3')
dataset_sp500_Q42014 <- create_quarter_data_set(sp500, '2014Q4')

dataset_sp500_Q12015 <- create_quarter_data_set(sp500, '2015Q1')
dataset_sp500_Q22015 <- create_quarter_data_set(sp500, '2015Q2')
dataset_sp500_Q32015 <- create_quarter_data_set(sp500, '2015Q3')
dataset_sp500_Q42015 <- create_quarter_data_set(sp500, '2015Q4')

dataset_sp500_Q12016 <- create_quarter_data_set(sp500, '2016Q1')
dataset_sp500_Q22016 <- create_quarter_data_set(sp500, '2016Q2')
dataset_sp500_Q32016 <- create_quarter_data_set(sp500, '2016Q3')
dataset_sp500_Q42016 <- create_quarter_data_set(sp500, '2016Q4')

dataset_sp500_Q12017 <- create_quarter_data_set(sp500, '2017Q1')
dataset_sp500_Q22017 <- create_quarter_data_set(sp500, '2017Q2')
dataset_sp500_Q32017 <- create_quarter_data_set(sp500, '2017Q3')
dataset_sp500_Q42017 <- create_quarter_data_set(sp500, '2017Q4')

dataset_sp500_Q12018 <- create_quarter_data_set(sp500, '2018Q1')
dataset_sp500_Q22018 <- create_quarter_data_set(sp500, '2018Q2')
dataset_sp500_Q32018 <- create_quarter_data_set(sp500, '2018Q3')
dataset_sp500_Q42018 <- create_quarter_data_set(sp500, '2018Q4')

dataset_sp500_Q12019 <- create_quarter_data_set(sp500, '2019Q1')
dataset_sp500_Q22019 <- create_quarter_data_set(sp500, '2019Q2')
dataset_sp500_Q32019 <- create_quarter_data_set(sp500, '2019Q3')
dataset_sp500_Q42019 <- create_quarter_data_set(sp500, '2019Q4')

dataset_sp500_Q12020 <- create_quarter_data_set(sp500, '2020Q1')
dataset_sp500_Q22020 <- create_quarter_data_set(sp500, '2020Q2')
dataset_sp500_Q32020 <- create_quarter_data_set(sp500, '2020Q3')
dataset_sp500_Q42020 <- create_quarter_data_set(sp500, '2020Q4')

dataset_sp500_Q12021 <- create_quarter_data_set(sp500, '2021Q1')
dataset_sp500_Q22021 <- create_quarter_data_set(sp500, '2021Q2')
dataset_sp500_Q32021 <- create_quarter_data_set(sp500, '2021Q3')
dataset_sp500_Q42021 <- create_quarter_data_set(sp500, '2021Q4')


#Bind Quarter Datasets Into Cleaned Dataset With Quarter Stacked
stacked_quarter_dataset_sp500 <- rbind(dataset_sp500_Q12014,dataset_sp500_Q22014,dataset_sp500_Q32014,dataset_sp500_Q42014,
                                       dataset_sp500_Q12015,dataset_sp500_Q22015,dataset_sp500_Q32015,dataset_sp500_Q42015,
                                       dataset_sp500_Q12016,dataset_sp500_Q22016,dataset_sp500_Q32016,dataset_sp500_Q42016,
                                       dataset_sp500_Q12017,dataset_sp500_Q22017,dataset_sp500_Q32017,dataset_sp500_Q42017,
                                       dataset_sp500_Q12018,dataset_sp500_Q22018,dataset_sp500_Q32018,dataset_sp500_Q42018,
                                       dataset_sp500_Q12019,dataset_sp500_Q22019,dataset_sp500_Q32019,dataset_sp500_Q42019,
                                       dataset_sp500_Q12020,dataset_sp500_Q22020,dataset_sp500_Q32020,dataset_sp500_Q42020,
                                       dataset_sp500_Q12021,dataset_sp500_Q22021,dataset_sp500_Q32021,dataset_sp500_Q42021
)


#Final Dataset With Quarters Stacked
stacked_quarter_dataset_sp500 <- cbind(
  stacked_quarter_dataset_sp500, base_data_set_sp500)


#Stack StoxxEurope600 and SP500 Datasets
total_data_set_STOXXEUROPE600_and_sp500 <- rbind(stacked_quarter_dataset_sp500,cleaned_total_dataset_STOXXEUROPE600 )


#Write Function For Fundamentals Dataset
create_quarter_data_set_fundamentals <- function(data, Quarter){ # Create Dataset For Each Quarter
  df <- as.data.frame(data)
  df <- df[, grep(Quarter, colnames(df))]
  # Rename Colnames By Removing Quarter Identifier From The Names 
  old_col_names <- colnames(df)
  new_col_names <- stringr::str_remove(old_col_names, paste0("_", Quarter))
  
  #Unique Identifier: Use The Issue Symbol or CUSIP
  unique_identifier_CUSIP <-  data$CUSIP
  
  colnames(df) <- new_col_names
  df$Quarter <- Quarter
  df <- cbind(df, unique_identifier_CUSIP)
  df # Return the final dataset
  
}


#Load Fundamental Dataset
fundamentals <- read_excel('STOXXEUROPE600_SP500_FUNDAMENTALS.xlsx', sheet = 'Tab1')


#Create Base Dataset
base_data_set_fundamentals <- create_base_data_set(fundamentals)


#Create Quarter Dataset
dataset_fundamentals_Q12014 <- create_quarter_data_set_fundamentals(fundamentals, '2014Q1')
dataset_fundamentals_Q22014 <- create_quarter_data_set_fundamentals(fundamentals, '2014Q2')
dataset_fundamentals_Q32014 <- create_quarter_data_set_fundamentals(fundamentals, '2014Q3')
dataset_fundamentals_Q42014 <- create_quarter_data_set_fundamentals(fundamentals, '2014Q4')

dataset_fundamentals_Q12015 <- create_quarter_data_set_fundamentals(fundamentals, '2015Q1')
dataset_fundamentals_Q22015 <- create_quarter_data_set_fundamentals(fundamentals, '2015Q2')
dataset_fundamentals_Q32015 <- create_quarter_data_set_fundamentals(fundamentals, '2015Q3')
dataset_fundamentals_Q42015 <- create_quarter_data_set_fundamentals(fundamentals, '2015Q4')

dataset_fundamentals_Q12016 <- create_quarter_data_set_fundamentals(fundamentals, '2016Q1')
dataset_fundamentals_Q22016 <- create_quarter_data_set_fundamentals(fundamentals, '2016Q2')
dataset_fundamentals_Q32016 <- create_quarter_data_set_fundamentals(fundamentals, '2016Q3')
dataset_fundamentals_Q42016 <- create_quarter_data_set_fundamentals(fundamentals, '2016Q4')

dataset_fundamentals_Q12017 <- create_quarter_data_set_fundamentals(fundamentals, '2017Q1')
dataset_fundamentals_Q22017 <- create_quarter_data_set_fundamentals(fundamentals, '2017Q2')
dataset_fundamentals_Q32017 <- create_quarter_data_set_fundamentals(fundamentals, '2017Q3')
dataset_fundamentals_Q42017 <- create_quarter_data_set_fundamentals(fundamentals, '2017Q4')

dataset_fundamentals_Q12018 <- create_quarter_data_set_fundamentals(fundamentals, '2018Q1')
dataset_fundamentals_Q22018 <- create_quarter_data_set_fundamentals(fundamentals, '2018Q2')
dataset_fundamentals_Q32018 <- create_quarter_data_set_fundamentals(fundamentals, '2018Q3')
dataset_fundamentals_Q42018 <- create_quarter_data_set_fundamentals(fundamentals, '2018Q4')

dataset_fundamentals_Q12019 <- create_quarter_data_set_fundamentals(fundamentals, '2019Q1')
dataset_fundamentals_Q22019 <- create_quarter_data_set_fundamentals(fundamentals, '2019Q2')
dataset_fundamentals_Q32019 <- create_quarter_data_set_fundamentals(fundamentals, '2019Q3')
dataset_fundamentals_Q42019 <- create_quarter_data_set_fundamentals(fundamentals, '2019Q4')

dataset_fundamentals_Q12020 <- create_quarter_data_set_fundamentals(fundamentals, '2020Q1')
dataset_fundamentals_Q22020 <- create_quarter_data_set_fundamentals(fundamentals, '2020Q2')
dataset_fundamentals_Q32020 <- create_quarter_data_set_fundamentals(fundamentals, '2020Q3')
dataset_fundamentals_Q42020 <- create_quarter_data_set_fundamentals(fundamentals, '2020Q4')

dataset_fundamentals_Q12021 <- create_quarter_data_set_fundamentals(fundamentals, '2021Q1')
dataset_fundamentals_Q22021 <- create_quarter_data_set_fundamentals(fundamentals, '2021Q2')
dataset_fundamentals_Q32021 <- create_quarter_data_set_fundamentals(fundamentals, '2021Q3')
dataset_fundamentals_Q42021 <- create_quarter_data_set_fundamentals(fundamentals, '2021Q4')


#Stack Quarter Datasets
stacked_quarter_dataset_fundamentals <- rbind (
  dataset_fundamentals_Q12014, dataset_fundamentals_Q22014, dataset_fundamentals_Q32014,dataset_fundamentals_Q42014,
  dataset_fundamentals_Q12015, dataset_fundamentals_Q22015, dataset_fundamentals_Q32015,dataset_fundamentals_Q42015,
  dataset_fundamentals_Q12016, dataset_fundamentals_Q22016, dataset_fundamentals_Q32016,dataset_fundamentals_Q42016,
  dataset_fundamentals_Q12017, dataset_fundamentals_Q22017, dataset_fundamentals_Q32017,dataset_fundamentals_Q42017,
  dataset_fundamentals_Q12018, dataset_fundamentals_Q22018, dataset_fundamentals_Q32018,dataset_fundamentals_Q42018,
  dataset_fundamentals_Q12019, dataset_fundamentals_Q22019, dataset_fundamentals_Q32019,dataset_fundamentals_Q42019,
  dataset_fundamentals_Q12020, dataset_fundamentals_Q22020, dataset_fundamentals_Q32020,dataset_fundamentals_Q42020,
  dataset_fundamentals_Q12021, dataset_fundamentals_Q22021, dataset_fundamentals_Q32021,dataset_fundamentals_Q42021
)


# Final Fundamental Stacked Dataset
stacked_quarter_dataset_fundamentals <- cbind(
  stacked_quarter_dataset_fundamentals, base_data_set_fundamentals)


#Change Column Name In CUSIP
names(total_data_set_STOXXEUROPE600_and_sp500)[10] = 'CUSIP'


#Trim CUSIP By 1 Substring
stacked_quarter_dataset_fundamentals$CUSIP[length(stacked_quarter_dataset_fundamentals$CUSIP) > 8 ] <- substring(stacked_quarter_dataset_fundamentals$CUSIP,1,8)


#Create Column Index
stacked_quarter_dataset_fundamentals$Index <- ifelse(stacked_quarter_dataset_fundamentals$`Exchange Country Name` == 'UNITED STATES', 'SP500', 'STOXXEUROPE600')


#Merge Bond Data And Fundamental Data
mergedData <- dplyr::left_join(total_data_set_STOXXEUROPE600_and_sp500, stacked_quarter_dataset_fundamentals, by = c("CUSIP", "Quarter"))


#Select Only Unique Rows From The Data Frame
mergedData <- mergedData %>% distinct(Issue_Symbol, Quarter, .keep_all = TRUE)


#Put Dates Into Right Format For Years To Maturity Calculation
mergedData$Issue_Date <- as.Date(mergedData$Issue_Date, format = "%d/%m/%Y")
mergedData$MaturityDate <- as.Date(mergedData$MaturityDate, format = "%d/%m/%Y")


summary(mergedData)
#Delete Columns
mergedData <- mergedData  %>% 
  select(-'unique_identifier_issueSymbol') %>%  
  select(-'unique_identifier_CUSIP') %>% 
  select(-'Symbol') %>%  
  select(-'Name')%>%  
  select(-'Exchange Country Name')%>%  
  select(-'ISIN')


#Create Clean Dataset
cleanData <- mergedData


#Merge Data Into Right String
cleanData$Issue_Symbol <- as.factor(cleanData$Issue_Symbol)
cleanData$Issue_Name <- as.factor(cleanData$Issue_Name)
cleanData$CUSIP <- as.factor(cleanData$CUSIP)
cleanData$NAICS <- as.factor(cleanData$NAICS)
cleanData$ESGOverall <- as.factor(cleanData$ESGOverall)
cleanData$LagESGOverall <- as.factor(cleanData$LagESGOverall)
cleanData$Lag4ESGOverall <- as.factor(cleanData$Lag4ESGOverall)
cleanData$Index <- as.factor(cleanData$Index)


#Dealing With NA ESG DATA
cleanData <- cleanData[!is.na(cleanData$ESGOverall),]

cleanData <- cleanData[!is.na(cleanData$E),]

cleanData <- cleanData[!is.na(cleanData$S),]

cleanData <- cleanData[!is.na(cleanData$G),]


#Dealing With NA Bond Data
cleanData$IntSpreadToTreasury <- ifelse(cleanData$IntSpreadToTreasury <=0, NA, cleanData$IntSpreadToTreasury)
cleanData <- cleanData[!is.na(cleanData$IntSpreadToTreasury),]

cleanData$IssueIntSpreadToTreasury <- ifelse(cleanData$IssueIntSpreadToTreasury <=0, NA, cleanData$IssueIntSpreadToTreasury)
cleanData <- cleanData[!is.na(cleanData$IssueIntSpreadToTreasury),]

cleanData <- cleanData[!is.na(cleanData$AmountOut),]

cleanData <- cleanData[!is.na(cleanData$IssueAmount),]

cleanData <- cleanData[!is.na(cleanData$Issue_Symbol),]

cleanData <- cleanData[!is.na(cleanData$Index),]


#Create Alternative ESG Measures
#ESG Standardized
cleanData$ESGOverallStd <- ifelse(cleanData$ESGOverall == 'AAA', 7/7, 
                                  ifelse(cleanData$ESGOverall == 'AA', 6/7,
                                         ifelse(cleanData$ESGOverall == 'A', 5/7,
                                                ifelse(cleanData$ESGOverall == 'BBB', 4/7,
                                                       ifelse(cleanData$ESGOverall == 'BB', 3/7,
                                                              ifelse(cleanData$ESGOverall == 'B', 2/7,
                                                                     ifelse(cleanData$ESGOverall == 'CCC', 1/7, NA)))))))


cleanData$LagESGOverallStd <- ifelse(cleanData$LagESGOverall == 'AAA', 7/7, 
                                  ifelse(cleanData$LagESGOverall == 'AA', 6/7,
                                         ifelse(cleanData$LagESGOverall == 'A', 5/7,
                                                ifelse(cleanData$LagESGOverall == 'BBB', 4/7,
                                                       ifelse(cleanData$LagESGOverall == 'BB', 3/7,
                                                              ifelse(cleanData$LagESGOverall == 'B', 2/7,
                                                                     ifelse(cleanData$LagESGOverall == 'CCC', 1/7, NA)))))))


###########################################################


#Create Control Variables
#LnIntSpreadToTreasury
cleanData$lnIntSpreadToTreasury <- log(cleanData$IntSpreadToTreasury)


#lnIssueIntSpreadToTreasury 
cleanData$lnIssueIntSpreadToTreasury <- log(cleanData$IssueIntSpreadToTreasury)


#lnAmountOut
cleanData$AmountOut <- cleanData$AmountOut +1
cleanData$lnAmountOut <- log(cleanData$AmountOut)


#lnIssueAmount
cleanData$IssueAmount <- cleanData$IssueAmount +1
cleanData$lnIssueAmount <- log(cleanData$IssueAmount)


#Quarter Date
cleanData$QuarterDate <- ifelse(cleanData$Quarter == '2014Q1', '31/03/2014', 
                                ifelse(cleanData$Quarter == '2014Q2', '30/06/2014', 
                                       ifelse(cleanData$Quarter == '2014Q3', '30/09/2014', 
                                              ifelse(cleanData$Quarter == '2014Q4', '31/12/2014', 
                                                     ifelse(cleanData$Quarter == '2015Q1', '31/03/2015',
                                                            ifelse(cleanData$Quarter == '2015Q2', '30/06/2015',
                                                                   ifelse(cleanData$Quarter == '2015Q3', '30/09/2015',
                                                                          ifelse(cleanData$Quarter == '2015Q4', '31/12/2015',
                                                                                 ifelse(cleanData$Quarter == '2016Q1', '31/03/2016',
                                                                                        ifelse(cleanData$Quarter == '2016Q2', '30/06/2016',
                                                                                               ifelse(cleanData$Quarter == '2016Q3', '30/09/2016',
                                                                                                      ifelse(cleanData$Quarter == '2016Q4', '31/12/2016',
                                                                                                             ifelse(cleanData$Quarter == '2017Q1', '31/03/2017',
                                                                                                                    ifelse(cleanData$Quarter == '2017Q2', '30/06/2017',
                                                                                                                           ifelse(cleanData$Quarter == '2017Q3', '30/09/2017',
                                                                                                                                  ifelse(cleanData$Quarter == '2017Q4', '30/12/2017',
                                                                                                                                         ifelse(cleanData$Quarter == '2018Q1', '31/03/2018',
                                                                                                                                                ifelse(cleanData$Quarter == '2018Q2', '30/06/2018',
                                                                                                                                                       ifelse(cleanData$Quarter == '2018Q3', '30/09/2018',
                                                                                                                                                              ifelse(cleanData$Quarter == '2018Q4', '31/12/2018',
                                                                                                                                                                     ifelse(cleanData$Quarter == '2019Q1', '31/03/2019',
                                                                                                                                                                            ifelse(cleanData$Quarter == '2019Q2', '30/06/2019',
                                                                                                                                                                                   ifelse(cleanData$Quarter == '2019Q3', '30/09/2019',
                                                                                                                                                                                          ifelse(cleanData$Quarter == '2019Q4', '31/12/2019',
                                                                                                                                                                                                 ifelse(cleanData$Quarter == '2020Q1', '31/03/2020',
                                                                                                                                                                                                        ifelse(cleanData$Quarter == '2020Q2', '30/06/2020',
                                                                                                                                                                                                               ifelse(cleanData$Quarter == '2020Q3', '30/09/2020',
                                                                                                                                                                                                                      ifelse(cleanData$Quarter == '2020Q4', '31/12/2020',
                                                                                                                                                                                                                             ifelse(cleanData$Quarter == '2021Q1', '31/03/2021',
                                                                                                                                                                                                                                    ifelse(cleanData$Quarter == '2021Q2', '30/06/2021',
                                                                                                                                                                                                                                           ifelse(cleanData$Quarter == '2021Q3', '30/09/2021',
                                                                                                                                                                                                                                                  ifelse(cleanData$Quarter == '2021Q4', '31/12/2021',
                                                                                                                                                                                                                                                         NA))))))))))))))))))))))))))))))))
cleanData$QuarterDate <- as.Date(cleanData$QuarterDate, format = "%d/%m/%Y")


#Year
cleanData$Year <- ifelse(cleanData$Quarter == '2014Q1', '2014', 
                         ifelse(cleanData$Quarter == '2014Q2', '2014',
                                ifelse(cleanData$Quarter == '2014Q3', '2014', 
                                       ifelse(cleanData$Quarter == '2014Q4', '2014', 
                                              ifelse(cleanData$Quarter == '2015Q1', '2015',
                                                     ifelse(cleanData$Quarter == '2015Q2', '2015',
                                                            ifelse(cleanData$Quarter == '2015Q3', '2015',
                                                                   ifelse(cleanData$Quarter == '2015Q4', '2015',
                                                                          ifelse(cleanData$Quarter == '2016Q1', '2016',
                                                                                 ifelse(cleanData$Quarter == '2016Q2', '2016',
                                                                                        ifelse(cleanData$Quarter == '2016Q3', '2016',
                                                                                               ifelse(cleanData$Quarter == '2016Q4', '2016',
                                                                                                      ifelse(cleanData$Quarter == '2017Q1', '2017',
                                                                                                             ifelse(cleanData$Quarter == '2017Q2', '2017',
                                                                                                                    ifelse(cleanData$Quarter == '2017Q3', '2017',
                                                                                                                           ifelse(cleanData$Quarter == '2017Q4', '2017',
                                                                                                                                  ifelse(cleanData$Quarter == '2018Q1', '2018',
                                                                                                                                         ifelse(cleanData$Quarter == '2018Q2', '2018',
                                                                                                                                                ifelse(cleanData$Quarter == '2018Q3', '2018',
                                                                                                                                                       ifelse(cleanData$Quarter == '2018Q4', '2018',
                                                                                                                                                              ifelse(cleanData$Quarter == '2019Q1', '2019',
                                                                                                                                                                     ifelse(cleanData$Quarter == '2019Q2', '2019',
                                                                                                                                                                            ifelse(cleanData$Quarter == '2019Q3', '2019',
                                                                                                                                                                                   ifelse(cleanData$Quarter == '2019Q4', '2019',
                                                                                                                                                                                          ifelse(cleanData$Quarter == '2020Q1', '2020',
                                                                                                                                                                                                 ifelse(cleanData$Quarter == '2020Q2', '2020',
                                                                                                                                                                                                        ifelse(cleanData$Quarter == '2020Q3', '2020',
                                                                                                                                                                                                               ifelse(cleanData$Quarter == '2020Q4', '2020',
                                                                                                                                                                                                                      ifelse(cleanData$Quarter == '2021Q1', '2021',
                                                                                                                                                                                                                             ifelse(cleanData$Quarter == '2021Q2', '2021',
                                                                                                                                                                                                                                    ifelse(cleanData$Quarter == '2021Q3', '2021',
                                                                                                                                                                                                                                           ifelse(cleanData$Quarter == '2021Q4', '2021', NA))))))))))))))))))))))))))))))))
cleanData$Year <- as.factor(cleanData$Year)


#IssueQuarter
cleanData$IssueQuarter <- as.yearqtr(cleanData$Issue_Date , format = "%d/%m/%Y")
cleanData$IssueQuarter <- gsub(' ', '', cleanData$IssueQuarter ,fixed = TRUE)
cleanData$IssueQuarter <- as.factor(cleanData$IssueQuarter)
cleanData$IssueQuarter <- ifelse(cleanData$IssueQuarter == cleanData$Quarter , 1 , NA)


#DaysToMaturity
cleanData$DaysToMaturity <- as.numeric(difftime(cleanData$MaturityDate, cleanData$QuarterDate, unit="days"))


#YearsToMaturity
cleanData$YearsToMaturity <- cleanData$DaysToMaturity/365


#IssueDaysToMaturity
cleanData$IssueDaysToMaturity <- as.numeric(difftime(cleanData$MaturityDate, cleanData$Issue_Date, unit="days"))


#IssueYearsToMaturity
cleanData$IssueYearsToMaturity <- cleanData$IssueDaysToMaturity/365


#Leverage
cleanData$Leverage <- cleanData$TotalLiabilities/cleanData$TotalAssets


#Size
cleanData$Size <- log(cleanData$TotalAssets)


#EBITDA
cleanData$EBITDA <- cleanData$EBIT + cleanData$`D&A`


#InterestCoverage
cleanData$InterestCoverage <- cleanData$EBITDA/cleanData$InterestExpense


#ROA
cleanData$ROA <- cleanData$NetIncome/cleanData$TotalAssets


#Loss
cleanData$Loss <- ifelse(cleanData$NetIncome <0, 1, 0 ) 
cleanData$Loss <- as.numeric(cleanData$Loss)


#AltmanZScore
cleanData$AltmanZ <- 1.2 * (cleanData$WC/cleanData$TotalAssets) + 1.4 * (cleanData$RetainedEarnings/cleanData$TotalAssets) +
  3.3 * (cleanData$EBIT/cleanData$TotalAssets) + 0.6 * (cleanData$MarketCapitalization/cleanData$TotalLiabilities) + 0.999 * cleanData$Sales/cleanData$TotalAssets
cleanData<- cleanData[!is.na(cleanData$AltmanZ),]


#AltmanZ Classifications
cleanData$AltmanZClass <- ifelse(cleanData$AltmanZ >=3 , 'SafeZone', ifelse((cleanData$AltmanZ >=1.8) & (cleanData$AltmanZ<3), 'GreyZone', ifelse(cleanData$AltmanZ <1.8 , 'DistressZone', NA)))
cleanData$AltmanZClass <- as.factor(cleanData$AltmanZClass)


#Tobins Q
cleanData$TobinsQ <- (cleanData$TotalAssets - (cleanData$TotalAssets - cleanData$TotalLiabilities) + cleanData$MarketCap) / cleanData$TotalAssets


#WC Ratio
cleanData$WC <- cleanData$WC/cleanData$TotalAssets


#NAICS
cleanData$NAICS <- as.character(cleanData$NAICS)
cleanData$NAICS <-substr(cleanData$NAICS, start=1,stop=2)
cleanData$NAICS <- ifelse(cleanData$NAICS == 11 ,'AgricultureForestryFishingandHunting' , 
                          ifelse(cleanData$NAICS == 21 , 'MiningQuarryingandOilandGasExtraction', 
                                 ifelse(cleanData$NAICS == 22 , 'Utilities',
                                        ifelse(cleanData$NAICS == 23 , 'Construction',
                                               ifelse(cleanData$NAICS == 31, 'Manufacturing',
                                                      ifelse(cleanData$NAICS == 42 , 'WholesaleTrade',
                                                             ifelse(cleanData$NAICS == 44 , 'RetailTrade',
                                                                    ifelse(cleanData$NAICS == 48, 'TransportationandWarehousing',
                                                                           ifelse(cleanData$NAICS == 51, 'Information',
                                                                                  ifelse(cleanData$NAICS == 52 , NA,
                                                                                         ifelse(cleanData$NAICS == 53, 'RealEstateandRentalandLeasing',
                                                                                                ifelse(cleanData$NAICS == 54, 'ProfessionalScientificandTechnicalServices',
                                                                                                       ifelse(cleanData$NAICS == 55, 'ManagementofCompaniesandEnterprises',
                                                                                                              ifelse(cleanData$NAICS == 56, 'AdministrativeandSupportandWasteManagementandRemediation',
                                                                                                                     ifelse(cleanData$NAICS == 61 , 'EducationalServices ',
                                                                                                                            ifelse(cleanData$NAICS == 62, 'HealthCareandSocialAssistance',
                                                                                                                                   ifelse(cleanData$NAICS == 71, 'ArtsEntertainmentandRecreation',
                                                                                                                                          ifelse(cleanData$NAICS == 72, 'AccommodationandFoodServices',
                                                                                                                                                 ifelse(cleanData$NAICS == 81, 'OtherServices',
                                                                                                                                                        ifelse(cleanData$NAICS == 92, 'PublicAdministration', 
                                                                                                                                                               ifelse(cleanData$NAICS == 32 , 'Manufacturing' , 
                                                                                                                                                                      ifelse(cleanData$NAICS == 33 ,'Manufacturing' , 
                                                                                                                                                                             ifelse(cleanData$NAICS == 45 , 'RetailTrade',
                                                                                                                                                                                    ifelse(cleanData$NAICS == 49, 'TransportationandWarehousing',NA))))))))))))))))))))))))
cleanData$NAICS <- as.factor(cleanData$NAICS)


#High-Risk Industry
cleanData$SIC <- ifelse(cleanData$SIC == 2600, 'HighRisk',
                        ifelse(cleanData$SIC == 2800, 'HighRisk',
                               ifelse(cleanData$SIC == 5160, 'HighRisk',
                                      ifelse(cleanData$SIC == 5161, 'HighRisk',
                                             ifelse(cleanData$SIC == 5169, 'HighRisk',
                                                    ifelse(cleanData$SIC == 2910, 'HighRisk',
                                                           ifelse(cleanData$SIC == 2911, 'HighRisk',
                                                                  ifelse(cleanData$SIC == 2900, 'HighRisk',
                                                                         ifelse(cleanData$SIC == 3300, 'HighRisk',
                                                                                ifelse(cleanData$SIC == 1000, 'HighRisk',
                                                                                       ifelse(cleanData$SIC == 1200, 'HighRisk',
                                                                                              'LowRisk')))))))))))


cleanData$SIC <- as.factor(cleanData$SIC)


#Create Binary Variables For Summary Statistics
cleanData$SafeZone <- ifelse(cleanData$AltmanZClass == 'SafeZone', 1, 0)
cleanData$GreyZone <- ifelse(cleanData$AltmanZClass == 'GreyZone', 1, 0)
cleanData$DistressZone <- ifelse(cleanData$AltmanZClass == 'DistressZone', 1, 0)
cleanData$Europe <- ifelse(cleanData$Index == 'STOXXEUROPE600', 1, 0)
cleanData$US <- ifelse(cleanData$Index == 'SP500', 1, 0)


#Deal with Inf and Nan
cleanData$lnIssueIntSpreadToTreasury[which(cleanData$lnIssueIntSpreadToTreasury==-Inf | cleanData$lnIssueIntSpreadToTreasury==Inf)]= NA
cleanData$lnIssueAmount[which(cleanData$lnIssueAmount==-Inf | cleanData$lnIssueAmount==Inf)]= NA
cleanData$lnIntSpreadToTreasury[which(cleanData$lnIntSpreadToTreasury==-Inf | cleanData$lnIntSpreadToTreasury==Inf)]= NA
cleanData$lnAmountOut[which(cleanData$lnAmountOut==-Inf | cleanData$lnAmountOut==Inf)]= NA
cleanData$IssueYearsToMaturity[which(cleanData$IssueYearsToMaturity==-Inf | cleanData$IssueYearsToMaturity==Inf)]= NA
cleanData$YearsToMaturity[which(cleanData$YearsToMaturity==-Inf | cleanData$YearsToMaturity==Inf)]= NA
cleanData$Put_Flag[which(cleanData$Put_Flag==-Inf | cleanData$Put_Flag==Inf)]= NA
cleanData$Call_Flag[which(cleanData$Call_Flag==-Inf | cleanData$Call_Flag==Inf)]= NA
cleanData$AltmanZ[which(cleanData$AltmanZ==-Inf | cleanData$AltmanZ==Inf)]= NA
cleanData$Volatility[which(cleanData$Volatility==-Inf | cleanData$Volatility==Inf)]= NA
cleanData$Leverage[which(cleanData$Leverage==-Inf | cleanData$Leverage==Inf)]= NA
cleanData$InterestCoverage[which(cleanData$InterestCoverage==-Inf | cleanData$InterestCoverage==Inf)]= NA
cleanData$Loss[which(cleanData$Loss==-Inf | cleanData$Loss==Inf)]= NA
cleanData$ROA[which(cleanData$ROA==-Inf | cleanData$ROA==Inf)]= NA
cleanData$Size[which(cleanData$Size==-Inf | cleanData$Size==Inf)]= NA
cleanData$WC[which(cleanData$WC==-Inf | cleanData$WC==Inf)]= NA
cleanData$TobinsQ[which(cleanData$TobinsQ==-Inf | cleanData$TobinsQ==Inf)]= NA


cleanData <-cleanData[!is.na(cleanData$ESGOverallStd),]
cleanData <-cleanData[!is.na(cleanData$E),]
cleanData <-cleanData[!is.na(cleanData$S),]
cleanData <-cleanData[!is.na(cleanData$G),]
cleanData <-cleanData[!is.na(cleanData$lnIssueIntSpreadToTreasury),]
cleanData <-cleanData[!is.na(cleanData$lnIssueAmount),]
cleanData$IssueYearsToMaturity <- ifelse(cleanData$IssueYearsToMaturity >50 , NA , ifelse(cleanData$IssueYearsToMaturity <0 , NA , cleanData$IssueYearsToMaturity))
cleanData <-cleanData[!is.na(cleanData$IssueYearsToMaturity),]

cleanData <-cleanData[!is.na(cleanData$lnIntSpreadToTreasury),]
cleanData <-cleanData[!is.na(cleanData$lnAmountOut),]
cleanData$YearsToMaturity <- ifelse(cleanData$YearsToMaturity >50 , NA , ifelse(cleanData$YearsToMaturity <0 , NA , cleanData$YearsToMaturity))
cleanData <-cleanData[!is.na(cleanData$YearsToMaturity),]
cleanData <-cleanData[!is.na(cleanData$NAICS),]
cleanData <-cleanData[!is.na(cleanData$Year),]
cleanData <-cleanData[!is.na(cleanData$Call_Flag),]
cleanData <-cleanData[!is.na(cleanData$AltmanZ),]
cleanData <-cleanData[!is.na(cleanData$TobinsQ),]
cleanData <-cleanData[!is.na(cleanData$Volatility),]
cleanData <-cleanData[!is.na(cleanData$Leverage),]
cleanData <-cleanData[!is.na(cleanData$InterestCoverage),]
cleanData <-cleanData[!is.na(cleanData$Loss),]
cleanData <-cleanData[!is.na(cleanData$ROA),]
cleanData <-cleanData[!is.na(cleanData$Size),]
cleanData <-cleanData[!is.na(cleanData$WC),]
cleanData <-cleanData[!is.na(cleanData$Index),]


#Safe Dataset Before Truncation
cleanDatabeforetrim <- cleanData


#Distribution And Truncation
hist(cleanData$TobinsQ,
     xlab = "TobinsQ",
     main = "TobinsQ",
     breaks = sqrt(nrow(cleanData))
) 
quantile(cleanData$TobinsQ, probs = seq(0, 1, 1/100)) 
cleanData$TobinsQ <- ifelse(cleanData$TobinsQ >8.2499650, NA, ifelse(cleanData$TobinsQ <0.865813, NA, cleanData$TobinsQ))
#cleanData <-cleanData[!is.na(cleanData$TobinsQ),]


#Distribution And Truncation
hist(cleanData$Volatility,
     xlab = "Volatility",
     main = "Volatility",
     breaks = sqrt(nrow(cleanData))
) 
quantile(cleanData$Volatility, probs = seq(0, 1, 1/100)) 
cleanData$Volatility <- ifelse(cleanData$Volatility >1.836086e+01 , NA, ifelse(cleanData$Volatility <3.662666e+00, NA, cleanData$Volatility))
#cleanData <-cleanData[!is.na(cleanData$Volatility),]


#Distribution And Truncation
hist(cleanData$Leverage,
     xlab = "Leverage",
     main = "Leverage",
     breaks = sqrt(nrow(cleanData))
) 
quantile(cleanData$Leverage, probs = seq(0, 1, 1/100)) 
cleanData$Leverage <- ifelse(cleanData$Leverage >1.2104045 , NA, ifelse(cleanData$Leverage <0.3668782, NA, cleanData$Leverage))
#cleanData <-cleanData[!is.na(cleanData$Leverage),]


#Distribution And Truncation
hist(cleanData$InterestCoverage,
     xlab = "InterestCoverage",
     main = "InterestCoverage",
     breaks = sqrt(nrow(cleanData))
) 
quantile(cleanData$InterestCoverage, probs = seq(0, 1, 1/100)) 
cleanData$InterestCoverage <- ifelse(cleanData$InterestCoverage >1.062133e+02 , NA, ifelse(cleanData$InterestCoverage <(-2.417808e+00), NA, cleanData$InterestCoverage))
#cleanData <-cleanData[!is.na(cleanData$InterestCoverage),]


#Distribution And Truncation
hist(cleanData$ROA,
     xlab = "ROA",
     main = "ROA",
     breaks = sqrt(nrow(cleanData))
)
quantile(cleanData$ROA, probs = seq(0, 1, 1/100)) 
cleanData$ROA <- ifelse(cleanData$ROA >0.0726016263  , NA, ifelse(cleanData$ROA <(-0.0530751138), NA, cleanData$ROA))
#cleanData <-cleanData[!is.na(cleanData$ROA),]


#Distribution And Truncation
hist(cleanData$WC,
     xlab = "WC",
     main = "WC",
     breaks = sqrt(nrow(cleanData))
) 
quantile(cleanData$WC, probs = seq(0, 1, 1/100)) 
cleanData$WC <- ifelse(cleanData$WC >0.5074887858   , NA, ifelse(cleanData$WC <(-0.1190026645), NA, cleanData$WC))
#cleanData <-cleanData[!is.na(cleanData$WC),]


#Clean Dataset
cleanData <-cleanData[!is.na(cleanData$TobinsQ),]
cleanData <-cleanData[!is.na(cleanData$Volatility),]
cleanData <-cleanData[!is.na(cleanData$Leverage),]
cleanData <-cleanData[!is.na(cleanData$InterestCoverage),]
cleanData <-cleanData[!is.na(cleanData$ROA),]
cleanData <-cleanData[!is.na(cleanData$WC),]


##################################################################################


#ESG Quantile
summary(cleanData$ESGOverallStdQuant)
cleanData$ESGOverallStdQuant <- ntile(cleanData$ESGOverallStd, 4) 
cleanData$ESGOverallStdQuant <- ifelse(cleanData$ESGOverallStdQuant ==4, 'Quant4', ifelse(cleanData$ESGOverallStdQuant ==3, 'Quant3', ifelse(cleanData$ESGOverallStdQuant==2, 'Quant2', ifelse(cleanData$ESGOverallStdQuant==1,'Quant1', NA))))
cleanData$ESGOverallStdQuant <- as.factor(cleanData$ESGOverallStdQuant)  
summary(cleanData$ESGOverallStdQuant)


summary(cleanData$LagESGOverallStd)
cleanData$LagESGOverallStdQuant <- ntile(cleanData$LagESGOverallStd, 4) 
cleanData$LagESGOverallStdQuant <- ifelse(cleanData$LagESGOverallStdQuant ==4, 'Quant4', ifelse(cleanData$LagESGOverallStdQuant ==3, 'Quant3', ifelse(cleanData$LagESGOverallStdQuant==2, 'Quant2', ifelse(cleanData$LagESGOverallStdQuant==1,'Quant1', NA))))
cleanData$LagESGOverallStdQuant <- as.factor(cleanData$LagESGOverallStdQuant)  
summary(cleanData$LagESGOverallStdQuant)


###########################################################


#ESG Above Median Or Below
cleanData$ESGOverallStdMed <- ifelse(cleanData$ESGOverallStd >= median(cleanData$ESGOverallStd), 'AboveMed' , 'BelowMed')
cleanData$ESGOverallStdMed <- as.factor(cleanData$ESGOverallStdMed)


cleanData$LagESGOverallStdMed <- ifelse(cleanData$LagESGOverallStd >= median(cleanData$LagESGOverallStd), 'AboveMed' , 'BelowMed')
cleanData$LagESGOverallStdMed <- as.factor(cleanData$LagESGOverallStdMed)


#ESG Above Mean or Below
cleanData$ESGOverallStdMn <- ifelse(cleanData$ESGOverallStd >= mean(cleanData$ESGOverallStd), 'AboveMn' , 'BelowMn')
cleanData$ESGOverallStdMn <- as.factor(cleanData$ESGOverallStdMn)


cleanData$LagESGOverallStdMn <- ifelse(cleanData$LagESGOverallStd >= mean(cleanData$LagESGOverallStd), 'AboveMn' , 'BelowMn')
cleanData$LagESGOverallStdMn <- as.factor(cleanData$LagESGOverallStdMn)


###########################################################


#E Quartiles
summary(cleanData$E)
cleanData$EQuant <- ntile(cleanData$E, 4) 
cleanData$EQuant <- ifelse(cleanData$EQuant ==4, 'Quant4', ifelse(cleanData$EQuant ==3, 'Quant3', ifelse(cleanData$EQuant==2, 'Quant2', ifelse(cleanData$EQuant==1,'Quant1', NA))))
cleanData$EQuant <- as.factor(cleanData$EQuant)  
summary(cleanData$EQuant)


summary(cleanData$LagE)
cleanData$LagEQuant <- ntile(cleanData$LagE, 4) 
cleanData$LagEQuant <- ifelse(cleanData$LagEQuant ==4, 'Quant4', ifelse(cleanData$LagEQuant ==3, 'Quant3', ifelse(cleanData$LagEQuant==2, 'Quant2', ifelse(cleanData$LagEQuant==1,'Quant1', NA))))
cleanData$LagEQuant <- as.factor(cleanData$LagEQuant)  
summary(cleanData$LagEQuant)


###########################################################


#E Above Mean or Below
cleanData$EMn <- ifelse(cleanData$E >=mean(cleanData$E) , 'AboveMn' , 'BelowMn')
cleanData$EMn <- as.factor(cleanData$EMn)
cleanData$LagEMn <- ifelse(cleanData$LagE >=mean(cleanData$LagE) , 'AboveMn' , 'BelowMn')
cleanData$LagEMn <- as.factor(cleanData$LagEMn)


cleanData$EMed <- ifelse(cleanData$E >=median(cleanData$E) , 'AboveMed' , 'BelowMed')
cleanData$EMed <- as.factor(cleanData$EMed)
cleanData$LagEMed <- ifelse(cleanData$LagE >=median(cleanData$LagE) , 'AboveMed' , 'BelowMed')
cleanData$LagEMed <- as.factor(cleanData$LagEMed)


###########################################################


#S Quartiles
summary(cleanData$S)
cleanData$SQuant <- ntile(cleanData$S, 4) 
cleanData$SQuant <- ifelse(cleanData$SQuant ==4, 'Quant4', ifelse(cleanData$SQuant ==3, 'Quant3', ifelse(cleanData$SQuant==2, 'Quant2', ifelse(cleanData$SQuant==1,'Quant1', NA))))
cleanData$SQuant <- as.factor(cleanData$SQuant) 
summary(cleanData$SQuant)


summary(cleanData$LagS)
cleanData$LagSQuant <- ntile(cleanData$LagS, 4) 
cleanData$LagSQuant <- ifelse(cleanData$LagSQuant ==4, 'Quant4', ifelse(cleanData$LagSQuant ==3, 'Quant3', ifelse(cleanData$LagSQuant==2, 'Quant2', ifelse(cleanData$LagSQuant==1,'Quant1', NA))))
cleanData$LagSQuant <- as.factor(cleanData$LagSQuant) 
summary(cleanData$LagSQuant)


###########################################################


#S Above Mean or Below
cleanData$SMn <- ifelse(cleanData$S >=mean(cleanData$S) , 'AboveMn' , 'BelowMn')
cleanData$SMn <- as.factor(cleanData$SMn)
cleanData$LagSMn <- ifelse(cleanData$LagS >=mean(cleanData$LagS) , 'AboveMn' , 'BelowMn')
cleanData$LagSMn <- as.factor(cleanData$LagSMn)


cleanData$SMed <- ifelse(cleanData$S >=median(cleanData$S) , 'AboveMed' , 'BelowMed')
cleanData$SMed <- as.factor(cleanData$SMed)
cleanData$LagSMed <- ifelse(cleanData$LagS >=median(cleanData$LagS) , 'AboveMed' , 'BelowMed')
cleanData$LagSMed <- as.factor(cleanData$LagSMed)


###########################################################


#G Quartiles
summary(cleanData$G)
cleanData$GQuant <- ntile(cleanData$G, 4) 
cleanData$GQuant <- ifelse(cleanData$GQuant ==4, 'Quant4', ifelse(cleanData$GQuant ==3, 'Quant3', ifelse(cleanData$GQuant==2, 'Quant2', ifelse(cleanData$GQuant==1,'Quant1', NA))))
cleanData$GQuant <- as.factor(cleanData$GQuant) 
summary(cleanData$GQuant)


summary(cleanData$LagG)
cleanData$LagGQuant <- ntile(cleanData$LagG, 4) 
cleanData$LagGQuant <- ifelse(cleanData$LagGQuant ==4, 'Quant4', ifelse(cleanData$LagGQuant ==3, 'Quant3', ifelse(cleanData$LagGQuant==2, 'Quant2', ifelse(cleanData$LagGQuant==1,'Quant1', NA))))
cleanData$LagGQuant <- as.factor(cleanData$LagGQuant) 
summary(cleanData$LagGQuant)


###########################################################


#G Above Mean or Below
cleanData$GMn <- ifelse(cleanData$G >=mean(cleanData$G) , 'AboveMn' , 'BelowMn')
cleanData$GMn <- as.factor(cleanData$GMn)
cleanData$LagGMn <- ifelse(cleanData$LagG >=mean(cleanData$LagG) , 'AboveMn' , 'BelowMn')
cleanData$LagGMn <- as.factor(cleanData$LagGMn)


cleanData$GMed <- ifelse(cleanData$G >=median(cleanData$G) , 'AboveMed' , 'BelowMed')
cleanData$GMed <- as.factor(cleanData$GMed)
cleanData$LagGMed <- ifelse(cleanData$LagG >=median(cleanData$LagG) , 'AboveMed' , 'BelowMed')
cleanData$LagGMed <- as.factor(cleanData$LagGMed)


###########################################################


#Create Issue Dataset
cleanDataIssue <- cleanData
cleanDataIssue <- cleanDataIssue  %>% 
  select(-'AmountOut') %>%  
  select(-'IntSpreadToTreasury') %>% 
  select(-'Issue_Name') %>% 
  select(-'Issue_Date') %>% 
  select(-'MaturityDate')%>%  
  select(-'InterestExpense')%>%  
  select(-'NetIncome')%>%  
  select(-'TotalLiabilities')%>%  
  select(-'Sales')%>%  
  select(-'RetainedEarnings')%>%  
  select(-'EBIT')%>%  
  select(-'D&A') %>%  
  select(-'lnIntSpreadToTreasury')%>%
  select(-'lnAmountOut')%>%  
  select(-'QuarterDate')%>%  
  select(-'DaysToMaturity')%>%  
  select(-'IssueDaysToMaturity')%>%  
  select(-'EBITDA') 
cleanDataIssue <-cleanDataIssue[!is.na(cleanDataIssue$IssueQuarter),]

##################################################################################


#Count Firms
length(unique(cleanDataIssue$CUSIP))


##################################################################################


#Count Industries
summary(cleanDataIssue$NAICS)


#AccommodationandFoodServices 
52/4214*100
#AdministrativeandSupportandWasteManagementandRemediation 
65/4214*100
#AgricultureForestryFishingandHunting                           
8/4214*100
#ArtsEntertainmentandRecreation 
63/4214*100
#Construction                            
35/4214*100  
#HealthCareandSocialAssistance 
60/4214*100 
#Information                 
727/4214*100            
#ManagementofCompaniesandEnterprises
4/4214*100 
#Manufacturing                    
1672/4214*100               
#MiningQuarryingandOilandGasExtraction 
196/4214*100 
#ProfessionalScientificandTechnicalServices                           
79/4214*100    
#RealEstateandRentalandLeasing 
6/4214*100 
#RetailTrade                            
231/4214*100                    
#TransportationandWarehousing 
260/4214*100 
#Utilities        
700/4214*100  
#WholesaleTrade 
56/4214*100 


##################################################################################


#Create Yearly Data Set
#Issue
cleanDataIssue2014 <- cleanDataIssue
cleanDataIssue2014$Year <- ifelse(cleanDataIssue2014$Year == '2014', cleanDataIssue2014$Year, NA)
cleanDataIssue2014 <-cleanDataIssue2014[!is.na(cleanDataIssue2014$Year),]
cleanDataIssue2014 <-cleanDataIssue2014[!is.na(cleanDataIssue2014$IssueQuarter),]
summary(cleanDataIssue2014$NAICS)


#AccommodationandFoodServices 
1/379*100
#AdministrativeandSupportandWasteManagementandRemediation 
6/379*100
#AgricultureForestryFishingandHunting                           
0/379*100
#ArtsEntertainmentandRecreation 
9/379*100
#Construction                            
2/379*100 
#HealthCareandSocialAssistance 
5/379*100
#Information                 
59/379*100           
#ManagementofCompaniesandEnterprises
4/379*100
#Manufacturing                    
129/379*100             
#MiningQuarryingandOilandGasExtraction 
17/379*100
#ProfessionalScientificandTechnicalServices                           
10/379*100  
#RealEstateandRentalandLeasing 
3/379*100
#RetailTrade                            
20/379*100                 
#TransportationandWarehousing 
44/379*100 
#Utilities        
59/379*100 
#WholesaleTrade 
11/379*100


cleanDataIssue2015 <- cleanDataIssue
cleanDataIssue2015$Year <- ifelse(cleanDataIssue2015$Year == '2015', cleanDataIssue2015$Year, NA)
cleanDataIssue2015 <-cleanDataIssue2015[!is.na(cleanDataIssue2015$Year),]
cleanDataIssue2015 <-cleanDataIssue2015[!is.na(cleanDataIssue2015$IssueQuarter),]
summary(cleanDataIssue2015$NAICS)


#AccommodationandFoodServices 
4/503*100
#AdministrativeandSupportandWasteManagementandRemediation 
5/503*100
#AgricultureForestryFishingandHunting                           
0/503*100
#ArtsEntertainmentandRecreation 
32/503*100
#Construction                            
3/503*100 
#HealthCareandSocialAssistance 
20/503*100
#Information                 
49/503*100           
#ManagementofCompaniesandEnterprises
0/503*100
#Manufacturing                    
221/503*100             
#MiningQuarryingandOilandGasExtraction 
21/503*100
#ProfessionalScientificandTechnicalServices                           
9/503*100  
#RealEstateandRentalandLeasing 
0/503*100
#RetailTrade                            
23/503*100                 
#TransportationandWarehousing 
38/503*100 
#Utilities        
77/503*100 
#WholesaleTrade 
11/503*100


cleanDataIssue2016 <- cleanDataIssue
cleanDataIssue2016$Year <- ifelse(cleanDataIssue2016$Year == '2016', cleanDataIssue2016$Year, NA)
cleanDataIssue2016 <-cleanDataIssue2016[!is.na(cleanDataIssue2016$Year),]
cleanDataIssue2016 <-cleanDataIssue2016[!is.na(cleanDataIssue2016$IssueQuarter),]
summary(cleanDataIssue2016$NAICS)


#AccommodationandFoodServices 
12/469*100
#AdministrativeandSupportandWasteManagementandRemediation 
6/469*100
#AgricultureForestryFishingandHunting                           
0/469*100
#ArtsEntertainmentandRecreation 
6/469*100
#Construction                            
3/469*100 
#HealthCareandSocialAssistance 
3/469*100
#Information                 
64/469*100           
#ManagementofCompaniesandEnterprises
0/469*100
#Manufacturing                    
191/469*100             
#MiningQuarryingandOilandGasExtraction 
30/469*100
#ProfessionalScientificandTechnicalServices                           
17/469*100  
#RealEstateandRentalandLeasing 
0/469*100
#RetailTrade                            
22/469*100                 
#TransportationandWarehousing 
18/469*100 
#Utilities        
91/469*100 
#WholesaleTrade 
6/469*100


cleanDataIssue2017 <- cleanDataIssue
cleanDataIssue2017$Year <- ifelse(cleanDataIssue2017$Year == '2017', cleanDataIssue2017$Year, NA)
cleanDataIssue2017 <-cleanDataIssue2017[!is.na(cleanDataIssue2017$Year),]
cleanDataIssue2017 <-cleanDataIssue2017[!is.na(cleanDataIssue2017$IssueQuarter),]
summary(cleanDataIssue2017$NAICS)


#AccommodationandFoodServices 
8/506*100
#AdministrativeandSupportandWasteManagementandRemediation 
4/506*100
#AgricultureForestryFishingandHunting                           
0/506*100
#ArtsEntertainmentandRecreation 
5/506*100
#Construction                            
3/506*100 
#HealthCareandSocialAssistance 
3/506*100
#Information                 
100/506*100           
#ManagementofCompaniesandEnterprises
0/506*100
#Manufacturing                    
187/506*100             
#MiningQuarryingandOilandGasExtraction 
20/506*100
#ProfessionalScientificandTechnicalServices                           
6/506*100  
#RealEstateandRentalandLeasing 
0/506*100
#RetailTrade                            
41/506*100                 
#TransportationandWarehousing 
31/506*100 
#Utilities        
89/506*100 
#WholesaleTrade 
9/506*100


cleanDataIssue2018 <- cleanDataIssue
cleanDataIssue2018$Year <- ifelse(cleanDataIssue2018$Year == '2018', cleanDataIssue2018$Year, NA)
cleanDataIssue2018 <-cleanDataIssue2018[!is.na(cleanDataIssue2018$Year),]
cleanDataIssue2018 <-cleanDataIssue2018[!is.na(cleanDataIssue2018$IssueQuarter),]
summary(cleanDataIssue2018$NAICS)


#AccommodationandFoodServices 
9/432*100
#AdministrativeandSupportandWasteManagementandRemediation 
10/432*100
#AgricultureForestryFishingandHunting                           
6/432*100
#ArtsEntertainmentandRecreation 
2/432*100
#Construction                            
1/432*100 
#HealthCareandSocialAssistance 
3/432*100
#Information                 
103/432*100           
#ManagementofCompaniesandEnterprises
0/432*100
#Manufacturing                    
118/432*100             
#MiningQuarryingandOilandGasExtraction 
5/432*100
#ProfessionalScientificandTechnicalServices                           
6/432*100  
#RealEstateandRentalandLeasing 
3/432*100
#RetailTrade                            
30/432*100                 
#TransportationandWarehousing 
33/432*100 
#Utilities        
94/432*100 
#WholesaleTrade 
9/432*100


cleanDataIssue2019 <- cleanDataIssue
cleanDataIssue2019$Year <- ifelse(cleanDataIssue2019$Year == '2019', cleanDataIssue2019$Year, NA)
cleanDataIssue2019 <-cleanDataIssue2019[!is.na(cleanDataIssue2019$Year),]
cleanDataIssue2019 <-cleanDataIssue2019[!is.na(cleanDataIssue2019$IssueQuarter),]
summary(cleanDataIssue2019$NAICS)


#AccommodationandFoodServices 
9/674*100
#AdministrativeandSupportandWasteManagementandRemediation 
6/674*100
#AgricultureForestryFishingandHunting                           
0/674*100
#ArtsEntertainmentandRecreation 
7/674*100
#Construction                            
4/674*100 
#HealthCareandSocialAssistance 
11/674*100
#Information                 
167/674*100           
#ManagementofCompaniesandEnterprises
0/674*100
#Manufacturing                    
260/674*100             
#MiningQuarryingandOilandGasExtraction 
40/674*100
#ProfessionalScientificandTechnicalServices                           
10/674*100  
#RealEstateandRentalandLeasing 
0/674*100
#RetailTrade                            
20/674*100                 
#TransportationandWarehousing 
33/674*100 
#Utilities        
106/674*100 
#WholesaleTrade 
1/674*100


cleanDataIssue2020 <- cleanDataIssue
cleanDataIssue2020$Year <- ifelse(cleanDataIssue2020$Year == '2020', cleanDataIssue2020$Year, NA)
cleanDataIssue2020 <-cleanDataIssue2020[!is.na(cleanDataIssue2020$Year),]
cleanDataIssue2020 <-cleanDataIssue2020[!is.na(cleanDataIssue2020$IssueQuarter),]
summary(cleanDataIssue2020$NAICS)


#AccommodationandFoodServices 
9/837*100
#AdministrativeandSupportandWasteManagementandRemediation 
16/837*100
#AgricultureForestryFishingandHunting                           
1/837*100
#ArtsEntertainmentandRecreation 
0/837*100
#Construction                            
13/837*100 
#HealthCareandSocialAssistance 
11/837*100
#Information                 
109/837*100           
#ManagementofCompaniesandEnterprises
0/837*100
#Manufacturing                    
424/837*100             
#MiningQuarryingandOilandGasExtraction 
39/837*100
#ProfessionalScientificandTechnicalServices                           
17/837*100  
#RealEstateandRentalandLeasing 
0/837*100
#RetailTrade                            
48/837*100                 
#TransportationandWarehousing 
35/837*100 
#Utilities        
111/837*100 
#WholesaleTrade 
4/837*100


cleanDataIssue2021 <- cleanDataIssue
cleanDataIssue2021$Year <- ifelse(cleanDataIssue2021$Year == '2021', cleanDataIssue2021$Year, NA)
cleanDataIssue2021 <-cleanDataIssue2021[!is.na(cleanDataIssue2021$Year),]
cleanDataIssue2021 <-cleanDataIssue2021[!is.na(cleanDataIssue2021$IssueQuarter),]
summary(cleanDataIssue2021$NAICS)


#AccommodationandFoodServices 
0/414*100
#AdministrativeandSupportandWasteManagementandRemediation 
12/414*100
#AgricultureForestryFishingandHunting                           
1/414*100
#ArtsEntertainmentandRecreation 
2/414*100
#Construction                            
6/414*100 
#HealthCareandSocialAssistance 
4/414*100
#Information                 
76/414*100           
#ManagementofCompaniesandEnterprises
0/414*100
#Manufacturing                    
152/414*100             
#MiningQuarryingandOilandGasExtraction 
24/414*100
#ProfessionalScientificandTechnicalServices                           
4/414*100  
#RealEstateandRentalandLeasing 
0/414*100
#RetailTrade                            
27/414*100                 
#TransportationandWarehousing 
28/414*100 
#Utilities        
73/414*100 
#WholesaleTrade 
5/414*100


#########################################################################################


#Summary Statistics
summary(cleanDataIssue$IssueIntSpreadToTreasury)
sd(cleanDataIssue$IssueIntSpreadToTreasury, na.rm = TRUE)
summary(cleanDataIssue$IssueAmount)
sd(cleanDataIssue$IssueAmount, na.rm = TRUE)
summary(cleanDataIssue$IssueYearsToMaturity)
sd(cleanDataIssue$IssueYearsToMaturity, na.rm = TRUE)
summary(cleanDataIssue$Call_Flag)
sd(cleanDataIssue$Call_Flag, na.rm = TRUE)
summary(cleanDataIssue$Put_Flag)
sd(cleanDataIssue$Put_Flag, na.rm = TRUE)
summary(cleanDataIssue$ESGOverallStd)
sd(cleanDataIssue$ESGOverallStd, na.rm = TRUE)
summary(cleanDataIssue$E)
sd(cleanDataIssue$E, na.rm = TRUE)
summary(cleanDataIssue$S)
sd(cleanDataIssue$S, na.rm = TRUE)
summary(cleanDataIssue$G)
sd(cleanDataIssue$G, na.rm = TRUE)
summary(cleanDataIssue$SafeZone)
sd(cleanDataIssue$SafeZone, na.rm = TRUE)
summary(cleanDataIssue$GreyZone)
sd(cleanDataIssue$GreyZone, na.rm = TRUE)
summary(cleanDataIssue$DistressZone)
sd(cleanDataIssue$DistressZone, na.rm = TRUE)
summary(cleanDataIssue$TobinsQ)
sd(cleanDataIssue$TobinsQ, na.rm = TRUE)
summary(cleanDataIssue$Volatility)
sd(cleanDataIssue$Volatility, na.rm = TRUE)
summary(cleanDataIssue$Leverage)
sd(cleanDataIssue$Leverage, na.rm = TRUE)
summary(cleanDataIssue$InterestCoverage)
sd(cleanDataIssue$InterestCoverage, na.rm = TRUE)
summary(cleanDataIssue$Loss)
sd(cleanDataIssue$Loss, na.rm = TRUE)
summary(cleanDataIssue$ROA)
sd(cleanDataIssue$ROA, na.rm = TRUE)
summary(cleanDataIssue$TotalAssets)
sd(cleanDataIssue$TotalAssets, na.rm = TRUE)
summary(cleanDataIssue$WC)
sd(cleanDataIssue$WC, na.rm = TRUE)
summary(cleanDataIssue$Europe)
sd(cleanDataIssue$Europe, na.rm = TRUE)
summary(cleanDataIssue$US)
sd(cleanDataIssue$US, na.rm = TRUE)
summary(cleanData$Index)


###########################################################


#ESG Quantile
summary(cleanDataIssue$ESGOverallStd)
cleanDataIssue$ESGOverallStdQuant <- ntile(cleanDataIssue$ESGOverallStd, 4) 
cleanDataIssue$ESGOverallStdQuant <- ifelse(cleanDataIssue$ESGOverallStdQuant ==4, 'Quant4', ifelse(cleanDataIssue$ESGOverallStdQuant ==3, 'Quant3', ifelse(cleanDataIssue$ESGOverallStdQuant==2, 'Quant2', ifelse(cleanDataIssue$ESGOverallStdQuant==1,'Quant1', NA))))
cleanDataIssue$ESGOverallStdQuant <- as.factor(cleanDataIssue$ESGOverallStdQuant) 
summary(cleanDataIssue$ESGOverallStdQuant)


summary(cleanDataIssue$LagESGOverallStd)
cleanDataIssue$LagESGOverallStdQuant <- ntile(cleanDataIssue$LagESGOverallStd, 4) 
cleanDataIssue$LagESGOverallStdQuant <- ifelse(cleanDataIssue$LagESGOverallStdQuant ==4, 'Quant4', ifelse(cleanDataIssue$LagESGOverallStdQuant ==3, 'Quant3', ifelse(cleanDataIssue$LagESGOverallStdQuant==2, 'Quant2', ifelse(cleanDataIssue$LagESGOverallStdQuant==1,'Quant1', NA))))
cleanDataIssue$LagESGOverallStdQuant <- as.factor(cleanDataIssue$LagESGOverallStdQuant) 
summary(cleanDataIssue$LagESGOverallStdQuant)


###########################################################


#ESG Above Median Or Below
cleanDataIssue$ESGOverallStdMed <- ifelse(cleanDataIssue$ESGOverallStd >= median(cleanDataIssue$ESGOverallStd), 'AboveMed' , 'BelowMed')
cleanDataIssue$ESGOverallStdMed <- as.factor(cleanDataIssue$ESGOverallStdMed)


cleanDataIssue$LagESGOverallStdMed <- ifelse(cleanDataIssue$LagESGOverallStd >= median(cleanDataIssue$LagESGOverallStd), 'AboveMed' , 'BelowMed')
cleanDataIssue$LagESGOverallStdMed <- as.factor(cleanDataIssue$LagESGOverallStdMed)


#ESG Above Mean or Below
cleanDataIssue$ESGOverallStdMn <- ifelse(cleanDataIssue$ESGOverallStd >= mean(cleanDataIssue$ESGOverallStd), 'AboveMn' , 'BelowMn')
cleanDataIssue$ESGOverallStdMn <- as.factor(cleanDataIssue$ESGOverallStdMn)


cleanDataIssue$LagESGOverallStdMn <- ifelse(cleanDataIssue$LagESGOverallStd >= mean(cleanDataIssue$LagESGOverallStd), 'AboveMn' , 'BelowMn')
cleanDataIssue$LagESGOverallStdMn <- as.factor(cleanDataIssue$LagESGOverallStdMn)


###########################################################


#E Quartiles
summary(cleanDataIssue$E)
cleanDataIssue$EQuant <-  ntile(cleanDataIssue$E, 4) 
cleanDataIssue$EQuant <- ifelse(cleanDataIssue$EQuant ==4, 'Quant4', ifelse(cleanDataIssue$EQuant ==3, 'Quant3', ifelse(cleanDataIssue$EQuant==2, 'Quant2', ifelse(cleanDataIssue$EQuant==1,'Quant1', NA))))
cleanDataIssue$EQuant <- as.factor(cleanDataIssue$EQuant)
summary(cleanDataIssue$EQuant)


summary(cleanDataIssue$LagE)
cleanDataIssue$LagEQuant <-  ntile(cleanDataIssue$LagE, 4) 
cleanDataIssue$LagEQuant <- ifelse(cleanDataIssue$LagEQuant ==4, 'Quant4', ifelse(cleanDataIssue$LagEQuant ==3, 'Quant3', ifelse(cleanDataIssue$LagEQuant==2, 'Quant2', ifelse(cleanDataIssue$LagEQuant==1,'Quant1', NA))))
cleanDataIssue$LagEQuant <- as.factor(cleanDataIssue$LagEQuant)
summary(cleanDataIssue$LagEQuant)


###########################################################


#E Above Mean or Below
cleanDataIssue$EMn <- ifelse(cleanDataIssue$E >=mean(cleanDataIssue$E) , 'AboveMn' , 'BelowMn')
cleanDataIssue$EMn <- as.factor(cleanDataIssue$EMn)
cleanDataIssue$LagEMn <- ifelse(cleanDataIssue$LagE >=mean(cleanDataIssue$LagE) , 'AboveMn' , 'BelowMn')
cleanDataIssue$LagEMn <- as.factor(cleanDataIssue$LagEMn)
cleanDataIssue$Lag4EMn <- ifelse(cleanDataIssue$Lag4E >=mean(cleanDataIssue$Lag4E) , 'AboveMn' , 'BelowMn')
cleanDataIssue$Lag4EMn <- as.factor(cleanDataIssue$Lag4EMn)


cleanDataIssue$EMed <- ifelse(cleanDataIssue$E >=median(cleanDataIssue$E) , 'AboveMed' , 'BelowMed')
cleanDataIssue$EMed <- as.factor(cleanDataIssue$EMed)
cleanDataIssue$LagEMed <- ifelse(cleanDataIssue$LagE >=median(cleanDataIssue$LagE) , 'AboveMed' , 'BelowMed')
cleanDataIssue$LagEMed <- as.factor(cleanDataIssue$LagEMed)


###########################################################


#S Quartiles
summary(cleanDataIssue$S)
cleanDataIssue$SQuant <-  ntile(cleanDataIssue$S, 4) 
cleanDataIssue$SQuant <- ifelse(cleanDataIssue$SQuant ==4, 'Quant4', ifelse(cleanDataIssue$SQuant ==3, 'Quant3', ifelse(cleanDataIssue$SQuant==2, 'Quant2', ifelse(cleanDataIssue$SQuant==1,'Quant1', NA))))
cleanDataIssue$SQuant <- as.factor(cleanDataIssue$SQuant)
summary(cleanDataIssue$SQuant)


summary(cleanDataIssue$LagS)
cleanDataIssue$LagSQuant <-  ntile(cleanDataIssue$LagS, 4) 
cleanDataIssue$LagSQuant <- ifelse(cleanDataIssue$LagSQuant ==4, 'Quant4', ifelse(cleanDataIssue$LagSQuant ==3, 'Quant3', ifelse(cleanDataIssue$LagSQuant==2, 'Quant2', ifelse(cleanDataIssue$LagSQuant==1,'Quant1', NA))))
cleanDataIssue$LagSQuant <- as.factor(cleanDataIssue$LagSQuant)
summary(cleanDataIssue$LagSQuant)


###########################################################


#S Above Mean or Below
cleanDataIssue$SMn <- ifelse(cleanDataIssue$S >=mean(cleanDataIssue$S) , 'AboveMn' , 'BelowMn')
cleanDataIssue$SMn <- as.factor(cleanDataIssue$SMn)
cleanDataIssue$LagSMn <- ifelse(cleanDataIssue$LagS >=mean(cleanDataIssue$LagS) , 'AboveMn' , 'BelowMn')
cleanDataIssue$LagSMn <- as.factor(cleanDataIssue$LagSMn)


cleanDataIssue$SMed <- ifelse(cleanDataIssue$S >=median(cleanDataIssue$S) , 'AboveMed' , 'BelowMed')
cleanDataIssue$SMed <- as.factor(cleanDataIssue$SMed)
cleanDataIssue$LagSMed <- ifelse(cleanDataIssue$LagS >=median(cleanDataIssue$LagS) , 'AboveMed' , 'BelowMed')
cleanDataIssue$LagSMed <- as.factor(cleanDataIssue$LagSMed)

###########################################################


#G Quartiles
summary(cleanDataIssue$G)
cleanDataIssue$GQuant <-  ntile(cleanDataIssue$G, 4) 
cleanDataIssue$GQuant <- ifelse(cleanDataIssue$GQuant ==4, 'Quant4', ifelse(cleanDataIssue$GQuant ==3, 'Quant3', ifelse(cleanDataIssue$GQuant==2, 'Quant2', ifelse(cleanDataIssue$GQuant==1,'Quant1', NA))))
cleanDataIssue$GQuant <- as.factor(cleanDataIssue$GQuant)
summary(cleanDataIssue$GQuant)


summary(cleanDataIssue$LagG)
cleanDataIssue$LagGQuant <-  ntile(cleanDataIssue$LagG, 4) 
cleanDataIssue$LagGQuant <- ifelse(cleanDataIssue$LagGQuant ==4, 'Quant4', ifelse(cleanDataIssue$LagGQuant ==3, 'Quant3', ifelse(cleanDataIssue$LagGQuant==2, 'Quant2', ifelse(cleanDataIssue$LagGQuant==1,'Quant1', NA))))
cleanDataIssue$LagGQuant <- as.factor(cleanDataIssue$LagGQuant)
summary(cleanDataIssue$LagGQuant)


###########################################################


#G Above Mean or Below
cleanDataIssue$GMn <- ifelse(cleanDataIssue$G >=mean(cleanDataIssue$G) , 'AboveMn' , 'BelowMn')
cleanDataIssue$GMn <- as.factor(cleanDataIssue$GMn)
cleanDataIssue$LagGMn <- ifelse(cleanDataIssue$LagG >=mean(cleanDataIssue$LagG) , 'AboveMn' , 'BelowMn')
cleanDataIssue$LagGMn <- as.factor(cleanDataIssue$LagGMn)


cleanDataIssue$GMed <- ifelse(cleanDataIssue$G >=median(cleanDataIssue$G) , 'AboveMed' , 'BelowMed')
cleanDataIssue$GMed <- as.factor(cleanDataIssue$GMed)
cleanDataIssue$LagGMed <- ifelse(cleanDataIssue$LagG >=median(cleanDataIssue$LagG) , 'AboveMed' , 'BelowMed')
cleanDataIssue$LagGMed <- as.factor(cleanDataIssue$LagGMed)


###########################################################


#Correlation Matrix
cleanDataIssue %>%
  as.data.frame() %>%                
  select(lnIssueIntSpreadToTreasury, ESGOverallStd, lnIssueAmount, IssueYearsToMaturity, Call_Flag, SafeZone, GreyZone, DistressZone, TobinsQ, Volatility, Leverage, InterestCoverage, Loss, ROA, Size, WC, Europe, US) %>%                   
  as.matrix() %>%                   
  cor()


#VIF Test
R1 <- lm(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index  , data = cleanDataIssue)
ols_vif_tol(R1)


###########################################################


#Section 4.3.1
R1.Issue <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue)
summary(R1.Issue)
modelsummary(R1.Issue)


R2.Issue <- feols(lnIssueIntSpreadToTreasury ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue)
summary(R2.Issue)
modelsummary(R2.Issue)


#Bind Dummy To ESG Overall Quart
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssue$ESGOverallStdQuant) <- cbind(Quant2, Quant3, Quant4)
R8.Issue <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue)
summary(R8.Issue)
modelsummary(R8.Issue)


#Bind Dummy To ESG Pillar
cleanDataIssue$EQuant
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssue$EQuant) <- cbind(Quant2, Quant3, Quant4)
cleanDataIssue$SQuant
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssue$SQuant) <- cbind(Quant2, Quant3, Quant4)
cleanDataIssue$GQuant
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssue$GQuant) <- cbind(Quant2, Quant3, Quant4)
R9.Issue <- feols(lnIssueIntSpreadToTreasury ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue)
summary(R9.Issue)
modelsummary(R9.Issue)


cleanDataIssue$ESGOverallStdMn
AboveMn <- c(1,0)
contrasts(cleanDataIssue$ESGOverallStdMn) <- cbind(AboveMn)
R10.Issue <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdMn + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue)
summary(R10.Issue)
modelsummary(R10.Issue)


cleanDataIssue$ESGOverallStdMed
AboveMed <- c(1,0)
contrasts(cleanDataIssue$ESGOverallStdMed) <- cbind(AboveMed)
R11.Issue <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdMed + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue)
summary(R11.Issue)
modelsummary(R11.Issue)


cleanDataIssue$EMed
cleanDataIssue$SMed
cleanDataIssue$GMed
AboveMed <- c(1,0)
contrasts(cleanDataIssue$EMed) <- cbind(AboveMed)
contrasts(cleanDataIssue$SMed) <- cbind(AboveMed)
contrasts(cleanDataIssue$GMed) <- cbind(AboveMed)
R12.Issue <- feols(lnIssueIntSpreadToTreasury ~ EMed + SMed + GMed + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue)
summary(R12.Issue)
modelsummary(R12.Issue)


cleanDataIssue$EMn
cleanDataIssue$SMn
cleanDataIssue$GMn
AboveMn <- c(1,0)
contrasts(cleanDataIssue$EMn) <- cbind(AboveMn)
contrasts(cleanDataIssue$SMn) <- cbind(AboveMn)
contrasts(cleanDataIssue$GMn) <- cbind(AboveMn)
R13.Issue <- feols(lnIssueIntSpreadToTreasury ~ EMn + SMn + GMn + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue)
summary(R13.Issue)
modelsummary(R13.Issue)


###########################################################


#High Risk Dataset
cleanDataHighRiskIssue <- cleanDataIssue
cleanDataHighRiskIssue$SIC <- ifelse(cleanDataHighRiskIssue$SIC == 'HighRisk', cleanDataHighRiskIssue$SIC, NA)
cleanDataHighRiskIssue <-cleanDataHighRiskIssue[!is.na(cleanDataHighRiskIssue$SIC),]


cleanDataHighRiskIssue$ESGOverallStdQuant <- ntile(cleanDataHighRiskIssue$ESGOverallStd, 4) 
cleanDataHighRiskIssue$ESGOverallStdQuant<-ifelse(cleanDataHighRiskIssue$ESGOverallStdQuant ==4, 'Quant4', ifelse(cleanDataHighRiskIssue$ESGOverallStdQuant ==3, 'Quant3', ifelse(cleanDataHighRiskIssue$ESGOverallStdQuant==2, 'Quant2', ifelse(cleanDataHighRiskIssue$ESGOverallStdQuant==1,'Quant1', NA))))
cleanDataHighRiskIssue$ESGOverallStdQuant <- as.factor(cleanDataHighRiskIssue$ESGOverallStdQuant)
summary(cleanDataHighRiskIssue$ESGOverallStdQuant)


cleanDataHighRiskIssue$LagESGOverallStdQuant <- ntile(cleanDataHighRiskIssue$LagESGOverallStd, 4) 
cleanDataHighRiskIssue$LagESGOverallStdQuant<-ifelse(cleanDataHighRiskIssue$LagESGOverallStdQuant ==4, 'Quant4', ifelse(cleanDataHighRiskIssue$LagESGOverallStdQuant ==3, 'Quant3', ifelse(cleanDataHighRiskIssue$LagESGOverallStdQuant==2, 'Quant2', ifelse(cleanDataHighRiskIssue$LagESGOverallStdQuant==1,'Quant1', NA))))
cleanDataHighRiskIssue$LagESGOverallStdQuant <- as.factor(cleanDataHighRiskIssue$LagESGOverallStdQuant)
summary(cleanDataHighRiskIssue$LagESGOverallStdQuant)


cleanDataHighRiskIssue$EQuant <- ntile(cleanDataHighRiskIssue$E, 4) 
cleanDataHighRiskIssue$EQuant<-ifelse(cleanDataHighRiskIssue$EQuant ==4, 'Quant4', ifelse(cleanDataHighRiskIssue$EQuant ==3, 'Quant3', ifelse(cleanDataHighRiskIssue$EQuant==2, 'Quant2', ifelse(cleanDataHighRiskIssue$EQuant==1,'Quant1', NA))))
cleanDataHighRiskIssue$EQuant <- as.factor(cleanDataHighRiskIssue$EQuant)
summary(cleanDataHighRiskIssue$EQuant)


cleanDataHighRiskIssue$LagEQuant <- ntile(cleanDataHighRiskIssue$LagE, 4) 
cleanDataHighRiskIssue$LagEQuant<-ifelse(cleanDataHighRiskIssue$LagEQuant ==4, 'Quant4', ifelse(cleanDataHighRiskIssue$LagEQuant ==3, 'Quant3', ifelse(cleanDataHighRiskIssue$LagEQuant==2, 'Quant2', ifelse(cleanDataHighRiskIssue$LagEQuant==1,'Quant1', NA))))
cleanDataHighRiskIssue$LagEQuant <- as.factor(cleanDataHighRiskIssue$LagEQuant)
summary(cleanDataHighRiskIssue$LagEQuant)


cleanDataHighRiskIssue$SQuant <- ntile(cleanDataHighRiskIssue$S, 4) 
cleanDataHighRiskIssue$SQuant<-ifelse(cleanDataHighRiskIssue$SQuant ==4, 'Quant4', ifelse(cleanDataHighRiskIssue$SQuant ==3, 'Quant3', ifelse(cleanDataHighRiskIssue$SQuant==2, 'Quant2', ifelse(cleanDataHighRiskIssue$SQuant==1,'Quant1', NA))))
cleanDataHighRiskIssue$SQuant <- as.factor(cleanDataHighRiskIssue$SQuant)
summary(cleanDataHighRiskIssue$SQuant)


cleanDataHighRiskIssue$LagSQuant <- ntile(cleanDataHighRiskIssue$LagS, 4) 
cleanDataHighRiskIssue$LagSQuant<-ifelse(cleanDataHighRiskIssue$LagSQuant ==4, 'Quant4', ifelse(cleanDataHighRiskIssue$LagSQuant ==3, 'Quant3', ifelse(cleanDataHighRiskIssue$LagSQuant==2, 'Quant2', ifelse(cleanDataHighRiskIssue$LagSQuant==1,'Quant1', NA))))
cleanDataHighRiskIssue$LagSQuant <- as.factor(cleanDataHighRiskIssue$LagSQuant)
summary(cleanDataHighRiskIssue$LagSQuant)


cleanDataHighRiskIssue$GQuant <- ntile(cleanDataHighRiskIssue$G, 4) 
cleanDataHighRiskIssue$GQuant<-ifelse(cleanDataHighRiskIssue$GQuant ==4, 'Quant4', ifelse(cleanDataHighRiskIssue$GQuant ==3, 'Quant3', ifelse(cleanDataHighRiskIssue$GQuant==2, 'Quant2', ifelse(cleanDataHighRiskIssue$GQuant==1,'Quant1', NA))))
cleanDataHighRiskIssue$GQuant <- as.factor(cleanDataHighRiskIssue$GQuant)
summary(cleanDataHighRiskIssue$GQuant)


cleanDataHighRiskIssue$LagGQuant <- ntile(cleanDataHighRiskIssue$LagG, 4) 
cleanDataHighRiskIssue$LagGQuant<-ifelse(cleanDataHighRiskIssue$LagGQuant ==4, 'Quant4', ifelse(cleanDataHighRiskIssue$LagGQuant ==3, 'Quant3', ifelse(cleanDataHighRiskIssue$LagGQuant==2, 'Quant2', ifelse(cleanDataHighRiskIssue$LagGQuant==1,'Quant1', NA))))
cleanDataHighRiskIssue$LagGQuant <- as.factor(cleanDataHighRiskIssue$LagGQuant)
summary(cleanDataHighRiskIssue$LagGQuant)


###########################################################


#Low Risk Dataset
cleanDataLowRiskIssue <- cleanDataIssue
cleanDataLowRiskIssue$SIC <- ifelse(cleanDataLowRiskIssue$SIC == 'LowRisk', cleanDataLowRiskIssue$SIC, NA)
cleanDataLowRiskIssue <-cleanDataLowRiskIssue[!is.na(cleanDataLowRiskIssue$SIC),]


cleanDataLowRiskIssue$ESGOverallStdQuant <- ntile(cleanDataLowRiskIssue$ESGOverallStd, 4) 
cleanDataLowRiskIssue$ESGOverallStdQuant<-ifelse(cleanDataLowRiskIssue$ESGOverallStdQuant ==4, 'Quant4', ifelse(cleanDataLowRiskIssue$ESGOverallStdQuant ==3, 'Quant3', ifelse(cleanDataLowRiskIssue$ESGOverallStdQuant==2, 'Quant2', ifelse(cleanDataLowRiskIssue$ESGOverallStdQuant==1,'Quant1', NA))))
cleanDataLowRiskIssue$ESGOverallStdQuant <- as.factor(cleanDataLowRiskIssue$ESGOverallStdQuant)
summary(cleanDataLowRiskIssue$ESGOverallStdQuant)


cleanDataLowRiskIssue$LagESGOverallStdQuant <- ntile(cleanDataLowRiskIssue$LagESGOverallStd, 4) 
cleanDataLowRiskIssue$LagESGOverallStdQuant<-ifelse(cleanDataLowRiskIssue$LagESGOverallStdQuant ==4, 'Quant4', ifelse(cleanDataLowRiskIssue$LagESGOverallStdQuant ==3, 'Quant3', ifelse(cleanDataLowRiskIssue$LagESGOverallStdQuant==2, 'Quant2', ifelse(cleanDataLowRiskIssue$LagESGOverallStdQuant==1,'Quant1', NA))))
cleanDataLowRiskIssue$LagESGOverallStdQuant <- as.factor(cleanDataLowRiskIssue$LagESGOverallStdQuant)
summary(cleanDataLowRiskIssue$LagESGOverallStdQuant)


cleanDataLowRiskIssue$EQuant <- ntile(cleanDataLowRiskIssue$E, 4) 
cleanDataLowRiskIssue$EQuant<-ifelse(cleanDataLowRiskIssue$EQuant ==4, 'Quant4', ifelse(cleanDataLowRiskIssue$EQuant ==3, 'Quant3', ifelse(cleanDataLowRiskIssue$EQuant==2, 'Quant2', ifelse(cleanDataLowRiskIssue$EQuant==1,'Quant1', NA))))
cleanDataLowRiskIssue$EQuant <- as.factor(cleanDataLowRiskIssue$EQuant)
summary(cleanDataLowRiskIssue$EQuant)


cleanDataLowRiskIssue$LagEQuant <- ntile(cleanDataLowRiskIssue$LagE, 4) 
cleanDataLowRiskIssue$LagEQuant<-ifelse(cleanDataLowRiskIssue$LagEQuant ==4, 'Quant4', ifelse(cleanDataLowRiskIssue$LagEQuant ==3, 'Quant3', ifelse(cleanDataLowRiskIssue$LagEQuant==2, 'Quant2', ifelse(cleanDataLowRiskIssue$LagEQuant==1,'Quant1', NA))))
cleanDataLowRiskIssue$LagEQuant <- as.factor(cleanDataLowRiskIssue$LagEQuant)
summary(cleanDataLowRiskIssue$LagEQuant)


cleanDataLowRiskIssue$SQuant <- ntile(cleanDataLowRiskIssue$S, 4) 
cleanDataLowRiskIssue$SQuant<-ifelse(cleanDataLowRiskIssue$SQuant ==4, 'Quant4', ifelse(cleanDataLowRiskIssue$SQuant ==3, 'Quant3', ifelse(cleanDataLowRiskIssue$SQuant==2, 'Quant2', ifelse(cleanDataLowRiskIssue$SQuant==1,'Quant1', NA))))
cleanDataLowRiskIssue$SQuant <- as.factor(cleanDataLowRiskIssue$SQuant)
summary(cleanDataLowRiskIssue$SQuant)


cleanDataLowRiskIssue$LagSQuant <- ntile(cleanDataLowRiskIssue$LagS, 4) 
cleanDataLowRiskIssue$LagSQuant<-ifelse(cleanDataLowRiskIssue$LagSQuant ==4, 'Quant4', ifelse(cleanDataLowRiskIssue$LagSQuant ==3, 'Quant3', ifelse(cleanDataLowRiskIssue$LagSQuant==2, 'Quant2', ifelse(cleanDataLowRiskIssue$LagSQuant==1,'Quant1', NA))))
cleanDataLowRiskIssue$LagSQuant <- as.factor(cleanDataLowRiskIssue$LagSQuant)
summary(cleanDataLowRiskIssue$LagSQuant)


cleanDataLowRiskIssue$GQuant <- ntile(cleanDataLowRiskIssue$G, 4) 
cleanDataLowRiskIssue$GQuant<-ifelse(cleanDataLowRiskIssue$GQuant ==4, 'Quant4', ifelse(cleanDataLowRiskIssue$GQuant ==3, 'Quant3', ifelse(cleanDataLowRiskIssue$GQuant==2, 'Quant2', ifelse(cleanDataLowRiskIssue$GQuant==1,'Quant1', NA))))
cleanDataLowRiskIssue$GQuant <- as.factor(cleanDataLowRiskIssue$GQuant)
summary(cleanDataLowRiskIssue$GQuant)


cleanDataLowRiskIssue$LagGQuant <- ntile(cleanDataLowRiskIssue$LagG, 4) 
cleanDataLowRiskIssue$LagGQuant<-ifelse(cleanDataLowRiskIssue$LagGQuant ==4, 'Quant4', ifelse(cleanDataLowRiskIssue$LagGQuant ==3, 'Quant3', ifelse(cleanDataLowRiskIssue$LagGQuant==2, 'Quant2', ifelse(cleanDataLowRiskIssue$LagGQuant==1,'Quant1', NA))))
cleanDataLowRiskIssue$LagGQuant <- as.factor(cleanDataLowRiskIssue$LagGQuant)
summary(cleanDataLowRiskIssue$LagGQuant)


###########################################################


#Section 4.3.2
R1.HighriskIssue <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataHighRiskIssue)
summary(R1.HighriskIssue)
modelsummary(R1.HighriskIssue)
R1.LowriskIssue <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataLowRiskIssue)
summary(R1.LowriskIssue)
modelsummary(R1.LowriskIssue)

R2.HighriskIssue <- feols(lnIssueIntSpreadToTreasury ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataHighRiskIssue)
summary(R2.HighriskIssue)
modelsummary(R2.HighriskIssue)
R2.LowriskIssue <- feols(lnIssueIntSpreadToTreasury ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataLowRiskIssue)
summary(R2.LowriskIssue)
modelsummary(R2.LowriskIssue)


levels(cleanDataHighRiskIssue$ESGOverallStdQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
#Bind Dummy To ESG Overall Quant
contrasts(cleanDataHighRiskIssue$ESGOverallStdQuant) <- cbind(Quant2, Quant3, Quant4)
R8.HighriskIssue <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdQuant  + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataHighRiskIssue)
summary(R8.HighriskIssue)
modelsummary(R8.HighriskIssue)
levels(cleanDataLowRiskIssue$ESGOverallStdQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
#Bind Dummy To ESG Overall Quant
contrasts(cleanDataLowRiskIssue$ESGOverallStdQuant) <- cbind(Quant2, Quant3, Quant4)
R8.LowriskIssue <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataLowRiskIssue)
summary(R8.LowriskIssue)
modelsummary(R8.LowriskIssue)


summary(cleanDataHighRiskIssue$EQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataHighRiskIssue$EQuant) <- cbind(Quant2, Quant3, Quant4)
cleanDataHighRiskIssue$SQuant
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataHighRiskIssue$SQuant) <- cbind(Quant2, Quant3, Quant4)
cleanDataHighRiskIssue$GQuant
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataHighRiskIssue$GQuant) <- cbind(Quant2, Quant3, Quant4)
R9.HighriskIssue <- feols(lnIssueIntSpreadToTreasury ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataHighRiskIssue)
summary(R9.HighriskIssue)
modelsummary(R9.HighriskIssue)
summary(cleanDataLowRiskIssue$EQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataLowRiskIssue$EQuant) <- cbind(Quant2, Quant3, Quant4)
cleanDataLowRiskIssue$SQuant
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataLowRiskIssue$SQuant) <- cbind(Quant2, Quant3, Quant4)
cleanDataLowRiskIssue$GQuant
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataLowRiskIssue$GQuant) <- cbind(Quant2, Quant3, Quant4)
R9.LowriskIssue <- feols(lnIssueIntSpreadToTreasury ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataLowRiskIssue)
summary(R9.LowriskIssue)
modelsummary(R9.LowriskIssue)


###########################################################


#Stable Economy Dataset
cleanDataIssueSE<-cleanDataIssue
cleanDataIssueSE$Year <- as.factor(cleanDataIssueSE$Year)
cleanDataIssueSE$SE <- ifelse(cleanDataIssueSE$Year == '2020', NA, ifelse(cleanDataIssueSE$Year == '2021',NA, TRUE))
summary(cleanDataIssueSE$SE)
cleanDataIssueSE <-cleanDataIssueSE[!is.na(cleanDataIssueSE$SE),]


cleanDataIssueSE$ESGOverallStdQuant <- ntile(cleanDataIssueSE$ESGOverallStd, 4) 
cleanDataIssueSE$ESGOverallStdQuant<-ifelse(cleanDataIssueSE$ESGOverallStdQuant ==4, 'Quant4', ifelse(cleanDataIssueSE$ESGOverallStdQuant ==3, 'Quant3', ifelse(cleanDataIssueSE$ESGOverallStdQuant==2, 'Quant2', ifelse(cleanDataIssueSE$ESGOverallStdQuant==1,'Quant1', NA))))
cleanDataIssueSE$ESGOverallStdQuant <- as.factor(cleanDataIssueSE$ESGOverallStdQuant)
summary(cleanDataIssueSE$ESGOverallStdQuant)


cleanDataIssueSE$LagESGOverallStdQuant <- ntile(cleanDataIssueSE$LagESGOverallStd, 4) 
cleanDataIssueSE$LagESGOverallStdQuant<-ifelse(cleanDataIssueSE$LagESGOverallStdQuant ==4, 'Quant4', ifelse(cleanDataIssueSE$LagESGOverallStdQuant ==3, 'Quant3', ifelse(cleanDataIssueSE$LagESGOverallStdQuant==2, 'Quant2', ifelse(cleanDataIssueSE$LagESGOverallStdQuant==1,'Quant1', NA))))
cleanDataIssueSE$LagESGOverallStdQuant <- as.factor(cleanDataIssueSE$LagESGOverallStdQuant)
summary(cleanDataIssueSE$LagESGOverallStdQuant)


cleanDataIssueSE$EQuant <- ntile(cleanDataIssueSE$E, 4) 
cleanDataIssueSE$EQuant<-ifelse(cleanDataIssueSE$EQuant ==4, 'Quant4', ifelse(cleanDataIssueSE$EQuant ==3, 'Quant3', ifelse(cleanDataIssueSE$EQuant==2, 'Quant2', ifelse(cleanDataIssueSE$EQuant==1,'Quant1', NA))))
cleanDataIssueSE$EQuant <- as.factor(cleanDataIssueSE$EQuant)
summary(cleanDataIssueSE$EQuant)


cleanDataIssueSE$LagEQuant <- ntile(cleanDataIssueSE$LagE, 4) 
cleanDataIssueSE$LagEQuant<-ifelse(cleanDataIssueSE$LagEQuant ==4, 'Quant4', ifelse(cleanDataIssueSE$LagEQuant ==3, 'Quant3', ifelse(cleanDataIssueSE$LagEQuant==2, 'Quant2', ifelse(cleanDataIssueSE$LagEQuant==1,'Quant1', NA))))
cleanDataIssueSE$LagEQuant <- as.factor(cleanDataIssueSE$LagEQuant)
summary(cleanDataIssueSE$LagEQuant)


cleanDataIssueSE$SQuant <- ntile(cleanDataIssueSE$S, 4) 
cleanDataIssueSE$SQuant<-ifelse(cleanDataIssueSE$SQuant ==4, 'Quant4', ifelse(cleanDataIssueSE$SQuant ==3, 'Quant3', ifelse(cleanDataIssueSE$SQuant==2, 'Quant2', ifelse(cleanDataIssueSE$SQuant==1,'Quant1', NA))))
cleanDataIssueSE$SQuant <- as.factor(cleanDataIssueSE$SQuant)
summary(cleanDataIssueSE$SQuant)


cleanDataIssueSE$LagSQuant <- ntile(cleanDataIssueSE$LagS, 4) 
cleanDataIssueSE$LagSQuant<-ifelse(cleanDataIssueSE$LagSQuant ==4, 'Quant4', ifelse(cleanDataIssueSE$LagSQuant ==3, 'Quant3', ifelse(cleanDataIssueSE$LagSQuant==2, 'Quant2', ifelse(cleanDataIssueSE$LagSQuant==1,'Quant1', NA))))
cleanDataIssueSE$LagSQuant <- as.factor(cleanDataIssueSE$LagSQuant)
summary(cleanDataIssueSE$LagSQuant)


cleanDataIssueSE$GQuant <- ntile(cleanDataIssueSE$G, 4) 
cleanDataIssueSE$GQuant<-ifelse(cleanDataIssueSE$GQuant ==4, 'Quant4', ifelse(cleanDataIssueSE$GQuant ==3, 'Quant3', ifelse(cleanDataIssueSE$GQuant==2, 'Quant2', ifelse(cleanDataIssueSE$GQuant==1,'Quant1', NA))))
cleanDataIssueSE$GQuant <- as.factor(cleanDataIssueSE$GQuant)
summary(cleanDataIssueSE$GQuant)


cleanDataIssueSE$LagGQuant <- ntile(cleanDataIssueSE$LagG, 4) 
cleanDataIssueSE$LagGQuant<-ifelse(cleanDataIssueSE$LagGQuant ==4, 'Quant4', ifelse(cleanDataIssueSE$LagGQuant ==3, 'Quant3', ifelse(cleanDataIssueSE$LagGQuant==2, 'Quant2', ifelse(cleanDataIssueSE$LagGQuant==1,'Quant1', NA))))
cleanDataIssueSE$LagGQuant <- as.factor(cleanDataIssueSE$LagGQuant)
summary(cleanDataIssueSE$LagGQuant)


#Count Firms
length(unique(cleanDataIssueSE$CUSIP))


###########################################################

#Section 4.3.3
R1.cleanDataIssueSE <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueSE)
summary(R1.cleanDataIssueSE)
modelsummary(R1.cleanDataIssueSE)


R2.cleanDataIssueSE <- feols(lnIssueIntSpreadToTreasury ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueSE)
summary(R2.cleanDataIssueSE)
modelsummary(R2.cleanDataIssueSE)


levels(cleanDataIssueSE$ESGOverallStdQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
#Bind Dummy To ESG Overall Quant
contrasts(cleanDataIssueSE$ESGOverallStdQuant) <- cbind(Quant2, Quant3, Quant4)
R8.cleanDataIssueSE <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueSE)
summary(R8.cleanDataIssueSE)
modelsummary(R8.cleanDataIssueSE)


summary(cleanDataIssueSE$EQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueSE$EQuant) <- cbind(Quant2, Quant3, Quant4)
summary(cleanDataIssueSE$SQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueSE$SQuant) <- cbind(Quant2, Quant3, Quant4)
summary(cleanDataIssueSE$GQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueSE$GQuant) <- cbind(Quant2, Quant3, Quant4)
R9.cleanDataIssueSE <- feols(lnIssueIntSpreadToTreasury ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueSE)
summary(R9.cleanDataIssueSE)
modelsummary(R9.cleanDataIssueSE)


###########################################################

#Section 4.3.4
#COVID
cleanDataCOVIDIssue <- cleanDataIssue 
cleanDataCOVIDIssue$Year <- ifelse(cleanDataCOVIDIssue$Year == '2020', cleanDataCOVIDIssue$Year, ifelse(cleanDataCOVIDIssue$Year == '2021', cleanDataCOVIDIssue$Year, NA))
cleanDataCOVIDIssue <-cleanDataCOVIDIssue[!is.na(cleanDataCOVIDIssue$Year),]
summary(cleanDataCOVIDIssue$Quarter)
cleanDataCOVIDIssue$Shock1 <- ifelse(cleanDataCOVIDIssue$Quarter == '2020Q1', 1,0)
cleanDataCOVIDIssue$Shock2 <- ifelse(cleanDataCOVIDIssue$Quarter == '2020Q1', 1,ifelse(cleanDataCOVIDIssue$Quarter == '2020Q2', 1,0))
cleanDataCOVIDIssue$Shock3 <- ifelse(cleanDataCOVIDIssue$Quarter == '2020Q1', 1,ifelse(cleanDataCOVIDIssue$Quarter == '2020Q2', 1,ifelse(cleanDataCOVIDIssue$Quarter == '2020Q3', 1,0)))
cleanDataIssueCOVIDT <-cleanDataCOVIDIssue

#Count Firms
length(unique(cleanDataCOVIDIssue$CUSIP))


RShock1ESG <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + Shock1 + I(ESGOverallStd*Shock1)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssue)
summary(RShock1ESG)
modelsummary(RShock1ESG)


RShock1 <-feols(lnIssueIntSpreadToTreasury ~ E + S + G + Shock1 + I(ESGOverallStd*Shock1)+ lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssue)
summary(RShock1)
modelsummary(RShock1)


RShock2ESG <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + Shock2 + I(ESGOverallStd*Shock2)+ lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssue)
summary(RShock2ESG)
modelsummary(RShock2ESG)


RShock2 <-feols(lnIssueIntSpreadToTreasury ~ E + S + G + Shock2 + I(ESGOverallStd*Shock2)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssue)
summary(RShock2)
modelsummary(RShock2)


RShock3ESG <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + Shock3 + I(ESGOverallStd*Shock3)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssue)
summary(RShock3ESG)
modelsummary(RShock3ESG)


RShock3 <-feols(lnIssueIntSpreadToTreasury ~ E + S + G + Shock3 + I(ESGOverallStd*Shock3)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssue)
summary(RShock3)
modelsummary(RShock3)


###########################################################

#Section 4.3.5
#Create AltmanZ Classification Dataset
#DistressZone
cleanDataAltmanDistressZoneIssue <- cleanDataIssue
cleanDataAltmanDistressZoneIssue <-cleanDataAltmanDistressZoneIssue[!is.na(cleanDataAltmanDistressZoneIssue$IssueQuarter),]
cleanDataAltmanDistressZoneIssue$AltmanZClass <- ifelse(cleanDataAltmanDistressZoneIssue$AltmanZClass == 'DistressZone', 1 , 0)


#GreyZone
cleanDataAltmanGreyZoneIssue <- cleanDataIssue
cleanDataAltmanGreyZoneIssue <-cleanDataAltmanGreyZoneIssue[!is.na(cleanDataAltmanGreyZoneIssue$IssueQuarter),]
cleanDataAltmanGreyZoneIssue$AltmanZClass <- ifelse(cleanDataAltmanGreyZoneIssue$AltmanZClass == 'GreyZone', 1 , 0)


#SafeZone
cleanDataAltmanSafeZoneIssue <- cleanDataIssue
cleanDataAltmanSafeZoneIssue <-cleanDataAltmanSafeZoneIssue[!is.na(cleanDataAltmanSafeZoneIssue$IssueQuarter),]
cleanDataAltmanSafeZoneIssue$AltmanZClass <- ifelse(cleanDataAltmanSafeZoneIssue$AltmanZClass == 'SafeZone', 1 , 0)


R1DistressZoneIssue <- feols(AltmanZClass ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataAltmanDistressZoneIssue)
summary(R1DistressZoneIssue)
modelsummary(R1DistressZoneIssue)


R2DistressZoneIssue <- feols(AltmanZClass ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataAltmanDistressZoneIssue)
summary(R2DistressZoneIssue)
modelsummary(R2DistressZoneIssue)


R3DistressZoneIssue <- feols(AltmanZClass ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataAltmanDistressZoneIssue)
summary(R3DistressZoneIssue)
modelsummary(R3DistressZoneIssue)


R4DistressZoneIssue <- feols(AltmanZClass ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataAltmanDistressZoneIssue)
summary(R4DistressZoneIssue)
modelsummary(R4DistressZoneIssue)


R1GreyZoneIssue <- feols(AltmanZClass ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataAltmanGreyZoneIssue)
summary(R1GreyZoneIssue)
modelsummary(R1GreyZoneIssue)


R2GreyZoneIssue <- feols(AltmanZClass ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataAltmanGreyZoneIssue)
summary(R2GreyZoneIssue)
modelsummary(R2GreyZoneIssue)


R3GreyZoneIssue <- feols(AltmanZClass ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataAltmanGreyZoneIssue)
summary(R3GreyZoneIssue)
modelsummary(R3GreyZoneIssue)


R4GreyZoneIssue <- feols(AltmanZClass ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataAltmanGreyZoneIssue)
summary(R4GreyZoneIssue)
modelsummary(R4GreyZoneIssue)


R1SafeZoneIssue <- feols(AltmanZClass ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataAltmanSafeZoneIssue)
summary(R1SafeZoneIssue)
modelsummary(R1SafeZoneIssue)


R2SafeZoneIssue <- feols(AltmanZClass ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataAltmanSafeZoneIssue)
summary(R2SafeZoneIssue)
modelsummary(R2SafeZoneIssue)


R3SafeZoneIssue <- feols(AltmanZClass ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataAltmanSafeZoneIssue)
summary(R3SafeZoneIssue)
modelsummary(R3SafeZoneIssue)


R4SafeZoneIssue <- feols(AltmanZClass ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataAltmanSafeZoneIssue)
summary(R4SafeZoneIssue)
modelsummary(R4SafeZoneIssue)


###########################################################


#Robustness Test Firm Fixed Effects
#Base
R1.IssueFE <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataIssue)
summary(R1.IssueFE)
modelsummary(R1.IssueFE)


R2.IssueFE <- feols(lnIssueIntSpreadToTreasury ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataIssue)
summary(R2.IssueFE)
modelsummary(R2.IssueFE)


R7.IssueFE <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdMn + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataIssue)
summary(R7.IssueFE)
modelsummary(R7.IssueFE)


R6.IssueFE <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdMed + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataIssue)
summary(R6.IssueFE)
modelsummary(R6.IssueFE)


R8.IssueFE <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataIssue)
summary(R8.IssueFE)
modelsummary(R8.IssueFE)


R9.IssueFE <- feols(lnIssueIntSpreadToTreasury ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataIssue)
summary(R9.IssueFE)
modelsummary(R9.IssueFE)


R12.IssueFE <- feols(lnIssueIntSpreadToTreasury ~ EMed + SMed + GMed + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataIssue)
summary(R12.IssueFE)
modelsummary(R12.IssueFE)


R13.IssueFE <- feols(lnIssueIntSpreadToTreasury ~ EMn + SMn + GMn + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataIssue)
summary(R13.IssueFE)
modelsummary(R13.IssueFE)


#High/Low Risk Dataset
R1.HighriskIssueFE <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataHighRiskIssue)
summary(R1.HighriskIssueFE)
modelsummary(R1.HighriskIssueFE)


R1.LowriskIssueFE <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataLowRiskIssue)
summary(R1.LowriskIssueFE)
modelsummary(R1.LowriskIssueFE)


R2.HighriskIssueFE <- feols(lnIssueIntSpreadToTreasury ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataHighRiskIssue)
summary(R2.HighriskIssueFE)
modelsummary(R2.HighriskIssueFE)


R2.LowriskIssueFE <- feols(lnIssueIntSpreadToTreasury ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataLowRiskIssue)
summary(R2.LowriskIssueFE)
modelsummary(R2.LowriskIssueFE)


R8.HighriskIssueFE <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataHighRiskIssue)
summary(R8.HighriskIssueFE)
modelsummary(R8.HighriskIssueFE)


R8.LowriskIssueFE <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataLowRiskIssue)
summary(R8.LowriskIssueFE)
modelsummary(R8.LowriskIssueFE)


R9.HighriskIssueFE <- feols(lnIssueIntSpreadToTreasury ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataHighRiskIssue)
summary(R9.HighriskIssueFE)
modelsummary(R9.HighriskIssueFE)


R9.LowriskIssueFE <- feols(lnIssueIntSpreadToTreasury ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataLowRiskIssue)
summary(R9.LowriskIssueFE)
modelsummary(R9.LowriskIssueFE)


#Stable Economy
R1.cleanDataIssueSEFE <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataIssueSE)
summary(R1.cleanDataIssueSEFE)
modelsummary(R1.cleanDataIssueSEFE)


R2.cleanDataIssueSEFE <- feols(lnIssueIntSpreadToTreasury ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataIssueSE)
summary(R2.cleanDataIssueSEFE)
modelsummary(R2.cleanDataIssueSEFE)


R8.cleanDataIssueSEFE <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataIssueSE)
summary(R8.cleanDataIssueSEFE)
modelsummary(R8.cleanDataIssueSEFE)


R9.cleanDataIssueSEFE <- feols(lnIssueIntSpreadToTreasury ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataIssueSE)
summary(R9.cleanDataIssueSEFE)
modelsummary(R9.cleanDataIssueSEFE)


#COVID
RShock1ESGFE <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + Shock1 + I(ESGOverallStd*Shock1)+ lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataCOVIDIssue)
summary(RShock1ESGFE)
modelsummary(RShock1ESGFE)


RShock1FE <-feols(lnIssueIntSpreadToTreasury ~ E + S + G + Shock1 + I(ESGOverallStd*Shock1)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataCOVIDIssue)
summary(RShock1FE)
modelsummary(RShock1FE)


RShock2ESGFE <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + Shock2 +I(ESGOverallStd*Shock2)+ lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataCOVIDIssue)
summary(RShock2ESGFE)
modelsummary(RShock2ESGFE)


RShock2FE <-feols(lnIssueIntSpreadToTreasury ~ E + S + G + Shock2 +I(ESGOverallStd*Shock2)+ lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataCOVIDIssue)
summary(RShock2FE)
modelsummary(RShock2FE)


RShock3ESGFE <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + Shock3 +I(ESGOverallStd*Shock3)+ lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataCOVIDIssue)
summary(RShock3ESGFE)
modelsummary(RShock3ESGFE)


RShock3FE <-feols(lnIssueIntSpreadToTreasury ~ E + S + G + Shock3 +I(ESGOverallStd*Shock3)+ lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataCOVIDIssue)
summary(RShock3FE)
modelsummary(RShock3FE)


#Distress
R1DistressZoneIssueFE <- feols(AltmanZClass ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataAltmanDistressZoneIssue)
summary(R1DistressZoneIssueFE)
modelsummary(R1DistressZoneIssueFE)


R2DistressZoneIssueFE <- feols(AltmanZClass ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataAltmanDistressZoneIssue)
summary(R2DistressZoneIssueFE)
modelsummary(R2DistressZoneIssueFE)


R3DistressZoneIssueFE <- feols(AltmanZClass ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataAltmanDistressZoneIssue)
summary(R3DistressZoneIssueFE)
modelsummary(R3DistressZoneIssueFE)


R4DistressZoneIssueFE <- feols(AltmanZClass ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataAltmanDistressZoneIssue)
summary(R4DistressZoneIssueFE)
modelsummary(R4DistressZoneIssueFE)


R1GreyZoneIssueFE <- feols(AltmanZClass ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataAltmanGreyZoneIssue)
summary(R1GreyZoneIssueFE)
modelsummary(R1GreyZoneIssueFE)


R2GreyZoneIssueFE <- feols(AltmanZClass ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataAltmanGreyZoneIssue)
summary(R2GreyZoneIssueFE)
modelsummary(R2GreyZoneIssueFE)


R3GreyZoneIssueFE <- feols(AltmanZClass ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataAltmanGreyZoneIssue)
summary(R3GreyZoneIssueFE)
modelsummary(R3GreyZoneIssueFE)


R4GreyZoneIssueFE <- feols(AltmanZClass ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataAltmanGreyZoneIssue)
summary(R4GreyZoneIssueFE)
modelsummary(R4GreyZoneIssueFE)


R1SafeZoneIssueFE <- feols(AltmanZClass ~ E + S + G + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataAltmanSafeZoneIssue)
summary(R1SafeZoneIssueFE)
modelsummary(R1SafeZoneIssueFE)


R2SafeZoneIssueFE <- feols(AltmanZClass ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataAltmanSafeZoneIssue)
summary(R2SafeZoneIssueFE)
modelsummary(R2SafeZoneIssueFE)


R3SafeZoneIssueFE <- feols(AltmanZClass ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataAltmanSafeZoneIssue)
summary(R3SafeZoneIssueFE)
modelsummary(R3SafeZoneIssueFE)


R4SafeZoneIssueFE <- feols(AltmanZClass ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year + CUSIP , data = cleanDataAltmanSafeZoneIssue)
summary(R4SafeZoneIssueFE)
modelsummary(R4SafeZoneIssueFE)


###########################################################


#Robustness Test Stable Economy
R2014IssueESG <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue2014)
summary(R2014IssueESG)
modelsummary(R2014IssueESG)


R2015IssueESG <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue2015)
summary(R2015IssueESG)
modelsummary(R2015IssueESG)


R2016IssueESG <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue2016)
summary(R2016IssueESG)
modelsummary(R2016IssueESG)


R2017IssueESG <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue2017)
summary(R2017IssueESG)
modelsummary(R2017IssueESG)


R2018IssueESG <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue2018)
summary(R2018IssueESG)
modelsummary(R2018IssueESG)


R2019IssueESG <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssue2019)
summary(R2019IssueESG)
modelsummary(R2019IssueESG)


###########################################################


#Robustness Test COVID Before Trim
cleanDataCOVIDIssueBT <- cleanDatabeforetrim 
cleanDataCOVIDIssueBT <-cleanDataCOVIDIssueBT[!is.na(cleanDataCOVIDIssueBT$IssueQuarter),]
cleanDataCOVIDIssueBT$Year <- ifelse(cleanDataCOVIDIssueBT$Year == '2020', cleanDataCOVIDIssueBT$Year, ifelse(cleanDataCOVIDIssueBT$Year == '2021', cleanDataCOVIDIssueBT$Year, NA))
cleanDataCOVIDIssueBT <-cleanDataCOVIDIssueBT[!is.na(cleanDataCOVIDIssueBT$Year),]
cleanDataCOVIDIssueBT$Shock1 <- ifelse(cleanDataCOVIDIssueBT$Quarter == '2020Q1', 1,0)
cleanDataCOVIDIssueBT$Shock2 <- ifelse(cleanDataCOVIDIssueBT$Quarter == '2020Q1', 1,ifelse(cleanDataCOVIDIssueBT$Quarter == '2020Q2', 1,0))
cleanDataCOVIDIssueBT$Shock3 <- ifelse(cleanDataCOVIDIssueBT$Quarter == '2020Q1', 1,ifelse(cleanDataCOVIDIssueBT$Quarter == '2020Q2', 1,ifelse(cleanDataCOVIDIssueBT$Quarter == '2020Q3', 1,0)))
length(unique(cleanDataCOVIDIssueBT$CUSIP))
length(unique(cleanDataCOVIDIssueBT$Issue_Symbol))


RShock1ESGBT <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + Shock1 + I(ESGOverallStd*Shock1)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssueBT)
summary(RShock1ESGBT)
modelsummary(RShock1ESGBT)


RShock1BT <-feols(lnIssueIntSpreadToTreasury ~ E + S + G + Shock1 + I(ESGOverallStd*Shock1)+ lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssueBT)
summary(RShock1BT)
modelsummary(RShock1BT)


RShock2ESGBT <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + Shock2 + I(ESGOverallStd*Shock2)+ lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssueBT)
summary(RShock2ESGBT)
modelsummary(RShock2ESGBT)


RShock2BT <-feols(lnIssueIntSpreadToTreasury ~ E + S + G + Shock2 + I(ESGOverallStd*Shock2)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssueBT)
summary(RShock2BT)
modelsummary(RShock2BT)


RShock3ESGBT <-feols(lnIssueIntSpreadToTreasury ~ ESGOverallStd + Shock3 + I(ESGOverallStd*Shock3)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssueBT)
summary(RShock3ESGBT)
modelsummary(RShock3ESGBT)


RShock3BT <-feols(lnIssueIntSpreadToTreasury ~ E + S + G + Shock3 + I(ESGOverallStd*Shock3)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssueBT)
summary(RShock3BT)
modelsummary(RShock3BT)


###########################################################


#Robustness Test Lagged ESG Variables
#Lag
cleanDataIssueLag<-cleanDataIssue
cleanDataIssueLag$LagESGOverallStd[which(cleanDataIssueLag$LagESGOverallStd==-Inf | cleanDataIssueLag$LagESGOverallStd==Inf)]= NA
cleanDataIssueLag <-cleanDataIssueLag[!is.na(cleanDataIssueLag$LagESGOverallStd),]


#Count Firms
length(unique(cleanDataIssueLag$CUSIP))


#Base
R1.IssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year, data = cleanDataIssueLag)
summary(R1.IssueLag)
modelsummary(R1.IssueLag)


R2.IssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagE + LagS + LagG + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueLag)
summary(R2.IssueLag)
modelsummary(R2.IssueLag)


#ESG Above Mean or Below
cleanDataIssueLag$LagESGOverallStdMn <- ifelse(cleanDataIssueLag$LagESGOverallStd >= mean(cleanDataIssueLag$LagESGOverallStd), 'AboveMn' , 'BelowMn')
cleanDataIssueLag$LagESGOverallStdMn <- as.factor(cleanDataIssueLag$LagESGOverallStdMn)
AboveMn <- c(1,0)
contrasts(cleanDataIssueLag$LagESGOverallStdMn) <- cbind(AboveMn)
R7.IssueLag <- feols(lnIssueIntSpreadToTreasury ~  LagESGOverallStdMn + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year  , data = cleanDataIssueLag)
summary(R7.IssueLag)
modelsummary(R7.IssueLag)


cleanDataIssueLag$LagESGOverallStdMed <- ifelse(cleanDataIssueLag$LagESGOverallStd >= median(cleanDataIssueLag$LagESGOverallStd), 'AboveMed' , 'BelowMed')
cleanDataIssueLag$LagESGOverallStdMed <- as.factor(cleanDataIssueLag$LagESGOverallStdMed)
AboveMed <- c(1,0)
contrasts(cleanDataIssueLag$LagESGOverallStdMed) <- cbind(AboveMed)
R6.IssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagESGOverallStdMed + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueLag)
summary(R6.IssueLag)
modelsummary(R6.IssueLag)


R8.IssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year  , data = cleanDataIssueLag)
summary(R8.IssueLag)
modelsummary(R8.IssueLag)


R9.IssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagEQuant + LagSQuant + LagGQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueLag)
summary(R9.IssueLag)
modelsummary(R9.IssueLag)


cleanDataIssueLag$LagEMed <- ifelse(cleanDataIssueLag$LagE >= median(cleanDataIssueLag$LagE), 'AboveMed' , 'BelowMed')
cleanDataIssueLag$LagEMed <- as.factor(cleanDataIssueLag$LagEMed)
AboveMed <- c(1,0)
contrasts(cleanDataIssueLag$LagEMed) <- cbind(AboveMed)
cleanDataIssueLag$LagSMed <- ifelse(cleanDataIssueLag$LagS >= median(cleanDataIssueLag$LagS), 'AboveMed' , 'BelowMed')
cleanDataIssueLag$LagSMed <- as.factor(cleanDataIssueLag$LagSMed)
AboveMed <- c(1,0)
contrasts(cleanDataIssueLag$LagSMed) <- cbind(AboveMed)
cleanDataIssueLag$LagGMed <- ifelse(cleanDataIssueLag$LagG >= median(cleanDataIssueLag$LagG), 'AboveMed' , 'BelowMed')
cleanDataIssueLag$LagGMed <- as.factor(cleanDataIssueLag$LagGMed)
AboveMed <- c(1,0)
contrasts(cleanDataIssueLag$LagGMed) <- cbind(AboveMed)
R12.IssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagEMed + LagSMed + LagGMed + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueLag)
summary(R12.IssueLag)
modelsummary(R12.IssueLag)


cleanDataIssueLag$LagEMn <- ifelse(cleanDataIssueLag$LagE >= mean(cleanDataIssueLag$LagE), 'AboveMn' , 'BelowMn')
cleanDataIssueLag$LagEMn <- as.factor(cleanDataIssueLag$LagEMn)
AboveMn <- c(1,0)
contrasts(cleanDataIssueLag$LagEMn) <- cbind(AboveMn)
cleanDataIssueLag$LagSMn <- ifelse(cleanDataIssueLag$LagS >= mean(cleanDataIssueLag$LagS), 'AboveMn' , 'BelowMn')
cleanDataIssueLag$LagSMn <- as.factor(cleanDataIssueLag$LagSMn)
AboveMn <- c(1,0)
contrasts(cleanDataIssueLag$LagSMn) <- cbind(AboveMn)
cleanDataIssueLag$LagGMn <- ifelse(cleanDataIssueLag$LagG >= mean(cleanDataIssueLag$LagG), 'AboveMn' , 'BelowMn')
cleanDataIssueLag$LagGMn <- as.factor(cleanDataIssueLag$LagGMn)
AboveMn <- c(1,0)
contrasts(cleanDataIssueLag$LagGMn) <- cbind(AboveMn)
R13.IssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagEMn + LagSMn + LagGMn + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueLag)
summary(R13.IssueLag)
modelsummary(R13.IssueLag)


#High Risk Dataset
R1.HighriskIssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataHighRiskIssue)
summary(R1.HighriskIssueLag)
modelsummary(R1.HighriskIssueLag)


R1.LowriskIssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year  , data = cleanDataLowRiskIssue)
summary(R1.LowriskIssueLag)
modelsummary(R1.LowriskIssueLag)


R2.HighriskIssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagE + LagS + LagG + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year  , data = cleanDataHighRiskIssue)
summary(R2.HighriskIssueLag)
modelsummary(R2.HighriskIssueLag)


R2.LowriskIssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagE + LagS + LagG + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year  , data = cleanDataLowRiskIssue)
summary(R2.LowriskIssueLag)
modelsummary(R2.LowriskIssueLag)


R8.HighriskIssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year  , data = cleanDataHighRiskIssue)
summary(R8.HighriskIssueLag)
modelsummary(R8.HighriskIssueLag)


R8.LowriskIssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year  , data = cleanDataLowRiskIssue)
summary(R8.LowriskIssueLag)
modelsummary(R8.LowriskIssueLag)


R9.HighriskIssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagEQuant + LagSQuant + LagGQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year  , data = cleanDataHighRiskIssue)
summary(R9.HighriskIssueLag)
modelsummary(R9.HighriskIssueLag)


R9.LowriskIssueLag <- feols(lnIssueIntSpreadToTreasury ~ LagEQuant + LagSQuant + LagGQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year  , data = cleanDataLowRiskIssue)
summary(R9.LowriskIssueLag)
modelsummary(R9.LowriskIssueLag)


#Stable Economy
#Count Firms
cleanDataIssueSECount <-cleanDataIssueSE[!is.na(cleanDataIssueSE$LagESGOverallStd),]
length(unique(cleanDataIssueSECount$CUSIP))


R1.cleanDataIssueSELag <- feols(lnIssueIntSpreadToTreasury ~ LagESGOverallStd + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueSE)
summary(R1.cleanDataIssueSELag)
modelsummary(R1.cleanDataIssueSELag)


R2.cleanDataIssueSELag <- feols(lnIssueIntSpreadToTreasury ~ LagE + LagS + LagG + lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueSE)
summary(R2.cleanDataIssueSELag)
modelsummary(R2.cleanDataIssueSELag)


levels(cleanDataIssueSE$LagESGOverallStdQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
#Bind Dummy To ESG Overall Quant
contrasts(cleanDataIssueSE$LagESGOverallStdQuant) <- cbind(Quant2, Quant3, Quant4)
R8.cleanDataIssueSELag <- feols(lnIssueIntSpreadToTreasury ~ LagESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueSE)
summary(R8.cleanDataIssueSELag)
modelsummary(R8.cleanDataIssueSELag)


summary(cleanDataIssueSE$LagEQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueSE$LagEQuant) <- cbind(Quant2, Quant3, Quant4)
summary(cleanDataIssueSE$LagSQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueSE$LagSQuant) <- cbind(Quant2, Quant3, Quant4)
summary(cleanDataIssueSE$LagGQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueSE$LagGQuant) <- cbind(Quant2, Quant3, Quant4)
R9.cleanDataIssueSELag <- feols(lnIssueIntSpreadToTreasury ~ LagEQuant + LagSQuant + LagGQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueSE)
summary(R9.cleanDataIssueSELag)
modelsummary(R9.cleanDataIssueSELag)


#COVID
cleanDataIssueCOVIDCount <-cleanDataCOVIDIssue[!is.na(cleanDataCOVIDIssue$LagESGOverallStd),]
length(unique(cleanDataIssueCOVIDCount$CUSIP))


RShock1ESGLag <-feols(lnIssueIntSpreadToTreasury ~ LagESGOverallStd + Shock1 + I(ESGOverallStd*Shock1)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssue)
summary(RShock1ESGLag)
modelsummary(RShock1ESGLag)


RShock1Lag <-feols(lnIssueIntSpreadToTreasury ~ LagE + LagS + LagG + Shock1 + I(ESGOverallStd*Shock1)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssue)
summary(RShock1Lag)
modelsummary(RShock1Lag)


RShock2ESGLag <-feols(lnIssueIntSpreadToTreasury ~ LagESGOverallStd + Shock2 + I(ESGOverallStd*Shock2)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssue)
summary(RShock2ESGLag)
modelsummary(RShock2ESGLag)


RShock2Lag <-feols(lnIssueIntSpreadToTreasury ~ LagE + LagS + LagG + Shock2 +I(ESGOverallStd*Shock2)+ lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssue)
summary(RShock2Lag)
modelsummary(RShock2Lag)


RShock3ESGLag <-feols(lnIssueIntSpreadToTreasury ~ LagESGOverallStd + Shock3 + I(ESGOverallStd*Shock3)+lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssue)
summary(RShock3ESGLag)
modelsummary(RShock3ESGLag)


RShock3Lag <-feols(lnIssueIntSpreadToTreasury ~ LagE + LagS + LagG + Shock3 +I(ESGOverallStd*Shock3)+ lnIssueAmount + IssueYearsToMaturity + Call_Flag  + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataCOVIDIssue)
summary(RShock3Lag)
modelsummary(RShock3Lag)














#COVID DEFENSE
cleanDataIssueCOVIDT$ESGOverallStdQuant <- ntile(cleanDataIssueCOVIDT$ESGOverallStd, 4) 
cleanDataIssueCOVIDT$ESGOverallStdQuant<-ifelse(cleanDataIssueCOVIDT$ESGOverallStdQuant ==4, 'Quant4', ifelse(cleanDataIssueCOVIDT$ESGOverallStdQuant ==3, 'Quant3', ifelse(cleanDataIssueCOVIDT$ESGOverallStdQuant==2, 'Quant2', ifelse(cleanDataIssueCOVIDT$ESGOverallStdQuant==1,'Quant1', NA))))
cleanDataIssueCOVIDT$ESGOverallStdQuant <- as.factor(cleanDataIssueCOVIDT$ESGOverallStdQuant)
summary(cleanDataIssueCOVIDT$ESGOverallStdQuant)


cleanDataIssueCOVIDT$LagESGOverallStdQuant <- ntile(cleanDataIssueCOVIDT$LagESGOverallStd, 4) 
cleanDataIssueCOVIDT$LagESGOverallStdQuant<-ifelse(cleanDataIssueCOVIDT$LagESGOverallStdQuant ==4, 'Quant4', ifelse(cleanDataIssueCOVIDT$LagESGOverallStdQuant ==3, 'Quant3', ifelse(cleanDataIssueCOVIDT$LagESGOverallStdQuant==2, 'Quant2', ifelse(cleanDataIssueCOVIDT$LagESGOverallStdQuant==1,'Quant1', NA))))
cleanDataIssueCOVIDT$LagESGOverallStdQuant <- as.factor(cleanDataIssueCOVIDT$LagESGOverallStdQuant)
summary(cleanDataIssueCOVIDT$LagESGOverallStdQuant)


cleanDataIssueCOVIDT$EQuant <- ntile(cleanDataIssueCOVIDT$E, 4) 
cleanDataIssueCOVIDT$EQuant<-ifelse(cleanDataIssueCOVIDT$EQuant ==4, 'Quant4', ifelse(cleanDataIssueCOVIDT$EQuant ==3, 'Quant3', ifelse(cleanDataIssueCOVIDT$EQuant==2, 'Quant2', ifelse(cleanDataIssueCOVIDT$EQuant==1,'Quant1', NA))))
cleanDataIssueCOVIDT$EQuant <- as.factor(cleanDataIssueCOVIDT$EQuant)
summary(cleanDataIssueCOVIDT$EQuant)


cleanDataIssueCOVIDT$LagEQuant <- ntile(cleanDataIssueCOVIDT$LagE, 4) 
cleanDataIssueCOVIDT$LagEQuant<-ifelse(cleanDataIssueCOVIDT$LagEQuant ==4, 'Quant4', ifelse(cleanDataIssueCOVIDT$LagEQuant ==3, 'Quant3', ifelse(cleanDataIssueCOVIDT$LagEQuant==2, 'Quant2', ifelse(cleanDataIssueCOVIDT$LagEQuant==1,'Quant1', NA))))
cleanDataIssueCOVIDT$LagEQuant <- as.factor(cleanDataIssueCOVIDT$LagEQuant)
summary(cleanDataIssueCOVIDT$LagEQuant)


cleanDataIssueCOVIDT$SQuant <- ntile(cleanDataIssueCOVIDT$S, 4) 
cleanDataIssueCOVIDT$SQuant<-ifelse(cleanDataIssueCOVIDT$SQuant ==4, 'Quant4', ifelse(cleanDataIssueCOVIDT$SQuant ==3, 'Quant3', ifelse(cleanDataIssueCOVIDT$SQuant==2, 'Quant2', ifelse(cleanDataIssueCOVIDT$SQuant==1,'Quant1', NA))))
cleanDataIssueCOVIDT$SQuant <- as.factor(cleanDataIssueCOVIDT$SQuant)
summary(cleanDataIssueCOVIDT$SQuant)


cleanDataIssueCOVIDT$LagSQuant <- ntile(cleanDataIssueCOVIDT$LagS, 4) 
cleanDataIssueCOVIDT$LagSQuant<-ifelse(cleanDataIssueCOVIDT$LagSQuant ==4, 'Quant4', ifelse(cleanDataIssueCOVIDT$LagSQuant ==3, 'Quant3', ifelse(cleanDataIssueCOVIDT$LagSQuant==2, 'Quant2', ifelse(cleanDataIssueCOVIDT$LagSQuant==1,'Quant1', NA))))
cleanDataIssueCOVIDT$LagSQuant <- as.factor(cleanDataIssueCOVIDT$LagSQuant)
summary(cleanDataIssueCOVIDT$LagSQuant)


cleanDataIssueCOVIDT$GQuant <- ntile(cleanDataIssueCOVIDT$G, 4) 
cleanDataIssueCOVIDT$GQuant<-ifelse(cleanDataIssueCOVIDT$GQuant ==4, 'Quant4', ifelse(cleanDataIssueCOVIDT$GQuant ==3, 'Quant3', ifelse(cleanDataIssueCOVIDT$GQuant==2, 'Quant2', ifelse(cleanDataIssueCOVIDT$GQuant==1,'Quant1', NA))))
cleanDataIssueCOVIDT$GQuant <- as.factor(cleanDataIssueCOVIDT$GQuant)
summary(cleanDataIssueCOVIDT$GQuant)


cleanDataIssueCOVIDT$LagGQuant <- ntile(cleanDataIssueCOVIDT$LagG, 4) 
cleanDataIssueCOVIDT$LagGQuant<-ifelse(cleanDataIssueCOVIDT$LagGQuant ==4, 'Quant4', ifelse(cleanDataIssueCOVIDT$LagGQuant ==3, 'Quant3', ifelse(cleanDataIssueCOVIDT$LagGQuant==2, 'Quant2', ifelse(cleanDataIssueCOVIDT$LagGQuant==1,'Quant1', NA))))
cleanDataIssueCOVIDT$LagGQuant <- as.factor(cleanDataIssueCOVIDT$LagGQuant)
summary(cleanDataIssueCOVIDT$LagGQuant)


#Count Firms
length(unique(cleanDataIssueCOVIDT$CUSIP))


###########################################################


levels(cleanDataIssueCOVIDT$ESGOverallStdQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
#Bind Dummy To ESG Overall Quant
contrasts(cleanDataIssueCOVIDT$ESGOverallStdQuant) <- cbind(Quant2, Quant3, Quant4)
R8.cleanDataIssueCOVIDT <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueCOVIDT)
summary(R8.cleanDataIssueCOVIDT)
modelsummary(R8.cleanDataIssueCOVIDT)


summary(cleanDataIssueCOVIDT$EQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueCOVIDT$EQuant) <- cbind(Quant2, Quant3, Quant4)
summary(cleanDataIssueCOVIDT$SQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueCOVIDT$SQuant) <- cbind(Quant2, Quant3, Quant4)
summary(cleanDataIssueCOVIDT$GQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueCOVIDT$GQuant) <- cbind(Quant2, Quant3, Quant4)
R9.cleanDataIssueCOVIDT <- feols(lnIssueIntSpreadToTreasury ~ EQuant + SQuant + GQuant + lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueCOVIDT)
summary(R9.cleanDataIssueCOVIDT)
modelsummary(R9.cleanDataIssueCOVIDT)


levels(cleanDataIssueCOVIDT$ESGOverallStdQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
#Bind Dummy To ESG Overall Quant
contrasts(cleanDataIssueCOVIDT$ESGOverallStdQuant) <- cbind(Quant2, Quant3, Quant4)
R10.cleanDataIssueCOVIDT <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdQuant + Shock1 + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueCOVIDT)
summary(R10.cleanDataIssueCOVIDT)
modelsummary(R10.cleanDataIssueCOVIDT)


summary(cleanDataIssueCOVIDT$EQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueCOVIDT$EQuant) <- cbind(Quant2, Quant3, Quant4)
summary(cleanDataIssueCOVIDT$SQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueCOVIDT$SQuant) <- cbind(Quant2, Quant3, Quant4)
summary(cleanDataIssueCOVIDT$GQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueCOVIDT$GQuant) <- cbind(Quant2, Quant3, Quant4)
R11.cleanDataIssueCOVIDT <- feols(lnIssueIntSpreadToTreasury ~ EQuant + SQuant + GQuant + Shock1+ lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueCOVIDT)
summary(R11.cleanDataIssueCOVIDT)
modelsummary(R11.cleanDataIssueCOVIDT)



levels(cleanDataIssueCOVIDT$ESGOverallStdQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
#Bind Dummy To ESG Overall Quant
contrasts(cleanDataIssueCOVIDT$ESGOverallStdQuant) <- cbind(Quant2, Quant3, Quant4)
R12.cleanDataIssueCOVIDT <- feols(lnIssueIntSpreadToTreasury ~ ESGOverallStdQuant + Shock1 + I(ESGOverallStd*Shock1) + lnIssueAmount + IssueYearsToMaturity + Call_Flag + AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueCOVIDT)
summary(R12.cleanDataIssueCOVIDT)
modelsummary(R12.cleanDataIssueCOVIDT)


summary(cleanDataIssueCOVIDT$EQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueCOVIDT$EQuant) <- cbind(Quant2, Quant3, Quant4)
summary(cleanDataIssueCOVIDT$SQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueCOVIDT$SQuant) <- cbind(Quant2, Quant3, Quant4)
summary(cleanDataIssueCOVIDT$GQuant)
Quant2 <- c(0,1,0,0)
Quant3 <- c(0,0,1,0)
Quant4 <- c(0,0,0,1)
contrasts(cleanDataIssueCOVIDT$GQuant) <- cbind(Quant2, Quant3, Quant4)
R13.cleanDataIssueCOVIDT <- feols(lnIssueIntSpreadToTreasury ~ EQuant + SQuant + GQuant + Shock1 + I(ESGOverallStd*Shock1) +  lnIssueAmount + IssueYearsToMaturity + Call_Flag +  AltmanZClass + TobinsQ + Volatility + Leverage + InterestCoverage + Loss + ROA + Size + WC + Index | NAICS + Year , data = cleanDataIssueCOVIDT)
summary(R13.cleanDataIssueCOVIDT)
modelsummary(R13.cleanDataIssueCOVIDT)


###########################################################
