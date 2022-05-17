rm(list=ls())
dev.off()
gc()

library(ggplot2)
library(raster)
library(leaflet)
library(dplyr)
library(stringr)
library(lmtest)
library(TSstudio)
library(reshape2)
library(urca)
library(tseries)
library(forcats)
library(forecast)
library(vars)
setwd("C:/Users/Yuyang Shi/Desktop/Thesis")

# Process Prison Raw Data

# admission.raw = read.csv(file = "Inmate_Admissions.csv",
#                      check.names = FALSE)
# discharge.raw = read.csv(file = "Inmate_Discharges.csv",
#                       check.names = FALSE)

# prison.raw = rbind(admission.raw, discharge.raw[, -6]) # Combine
# prison.raw = prison.raw[, -c(5, 6)]
# prison.unique = function(){
#   list = unique(prison.raw$INMATEID)
#   output = vector()
#   for(i in 1:length(list)){
#     print(i)
#     temp.raw = prison.raw[prison.raw$INMATEID == list[i], ]
#     temp = unique(temp.raw)
## No Mission Data In Admission
#     admission = temp$ADMITTED_DT
## Condition:
## If There Is Discharge Record For Inmate Skip
## 2. Admission Discharge Match Perfectly 
#     if (sum(temp$DISCHARGED_DT == "") == 0 || dim(temp.raw)[1] == 2 * length(admission)){
#       output = rbind(output, temp)
#     } else {
## Find Latest Not Discharge Record
#       newest = max(temp$ADMITTED_DT[which(temp$DISCHARGED_DT == "")])
#       index = vector()
## Make New Record That Is Not Discharge
#       if (length(which(temp$ADMITTED_DT == newest)) == 1){
#         index = intersect(which(temp$ADMITTED_DT == newest),
#                           which(temp$DISCHARGED_DT == ""))
#       }
#       output = rbind(output, temp[c(which(temp$DISCHARGED_DT != ""), index), ])
#     }
#   }
#   return(output)
# }
# test = prison.unique()
# prison = test

# Finish Prison Raw Data

# Process Prison Data

prison = read.csv(file = "Prison.csv", check.names = FALSE)
Sys.setlocale("LC_TIME", "C") # Bug
calculate.population = function(){
  date.admission = prison$ADMITTED_DT
  table.admission = as.data.frame(table(date.admission))
  duplicate.admission = table.admission[which(table.admission$Freq > 1), 1]
  for (i in 1:length(duplicate.admission)){ # Test Suspect Duplicates
    if (!i%%1000)
      print(i)
    inmate = prison[which(prison$ADMITTED_DT == duplicate.admission[i]), ]$INMATEID
    record = length(inmate) - length(unique(inmate)) # Difference
    if(record > 0){
      index = which(date.admission == duplicate.admission[i])[-(1:length(unique(inmate)))]
      date.admission = date.admission[-index]
    }
  }
  
  date.admission = as.Date(as.POSIXct(date.admission, format = '%m/%d/%Y %H:%M:%S',
                                      tz = "GMT"))
  sum(is.na(date.admission)) # Check Missing Value
  date.admission = format(date.admission, "%Y-%m")
  freq.admission = table(date.admission)
  freq.admission = as.data.frame(freq.admission)
  colnames(freq.admission)[1] = "Month"
  
  date.discharge = as.Date(as.POSIXct(prison$DISCHARGED_DT, format='%m/%d/%Y %H:%M:%S',
                                      tz="GMT"))
  sum(is.na(date.discharge)) # Check Missing Value
  date.discharge = na.omit(date.discharge)
  sum(is.na(date.discharge)) # Check Missing Value
  
  date.discharge = format(date.discharge, "%Y-%m")
  freq.discharge = table(date.discharge)
  freq.discharge = as.data.frame(freq.discharge)
  colnames(freq.discharge)[1] = "Month"
  
  setdiff(freq.admission$Month, freq.discharge$Month)
  freq.prison = merge(freq.admission, freq.discharge, by = "Month")
  setdiff(freq.admission$Month, freq.prison$Month)
  setdiff(freq.discharge$Month, freq.prison$Month)

  colnames(freq.prison)[2:3] = c("Admission", "Discharge")
  freq.prison$Flow = freq.prison$Admission - freq.prison$Discharge # Population Flow
  
  for (i in 1:dim(freq.prison)[1]){
    if (i == 1){
      freq.prison$Total[i] = sum(freq.prison$Flow[1],
                                 sum(freq.admission$Freq[1:which(freq.admission$Month == "2014-01") - 1]))
      # Initial Population
    } else {
      freq.prison$Total[i] = sum(freq.prison$Flow[i], freq.prison$Total[i - 1])
      # Previous Population + Population Flow
    }
  }
  return(freq.prison)
}
prison.population = calculate.population()
prison.population = prison.population[-dim(prison.population)[1], ]

# Finish Prison Data

# Process House Raw Data
# house.raw = read.csv(file = "Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_raw_mon.csv",
#                      check.names= FALSE)
# house.raw = read.csv(file = "Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv",
#                             check.names= FALSE)
# house.raw = house.raw[which(house.raw$RegionName == "Binghamton, NY"), ]
# house.raw = process(house.raw)
# house.raw$Year = as.Date(house.raw$Year)
# house.raw$Value = as.numeric(house.raw$Value)
# graph.house = ggplot(house.raw) + geom_line(aes(x = Year, y = Value))
# graph.house + scale_x_date(date_labels = "%Y") + ylab("Zillow Home Value Index")


house.single.raw = read.csv(file = "Binghamton/Metro_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_mon.csv",
                     check.names= FALSE)
house.1.raw = read.csv(file = "Binghamton/Metro_zhvi_bdrmcnt_1_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv",
                       check.names= FALSE)
house.2.raw = read.csv(file = "Binghamton/Metro_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv",
                       check.names= FALSE)
house.3.raw = read.csv(file = "Binghamton/Metro_zhvi_bdrmcnt_3_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv",
                       check.names= FALSE)
house.4.raw = read.csv(file = "Binghamton/Metro_zhvi_bdrmcnt_4_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv",
                       check.names= FALSE)
house.5.raw = read.csv(file = "Binghamton/Metro_zhvi_bdrmcnt_5_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv",
                       check.names= FALSE)
house.condo.raw = read.csv(file = "Binghamton/Metro_zhvi_uc_condo_tier_0.33_0.67_sm_sa_mon.csv",
                            check.names= FALSE)
house.single.raw = house.single.raw[which(house.single.raw$RegionName == "Binghamton, NY"), ]
house.condo.raw = house.condo.raw[which(house.condo.raw$RegionName == "Binghamton, NY"), ]
house.1.raw = house.1.raw[which(house.1.raw$RegionName == "Binghamton, NY"), ]
house.2.raw = house.2.raw[which(house.2.raw$RegionName == "Binghamton, NY"), ]
house.3.raw = house.3.raw[which(house.3.raw$RegionName == "Binghamton, NY"), ]
house.4.raw = house.4.raw[which(house.4.raw$RegionName == "Binghamton, NY"), ]
house.5.raw = house.5.raw[which(house.5.raw$RegionName == "Binghamton, NY"), ]

process = function(house){
  house = rbind(colnames(house),house)
  house = t(house)[6:304, ]
  rownames(house) = seq(1, 299)
  colnames(house) = c("Year", "Value")
  return(as.data.frame(house))
}

house.single = process(house.single.raw)
house.condo = process(house.condo.raw)
house.1 = process(house.1.raw)
house.2 = process(house.2.raw)
house.3 = process(house.3.raw)
house.4 = process(house.4.raw)
house.5 = process(house.5.raw)
house.list = c("house.single.raw", "house.condo.raw", "house.1.raw", "house.2.raw",
                  "house.3.raw", "house.4.raw", "house.5.raw")
rm(list = house.list)


county = data.frame(State = "New York", County = c("Kings County", "Queens County",
                                                   "New York County", "Bronx County",
                                                   "Richmond County"), Value = 0,
                    Borough = c("Brooklyn", "Queens", "Manhattan", "Bronx", "Staten Island"),
                    Abbreviation = c("K", "Q", "M", "B", "S"), stringsAsFactors = FALSE)
# Define County



find.county = function(){
  single.index = vector()
  condo.index = vector()
  for (i in 1:dim(county)[1]){
    single.index = c(single.index, which(house.single.raw$RegionName == county[i, 2]))
    condo.index = c(condo.index, which(house.condo.raw$RegionName == county[i, 2]))
  }
  return(rbind(single.index, condo.index))
}

house.month.limit = c("2014-01-31", "2020-09-30")
house.month = c("1996-01-31", "2020-09-30")

transpose.house = function(index, range.limit, range){
  house.single = house.single.raw[index[1, ], ]
  house.condo = house.condo.raw[index[2, ], ]
  
  date.house = as.Date(intersect(colnames(house.single)[10:length(colnames(house.single))],
                                 colnames(house.condo)[10:length(colnames(house.condo))]))

  price.house.single = house.single[, 10:dim(house.single)[2]]
  price.house.condo = house.condo[, 10:dim(house.condo)[2]]
  price.house.single[is.na(price.house.single)] = 0
  price.house.condo[is.na(price.house.condo)] = 0
  price.house.single = t(as.matrix(price.house.single))
  price.house.condo = t(as.matrix(price.house.condo))
  
  combine.house.single = data.frame(date.house, price.house.single)
  combine.house.condo = data.frame(date.house, price.house.condo)
  colnames(combine.house.single) = c("Date", house.single$RegionName)
  colnames(combine.house.condo) = c("Date", house.condo$RegionName)
  
  # Limit Range
  combine.house.single.crime.limit = combine.house.single[seq(which(combine.house.single$Date == range.limit[1]),
                            which(combine.house.single$Date == range.limit[2])), ]
  combine.house.condo.crime.limit = combine.house.condo[seq(which(combine.house.condo$Date == range.limit[1]),
                            which(combine.house.condo$Date == range.limit[2])), ]
  combine.house.single.crime = combine.house.single[seq(which(combine.house.single$Date == range[1]),
                                                        which(combine.house.single$Date == range[2])), ]
  combine.house.condo.crime = combine.house.condo[seq(which(combine.house.condo$Date == range[1]),
                                                      which(combine.house.condo$Date == range[2])), ]
  return(list(combine.house.single.crime.limit, combine.house.condo.crime.limit,
              combine.house.single.crime, combine.house.condo.crime))
}
house.single.limit = transpose.house(find.county(), house.month.limit, house.month)[[1]]
house.condo.limit = transpose.house(find.county(), house.month.limit, house.month)[[2]]
house.single = transpose.house(find.county(), house.month.limit, house.month)[[3]]
house.condo = transpose.house(find.county(), house.month.limit, house.month)[[4]]
# Finish House Raw Data

# Process Population Raw Data

population.raw = read.csv(file = "NYC_2010pop_2020precincts.csv",
                     check.names = FALSE)

population = population.raw[, 1:2] # 2st Column: Total Population
colnames(population) = c("Precinct", "Total")

# Finish Population Raw Data

# Process Unemployment Raw Data

decompose = function(){
  temp = vector()
  for (i in 1:dim(county)[1]){
    unemployment.raw = read.csv(file = paste("Unemployment Rate ", county[i, 2],
                                             ".csv", sep = ""), check.names = FALSE)
    if (i == 1){
      unemployment.raw$Label = paste(unemployment.raw$Label, "01")
      temp = cbind(temp, format(as.Date(unemployment.raw$Label,
                                        format = "%Y %b %d"), "%Y-%m"))
      colnames(temp) = "Month"
    }
    temp = cbind(temp, unemployment.raw[, 5])
    colnames(temp)[dim(temp)[2]] = county[i, 2]
  }
  return(temp)
}
unemployment = decompose()
unemployment = unemployment[-dim(unemployment)[1], ]

# Finish Unemployment Raw Data



























# Process Crime Raw Data

crime.historical.raw = read.csv(file = "NYPD_Arrests_Data__Historic_.csv",
                     check.names = FALSE)
crime.recent.raw = read.csv(file = "NYPD_Arrest_Data__Year_to_Date_.csv",
                                check.names = FALSE)
# Drop Last Column
crime.historical.raw = crime.historical.raw[, -dim(crime.historical.raw)[2]]
crime.recent.raw = crime.recent.raw[, -dim(crime.recent.raw)[2]]
crime.raw = rbind(crime.historical.raw, crime.recent.raw)

define.crime = function(){
  homicide = c("HOMICIDE-NEGLIGENT,UNCLASSIFIE", "MURDER & NON-NEGL. MANSLAUGHTE",
               "HOMICIDE-NEGLIGENT-VEHICLE", "HOMICIDE-NEGLIGENT,UNCLASSIFIED", 
               "MURDER & NON-NEGL. MANSLAUGHTER")
  assault = c("FELONY ASSAULT", "OFFENSES AGAINST THE PERSON",
              "OFFENSES AGAINST PUBLIC SAFETY")
  robbery = c("ROBBERY", "CRIMINAL TRESPASS", "BURGLARY")
  sex = c("SEX CRIMES", "RAPE")
  arson = "ARSON"
  theft = c("THEFT-FRAUD", "OTHER OFFENSES RELATED TO THEFT", "GRAND LARCENY", 
            "GRAND LARCENY OF MOTOR VEHICLE", "OTHER OFFENSES RELATED TO THEF",
            "THEFT OF SERVICES")
  index.crime = list(homicide, assault, robbery, sex, arson, theft)
  return(index.crime)
}
index.crime = define.crime()
crime.name = c("Homicide", "Assault", "Robbery", "Sex", "Arson", "Theft")

precinct = function(){
  index = sort(unique(crime.raw$ARREST_PRECINCT))
  index = index[-18] # No Population Data For Precinct 27
  list = vector()
  for (i in 1:length(index)){
    if (!i%%10)
      print(index[i])
    data = crime.raw[which(crime.raw$ARREST_PRECINCT == index[i]),
                     c(1, 2, 6, 10, 17, 18)]
    # Specific Precinct
    each = vector()
    each = append(each, list(index[i]))
    for (j in 1:length(crime.name)){ # For Each Precinct
      temp = vector()
      for(k in 1:length(index.crime[[j]])){ # For Each Index Crime
        temp = rbind(temp, 
                     data[which(data$OFNS_DESC == index.crime[[j]][[k]]), ])
      }
      each = append(each, list(temp))
    }
    list = append(list, list(each))
  }
  return(list)
}
list.precinct.crime = precinct()

crime.month = sort(unique(format(as.Date(unique(crime.raw$ARREST_DATE),
                                         format="%m/%d/%Y"), "%Y-%m")))

generate = function(){
  temp = vector()
  for (i in 1:length(list.precinct.crime)){
    if (!i%%10)
      print(i)
    wrap = vector()
    for (j in 1:length(list.precinct.crime[[i]])){
      if (j == 1){
        wrap = list(list.precinct.crime[[i]][[j]])
      } else {
        month = cbind(vector(), crime.month)
        colnames(month) = "Month"
        frequency = vector()
        frequency = as.data.frame(table(format(as.Date(list.precinct.crime[[i]][[j]]$ARREST_DATE,
                                                       format="%m/%d/%Y"), "%Y-%m")))
        colnames(frequency)[1] = "Month"
        frequency = merge(month, frequency, all = TRUE) # Merge By Month
        if (dim(frequency)[2] == 1){
          frequency[, 2] = 0
        } else{
          frequency[is.na(frequency)[, 2], 2] = 0 # NA To Zero
        }
        if (j == 2){
          combine = frequency
        } else {
          combine = cbind(combine, frequency[, 2])
        }
        colnames(combine)[dim(combine)[2]] = crime.name[j - 1]
      }
    }
    combine = combine[which(combine$Month == "2014-01"):dim(combine)[1], ] # Range
    combine$Sum = rowSums(combine[, 2:dim(combine)[2]])
    combine$Mean = combine$Sum / length(crime.name)
    wrap = append(wrap, list(combine))
    temp = append(temp, list(wrap))
  }
  for (i in 1:length(temp)){
    temp[[i]][[2]][, 2:dim(temp[[i]][[2]])[2]] = temp[[i]][[2]][,
                                                                2:dim(temp[[i]][[2]])[2]] * 100000 / population$Total[i]
  }
  return(temp)
}
crime = generate()

# Finish Crime Raw Data

# Begin Mapping Precinct To County

map.county = function(){
  precinct.county = data.frame(Borough = crime.raw$ARREST_BORO,
                              Precinct = crime.raw$ARREST_PRECINCT)
  precinct.county = unique(precinct.county)
  precinct.county = precinct.county[-which(precinct.county$Borough == ""), ]
  # Remove Empty
  duplicate = sort(unique(precinct.county$Precinct[duplicated(precinct.county$Precinct)]))

  duplicate.county = c(3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 1,
                       1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 5) # Manually
  output = data.frame(Precinct = duplicate, County = county$County[duplicate.county])
  for(i in 1:length(duplicate)){
    index = which(precinct.county$Precinct == duplicate[i])
    precinct.county = precinct.county[-index, ]
  }
  precinct.county = precinct.county[-which(precinct.county$Precinct == 27), ]
  # No Population Data For Precinct 27
  combine = merge(precinct.county, county, by = "Borough")
  output = rbind(output, data.frame(Precinct = sort(precinct.county$Precinct),
                                    County = combine[order(combine$Precinct), ]$County))
  return(output)
}
match.precinct = map.county()
match.precinct = match.precinct[order(match.precinct$Precinct), ]
rownames(match.precinct) = seq(1, 77)

crime.mean = function(){
  month = crime[[1]][[2]][, 1]
  output = vector()
  for (i in 1:dim(county)[1]){
    temp = which(match.precinct$County == county$County[i])
    list = vector()
    for (j in 1:length(temp)){
      list = append(list, list(crime[[temp[j]]][[2]][, 2:(length(crime.name) + 1)]))
    }
    average = Reduce("+", list) / length(list)
    average$Sum = rowSums(average)
    average$Mean = average$Sum / length(crime.name)
    average$Month = month
    output = append(output, list(append(list(county$County[i]), list(average))))
  }
  return(output)
}
crime.county = crime.mean()

# Finish Mapping Precinct To County












# Process Crime Raw Data
# 
# crime.historical.raw = read.csv(file = "NYPD_Complaint_Data_Historic.csv",
#                                 check.names = FALSE)
# crime.recent.raw = read.csv(file = "NYPD_Complaint_Data_Current__Year_To_Date_.csv",
#                             check.names = FALSE)
# # Drop Last Column
# crime.recent.raw = crime.recent.raw[, -dim(crime.recent.raw)[2]]
# crime.raw = rbind(crime.historical.raw, crime.recent.raw)
# rm(crime.historical.raw)
# rm(crime.recent.raw)
# gc()
# 
# crime.raw = crime.raw[which(crime.raw$CRM_ATPT_CPTD_CD == "COMPLETED"), ]
# unique(crime.raw$CRM_ATPT_CPTD_CD)
# # All Completed Crime
# crime.raw = crime.raw[-which(crime.raw$OFNS_DESC == ""), ]
# # Delete Rows Of Empty Offense Discretion
# gc()
# 
# crime.raw.empty.precinct = crime.raw[which(is.na(crime.raw$ADDR_PCT_CD)), ]
# crime.raw = crime.raw[-which(is.na(crime.raw$ADDR_PCT_CD)), ]
# # Delete No Arrest Precinct
# crime.raw.invaild.precinct = crime.raw[which(crime.raw$ADDR_PCT_CD == "-99"), ]
# crime.raw = crime.raw[-which(crime.raw$ADDR_PCT_CD == "-99"), ]
# # Delete Invalid Arrest Precinct
# 
# crime.raw = crime.raw[!duplicated(crime.raw$CMPLNT_NUM), ]

crime.name = unique(crime.raw$OFNS_DESC)

list.each.crime.table.by.precinct = function(){
  list = vector()
  index = sort(unique(crime.raw$ADDR_PCT_CD))
  for (i in 1:length(index)){
    if (!i%%10)
      print(i)
    data = crime.raw[which(crime.raw$ADDR_PCT_CD == index[i]),
                     c(6, 7, 9, 13, 14)]
    # Specific Precinct
    each = vector()
    each = append(each, list(index[i]))
    for (j in 1:length(crime.name)){ # For Each Precinct
      temp = vector()
      temp = rbind(temp, 
                     data[which(data$OFNS_DESC == crime.name[j]), ])
      each = append(each, list(temp))
    }
    list = append(list, list(each))
  }
  return(list)
}
list.each.crime.table.by.precinct = list.each.crime.table.by.precinct()

crime.month = sort(unique(format(as.Date(unique(crime.raw$RPT_DT),
                                         format="%m/%d/%Y"), "%Y-%m")))

list.each.crime.rate.by.precinct = function(){
  month = cbind(vector(), crime.month)
  colnames(month) = "Month"
  temp = vector()
  for (i in 1:length(list.each.crime.table.by.precinct)){ # 77 Precinct
    if (!i%%10)
      print(i)
    wrap = vector()
    for (j in 1:length(list.each.crime.table.by.precinct[[i]])){ # 71 Crimes
      if (j == 1){ # First Precinct
        wrap = list(list.each.crime.table.by.precinct[[i]][[j]])
      } else {
        frequency = vector()
        frequency = as.data.frame(table(format(as.Date(list.each.crime.table.by.precinct[[i]][[j]]$RPT_DT,
                                                       format="%m/%d/%Y"), "%Y-%m")))
        # Convert Into Universal Format Then Format Into Only Year Month Do Frequency Table
        colnames(frequency)[1] = "Month"
        frequency = merge(month, frequency, all = TRUE) # Merge By Month
        
        if (dim(frequency)[2] == 1){
          # No Frequency Column Happens When No Crime This Month
          frequency[, 2] = 0
        } else{
          frequency[is.na(frequency)[, 2], 2] = 0 # NA To Zero
        }
        
        if (j == 2){
          combine = frequency # First Crime Including Month
        } else {
          combine = cbind(combine, frequency[, 2]) # First Column Month
        }
        colnames(combine)[dim(combine)[2]] = crime.name[j - 1]
        # j = 1 Precinct Crime Begin With 2
      }
    }
    wrap = append(wrap, list(combine)) # Wrap Crime With Its Precinct
    temp = append(temp, list(wrap)) # Big List
  }
  for (i in 1:length(temp)){
    temp[[i]][[2]][, 2:dim(temp[[i]][[2]])[2]] = temp[[i]][[2]][,
                                                                2:dim(temp[[i]][[2]])[2]] * 100000 / population$Total[i]
  }
  return(temp)
}
list.each.crime.rate.by.precinct = list.each.crime.rate.by.precinct()

# Finish Crime Raw Data

# Begin Mapping Precinct To County By Average

map.county.to.precinct = function(){
  precinct.county.raw = data.frame(Borough = crime.raw$BORO_NM,
                               Precinct = crime.raw$ADDR_PCT_CD)
  precinct.county.raw = precinct.county.raw[-which(precinct.county.raw$Borough == ""), ]
  # Remove Empty Borough
  precinct.county.raw = as.data.frame(table(precinct.county.raw))
  index = unique(precinct.county.raw$Precinct)
  output = data.frame(Borough = NULL, Precinct = NULL)
  for (i in 1:length(index)){
    borough = precinct.county.raw$Borough[which(precinct.county.raw$Freq == 
                                        max(precinct.county.raw$Freq[which(precinct.county.raw$Precinct ==
                                                                             index[i])]))]
    # Find Given Precinct Frequency
    # Find Max Frequency
    # Find Corresponding Borough
    output = rbind(output, c(as.character(index[i]), as.character(borough)))
  }
  colnames(output) = c("Precinct", "Borough")
  output$Borough = str_to_title(str_to_lower(output$Borough))
  combine = merge(county, output, by = "Borough")
  combine$Precinct = as.numeric(combine$Precinct)
  combine = combine[order(combine$Precinct), ]
  return(combine)
}
map.county.to.precinct = map.county.to.precinct()
rownames(map.county.to.precinct) = seq(1, 77)

list.each.crime.rate.by.county = function(){
  output = vector()
  for (i in 1:dim(county)[1]){
    temp = which(map.county.to.precinct$County == county$County[i])
    # Get Precincts Given County Name
    list = vector()
    for (j in 1:length(temp)){
      list = append(list, list(list.each.crime.rate.by.precinct[[temp[j]]][[2]][,
                                                                                    2:(length(crime.name) + 1)]))
      # Get Crime Data By Given Precincts
      # Wrap it Into A List
    }
    average = Reduce("+", list) / length(list)
    average$Month = house.single$Date
    output = append(output, list(append(list(county$County[i]), list(average))))
  }
  return(output)
}
list.each.crime.rate.by.county = list.each.crime.rate.by.county()

# Finish Mapping Precinct To County

save.image()
load(".RData")

# Time Series Stationary

for (i in 1:length(list.each.crime.rate.by.county)){
  
}

test.crime.county = list.each.crime.rate.by.county[[i]][[2]]
test.crime.county = test.crime.county[, -which(colSums(test.crime.county[, -dim(test.crime.county)[2]]) == 0)]
for (j in 1:(dim(test.crime.county)[2] - 1)){
  test.crime.county[, j] = c(0, diff(test.crime.county[, j]))
  if (adf.test(test.crime.county[, j])$p.value <= 0.05 &&
      pp.test(test.crime.county[, j])$p.value <= 0.05){
  }
    else{
      print(paste(colnames(test.crime.county)[j], "Not Stationary"))
    }
}

for (k in 1:(dim(test.crime.county)[2] - 1)){
  temp = data.frame(Housing = house,
                    Crime = test.crime.county[, k])
  # temp = data.frame(Housing = house.single[, which(colnames(house.single) ==
  #                                                    list.each.crime.rate.by.county[[i]][[1]])],
  #                   Crime = test.crime.county[, k])
  lag = VARselect(temp, lag.max = 12, type = "const")[["selection"]][["SC(n)"]]
  print(grangertest(Housing ~ Crime, order = lag, data = temp))
}


temp = data.frame(Housing = house,
                  Crime = a)
# temp = data.frame(Housing = house.single[, which(colnames(house.single) ==
#                                                    list.each.crime.rate.by.county[[i]][[1]])],
#                   Crime = test.crime.county[, k])
lag = VARselect(temp, lag.max = 12, type = "const")[["selection"]][["SC(n)"]]
grangertest( test.crime.county[, 1],house, order = lag)


adf.test(house.single$`Kings County`)
adf.test(house)

a = c(0, diff(as.numeric(unemployment[, 2])))
adf.test(a)

house = c(0, diff(house.single$`Kings County`))

Month = as.Date(rownames(house.single))



inflation = read.csv(file = "Binghamton/ATNHPIUS13780Q.csv",
                     check.names = FALSE)
colnames(inflation) = c("Date", "Value")
inflation = inflation[51:149, ]

inflation = ts(inflation$Value, frequency = 4, start = 1996)
inflation = decompose(inflation)
inflation = seasadj(inflation)
inflation = ts_reshape(inflation)
inflation = melt(inflation, id.vars = "quarter")
inflation$quarter[which(inflation$quarter == 4)] = 10
inflation$quarter[which(inflation$quarter == 3)] = 7
inflation$quarter[which(inflation$quarter == 2)] = 4
inflation$quarter = as.character(inflation$quarter)
inflation$Date = paste(inflation$variable, inflation$quarter, "1", sep = "-")
inflation$Date = as.Date(inflation$Date)
colnames(inflation)[3] = "Value"

inflation$Value = inflation$Value/167.78203*100
inflation = inflation[-100, ]

graph.inflation = ggplot(inflation, aes(x = Date, y = Value)) + geom_line()
graph.inflation + scale_x_date(date_labels = "%Y") + ylab("Inflation Index 2012 Quarter 1 Dollar")




season = function(house){
  house = as.numeric(house)
  time = ts(house, frequency = 12, start = 1996)
  time = decompose(time)
  plot(time)
  time = seasadj(time)
  
  time.data = ts_reshape(time)
  time.data = melt(time.data, id.vars = "month")
  colnames(time.data) = c("Month", "Year", "Value")
  
  time.data$Month = as.character(time.data$Month)
  time.data$Date = paste(time.data$Year, time.data$Month, "1", sep = "-")
  time.data$Date = as.Date(time.data$Date)
  colnames(time.data)[3] = "Value"
  
  time.data = time.data[-c(298,299, 300), ]
  j = 1
  for(i in 1:99){
    for (k in 1:3){
      time.data$Value[j] = time.data$Value[j] / inflation$Value[i] * 100
      j = j + 1
    }
  }
  return(time.data$Value)
}
house.date = house.single$Year[-c(298, 299)]
house.single = season(house.single$Value)
house.condo = season(house.condo$Value)
house.1 = season(house.1$Value)
house.2 = season(house.2$Value)
house.3 = season(house.3$Value)
house.4 = season(house.4$Value)
house.5 = season(house.5$Value)

adf.test(time.data$Value)

# graph.time.data = ggplot(time.data, aes(x = Month, y = Value)) + geom_line(aes(color = Year, group = Year))
# graph.time.data


adf.test(time.data$Value)
adf.test(diff(time.data$Value))
time.data$Station = diff(time.data$Value)

graph.time.data = ggplot(time.data, aes(x = Month, y = Station)) + geom_line()
graph.time.data + scale_x_date(date_labels = "%Y")








# for (i in 1:k){
#   if (x(i) Grangercause y){
#     if (!y Grangercause x(i))
#       then t-test obtain sinificant window sizes
#     x(i) add to feature selection set with its window sizes
#   }
# }






graph.prison = ggplot(house.condo, aes(x = Month,
                                             y = `Kings County`)) + geom_line()
graph.prison + scale_x_date(date_labels = "%Y-%m-%d")



# Graph

USA <- getData("GADM", country = "usa", level = 2)

temp = merge(USA, county, by.x = c("NAME_1", "NAME_2"), by.y = c("state", "county"),
             all.x = TRUE)
mypal <- colorNumeric(palette = "viridis", domain = temp$value, na.color = "grey")
leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>%
  addPolygons(data = USA, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.3,
              fillColor = ~mypal(temp$value),
              popup = paste("Region: ", temp$NAME_2, "<br>",
                            "Value: ", temp$value, "<br>")) %>%
  addLegend(position = "bottomleft", pal = mypal, values = temp$value,
            title = "Value",
            opacity = 1)

# leaflet() %>% addTiles() %>% addCircleMarkers(data = crime, lat = ~Latitude, 
#                                               lng = ~Longitude, radius = ~3)


house = cbind(house.date, house.single, house.condo, house.1, house.2,
              house.3, house.4, house.5)
house = as.data.frame(house)
colnames(house) = c("Year", "Single", "Condo/Co-op", "1-Bedroom", "2-Bedroom", "3-Bedroom",
                    "4-Bedroom","5-Bedroom")
house$Year = as.Date(house$Year)
house = melt(house, id = "Year")
house$value = as.numeric(house$value)
colnames(house)[2] = "Variable"

graph.house = ggplot(house) +
  geom_line(aes(x = Year, y = value, colour = Variable)) +
  scale_colour_manual(values = c("red", "blue", "black",
                                 "green", "pink", "gray", "violet", "snow"))
graph.house + scale_x_date(date_labels = "%Y") + ylab("Zillow Home Value Index, Seasonally Adjusted, Inflation Adjusted 2012 Quarter 1 Dollar")

save.image()
load(".RData")

# race.raw = read.csv(file = "Binghamton/ACS Race DEMOGRAPHIC AND HOUSING ESTIMATES.csv",
#                        check.names= FALSE)
# race.raw$Year = as.Date(as.character(race.raw$Year), format = "%Y")
# race = race.raw[, c(1, 8, 9, 10)]
# race = melt(race, id = "Year")
# colnames(race)[2] = "Variable"
# graph.race = ggplot(race) +
#   geom_line(aes(x = Year, y = value, colour = Variable)) +
#   scale_colour_manual(values=c("red","green","blue"))
# graph.race + scale_x_date(date_labels = "%Y") + ylab("Non-White Race Percentage")
# 
# race = race.raw[, c(1, 3, 4)]
# race = melt(race, id = "Year")
# colnames(race)[2] = "Variable"
# graph.race = ggplot(race) +
#   geom_line(aes(x = Year, y = value, colour = Variable)) +
#   scale_colour_manual(values=c("red", "blue"))
# graph.race + scale_x_date(date_labels = "%Y") + ylab("Gender Distribution")

# poverty.raw = read.csv(file = "Binghamton/ACS POVERTY STATUS IN THE PAST 12 MONTHS.csv",
#                        check.names= FALSE)
# poverty.raw$Year = as.Date(as.character(poverty.raw$Year), format = "%Y")
# poverty = poverty.raw[, c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)]
# poverty = melt(poverty, id = "Year")
# colnames(poverty)[2] = "Variable"
# graph.poverty = ggplot(poverty) +
#   geom_line(aes(x = Year, y = value, colour = Variable)) +
#   scale_colour_manual(values = c("red", "blue", "yellow", "black", "orange",
#                                  "green", "pink", "gray", "violet", "snow"))
# graph.poverty + scale_x_date(date_labels = "%Y") + ylab("Poverty Percentage")
# 
# graph.poverty = ggplot(poverty.raw, aes(x = Year, y = `Percent Poverty`)) + geom_line()
# graph.poverty + scale_x_date(date_labels = "%Y") + ylab("Poverty Percentage")

income.raw = read.csv(file = "Binghamton/CAINC1 Personal Income Summary Personal Income, Population, Per Capita Personal Income.csv",
                     check.names= FALSE)
population = t(income.raw[2, -1])
population = as.data.frame(population)
population$Year = as.character(rownames(population))
colnames(population)[1] = "Population"
population$Year = as.Date(population$Year, format = "%Y")

graph.population = ggplot(population, aes(x = Year, y = Population)) + geom_line()
graph.population + scale_x_date(date_labels = "%Y-%m-%d")

income = t(income.raw[1, -1])
income = as.data.frame(income)
income$Year = as.character(rownames(income))
colnames(income)[1] = "Personal Income"
income$Year = as.Date(income$Year, format = "%Y")

graph.income = ggplot(income, aes(x = Year, y = `Personal Income`)) + geom_line()
graph.income + scale_x_date(date_labels = "%Y")

product.raw = read.csv(file = "Binghamton/CAGDP1 Gross Domestic Product (GDP) summary by county and metropolitan area.csv",
                      check.names= FALSE)
product = t(product.raw[3, -1])
product = as.data.frame(product)
product$Year = as.character(rownames(product))
colnames(product)[1] = "Real GDP"
product$Year = as.Date(product$Year, format = "%Y")

inflation.year = inflation[21:97, ]$Value
inflation.new = vector()
for(i in 1:19){
  temp = mean(inflation.year[(i-1)*4 + 1], inflation.year[(i-1)*4 + 2], inflation.year[(i-1)*4 + 3], inflation.year[(i-1)*4 + 4])
  inflation.new = c(inflation.new, temp)
  }
product$`Real GDP` = product$`Real GDP` / inflation.new

graph.income = ggplot(product, aes(x = Year, y = `Real GDP`)) + geom_line()
graph.income + scale_x_date(date_labels = "%Y") + ylab("Real GDP, Inflation Adjusted 2012 Quarter 1 Dollar")

# mortgage.raw = read.csv(file = "Binghamton/Primary Mortgage Market Survey.csv",
#                          check.names= FALSE)
# index = seq(1, 1762)
# mortgage.raw = mortgage.raw[-index, ]
# mortgage = mortgage.raw[, c(1, 2, 4, 6)]
# mortgage$Week = as.Date(as.character(mortgage$Week), format = "%Y-%m-%d")
# 
# mortgage = melt(mortgage, id = "Week")
# colnames(mortgage)[2] = "Variable"
# 
# graph.mortgage = ggplot(mortgage) +
#   geom_line(aes(x = Week, y = value, colour = Variable)) +
#   scale_colour_manual(values = c("red", "blue", "black",
#                                  "green", "violet", "gray"))
# graph.mortgage + scale_x_date(date_labels = "%Y") + ylab("Mortgage Rate")


# jail.raw = read.csv(file = "Binghamton/Jail_Population_By_County__Beginning_1997.csv",
#                         check.names= FALSE)
# index = c(which(jail.raw$`Facility Name` == "Broome County Jail"), which(jail.raw$`Facility Name` == "Tioga County Jail"))
# 
# jail.raw = jail.raw[index, ]
# jail = jail.raw[, c(1, 2, 6)]
# jail$Year = as.Date(as.character(jail$Year), format = "%Y")
# 
# colnames(jail)[2] = "Variable"
# colnames(jail)[3] = "Value"
# 
# graph.jail = ggplot(jail) +
#   geom_line(aes(x = Year, y = Value, colour = Variable)) +
#   scale_colour_manual(values = c("red", "blue", "black",
#                                  "green", "violet", "gray"))
# graph.jail + scale_x_date(date_labels = "%Y") + ylab("Jail Population")

# education.raw = read.csv(file = "Binghamton/ACS EDUCATIONAL ATTAINMENT.csv",
#                      check.names= FALSE)
# education = education.raw[, c(1, 3, 4, 5, 6, 7, 8)]
# education$Year = as.Date(as.character(education$Year), format = "%Y")
# education = melt(education, id = "Year")
# colnames(education)[2] = "Variable"
# 
# graph.education = ggplot(education) +
#   geom_line(aes(x = Year, y = value, colour = Variable)) +
#   scale_colour_manual(values = c("red", "blue", "black",
#                                   "green", "violet", "gray"))
# graph.education + scale_x_date(date_labels = "%Y") + ylab("Education Attainment")

# age.raw = read.csv(file = "Binghamton/ACS AGE AND SEX.csv",
#                      check.names= FALSE)
# age = age.raw[, c(1, 4, 7, 10, 13, 16, 17)]
# age$Year = as.Date(as.character(age$Year), format = "%Y")
# age = melt(age, id = "Year")
# colnames(age)[2] = "Variable"
# 
# graph.age = ggplot(age) +
#   geom_line(aes(x = Year, y = value, colour = Variable)) +
#   scale_colour_manual(values = c("red", "blue", "black",
#                                   "green", "violet", "gray"))
# graph.age + scale_x_date(date_labels = "%Y") + ylab("Age")

# crime = crime.raw[6:25, ]
# crime = crime[, c(1, 6, 12)]
# crime$Year = as.character(crime$Year)
# crime$Year = as.Date(crime$Year, format = "%Y")
# crime = melt(crime, id = "Year")
# colnames(crime)[2] = "Variable"
# 
# graph.violent = ggplot(crime) +
#   geom_line(aes(x = Year, y = value, colour = Variable)) +
#   scale_colour_manual(values = c("red", "blue"))
# graph.violent + scale_x_date(date_labels = "%Y") + ylab("Crime Rate")



# police.predict = crime.raw
# police.predict = police.predict[-2, ]
# police.predict = police.predict[-3, ]
# police.predict = police.predict[-3, ]
# police.predict = police.predict[, -14]
# police.predict = police.predict[, -14]
# police.predict = police.predict[, -15]
# test = which(is.na(police.predict$officers))
# police.train = police.predict[-test, -1]
# police.test = police.predict[test, -1]
# 
# logit <- function(x) log(x/(1-x))
# ilogit <- function(x) exp(x)/(1+exp(x))
# 
# police.model = lm(logit(officers/200)~.,data = police.train)
# summary(police.model)
# ilogit(predict(police.model, newdata = police.test[,-13]))* 200
# 
# 
# graph.police = ggplot(crime, aes(x = Year, y = officers)) + geom_line()
# graph.police + scale_x_date(date_labels = "%Y") +
#   ylab("Police Officer Employees") + geom_rect(alpha = 0.02, mapping = aes(xmin=as.Date("2000-01-19"), xmax=as.Date("2001-07-19"),
#                                                     ymin=0, ymax=Inf)) + 
#   geom_rect(alpha = 0.02, mapping = aes(xmin=as.Date("2012-07-19"), xmax=as.Date("2013-07-19"),
#                                         ymin=0, ymax=Inf))




graph.prison = ggplot(freq.prison, aes(x = Month, y = Total)) + geom_line()
graph.prison + scale_x_date(date_labels = "%Y-%m-%d")


graph.house = ggplot(new.house, aes(x = Date)) +
  geom_line(aes(y = new.house[, 2])) + 
  geom_line(aes(y = new.house[, 3]))
graph.house + scale_x_date(date_labels = "%Y-%m-%d")

graph.crime = ggplot(new.crime, aes(x = Date, y = Freq)) + geom_line()
graph.crime + scale_x_date(date_labels = "%Y-%m-%d")

new.house$Average = rowMeans(new.house[, -1])
graph = ggplot(new.house, aes(x = Date, y = Average)) + geom_line()
graph + scale_x_date(date_labels = "%Y-%m-%d")

test = cbind(new.house$Average, new.crime$Freq)
test = as.data.frame(test)
colnames(test) = c("Value", "Freq")
model = lm(Value ~ Freq, data = test)
summary(model)
yhat = predict(model)
newpre = data.frame(yhat, new.house$Average, yhat - new.house$Average)
