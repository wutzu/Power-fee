#1 電費計算
library(lubridate)
power <- function(startdate,enddate,kWh,public,houses){
  startdate <- as.Date(startdate,format="%Y-%m-%d") 
  enddate <- as.Date(enddate,format="%Y-%m-%d")
  if(month(startdate) %in% c(6,7,8,9) & month(enddate) %in% c(6,7,8,9)){
    summerdays <- difftime(enddate, startdate, units="days") 
    normaldays = 0
    summerdays <- as.integer(summerdays)
  }
  else if(month(startdate) %in% c(6,7,8,9) & month(enddate) %in% c(10,11,12)){
    summerdays <- difftime("2021-10-1",startdate, units = "days")
    normaldays <- difftime(enddate, "2021-10-01", units = "days")
    summerday <- as.integer(summerdays)
    summerdays <- summerday + 1
    normalday <- as.integer(normaldays)
    normaldays <- normalday + 1
  }
  else if(month(startdate) %in% c(1,2,3,4,5) & month(enddate) %in% c(6,7,8,9)){
    summerdays <- difftime(enddate ,"2021-06-01", units = "days")
    normaldays <- difftime("2021-06-01", startdate, units = "days")
    summerday <- as.integer(summerdays)
    summerdays <- summerday + 1
    normalday <- as.integer(normaldays)
    normaldays <- normalday + 1
  }
  else{
    normaldays <- difftime(enddate, startdate, units="days") 
    summerdays = 0
    normaldays <- as.integer(normaldays)
  }
  days <- summerdays + normaldays
  summer <- summerdays/days
  normal <- normaldays/days
  publicprice <- public/houses
  kWhone <- kWh/2 
  if (kWh<=40){
    powerprice <- 1.63*40+publicprice
  }
  else if (kWhone>40 & kWhone<=120){
    powerprice <- (1.63*kWh*summer)+(1.63*kWh*normal)+publicprice
  }
  else if(kWhone>120 & kWhone<=330){
    powerprice <- (1.63*240*summer)+(2.38*(kWh-240)*summer)+(1.63*240*normal)+
      (2.10*(kWh-240)*normal)+publicprice
  }
  else if(kWhone>330 & kWhone<=500){
    powerprice <- (1.63*240*summer)+(2.38*420*summer)+(3.52*(kWh-240-420)*summer)+
      (1.63*240*normal)+(2.10*420*normal)+(2.89*(kWh-240-420)*normal)+publicprice
  }
  else if(kWhone>500 & kWhone<=700){
    powerprice <- (1.63*240*summer)+(2.38*420*summer)+(3.52*340*summer)+
      (4.8*(kWh-240-420-340)*summer)+(1.63*240*normal)+(2.10*420*normal)+
      (2.89*340*normal)+(3.94*(kWh-240-420-340)*normal)+publicprice
  }
  else if(kWhone>700 & kWhone<=1000){
    powerprice <- (1.63*240*summer)+(2.38*420*summer)+(3.52*340*summer)+
      (4.8*400*summer)+(5.66*(kWh-240-420-340-400)*summer)+(1.63*240*normal)+
      (2.10*420*normal)+(2.89*340*normal)+(3.94*400*normal)+
      (4.6*(kWh-240-420-340-400)*normal)+publicprice
  }
  else{
    powerprice <- (1.63*240*summer)+(2.38*420*summer)+(3.52*340*summer)+
      (4.8*400*summer)+(5.66*600*summer)+(6.41*(kWh-240-420-340-400-600)*summer)+
      (1.63*240*normal)+(2.10*420*normal)+(2.89*340*normal)+(3.94*400*normal)+
      (4.6*600*normal)+(5.03*(kWh-240-420-340-400-600)*normal)+publicprice
  }
  print(round(powerprice))
}
power('2021-05-11','2021-07-06',651,14409,74)
