install.packages("demography")
library(demography)

#PASTING MY DATA PATH:
#luxembourg<-read.demogdata("C:/Users/a74231/Downloads/Mx_1x1.txt","C:/Users/a74231/Downloads/Exposures_1x1.txt",type="mortality",label="Luxembourg",skip=2)

luxembourg<-read.demogdata("Mx_1x1.txt", "Exposures_1x1.txt", type="mortality", label="Luxembourg", skip=2)


print(names(luxembourg$rate))
series_used<-"total"   

print(luxembourg)


#2.LIFE TABLES

lux.lt<-lifetable(luxembourg,series=series_used,years=luxembourg$year,ages=luxembourg$age,max.age=100,type="period")

lux_2020.lt<-lifetable(luxembourg,series=series_used,years=2020,ages=luxembourg$age,max.age=100,type="period")
print(lux_2020.lt)

cat("\nPeriod life expectancy at birth, Luxembourg 2020:",
    round(lux_2020.lt$ex[1], 4), " years\n")
plot(lux.lt, main = "Life Expectancy. Luxembourg, 1960-2024")


#3.LIFE EXPECTANCY TREND, 1960-2024 

lux_e0<-life.expectancy(luxembourg,series=series_used,years=luxembourg$year,type="period")
plot(lux_e0, main = "Life Expectancy at Birth. Luxembourg, 1960-2024",xlab = "Year", ylab = "e0 (years)")


e0_female<-life.expectancy(luxembourg, series="female",
                             years = luxembourg$year, type="period",max.age=100)
e0_male <-life.expectancy(luxembourg,series="male",
                             years = luxembourg$year, type = "period", max.age = 100)

cat("\nNon-finite values remaining in e0_female: ",sum(!is.finite(e0_female)), "\n")
cat("Non-finite values remaining in e0_male:",sum(!is.finite(e0_male)),"\n")

plot(e0_female,col="red", ylim=c(65, 90), xlab="Year",
     ylab="Life Expectancy",
     main="Life Expectancy at Birth by Sex. Luxembourg, 1960-2024 (smoothed)")
lines(e0_male,col="blue",lty = 2)
legend("topleft",c("Female","Male"),col=c("red", "blue"),lty = 1:2)


#4. Explicit female-minus-male gap for every year
sex_gap<-data.frame(year=luxembourg$year,e0_female=as.numeric(e0_female),e0_male = as.numeric(e0_male))
sex_gap$gap_F_minus_M<-sex_gap$e0_female - sex_gap$e0_male

if (any(!is.finite(sex_gap$gap_F_minus_M))) {
  cat("\nWARNING: non-finite values remain after smoothing, in these years:\n")
  print(sex_gap[!is.finite(sex_gap$gap_F_minus_M),])
  sex_gap <- sex_gap[is.finite(sex_gap$gap_F_minus_M),]
}

# Identify max / min gap years 
max_gap_row<-sex_gap[which.max(sex_gap$gap_F_minus_M),]
min_gap_row<-sex_gap[which.min(sex_gap$gap_F_minus_M),]

selected_years<-c(1960, 1990, 2020, max(sex_gap$year))
cat("\nSex gap in selected years:\n")
print(sex_gap[sex_gap$year %in% selected_years,])


#5. GDP PER CAPITA CONTEXT 

# SOURCE:World in Data
# URL: https://ourworldindata.org/grapher/access-to-clean-fuels-for-cooking-vs-gdp-per-capita


owid<-read.csv("C:/Users/a74231/Downloads/access-to-clean-fuels-for-cooking-vs-gdp-per-capita.csv")
lux_owid<-subset(owid, entity=="Luxembourg")


par(mfrow = c(1, 2))
plot(lux_e0, main="Life Expectancy at Birth. Luxembourg")
plot(lux_owid$year,lux_owid$ny_gdp_pcap_pp_kd, type = "l", col = "darkgreen",xlab="Year",ylab="GDP per capita (PPP, constant 2021 international $)",main="GDP per capita. Luxembourg, 1990-2025")
par(mfrow=c(1, 1))


#6. LEE-CARTER MODEL 

lux.LC <- lca(
  luxembourg,
  series=series_used,
  years=1990:2024,
  ages=0:100,
  interpolate=TRUE
)

plot(lux.LC)

lux.forecast<-forecast(lux.LC, h = 30)
plot(lux.forecast)

lux_e0_forecast<-life.expectancy(lux.forecast,type ="period")
plot(lux_e0_forecast,main="Forecasted Life Expectancy at Birth. Luxembourg,2025-2054")
print(lux_e0_forecast)


#7.VALIDATION / HOLDOUT TEST 

lux.LC_holdout <-lca(luxembourg,series= series_used,years=1990:2014,ages=0:100,interpolate=TRUE)

holdout_forecast<-forecast(lux.LC_holdout, h = 10)  # forecasts 2015-2024
e0_predicted<-life.expectancy(holdout_forecast, type="period")

e0_actual<-life.expectancy(luxembourg, series=series_used,
                             years=2015:2024,type="period")

validation_table<-data.frame(year=2015:2024,actual_e0 = as.numeric(e0_actual),predicted_e0 = as.numeric(e0_predicted))
validation_table$error<-validation_table$actual_e0 - validation_table$predicted_e0

cat("\nValidation: actual vs predicted e0, 2015-2024\n")
print(validation_table)
cat("\nMean absolute error (years):",round(mean(abs(validation_table$error)), 4),"\n")


#save files 
write.csv(sex_gap,"luxembourg_sex_gap.csv",row.names=FALSE)
write.csv(validation_table,"luxembourg_validation.csv", row.names=FALSE)
write.csv(
  data.frame(year = as.numeric(time(lux_e0_forecast)),e0_forecast=as.numeric(lux_e0_forecast)),"luxembourg_e0_forecast.csv",row.names = FALSE)

#FINAL FLIESSS

sex_gap_finite<-sex_gap[is.finite(sex_gap$e0_female) & is.finite(sex_gap$e0_male) &is.finite(sex_gap$gap_F_minus_M),]

max_gap_row <- sex_gap_finite[which.max(sex_gap_finite$gap_F_minus_M), ]
min_gap_row <- sex_gap_finite[which.min(sex_gap_finite$gap_F_minus_M), ]

cat("\nMaximum female-male gap (valid years only):\n"); print(max_gap_row)
cat("\nMinimum female-male gap (valid years only):\n"); print(min_gap_row)

# Selected reference yearss
selected_years <- c(1960, 1990, 2020, max(sex_gap_finite$year))
cat("\nSex gap in selected years:\n")
print(sex_gap_finite[sex_gap_finite$year %in% selected_years,])

cat("\nYears excluded due to non-finite values: ",nrow(sex_gap) - nrow(sex_gap_finite), "\n")
print(sex_gap[!is.finite(sex_gap$gap_F_minus_M), c("year","e0_female","e0_male","gap_F_minus_M")])

# Save 
write.csv(sex_gap_finite,"luxembourg_sex_gap_clean.csv",row.names=FALSE)













