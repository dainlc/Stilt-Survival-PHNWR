library(readr)
waiawa <- read_csv("Hawaiian Stilt project/R code for processing data/2019_Waiwa stilt nests.csv")
head(waiawa)

nestid <- waiawa$`Nest Id`
fate <- waiawa$`Nest Fate (Hatch, Partial Hatch, Abandoned, Depredated, Unknown)`
eggs <- waiawa$`# of Eggs Layed`
hatch <- waiawa$`# of Eggs Hatched`
date <- waiawa$`Date Found mm/dd/yyyy`

library(lubridate)
date <- mdy(date)

w.e <- subset(waiawa, date >= "2019-03-25" & date<= "2019-05-08")
w.l <- subset(waiawa, date > "2019-05-08" & date <= "2019-07-04")

head(w.e)

t.test(w.e$`# of Eggs Layed`, w.l$'# of Eggs Layed')
t.test(w.e$`Percent Hatched`, w.l$`Percent Hatched`)




