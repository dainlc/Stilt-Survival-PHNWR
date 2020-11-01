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

mean(w.e$`# of Eggs Layed`, na.rm = TRUE)
we.sd = sd(w.e$`# of Eggs Layed`, na.rm = TRUE)
we.se = we.sd/sqrt(10)
we.se

wl.sd = sd(w.l$`# of Eggs Layed`, na.rm = TRUE)
wl.se = wl.sd/sqrt(10)
wl.se

mean(w.e$`Percent Hatched`, na.rm = TRUE)
h.we.sd = sd(w.e$`Percent Hatched`, na.rm = TRUE)
h.we.se = we.sd/sqrt(10)
h.we.se

mean(w.l$`Percent Hatched`, na.rm = TRUE)
h.wl.sd = sd(w.l$`Percent Hatched`, na.rm = TRUE)
h.wl.se = h.wl.sd/sqrt(10)
h.wl.se







