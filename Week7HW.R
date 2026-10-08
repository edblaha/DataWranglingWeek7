## 1. Browse Bartels (2000) - “Partisanship and Voting Behavior, 1952-1996”. 
#Using the Cumulative American National Election Studies (ANES) file, 
#replicate both parts of Figure 1.
#Note: you can get very close on the top half of figure 1. 
#The bottom half, however, will look a little different in the early years 
#due to Bartels having access to a different version of the ANES than is currently 
#publicly available.

library(tidyverse)
library(ggplot2)
library(haven)
ANES <- read_dta("/Users/edwardblaha/DataWrangling/Week 7/ANES 1948-2020 Cumulative/anes_timeseries_cdf_stata_20220916.dta")

ANES_partisanship_year <- ANES |>
  select(VCF0004, VCF0305) |>
  filter(!is.na(VCF0305))
 
#asked Claude how to turn a prop table into a data frame, and it shared the as.data.frame.matrix 
#function 
ANES_partisanship_year_condensed <- as.data.frame.matrix(prop.table(table(ANES$VCF0004, ANES$VCF0305), margin = 1))
ANES_partisanship_year_condensed <- ANES_partisanship_year_condensed |> select(-"0")
ANES_partisans<- ANES_partisanship_year_condensed |> select(-"1")|> select(-"2")
ANES_independents<- ANES_partisanship_year_condensed |> select(-"3")|> select(-"4")

#Year became the index, so I asked Claude how to turn it into a new column titled "Year"
ANES_partisans    <- tibble::rownames_to_column(ANES_partisans, "year")
ANES_independents <- tibble::rownames_to_column(ANES_independents, "year")

ANES_partisans$year    <- as.numeric(ANES_partisans$year)
ANES_independents$year <- as.numeric(ANES_independents$year)

#Subset the data for only presidential election years between 1952 and 1996

ANES_partisans_pres <- subset(ANES_partisans, year %in% seq(1952, 1996, by = 4))



PartisanPlot <- ggplot(ANES_partisans_pres, aes(x = year)) +
  geom_line(aes(y = `3`, linetype = '"Weak" Identifiers')) +
  geom_line(aes(y = `4`, linetype = '"Strong" Identifiers')) +
  labs(title = "Proportions of National Election Study Sample", x = "", y = "Proportion", color = NULL, linetype = "Party ID Strength")+ 
  geom_point(aes(y = `3`), color = "black") +
  geom_point(aes(y = `4`), color = "black") +
  scale_y_continuous(limits = c(0, 0.5))+ 
  theme(plot.title = element_text(hjust = 0.5))
#last line supplied by Claude to help me center the titles 

ANES_independents_pres <- subset(ANES_independents, year %in% seq(1952, 1996, by = 4))



IndePlot <- ggplot(ANES_independents_pres, aes(x = year)) +
  geom_line(aes(y = `2`, linetype = 'Independent "Leaners"')) +
  geom_line(aes(y = `1`, linetype = '"Pure" Independents')) +
  labs( x = "Year", y = "Proportion", color = NULL, linetype = "Party ID Strength")+ 
  geom_point(aes(y = `1`), color = "black") +
  geom_point(aes(y = `2`), color = "black") +
  scale_y_continuous(limits = c(0, 0.5))

#Googled what ggplot2 code allows me to stack the plots, loaded patchwork 
library(patchwork)

StackedPlots <- PartisanPlot / IndePlot

StackedPlots




#Extend the analysis to include recent years as well.

ANES_partisans_pres_recent <- subset(ANES_partisans, year %in% seq(1952, 2020, by = 4))
ANES_independents_pres_recent <- subset(ANES_independents, year %in% seq(1952, 2020, by = 4))

PartisanPlot2 <- ggplot(ANES_partisans_pres_recent, aes(x = year)) +
  geom_line(aes(y = `3`, linetype = '"Weak" Identifiers')) +
  geom_line(aes(y = `4`, linetype = '"Strong" Identifiers')) +
  labs(title = "Proportions of National Election Study Sample", x = "", y = "Proportion", color = NULL, linetype = "Party ID Strength")+ 
  geom_point(aes(y = `3`), color = "black") +
  geom_point(aes(y = `4`), color = "black") +
  scale_y_continuous(limits = c(0, 0.5))+ 
  theme(plot.title = element_text(hjust = 0.5))
IndePlot2 <- ggplot(ANES_independents_pres_recent, aes(x = year)) +
  geom_line(aes(y = `2`, linetype = 'Independent "Leaners"')) +
  geom_line(aes(y = `1`, linetype = '"Pure" Independents')) +
  labs( x = "Year", y = "Proportion", color = NULL, linetype = "Party ID Strength")+ 
  geom_point(aes(y = `1`), color = "black") +
  geom_point(aes(y = `2`), color = "black") +
  scale_y_continuous(limits = c(0, 0.5))


StackedPlots2 <- PartisanPlot2 / IndePlot2
StackedPlots2

#Extend the analysis to 2024, using the 2024 ANES file. (This will require using the 2024 file in conjunction with the cumulative file.)
#repeating previous steps 
ANES2024 <- read_dta('/Users/edwardblaha/DataWrangling/Week 7/ANES 2024 Study/anes_timeseries_2024_stata_20250808.dta')
ANES2024_partisanship <- ANES2024 |>
  select(V241227x) |>
  filter(V241227x %in% c(1:7, -8, -9)) |>
  mutate(VCF0305 = case_when(
    V241227x == 4           ~ 1,   # Pure independent
    V241227x %in% c(3, 5)   ~ 2,   # Independent leaner
    V241227x %in% c(2, 6)   ~ 3,   # Weak partisan
    V241227x %in% c(1, 7)   ~ 4,   # Strong partisan
    V241227x %in% c(-8, -9) ~ 0),  # Don't know / refused
    Year = 2024)

ANES2024_partisanship <- as.data.frame.matrix(
 prop.table(table(ANES2024_partisanship$Year, ANES2024_partisanship$VCF0305), margin = 1))

ANES2024_partisanship <- tibble::rownames_to_column(ANES2024_partisanship, "year")
ANES_partisans2024<- ANES2024_partisanship |> select(-"1")|> select(-"2")
ANES_independents2024<- ANES2024_partisanship |> select(-"3")|> select(-"4")
ANES_independents2024$year <- as.numeric(ANES_independents2024$year)
ANES_partisans2024$year <- as.numeric(ANES_partisans2024$year)
#Adding 2024 to the full set for each

ANES_independents_pres_full <- bind_rows(ANES_independents_pres_recent, ANES_independents2024)
ANES_partisans_pres_full   <- bind_rows(ANES_partisans_pres_recent, ANES_partisans2024)


PartisanPlot3 <- ggplot(ANES_partisans_pres_full, aes(x = year)) +
  geom_line(aes(y = `3`, linetype = '"Weak" Identifiers')) +
  geom_line(aes(y = `4`, linetype = '"Strong" Identifiers')) +
  labs(title = "Proportions of National Election Study Sample", x = "", y = "Proportion", color = NULL, linetype = "Party ID Strength")+ 
  geom_point(aes(y = `3`), color = "black") +
  geom_point(aes(y = `4`), color = "black") +
  scale_y_continuous(limits = c(0, 0.5))+ 
  theme(plot.title = element_text(hjust = 0.5))
IndePlot3 <- ggplot(ANES_independents_pres_full, aes(x = year)) +
  geom_line(aes(y = `2`, linetype = 'Independent "Leaners"')) +
  geom_line(aes(y = `1`, linetype = '"Pure" Independents')) +
  labs( x = "Year", y = "Proportion", color = NULL, linetype = "Party ID Strength")+ 
  geom_point(aes(y = `1`), color = "black") +
  geom_point(aes(y = `2`), color = "black") +
  scale_y_continuous(limits = c(0, 0.5))


StackedPlots3 <- PartisanPlot3 / IndePlot3
StackedPlots3

#Create figures that include midterm election years as well.

Midterm_ANES_independents <- bind_rows(ANES_independents, ANES_independents2024)
Midterm_ANES_partisans    <- bind_rows(ANES_partisans, ANES_partisans2024)
Midterm_ANES_partisans    <- subset(Midterm_ANES_partisans, year %in% seq(1952, 2024, by = 2))
Midterm_ANES_independents <- subset(Midterm_ANES_independents, year %in% seq(1952, 2024, by = 2))


MidtermPartisanPlot <- ggplot(Midterm_ANES_partisans, aes(x = year)) +
  geom_line(aes(y = `3`, linetype = '"Weak" Identifiers')) +
  geom_line(aes(y = `4`, linetype = '"Strong" Identifiers')) +
  labs(title = "Proportions of National Election Study Sample", x = "", y = "Proportion", color = NULL, linetype = "Party ID Strength")+ 
  geom_point(aes(y = `3`), color = "black", size = 0.8) +
  geom_point(aes(y = `4`), color = "black", size = 0.8) +
  scale_y_continuous(limits = c(0, 0.5))+ 
  theme(plot.title = element_text(hjust = 0.5))
MidtermIndePlot <- ggplot(Midterm_ANES_independents, aes(x = year)) +
  geom_line(aes(y = `2`, linetype = 'Independent "Leaners"')) +
  geom_line(aes(y = `1`, linetype = '"Pure" Independents')) +
  labs( x = "Year", y = "Proportion", color = NULL, linetype = "Party ID Strength")+ 
  geom_point(aes(y = `1`), color = "black", size = 0.8) +
  geom_point(aes(y = `2`), color = "black", size = 0.8) +
  scale_y_continuous(limits = c(0, 0.5))


MidtermPlot <- MidtermPartisanPlot / MidtermIndePlot
MidtermPlot


#Original Work: explore the codebook for the ANES. Find two interesting variables and create compelling univariate graphs to illustrate their central tendency, distribution, and spread.
#Variable 2: Feelings toward big business over time (VCF0209)
ANES_BigBusiness <- ANES |>
  select(VCF0004, VCF0209) |>
  filter(VCF0209 <= 97) |>
  group_by(VCF0004) |>
  summarise(BigBiz = mean(VCF0209)) |>
  rename(year = VCF0004)

BusinessPlot <- ggplot(ANES_BigBusiness, aes(x = year, y = BigBiz)) +
  geom_line() +
  geom_point(color = "black") +
  labs(title = "Feelings Toward Big Business, 1964-2020",
       x = "Year", y = "Mean Thermometer (Degrees)") +
  scale_y_continuous(limits = c(0, 100))+ 
  theme(plot.title = element_text(hjust = 0.5))
BusinessPlot 


ANES_envparty <- ANES |>
  select(VCF0004, VCF9008) |>
  filter(VCF9008 %in% c(1, 3, 5)) |>
  group_by(VCF0004) |>
  summarise(Democrats = mean(VCF9008 == 1),
            Same = mean(VCF9008 == 3),
            Republicans = mean(VCF9008 == 5)) |>
  rename(year = VCF0004)
#Asked Claude how to set line colors 
EnvPartyPlot <- ggplot(ANES_envparty, aes(x = year)) +
  geom_line(aes(y = Democrats, color = "Democrats")) +
  geom_line(aes(y = Republicans, color = "Republicans")) +
  geom_line(aes(y = Same, color = "Same by both")) +
  geom_point(aes(y = Democrats), color = "black") +
  geom_point(aes(y = Republicans), color = "black") +
  geom_point(aes(y = Same), color = "black") +
  scale_color_manual(values = c("Democrats" = "blue",
                                "Republicans" = "red",
                                "Same by both" = "grey50"))+
  labs(title = "Which Party Would Better Handle the Environment?",
       x = "Year", y = "Proportion", color = "Party") +
  scale_y_continuous(limits = c(0, 1))+ 
  theme(plot.title = element_text(hjust = 0.5))
EnvPartyPlot
#Bonus: Replicate Figure 2 from Bartels as well; add in recent years and 2024.


