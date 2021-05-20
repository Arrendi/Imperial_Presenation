# Imperial_Presentation
Presentation made to interview panel at Imperial describing a problem and how it was resolved.
---
title: "Covid-19 Case Fatality Rate (CFR) in Belize:"
author: "Antonio M. Hegar, MPH"
date: '21st May, 2021'
output:
  xaringan::moon_reader:
    lib_dir: libs
    nature:
      titleSlideClass: ["center", "middle", "inverse"]
      highlightStyle: tomorrow-night-blue
      countIncrementalSlides: false
      nature:
      autoplay: 20000
      countdown: 20000
      highlightLines: true
---
class: left, top

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Background

--

The Central American Country of Belize recorded its first PCR confirmed case 
of Covid-19 on the 3rd of March, 2021. 

--

As of 17th May of this year the total number of confirmed Covid-19 cases nationally
stood at 12,742 with 323 deaths out of a estimated country population of 420,000 
for an overall case fatality rate of **2.53%**.

--

Nevertheless it has become apparent to clinicians and public health professionals
alike that there exists significant disparities in mortality among and between
demographic groups, but primarily age, gender, and ethnicity.

--

Given this reality, the Ministry of Health and Wellness of Belize needed to have
a firm understanding of the groups at greatest risk in order to formulate policy
and education campaigns, as well as to prioritise groups for vaccination.

---
class: center, middle
#The International Perspective


>Researchers and policy experts agree that sexdisaggregated data on COVID-19 
infection and case fatality rate are needed to develop gender-equitable
solutions to this pandemic...

>Maintenance of high-quality sex-disaggregated data is required to monitor these
differences across countries. To generate high-quality data, testing surveillance 
should be equitable, and should cover population subgroups of all socioeconomic
strata.

_A;, D. N. R. (2020, November 5). Sex differences in COVID-19 case fatality: do we know enough? The Lancet. Global health. <https://pubmed.ncbi.nlm.nih.gov/33160453/>_. 


---
class: left, top
## Objectives and Solution

Given the preceding, the primary objectives are to:

--

* Outline the national morbidity and mortality trends for the first year.

--

* Disaggregate the CFR by age, sex, ethnicity, and occupation.

--

* Calculate the age adjusted relative risk of death.

--

* Demonstrate the impact of interventions on morbidity and mortality.

---
class: center, middle
## Morbidity

```{r, echo = FALSE, message=FALSE, warning=FALSE, fig.width=12, fig.height=8}

setwd("C:/Users/ahegar/OneDrive/CovMoh/Spreadsheets/Spreadsheets/Data")
.libPaths(c("C:/Users/ahegar/OneDrive/CovMoh/R_Code/Packages",.libPaths()))
library(tidyverse)
library(scales)
library(readxl)
library(ggplot2)
library(visdat)
library(knitr)
library(rmarkdown)
library(lubridate)
theme_set(theme_minimal())

Lab_Rep <- read_excel("Lab_Results.xlsx", range = "A1:AP12743",
                      col_names = TRUE, na = "", col_types = NULL)

Cov.day <-  Lab_Rep %>%
  filter(active=="1") %>%
  group_by(proxy_swab_date) %>%
  tally() %>%
  filter(proxy_swab_date > "2020-02-28" & proxy_swab_date < "2021-03-02") %>%
  rename(Date = proxy_swab_date, Num. = n) %>%
  mutate(Cumulative = cumsum(Num.))

ggplot(Cov.day, aes(x=Date, y=Num.)) +
       geom_col(color = "dodgerblue3", size = 3) +
  stat_smooth( color = "#FC4E07", fill = "#FC4E07",
         method = "loess") +
  labs(title = "Graph of New Covid-19 Cases 1st March 2020 - 1st March 2021") +
  xlab("Date") + ylab("Covid Incidence Per Day") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_datetime(breaks = date_breaks("30 days"),
                   labels = date_format("%d/%m"))

```

---
class: center, middle
## Calculating CFR during an Ongoing Epidemic

### WHO Definition

**Case fatality ratio (CFR)** is the proportion of individuals diagnosed with a disease who die from that disease and is therefore a measure of severity among detected cases:

--

Case Fatality ratio (CFR, in%) =

--

$$
\frac{No.of.deaths.from.disease}{No.of.confirmed.cases.of.disease+No.recovered.from.disease}\cdot 100
$$

>_., W. H. O. (2021). Estimating mortality from COVID-19. World Health Organization. <https://www.who.int/news-room/commentaries/detail/estimating-mortality-from-covid-19>_.

---
class: center, middle
## Total Case Fatality Rate (CFR) by Age Group

```{r, echo = FALSE, message=FALSE, warning=FALSE, fig.width=12, fig.height=8}

setwd("C:/Users/ahegar/OneDrive/CovMoh/Spreadsheets/Spreadsheets/Data")
.libPaths(c("C:/Users/ahegar/OneDrive/CovMoh/R_Code/Packages",.libPaths()))
library(tidyverse)
library(scales)
library(readxl)
library(janitor)
library(visdat)
library(png)
library(DT)
library(reactable)
library(viridis)
theme_set(theme_minimal())

Lab_Rep <- read_excel("Lab_Results.xlsx", range = "A1:AP12380",
                      col_names = TRUE, na = "", col_types = NULL)

Cov2020 <- Lab_Rep %>%
           filter(proxy_swab_date < "2021-03-02" &
                  status != "Deceased - NONCOVID") %>%
           select(-first_name,-last_name,-ssi_number,
                  -interview__key,-pui__id,-lab_id)%>%
  mutate(Age.Groups = case_when(age >= 0  & age <= 04  ~ '0-04',
         age >= 5  & age <= 9  ~ '05-09',age >=10  & age <= 14 ~ '10-14',
         age >= 15 & age <= 19 ~ '15-19',age >= 20 & age <= 24 ~ '20-24',
         age >= 25 & age <= 30 ~ '25-30',age >= 31 & age <= 34 ~ '31-34',
         age >= 35 & age <= 39 ~ '35-39',age >= 40 & age <= 44 ~ '40-44',
         age >= 45 & age <= 49 ~ '45-49',age >= 50 & age <= 54 ~ '50-54',
         age >= 55 & age <= 59 ~ '55-59',age >= 60 & age <= 64 ~ '60-64',
         age >= 65 & age <= 69 ~ '65-69',age >= 70 & age <= 74 ~ '70-74',
         age >= 75 & age <= 79 ~ '75-79',age >= 80 & age <= 120 ~ '80+'))%>%
  mutate_if(is.character,as.factor)

#Calculating CFR for Covid by Sex and Age
CovCFR <- Cov2020 %>%
  filter(status != "Deceased") %>%
          group_by(Age.Groups) %>%
          summarise(AgeSum = n())

CovCFR2 <- Cov2020 %>%
           filter(status == "Deceased") %>%
           group_by(Age.Groups) %>%
           summarise(DeathSum = n())

AgeTotal <- aggregate(CovCFR$AgeSum, by=list(CovCFR$Age.Groups), FUN=sum)
DeathTotal <- aggregate(CovCFR2$DeathSum, by=list(CovCFR2$Age.Groups), FUN=sum)

CFR_Crude <- left_join(AgeTotal,DeathTotal, by="Group.1") %>%
             rename(Age_Group = Group.1, Total = x.x, Deaths = x.y) %>%
             replace_na(list(Deaths = 0))%>%
             mutate(CFR = round((Deaths/Total)*100,digits = 1)) 

CFR_Totals <- colSums(CFR_Crude[,2:4])

CFR_Final <- CFR_Crude %>%
             add_row(Age_Group = "Grand Total", Total = CFR_Totals[[1]],
             Deaths = CFR_Totals[[2]], CFR = CFR_Totals[[3]])

ggplot(CFR_Crude, aes(x=Age_Group, y=CFR)) +
  geom_col(color = "dodgerblue3", size = 3) +
  stat_smooth( color = "#FC4E07", fill = "#FC4E07") +
  geom_text(aes(label = CFR, vjust = -0.5), size = 4.5) +
  labs(
    title = "Total Case Fatality Rate in Belize for Covid-19 by Age Group",
    subtitle = "From 3rd March 2020 - 1st March 2021",
    caption = "Data from BHIS, Epi Unit, Belize MoH") +
  xlab("Age Group") + ylab("Case Fatality Rate (CFR) %") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
              

```
---
class: center, middle
##Female CFR

```{r, echo = FALSE, message=FALSE, warning=FALSE, fig.width=12, fig.height=8}

FCovCFR <- Cov2020 %>%
  filter(status != "Deceased") %>%
  filter(sex == "Female") %>%
  group_by(Age.Groups) %>%
  summarise(AgeSum = n()) 

FCovCFR2 <- Cov2020 %>%
  filter(status == "Deceased" & sex == "Female") %>%
  group_by(Age.Groups) %>%
  summarise(DeathSum = n())

FAgeTotal <- aggregate(FCovCFR$AgeSum, by=list(FCovCFR$Age.Groups), FUN=sum)
FDeathTotal <- aggregate(FCovCFR2$DeathSum,by=list(FCovCFR2$Age.Groups),FUN=sum)

FCFR_Final <- left_join(FAgeTotal,FDeathTotal, by="Group.1") %>%
  rename(Age_Group = Group.1, Total = x.x, Deaths = x.y) %>%
  replace_na(list(Deaths = 0))%>%
  mutate(CFR = round((Deaths/Total)*100,digits = 1))

ggplot(FCFR_Final, aes(x=Age_Group, y=CFR)) +
  geom_col(color = "brown1", size = 3) +
  stat_smooth( color = "#FC4E07", fill = "#FC4E07") +
  geom_text(aes(label = CFR, vjust = -0.5), size = 4.5) +
  labs(
    title = "Female Case Fatality Rate in Belize for Covid-19 by Age Group",
    subtitle = "From 3rd March 2020 - 1st March 2021",
    caption = "Data from BHIS, Epi Unit, Belize MoH") +
  xlab("Age Group") + ylab("Case Fatality Rate (CFR) %") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
```
---
class: center, middle
##Male CFR

```{r, echo = FALSE, message=FALSE, warning=FALSE, fig.width=12, fig.height=8}

MCovCFR <- Cov2020 %>%
  filter(status != "Deceased") %>%
  filter(sex == "Male") %>%
  group_by(Age.Groups) %>%
  summarise(AgeSum = n()) 

MCovCFR2 <- Cov2020 %>%
  filter(status == "Deceased" & sex == "Female") %>%
  group_by(Age.Groups) %>%
  summarise(DeathSum = n())

MAgeTotal <- aggregate(MCovCFR$AgeSum, by=list(MCovCFR$Age.Groups), FUN=sum)
MDeathTotal <- aggregate(MCovCFR2$DeathSum, by=list(MCovCFR2$Age.Groups), FUN=sum)

MCFR_Final <- left_join(MAgeTotal,MDeathTotal, by="Group.1") %>%
  rename(Age_Group = Group.1, Total = x.x, Deaths = x.y) %>%
  replace_na(list(Deaths = 0))%>%
  mutate(CFR = round((Deaths/Total)*100,digits = 1))

ggplot(MCFR_Final, aes(x=Age_Group, y=CFR)) +
  geom_col(color = "darkgoldenrod1", size = 3) +
  stat_smooth( color = "#FC4E07", fill = "#FC4E07") +
  geom_text(aes(label = CFR, vjust = -0.5), size = 4.5) +
  labs(
    title = "Male Case Fatality Rate in Belize for Covid-19 by Age Group",
    subtitle = "From 3rd March 2020 - 1st March 2021",
    caption = "Data from BHIS, Epi Unit, Belize MoH") +
  xlab("Age Group") + ylab("Case Fatality Rate (CFR) %") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
```
---
class: center, middle
##Ethnicity

```{r, echo = FALSE, message=FALSE, warning=FALSE, fig.width=12, fig.height=8}
EthCFR <- Cov2020 %>%
  filter(status != "Deceased") %>%
  group_by(ethnicity) %>%
  mutate(ethnicity=fct_collapse(ethnicity,
  Hispanic = c("Hispanic","Mestizo","Spanish","Latino"),
  Mayan = c("Yucatecan","Mopan","Ketchi","Mayan"))) %>%
  summarise(EthSum = n()) 

EthCFR2 <- Cov2020 %>%
  filter(status == "Deceased") %>%
  group_by(ethnicity) %>%
  mutate(ethnicity=fct_collapse(ethnicity,
  Hispanic = c("Hispanic","Mestizo","Spanish","Latino"),
  Mayan = c("Yucatecan","Mopan","Ketchi","Mayan"))) %>%
  summarise(DeathSum = n())

EthAgeT <- aggregate(EthCFR$EthSum, by=list(EthCFR$ethnicity),FUN=sum)
EthDeathT <- aggregate(EthCFR2$DeathSum, by=list(EthCFR2$ethnicity),FUN=sum)

Eth_Final <- left_join(EthAgeT,EthDeathT, by="Group.1") %>%
  rename(Ethnicity = Group.1, Total = x.x, Deaths = x.y) %>%
  replace_na(list(Deaths = 0))%>%
  mutate(CFR = round((Deaths/Total)*100,digits = 1))

ggplot(Eth_Final, aes(x=Ethnicity, y=CFR)) +
  geom_col(color = "magenta4", size = 3) +
  stat_smooth( color = "magenta4", fill = "#FC4E07") +
  geom_text(aes(label = CFR, hjust = -0.25), size = 5, fontface="bold") +
  labs(
    title = "Case Fatality Rate in Belize for Covid-19 by Ethnicity",
    subtitle = "From 3rd March 2020 - 1st March 2021",
    caption = "Data from BHIS, Epi Unit, Belize MoH") +
  xlab("Ethnicity") + ylab("Case Fatality Rate (CFR) %") +
  theme_gray() +
  theme(plot.title = element_text(hjust = -0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_flip()
```
---
class: center, middle
##Occupation

```{r, echo = FALSE, message=FALSE, warning=FALSE, fig.width=12, fig.height=8}
OcuCFR <- Cov2020 %>%
 filter(status != "Deceased") %>%
  mutate(Occupation=fct_collapse(occupation,
  Office.Worker = c(
  "Accountant","Accounts Clerk",
  "Administrative Assistant","Banker","Book Keepers",
  "Call Center Operator","Civil Servant","Clerk","Computer Programmer",
  "Computer Technicians","Director","Draftsman","Technicians",
  "Insurance Representative","Journalist","Manager","Meterologist",
  "Office Assistant","Printer","Receptionist","Secretary","Supervisor",
  "Technicians","Telephone Operator","Operator","Public Officer"),
  Arts.Creative= c("Architect","Artist","Entertainer","Musician","Painter",
  "Photographer","Radio Announcer"),
  Food.Beverage=c("Bakers","Bartender","Butchers","Chef","Cook",
  "Street Food Vendor"),
  Customer.Service=c("Barber","Beautician","Merchant","Messenger","Postman",
  "Sales Representative","Shop Salesperson","Tellers","Tourguides",
  "Waiter/Waitress"),
  Sports=c("Athletes/Sportsperson"),
  Construction.etc=c("Building Contractor","Carpenter","Surveyor",
  "Construction Worker","Electrician","Engineer","Mason"),
  Drivers.etc.=c("Bus Conductor","Conductors","Driver","Pilot","Taxi Driver",
                 "Transport Officers"),
  Self.Employed=c("Babysitter","Businessman","Businessperson","Catering",
  "Consultant","Electronics Repair","Hairdresser","Inspector/Detective",
  "Plumber","Seamstress","Street Vendor","Tailor","Jeweller"),
  Agriculture=c("Cane Cutters","Farm Labourer","Farmer","Fishermen",
                "Veterinarian"),
  Service.Sector=c("Cashier","Bank Clerk","Domestic Work","Lawyer",
  "Tax Collectors","Religious Professionals","School Warden","Security"),
  Clinical=c("Clinical Psychologist","Community Nurse Aide","Dentist","Nurse",
  "Hospital Worker","Medical Doctor","Medical Laboratory Technologist",
  "Pharmaceutical Assistant","Pharmacist","Physiotherapist","X-ray Technician"),
  Defence=c("Coast Guard","Fire Fighters","Forestry Worker","Police Officer",
  "Soldier","Watchman"),
  Frontline.Other=c("Customs/Border Inspector","Healthcare Administrator",
  "Immigration Officer","Prison Guards","Public Health Inspector",
  "Social Worker","Vector Control Technician"),
  Industrial.Manual=c("Factory Worker","Foreman","Garbage Collector","Sewers",
  "Janitor","Labourer","Maintenance","Mechanic","Miner","Sanitarians",
  "Stevedore","Welders","Cleaner"),
  Domestic=c("Housewife","Retired"),
  Education=c("Student","Teacher"))) %>%
  group_by(Occupation) %>%
  summarise(OcuSum = n()) 

OcuCFR2 <- Cov2020 %>%
  filter(status == "Deceased") %>%
  mutate(Occupation=fct_collapse(occupation,
  Office.Worker = c("Accountant","Accounts Clerk",
  "Administrative Assistant","Banker","Book Keepers",
  "Call Center Operator","Civil Servant","Clerk","Computer Programmer",
  "Computer Technicians","Director","Draftsman","Technicians",
  "Insurance Representative","Journalist","Manager","Meterologist",
  "Office Assistant","Printer","Receptionist","Secretary","Supervisor",
  "Technicians","Telephone Operator","Operator","Public Officer"),
  Arts.Creative= c("Architect","Artist","Entertainer","Musician","Painter",
  "Photographer","Radio Announcer"),
  Food.Beverage=c("Bakers","Bartender","Butchers","Chef","Cook",
  "Street Food Vendor"),
  Customer.Service=c("Barber","Beautician","Merchant","Messenger","Postman",
  "Sales Representative","Shop Salesperson","Tellers","Tourguides",
  "Waiter/Waitress"),
  Sports=c("Athletes/Sportsperson"),
  Construction.etc=c("Building Contractor","Carpenter","Surveyor",
  "Construction Worker","Electrician","Engineer","Mason"),
  Drivers.etc.=c("Bus Conductor","Conductors","Driver","Pilot","Taxi Driver",
  "Transport Officers"),
  Self.Employed=c("Babysitter","Businessman","Businessperson","Catering",
  "Consultant","Electronics Repair","Hairdresser","Inspector/Detective",
  "Plumber","Seamstress","Street Vendor","Tailor","Jeweller"),
  Agriculture=c("Cane Cutters","Farm Labourer","Farmer","Fishermen",
  "Veterinarian"),
  Service.Sector=c("Cashier","Bank Clerk","Domestic Work","Lawyer",
  "Tax Collectors","Religious Professionals","School Warden","Security"),
  Clinical=c("Clinical Psychologist","Community Nurse Aide","Dentist","Nurse",
  "Hospital Worker","Medical Doctor","Medical Laboratory Technologist",
  "Pharmaceutical Assistant","Pharmacist","Physiotherapist","X-ray Technician"),
  Defence=c("Coast Guard","Fire Fighters","Forestry Worker","Police Officer",
  "Soldier","Watchman"),
  Frontline.Other=c("Customs/Border Inspector","Healthcare Administrator",
  "Immigration Officer","Prison Guards","Public Health Inspector",
  "Social Worker","Vector Control Technician"),
  Industrial.Manual=c("Factory Worker","Foreman","Garbage Collector","Sewers",
  "Janitor","Labourer","Maintenance","Mechanic","Miner","Sanitarians",
  "Stevedore","Welders","Cleaner"),
  Domestic=c("Housewife","Retired"),
  Education=c("Student","Teacher"))) %>%
  group_by(Occupation) %>%
  summarise(DeathSum = n())

OcuTotal <- aggregate(OcuCFR$OcuSum, 
                      by=list(OcuCFR$Occupation), FUN=sum)
OcuDeath <- aggregate(OcuCFR2$DeathSum, 
                      by=list(OcuCFR2$Occupation), FUN=sum)

Ocu_Final <- left_join(OcuTotal,OcuDeath, by="Group.1") %>%
  rename(Occupation = Group.1, Total = x.x, Deaths = x.y) %>%
  replace_na(list(Deaths = 0))%>%
  mutate(CFR = round((Deaths/Total)*100,digits = 1))

ggplot(Ocu_Final, 
       aes(reorder(Occupation ,-CFR),CFR, fill = Occupation )) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = CFR, hjust = 0.05), size = 3, fontface="bold") +
  labs(
    title = "Covid-19 Case Fatality Rate by Occupation", 
    x = "Occupation", y = "CFR %", face = "bold",
    subtitle = "From 3rd March 2020 - 31st March 2021",
    caption = "Data from BHIS, Epi Unit, Belize MoH")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="top") +
  coord_flip()

```
---
class: center, middle
## Calculating the Relative Risk

```{r, echo = FALSE, message=FALSE, warning=FALSE, fig.width=12, fig.height=8}
setwd("C:/Users/ahegar/OneDrive/CovMoh/Spreadsheets/Spreadsheets/Data")

TotalDeaths <- read_excel("CombinedDeath.xlsx", range = "A1:AO10949",
                      col_names = TRUE, na = "", col_types = NULL)

DeathAge <- TotalDeaths %>%
  mutate(Age_Group = case_when(age >= 0  & age <= 04  ~ '0-04',
         age >= 5  & age <= 9  ~ '05-09',age >=10  & age <= 14 ~ '10-14',
         age >= 15 & age <= 19 ~ '15-19',age >= 20 & age <= 24 ~ '20-24',
         age >= 25 & age <= 30 ~ '25-30',age >= 31 & age <= 34 ~ '31-34',
         age >= 35 & age <= 39 ~ '35-39',age >= 40 & age <= 44 ~ '40-44',
         age >= 45 & age <= 49 ~ '45-49',age >= 50 & age <= 54 ~ '50-54',
         age >= 55 & age <= 59 ~ '55-59',age >= 60 & age <= 64 ~ '60-64',
         age >= 65 & age <= 69 ~ '65-69',age >= 70 & age <= 74 ~ '70-74',
         age >= 75 & age <= 79 ~ '75-79',age >= 80 & age <= 120 ~ '80+'))%>%
  mutate_if(is.character,as.factor) 

PopAgeEst <- c( 227674,236131,226914,208544,183826,159534,137451,124464, 
                104380,90002,70554,51994,37448,26460,21035,15422,18861)

DeathAgeGroups <- DeathAge %>%
                  group_by(Age_Group)%>%
                  tally() %>% rename(Age.Tally = n)

NationalDeath <- cbind(DeathAgeGroups,PopAgeEst) %>%
                 mutate(TotalCFR = round((Age.Tally/PopAgeEst)*100,digits = 2))
Deathsums <- colSums(NationalDeath[,2:4])
NatDeaths <- NationalDeath %>%
             add_row(Age_Group = "Grand Total", Age.Tally = Deathsums[[1]],
             PopAgeEst = Deathsums[[2]], TotalCFR = Deathsums[[3]])

#Calculation of CFR and RR from merged database
Covid.RR <- left_join(CFR_Final,NatDeaths, by = "Age_Group") %>%
            mutate(CFRDiff = (CFR - TotalCFR)*100,
            RR = round(CFR/TotalCFR,digits = 2)) %>%
            select(-Age.Tally,-PopAgeEst,-CFRDiff)

datatable(Covid.RR, class = 'cell-border stripe')

```
---
class: center, middle
## The Outcome

![Vaccine Schedule](https://bit.ly/33YxYw7)

---
class: center, middle
## Impact of Interventions and Policy Changes
### Belize Ro Since Inception of the Pandemic

```{r, echo = FALSE, message=FALSE, warning=FALSE, fig.width=12, fig.height=8}
setwd("C:/Users/ahegar/OneDrive/CovMoh/Spreadsheets/Spreadsheets/Data")
library(EpiEstim)
library(lubridate)

Lab_Rep <- read_excel("Lab_Results.xlsx", range = "A1:AP12743",
                      col_names = TRUE, na = "", col_types = NULL)

#Two-week interval Rt Calculation 
CI <- Lab_Rep %>%
  filter(active == "1" & 
           proxy_swab_date > "2020-02-28") %>%
  group_by(proxy_swab_date) %>%
  tally() %>%
  rename(I = n, Swab_Date = proxy_swab_date)%>%
  na.omit()

CIdf <- data.frame(CI)

CIdf$Swab_Date <-as.POSIXct(CIdf$Swab_Date,"%Y/%m/%d")

Time <- nrow(CI)
t_start <- seq(2, Time - 13) # starting at 2 as conditional on the past observations
t_end <- t_start + 13 # adding 6 to get 7-day windows as bounds included in window

Nat_Rs <- estimate_R(CIdf, 
                     method = "parametric_si", 
                     config = make_config(list(
                       t_start = t_start,
                       t_end = t_end,
                       mean_si = 5, 
                       std_si = 2.3)))

Cov.dates <- seq(as.Date("2020/03/01"),by="day",length.out = length(Nat_Rs$R[[8]]))

plot(Cov.dates, Nat_Rs$R[[8]],type="b",pch = 20,main = "Plot of National Ro",cex=1.5,
     col = "darkred",xlab = "Date", ylab = "Ro Value", ylim = c(0,4.5), xaxt = "n")
axis(1, Cov.dates, format(Cov.dates, "%Y-%m-%d"))
lines(Cov.dates,Nat_Rs$R[[5]],col = "deepskyblue", type = "b", lty = 2)
lines(Cov.dates,Nat_Rs$R[[11]],col = "darkgreen", type = "b", lty = 2)
abline(h=1, col="black", lty=4)
legend("topleft", legend=c("Upper Value", "Median Value", "Lower Value","Threshold"),
       col=c("darkgreen", "darkred","deepskyblue","black"), lty= c(2,1,2,4), cex=0.8)


```
---
class: center, middle
## 14 Day Rolling Average Since Beginning of 2021

```{r, echo = FALSE, message=FALSE, warning=FALSE, fig.width=12, fig.height=8}
setwd("C:/Users/ahegar/OneDrive/CovMoh/Spreadsheets/Spreadsheets/Data")
Lab_Rep <- read_excel("Lab_Results.xlsx", range = "A1:AP12743",
                      col_names = TRUE, na = "", col_types = NULL)

Cov.day <-  Lab_Rep %>%
  filter(active=="1") %>%
  group_by(proxy_swab_date) %>%
  tally() %>%
  filter(proxy_swab_date > "2020-12-31") %>%
  rename(Date = proxy_swab_date, Num. = n) %>%
  mutate(Cumulative = cumsum(Num.))

# Rolling Average of Positives
Cov.Roll <- Cov.day %>%
  mutate(Move.Avg3 = zoo::rollmean(Num.,k=3,fill=NA),
         Move.Avg7 = zoo::rollmean(Num.,k=7,fill=NA),
         Move.Avg14 = zoo::rollmean(Num.,k=14,fill=NA)) %>%
  ungroup()

# 14 Day Average
ggplot(Cov.Roll, aes(x=Date, y=Move.Avg14)) +
  geom_col(color = "dodgerblue3", size = 1) +
  stat_smooth( color = "#FC4E07", fill = "#FC4E07",
               method = "loess") +
  geom_text(aes(label = round(Move.Avg7), vjust = -0.5), size = 2) +
  labs(title ="14 Day Rolling Average of Covid-19 Cases 1st Jan.- 17th May 2021") +
  xlab("Date") + ylab("Positive Cases Detected") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(16.5,7.5,16.5,7.5,"pt"))+
  scale_x_datetime(breaks = date_breaks("14 days"),
                   labels = date_format("%d/%m"))

```
---
class: center, middle
## Mortality

```{r, echo = FALSE, message=FALSE, warning=FALSE, fig.width=12, fig.height=8}

setwd("C:/Users/ahegar/OneDrive/CovMoh/Spreadsheets/Spreadsheets/Data")
.libPaths(c("C:/Users/ahegar/OneDrive/CovMoh/R_Code/Packages",.libPaths()))
library(tidyverse)
library(readxl)
library(scales)
library(smoother)
library(visdat)
library(ggpubr)
library(forcats)
theme_set(theme_pubr())

Lab_Rep <- read_excel("Lab_Results.xlsx", range = "A1:AP12683",
                      col_names = TRUE, na = "", col_types = NULL)

Cat.Death <- Lab_Rep %>%
  filter(date_of_death >= "2020-02-28" & status == "Deceased")%>%
  select(-interview__key,-pui__id,-comments,-recovered_date,
         -maiden_name,-middle_name) %>%
  mutate_if(is.character,as.factor) 

#Timeline by numbers and cumulative
Death.day <-  Cat.Death %>%
  group_by(date_of_death,sex) %>%
  tally() %>%
  filter(date_of_death > "2020-02-28") %>%
  rename(Date = date_of_death, Num. = n, Gender = sex) 

ggplot(Death.day, aes(x=Date, y=Num., fill=Gender)) + 
  geom_bar(stat="identity") +  
  labs(title = "Covid-19 Deaths in Belize",
       subtitle = "From 1st March 2020 - 17th May 2021",
       caption = "Data from BHIS, Epi Unit, Belize MoHW") +
  xlab("Date") + ylab("No.of Deaths") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_x_datetime(breaks = date_breaks("60 days"),
                   labels = date_format("%d/%m")) 

```

