#v3: using new global "plate_count.xlsx" file where raw output 
# 6 april: include symbols/colour for round and expo/normal plating
# 4 may: solved zero values with: scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))


setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2023_montpellier/experiments/plate_counts")
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(readxl)
library(scales) #for "faux" logscale

day00.1 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d00_r01")
day07.1 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d07_r01")
day14.1 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d14_r01")
day21.1 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d21_r01")

day00.2 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d00_r02")
day07.2 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d07_r02")
day14.2 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d14_r02")
day21.2 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d21_r02")

day00.3 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d00_r03")
day07.3 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d07_r03")
day14.3 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d14_r03")
day21.3 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d21_r03")

day00.4 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d00_r04")
day07.4 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d07_r04")
day14.4 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d14_r04")
day21.4 <- readxl::read_xlsx(path="plate_counts.xlsx", sheet = "d21_r04")


## combine all separate raw files
cfus <- rbind(day00.1, day07.1, day14.1, day21.1, 
              day00.2, day07.2, day14.2, day21.2,
              day00.3, day07.3, day14.3, day21.3,
              day00.4, day07.4, day14.4, day21.4)

# rename columns, remove unneeded columns
cfus <- cfus %>% 
  rename(
    name = "N° échantillon",
    count = "Nbre d'UFC",
    CFU = "UFC/mL")  %>%
  select(-c("Prorata UFC", "Surface (%)", "Auto/Forcé", "Ø UFC moyen (mm)", "Ø UFC min (mm)", "Ø UFC max (mm)", "Date")) 

#make sure data are correctly factor / numeric
cfus$round <- as.factor(cfus$round)
cfus$community <- as.factor(cfus$community)
cfus$rep <- as.factor(cfus$rep)
cfus$method <- as.factor(cfus$method)
cfus$day <- as.factor(cfus$day)
cfus$CFU <- as.numeric(cfus$CFU)
cfus$Dilution <- as.numeric(cfus$Dilution)

cfus <- cfus %>% mutate(lawn = if_else(count=="lawn", "yes", "no"))



# adjust minimal CFUs for lawns, CFU = 0 when count is 0 only when dilution was 10^0
cfus <- cfus %>% 
  mutate(CFU = ifelse(count == "lawn" & Dilution == 10^0, 10^3, CFU)) %>%
  mutate(CFU = ifelse(count == "lawn" & Dilution == 10^-1, 10^4, CFU)) %>%
  mutate(CFU = ifelse(count == "lawn" & Dilution == 10^-2, 10^5, CFU)) %>%
  mutate(CFU = ifelse(count == "lawn" & Dilution == 10^-3, 10^6, CFU)) %>%
  mutate(CFU = ifelse(count == "lawn" & Dilution == 10^-4, 10^7, CFU)) %>%
  mutate(CFU = ifelse(count == 0 & Dilution == 1, 0, CFU)) %>%
  mutate(CFU = ifelse(count == 0 & Dilution == 0.1, 0.1, CFU))%>%
  mutate(CFU = ifelse(count == 0 & Dilution < 0.1, NA, CFU))
# cfus$CFU[is.na(cfus$count)& cfus$media == "plant"] <- 0
# cfus$CFU[is.na(cfus$count)& cfus$media == "brett"] <- 0


yeast <- filter(cfus, media == "yeast")
plant <- filter(cfus, media == "plant")
brett <- filter(cfus, media == "brett")


yeast %>%
  # filter(community != "C10" ) %>%
  # filter(community != "C08" ) %>%
  # filter(day != "d14" )%>%
  # filter(day != "d21" )%>%
  ggplot(aes(x = community, y = CFU)) + #need to get rid of zeros
  # geom_boxplot() +
  geom_jitter(aes(colour = round, shape = lawn), width = 0.2) +
  scale_colour_manual(values = c("skyblue", "deepskyblue2","deepskyblue3","deepskyblue4"))+
  geom_hline(yintercept = 10^6, linetype='dashed', col = 'grey', linewidth = 0.75) +
  # geom_hline(yintercept = log10(10^8), linetype='dashed', col = 'turquoise', linewidth = 1) +
  ggtitle("total yeast") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~day)+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                     breaks=c(0, 10^2, 10^4, 10^6, 10^8))



# # all combined
# yeast %>%
#   # filter(community != "C10" ) %>%
#   # filter(community != "C08" ) %>%
#   # filter(day == "d14" )%>%
#   # filter(day != "d21" )%>%
#   ggplot(aes(x = day, y = CFU)) + #need to get rid of zeros
#   # geom_boxplot() +
#   geom_jitter(aes(col = round, shape = method), width = 0.2, height = 0.2) +
#   geom_hline(yintercept = 10^6, linetype='dashed', col = 'lightblue', linewidth = 0.75) +
#   # geom_hline(yintercept = log10(10^8), linetype='dashed', col = 'turquoise', linewidth = 1) +
#   ggtitle("total yeast: PLATE COUNTS") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
#                      breaks=c(0, 10^2, 10^4, 10^6, 10^8))
# 



plant %>% 
  # filter(round == "r03") %>%
  # filter(method == "norm") %>%
  # filter(community != "C01" ) %>%
  # filter(day != "d14" )%>%
  # filter(day != "d21" )%>%
  ggplot(aes(x = community, y = CFU)) +
  # geom_boxplot() +
  geom_jitter(aes(col = round, shape = method), width = 0.2) +
  scale_colour_manual(values = c("violetred", "violetred2","violetred3", "violetred4"))+
  geom_hline(yintercept = 10^3, linetype='dashed', col = 'grey', linewidth = 0.75) +
  ggtitle("L. plantarum") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "italic")) +
  facet_wrap(~day) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                     breaks=c(0, 10^2, 10^4))


brett %>% 
  # filter(method == "norm") %>%
  # filter(count!= "lawn") %>%
  # filter(round == "r04") %>%
  # filter(day != "d21" )%>%
  ggplot(aes(x = community, y = CFU)) +
  # geom_boxplot() +
  geom_jitter(aes(col = round, shape = lawn), width = 0.2) +
  scale_colour_manual(values = c("lightgreen","green2", "green3","green4"))+
  geom_hline(yintercept = 10^3, linetype='dashed', col = 'grey', linewidth = 0.75) +
  ggtitle('B. bruxellensis') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "italic")) +
  facet_wrap(~day) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                     breaks=c(0, 10^2, 10^4, 10^6, 10^8))




# check differences between exponential vs. normal counts





