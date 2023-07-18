# v2: started june8 with all 4 rounds of data
# TO DO: try to merge meta file with yes/no for each species? 

setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2023_montpellier/experiments/weights")
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(readxl)

r01 <- readxl::read_xlsx(path="fermini_leale.xlsx", sheet = "r1_raw")%>%
  mutate(round = "r01")

r02 <- readxl::read_xlsx(path="fermini_leale.xlsx", sheet = "r2_raw")%>%
  mutate(round = "r02")

r03 <- readxl::read_xlsx(path="fermini_leale.xlsx", sheet = "r3_raw")%>%
  mutate(round = "r03")

r04 <- readxl::read_xlsx(path="fermini_leale.xlsx", sheet = "r4_raw")%>%
  mutate(round = "r04")


df <- rbind(r01, r02, r03, r04)

# rename columns, remove unneccesary column
df <- df%>% 
  rename(
   time = "Temps (h)",
    mass = "Masse (g)",
    volume = "Volume (L)",
 CO2cum = "CO2 cumulé (g/L)",
  CO2speed = "Vitesse CO2 (g/L/h)")  %>%
  select(-c("Prélèvement (ID)")) 

df<- df %>% drop_na(day)# confirm it eliminated correct rows (i.e., one value for each day/fermenter)

#make relevant values numeric
df$time <- as.numeric(df$time)
df$day <- as.numeric(df$day)
df$CO2cum <- as.numeric(df$CO2cum)

# # make a new column of sacc or not 
# df$comm2 <- gsub("C","", df$community) 
# df$comm2 <- as.numeric(df$comm2)
# df$sacc <- ifelse(df$comm2 < 10, "yes", "no")


# combine in meta data of species absence/presence
META <- readxl::read_xlsx(path="fermini_leale.xlsx", sheet = "META")
df <- merge(df, META)

  
df %>% 
  # filter(Sc == "no" ) %>%
  group_by(Sc, day, community) %>%
  summarise_at("CO2cum", c(mean, sd), na.rm = TRUE) %>%
  ggplot(aes(x = day, y = fn1, col = community
             , linetype=Sc
             # , linetype=Lt
             # , linetype=Td
             )) +
  geom_line(size=0.8) +
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2))+
  # facet_wrap(~community) +
  theme_bw(base_size = 15) +
  geom_hline(yintercept=100, size = 0.4, linetype='dotted') +
  # theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks=c(1, 2, 3, 7, 14, 21))+
  labs(y = "CO2 cumulative", title = "all 17 communities") 


model <- aov(CO2cum ~ Td*day, data = df)
summary(model)




# average of Td or Lt comm together (non-sacc comm)
df %>% 
  filter(Sc == "no" ) %>%
  group_by(day, Lt) %>%
  summarise_at("CO2cum", c(mean, sd), na.rm = TRUE) %>%
  ggplot(aes(x = day, y = fn1, col = Lt
             # , linetype=Sc
             # , linetype=Lt
             # , linetype=Td
  )) +
  geom_line(size=0.8) +
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2))+
  # facet_wrap(~community) +
  theme_bw(base_size = 15) +
  geom_hline(yintercept=100, size = 0.4, linetype='dotted') +
  # theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks=c(1, 2, 3, 7, 14, 21))+
  labs(y = "CO2 cumulative", title = "C10-C17 non-Sc communities") 


non_sacc <- df %>% filter(Sc == "no" )

model <- aov(CO2cum ~ Lt+Td+Sb+day, data = non_sacc)
summary(model)

