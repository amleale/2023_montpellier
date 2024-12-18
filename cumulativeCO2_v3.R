setwd("")
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(car) # for ANOVAs
library(lme4) # for ANOVAs
library(vegan) #for posthoc 
library(emmeans) #for posthoc

r01 <- readxl::read_xlsx(path="fermini_cumulativeCO2.xlsx", sheet = "r1_raw")%>%
  mutate(round = "r01")

r02 <- readxl::read_xlsx(path="fermini_cumulativeCO2.xlsx", sheet = "r2_raw")%>%
  mutate(round = "r02")

r03 <- readxl::read_xlsx(path="fermini_cumulativeCO2.xlsx", sheet = "r3_raw")%>%
  mutate(round = "r03")

r04 <- readxl::read_xlsx(path="fermini_cumulativeCO2.xlsx", sheet = "r4_raw")%>%
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

# combine in meta data of species absence/presence
META <- readxl::read_xlsx(path="fermini_cumulativeCO2.xlsx", sheet = "META")
df <- merge(df, META)

# figure 2  (good pdf size = 6x8 in )
# Calculate overall average and standard deviation for each Sc group
overall_avg <- df %>%
  filter(community != "C01" & community != "C10") %>%
  group_by(Sc, day) %>%
  summarise_at("CO2cum", c(mean, sd), na.rm = TRUE)

# Calculate overall average and standard deviation for each community and Sc group
overall_avg_community <- df %>%
  filter(community != "C01" & community != "C10") %>%
  group_by(community, Sc, day) %>%
  summarise_at("CO2cum", c(mean, sd), na.rm = TRUE)

# Plot individual community lines and average CO2cum for each Sc group
ggplot() +
  geom_line(
    data = overall_avg_community,
    aes(x = day, y = fn1, group = community, colour = Sc),
    size = 1, linetype = "solid") +
  geom_line(
    data = overall_avg,
    aes(x = day, y = fn1, group = Sc),
    colour = "black", linetype = "dashed", size = 1) +
  geom_errorbar(
    data = overall_avg,
    aes(x = day, ymin = fn1 - fn2, ymax = fn1 + fn2),
    colour = "black", width = 0.2) +  # Add error bars
  scale_colour_manual(
    name = expression(italic("S. cerevisiae?")),
    values = c("grey", "deepskyblue"),  
    labels = c("absent", "present")  
  ) +
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(0, 1, 2, 7, 14, 21)) +
  ylab(bquote("cumulative CO"[2] ~ "(g/L)")) +
  theme(legend.position = "right")  # Move legend to the top






### linear models
# supplementary table S7
df2 <- df %>% 
  # filter(Sc != "yes" ) %>% #remove if see rank still true in absence of Sc? 
  # filter(richness != 4 ) %>% # need to remove this for richness, because only 1 community, leads to perfect prediction
  filter(community != "C01" ) %>% #remove Sc control 
  filter(community != "C10" ) 

lm_all <- lm(CO2cum ~ Sc + Lt + Td + Sb + as.factor(richness) + day, data = df2) 
Anova(lm_all,  type = 3)
summary(lm_all)



### SPECIES / IDENTITY effect BY DAY
# supplementary Tables S4, S5, 
df2 <- df %>% 
  # filter(Sc != "yes" ) %>% #remove if see rank still true in absence of Sc? 
  # filter(richness != 4 ) %>% # need to remove this for richness, because only 1 community, leads to perfect prediction
  filter(community != "C01" ) %>% #remove Sc control 
  filter(community != "C10" ) %>%
  filter(day == 21 )


### LINEAR MODEL 
lm_all <- lm(CO2cum ~ Sc + Lt + Td + Sb + as.factor(richness) 
             +day,
             data = df2) 
# Anova(lm_all,  type = 3)
summary(lm_all)


#### PERMANOVA for spread in data 
# supplementary table S6
# check what you are filtering
df2 <- df %>% 
  # filter(Sc != "yes" ) %>% #remove if see rank still true in absence of Sc? 
  # filter(richness != 4 ) %>% # need to remove this for richness, because only 1 community, leads to perfect prediction
  filter(community != "C01" ) %>% #remove Sc control 
  filter(community != "C10" ) 

# # Remove rows with missing values in either 'CO2cum' or 'Sc'
df_no_missing <- na.omit(df2[c("CO2cum", "Sc")])

# # Calculate beta diversity using Euclidean distance
beta_diversity <- vegdist(df_no_missing$CO2cum, method = "euclidean")

# # Run PERMANOVA
result <- adonis2(beta_diversity ~ Sc, data = df_no_missing)
print(result)
summary(result)


#### Sc communities  (can't use community*day interaction...)
## Supplementary Table S8
sacc <- df %>%
  filter(Sc == "yes" ) %>%
  filter(community != "C01" ) %>% 
  filter(community != "C10" ) %>%
  filter(day == 07 )

lm_sacc <- lm(CO2cum ~ as.factor(richness), data = sacc )
Anova(lm_sacc , type = 3)  # no effect of community
summary(lm_sacc)








### these analyses are not currently used. 


# # T. del communities 
# td_comm <- df %>%
#   filter(Td == "no" ) %>%
#   filter(community != "C01" ) %>% 
#   filter(community != "C10" ) %>%
#   filter(day == 21 )
# 
# mean(td_comm $CO2cum)
# sd(td_comm $CO2cum)





#### Sc-free communities 
# saccfree <- df %>%
#   filter(Sc == "no" ) %>%
#   filter(community != "C01" ) %>% 
#   filter(community != "C10" ) %>%
#   filter(day == 7 )
# 
# lm_saccfree <- lm(CO2cum ~ community, data = saccfree)
# Anova(lm_saccfree , type = 3)  # no effect of community
# summary(lm_saccfree)
# 
# 
# 
# ### LEVENE's test (test spread in data)
# day1 <- df2 %>% filter(day == 1)
# day2 <- df2 %>% filter(day == 2)
# day7 <- df2 %>% filter(day == 7)
# day14 <- df2 %>% filter(day == 14)
# day21 <- df2 %>% filter(day == 21)
# 
# leveneTest(CO2cum ~ Sc, data = df2) # overall, across all days, no significant difference
# 
# leveneTest(CO2cum ~ Sc, data = day1) 
# leveneTest(CO2cum ~ Sc, data = day2)
# leveneTest(CO2cum ~ Sc, data = day7) #significant
# leveneTest(CO2cum ~ Sc, data = day14) #significant
# leveneTest(CO2cum ~ Sc, data = day21) #significant






# 
# #### plot average of when species is present or not (potential supplementary)
# df %>%
#   filter(community != "C01" ) %>%
#   filter(community != "C10" ) %>%
#   # filter(Sc == "no" ) %>% #if want to just looks at NS
#   group_by(day, Sb) %>%
#   summarise_at("CO2cum", c(mean, sd), na.rm = TRUE) %>%
#   ggplot(aes(x = day, y = fn1, 
#              col = Sb
#   )) +
#   geom_line(size=0.8) +
#   geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2))+
#   # facet_wrap(~community) +
#   theme_bw(base_size = 15) +
#   geom_hline(yintercept=100, size = 0.4, linetype='dotted') +
#   # theme(axis.text.x = element_text(angle = 90)) +
#   scale_x_continuous(breaks=c(0, 1, 2, 7, 14, 21))+
#   labs(y = "CO2 cumulative", 
#        # title = "non-Sc communities",
#        title = "all 15 communities"
#   ) +
#   ylab(bquote("cumulative CO"[2]~"(g/L)"))






