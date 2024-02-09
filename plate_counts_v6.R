## from v6: 
# making heatplots of absence/presence frequency 
# 

# still from v5:
# 13-dec-2023  trying to do fishers exact test
# 19-dec-2023 trying binomial regression when remove C09 (only one with richness =4)

setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2023_montpellier/experiments/data/plate_counts")
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(readxl)
library(scales) #for "faux" logscale
library(patchwork)#for combining plots

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

# combine in meta data of species absence/presence, richness
META <- readxl::read_xlsx(path="~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2023_montpellier/experiments/data/weights/fermini_leale.xlsx", sheet = "META")
cfus <- merge(cfus, META)

# make note of if lawn or not
cfus <- cfus %>% mutate(lawn = if_else(count=="lawn", "yes", "no"))

# ensure 'round' and 'day' are factors
cfus$round <- as.factor(cfus$round)
cfus$day <- as.factor(cfus$day)

# adjust minimal CFUs for lawns, CFU = 0 when count is 0 only when dilution was 10^0
# NEW DATA FRAME [cfus2]
cfus2 <- cfus %>% 
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

# make separate for plotting /
yeast <- filter(cfus2, media == "yeast")
plant <- filter(cfus2, media == "plant")
brett <- filter(cfus2, media == "brett")

##### DOT PLOTS ######
yeast %>%
  # filter(community != "C10" ) %>%
  # filter(community != "C08" ) %>%
  # filter(day != "d14" )%>%
  # filter(day != "d21" )%>%
  # mutate(day = recode(day, "d00" = "day 0", "d07" = "day 7", "d14" = "day 14", "d21" = "day 21")) %>%
  filter(!is.na(lawn)) %>%
  ggplot(aes(x = community, y = CFU)) + #need to get rid of zeros
  # geom_boxplot() +
  geom_jitter(aes(colour = round, shape = lawn), width = 0.2) +
  scale_colour_manual(
    values = c("skyblue", "deepskyblue2","deepskyblue3","deepskyblue4"),
    breaks = c("r01","r02","r03","r04"),
    labels = c("1","2","3","4")
    ) +
  geom_hline(yintercept = 10^6, linetype='dashed', col = 'grey', linewidth = 0.75) +
  # geom_hline(yintercept = log10(10^8), linetype='dashed', col = 'turquoise', linewidth = 1) +
  # ggtitle("total yeast") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~day)+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                     breaks=c(0, 10^2, 10^4, 10^6, 10^8)) +
  labs(color = "replicate", shape = "lawn?", y = "CFU/mL") 


plant %>% 
  # filter(round == "r03") %>%
  # filter(method == "norm") %>%
  # filter(community != "C01" ) %>%
  # filter(day != "d14" )%>%
  # filter(day != "d21" )%>%
  # mutate(day = recode(day, "d00" = "day 0", "d07" = "day 7", "d14" = "day 14", "d21" = "day 21")) %>%
  filter(!is.na(lawn)) %>%
  ggplot(aes(x = community, y = CFU)) +
  # geom_boxplot() +
  geom_jitter(aes(col = round, 
                  # shape = method
                  ), 
              width = 0.2) +
  scale_colour_manual(
    values = c("violetred", "violetred2","violetred3", "violetred4"),
    breaks = c("r01","r02","r03","r04"),
    labels = c("1","2","3","4")
    )+
  geom_hline(yintercept = 10^3, linetype='dashed', col = 'grey', linewidth = 0.75) +
  # ggtitle("L. plantarum") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "italic")) +
  facet_wrap(~day) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                     breaks=c(0, 10^2, 10^4)) +
  labs(color = "replicate", y = "CFU/mL") 


brett %>% 
  # filter(method == "norm") %>%
  # filter(count!= "lawn") %>%
  # filter(round == "r04") %>%
  # filter(day != "d21" )%>%
  # mutate(day = recode(day, "d00" = "day 0", "d07" = "day 7", "d14" = "day 14", "d21" = "day 21")) %>%
  filter(!is.na(lawn)) %>%
  ggplot(aes(x = community, y = CFU)) +
  # geom_boxplot() +
  geom_jitter(aes(col = round, shape = lawn), width = 0.2) +
  scale_colour_manual(values = c("lightgreen","green2", "green3","green4"),
                      breaks = c("r01","r02","r03","r04"),
                      labels = c("1","2","3","4")
                      )+
  geom_hline(yintercept = 10^3, linetype='dashed', col = 'grey', linewidth = 0.75) +
  # ggtitle('B. bruxellensis') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "italic")) +
  facet_wrap(~day) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                     breaks=c(0, 10^2, 10^4, 10^6, 10^8)) +
  labs(color = "replicate", shape = "lawn?", y = "CFU/mL") 


#### HEATMAPS OF ABSENCE/PRESENCE #### 
binary <- cfus2 %>%  
  mutate(present = ifelse(CFU > 0, 1, 0)) %>% # turn CFU counts into binary
  filter(!is.na(present)) %>% # remove if NAs,
  # filter(media == "yeast") %>% # filter for selective media type
  # filter(media == "brett") %>% # filter for selective media type
  filter(media == "plant") %>% # filter for selective media type
  filter(community != "C01") %>% # remove control of only Sc
  filter(community != "C10") # remove control of only Bb and Lt

binary_freq <- binary %>%
  group_by(community, day) %>%
  summarise(
    Mean_Value = mean(present)
  )

# for total YEAST
plotA <- ggplot(binary_freq, 
       aes(y = community, x = day, fill = Mean_Value)) +
  geom_tile(colour = "grey") +
  scale_fill_gradient(low = "white", high = "black", name = "proportion non-empty plates") +
  labs(title = "viable resident yeast", y = "Community") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")


# for B.BRUX
plotB <- ggplot(binary_freq, 
       aes(y = community, x = day, fill = Mean_Value)) +
  geom_tile(colour = "grey") +
  scale_fill_gradient(low = "white", high = "black", name = "proportion non-empty plates",
                      labels = scales::number_format(accuracy = 0.1),
                      breaks = c(0, 0.5, 1)) +
  labs(title = expression(italic("B. bruxellensis")), y = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")


# for L.PLANT
plotC <- ggplot(binary_freq, 
                aes(y = community, x = day, fill = Mean_Value)) +
  geom_tile(colour = "grey") +
  scale_fill_gradient(low = "white", high = "black", name = "proportion non-empty plates") +
  labs(title = expression(italic("L. plantarum")), y = "") + 
  theme_bw(base_size = 14) +
  theme(legend.position = "none")


# Combine plots (good size = 8 x 14 in)
combined_plots <- plotA + plotB + plotC + plot_layout(ncol = 3)
combined_plots


#### FISHERS EXACT TEST #######
# (need to do separate for media x day x species)
binary <- cfus2 %>%
  mutate(present = ifelse(CFU > 0, "yes", "no")) %>% # turn CFU counts into binary
  filter(!is.na(present)) %>% # remove if NAs,
  filter(day == "d21") %>% #### PROBLEM - can't work work everything is absent/all present (so not at d00, d07)
  filter(media == "yeast") %>% # filter for selective media type
  filter(community != "C01") %>% # remove control of only Sc
  filter(community != "C10") %>%# remove control of only Bb and Lt
  filter(Sc != "yes") # if performing tests only on Sc-free communities (i.e., avoid confounding effect of richness & Sc)

#for species effects 
binary2 <- binary %>%  #[issue with Td effect on Bb at d14, d21]
  # select(present, richness)
  # select(present, Sc)
  # select(present, Lt)
  # select(present, Td)
  select(present, Sb)

table(binary2)

# Fisher's exact test with raw data
test <- fisher.test(table(binary2))
test 

# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  binary2, present, 
  # Sc,
  Lt,
  # Td,
  # Sb,
  # richness,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.001, "< 0.001", round(test$p.value, 3))
  )
)


#Different visualisation in ggplot
contingency_table <- table(
  # binary$Sc,
  # binary$Lt, 
  # binary$Td, 
  # binary$Sb, 
  binary$richness, 
  binary$present)

# Convert the contingency table to a data frame
df <- as.data.frame(as.table(contingency_table))
# new_colnames <- c("S.bacillarus", "present", "Freq")  # Replace with your desired names
new_colnames <- c("S.cerevisiae", "present", "Freq")  # Replace with your desired names
# new_colnames <- c("T.delbreuckii", "present", "Freq")  # Replace with your desired names
# new_colnames <- c("L.thermotolerans", "present", "Freq")  # Replace with your desired names
# new_colnames <- c("richness", "present", "Freq")  # Replace with your desired names
colnames(df) <- new_colnames

# Create a mosaic plot using ggplot2
ggplot(df, 
       # aes(x = S.bacillarus, y = present, fill = Freq)
       aes(x = S.cerevisiae, y = present, fill = Freq)
       # aes(x = T.delbreuckii, y = present, fill = Freq)
       # aes(x = L.thermotolerans, y = present, fill = Freq)
       # aes(x = richness, y = present, fill = Freq)
       ) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = Freq), vjust = 1.5) +
  # scale_fill_gradient(low = "lightblue", high = "darkblue") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  # scale_fill_gradient(low = "lightpink", high = "deeppink") +
  theme_minimal() +
  labs(title = "B. bruxellensis, day 14", x = "S.cerevisiae", y = "present")








## alanna playing around 19-dec-2023
#confirm data you want to use 
binary2 <- cfus2 %>%
  mutate(present = ifelse(CFU > 0, 1, 0)) %>% # turn CFU counts into binary
  filter(!is.na(present)) %>% # remove if NAs,
  filter(media == "brett") %>%
  filter(day != "d00") %>% # brett only polymorph at day07, 14, 21
  # filter(media == "plant") %>%
  # filter(day == "d07") %>% # plantarum only polymorph at day07
  # filter(media == "yeast") %>%
  # filter(day == "d14" | "d21") %>% # yeast only polymorph at day14, 21
  filter(community != "C01") %>% # remove control of only Sc
  filter(community != "C10") # remove control of only Bb and Lt


# # Fit the GLMM using glmmTMB
model <- glm(present ~ Sc, data = binary2, family = binomial) # need to incorporate random effect of round
summary(model) # no effect of Sc on brett??? that doesn't seem right. 

model <- glm(present ~ Sc + day, data = binary2, family = binomial) # need to incorporate random effect of round
summary(model)




# model <- glm(present ~ Sc + (1|round), data = binary2, family = binomial) # random effect of round not working 

# model <- glm(absent ~ Sc + (1|round) + (1|day),
#                  data = binary, family = binomial,
#                  control = glmmTMBControl(optimizer = optim, optCtrl = list(maxfun = 1000)))









# check differences between exponential vs. normal counts





