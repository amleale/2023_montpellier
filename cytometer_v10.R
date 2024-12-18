
##############################################################################################################
library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car) # for ANOVAs
library(lme4) # for ANOVAs
library(vegan) #for posthoc 
library(emmeans) #for posthoc
library(patchwork)#for combining plots
library(ggpubr) # for t-test p-values on plots


# Loading and transforming data #####
# remember to rename top of columns in excel file (DONE)
setwd("")

# until script line 160 is data import
#round 1 import
day00.1 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r1_d00") 
day00.1 <- day00.1 %>%
  mutate(day = "d00") %>%
  mutate(round = "r01")

day01.1 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r1_d01") 
day01.1 <- day01.1 %>%
  mutate(day = "d01") %>%
  mutate(round = "r01")

day02.1 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r1_d02") 
day02.1 <- day02.1 %>%
  mutate(day = "d02") %>%
  mutate(round = "r01")
  
day07.1 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", "r1_d07")
day07.1 <- day07.1 %>%
  mutate(day = "d07") %>%
  mutate(round = "r01")

day14.1 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r1_d14")
day14.1 <- day14.1 %>%
  mutate(day = "d14") %>%
  mutate(round = "r01")

day21.1 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r1_d21")
day21.1 <- day21.1 %>%
  mutate(day = "d21") %>%
  mutate(round = "r01")


#round 2 import
day00.2 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r2_d00") 
day00.2 <- day00.2 %>%
  mutate(day = "d00") %>%
  mutate(round = "r02")

day01.2 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r2_d01") 
day01.2 <- day01.2 %>%
  mutate(day = "d01") %>%
  mutate(round = "r02")

day02.2 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r2_d02") 
day02.2 <- day02.2 %>%
  mutate(day = "d02") %>%
  mutate(round = "r02")

day07.2 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", "r2_d07")
day07.2 <- day07.2 %>%
  mutate(day = "d07") %>%
  mutate(round = "r02")

day14.2 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r2_d14")
day14.2 <- day14.2 %>%
  mutate(day = "d14") %>%
  mutate(round = "r02")

day21.2 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r2_d21")
day21.2 <- day21.2 %>%
  mutate(day = "d21") %>%
  mutate(round = "r02")


#round 3 import
day00.3 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r3_d00")
day00.3 <- day00.3 %>%
  mutate(day = "d00") %>%
  mutate(round = "r03")

day01.3 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r3_d01")
day01.3 <- day01.3 %>%
  mutate(day = "d01") %>%
  mutate(round = "r03")

day02.3 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r3_d02")
day02.3 <- day02.3 %>%
  mutate(day = "d02") %>%
  mutate(round = "r03")

day07.3 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", "r3_d07")
day07.3 <- day07.3 %>%
  mutate(day = "d07") %>%
  mutate(round = "r03")

day14.3 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r3_d14")
day14.3 <- day14.3 %>%
  mutate(day = "d14") %>%
  mutate(round = "r03")

day21.3 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r3_d21")
day21.3 <- day21.3 %>%
  mutate(day = "d21") %>%
  mutate(round = "r03")


#round 4 import
day00.4 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r4_d00")
day00.4 <- day00.4 %>%
  mutate(day = "d00") %>%
  mutate(round = "r04")

day01.4 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r4_d01")
day01.4 <- day01.4 %>%
  mutate(day = "d01") %>%
  mutate(round = "r04")

day02.4 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r4_d02")
day02.4 <- day02.4 %>%
  mutate(day = "d02") %>%
  mutate(round = "r04")

day07.4 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", "r3_d07")
day07.4 <- day07.4 %>%
  mutate(day = "d07") %>%
  mutate(round = "r04")

day14.4 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r4_d14")
day14.4 <- day14.4 %>%
  mutate(day = "d14") %>%
  mutate(round = "r04")

day21.4 <- readxl::read_xlsx(path="cytometer_raw_EXCEL.xlsx", sheet = "r4_d21")
day21.4 <- day21.4 %>%
  mutate(day = "d21") %>%
  mutate(round = "r04")

# combine all dataframes together
mydata <- rbind(day00.1, day01.1, day02.1, day07.1, day14.1, day21.1, 
                day00.2, day01.2, day02.2, day07.2, day14.2, day21.2,
                day00.3, day01.3, day02.3, day07.3, day14.3, day21.3,
                day00.4, day01.4, day02.4, day07.4, day14.4, day21.4 )


# transformation of the table
vec <- which(!is.na(mydata$parameter1))
for(i in 1:(length(vec))){
  val1 <- vec[i]
  val2 <- (vec[i+1])-1
  
  if(i == length(vec)){
    mydata$parameter1[val1:nrow(mydata)] <- mydata$parameter1[val1]
  }else{
    mydata$parameter1[val1:val2] <- mydata$parameter1[val1]
  }
}


vec2 <- which(!is.na(mydata$parameter2))
for(i in 1:(length(vec2))){
  val1 <- vec2[i]
  val2 <- (vec2[i+1])-1
  
  if(i == length(vec2)){
    mydata$parameter2[val1:nrow(mydata)] <- mydata$parameter2[val1]
  }else{
    mydata$parameter2[val1:val2] <- mydata$parameter2[val1]
  }
}


vec3 <- which(!is.na(mydata$groups))
for(i in 1:(length(vec3))){
  val1 <- vec3[i]
  val2 <- (vec3[i+1])-1
  
  if(i == length(vec3)){
    mydata$groups[val1:nrow(mydata)] <- mydata$groups[val1]
  }else{
    mydata$groups[val1:val2] <- mydata$groups[val1]
  }
}

vec4 <- which(!is.na(mydata$gates))
for(i in 1:(length(vec4))){
  val1 <- vec4[i]
  val2 <- (vec4[i+1])-1
  
  if(i == length(vec4)){
    mydata$gates[val1:nrow(mydata)] <- mydata$gates[val1]
  }else{
    mydata$gates[val1:val2] <- mydata$gates[val1]
  }
}

mytidy <- mydata %>% slice(6:n())

#remove junk rows & repeat columns
myclean <- mytidy %>%
  filter(Sample != "Sample") %>%
  filter(Group != "NA") %>% 
  select(-c("Group", "Workspace", "Autogate Status" ))#check that it is all "Group" workspace 

#renaming things
myclean <- myclean %>% 
  mutate(gates = str_replace(gates,  "Gate: Bb", "Bb")) %>%  
  mutate(gates = str_replace(gates,  "Gate: Brett?", "Brett")) %>%  
  mutate(gates = str_replace(gates,  "Gate: Live", "Live")) %>%
  mutate(gates = str_replace(gates,  "Gate: Dead", "Dead")) %>%
  mutate(gates = str_replace(gates,  "Gate: All Events", "All Events")) %>%
  mutate(gates = str_replace(gates,  "Gate: Noise", "Noise")) %>%
  mutate(gates = str_replace(gates,  "Gate: Lt - mCh", "Lt")) %>%
  mutate(gates = str_replace(gates,  "Gate: Sb/Td - GFP/mCit", "Sb/Td")) %>%
  # mutate(gates = str_replace(gates,  "Gate: Sc BC3 - mCit+mCh", "Sc")) %>% #wont work :()
  mutate(gates = str_replace(gates,  "Gate: LpSyto9", "Lp")) %>%
  mutate(gates = str_replace(gates,  "Gate: Levures", "Levures")) %>%
  mutate(gates = str_replace(gates,  "Gate: Sb", "Sb")) %>%
  mutate(gates = str_replace(gates,  "Gate: Td", "Td")) 

#fix long names
myclean$parameter1 <- gsub(".*X Parameter: ","", myclean$parameter1) 
myclean$parameter2 <- gsub(".*Y Parameter: ","", myclean$parameter2) 
myclean$gates <- gsub(".*mCh","Sc", myclean$gates) #won't work above, so will change here
myclean$groups <- gsub(".*Group: ","", myclean$groups) 

# make numeric
myclean$Count <- as.numeric(myclean$Count)
myclean$Concentration <- as.numeric(myclean$Concentration)
myclean$`%Total` <- as.numeric(myclean$`%Total`)
myclean$`%Gated` <- as.numeric(myclean$`%Gated`)

#myclean$Sample[myclean$Sample == "D9"] <- "C01"
# write_csv(myclean, "myclean.csv")

# check for weird to remove
unique(myclean$gates) # list all gate options (weird to maybe remove? = Gate: R1, Brett?, Noise? )
unique(myclean$Sample) # check for weird things (currently to remove ="C01(1)", "D09", "F9", "C10_2")
unique(myclean$groups) 

# need to try and remove junk values 
myclean <- myclean %>% 
  filter(Sample != "C01(1)") %>% 
  filter(Sample != "D9") %>% 
  filter(Sample != "F9") %>% 
  filter(Sample != "C10_2")
unique(myclean$Sample) # check again 


## Select for total yeast #######
my_Total_population <- myclean %>%
  filter(parameter1 == "FSC-A") %>%
  filter(parameter2 == "SSC-A") %>%
  filter(gates == "Levures") %>%
  filter(groups == "IP") %>%
  mutate(type = "Total_population") %>%
  select(-parameter1, -parameter2,-gates, -groups,-Gate) %>%
  rename("Total_Concentration" = Concentration)

#table(my_Total_population$Sample, my_Total_population$day)

my_Total_population %>%
  # filter(Count > 1000) %>%
  ggplot(aes(x = Count)) +
  geom_histogram() +
  geom_vline(xintercept = 1000, col = 2)


## Live cells concentration ####
my_live_population <- myclean %>%
  filter(parameter1 == "BL3-H") %>%
  filter(parameter2 == "SSC-H") %>%
  filter(gates == "Live") %>%
  filter(groups == "IP") %>%
  mutate(type = "Live_population") %>%
  select(-parameter1, -parameter2,-gates, -groups,-Gate) %>%
  rename("Live_Concentration" = Concentration,
         "Live_Count" = Count)

my_live_population %>%
  filter(Live_Count < 1000) %>%
  as.data.frame()

my_live_population %>%
  ggplot(aes(x = Live_Count)) +
  geom_histogram() +
  geom_vline(xintercept = 1000, col = 2)

# not sure what this is ???
sum(my_live_population$Live_Count < 1000) / nrow(my_live_population) * 100


## Sc concentration ####
my_Sc_population <- myclean %>%
  filter(parameter1 == "YL2-H") %>%
  filter(parameter2 == "BL1-H") %>%
  filter(gates == "Sc") %>%
  filter(groups == "IP") %>%
  mutate(type = "Sc_population") %>%
  select(-parameter1, -parameter2,-gates, -groups,-Gate) %>%
  rename("Sc_Concentration" = Concentration,
         "Sc_Count" = Count)

my_Sc_population %>%
  filter(Sc_Count < 1000) %>%
  as.data.frame()

my_Sc_population %>%
  ggplot(aes(x = Sc_Count)) +
  geom_histogram() +
  geom_vline(xintercept = 1000, col = 2)

# again, not sure what this is?
sum(my_Sc_population$Sc_Count < 1000)/nrow(my_Sc_population) * 100

## Sb concentration ####
my_Sb_population <- myclean %>%
  filter(parameter1 == "VL2-H") %>%
  filter(parameter2 == "SSC-H") %>%
  filter(gates == "Sb") %>%
  filter(groups == "IP") %>%
  mutate(type = "Sb_population") %>%
  select(-parameter1, -parameter2,-gates, -groups,-Gate) %>%
  rename("Sb_Concentration" = Concentration,
         "Sb_Count" = Count)

## Lt concentration ####
my_Lt_population <- myclean %>%
  filter(parameter1 == "YL2-H") %>%
  filter(parameter2 == "BL1-H") %>%
  filter(gates == "Lt") %>%
  filter(groups == "IP") %>%
  mutate(type = "Lt_population") %>%
  select(-parameter1, -parameter2,-gates, -groups,-Gate) %>%
  rename("Lt_Concentration" = Concentration,
         "Lt_Count" = Count)

## Td concentration ####
my_Td_population <- myclean %>%
  filter(parameter1 == "VL2-H") %>%
  filter(parameter2 == "SSC-H") %>%
  filter(gates == "Td") %>%
  filter(groups == "IP") %>%
  mutate(type = "Td_population") %>%
  select(-parameter1, -parameter2,-gates, -groups,-Gate) %>%
  rename("Td_Concentration" = Concentration,
         "Td_Count" = Count)

## see where noise is (i.e., counts that are in negative control with PBS+IP)
noise_Sc <- my_Sc_population %>% filter(Sample=="PBS+IP")
noise_Sb <- my_Sb_population %>% filter(Sample=="PBS+IP")
noise_Td <- my_Td_population %>% filter(Sample=="PBS+IP")
noise_Lt <- my_Lt_population %>% filter(Sample=="PBS+IP") #can see noise is in the Lt gate...

# check that tables were actually made
table(my_live_population$Sample, my_live_population$day)
table(my_Sc_population$Sample,my_Sc_population$day)
table(my_Sb_population$Sample,my_Sb_population$day)
table(my_Lt_population$Sample,my_Lt_population$day)
table(my_Td_population$Sample,my_Td_population$day)

# Fusionner les tableaux 
TOTAL <- my_Total_population  %>% select(day, Sample, round, Count, Total_Concentration)
LIVE <- my_live_population  %>% select(day, Sample, round, Live_Count, Live_Concentration)
SC <- my_Sc_population  %>% select(day, Sample, round, Sc_Count, Sc_Concentration)
SB <- my_Sb_population  %>% select(day, Sample, round, Sb_Count, Sb_Concentration)
LT <- my_Lt_population  %>% select(day, Sample, round, Lt_Count, Lt_Concentration)
TD <- my_Td_population  %>% select(day, Sample, round, Td_Count, Td_Concentration)

fused <- left_join(LIVE, SC) %>%
  left_join(SB) %>%
  left_join(LT) %>%
  left_join(TD) %>%
  left_join(TOTAL) 

# calculate proportion of each species from total live
fused <- fused %>%
  mutate(Sc_prop = Sc_Concentration/Live_Concentration) %>%
  mutate(Sb_prop = Sb_Concentration/Live_Concentration) %>%
  mutate(Lt_prop = Lt_Concentration/Live_Concentration) %>%
  mutate(Td_prop = Td_Concentration/Live_Concentration) %>%
  mutate(percent_alive = Live_Concentration/Total_Concentration*100)

wrong <- fused %>% filter(percent_alive >=100) # check that there's 0 cases where live_concentration > total_concentration


#### Loading the META dataset of dilution factor, species compositions, species richness
mydilutions <- readxl::read_xlsx(path="META_cyto.xlsx")  %>%
  pivot_longer(cyto_d00:cyto_d21, names_to = "day", values_to = "dilution") %>% 
  separate(day, c("A", "day")) %>% 
  select(-A) %>%
  rename("Sample" = comm)
  
## fusion of the two tables [JOINED dataframe]
joined <- left_join(fused, mydilutions)

# concentration calculation with the dilutions 
# 1000 is to change from uL to mL
joined <- joined %>%
  mutate(Live_Concentration = Live_Concentration*dilution*1000) %>%
  mutate(Sc_Concentration = Sc_Concentration*dilution*1000) %>%
  mutate(Sb_Concentration = Sb_Concentration*dilution*1000) %>%
  mutate(Lt_Concentration = Lt_Concentration*dilution*1000) %>%
  mutate(Td_Concentration = Td_Concentration*dilution*1000) %>%
  mutate(Total_Concentration = Total_Concentration*dilution*1000)

# keep only rows referring to communities 
joined <- joined %>% 
  filter(Sample != "IP") %>% 
  filter(Sample != "PBS+IP") %>% 
  filter(Sample != "IP+Syto9") %>% 
  filter(Sample != "Syto9") %>%
  filter(day !=14 | round != "r02") %>% # I think I forgot IP here, so remove this data...
  filter(Live_Count > 200) %>% #remove background junk (4 data points)
  filter(percent_alive < 10000) 
  
# turn day into numeric 
joined$day <- gsub(".*d","", joined$day)
joined$day <- as.numeric(joined$day )






####### CONCENTRATIONS ##########
## [mylong_conc dataframe]
## make day numeric, make into long format for proportions and concentrations
mylong_conc <- joined %>% 
  select(day, Sample, round, Sc_Concentration, Sb_Concentration, Lt_Concentration, Td_Concentration) %>%
  pivot_longer(
    cols = Sc_Concentration:Td_Concentration,
    values_to = "concentration",
    names_to = "species"
  ) %>%
  mutate(species = factor(species,  #just reorder so Sc first, matches order in other plots
        levels = c("Sc_Concentration", "Lt_Concentration", "Sb_Concentration", "Td_Concentration"))) 

##### CONCENTRATIONS PLOTS = fig. 4 and S4 (feb. 2024). good size = 
mylong_conc %>%
  # filter(day > 4) %>%
  filter(Sample != "C01") %>%
  filter(Sample != "C10") %>%
  group_by(day, Sample, species) %>%
  summarise_if(is.numeric, c(mean, sd), na.rm = TRUE) %>%
  ggplot(aes(x = day, y = fn1, col = species, group = species)) +
  scale_colour_manual(
    values = c("deepskyblue","chartreuse3", "gold",  "coral"),
    breaks = c("Sc_Concentration","Lt_Concentration", "Sb_Concentration",  "Td_Concentration"),
    labels = list(
      expression(italic("S. cerevisiae")),
      expression(italic("L. thermotolerans")),
      expression(italic("S. bacillaris")),
      expression(italic("T. delbreuckii"))
    )
  ) +
  # geom_ribbon(aes(ymin = 0, ymax = 10^4), fill = "grey", alpha = 0.2) + # for figS4 log10 scale
  geom_line(size = 0.8) +
  geom_hline(yintercept=10^4, colour = "grey") +
  geom_errorbar(aes(ymin = fn1 - fn2, ymax = fn1 + fn2)) +
  facet_wrap(~Sample, ncol = 5) +
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = c(1, 2, 3, 7, 14, 21)) +
  # scale_y_log10() +# for figS4 log10 scale
  labs(y = "mean live cells/mL") +
  theme(legend.text = element_text(hjust = -0.1))




#### PLOTS of MAX POPLN SIZE #####
max_conc <- joined %>%
group_by(Sample, round, Sc, Lt, Td, Sb, richness) %>%
  summarise(max_conc = max(Live_Concentration, na.rm = TRUE) )

max_conc <- max_conc %>% 
  filter(Sample != "C01" ) %>% #remove just Sc control 
  filter(Sample != "C10" ) #remove just Bb and Lp control 

# check max counts make sense (i.e., lower in NS communities, varied in C10, )
# Create a factor variable with the desired order
richness_order <- c("C02", "C11", "C12", "C13", 
                    "C03", "C04", "C05", "C14", "C15", "C16",
                    "C06", "C07", "C08", "C17",
                    "C09")  # Fill in the desired order
# reorder by richness
max_conc$Sample <- factor(max_conc$Sample, levels = richness_order)

# plot fig. 5A (feb 2024): 6x10 good pdf export size
max_conc <- max_conc %>%
  mutate(richness_label = paste0("", richness)) # used to label with richenss levels

max_conc %>% 
  ggplot(aes(x = Sample, y = max_conc, color = Sc, fill = Sc)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("grey90", "skyblue"), 
    labels = c("absent", "present")) +  # Set the legend labels for fill
  scale_colour_manual(
    values = c("grey50", "deepskyblue"), 
    labels = c("absent", "present")) +  # Set the legend labels for color
  labs(y = "max live cells/mL", x = "community", color = "S. cerevisiae?", fill = "S. cerevisiae?") +  # Change both legend titles
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_text(face = "italic"),
    legend.position = "right"
    )
# +
  # geom_text(aes(label = richness_label, y = -0.5), vjust = 0, size = 4)  # Plotting richness below x-axis labels


# plots for fig. 5 B-D (feb. 2024)

# plot Sc. vs. non.Sc max_conc %>% 
plot2 <- max_conc %>% 
  ggplot(aes(x = Sc, y = max_conc, color=Sc, fill = Sc)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("grey90", "skyblue"), 
    labels = c("absent", "present")) +  # Set the legend labels for fill
  scale_colour_manual(
    values = c("grey50", "deepskyblue"), 
    labels = c("absent", "present")) +  # Set the legend labels for color
  labs(y = "max live cells/mL", x = "", color = "S. cerevisiae?", fill = "S. cerevisiae?", size =20) +  # Change both legend titles
  theme_bw(base_size = 14) +
  theme(legend.title = element_text(face = "italic"),
        legend.position = "bottom",
        legend.direction = "vertical",
        axis.text.x = element_blank())  +
  stat_compare_means(method = "t.test", label = "p.format", size = 5)  # Add p-value comparisons between groups

# plot Td vs. non.Td 
plot3 <- max_conc %>% 
  ggplot(aes(x = Td, y = max_conc, color=Td, fill = Td)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("grey90", "coral"), 
    labels = c("absent", "present")) +  # Set the legend labels for fill
  scale_colour_manual(
    values = c("grey50", "coral2"), 
    labels = c("absent", "present")) +  # Set the legend labels for color
  labs(y = "", x = "", color = "T. delbrueckii?", fill = "T. delbrueckii?") +  # Change both legend titles
  theme_bw(base_size = 14) +
  theme(legend.title = element_text(face = "italic"),
        legend.position = "bottom",
        legend.direction = "vertical",
        axis.text.x = element_blank())  +
  stat_compare_means(method = "t.test", label = "p.format", size = 5)  # Add p-value comparisons between groups


# plot Sb vs. non Sb 
plot4 <- max_conc %>% 
  ggplot(aes(x = Sb, y = max_conc, color=Sb, fill = Sb)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("grey90", "gold"), 
    labels = c("absent", "present")) +  # Set the legend labels for fill
  scale_colour_manual(
    values = c("grey50", "gold3"), 
    labels = c("absent", "present")) +  # Set the legend labels for color
  labs(y = "", x = "", color = "S. bacillaris?", fill = "S. bacillaris?") +  # Change both legend titles
  theme_bw(base_size = 14) +
  theme(legend.title = element_text(face = "italic"),
        legend.position = "bottom",
        legend.direction = "vertical",
        axis.text.x = element_blank())  +
  stat_compare_means(method = "t.test", label = "p.format", size = 5)  # Add p-value comparisons between groups


# plot Lt vs. non Lt 
plot5 <- max_conc %>% 
  ggplot(aes(x = Lt, y = max_conc, color=Lt, fill = Lt)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("grey90", "chartreuse1"), 
    labels = c("absent", "present")) +  # Set the legend labels for fill
  scale_colour_manual(
    values = c("grey50", "chartreuse3"), 
    labels = c("absent", "present")) +  # Set the legend labels for color
  labs(y = "", x = "", color = "L. thermotolerans?", fill = "L. thermotolerans?") +  # Change both legend titles
  theme_bw(base_size = 14) +
  theme(legend.title = element_text(face = "italic"),
        legend.position = "bottom",
        legend.direction = "vertical",
        axis.text.x = element_blank())  +
  stat_compare_means(method = "t.test", label = "p.format", size = 5) # Add p-value comparisons between groups

# Combine plots (good size is 6x10 for pdf export)
combined_plots <-  plot2 + plot3 + plot4 + plot5 + plot_layout(ncol = 5)
combined_plots


#### ANOVAs of max CFUs
# suppl. Table S9
# compare 4 monocultures (Sc = C02, Lt = C11, Sb = C12, Td = C13)
max_conc2 <- max_conc %>% filter(richness == 1)
lm_rich <- lm(max_conc ~ Sample, data = max_conc2) 
Anova(lm_rich, type = 3)  
summary(lm_rich)

# plot maxCFU of monocultures fig. S5
max_conc2 %>% 
  ggplot(aes(x = Sample, y = max_conc, colour= Sample)) +
  geom_jitter(size = 3, width = 0.1) +
  scale_colour_manual(
    values = c("deepskyblue","chartreuse3", "gold",  "coral"),
    labels = list(
      expression(italic("S. cerevisiae")),
      expression(italic("L. thermotolerans")),
      expression(italic("S. bacillaris")),
      expression(italic("T. delbreuckii"))
    )) +
  labs(y = "max live cells/mL", x = "community", fill = "monoculture", colour = "monoculture") +  # Change both legend titles
  theme_bw(base_size = 14)+
  stat_compare_means(method = "anova")+      
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "C02")   


# all communities 
max_conc2 <- max_conc 
# %>% filter(richness != 4) # remove because only 1 pop'ln has 4 species
# 
# #for Thibault's help 
# write.csv(max_conc2, "max_conc_Sb_problem.csv", row.names= TRUE)

# richness and all species (problem with including Sb...) 
# Suppl. Table S10
lm_rich <- lm(formula = max_conc ~ Sc + Lt + Td + Sb + as.factor(richness) , data = max_conc2)
Anova(lm_rich, type = 3)  
summary(lm_rich)

# species effects only
# Suppl. Table S11
lm_rich <- lm(formula = max_conc ~ + Sc + Lt + Td + Sb, data = max_conc2)
Anova(lm_rich, type = 3)  
summary(lm_rich)

# # richness only, species effects excluded 
# lm_rich <- lm(max_conc ~ as.factor(richness), data = max_conc2) 
# Anova(lm_rich, type = 3)  
# summary(lm_rich)

# # richness and Sc effect 
# lm_rich <- lm(max_conc ~ as.factor(richness) + Sc, data = max_conc2) 
# Anova(lm_rich, type = 3)  
# summary(lm_rich) 
# emmeans(lm_rich, pairwise ~ richness) 














# # for each species alone (i.e., t-test,), use ALL DATA (dataframe "max_conc" not "max_conc2")
# lm_1species <- lm(max_conc ~  Sb, data = max_conc) 
# Anova(lm_1species, type = 3)  
# summary(lm_1species) 


# both richness and identity (can't do both....)
# max_conc2 <- max_conc %>% filter(richness != 4)# remove because only 1 pop'ln has 4 species
# lm_species_richness <- lm(max_conc ~ Sc + Lt + Td + Sb + richness, data = max_conc2) 
# Anova(lm_species_richness, type = 3)  
# summary(lm_species_richness)


# no interaction (species identity)
# # lm_species <- lm(max_conc ~ richness , data = max_conc) 
# lm_species <- lm(max_conc ~ richness + Sc, data = max_conc)
# Anova(lm_species, type = 3)  
# summary(lm_species) # all significant except Sb (Sc+, Lt+, Td- )
# # emmeans(lm_species, pairwise ~ Sc|Lt|Td|Sb) 


# # interactions
# lm_species <- lm(max_conc ~ Sc * Lt * Td * Sb, data = max_conc) 
# Anova(lm_species, type = 3)  
# summary(lm_species) # only Sc (+) and Td (-) significant when interactions included
# # emmeans(lm_species, pairwise ~ Sc|Lt|Td|Sb) 


# # S. cerevisiae
# lm_Sc <- lm(max_conc ~ Sc, data = max_conc) 
# Anova(lm_Sc, type = 3)  
# # summary(lm_Sc)
# emmeans(lm_Sc, pairwise ~ Sc) # + effect of Sc (p < 0.0001)
# 
# # L. thermotolerans 
# lm_Lt <- lm(max_conc ~ Lt, data = max_conc) 
# Anova(lm_Lt, type = 3)  
# # summary(lm_Lt)
# emmeans(lm_Lt, pairwise ~ Lt) # marginal + effect of Lt (p = 0.04727)
# 
# # T. delbrueckii 
# lm_Td <- lm(max_conc ~ Td, data = max_conc) 
# Anova(lm_Td, type = 3)  
# # summary(lm_Td)
# emmeans(lm_Td, pairwise ~ Td) # small - effect of Td (p = 0.03792)
# 
# # S. bacillaris 
# lm_Sb <- lm(max_conc ~ Sb, data = max_conc) 
# Anova(lm_Sb, type = 3)  
# # summary(lm_Sb)
# emmeans(lm_Sb, pairwise ~ Sb) # no effect of Sb (p = 0.523)







# ############ PROPORTIONS (not good b/c doesn't account for actual cell #s) #######
# # just testing things (percent alive for each species)
# mylong_prop <- joined %>%
#   select(day, Sample, round, Sc_prop, Sb_prop, Lt_prop, Td_prop) %>%
#   pivot_longer(
#     cols = Sc_prop:Td_prop,
#     values_to = "proportion",
#     names_to = "species"
#   )
# alanna <- joined %>%
#   select(day, Sample, round, Sc_prop, Sb_prop, Lt_prop, Td_prop, percent_alive) %>%
#   pivot_longer(
#     cols = Sc_prop:Td_prop,
#     values_to = "proportion",
#     names_to = "species"
#   )
# # 
# alanna <- alanna %>% mutate(prop_alive = proportion*percent_alive)
# 
# alanna %>%
#   group_by(day, Sample, species) %>%
#   summarise_if(is.numeric, c(mean, sd), na.rm = TRUE) %>%
#   ggplot(aes(x = day, y = prop_alive_fn1, col = species, group = species)) +
#   scale_colour_manual(values=c("chartreuse3", "gold", "deepskyblue", "coral"))+
#   geom_line(size=1) +
#   geom_errorbar(aes(ymin=prop_alive_fn1-prop_alive_fn2, ymax=prop_alive_fn1+prop_alive_fn2))+
#   facet_wrap(~Sample) +
#   theme_bw(base_size = 15) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   labs(y = "proportion alive")
# 
# ### line graph
# mylong_prop%>%
#   group_by(day, Sample, species) %>%
#   summarise_if(is.numeric, c(mean, sd), na.rm = TRUE) %>%
#   ggplot(aes(x = day, y = fn1, col = species, group = species)) +
#   scale_colour_manual(values=c("chartreuse3", "gold", "deepskyblue", "coral"))+
#   geom_line(size=0.8) +
#   geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2))+
#   facet_wrap(~Sample) +
#   theme_bw(base_size = 15) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   labs(y = "proportion")















###### graphs currently not used ########

# # plot live cells over time
# joined %>%
#   group_by(Sample, day) %>%
#   summarise(mean_Live_Concentration = mean(Live_Concentration),
#             se_Live_Concentration = sd(Live_Concentration) / sqrt(n())) %>%
#   ggplot(aes(x = day, y = mean_Live_Concentration, group = Sample, color = Sample)) +
#   geom_line(size = 1) +
#   geom_errorbar(aes(ymin = mean_Live_Concentration - se_Live_Concentration,
#                     ymax = mean_Live_Concentration + se_Live_Concentration),
#                 width = 0.2,
#                 color = "black", size = 0.5) +
#   geom_hline(yintercept = 10^6, linetype = 'dashed', 
#              col = 'lightblue', linewidth = 0.75) +
#   labs(y = "TOTAL live cells/mL") + 
#   theme_bw() +
#   scale_x_continuous(breaks = c(0, 1, 2, 7, 14, 21)) +
#   scale_y_log10() +
#   facet_wrap(~Sample)
# 
# 
# 
# 
# # Graph of live proportion 
# joined %>%
#   mutate(percent_dead = 100 - percent_alive)%>%
#   select(day, Sample, round, dead = percent_dead, live = percent_alive) %>%
#   group_by(day, Sample) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE) %>%
#   pivot_longer(dead:live,names_to = "statu", values_to = "percentage") %>%
#   ggplot(aes(x = as.factor(day), y = percentage, fill = statu)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~Sample)
# 
# joined %>%
#   filter(day == 0) %>%
#   select(day, Sample, round,Sc_prop,   Sb_prop, Lt_prop,Td_prop) %>%
#   group_by(day, Sample) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE) %>%
#   pivot_longer(Sc_prop:Td_prop,names_to = "Species", values_to = "percentage") %>%
#   ggplot(aes(x = Sample, y = percentage, fill = Species)) +
#   scale_fill_manual(values=c("chartreuse3", "gold", "deepskyblue", "coral"))+
#   ggtitle("live proportion: day 0") +
#   geom_bar(stat = "identity")
# 
# joined %>%
#   # filter(day != 21) %>%
#   # filter(day != 14) %>%
#   # filter(day != 7) %>%
#   select(day, Sample, round,Sc_prop,   Sb_prop, Lt_prop,Td_prop) %>%
#   group_by(day, Sample) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE) %>%
#   pivot_longer(Sc_prop:Td_prop,names_to = "Species", values_to = "percentage") %>%
#   ggplot(aes(x = Sample, y = percentage, fill = Species)) +
#   scale_fill_manual(values=c("chartreuse3", "gold", "deepskyblue", "coral"))+
#   geom_bar(stat = "identity") +
#   facet_wrap(~day, ncol = 1)
# 
# 
# joined %>% # PROBLEM: C10 not looking much different than others. almost as if C10 & C11 switched in day 3
#   filter(day != 21) %>%
#   filter(day != 14) %>%
#   filter(day != 7) %>%
#   select(day, Sample, round,Sc_Concentration,   Sb_Concentration, Lt_Concentration,Td_Concentration) %>%
#   group_by(day, Sample) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE) %>%
#   pivot_longer(Sc_Concentration:Td_Concentration,names_to = "Species", values_to = "Population") %>%
#   ggplot(aes(x = Sample, y = log10(Population), fill = Species)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~day, ncol = 1)
# 
