# v8 worked on with Thibault: 27.04.2023
# continued by Alanna: 04.05.2023 - changed colours, seeing that noise is mostly from Lp gating, 
# removing data points with <200 counts (like a detection threshold)

##############################################################################################################
# TO DO: REMOVE PBS OR PBS+IP NOISE


##############################################################################################################
library(readxl)
library(tidyverse)

# Loading and transforming data #####
# remember to rename top of columns in excel file
setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2023_montpellier/experiments/cytometer")
# setwd("/Users/thibault/Google Drive/Boulot/Projets/Projet Alanna")

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


mydata <- rbind(day00.1, day01.1, day02.1, day07.1, day14.1, day21.1, 
                day00.2, day01.2, day02.2, day07.2, day14.2, day21.2,
                day00.3, day01.3, day02.3, day07.3, day14.3, day21.3,
                day00.4, day01.4, day02.4, day07.4, day14.4, day21.4 )


# transformation of the tab
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

unique(myclean$gates) # list all gate options (weird to remove? = Gate: R1, Brett?, Noise? )
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
sum(my_live_population$Live_Count < 1000)/nrow(my_live_population) * 100


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

## seeing noise 
noise_Sc <- my_Sc_population %>% filter(Sample=="PBS+IP")
noise_Sb <- my_Sb_population %>% filter(Sample=="PBS+IP")
noise_Td <- my_Td_population %>% filter(Sample=="PBS+IP")
noise_Lt <- my_Lt_population %>% filter(Sample=="PBS+IP") #can see noise is in the Lt gate...



# check that tables actually made
table(my_live_population$Sample, my_live_population$day)
table(my_Sc_population$Sample,my_Sc_population$day)
table(my_Sb_population$Sample,my_Sb_population$day)
table(my_Lt_population$Sample,my_Lt_population$day)
table(my_Td_population$Sample,my_Td_population$day)


# Fusionner les tableaux #####
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

fused <- fused %>%
  mutate(Sc_prop = Sc_Concentration/Live_Concentration) %>%
  mutate(Sb_prop = Sb_Concentration/Live_Concentration) %>%
  mutate(Lt_prop = Lt_Concentration/Live_Concentration) %>%
  mutate(Td_prop = Td_Concentration/Live_Concentration) %>%
  mutate(percent_alive = Live_Concentration/Total_Concentration*100)

# wrong <- fused %>% filter(percent_alive >=100) # check that there's 0 cases where live_concentration > total_concentration


#### Loading the dataset of dilutions #####
# also includes consortia compositions (yes/no for Sc, Lt, Td, Sb)
mydilutions <- readxl::read_xlsx(path="META_cyto.xlsx")  %>%
  pivot_longer(cyto_d00:cyto_d21, names_to = "day", values_to = "dilution") %>% 
  separate(day, c("A", "day")) %>% 
  select(-A) %>%
  rename("Sample" = comm)
  
# fusion of the two tables
joined <- left_join(fused, mydilutions)

# concentration calculation with the dilutions 
# 1000 ia to change from microL to mL
joined <- joined %>%
  mutate(Live_Concentration = Live_Concentration*dilution*1000) %>%
  mutate(Sc_Concentration = Sc_Concentration*dilution*1000) %>%
  mutate(Sb_Concentration = Sb_Concentration*dilution*1000) %>%
  mutate(Lt_Concentration = Lt_Concentration*dilution*1000) %>%
  mutate(Td_Concentration = Td_Concentration*dilution*1000) %>%
  mutate(Total_Concentration = Total_Concentration*dilution*1000)

joined <- joined %>% 
  filter(Sample != "IP") %>% 
  filter(Sample != "PBS+IP") %>% 
  filter(Sample != "IP+Syto9") %>% 
  filter(Sample != "Syto9") %>%
  filter(day !=14 | round != "r02") %>% # I think I forgot IP here, so remove this data...
  filter(Live_Count > 200) %>% #remove background junk (4 data points)
  filter(percent_alive <10000) 
  
# turn day into numeric 
joined$day <- gsub(".*d","", joined$day)
joined$day <- as.numeric(joined$day )

# plot live cells once transformed with dilution factor
#total live cells 

# plot percent alive over time
joined %>%
  ggplot(aes(x = day, y = percent_alive, col = Sample, shape = round)) +
  # geom_point(size = 2) +
  geom_jitter(width=0.1, size = 2) +
  labs(y = "percent_alive") +
  scale_x_continuous(breaks=c(0, 1, 2, 7, 14, 21))+
  theme_bw()+
  facet_wrap(~Sample)

# plot live cells over time
joined %>%
  ggplot(aes(x = day, y = log10(Live_Concentration), col = Sample, shape = round)) +
  geom_jitter(width=0.1, size = 3) +
  geom_hline(yintercept = log10(10^6), linetype='dashed', 
             col = 'lightblue', linewidth = 0.75) +
  labs(y = "log10(TOTAL live cells/mL)") + 
  theme_bw() +
  scale_x_continuous(breaks=c(0, 1, 2, 7, 14, 21))
# +
#   facet_wrap(~Sample)


# Graph of live proportion #####
joined %>%
  mutate(percent_dead = 100 - percent_alive)%>%
  select(day, Sample, round, dead = percent_dead, live = percent_alive) %>%
  group_by(day, Sample) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  pivot_longer(dead:live,names_to = "statu", values_to = "percentage") %>%
  ggplot(aes(x = as.factor(day), y = percentage, fill = statu)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Sample)
  
joined %>%
  filter(day == 0) %>%
  select(day, Sample, round,Sc_prop,   Sb_prop, Lt_prop,Td_prop) %>%
  group_by(day, Sample) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  pivot_longer(Sc_prop:Td_prop,names_to = "Species", values_to = "percentage") %>%
  ggplot(aes(x = Sample, y = percentage, fill = Species)) +
  scale_fill_manual(values=c("chartreuse3", "gold", "deepskyblue", "coral"))+
  ggtitle("live proportion: day 0") +
  geom_bar(stat = "identity")

joined %>%
  # filter(day != 21) %>%
  # filter(day != 14) %>%
  # filter(day != 7) %>%
  select(day, Sample, round,Sc_prop,   Sb_prop, Lt_prop,Td_prop) %>%
  group_by(day, Sample) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  pivot_longer(Sc_prop:Td_prop,names_to = "Species", values_to = "percentage") %>%
  ggplot(aes(x = Sample, y = percentage, fill = Species)) +
  scale_fill_manual(values=c("chartreuse3", "gold", "deepskyblue", "coral"))+
  geom_bar(stat = "identity") +
  facet_wrap(~day, ncol = 1)


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





######  make day numeric, make into long format for proportions and concentrations
mylong_conc <- joined %>% 
  select(day, Sample, round, Sc_Concentration, Sb_Concentration, Lt_Concentration, Td_Concentration) %>%
  pivot_longer(
    cols = Sc_Concentration:Td_Concentration,
    values_to = "concentration",
    names_to = "species"
  )

mylong_prop <- joined %>% 
  select(day, Sample, round,Sc_prop, Sb_prop,Lt_prop, Td_prop) %>%
  pivot_longer(
    cols = Sc_prop:Td_prop,
    values_to = "proportion",
    names_to = "species"
  )


# ALANNA testing things (percent alive for each species)
alanna <- joined %>% 
  select(day, Sample, round, Sc_prop, Sb_prop, Lt_prop, Td_prop, percent_alive) %>%
  pivot_longer(
    cols = Sc_prop:Td_prop,
    values_to = "proportion",
    names_to = "species"
  )

alanna <- alanna %>% mutate(prop_alive = proportion*percent_alive) 

alanna %>%
  group_by(day, Sample, species) %>%
  summarise_if(is.numeric, c(mean, sd), na.rm = TRUE) %>%
  ggplot(aes(x = day, y = prop_alive_fn1, col = species, group = species)) +
  scale_colour_manual(values=c("chartreuse3", "gold", "deepskyblue", "coral"))+
  geom_line(size=1) +
  geom_errorbar(aes(ymin=prop_alive_fn1-prop_alive_fn2, ymax=prop_alive_fn1+prop_alive_fn2))+
  facet_wrap(~Sample) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "proportion alive") 




##### CONCENTRATIONS PLOTS
### line graph
mylong_conc %>%
  group_by(day, Sample, species) %>%
  summarise_if(is.numeric, c(mean, sd), na.rm = TRUE) %>%
  ggplot(aes(x = day, y = fn1, col = species, group = species)) +
  scale_colour_manual(values=c("chartreuse3", "gold", "deepskyblue", "coral"))+
  geom_line(size=0.8) +
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2))+
  # geom_hline(yintercept =10^6)+
  facet_wrap(~Sample) +
  theme_bw(base_size = 15) +
  # theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks=c(1, 2, 3, 7, 14, 21))+
  labs(y = "mean cells/mL") 

# ### barplot
# mylong_conc %>%  
#   # filter(Sample == "C17") %>%
#   # group_by(day, Sample , species) %>%
#   # summarise_if(is.numeric, c(mean,sd), na.rm = TRUE) %>%
#   # filter(round == "r01") %>%
#   ggplot(aes(x=interaction(round, as.factor(day)), y = concentration, fill = species)) +
#   # ggplot(aes(x=round, y = concentration, fill = species)) +
#   geom_col() +
#   facet_wrap(~Sample) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   labs(y = "mean cells/mL", x = "day") 


############ PROPORTIONS
### line graph
mylong_prop%>%
  group_by(day, Sample, species) %>%
  summarise_if(is.numeric, c(mean, sd), na.rm = TRUE) %>%
  ggplot(aes(x = day, y = fn1, col = species, group = species)) +
  scale_colour_manual(values=c("chartreuse3", "gold", "deepskyblue", "coral"))+
  geom_line(size=0.8) +
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2))+
  facet_wrap(~Sample) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "proportion") 

### barplot
mylong_prop %>%  
  # filter(Sample == "C17") %>%
  # group_by(day, Sample , species) %>%
  # summarise_if(is.numeric, c(mean,sd), na.rm = TRUE) %>%
  # filter(round == "r01") %>%
  ggplot(aes(x=interaction(round, as.factor(day)), y = proportion, fill = species)) +
  scale_colour_manual(values=c("chartreuse3", "gold", "deepskyblue", "coral"))+
  # ggplot(aes(x=round, y = concentration, fill = species)) +
  geom_col() + ylim(0,1) +
  facet_wrap(~Sample) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "proportion", x = "day") 












