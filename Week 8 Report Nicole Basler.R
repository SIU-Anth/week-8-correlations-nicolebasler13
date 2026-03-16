library(readr)
BoneArt_Dataset <- read_csv("artBoneRicePatterson.csv")
View(BoneArt_Dataset)

#when Imported this data set I clicked the CSV provided and I clicked import data to rename the dataset for something more friendly. B
install.packages("tidyverse");install.packages("knitr");install.packages("readr")
install.packages("tidyr")

library(readr)
BoneArt_Dataset <- read_csv("artBoneRicePatterson.csv")
View(BoneArt_Dataset)

BoneArt_Dataset #this will just show a basic table of this data
library(tidyverse)

#the code below is to answer question one
BoneAbundance_table <- tibble(BoneArt_Dataset$bone_abundance)

summary(BoneAbundance_table)

BoneArtCount_table <- tibble(BoneArt_Dataset$art_count)

summary(BoneArtCount_table)

#this code below is to answer question two
Count_Table <- BoneArt_Dataset %>%
  group_by(taxa) %>%
  summarise(
    total_art_count = sum(art_count, na.rm = TRUE),
    total_bone_abundance = sum(bone_abundance, na.rm = TRUE),
    n = n()
  )

summary(Count_Table)

cor.test(Count_Table$total_art_count,Count_Table$total_bone_abundance)

bovines_data <- BoneArt_Dataset %>% filter(taxa == "Bovines")

summary(bovines_data)
cor.test(bovines_data$art_count, bovines_data$bone_abundance)

deer_data <- BoneArt_Dataset %>% filter(taxa == "Deer")

summary(deer_data)

cor.test(deer_data$art_count, deer_data$bone_abundance)

horse_data <- BoneArt_Dataset %>% filter(taxa == "Horse")

horse_sum <- summary(horse_data)

cor.test(horse_data$art_count, horse_data$bone_abundance)

ibex_data <- BoneArt_Dataset %>% filter(taxa == "Ibex")

summary(ibex_data)

cor.test(ibex_data$art_count,ibex_data$bone_abundance)

reindeer_data <- BoneArt_Dataset %>% filter(taxa == "Reindeer")

summary(reindeer_data)

cor.test(reindeer_data$art_count,reindeer_data$bone_abundance)

#My code for graphs for both questions below
ggplot() + 
  geom_jitter(data = BoneArt_Dataset, aes(x = art_count, y = bone_abundance, fill = taxa, color = taxa, shape = taxa )) +  
  scale_color_manual(values = c('magenta', 'red','orange','green','purple')) +  geom_pointrange() +
  theme_get() +
  
  facet_wrap(~ taxa) + labs(
    title = "Bone Art and Abundance Count by Taxa",
    subtitle = "BoneArt_Dataset",
    x = "Art Count",
    y = "Bone Abundance"
  ) + theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )
cor.test(BoneArt_Dataset$art_count,BoneArt_Dataset$bone_abundance)

ggplot() + 
  geom_histogram(data = BoneArt_Dataset, aes(x = art_count, fill = taxa, color = taxa,   )) 
   + facet_wrap(~ taxa) + labs( title = "Bone Art Count by Taxa", subtitle = "BoneArt_Dataset", x = "Art Count",
    y = "Bone Abundance"
  ) + theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )


ggplot() + 
  geom_histogram(data = BoneArt_Dataset, aes(x = bone_abundance, fill = taxa, color = taxa,   )) +  
   + facet_wrap(~ taxa) + labs(title = "BoneAbundance Count by Taxa", subtitle = "BoneArt_Dataset",x = "Art Count",
    y = "Bone Abundance") + theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

