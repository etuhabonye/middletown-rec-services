library(readr)

Middletown_Rec_Services_Data_012622 <- read_csv("Middletown_Rec_Services_Data_012622.csv")

copy <- Middletown_Rec_Services_Data_012622

library(tidyr)
library(dplyr)
copy %>% 
  unite("Race", Q10_1:Q10_6, na.rm = TRUE) %>% 
  unite("Best_Way_to_hear", Q16_1:Q16_6, na.rm = TRUE, sep = " ,") %>% 
  unite("Aquatic Interest", Q39_1:Q39_9, na.rm = TRUE, sep = " ,") %>% 
  unite("Art Interest", Q40_1:Q40_6, na.rm = TRUE, sep = " ,") %>% 
  unite("Game Night Interest", Q41_1:Q41_6, na.rm = T, sep = " ,") %>% 
  unite("Fitness Class Interest", Q42_1:Q42_7, na.rm = T, sep = " ,") %>% 
  unite("Intramural Sports Interst", Q43_1:Q43_8, na.rm = T, sep = " ,") %>% 
  unite("Programs Interest", Q44_1:Q44_5, na.rm = T, sep = " ,") %>% 
  unite("How Hear", Q15_1:Q15_8, na.rm = T, sep = ",")-> test

test %>% 
  select(ResponseId, Q49_1, Q2_1,Q3_1:Q3_10, Q5, Q8, Q9,Q19, Q21,  Race, Best_Way_to_hear, `Aquatic Interest`, `Art Interest`, `Game Night Interest`,
         `Fitness Class Interest`, `Intramural Sports Interst`, `Programs Interest`, Q14,`How Hear`)-> first_subset

#write.csv(first_subset,"first_subset.csv")

first_subset <- first_subset[-c(1,2),]

first_subset$Q3_1 <- as.numeric(first_subset$Q3_1)
first_subset$Q3_2 <- as.numeric(first_subset$Q3_2)
first_subset$Q3_3 <- as.numeric(first_subset$Q3_3)
first_subset$Q3_4 <- as.numeric(first_subset$Q3_4)
first_subset$Q3_5 <- as.numeric(first_subset$Q3_5)
first_subset$Q3_6 <- as.numeric(first_subset$Q3_6)
first_subset$Q3_7 <- as.numeric(first_subset$Q3_7)
first_subset$Q3_8 <- as.numeric(first_subset$Q3_8)
first_subset$Q3_9 <- as.numeric(first_subset$Q3_9)
first_subset$Q3_10 <- as.numeric(first_subset$Q3_10)
first_subset$`Average_Age` <- rowMeans(first_subset[, 4:13], na.rm = T)
first_subset$White <- df$Q10_1
first_subset$Black <- df$Q10_3
first_subset$AlaskanNative <- df$Q10_3
first_subset$Asian <- df$Q10_4
first_subset$NativeHawaiian <- df$Q10_5
first_subset$OtherRace <- df$Q10_6
first_subset$Q19[first_subset$Q19 == "Other:"] <- NA

#write.csv(first_subset,"heythere.csv")

first_subset %>%
  mutate(Race=ifelse(Race=="White"| Race== "Black or African American" | Race == "Asian" | 
                      Race == "American Indian or Alaskan Native" | Race == "Other",Race,"Multiracial"))-> first_subset

df <- Middletown_Rec_Services_Data_012622[-c(1,2),]
