library(tidyverse)
library(readstata13)
library(openxlsx)

#### village household surveys ####
df1 <- as_tibble(read.dta13("data/tnc_cleandataset.dta"))
df1$village <- str_to_lower(as.character(df1$village))

##### village coded binary ####
df2 <- as_tibble(read.xlsx("data/TNC_RangelandMGT_codesbyvillage.xlsx"))

names(df2) <- str_replace(names(df2), "\\.Gr=.*","")
names(df2) <- str_replace_all(names(df2), "[^[:alnum:]]", " ")
names(df2) <- str_replace_all(names(df2), "    ", " ")
names(df2) <- str_replace_all(names(df2), "   ", " ")
names(df2) <- str_replace_all(names(df2), "  ", " ")
names(df2) <- trimws(names(df2))
names(df2)[1] <- "village"

df2$village <- trimws(str_replace_all(df2$village, "[^[:alnum:]]", " "))
df2$village <- str_replace(df2$village," Village.*","")
df2 <- df2 %>% filter(village != "Totals") %>% select(-Totals)
df2 <- df2 %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "code", values_to = "val_binary")

#### code manager ####
df_cods <- as_tibble(read.xlsx("data/TNC_Rangeland MGT_Code Manager.xlsx"))

names(df_cods) <- str_to_lower(names(df_cods))
df_cods$code <- trimws(str_replace_all(df_cods$code, "[^[:alnum:]]", " "))
df_cods$code <- str_replace_all(df_cods$code, "   ", " ")
df_cods$code <- str_replace_all(df_cods$code, "  ", " ")
df_cods$comment <- str_remove_all(df_cods$comment, "\u2029")
df_cods <- df_cods %>% select(-x1)

#### merge them ####
df2 <- df2 %>% 
  left_join(df_cods, by = "code")  %>% 
  mutate(across(where(is.character), ~str_to_lower(.x)))

##### village coded scores ####
df3 <- as_tibble(read.xlsx("data/APPENDIX A4.xlsx"))
names(df3)[names(df3)=="02.1.Invasives"] <- "O2.1.Invasives"
names(df3)[names(df3)=="A4.2.-.Leadership.authority"] <- "A4.2.Leadership.authority"
names(df3)[1] <- "village"

df3 <- df3 %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "dimension", values_to = "score") %>% 
  mutate(across(where(is.character), ~str_to_lower(.x)))

#### harmonize village names ####
df1 <- df1 %>% mutate(village = ifelse(grepl("eleng", village), "eleng'ata", village),
                      village = ifelse(grepl("ngoswa", village), "ngoswaki", village))
df2 <- df2 %>% mutate(village = ifelse(grepl("eleng", village), "eleng'ata", village),
                      village = ifelse(grepl("ngoswa", village), "ngoswaki", village))
df3 <- df3 %>% mutate(village = ifelse(grepl("eleng", village), "eleng'ata", village),
                      village = ifelse(grepl("ngoswa", village), "ngoswaki", village))

write_csv(df1, "data_processed/df_househ.csv")
write_csv(df2, "data_processed/df_vilcod.csv")
write_csv(df3, "data_processed/df_scores.csv")


# further ideas:
# - human pop / demographics
# - rainfall
# - afrobarometer (GPS? villages? districts?)
# - other nature and social and economic data

