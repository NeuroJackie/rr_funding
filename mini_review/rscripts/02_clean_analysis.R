###############
# Load ####
###########

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

### 
# import data ####
#####

no_note <- read_csv("analysis/analysis_not_noted.csv", trim_ws = T)
note <- read_csv("analysis/analysis_noted.csv", trim_ws = T)

df <- rbind(note, no_note)

citekey <- read_csv("data/03_inc_citekeys.csv") %>%
  select(c("Title", "Citekey"))

colnames(citekey) <- tolower(colnames(citekey))

df <- full_join(df, citekey, by = "title")


#########################
# Separate evidence_pro #### 
##################

# separate

nmax <- max(str_count(df$evidence_pro, ";"), na.rm = T) + 1

df <- separate(df, evidence_pro, into = paste0("evidence_pro", seq_len(nmax)), sep = ";", fill = "warn") 

# Combine all evidence_pro columns

df <- pivot_longer(df, cols = contains("evidence_pro"), names_to = "num", values_to = "evidence_pro")

# drop num col

df$num <- NULL

# remove duplicated rows

df <- df[!duplicated(df), ]
#########################
# Separate evidence_con #### 
##################

# separate

nmax <- max(str_count(df$evidence_con, ";"), na.rm = T) + 1

df <- separate(df, evidence_con, into = paste0("evidence_con", seq_len(nmax)), sep = ";", fill = "warn") 


# Combine all evidence_con cols

df <-  pivot_longer(df, cols = contains("evidence_con"), names_to = "num", values_to = "evidence_con")

# drop member_num col

df$num <- NULL

# remove duplicated rows

df <- df[!duplicated(df), ]
#########################
# Separate opinion_pro #### 
##################

nmax <- max(str_count(df$opinion_pro, ";"), na.rm = T) + 1

df <- separate(df, opinion_pro, into = paste0("opinion_pro", seq_len(nmax)), sep = ";", fill = "warn") 

# Combine all opinion_pro cols

df <-  pivot_longer(df, cols = contains("opinion_pro"), names_to = "num", values_to = "opinion_pro")

# drop member_num col

df$num <- NULL

# remove duplicated rows

df <- df[!duplicated(df), ]
#####################
# separate  ###
#########################

# separate opinion_con_buts (correct mispelling in separed cols)

nmax <- max(str_count(df$opinion_con_buts, ";"), na.rm = T) + 1

df <- separate(df, opinion_con_buts, into = paste0("opinion_con_buts", seq_len(nmax)), sep = ";", fill = "warn")

# Combine all opinion_con_buts cols

df <-  pivot_longer(df, cols = contains("opinion_con_buts"), names_to = "num", values_to = "opinion_con_buts")

# drop member_num col

df$num <- NULL

# remove duplicated rows

df <- df[!duplicated(df), ]

####################
# remove whitespace ####
####################

# gsub whitespace

df <- lapply(df, function(x) gsub("[^\u0001-\u007F]+", "",x)) %>%
  lapply(function(x) gsub("[\t\n\r\v\f]", "",x)) %>%
  as.data.frame(stringsAsFactors = F)

# create function to check all cols for any NA values that are not set as NA
check_na <- function(col){
  # remove all NA values
  x <- col[!is.na(col)]
  # throw error if there are "" values or " " values
  if(length(unique(x[x == ""])) >0 | length(unique(x[x == " "])) >0) {
    warning("col contains unset NA values")
  } else {
    #else no error
    print("no unset NA values")
  }
}

# check all NAs set

lapply(df, check_na)

# create function to set NA values

set_na <- function(col){
  col[col == ""] <- NA
  col[col == " "] <- NA
}

#  set_na for columns that threw an error in lapply(df, check_na)

df$opinion_con_buts <- set_na(df$opinion_con_buts)
df$opinion_pro <- set_na(df$opinion_pro)

# check all NAs set again

lapply(df, check_na)

##############
# export ####
############

write.csv(df, "outputs/analysis_clean.csv", row.names = F, fileEncoding = "UTF-8")
