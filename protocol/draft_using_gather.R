
library(tidyverse)
library(knitr)

options(knitr.kable.NA = "")

### 
# import data ####
#####

no_note <- read_csv("../mini_review/data/analysis_not_noted.csv")
note <- read_csv("../mini_review/data/analysis_noted.csv")

df <- rbind(note, no_note)

#########################
# Separate empirical_pro #### 
##################

# separate

nmax <- max(str_count(df$empirical_pro, ";"), na.rm = T) + 1

df <- separate(df, empirical_pro, into = paste0("empirical_pro", seq_len(nmax)), sep = ";", fill = "warn") 

# Transform team members into columns

df <-  gather(df,  key= "num", value = "empirical_pro", contains("empirical_pro"), na.rm = F)

# drop num col

df$num <- NULL

# remove duplicated rows

df <- df[!duplicated(df), ]
#########################
# Separate empirical_con #### 
##################

# separate

nmax <- max(str_count(df$empirical_con, ";"), na.rm = T) + 1

df <- separate(df, empirical_con, into = paste0("empirical_con", seq_len(nmax)), sep = ";", fill = "warn") 


# Transform team members into columns

df <-  gather(df,  key= "num", value = "empirical_con", contains("empirical_con"), na.rm = T)

# drop member_num col

df$num <- NULL

# remove duplicated rows

df <- df[!duplicated(df), ]
#########################
# Separate pro_opinion #### 
##################

nmax <- max(str_count(df$pro_opinion, ";"), na.rm = T) + 1

df <- separate(df, pro_opinion, into = paste0("pro_opinion", seq_len(nmax)), sep = ";", fill = "warn") 

# Transform team members into columns

df <-  gather(df,  key= "num", value = "pro_opinion", contains("pro_opinion"), na.rm = F)

# drop member_num col

df$num <- NULL

# remove duplicated rows

df <- df[!duplicated(df), ]
#####################
# separate critique_responses_opintion ###
#########################

# separate critique_responses_opintion (correct mispelling in separed cols)

nmax <- max(str_count(df$critique_responses_opintion, ";"), na.rm = T) + 1

df <- separate(df, critique_responses_opintion, into = paste0("critique_responses", seq_len(nmax)), sep = ";", fill = "warn")

# Transform team members into columns

df <-  gather(df,  key= "num", value = "critique_responses", contains("critique_responses"), na.rm = F)

# drop member_num col

df$num <- NULL

# remove duplicated rows

df <- df[!duplicated(df), ]

ipa_emp_pro <- df[grep("IPA", df$empirical_pro, ignore.case = T), ] 

apply(df, 1, function(x)as.integer(any(grep("IPA",x))))

n.obs <- sapply(ipa, length)
seq.max <- seq_len(max(n.obs))
ipa <- sapply(ipa, "[", i = seq.max) %>%
  as.data.frame(stringsAsFactors = F) 
