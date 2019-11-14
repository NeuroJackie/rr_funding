###################
# Create headers  ####
##################

# Create variable for input file

input_file <- "data/03_rr_zotero_export.csv"

# Read in headers on second row

headers <- read.csv(input_file, nrows = 1, header = FALSE, stringsAsFactors = FALSE, strip.white = T, encoding = "UTF-8")

# Transpose headers

headers <- t(headers)

# clean headers

headers <- tolower(headers)
headers <- gsub("\\s", "_", headers)

##############
# Import  data####
##############
# import data skipping first row (headers)

df <- read.csv(input_file, skip = 1, header =  FALSE, col.names = headers,
               na.strings = c("", " "), stringsAsFactors = FALSE, 
               strip.white = T, encoding = "UTF-8", row.names = NULL)

###################
# Drop empty cols #
#################

df <- df[colSums(!is.na(df)) > 0]

# replace publication_year with year

colnames(df) <- gsub("publication_", "", colnames(df))

##################
# extract include ####
#####################

inc <- df[grep("include", df$manual_tags), ]
exc <- df[grep("exclude", df$manual_tags), ]

############
# clean notes ####
#############

row.names(inc) <- NULL

inc$notes <- gsub("<br />", " ", inc$notes)

###############
# export title notes & author ####
#############

inc_has_note <- inc[!is.na(inc$notes), c("notes", "author", "title", "year", "manual_tags")]

inc_no_note <- inc[is.na(inc$notes), c("notes", "author", "title", "year", "manual_tags")]

if(sum(grepl("note", inc_no_note$manual_tags)) != 0) {
  stop("articles with empty note fields have note tag")
}

write.csv(inc_has_note, "outputs/noted.csv", row.names = F, fileEncoding = "UTF-8")
write.csv(inc_has_note, "outputs/not_noted.csv", row.names = F, fileEncoding = "UTF-8")