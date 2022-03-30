library(irr)

# load all papers (incl. headers and remove white space in each cell)
df <- read.csv("AllPapers.csv", 
                      header=TRUE,
                      strip.white=TRUE)

# extract the two rater columns
ratings <- df[, c("SuggestedInclusion1","SuggestedInclusion2")]

# crosstable (just for information)
xtabs(~ratings$SuggestedInclusion1 + ratings$SuggestedInclusion2)

# percentage agreement
agree(ratings)

#Cohen's Kappa with equal weights
kappa2(ratings,"equal",sort.levels = TRUE)

