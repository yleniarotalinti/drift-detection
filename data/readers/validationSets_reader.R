#########
# DATASET
#########

#read ARFF file (dataset)
switch (dataset,
  "Abrupt" = {
    file.path <- "data/sources/abrupt_dataset_orig.arff"
  },
  "Gradual_W10K" = {
    file.path <- "data/sources/gradualW10K_dataset_orig.arff"
  },
  "Gradual_W20K" = {
    file.path <- "data/sources/gradualW20K_dataset_orig.arff"
  },
)

df <-read.csv(file.path, header=FALSE, comment.char = "@") 
#delete NAN column
df$V11<-NULL
#rename columns
names(df)<-c("salary", "commission", "age", "elevel", "car", "zipcode", "hvalue", "hyears", "loan", "class")

#refactor datatype
df$class <- ifelse(df$class =="groupB",1,0)
factors.col <- c("elevel", "car", "zipcode", "class")
df[,factors.col] <- lapply(df[,factors.col], factor)

rm(file.path, factors.col)
