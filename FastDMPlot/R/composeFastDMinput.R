# This script composes the input for the FastDM software.

# In data all raw values are stored, as they were
# read in from the file system.
# In this script, the raw values are going to be
# filtered by condition and stored in one csv file
# per subject.
# This output is then used by fast-dm for creating a log
# file with all the diffusion model parameters for one
# subject. This logfile is used by the functions to
# calculate the CDF parameters and compose the plot.

csv_path <- "./csvfiles"

# List of file names
all_files <- list.files(path = csv_path, pattern = "*.csv")

# store all files in a dataframe called data
dat <- list()
for (i in seq_along(all_files)) {
  dat[[i]] <- read.csv(file = file.path(csv_path, all_files[i]), header=TRUE, sep=",")
}


rawdata.light.rt <- list()
rawdata.light.acc <- list()
rawdata.NOlight.rt <- list()
rawdata.NOlight.acc <- list()
RAWDATA.light <- list()
RAWDATA.NOlight <- list()

rawdata.light800.rt <- list()
rawdata.light0.rt <- list()
rawdata.light800.acc <- list()
rawdata.light0.acc <- list()
RAWDATA.light0 <- list()
RAWDATA.light800 <- list()

for (i in 1:length(dat)){
  # This is the input for the diffusion model for the model QPF plot  for which the conditions must be seperated.
  rawdata.light800.rt[[i]] <- subset(dat[[i]]$reactionTime_PT, dat[[i]]$fading_function == 800)
  rawdata.light0.rt[[i]]   <- subset(dat[[i]]$reactionTime_PT, dat[[i]]$fading_function == 0)
  rawdata.light800.acc[[i]] <- subset(dat[[i]]$correct, dat[[i]]$fading_function == 800)
  rawdata.light0.acc[[i]]   <- subset(dat[[i]]$correct, dat[[i]]$fading_function == 0)

  RAWDATA.light800[[i]] <- cbind(rawdata.light800.rt[[i]], rawdata.light800.acc[[i]])
  RAWDATA.light0[[i]]   <- cbind(rawdata.light0.rt[[i]], rawdata.light0.acc[[i]])
}



for (i in 1:length(dat)) {

  # This dataframe contains all the rt and accuracy values
  # that were recorded under the fading=800 or fading=0 condition.
  rawdata.light.rt[[i]]  <- subset(dat[[i]]$reactionTime_PT, dat[[i]]$fading_function==800 | dat[[i]]$fading_function==0)
  rawdata.light.acc[[i]] <- subset(dat[[i]]$correct,         dat[[i]]$fading_function==800 | dat[[i]]$fading_function==0)
  # This matrix contains all corresponding accuracy responses to the rt values
  RAWDATA.light[[i]] <- cbind(rawdata.light.acc[[i]], rawdata.light.rt[[i]])


  # This dataframe contains all the rt and accuracy values
  # that were recorded under the fading=-1 (NO LIGHT) condition.
  rawdata.NOlight.rt[[i]] <- subset(dat[[i]]$reactionTime_PT, dat[[i]]$fading_function==-1)
  rawdata.NOlight.acc[[i]] <- subset(dat[[i]]$correct, dat[[i]]$fading_function==-1)
  # This matrix contains all corresponding accuracy responses to the rt values
  RAWDATA.NOlight[[i]] <- cbind(rawdata.NOlight.acc[[i]], rawdata.NOlight.rt[[i]])
}

# Export the entries from RAWDATA.light and RAWDATA.NOlight into csv files for fast-dm.exe, one
# csv file per subject.

for(s in 1:length(dat)){
  write.table(RAWDATA.light[[s]],
            file=paste("subj", s, "LIGHT.csv", sep=""),
            row.names=FALSE,
            col.names = FALSE,
            quote=FALSE,
            sep=" ")

  write.table(RAWDATA.NOlight[[s]],
            file=paste("subj", s, "NOlight.csv", sep=""),
            row.names = FALSE,
            quote=FALSE,
            col.names = FALSE,
            sep=" ")
}


for(s in 1:length(dat)){
  # This is the input for the diff model when output seperated by condition is needed.
  write.table(RAWDATA.light800[[s]],
              file = paste("subj", s, ".800.csv", sep=""),
              row.names = FALSE,
              quote = FALSE,
              col.names = FALSE,
              sep=" ")
  write.table(RAWDATA.light0[[s]],
              file = paste("subj", s, ".0.csv", sep=""),
              row.names = FALSE,
              quote = FALSE,
              col.names = FALSE,
              sep=" ")
}



