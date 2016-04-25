## PDF
path <- setwd("D:/Documents/Master/WS15-16/PracticalProject/Fast-DMSoftware/win/Win_Binaries_30_2") 
estimatedParams.diffmodel <- read.table(paste(path, "/logFilesKS/all_subjKS.log", sep=""), header = TRUE)

a <- estimatedParams.diffmodel$a
z <- 0.5
v <- estimatedParams.diffmodel$v
t <- estimatedParams.diffmodel$t0

outputname <- vector(mode="character", length = length(subjects))

for (i in seq_along(subjects)){
  outputname[i] <- paste(subjects[i], ".dat", sep = "")
}
input <- paste("plot-density.exe", "-a", a ,"-z" ,z, "-v", v, "-t", t ,"-o" ,outputname)

# system call with parameters. output is in wd
for (comm in input){
  system(comm, invisible = FALSE, intern = TRUE, wait = TRUE)
}


## PDF
pdf_path <- "./pdfparams"
all_pdf_files <- list.files(path = pdf_path, pattern = "*.dat")
pdf_params <- list()
for (i in seq_along(all_lst_files)) {
  pdf_params[[i]] <- read.table(file = file.path(pdf_path, all_pdf_files[i]), header=TRUE, sep="")
}

### cdf params
setwd("~/Master/WS15-16/HiwiJobAndreas/rprojects/Auswertung")
lst_path <- "./cdfParams"
corrected_lst_path <- "./correctedcdfParams"
all_lst_files <- list.files(path=lst_path, pattern = "*.lst")
corrected_lst_files <- list.files(path=corrected_lst_path, pattern = "*.lst")
# list of all data
cdf_params <- list()
for (i in seq_along(all_lst_files)) {
  cdf_params[[i]] <- read.table(file = file.path(lst_path, all_lst_files[i]), header=TRUE, sep="")
}

corrected_cdfparams <- list()
for (i in seq_along(corrected_lst_files)){
  corrected_cdfparams[[i]] <- read.table(file = file.path(corrected_lst_path, corrected_lst_files[i]), header=TRUE, sep="")
}

# plot of ratio from estimated t0 to mean of reaction times
difference_t_mean <- t - mean_rt_PT
plot(difference_t_mean, type="l")


## clean up workspace
# keep cdf_params and pdf_params
rm(a,z,v,t, all_lst_files, all_pdf_files, lst_path, pdf_path, path, outputname, input )
