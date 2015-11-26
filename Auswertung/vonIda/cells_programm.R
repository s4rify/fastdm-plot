
# insert here the working directory, where all the datasets are saved in as txt-files
setwd("G:/Cells/WD")
WD <- getwd();

# List of file names
all_cells <- list.files(path=WD, pattern = "*.txt")


# list of all data
data <- list()
for (i in seq_along(all_cells)) {
  data[[i]] <- read.table(file = all_cells[i], header=TRUE, dec=",")
}

#---------------FUNCTION FOR PREPARING DATA----------------------------------------------------------------
msi_colonius <- function(cell, rows=1:20) {
  # order visual spike values in increasing order
  cell[rows,'V'] <- sort(cell[rows, 'V'], na.last = TRUE, decreasing = FALSE)
  cell[rows,'V.1'] <- sort(cell[rows, 'V.1'], na.last = TRUE, decreasing = FALSE)
 
  # order aditory spike values in decreasing order
  cell[rows,'A'] <- sort(cell[rows, 'A'], na.last = TRUE, decreasing = TRUE)
  cell[rows,'A.1'] <- sort(cell[rows, 'A.1'], na.last = TRUE, decreasing = TRUE)

  ####
  # max(V,A)
  for (i in rows) {
      cell[i, 'max'] <- max(cell[i,'V'], cell[i, 'A'])
  }
  
  # max(V.1, A.1)
  for (i in rows) { 
      cell[i, 'max.1'] <- max(cell[i,'V.1'], cell[i,'A.1'])
  }
  
  ####
  # calculate the means and SDs of all columns and add to data.frame
  M <- c(NA, mean(cell$V), mean(cell$A), mean(cell$AV),
          mean(cell$V.1), mean(cell$A.1), mean(cell$AV.1), mean(cell$max), mean(cell$max.1))

  SD <- c(NA, sd(cell$V), sd(cell$A), sd(cell$AV),
       sd(cell$V.1), sd(cell$A.1), sd(cell$AV.1), sd(cell$max), sd(cell$max.1))
  
  cell <- rbind(cell, M, SD )
  cell <- cbind(cell$Trial, cell$V, cell$A, cell$max, cell$AV,
                cell$V.1, cell$A.1, cell$max.1, cell$AV.1)
}

#------------FUNCTIONS FOR CALCULATING MSI---------------------------------------------------------------------
# OLD MSI-function for "Response with S.A."
MSI <- function(cell, rows=1:20) {
  ((mean(cell[rows,'AV']) - max(mean(cell[rows,'V']), mean(cell[rows,'A'])))/
     max(mean(cell[rows,'V']), mean(cell[rows,'A'])))*100  
}

# OLD MSI-function for "Response without S.A."
MSI.1 <- function(cell, rows=1:20) {
  ((mean(cell[rows,'AV.1']) - max(mean(cell[rows,'V.1']), mean(cell[rows,'A.1'])))/
     max(mean(cell[rows,'V.1']), mean(cell[rows,'A.1'])))*100  
}

#----------
#NEW MSI-function for "Response with S.A."
MSI_new <- function(cell, rows=1:20) {
  ((mean(cell[rows, 'AV']) - mean(cell[rows, 'max_V_A']))/
     mean(cell[rows, 'max_V_A']))*100
}

#NEW MSI-function for "Response without S.A."
MSI.1_new <- function(cell, rows=1:20) {
  ((mean(cell[rows, 'AV.1']) - mean(cell[rows, 'max.1_V.1_A.1']))/
     mean(cell[rows, 'max.1_V.1_A.1']))*100
}


#---------APPLY THE FUNCTIONS---------------------------------------------------------------------

##### apply the preparing-function to each of the datasets
data_new <- list()
for (j in seq_along(all_cells)) {
  
  data_new[[j]] <- msi_colonius(cell=data[[j]])
  colnames(data_new[[j]]) <- c('Trial', 'V', 'A', 'max_V_A', 'AV', 'V.1', 'A.1', 'max.1_V.1_A.1', 'AV.1')
}


##### calculate the MSI values by old and new formula and save in matrix "msi_list"
msi_list <- matrix(data=NA, nrow=4, ncol=length(all_cells))
for (j in seq_along(all_cells)) {
  msi_list[1,j] <- MSI(cell=data_new[[j]])
  msi_list[2,j] <- MSI_new(cell=data_new[[j]])
  msi_list[3,j] <- MSI.1(cell=data_new[[j]])
  msi_list[4,j] <- MSI.1_new(cell=data_new[[j]])
  rownames(msi_list) <- c('MSI_old', 'MSI_new', 'MSI.1_old', 'MSI.1_new')
  colnames(msi_list) <- c('Cell1', 'Cell2', 'Cell3')
}

data_new
msi_list

