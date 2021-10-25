#CMSC 197: DATA SCIENCE
#JEWEL JADE BARTOLOME 
#BS STATISTICS III 
#1ST MINI PROJECT

##PART 1
#Problem 1: Write a function named pollutantMean that calculates the mean of a pollutant (sulfate or nitrate) across a 
#specified list of monitors. The function pollutantMean takes 3 argument: directory, pollutant, and id. Given a 
#vector monitor ID numbers, pollutantMean reads that monitor’s particulate matter data from the directory specified 
#in the directory argument and returns the means of the pollutant across all of the monitors, ignoring any missing 
#values coded as NA. 

#STEPS

#1: Create new project and set up working directory 
#2:Use getwd() function to see working directory
#3: transfer the unzipped files to the working directory folder
#4: create a variable that stores all .csv files in the console
#data <- read.csv ("001.csv", header=TRUE)

#store the working directory to specdata to make it easier to recall the directory 

specdata <- ("C:/Users/customer/Desktop/specdata/")

#pollutantmean is the function that holds 3 arguments; the directory, pollutant and id.
#directory is the character vector including all .csv files
#pollutant is a character vector where 'nitrate' and 'sulfate' values are stored
#id is a numerical vector that corresponds a .csv file respectively. (1-332 files)

pollutantmean <- function(directory, pollutant, id=1:332){
  
  #csv_files is the variable that stores all .csv files only, to be used in the loop
  #the list.files function creates the character vector of the names of the files
  #the pattern function only takes .csv files based on the argument
  
  csv_files <- list.files(path=directory, pattern= ".csv") 
  
  #let x represent the numeric variables; it is an empty numeric vector
  
  x <- numeric() 
  
  #create a 'for' loop that will run all monitor ids
  
  for(i in id){
    
    #assign all .csv files read to csv_data 
    
    csv_data <- read.csv(csv_files[i])
    
    #update the empty numerical vector x by storing the values taken from the pollutant vector 
    #the double brackets is necessary to access a single element, instead of a list in every file
    
    x<- c(x, csv_data[[pollutant]])
    
  }
  
  #after storing all numeric values in x, compute the mean of either "sulfate" of "nitrate" pollutants
  #na.rm= TRUE is the argument used to ignore all missing values
  
  mean (x, na.rm=TRUE)
}

#save the Rscript and run the codes
#In the console, Set the pollutantmean.R as the source function by using source("pollutantmean.R")

#RUN SAMPLE CODES
#pollutantmean(specdata, "sulfate", 1:10)
#pollutantmean(specdata, "nitrate", 70:72)
#pollutantmean(specdata, "nitrate", 23)


#PART 2

#write a function named complete that reads a directory full of files and reports the number of completely observed cases in each data file.
#The function should return a data frame where the first column is the name of the file, and the second column is the number of complete cases.


#store the working directory to specdata to make it easier to recall the directory 

specdata <- ("C:/Users/customer/Desktop/specdata/")

#complete is the function that stores 2 arguments; directory and id 
#directory is the character vector including all .csv files
#id is a numerical vector that corresponds a .csv file respectively. (1-332 files)

complete<- function(directory, id=1:332){
  
  #complete_list is the vector that lists all .csv files from the directory using the list.files function
  #the pattern argument only considers .csv files
  
  complete_list <- list.files (path=directory, pattern=".csv")
  
  #nobs means the number of observations; it is an empty numeric vector
  
  nobs <- numeric()
  
  #create a 'for' loop that will run all observations
  
  for (i in id){
    
    #using the read.csv function, it will read all .csv observations and store it in the data_list
    
    data_list <- read.csv(complete_list[i])
    
    #the data_sum is a vector that stores the sum of all the cases in the data_list
    
    data_sum <- sum(complete.cases(data_list))
    
    #update the vector nobs by storing the sum of all cases from the data_sum
    
    nobs <- c(nobs, data_sum)
    
  }
  #create a data frame that includes the variables id and nobs. It will show the desired information depending on the given
  
  data.frame(id, nobs)
}

#save the Rscript and run the codes
#In the console, Set the complete.R as the source function by using source("complete.R")

#RUN SAMPLE CODES
#complete(specdata, 1)
#complete(specdata, c(2,4,8,10,12))
#complete(specdata, 30:25)
#complete(specdata, 3)


#PART 3
#write a function named corr that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the 
#number of completely observed cases (on all variables) is greater than the threshold. The function should return a 
#vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0

#store the working directory to specdata to make it easier to recall the directory 

specdata <- ("C:/Users/customer/Desktop/specdata/")

#corr is a function that stores the directory and the threshold argument equal to 0

corr <- function(directory, threshold = 0){
  
  #corr_list is the vector that lists all .csv files from the directory using the list.files function
  #the pattern argument only considers .csv files
  
  corr_list <- list.files(path=directory, pattern=".csv")
  
  #corr_df is a complete function where the directory is called
  #df means data frame
  
  corr_df <- complete(directory)
  
  #ids is the vector that stores the data frame corr_df where the variable "nobs" are taken in terms of rows 
  #and it should be greater than the threshold valued at zero, for every id ($id)
  
  ids <- corr_df[corr_df["nobs"] > threshold, ]$id
  
  #num_corr is an empty numerical vector
  
  num_corr <- numeric()
  
  #create a for loop for the ids
  
  for(i in ids){
    
    #corr_data is a vector that stores all the read .csv files stored in corr_list
    
    corr_data <- read.csv(corr_list[i])
    
    #comp_df is the variable where all complete cases from the corr_data are stored
    
    comp_df <- corr_data[complete.cases(corr_data), ]
    
    #num_corr stores the data which computes the correlation of sulfate and nitrate using the cor function
    
    num_corr <- c(num_corr, cor(comp_df$sulfate, comp_df$nitrate))
  }
  
  #the result of the loop is returned/ updated to the empty num_corr vector
  #the num_corr vector now stores the numeric values of the correlation between nitrate and sulfate
  
  return(num_corr)
}

#save the Rscript and run the codes
#In the console, Set the corr.R as the source function by using source("corr.R")
#store the corr function to cr

#RUN SAMPLE CODES

#cr<- corr(specdata, 150)
#head(cr); summary(cr)

#cr<- corr(specdata, 400)
#head(cr); summary(cr)

#cr<- corr(specdata, 5000)
#head(cr); summary(cr); length(cr)

#cr<- corr(specdata)   
#head(cr); summary(cr); length(cr)


#PART 4

#1: Create new project and set up working directory 
#2:Use getwd() function to see working directory
#3: transfer the unzipped files to the working directory folder

#Store the read outcome-of-care-measures.csv file in the variable outcome 
#set the argument for colClasses for characters only

outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")

#use the ncol/ nrow functions to view the number of variables (rows/ columns)

ncol(outcome)
nrow(outcome)

#using the as.numeric function, generate the numeric values of outcome[,11]
#11 is equal to the number of columns in the data set
#the NAs dooes not affect the overall outcome 

outcome[, 11] <- as.numeric(outcome[, 11])

#generate a histogram from the data in outcome[,11] using the hist function
#the main argument sets the title of the graph which is "Hospital 30-Day Death (Mortality) Rates from Heart Attack"
#xlab is the argument for labeling the x-axis with "Deaths"
#col sets the color of the graph, which is lightblue
#border sets the color of the border which is black

hist(outcome[, 11], main= "Hospital 30-Day Death (Mortality) Rates from Heart Attack", xlab = "Deaths", col = "lightblue", border = "black")

#after running the function, a tab for the graph will automatically open.
