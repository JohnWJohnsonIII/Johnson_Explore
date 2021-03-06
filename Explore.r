#Homework 7

##Prof G: Please submit a completed assignment

explore <-function(data, vect, min_cor) {
    #function1
    #function2
    r_squared(data)
    #4
      #function4bi sum_stats goes here
      rsquare(data) 
      #functionbiii pearson_r(data, min_cor)
}

num_vars <- function(data) {
  num_vars <- which(sapply(data, is.numeric))
  num_vars <- data[num_vars]
  return (num_vars)
}



#1
#for every numerical variable
  #at each bin size specified in the bin sizes input parameter
  #plot a count histogram
  #plot a density histogram
  #in each, have a red line at the mean
#plot individually or as a grid
num_var_plot <- function(data, vector){
  require(ggplot2)
  require(grid)
  frame <- num_vars(data) #create data frame of numeric variables
  n <- ncol(frame) #n = number of variables in numeric data frame
  bin_size <- vector # bin sizes dictated by values in vector given in input parameter
  for (i in 1:n){
    count_hists <- ggplot(frame, aes(x=frame[i], fill = 'blue')) 
    count_hists <- count_hists + geom_histogram(binwidth=bin_size[i]) +
      abline(v = mean(frame[i]), color = 'red') #abline should generate red line at mean of variable being plotted
    i + 1      #adding to counter should move loop to next variable
  return(count_hists)}
  for (i in 1:n){
    dens_hists <- ggplot(frame, aes(x=frame[i], fill = 'blue'))
    dens_hists <- dens_hists + geom_histogram(binwidth = bin_size[i]) +
      aes(y=..density..) + abline(v = mean(frame[i]), color = 'red') #aes creates density histogram
    i + 1 # adding 1 to counter should move to next variable
  return(dens_hists)
  plot.new()
  }
}

num_var_plot(diamonds, c(5, 20, 50))

#2 plot a gray bar graph for every categorical and binary variables
cat_var_plot <- function(data) {
  require(ggplot2)
  require(grid)
  n <- ncol(data) 
  cat_vars <- data.frame()
  for (i in 1:n){
    if (class(data[i]) == "ordered"){
      cat_vars[i] <- data[i]
    } else if (class(data[i]) == "factor") {
      cat_vars[i] <- data[i]
    } else {
      data[i] <- NA
  }}
  var_plot <- ggplot2(cat_vars, x = cat_vars[i])
  var_plot <- var_plot + geom_bar(aes(fill= 'gray'))
}
cat_var_plot(diamonds)


#3 calculate the r^2 value for every pair of numerical variables in data set

r_squared <- function(data){ #creating function. will use data set
  for (i in data) 
  frame <- num_vars(data) #creates new data frame for numeric variables
  r_squared <- cor(frame)^2 #calculates Pearson's R, then squares it to get R-Squared of each pair
  r_squared[col(r_squared) == row(r_squared) | upper.tri(r_squared)] <- NA  #assigns value of NA to repeat pairs
  r_squared <- subset(as.data.frame.table(r_squared), !is.na(Freq))  #creates table of pairs and corresponding r-squared
  colnames(r_squared) <- c("X", "Y", "R-Squared") #renames columns to appropriate title
return(r_squared) 
}

r_squared(diamonds) 


#4 All items returned in an R list

#4a #frequency table for every categorical and binary variable
freq_table <- function(data){
 freq_table <- which(sapply(data, is.ordered)) #determining categorical data
 freq_table_frame <- data[freq_table] #creating data frame of categorical data
 n <- ncol(freq_table_frame) #determine number of times for loop to execute
 for (i in 1:n){ #create loop
   freq_table_frame <- frequency(freq_table_frame[i]) 
   i + 1
 return(freq_table_frame)
 }
}
freq_table(diamonds)


#4bi
sum_stats <- function(data){ #not looping through all numeric variables
  frame <- num_vars(data) #create new data frame for numeric variables
  n <- ncol(frame) #determine number of times loop needs to execute
  for (i in 1:n){ #create for loop
    sum_stats <- summary(frame[i]) #generate summary statistics for column i and put in sum_stats #append 
    i + 1 #add 1 to count so next execution of loop moves to next column in data frame
  return(sum_stats)} #return all summary stats executed once loop complete
}

sum_stats(diamonds)


#4bii
rsquare <- function(data) {
  frame <- num_vars(data)
  rsquare <- cor(frame, method = "pearson")^2
  rsquare[col(rsquare) == row(rsquare) | upper.tri(rsquare)] <- NA
  rsquare <- subset(as.data.frame.table(rsquare), !is.na(Freq))
  colnames(rsquare) <- c("X", "Y", "R Squared")
  return(rsquare)
}
rsquare(diamonds)

#4biii
#THRESHHOLD NOT WORKING
pearson_r <- function(data, min_cor) {
  frame <- num_vars(data)
  pearson_r <- cor(frame, method = "pearson") #calculates variance for all pairs in data frame  
  pearson_r[col(pearson_r) == row(pearson_r) | upper.tri(pearson_r)] <- NA #designates repeat pairs as NA
  for (i in pearson_r[3]) {
    print (i)
    if (abs([i]) < min_cor) {
      return NA
  } else {
      return [i]
  }
  pearson_r <- subset(as.data.frame.table(pearson_r), !is.na(Freq)) #excludes repeat pairs, creates data table of variance
  colnames(pearson_r) <- c("X", "Y", "Pearson's r") # renames columns to X, Y, and Pearson's r
  return(pearson_r)
}

pearson_r(diamonds, 0.4)


#5
