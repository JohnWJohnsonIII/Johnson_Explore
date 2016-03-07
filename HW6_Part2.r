#HW6--PART 2

##Prof G: Did not require ggplot or load Diamonds?
require(ggplot2)
data(diamonds)

#1
extracted_data <- which(sapply(diamonds, is.numeric))
extracted_data_frame <- diamonds[extracted_data]
#print(extracted_data_frame)

#2
##Prof G: Did not get the variable pairs combined in a string like "Carat-Price"
##Prof G: See my function below:

# ####################################################
# # Function to generate R-square values and correlation
# # values. All R-square values are returned in one dataframe.
# # Only correlation (R) values that exceed the parameter 
# # threshold are returned in a separate dataframe.
# # The function also creates labels by combining the
# # names of the columns pairs into a single string
# # like "Carat-Price"
# ####################################################
# get_rsq_corr <- function(Num_Frame, threshold) {
#    #Create correlation matrix using cor function
#    #Specify pearson correlation
#    corr_a <- cor(Num_Frame, method="pearson")
#    #Set up a threshold and null vectors before entering loop
#    r_square <- NULL
#    rsq_names <- NULL
#    corr_list <- NULL
#    corr_names <- NULL
#    
#    #Get the length of the one dimension of the square matrix
#    len <- length(corr_a[1,])
#    
#    #Only loop through the upper right triangle
#    for (i in (1:(len-1))) {
#       for (j in ((i+1):len)) {
#          #Form the name pair and add to the named pair vector
#          pair_name <- paste(names(corr_a[,1])[[i]],names(corr_a[1,])[[j]],sep="-")
#          rsq_names <- c(rsq_names, pair_name)
#          
#          #Add the r_square value to the value vector
#          r_square <- c(r_square, corr_a[i,j]^2)
#          
#          #if the threshold is exceeded, add the name and value to the
#          #respective correlation vectors
#          if (abs(corr_a[i,j]) > threshold) {
#             corr_names <- c(corr_names, pair_name)
#             corr_list <- c(corr_list, corr_a[i,j]) 
#          }
#       }
#    }
#    
#    #create the dataframes and label the columns
#    rsq_df <- data.frame(cbind(rsq_names, r_square))
#    names(rsq_df)[1] <- "Pair"
#    names(rsq_df)[2] <- "Value"
#    corr_df <- data.frame(cbind(corr_names, corr_list))
#    names(corr_df)[1] <- "Pair"
#    names(corr_df)[2] <- "Value"
#    return(list("rsquare"=rsq_df, "correlation"=corr_df))
# }



#column containing names of each variable separated by '-'
data_set <- data.frame(extracted_data_frame)
pearson_r <- cor(data_set, method = "pearson") #correlates variables for pearson's r
pearson_r[col(pearson_r) == row(pearson_r) | upper.tri(pearson_r)] <- NA #identifying duplicate pairs and value as NA
pearson_r <- subset(as.data.frame.table(pearson_r), !is.na(Freq)) #excluding NA values from output
colnames(pearson_r) <- c("X","Y", "Pearson's 'r'") #relabels columns
#pearson_output <- paste(pearson_r[1], pearson_r[2], sep = "-", collapse = FALSE) #combine X & Y, creating hyphenated label
print(pearson_r) #running this one for now because pearson_output isn't working right
#print(pearson_output) #commented out because not working right. attempting to fix.

#3 #create and label a scatterplot for every pair of numeric variables
  #add a title that contains the 'r' of the pair

##Prof G: Where is function ggpairs?
ggpairs(extracted_data_frame) #builds a scatterplot matrix of all variables in the data frame. #extracted_data_frame is the numeric data pulled in #1
#also returns pearson's r of each pair
