# This R Code demonstrates everything we did in class 1 with an example dataset. 
# For more detailed and basic examples refer to the presentation.


# Before you work on the code, please set your working directory to the folder that 
# contains the sample dataset. You can either do this by navigating to the folder in your RStudio 
# Files tab, and then clicking on "More--> Set As Working Directory", or by using the setwd() command
# setwd("~/FIN 567/R") 

# Setwd sets your working directory. I have used the filepath I have used to locate my folder. 
# Please edit the filepath according to where your file is stored


df = read.csv("sample data.csv") # reading in the sample csv file containing stock data

# Some Basic functions before we manipulating the data

nrow(df) # number of rows in df, the data frame containing stock data
ncol(df) # number of columns
head(df, 5) # display 1st 5 rows on df
tail(df) # display 1st 6 (default value of the argument)
str(df) # function to compactly display the structure of df
summary(df) #summary statistics of each column

ftse = df[,3] # subsetting the 3rd column of df containing FTSE index and saving it in ftse vector
dax = df[,4] #doing the same for DAX, which is contained in the 4th column
gbp = df[,5] #storing GBP conversion rate separately
eur = df[,6] #doing the same for EUR

newftse = ftse*gbp #converting the prices of FTSE index to dollar byy dividing 
                   #with the corresponding currency rate 
newdax = dax*eur #same for DAX index

#Note that in practice you probably won't be saving each of the columns in seperate vectors. 
#Usually we would just combine these operations and store just the final value in a vector
#To illustrate this I have combined the above commands together in the following two commands 

#newftse = df[,3]*df[,5] 
#newdax = df[,4]*df[,6]  

newprices = data.frame(sp500 = df[,2], newFTSE = newftse, newDAX = newdax) 
#creating a new data frame combining the stock indices of 3 markets in USD

logret = log(newprices[1:nrow(newprices)-1,])-log(newprices[2:nrow(newprices),])
#computing a new data frame that contains log returns of previous data frame

logret = data.frame(Date = df[1:nrow(logret),1],logret)
#combining logret with the "Date" column from the original data frame and overwriting logret

ftse_rvrse = ftse[length(ftse):1] #reversing the order of the ftse vector
ftse_rvrse[length(ftse_rvrse)] #accessing the last element of the newly reversed ftse_rvrse vector
ftse[1] #You will notice that the fist element of the ftse and the 
        #last element of the ftse_rvrse vector are the same

#Similarly let's try to reverse the logreturns dataframe to show the oldest dates first

logret_rvrse = logret[nrow(logret):1,] #notice the difference is only a comma giving a second attribute
logret_rvrse[1,] #displaying 1st row of the reversed dataframe logret_rvrse
logret[nrow(logret),] #displaying last row of logret. Note that they are the same

##Example for matrix multiplication
mat1 = matrix(1:4,2,2)
mat1
mat2 = matrix(2:5, 2,2)
mat2
mat_res = mat1*mat2
mat_res # element wise multiplication
mat_res1 = mat1%*%mat2 #matrix multiplication
mat_res1
mat3 = matrix(2:7, 2, 3)
mat3
mat4 = matrix(rep(10,6), 2,3) #matrix with 6 same elements
mat4
mat_res3 = mat3*mat4 # element wise multiplication
mat_res3
mat_res4 = mat3%*%mat4 #Matrix multiplication. Gives an error, as the condition for matrix multiplication is not met

