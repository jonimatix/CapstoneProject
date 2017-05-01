setwd("C:\\Users\\Jonathan\\Desktop\\Coursera Scripts\\10. Data Science Capstone\\")

# Blogs file
con <- file(".\\Data\\final\\en_US\\en_US.blogs.txt", "r")

res <- readLines(con)
TotalNoOfLines <- length(res) # 899288
max(nchar(res)) # 40835

close(con)


# News file
conNews <- file(".\\Data\\final\\en_US\\en_US.news.txt", "r")

res <- readLines(conNews)
TotalNoOfLines <- length(res) # 77259
max(nchar(res))  # 5760

close(conNews)


# Twitter file
conTwitter <- file(".\\Data\\final\\en_US\\en_US.twitter.txt", "r")

res <- readLines(conTwitter)
TotalNoOfLines <- length(res) # 2360148
max(nchar(res))  # 213

close(conTwitter)

# Count instances of the words
loveCount <- length(grep("love", res, perl=TRUE))
hateCount <- length(grep("hate", res, perl=TRUE))

loveCount/hateCount

# Get location of specific word
biostatsIndex <- grep("biostats", res, perl=TRUE)
biostatsIndex

res[biostatsIndex] 

grep("A computer once beat me at chess, but it was no match for me at kickboxing", res, perl=TRUE)






# 
# 
# 
# ## Loop over a file connection
# for(i in 1:100) {
#     tmp <- scan(con, nlines  = 3)
#     ## do something on a line of data 
#     print(tmp)
# }
# 
# 
# 
# sample(x,10,replace=F)
# 
# while((linesread<-length(readLines(con,readsizeof)))>0)    # calculate number of lines. Also a better solution for big file
#     nooflines<-nooflines+linesread
# 
# for(i in 1:1000) {
#     tmp <- scan(file=con, nlines=1, quiet=TRUE)
#     print(is.vector(tmp))
#     print(tmp)
# }
# con
# 
# ?rbinom
# 
# rbinom(n = 1, size = 1, prob = 0.2)
# 
# readLines(con, 1) ## Read the first line of text 
# readLines(con, 1) ## Read the next line of text 
# readLines(con, 5) ## Read in the next 5 lines of text 
# 
# close(con) ## It's important to close the connection when you are done
# 
