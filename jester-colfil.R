library(dplyr)

theURL <- "https://raw.githubusercontent.com/Tyllis/Data607/master/jester-data-1.csv"
raw <- read.csv(theURL, header = F, stringsAsFactors = F)
num_rate <- raw[,1]
jester <- raw[,-1]
num_row <- dim(jester)[1]
num_col <- dim(jester)[2]
names(jester) <- c(1:num_col)

# Replace all value 99 into NA
jester <- as.data.frame(apply(jester, 2, function(x) replace(x, x==99, NA)))

# Save indices of NA
na_idx <- apply(jester, 1, function(x) which(is.na(x)))

################################################################################

# Function that takes a vector and returns the magnitude of the vector
mag <- function(x) sqrt(sum(x^2, na.rm=T))

# Function that takes two vectors and returns the dot product of the two vectors
dot <- function(x,y) sum(x*y, na.rm=T)

# Function that takes two vectors and returns the cosine of the angles between the two vectors
cosine <- function(x,y) dot(x,y)/ (mag(x)*mag(y))

# Function that takes a vector and subtract the mean from each vector element.
center <- function(x) x-mean(x, na.rm=T)

# Function that takes two vectors and return the similarity of the vectors.
sim <- function(x,y) cosine(center(x), center(y))

# Create similarity matrix
sim_mat <- c()
for (col in 1:num_col){
  sim_mat <- rbind(sim_mat, apply(jester, 2, sim, jester[,col]))
}

# Function that takes a similarity vector x and a integer n and returns the top n neighbors 
# but excluding the elements in the vector i which contains the indices. It returns
# a vector containing the similarity values, and the names of the vector are their indices.
n_neighbor <- function(x, i, n){
  if(length(na_idx[[i]])!=0) x=x[-na_idx[[i]]]
  head(sort(x, decreasing = T), n+1)[-1]
} 

# Function that takes a jokeID and a userID and estimate a rating based on collaborating filtering
cal_rating <- function(userID, jokeID, n){
  neighbors <- n_neighbor(sim_mat[jokeID,], userID, n)  
  idx <- as.integer(names(neighbors))
  val <- jester[userID, idx]
  weighted.mean(val, neighbors)
}

b <- proc.time()
jester_cf <- jester
predict_ratings <- list()
for (i in 1:num_row){
  print(paste("Processing Row", i))
  temp <- c()
  if (length(na_idx[[i]])!=0){ 
    for (j in na_idx[[i]]){
      val <- cal_rating(i,j,5)
      jester_cf[i,j] <- val
      temp <- c(temp, val)
    }
    names(temp) <- na_idx[[i]]
  }
  predict_ratings[[i]] <- temp
}
e <- proc.time()
e-b

write.csv(jester_cf, "C:\\Users\\jun.yan\\Desktop\\Course\\jester\\jester_cf.csv")

plot(jester[,1],jester[,2], xlab="Joke #1 Ratings", ylab="Joke #2 Ratings")
sim(jester[,1],jester[,2])
plot(jester[,8],jester[,98], xlab="Joke #8 Ratings", ylab="Joke #98 Ratings")
sim(jester[,8],jester[,98])

weighted.mean(c(8.2,6.5,5.5,9.0,-4.3),c(0.32,0.21,0.16,0.11,0.07))
View(jester)

### analysis
a <- predict_ratings[[3]]
b <- as.numeric(jester[3,][-which(is.na(jester[3,]))])
par(mfrow=c(1,2)) 
hist(a, xlim=)
hist(b)

max_rating <- max(predict_ratings[[3]])
max_rating
which(predict_ratings[[3]] == max_rating)
jokeID <- names(predict_ratings[[3]])[30]
jokeID