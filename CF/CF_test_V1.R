# Load required library
library(recommenderlab)
library(ggplot2) # For plots
# Load the data we are going to work with
data(MovieLense)
MovieLense

#Visualizing a sample of this
image(MovieLense, main="Raw ratings") #Appears that some movies were not rated at all by first few users, maybe because these movies were released later?

summary(getRatings(MovieLense)) # Skewed to the right

#Visualizing ratings
qplot(getRatings(MovieLense), binwidth = 1,main = "Histogram of ratings", xlab = "Rating")

#What about normalization?
qplot(getRatings(normalize(MovieLense, method = "Z-score")),main = "Histogram of normalized ratings", xlab = "Rating")
summary(getRatings(normalize(MovieLense, method = "Z-score"))) #Normalization does work a bit! (It is able to adjust for bias of individual raters)

# How many movies did people rate on average?
qplot(rowCounts(MovieLense), binwidth = 10,main = "Movies Rated on average", xlab = "# of users", ylab = "# of movies rated") #We see some people got tired of rating movies

# What is the mean rating of each movie
qplot(colMeans(MovieLense), binwidth = .1, main = "Mean rating of Movies", xlab = "Rating", ylab = "# of movies") # See big spikes for 1, 3 and 5 (people got tired of rating) ==> give binary treatment later

#############################################################################################

#What algorithms can we test?
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

# We have a few options
 # Split the data into train and test. Here, train is 90%.
 # For testing it will take any 10 movie ratings by the user
 # and predict n others. Then compare to see if they match.

scheme <- evaluationScheme(MovieLense, method = "split", train = .9,k = 1, given = 10, goodRating = 4)
scheme

# Here we are using split, but other schemes are also available
# For production testing, STRONGLY recommended using cross-validation scheme

# Let's check some algorithms against each other
algorithms <- list(
  "random items" = list(name = "RANDOM", param = list(normalize = "Z-score"))
  #"popular items" = list(name = "POPULAR", param = list(normalize = "Z-score")),
  #"user-based CF" = list(name = "UBCF", param = list(normalize = "Z-score",
                                                 #method = "Cosine",
                                                 #nn = 50, minRating = 3)), #minRating ==> Do not include rating less than 3
  #"item-based CF" = list(name = "IBCF", param = list(normalize = "Z-score"
                                                     #))
  
)

#run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n = c(1, 3, 5, 10, 15, 20)) ##ERROR minRating in user-based CF is not recognized##

# Draw ROC curve
plot(results, annotate = 1:4, legend = "topleft") ##Looks weird I think it is because of hte previous error
# See precision / recall
plot(results,"prec/rec", annotate = 3) ##Looks weird I think it is because of hte previous error
#Comment:
  #Popular best: All this means that the movies rated high are usually liked by everyone and are safe recommendations.

#######################

##Let's implement few collaborative filtering algorithms

#Let's start with a regular matrix of 5 users, 10 items
set.seed(2358)
my.mat <- matrix(sample(c(as.numeric(-2:2), NA), 50,
                          replace = TRUE,
                          prob = c(rep(.4/5,5), .6)), ncol = 10,
                  dimnames = list(user = paste("u", 1:5, sep = ''),
                                  item = paste("i", 1:10, sep = '')))
my.mat
# Here user u2 has rated i2 as 2 and i3 as 0.
# Please note that 0 could be a valid value
# All unrated values are NA

# Convert this to realRatingMatrix
(my.realM <- as(my.mat, "realRatingMatrix"))

str(my.realM)
# Hmm, can we look at the underlying object?
rating.obj <- my.realM@data
# This is the class called sparse Matrix (notice the uppercase M)
# By default all 0 s in Matrix are dropped to save space.
# Since we expect mostly NAs, it has taken our input mat,
# and converted it to 0 s. We can do this another way

dropNA(my.mat)

identical (rating.obj, dropNA(my.mat))

# OK, let's convert it back
as.matrix(rating.obj)

#This is wrong. We had NAs!
# What happened here is as.matrix applied to Matrix class,
# and so it translated it zeroes instead of NAs.
# For the right translation, we need -
as (my.realM, "matrix")

################################################################################
# Latent Factor Collaborative Filtering                                        #
################################################################################

## helper functions and registry
# From AAA.R
.get_parameters <- function(p, parameter) {
    if(!is.null(parameter) && length(parameter) != 0) {
      o <- pmatch(names(parameter), names(p))
      
      if(any(is.na(o)))
      stop(sprintf(ngettext(length(is.na(o)),
                   "Unknown option: %s",
                   "Unknown options: %s"),
               paste(names(parameter)[is.na(o)],
                   collapse = " ")))
      
      p[o] <- parameter
    }
    
    p
}

# Now our new method using SVD
REAL_SVD <- function(data, parameter = NULL) {
  
  p <- .get_parameters(list(
    categories = 50,
    method = "Cosine",
    normalize = "center",
    normalize_sim_matrix = FALSE,
    alpha = 0.5,
    treat_na = "0",
    minRating = NA
    ), parameter)
  
  # Do we need to normalize data?
  if(!is.null(p$normalize))
    data <- normalize(data, method = p$normalize)
    
  # Just save everything for now.
  model <- c(list(
      description = "full matrix",
      data = data
      ), p)
    
  predict <- function(model, newdata, n = 10,
                      type = c("topNList", "ratings"), ...) {
    
    type <- match.arg(type)
    n <- as.integer(n)
    
    # Do we need to denormalize?
    if(!is.null(model$normalize))
      newdata <- normalize(newdata, method = model$normalize)
    
    # Get the old data
    data <- model$data@data
    # Add new data to it to create combined matrix
    data <- rBind(data, newdata@data)
    
    ### svd does as.matrix which sets all missing values to 0!
    # So we have to treat missing values before we pass it to svd (fix by Michael Hahsler)
    data <- as(data, "matrix")
    if(model$treat_na == "min") data[is.na(data)] <- min(data, na.rm = TRUE)
    else if(model$treat_na == "mean") data[is.na(data)] <- mean(data, na.rm = TRUE)
    else if(model$treat_na == "median") data[is.na(data)] <- median(data, na.rm = TRUE)
    else if(model$treat_na == "max") data[is.na(data)] <- max(data, na.rm = TRUE)
    else if(model$treat_na == "0") data[is.na(data)] <- 0
    else stop("No valid way to treat NAs specified (treat_na)!")
    
    # Calculate SVD using available function
    s<-svd(data)
    # Get Diag but only of p
    elements
    S <- diag(s$d[1:p$categories])
    
    # Multiply it back up, but only using p elements
    ratings <- s$u[,1:p$categories] %*% S %*% t(s$v[,1:p$categories])
    
    # Put back correct names
    rownames(ratings) <- rownames(data)
    colnames(ratings) <- colnames(data)
    
    # Only need to give back new users
    ratings <- ratings[(dim(model$data@data)[1]+1):dim(ratings)[1],]
    
    # Convert to right type
    ratings <- new("realRatingMatrix", data = dropNA(ratings))
    ## prediction done
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(!is.null(model$normalize))
      ratings <- denormalize(ratings)
    
    if(type == "ratings") return(ratings)
    
    getTopNLists(ratings, n = n, minRating = model$minRating)
    
  }
  
  ## construct recommender object
  new("Recommender", method = "SVD", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
  
}

# Add it to registry
recommenderRegistry$set_entry(
  method="SVD", dataType = "realRatingMatrix", fun=REAL_SVD,
  description="Recommender based on SVD approximation (real data).")

#Fixing missing value (see as done before)
algorithms <- list(
  "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=50, minRating=3)),
  "item-based CF" = list(name="IBCF"", param=list(normalize = "Z-score"
                                                  )),
  "SVD CF" = list(name="SVD", param=list(normalize = "Z-score"
                                         treat_na = "0"
                                        ))
)

# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")
# See precision / recall
plot(results, "prec/rec", annotate=3)

################################################################################
#If more users than items, method of regular SVD works good too
#Let's try it out

REAL_PCA <- function(data, parameter= NULL) { ##GIVE warning message
  
  p <- .get_parameters(list(
    categories = 20,
    method="Cosine",
    normalize = "center",
    normalize_sim_matrix = FALSE,
    alpha = 0.5,
    na_as_zero = FALSE,

    minRating = NA
    ), parameter)
  
  if(!is.null(p$normalize))
    data <- normalize(data, method=p$normalize)
    
  # Perform PCA
  data <- data@data
 
  #We will use princomp function, there are other methods available as well in R
  # princomp does an as.matrix as well but it does not matter in this case
  pcv<-princomp(data, cor=TRUE)
   
  # Get the loadings
  lpcv<-loadings(pcv)
   
  # Total number of categories
  cats <- min(dim(lpcv)[2], p$categories)
  
  # det(lpcv[,1:99] %*% t(lpcv[, 1:99]))
  # This is just a check. If this is close to 1 that means we did well.
   
  # Convert to right type
  itemcat <- new("realRatingMatrix",
                 data = as(lpcv[,1:cats], "dgCMatrix"))
  
  # Save the model
  model <- c(list(
      description = "PCA: Reduced item-category matrix",
      itemcat = itemcat
      ), p)
    
  predict <- function(model, newdata, n = 10,
                      type=c("topNList", "ratings"), ...) {
    
      type <- match.arg(type)
      n <- as.integer(n)
      
      if(!is.null(model$normalize))
        newdata <- normalize(newdata, method=model$normalize)
        
      ## predict all ratings
      u <- as(newdata, "dgCMatrix")
      itemcat <- as(model$itemcat, "dgCMatrix")
      ratings <- u %*% itemcat %*% t(itemcat)
       
      ratings <- new("realRatingMatrix", data=dropNA(ratings),
                     normalize = getNormalize(newdata))
      ## prediction done
        
      ratings <- removeKnownRatings(ratings, newdata)
      if(!is.null(model$normalize))
        ratings <- denormalize(ratings)
     
      if(type=="ratings") return(ratings)
      
      getTopNLists(ratings, n=n, minRating=model$minRating)
     
  }
  ## construct recommender object
  new("Recommender", method = "PCA", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}

# Add to registry
recommenderRegistry$set_entry(
  method="PCA", dataType = "realRatingMatrix", fun=REAL_PCA,
  description="Recommender based on PCA approximation (real data).")

rm(MovieLense, scheme) # Clean up
data(Jester5k) # Load another dataset
scheme.jester <- evaluationScheme(Jester5k, method = "split", train = .9,
                                  k = 1, given = 10, goodRating = 4)
scheme.jester

algorithms <- list(
  "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                   method="Cosine",
                                                   nn=50, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
                                                   )),
  "SVD CF" = list(name="SVD", param=list(normalize = "Z-score",
                                           treat_na = "0"
                                           )),
  "PCA CF" = list(name="PCA", param=list(normalize = "Z-score"
                                           ))
)
# run algorithms, predict next n movies 
results <- evaluate(scheme.jester, algorithms, n=c(1, 3, 5, 10, 15, 20)) ##ERROR


# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft") 
# See precision / recall
plot(results, "prec/rec", annotate=3)


################################################################################
# Low rank matrix factorization using stochastic gradient descent              #
################################################################################

