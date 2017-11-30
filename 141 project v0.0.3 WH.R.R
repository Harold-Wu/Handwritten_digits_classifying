#########################################
#v0.0.1: Q1, 2, 3a, 4.  *WH*
#V0.0.2: Q1 revise: as.factor. Q2 revise: add error warning. Add comments.  *WH*
#v0.0.3: Q1 revise: add error warning. Q4 revise: rewrite the function.  *WH*
#########################################
#1
unzip("digits.zip")
unzip("digits.zip", list = T)

read_digits <- function(file_type = NULL) {
    if (file_type != "train" & file_type != "test") {
        stop("no such file name, please input either train or test.")    #if input file_type is wrong, return error.
    }
    rawdata <- read.table(paste(file_type, ".txt", sep = ""))    #paste full file name.
    rawdata[,1] <- as.factor(rawdata[,1])    #convert the labels into factor.
    rawdata
}

#2
train_data <- read_digits("train")
test_data <- read_digits("test")
grays = rgb(red = 0:255/255, blue = 0:255/255, green = 0:255/255)    #define the grayscale color.
view_digit <- function(file_type = NULL, observation_number = NULL) {
    if (file_type != "train" & file_type != "test") {
        stop("no such file name, please input either train or test.")    #if input file_type is wrong, return error.
    }
    if (file_type == "train" & observation_number %in% c(1:7291) == F) {
        stop("exceed the maximum observations, please input an integer between 1 to 7291.")    #if input obs # is wrong, return error.
    } else if (file_type == "test" & observation_number %in% c(1:2007) == F) {
        stop("exceed the maximum observations, please input an integer between 1 to 2007.")
    }
    if (file_type == "train") {
        rawdata <- train_data
    } else if (file_type == "test") {
        rawdata <- test_data
    }
    digit_matrix <- matrix(as.numeric(rawdata[observation_number, 2:257]), 16, 16)    #convert pixel data into matrix.
    digit_matrix <- digit_matrix[,ncol(digit_matrix):1]    #turn the matrix upside down.
    image(digit_matrix, col = grays, axes = F)    #display image.
}

view_digit("train", 1123)    #an example.

#3
par(mfrow = c(2,5))    #align the output images into 2 rows & 5 columns.
for (i in 0:9) {
    digit_data <- train_data[which(train_data$V1 == i),]    #extract data.
    digit_mean <- colMeans(digit_data[, 2:257])    #calculate mean for each pixel points.
    digit_mean <- matrix(as.numeric(digit_mean), 16, 16)    #convert pixel data into matrix.
    digit_mean <- digit_mean[,ncol(digit_mean):1]    #turn the matrix upside down.
    image(digit_mean, col = grays, axes = F)    #display image.
}
#########3.2

#4
predict_knn <- function (test_points = NULL, train_points = NULL, Minkowski_p = 2, k = 3) {
    train_index <- train_points[,1]    #extract label (col1) in training data.
    train_matrix <- as.matrix(train_points[,2:257])    #extract pixel points data in training data.
    real_index <- test_points[,1]    #extract label (col1) in testing data.
    test_matrix <- as.matrix(test_points[,2:257])    #extract pixel points data in testing data.
    train_nrow <- nrow(train_matrix)    #obs # in training data.
    test_nrow <- nrow(test_matrix)    #obs # in testing data.
    predict_result <- matrix(nrow = test_nrow, ncol = 2)    #build empty matrix to record the k nearest distance for each test obs.
    predict_result[,1] <- real_index    #fill col1 with testing real label.
    for (i in 1:test_nrow) {
        test_distance <- abs((rep(1, times = train_nrow) %*% t(test_matrix[i,])) - train_matrix)    #absolute difference value between each pixel pairs.
        test_distance <- rowSums(test_distance^Minkowski_p)^(1/Minkowski_p)    #claculate Minkowski Distance.
        test_distance <- cbind(train_index, test_distance)    #link distance with label.
        k_label <- test_distance[order(test_distance[,2])[1:k],1]    #sort and choose the k labels with k nearest distance.
        top_k_label <- sample(names(which(table(k_label) == max(table(k_label)))), 1)    #choose the one with highest prob and sample from the ties.
        predict_result[i,2] <- top_k_label    #fill in the labels.
        colnames(predict_result) <- c("real_digit", "knn")    #change colname: the smallest number after "d" indicate the shortest distance.
    }
    predict_result    #output the table.
}

f <- predict_knn(test_data[1:100,], train_data, 2, 3)    #an example.
# prediction of full test data (2007 obs) takes 52.115 s, will be improve later...




