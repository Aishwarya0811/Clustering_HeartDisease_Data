
# loading the data
heart_disease = read.csv(file = "datasets/heart_disease_patients.csv")

# print the first ten rows of the data set
 head(heart_disease,n=10)

# check that only numeric variables
lapply(heart_disease, class)

# These packages need to be loaded in the first @tests cell. 
library(testthat) 
library(IRkernel.testthat)

soln_heart <- read.csv('datasets/heart_disease_patients.csv')

run_tests({
    test_that("heart disease data loaded correctly", {
    expect_equal(heart_disease, soln_heart, info="heart_disease does not have right data, check the csv file name")
    expect_identical(lapply(heart_disease, class)[1][[1]], 'integer', info="variables not all type numeric")
    })
})

# evidence that the data should be scaled?
summary(heart_disease)

# remove id
heart_disease = heart_disease[ , !(names(heart_disease) %in% c("id"))]

# scaling data and saving as a data frame
scaled = scale(heart_disease,center = TRUE, scale = TRUE)

# what does data look like now?
summary(heart_disease)

soln_heart_disease = read.csv('datasets/heart_disease_patients.csv')
no_id = soln_heart_disease[ , !(names(soln_heart_disease) %in% c("id"))]
scaled_data = scale(no_id)
soln_heart_disease = scaled_data

run_tests({
    test_that("remove correct column", {
        expect_identical(colnames(no_id), colnames(heart_disease), info = "Did you remove the id column?")
    })
    
    test_that("scaled data properly", {
        expect_identical(scaled_data, scaled, info = "Did you scale the proper data set?")
    })
})

# set the seed so that results are reproducible
seed_val = 
set.seed(10)


# select a number of clusters
k = 5

# run the k-means algorithms
first_clust = kmeans(scaled, centers = 5, nstart = 1)

# how many patients are in each group
first_clust$size



soln_seed_val = 10
set.seed(soln_seed_val)
soln_k = 5
soln_first_clust = kmeans(soln_heart_disease, centers = soln_k, nstart = 1)


run_tests({
    test_that("correct seed", {
        expect_equal(soln_seed_val, seed_val, info = "Is the seed set to 10?")
    })
    
    test_that("correct number of clusters", {
        expect_equal(soln_k, k, info = "Are you using five clusters?")
    })
    test_that("correct implmentation of algorithm", {
        expect_equal(soln_first_clust$size, first_clust$size, info = "What is your nstart value?")
    })
})

# set the seed
seed_val = seed_val_2
set.seed(38)

# run the k-means algorithms
k = 5
second_clust = kmeans(scaled, centers = 5, nstart = 1)

# how many patients are in each group
second_clust$size

seed_val_2 = 38
set.seed(seed_val_2)
k_2 = 5
soln_second_clust = kmeans(soln_heart_disease, centers = k_2, nstart = 1)

run_tests({
    test_that("correct seed", {
        expect_equal(seed_val_2, seed_val, info = "Is the seed set to 10?")
    })
    
    test_that("correct number of clusters", {
        expect_equal(k_2, k, info = "Are you using five clusters?")
    })
    test_that("correct implmentation of algorithm", {
        expect_equal(soln_second_clust$size, second_clust$size, info = "What is your nstart value?")
    })
})

# adding cluster assignments to the data
soln[,first_clust$clusters] = first_clust
soln[,second_clust$clusters] = second_clust


# load ggplot2
library(ggplot2)

# creating the plots of age and chol for the first clustering algorithm
plot_one = ggplot(heart_disease, aes(x=age, y=chol, color=as.factor(first_clust))) + geom_point()
plot_one 

# creating the plots of age and chol for the second clustering algorithm
plot_two = ggplot(heart_disease, aes(x=age, y=chol, color=as.factor(second_clust))) + geom_point()
plot_two

soln = heart_disease
soln['first_clust'] = soln_first_clust$cluster
soln['second_clust'] = soln_second_clust$cluster

# creating the correct graphs and getting fingerprints
soln_plot_one = ggplot(soln, aes(x=age, y=chol, color=as.factor(first_clust))) + geom_point()
soln_plot_two = ggplot(soln, aes(x=age, y=chol, color=as.factor(second_clust))) + geom_point()

run_tests({
    test_that("cluster assignments added", {
        expect_identical(soln, heart_disease, info = "Did you add a column for both the first and second iteration?")
    })
    
    test_that("ggplot2 loaded", {
        expect_true('ggplot2' %in% .packages(), info = "Did you load ggplot2?")
    })

    test_that("first plot is correct", {
        expect_identical(soln_plot_one$labels, plot_one$labels, info = "Do you have the correct variables on the axes and used to color code?")
    })
    
    test_that("second plot is correct", {
        expect_identical(soln_plot_two$labels, plot_two$labels, info = "Do you have the correct variables on the axes and used to color code?")
    })
})

# executing hierarchical clustering with complete linkage
hier_clust_1 = hclust(dist(scaled), method= complete)

# printing the dendrogram
plot(hier_clust_1)

# getting cluster assignments based on number of selected clusters
hc_1_assign <- cutree(hc, k = 5)

soln_hier_clust_1 = hclust(dist(soln_heart_disease), method='complete')
soln_hc_1_assign = cutree(hier_clust_1, 5)
                          
run_tests({
    test_that("correctly implemented clustering algorithm", {
        expect_identical(soln_hier_clust_1$merge, hier_clust_1$merge, info = "Did you make the distance matrix?")
        expect_identical(soln_hier_clust_1$labels, hier_clust_1$labels, info = "Did you make the distance matrix?")
        expect_identical(soln_hier_clust_1$method, hier_clust_1$method, info = "Did you use complete linkage?")   


    })
    
    test_that("correct cutoff for cluster assignments", {
        expect_identical(soln_hc_1_assign, hc_1_assign, info = "Did you select five clusters?")
    })
})

# executing hierarchical clustering with complete linkage
hier_clust_2 = hclust(....)

# printing the dendrogram
plot(....)

# getting cluster assignments based on number of selected clusters
hc_2_assign <- ....

soln_hier_clust_2 = hclust(dist(soln_heart_disease), method='single')
soln_hc_2_assign = cutree(hier_clust_2, 5)
                          
run_tests({
    test_that("correctly implemented clustering algorithm", {
        expect_identical(soln_hier_clust_2$merge, hier_clust_2$merge, info = "Did you make the distance matrix?")
        expect_identical(soln_hier_clust_2$labels, hier_clust_2$labels, info = "Did you make the distance matrix?")
        expect_identical(soln_hier_clust_2$method, hier_clust_2$method, info = "Did you use single linkage?")     })
    
    test_that("correct cutoff for cluster assignments", {
        expect_identical(soln_hc_2_assign, hc_2_assign, info = "Did you select five clusters?")
    })
})

# adding assignments of chosen hierarchical linkage
heart_disease['hc_clust'] = ....

# remove 'sex', 'first_clust', and 'second_clust' variables
hd_simple = heart_disease[, !(names(heart_disease) %in% c(....))]

# getting mean and standard deviation summary statistics
clust_summary = do.call(data.frame, aggregate(. ~...., data = ...., function(x) c(avg = ...., sd = ....)))
clust_summary

soln['hc_clust'] = soln_hc_1_assign

soln_hd_simple = soln[, !(names(soln) %in% c("sex", "first_clust", "second_clust"))]

soln_clust_summary = do.call(data.frame, aggregate(. ~hc_clust, data = soln_hd_simple, function(x) c(avg = mean(x), sd = sd(x))))


run_tests({
    test_that("selected first cluster assignments", {
        expect_identical(soln['hc_clust'], heart_disease['hc_clust'], info = "You chose the incorrect hierarchical clustering assignments.")
    })
    
    test_that("removed columns properly", {
        expect_identical(soln_hd_simple, hd_simple, info = "Did you remove three columns?")
    })
    test_that("proper summary analysis", {
        expect_identical(soln_clust_summary, clust_summary, info = "Did you find the mean and standard deviation?")
    })
})

# plotting age and chol
plot_one = ggplot(...., aes(x=...., y=...., color=as.factor(hc_clust))) + geom_point()
plot_one 

# plotting oldpeak and trestbps
plot_two = ggplot(....) + geom_point()
plot_two

soln_plot_one = ggplot(soln, aes(x=age, y=chol, color=as.factor(hc_clust))) + geom_point()
soln_plot_two = ggplot(soln, aes(x=oldpeak, y=trestbps, color=as.factor(hc_clust))) + geom_point()
run_tests({
    test_that("plot one is correct", {
        expect_identical(soln_plot_one$labels, plot_one$labels, info = "Check that you are using the correct variables for the first plot")
    })
    
    test_that("plot two is correct", {
        expect_identical(soln_plot_two$labels, plot_two$labels, info = "Check that you are using the correct variables for the second plot")
    })
})

explore_kmeans = ....
explore_hierarch_complete = ....
explore_hierarch_single = ....

soln_1 = FALSE
soln_2 = TRUE
soln_3 = FALSE

run_tests({
    test_that("correct kmeans results", {
        expect_identical(soln_1, explore_kmeans, info = "Are the clusters stable between kmeans iterations?")
    })
    
    test_that("correct hierarchical with complete linkage results", {
        expect_identical(soln_2, explore_hierarch_complete, info = "Would you want to explore this method further?")
    })
    
    test_that("correct hierarchical with single linkage results", {
        expect_identical(soln_3, explore_hierarch_single, info = "Is the number of patients in each cluster balanced?")
    })
})
