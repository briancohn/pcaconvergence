# Loading required package: caret
# Loading required package: lattice
# Loading required package: ggplot2

require(caret)
require(lattice)
require(ggplot2)

get_nth_element_from_all_sublists <- function(big_list, n) {
	lapply(big_list, function(sublist) {sublist[n]})
}



load_files <- function(filenames, path){
	list_of_point_matrices <- mclapply(
		filename_list,
		function(i){
	  		read.csv(paste0(folder_path,i), header=FALSE)
		},
		mc.cores=8)
 }
 
 
 
 # here we are calculating the percent of explained variance for each principal component. 
#To this plot, we will add a line that indicates the amount of variance each variable 
# would contribute if all contributed the same amount.
# https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/#ref2
pca_muscle_solution_space <- function(filename, folder_path){
	dataset <- read.csv(paste0(folder_path,filename), header=FALSE)
	# log transform 
	require(caret)
	number_of_muscles = length(dataset[1,])
	dataset.pca <- prcomp(dataset, retx=TRUE, center=TRUE, scale.=TRUE)
	sd <- dataset.pca$sdev
	loadings <- dataset.pca$rotation
	rownames(loadings) <- colnames(dataset)
	scores <- dataset.pca$x
	var <- sd^2
	var.percent <- var/sum(var) * 100
	dev.new()
	barplot(var.percent, xlab="PC", ylab="Percent Variance", main=filename, names.arg=1:length(var.percent), las=1, ylim=c(0,max(var.percent)), col="gray")
	abline(h=1/ncol(dataset)*100, col="red")
	print(loadings)
  return(loadings)
}

pca_extract_vector_of_PC_explained_variance <- function(dataframe_of_7d_points) {
print('todo')
}
