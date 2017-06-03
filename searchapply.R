#Search Apply
#Author - Gowri Thampi
#v1 - 6/3/2017
#Description - Supply two datasets 1 and 2, and a search string to be found in search_column in dataset 1, 
#function 1 is then applied to all those rows of dataset 2 corresponding to search_string being found in search_column of dataset 1
SearchApply<-function(dataset1,dataset2,search_column,search_string,func){

	indices<-dataset1[,search_column]%in%search_string
	dataset2_sub<-dataset2[indices,]	
      return(apply(dataset2_sub,2,func))
}
