length<-runif(15,min=1,max=15)
width<-runif(15,min=1,max=15)
my.df<-data.frame("length"=length,"width"=width)

devtools::use_data(my.df,overwrite = TRUE)
