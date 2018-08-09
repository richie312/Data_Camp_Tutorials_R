
## Install the package
  install.packages("//ULVMCTMFIL109/DESKT`OP109B/AChatterjee/Desktop/Aritra_pckgs/caTools_1.17.1.1.zip",
                   repos=NULL,type="source",dependencies = TRUE)
## get the list of packages from the source folder
  
pckgs<-list.files("//ULVMCTMFIL109/DESKTOP109B/AChatterjee/Desktop/Aritra_pckgs")

## path for the source file

path<-"//ULVMCTMFIL109/DESKTOP109B/AChatterjee/Desktop/Aritra_pckgs/"


## loop in through the folder and install each pckgs

for (i in pckgs){
install.packages(paste(path,pckgs,sep=""),repos=NULL,dependencies=TRUE,type="source") 
  
}
  

