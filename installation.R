installRattle <- function(){
  
  directory <- choose.dir(default = "", caption = "Please elect RattleM folder");
  directory <- gsub("\\", "/", directory, fixed=TRUE)
  if(is.na(directory)){
    stop("Incorrect directory selected")
  }
  
  setRepositories(ind=1:2)
  options(devtools.install.args = "--no-multiarch") 
  devtools::install(directory, dependencies = TRUE)
}
checkRandJava <- function(){
  
  
  r32Bit <- "i386"
  r64Bit <- "x86_64"
  j32Bit <- "Java HotSpot(TM) Client VM"
  j64Bit <- "Java HotSpot(TM) 64-Bit Server VM"
  
  javaInfo <- system("java -version", intern = TRUE)
  
  javaVers <- strsplit(javaInfo[3],split=' (build', fixed=TRUE)[[1]][1]
  print(javaVers)
  rVersion <- R.version$arch
  
  if(identical(r32Bit, rVersion) && identical(j32Bit, javaVers)){
    return(TRUE);
  }
  if(identical(r64Bit, rVersion) && identical(j64Bit, javaVers)){
    return(TRUE);
  }
  stop("Installed R and Java versions must be equal! (both 32-bit or both 64-bit)")
}

checkRandJava()

if(require("devtools")){
  installRattle()
}else{
  install.packages("devtools", repos = "http://cran.us.r-project.org")
  installRattle()
}