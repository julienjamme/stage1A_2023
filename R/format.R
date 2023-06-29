format<-function(res){
  if (class(res$vars)=="character"){
  v1<-res$vars[1]
  v2<-res$vars[2]
  for (i in seq_along(res$tabs)){
    names(res$tabs)[[i]]<-paste("T",as.character(i),sep="")}
  
  setNames(lapply(seq_along(res$tabs), function(i) {
    setNames(list(res$hrcs[[i]]), paste(v1,v2,sep="_"))}),
    paste("T", seq_along(res$tabs),sep = ""))
  }
}


