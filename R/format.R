#' Title
#'
#' @param res 
#'
#' @return
#' @export
#'
#' @examples
format<-function(res){
  if (class(res$vars)=="character"){
  v1<-res$vars[1]
  v2<-res$vars[2]
  for (i in seq_along(res$tabs)){
    names(res$tabs)[[i]]<-paste("T",as.character(i),sep="")}
  
  res2<-setNames(lapply(seq_along(res$tabs), function(i) {
    setNames(list(res$hrcs[[i]]), paste(v1,v2,sep="_"))}),
    paste("T", seq_along(res$tabs),sep = ""))
  
  res3<-setNames(lapply(seq_along(res$tabs), function(i) {
    setNames(list(res$alt_tot[[i]]), paste(v1,v2,sep="_"))}),
    paste("T", seq_along(res$tabs),sep = ""))
}
  return (list(tabs=res$tabs,hrcs=res2,vars=res$vars,alt_tot=res3))
}

source("R/cas_gen_4_3.R")
source("R/passage_5_3.R")

tabs_5_4_to_3<-function(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir=FALSE,hrc_dir="hrc_alt"){
  if (length(totcode)==5){
    return(format(passer_de_5_a_3_var(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir = FALSE, hrc_dir = "hrc_alt")))
  }
  if (length(totcode)==4){
    return(format(passer_de_4_a_3_var(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir = FALSE, hrc_dir = "hrc_alt")))
  }
}
