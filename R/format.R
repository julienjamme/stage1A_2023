#' Title
#'
#' @param res 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
format<-function(res,nom_dfs){
  if (class(res$vars[1])=="character"){
    format2(res,nom_dfs)
  }
  if (class(res$vars)=="list"){
    format3(res,nom_dfs)
  }}
  
format2<-function(res,nom_dfs){
  if (class(res$vars[1])=="character"){
  v1<-res$vars[1]
  v2<-res$vars[2]
  var_cross<-paste(v1,v2,sep="_")
  
  for (i in seq_along(res$tabs)){
    names(res$tabs)[[i]]<-paste(nom_dfs,as.character(i),sep="")}
  
  res2<-setNames(lapply(seq_along(res$tabs), function(i) {
    setNames(list(res$hrcs[[i]]), var_cross)}),
    paste(nom_dfs, seq_along(res$tabs),sep = ""))
  
  res3<-setNames(lapply(seq_along(res$tabs), function(i) {
    setNames(list(res$alt_tot[[i]]), var_cross )}),
    paste(nom_dfs, seq_along(res$tabs),sep = ""))
}
  return (list(tabs=res$tabs,hrcs=res2,vars=res$vars,alt_tot=res3))
  
}

format3<-function(res,nom_dfs){
  if (class(res$vars)=="list"){
    v1<-res$vars[[2]][1]
    v2<-res$vars[[2]][2]
    v3<-res$vars[[1]][1]
    v4<-res$vars[[1]][2]
    var_cross<-paste(v1,v2,sep="_")
    
    var_cross2<-paste(v3,v4,sep="_")
    for (i in seq_along(res$tabs)) {
      names(res$tabs)[[i]] <- paste(nom_dfs, as.character(i), sep = "")
    }
    
    res2 <- setNames(lapply(seq_along(res$tabs), function(i) {
      list1 <- setNames(list(res$hrcs4_3[[i]]), var_cross)
      list2 <- setNames(list(res$hrcs5_4[[i]]), var_cross2)
      c(list1, list2)
    }), paste(nom_dfs, seq_along(res$tabs), sep = ""))
    
    res3 <- setNames(lapply(seq_along(res$tabs), function(i) {
      list1 <- setNames(list(res$alt_tot4_3[[i]]), var_cross)
      list2 <- setNames(list(res$alt_tot5_4[[i]]), var_cross2)
      c(list1, list2)
    }), paste(nom_dfs, seq_along(res$tabs), sep = ""))
    
  }
  return (list(tabs=res$tabs,hrcs=res2,vars=res$vars,alt_tot=res3))
}

tabs_5_4_to_3<-function(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir=FALSE,hrc_dir="hrc_alt"){
  if (length(totcode)==5){
     res<-passer_de_5_a_3_var(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir , hrc_dir )
    return(format(res,nom_dfs))
  }
  if (length(totcode)==4){
    res<-passer_de_4_a_3_var(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir , hrc_dir)
    return(format(res,nom_dfs))
  }
}
