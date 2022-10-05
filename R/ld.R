#background functions from the vwr r package

levenshtein.neighbors <-
  function(xsource, targets){
    results<-list()
    distances<-levenshtein.distance(xsource, targets)
    for (distance in min(distances):max(distances)){
      results[distance]=list(names(which(distances==distance)))
    }
    return(results)
  }

levenshtein.distance <-
  function(xsource, targets){
    distances<-stringdist::stringdist(xsource, targets, method='lv') # to help the package find the stringdist function
    names(distances)<-targets
    return(distances)
  }
