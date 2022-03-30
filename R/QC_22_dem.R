#' @import data.table
#' @import dplyr
runQC_tab_dem <- function(file.nm2, phase2.Summary, phase2.Observations, phase1.AgeSex, output.dir) {
  #print("Checking Phase2.2 Demographics ...")
  res=tab_compare_dem(phase2.Summary, phase2.Observations, phase1.AgeSex)
  nm.duplicated=res[duplicated(res[,c("sex", "age_group")]),c(c("sex", "age_group"))]
  tryCatch(sink.txt("5. Demographics\n\n", file=file.nm2, cat, append=T), error=function(e) NA)
  tryCatch(sink.txt("Checking duplicated rows:\n", file=file.nm2, cat, append=T), error=function(e) NA)
  if(dim(nm.duplicated)[1]!=0){
    nm.duplicated=unlist(lapply(1:dim(nm.duplicated)[1], function(ll) paste(paste0(colnames(nm.duplicated),"=",nm.duplicated[ll,]),collapse=":")))
    #print(paste0("Duplicated rows for:", paste(nm.duplicated, collapse=';')))
    tryCatch(sink.txt(paste0(paste(nm.duplicated,collapse=";"), "\n"), file=file.nm2, cat, append=T), error=function(e) NA)}else{
      sink.txt("no issue identified", file=file.nm2, cat, append=T)
    }
  sink.txt("\n\n", file=file.nm2, cat, append=T)
  sex.list=unique(tolower(phase2.Summary$sex))
  age.list=unique(tolower(phase2.Summary$age_group))
  issue.sex=sex.list[sex.list%in%c("other", "female", "male")!=1]
  issue.age_group=age.list[age.list%in%c("other", "00to02", "03to05", "06to11", "12to17", "18to20", "21to25", "26to49", "50to69", "70to79", "80plus")!=1]
  tryCatch(sink.txt("Checking for wrong demographic groups:\n", file=file.nm2, cat, append=T), error=function(e) NA)
  if((length(issue.sex)+length(issue.age_group))!=0){
    if(length(issue.sex)!=0){sink.txt(paste(paste0("wrong sex group: '", issue.sex, "'"), collapse="; "), file=file.nm2, cat, append=T);
      sink.txt("\n", file=file.nm2, cat, append=T)
    }else{sink.txt("no issue identified\n", file=file.nm2, cat, append=T)}

    if(length(issue.age_group)!=0){sink.txt(paste(paste0("wrong age group: '", issue.age_group, "'"), collapse="; "), file=file.nm2, cat, append=T)
      sink.txt("\n", file=file.nm2, cat, append=T)
    }else{sink.txt("no issue identified\n", file=file.nm2, cat, append=T)}

  }else{sink.txt("no issue identified\n", file=file.nm2, cat, append=T)}
  sink.txt("\n", file=file.nm2, cat, append=T)
}


tab_compare_dem=function(phase2.Summary, phase2.Observations, phase1.AgeSex){
  dat.dem.raw=phase2.Summary
  dat.dem=phase1.AgeSex
  dat.dem.raw[,c("sex", "age_group")]=apply(phase2.Summary[,c("sex", "age_group")],2, as.character)
  dat.dem[,c("sex", "age_group")]=apply(phase1.AgeSex[,c("sex", "age_group")],2, as.character)

  tmp.all=dat.dem.raw[,c("patient_num", "sex", "age_group")]
  tmp.severe=dat.dem.raw[which(dat.dem.raw$severe==1),c("patient_num", "sex", "age_group")]

  mysettings=dat.dem[,c(3:5)]
  res.p2=NULL
  for(ii in 1:dim(mysettings)[1]){
    sex.i=mysettings[ii,"sex"]
    age_group.i=mysettings[ii,"age_group"]
    if(sex.i=="all"){sex.i=unique(dat.dem.raw[,"sex"])}
    if(age_group.i=="all"){age_group.i=unique(dat.dem.raw[,"age_group"])}


    n_all=length(unique(tmp.all[which(tmp.all[,"sex"]%in%sex.i &
                                        tmp.all[,"age_group"]%in%age_group.i),"patient_num"]))

    n_severe=length(unique(tmp.all[which(tmp.severe[,"sex"]%in%sex.i &
                                           tmp.severe[,"age_group"]%in%age_group.i ),"patient_num"]))
    res.p2=rbind(res.p2,c(n_all, n_severe))
  }
  res=cbind(dat.dem[,-c(1,2)], res.p2)
  colnames(res)[-(1:3)]=c("p1.n_all","p1.n_severe",  "p2.n_all", "p2.n_severe")
  res=res[,c("sex","age_group", "Phase1.2 pts_all", "Phase2.2 pts_all", "Phase1.2 pts_ever_severe", "Phase2.2 pts_ever_severe")]
  res
}
