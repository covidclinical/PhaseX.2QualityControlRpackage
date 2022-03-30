#' @import data.table
#' @import dplyr
runQC_tab_lab <- function(file.nm2, phase2.ClinicalCourse, phase2.Observations, phase1.Labs, output.dir) {
  #print("Checking Phase2.2 Labs ...")
  junk=tab_compare_lab(myday=0, phase2.ClinicalCourse, phase2.Observations, phase1.Labs)
  res=junk$res
  nm.duplicated=res[duplicated(res[,"labname"]),c("labname")]
  tryCatch(sink.txt("\n\n2. Labs\n\n", file=file.nm2, cat, append=T), error=function(e) NA)

  nm.labname=NULL
  tryCatch(sink.txt(paste0("Checking differences between Phase1.2 and Phase2.2:\n\n"), file=file.nm2, cat, append=T), error=function(e) NA)
  for (nm in c("pts_all", "mean_all", "stdev_all")){
    nm1=paste0("p1.", nm)
    nm2=paste0("p2.", nm)
    nm.check=paste0("nm.diff.",nm)
    range.LB=round(res[,nm2]-pmax(res[,nm2]*0.025,5),5)
    range.UB=round(res[,nm2]+pmax(res[,nm2]*0.025,5),5)
    id.issue=which(round(res[,nm1],5)>range.UB|round(res[,nm1],5)<range.LB)
    tryCatch(sink.txt(paste0("Labs with different ", nm, " between Phase1.2 and Phase2.2:\n"), file=file.nm2, cat, append=T), error=function(e) NA)
    if(length(id.issue)!=0){
      res.print=data.frame(res[id.issue,c("labname",nm1, nm2)], range.LB[id.issue], range.UB[id.issue])
      colnames(res.print)=c("labname", "phase1", "phase2", "phase2.rangeL", "phase2.rangeU")
      rownames(res.print)=NULL

      max.print <- getOption('max.print')
      options(max.print=nrow(res.print) * ncol(res.print))
      sink(file=file.nm2, append=T)
      print(noquote(as.matrix(res.print)))
      options(max.print=max.print)
      sink()

          }else{
      sink.txt("no issue identified\n", file=file.nm2, cat, append=T)
    }
    nm.labname=c(nm.labname, res[id.issue,"labname"])
    sink.txt("\n", file=file.nm2, cat, append=T)
  }
  is.error=length(c(nm.labname,nm.duplicated))!=0
  is.error
}

#' @import data.table
#' @importFrom stats sd
#' @import dplyr
tab_compare_lab=function(myday, phase2.ClinicalCourse, phase2.Observations, phase1.Labs){
  code.dict=apply(code.dict, 2, as.character)
  code.dict.new=data.frame(code.dict)
  colnames(code.dict.new)[1]="concept_code"
  nm.lab=code.dict.new[,2]
  nm.lab=gsub(" ", "_", nm.lab)
  nm.lab=gsub("\\(", "", nm.lab)
  nm.lab=gsub("\\)", "", nm.lab)
  nm.lab=gsub("-", "_", nm.lab)
  code.dict.new[,2]=nm.lab
  dat=phase2.Observations
  dat$concept_code=as.character(dat$concept_code)
  dat=suppressMessages(left_join(dat, code.dict.new, by="concept_code"))
  nm.lab.add=unique(dat[which(is.na(dat$labname)==1&dat$concept_type=="LAB-LOINC"),"concept_code"])

  nm.lab=sort(unique(dat$labname))
  res.p2=NULL
  for(nm in nm.lab){
    tmp_all=dat[which(dat$days_since_admission==myday & dat$labname==nm),]
    loinc=unique(tmp_all[,"concept_code"])[1]
    if(dim(tmp_all)[1]!=0){
      #n_all=dim(tmp_all)[1]
      pts_all=length(unique(tmp_all$patient_num))
      tmp_all2=tmp_all[,c("patient_num", "value")]
      tmp_all2 = data.frame(data.table(tmp_all2)[, lapply(.SD, mean), by=patient_num])
      mean_all=mean(tmp_all2[,"value"],na.rm=T)
      stdev_all=sd(tmp_all2[,"value"],na.rm=T)
      mean_log_all=mean(log(tmp_all2[,"value"]+0.5), na.rm=T)
      stdev_log_all=sd(log(tmp_all2[,"value"]+0.5),na.rm=T)
      res.p2=rbind(res.p2, cbind(nm, data.frame(loinc=loinc, pts_all, mean_all, stdev_all, mean_log_all, stdev_log_all)))
    }
  }
  colnames(res.p2)[-c(1:2)]=paste0("p2.", colnames(res.p2)[-c(1:2)])
  colnames(res.p2)[1]="labname"
  colnames(res.p2)[2]="loinc"
  res.p1=NULL
  nm.lab1=sort(unique(phase1.Labs$loinc))
  for(nm in nm.lab1){
    tmp=phase1.Labs[which(phase1.Labs$days_since_admission==myday & phase1.Labs$loinc==nm), ]
    if(dim(tmp)[1]!=0){
      pts_all=tmp[, "pts_all"]
      mean_all=tmp[,"mean_value_all"]
      stdev_all=tmp[,"stdev_value_all"]
      mean_log_all=tmp[,"mean_log_value_all"]
      stdev_log_all=tmp[,"stdev_log_value_all"]

      res.p1=rbind(res.p1, cbind(nm, data.frame(pts_all, mean_all, stdev_all, mean_log_all, stdev_log_all)))
    }
  }
  nm.lab=res.p1[,1]
  nm.lab=gsub(" ", "_", nm.lab)
  nm.lab=gsub("\\(", "", nm.lab)
  nm.lab=gsub("\\)", "", nm.lab)
  res.p1[,1]=nm.lab
  colnames(res.p1)=paste0("p1.", colnames(res.p1))
  colnames(res.p1)[1]="loinc"
  res=suppressMessages(left_join(res.p1, res.p2, by="loinc"))
  res=res[,c("labname","loinc",
             "p1.pts_all","p2.pts_all",
             "p1.mean_all", "p2.mean_all",
             "p1.stdev_all", "p2.stdev_all",
             "p1.mean_log_all",  "p2.mean_log_all",
             "p1.stdev_log_all", "p2.stdev_log_all"
             # "p1.n_severe","p2.n_severe",
             # "p1.mean_severe", "p2.mean_severe",
             # "p1.stdev_severe", "p2.stdev_severe",
             # "p1.mean_log_severe",  "p2.mean_log_severe",
             # "p1.stdev_log_severe", "p2.stdev_log_severe"
  )]
  # colnames(res)=c("labname","loinc",
  #                 "p1.n_all","p2.n_all",
  #                 "p1.mean_all", "p2.mean_all",
  #                 "p1.stdev_all", "p2.stdev_all",
  #                 "p1.mean_log_all",  "p2.mean_log_all",
  #                 "p1.stdev_log_all", "p2.stdev_log_all",
  #                 "p1.n_severe","p2.n_severe",
  #                 "p1.mean_severe", "p2.mean_severe",
  #                 "p1.stdev_severe", "p2.stdev_severe",
  #                 "p1.mean_log_severe",  "p2.mean_log_severe",
  #                 "p1.stdev_log_severe", "p2.stdev_log_severe")
  list(res=res, nm.lab.add=nm.lab.add)
}
