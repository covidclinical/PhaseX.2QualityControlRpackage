#' @import data.table
#' @import dplyr
runQC_tab_lab <- function(file.nm2, phase2.ClinicalCourse, phase2.Observations, phase1.Labs, output.dir) {
  #print("Checking Phase2.2 Labs ...")
  junk=tab_compare_lab(myday=0, phase2.ClinicalCourse, phase2.Observations, phase1.Labs)
  res=junk$res
  nm.duplicated=res[duplicated(res[,"labname"]),c("labname")]
  tryCatch(sink.txt("\n\n2. Labs\n\n", file=file.nm2, cat, append=T), error=function(e) NA)
  tryCatch(sink.txt("Checking duplicated rows:\n", file=file.nm2, cat, append=T), error=function(e) NA)
  if(length(nm.duplicated)!=0){
    #print(paste0("Checking duplicated rows:", paste(nm.duplicated,collapse=";")))
    tryCatch(sink.txt(paste0(paste(nm.duplicated,collapse=";"), "\n"), file=file.nm2, cat, append=T), error=function(e) NA)}else{
      sink.txt("no issue identified", file=file.nm2, cat, append=T)
    }
  sink.txt("\n\n", file=file.nm2, cat, append=T)

  nm.labname=NULL
  tryCatch(sink.txt(paste0("Checking differences between Phase1.2 and Phase2.2:\n\n"), file=file.nm2, cat, append=T), error=function(e) NA)
  for (nm in c("n_all", "mean_all", "stdev_all",
               "n_severe", "mean_severe", "stdev_severe")){
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
      tryCatch(sink.txt(paste(apply(res.print,1, function(ll) paste0(paste(paste0(colnames(res.print), "=", ll),collapse="; "),
                                                                     "\n")),collapse=""), file=file.nm2, cat, append=T), error=function(e) NA)
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
  #data(code.dict, package="Phase2.2QualityControlRpackage")
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

  patient_severe=phase2.ClinicalCourse[which(phase2.ClinicalCourse$severe==1),"patient_num"]
  nm.lab=sort(unique(dat$labname))
  res.p2=NULL
  for(nm in nm.lab){
    tmp_all=dat[which(dat$days_since_admission==myday & dat$labname==nm),]
    tmp_severe=dat[which(dat$days_since_admission==myday & dat$labname==nm & dat$patient_num%in%patient_severe),]
    loinc=unique(tmp_all[,"concept_code"])[1]
    if(dim(tmp_all)[1]!=0){
      #n_all=dim(tmp_all)[1]
      n_all=length(unique(tmp_all$patient_num))
      tmp_all2=tmp_all[,c("patient_num", "value")]
      tmp_all2 = data.frame(data.table(tmp_all2)[, lapply(.SD, mean), by=patient_num])
      mean_all=mean(tmp_all2[,"value"],na.rm=T)
      stdev_all=sd(tmp_all2[,"value"],na.rm=T)
      mean_log_all=mean(log(tmp_all2[,"value"]+0.5), na.rm=T)
      stdev_log_all=sd(log(tmp_all2[,"value"]+0.5),na.rm=T)

      n_severe=length(unique(tmp_severe$patient_num))
      tmp_severe2=tmp_severe[,c("patient_num", "value")]
      tmp_severe2=data.frame(data.table(tmp_severe2)[,lapply(.SD, mean), by=patient_num])
      mean_severe=mean(tmp_severe2[,"value"],na.rm=T)
      stdev_severe=sd(tmp_severe2[,"value"],na.rm=T)
      mean_log_severe=mean(log(tmp_severe2[,"value"]+0.5), na.rm=T)
      stdev_log_severe=sd(log(tmp_severe2[,"value"]+0.5),na.rm=T)

      res.p2=rbind(res.p2, cbind(nm, data.frame(loinc=loinc, n_all, mean_all, stdev_all, mean_log_all, stdev_log_all,
                                                n_severe, mean_severe, stdev_severe, mean_log_severe, stdev_log_severe)))
    }
  }
  colnames(res.p2)[-c(1:2)]=paste0("p2.", colnames(res.p2)[-c(1:2)])
  colnames(res.p2)[1]="labname"
  colnames(res.p2)[2]="loinc"
  res.p1=NULL
  nm.lab2=sort(unique(phase1.Labs$loinc))
  for(nm in nm.lab2){
    tmp=phase1.Labs[which(phase1.Labs$days_since_admission==myday & phase1.Labs$loinc==nm), ]
    if(dim(tmp)[1]!=0){
      n_all=tmp[, "pts_all"]
      mean_all=tmp[,"mean_value_all"]
      stdev_all=tmp[,"stdev_value_all"]
      mean_log_all=tmp[,"mean_log_value_all"]
      stdev_log_all=tmp[,"stdev_log_value_all"]
      n_severe=tmp[, "pts_ever_severe"]
      mean_severe=tmp[,"mean_value_ever_severe"]
      stdev_severe=tmp[,"stdev_value_ever_severe"]
      mean_log_severe=tmp[,"mean_log_value_ever_severe"]
      stdev_log_severe=tmp[,"stdev_log_value_ever_severe"]

      res.p1=rbind(res.p1, cbind(nm, data.frame(n_all,mean_all, stdev_all, mean_log_all, stdev_log_all,
                                                n_severe,mean_severe, stdev_severe, mean_log_severe, stdev_log_severe)))
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
             "p1.n_all","p2.n_all",
             "p1.mean_all", "p2.mean_all",
             "p1.stdev_all", "p2.stdev_all",
             "p1.mean_log_all",  "p2.mean_log_all",
             "p1.stdev_log_all", "p2.stdev_log_all",
             "p1.n_severe","p2.n_severe",
             "p1.mean_severe", "p2.mean_severe",
             "p1.stdev_severe", "p2.stdev_severe",
             "p1.mean_log_severe",  "p2.mean_log_severe",
             "p1.stdev_log_severe", "p2.stdev_log_severe"
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
