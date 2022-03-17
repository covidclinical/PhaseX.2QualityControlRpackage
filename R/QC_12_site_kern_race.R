##########################
##########  Race
##########################
err_report_race_site=function(dat.RaceBy4CECode, dat.RaceByLocalCode, site.nm){
  err.label=c(
    "in RaceBy4CECode pts_all<pts_ever_severe",
    "in RaceByLocalCode pts_all<pts_ever_severe",
    "negative patient counts in RaceBy4CECode",
    "negative patient counts in RaceByLocalCode"
    #"total number of patients in RaceBy4CECode and RaceByLocalCode does not match"
  )

  err = NULL

  dat.site.r4ce=dat.RaceBy4CECode
  colnames(dat.site.r4ce)=tolower(colnames(dat.site.r4ce))
  dat.site.rloc=dat.RaceByLocalCode
  colnames(dat.site.rloc)=tolower(colnames(dat.site.rloc))

    # check that in RaceBy4CECode pts_all>pts_ever_severe
    id.nomiss=which(dat.site.r4ce[,"pts_all"]>0 & dat.site.r4ce[,"pts_ever_severe"]>0)
    id1=which(dat.site.r4ce[id.nomiss,"pts_all"] < dat.site.r4ce[id.nomiss,"pts_ever_severe"])
    err=c(err,length(id1)>1)

    # check that in RaceByLocalCode pts_all>pts_ever_severe
    id.nomiss=which(dat.site.rloc[,"pts_all"]>0 & dat.site.rloc[,"pts_ever_severe"]>0)
    id2=which(dat.site.rloc[id.nomiss,"pts_all"] < dat.site.rloc[id.nomiss,"pts_ever_severe"])
    err=c(err,length(id2)>1)

    # check if there are negative patient counts in RaceBy4CECode
    dat.check=dat.site.r4ce[,c("pts_all", "pts_ever_severe")]
    id3=which(dat.check[dat.check<0]%in%c(-999,-99)!=1)
    err=c(err,length(id3)>1)

    # check if there are negative patient counts in RaceByLocalCode
    dat.check=dat.site.rloc[,c("pts_all", "pts_ever_severe")]
    id4=which(dat.check[dat.check<0]%in%c(-999,-99)!=1)
    err=c(err,length(id4)>1)

    # check if patient count matches between 4CE and Local file
    # r.all = unique(dat.site.r4ce[, 'race_4ce'])
    # dat.site.rloc = dat.site.rloc

  report=data.frame(site.nm, label=err.label, err)
  err.report=report[report[,"err"]==T,c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}



##########################
## QC NUMBER 20: MISSING RACE GROUP
##########################


err_report_missing_race_group=function(dat.RaceByLocalCode, dat.RaceBy4CECode, site.nm, race.list.all){

  err.label=c("Missing race category")
  all_file = list(dat.RaceByLocalCode, dat.RaceBy4CECode)
  all_filename = c("RaceByLocalCode", "RaceBy4CECode")
  i = 1


  err = NULL
  missing_race_all = NULL

  for (file in all_file){

    filename_now = all_filename[i]
    race_list_now <- unique(unlist(file[c("race_4ce")]))

    #print(filename_now)
    missing_race = setdiff(race.list.all,c(race_list_now,"other"))
    missing_race_merged = paste( unlist(missing_race), collapse=',')
    missing_race_all = c(missing_race_all, missing_race_merged)
    #print(missing_race_merged)
    combined_data = list(filename_now,missing_race_merged)
    #print(combined_data)
    i=i+1
  }
  err = c(err, length(missing_race_all))
  #report= data.frame(filename=all_filename, missing_race=missing_race_all)
  err.label = c("filename", "missing_race")
  report = data.frame(site.nm, label=err.label, err)
  err.report = report[report[,"err"]==T, c("site.nm", "label")]
  list(err.report = err.report, err.label = err.label)
}





