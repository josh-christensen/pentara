

subject_gst <- function(data_frame, treat = "TRTP", base_visit = "Baseline", gst_components, direction)

#The GST function, it expects a CDISC/ADaMs style dataset
GST=function(data, trt="TRT01P", baseline="Baseline", gst_paramcd_list, direction=NULL, gst_names, sdcols=c("USUBJID", "CHG", "BASE", "AVISIT", "ANL01FL"), na.rm=T){
  dat=as.data.table(copy(data))
  if(!is.null(direction)){
    for(i in seq_along(direction)){
      name=names(direction)[i]
      dir=direction[i]
      dat[PARAMCD==name, `:=`(
        AVAL=AVAL*dir,
        BASE=BASE*dir,
        CHG=CHG*dir
      )]
    }
  }
  if(!trt%in%sdcols){
    sdcols=c(trt, sdcols)
  }
  if("sd"%in%sdcols|"base_sd"%in%sdcols){
    warning("Including 'sd' or 'base_sd' as a variable to retain will result in duplicated CHG values for all GSTs", immediate. = T)
  }
  sdcols=c(sdcols, "gst_vars")
  dat[AVISIT!=baseline, sd:=tryCatch(lm(CHG~eval(parse(text=trt)))|>sigma(), error=function(e) warning("Some sigma's for change scores were not estimated, proceed with caution!", immediate.=T)), by=.(AVISIT, PARAMCD)]
  dat[, base_sd:=tryCatch(sd(BASE[AVISIT==baseline], na.rm=T), error=function(e) warning("Some sigma's for baseline were not estimated, proceed with caution!")), by=PARAMCD]
  dat[,CHG:=CHG/sd]
  dat[,BASE:=BASE/base_sd]
  for(i in seq_along(gst_paramcd_list)){
    if(na.rm==F){
      dat[,use:=all(gst_paramcd_list[[i]]%in%PARAMCD), by=.(AVISIT, USUBJID)]
    }else{
      dat[,use:=T]
    }
    temp=copy(dat[PARAMCD%in%gst_paramcd_list[[i]]&use==T])|>
      _[, `:=`(
      CHG=mean(CHG)
    ), by=.(AVISIT, USUBJID)]
    temp[, BASE:=mean(BASE[AVISIT==baseline]), by=USUBJID]
    temp[,gst_vars:=paste(gst_paramcd_list[[i]], collapse=", ")]
    temp=temp[,.SD, .SDcols = sdcols]
    temp[,PARAMCD:=gst_names[i]]
    temp=unique(temp)
    data=rbind(data,temp, fill=T)
  }
  return(data)
}
