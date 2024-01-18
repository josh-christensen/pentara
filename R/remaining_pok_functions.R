#to edit default R template use this: usethis::edit_file("~/AppData/Roaming/RStudio/templates/default.R")
#To create new R template file use this snippet:
# # Create a templates folder
# fs::dir_create(path = "~/AppData/Roaming/RStudio/templates")
#
# # Create the file
# fs::file_create("~/AppData/Roaming/RStudio/templates/default.R")
#
# # Open the file in RStudio to edit it
# usethis::edit_file("~/AppData/Roaming/RStudio/templates/default.R")

require(ggplot2)
# this makes using R's native pipe with data.table easier
.DT=`[`

#this is also useful with R's native pipe
.S=`$`

#This creates CDF plots

cdf_function=function(data, inverted=invert[i], by_vector=c("AVISIT","ARM"), annotate=T, cutoff=0, group_var="ARM", facet_vars=NULL, param="", include_cutoff=T, legend_title="ARM", annotate_offset=0, digits=2, scales="fixed", nrow=1){
  temp=copy(data)
  if(inverted){
    temp[,CHG:=-1*CHG]
  }
  setorder(temp, CHG)

  if(is.data.table(cutoff)){
    temp=merge(temp, cutoff)
  }

  temp=temp[!is.na(CHG)]|>
    .DT(,N:=.N, by=by_vector)|>
    .DT(,index:=rank(CHG, ties.method="max"), by=by_vector)|>
    .DT(,CDF:=index/N, by=by_vector)
  zeros=temp[,min(CHG), by=by_vector]|>.DT(,CDF:=0)|>na.omit()|>setnames("V1", "CHG")
  temp=rbindlist(list(zeros,temp), fill=T)
  if(include_cutoff){
    if(inverted){
      temp[,CHG:=-1*CHG]
      responders=temp[CHG>=cutoff, max(CDF), by=by_vector]
    }else{
      responders=temp[CHG<=cutoff, max(CDF), by=by_vector]
    }
  }else{
    if(inverted){
      temp[,CHG:=-1*CHG]
      responders=temp[CHG>cutoff, max(CDF), by=by_vector]
    }else{
      responders=temp[CHG<cutoff, max(CDF), by=by_vector]
    }
  }

  responders[,`:=`(
    y=V1,
    V1= round(V1*100, digits)
  )]
  if(is.data.table(cutoff)){
    responders=merge(responders, cutoff)
    responders[,x:=cutoff+annotate_offset]
  }else{
    responders[,x:=cutoff+annotate_offset]
  }

  if(inverted){
    if(!include_cutoff){
      sign_equal=">"
    }else{
      sign_equal="\u2265"
    }
  }else{
    if(!include_cutoff){
      sign_equal="<"
    }else{
      sign_equal="\u2264"
    }
  }
  plot=ggplot(temp, aes(x=CHG, y=CDF, group=eval(parse(text=group_var)), color=eval(parse(text=group_var))))+geom_point()+geom_step()+theme_bw()+ggtitle(paste0(param,"\nN ", sign_equal," cut-off"))+ylab("cumulative probability")+xlab("Change")+guides(color=guide_legend(title = legend_title))
  if(annotate){
    if(is.data.table(cutoff)){
      plot=plot+geom_vline(data=cutoff, aes(xintercept = cutoff), linetype="dashed")+geom_text(data=responders, aes(x=x, y=y, label=paste0(V1,"%")), color="black")
    }else{
      plot=plot+geom_vline(xintercept = cutoff, linetype="dashed")+geom_text(data=responders, aes(x=x, y=y, label=paste0(V1,"%")), color="black")
    }
  }
  if(inverted){
    plot=plot+scale_x_reverse()
  }
  if(!is.null(facet_vars)){
    plot=plot+facet_wrap(vars(eval(parse(text=facet_vars))), scales=scales, nrow=nrow)
  }
  return(plot)
}

#helper functions
lenunique=function(x){
  length(unique(x))
}

len.na=function(x){
  length(na.omit(x))
}

#combinatorics enumerator. Creates a list of all possible selections of unique variable elements and all combinations that are sequential

enumerator=function(stuff){
  stuff=sort(unique(na.omit(stuff)))
  combs=list()
  k=1
  for(i in stuff){
    for(j in 0:sum(stuff>i)){
      index=match(i, stuff)
      combs[[k]]=stuff[index:(index+j)]
      k=k+1
    }
  }
  return(combs)
}
