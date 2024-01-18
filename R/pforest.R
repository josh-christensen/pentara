#Forest plots

forest_line=function(data, xlim){
  dat=copy(data)
  dat=as.data.table(dat)
  dat[,arr_low:=fifelse(lower.CL<xlim[1],1,NaN)]
  dat[,arr_high:=fifelse(upper.CL>xlim[2],1,NaN)]
  plot=ggplot(data=dat, aes(y=0, x=estimate, xmin=lower.CL, xmax=upper.CL))+
    geom_vline(xintercept=0, linetype=2)+
    geom_errorbar(width=.25)+
    geom_point()+
    geom_segment(aes(x=estimate, xend=arr_low*xlim[1], y=0, yend=0), arrow=arrow(angle=30, length=unit(.35, "npc")))+
    geom_segment(aes(x=estimate, xend=arr_high*xlim[2], y=0, yend=0), arrow=arrow(angle=30, length=unit(.35, "npc")))+
    theme_void()+
    coord_cartesian(xlim=xlim, ylim=c(-.5,.5), expand=0)
  return(plot)
}

forest_plot=function(data, xlim=c(-2,2), keep, digits=3, fig_height=.35, fig_width=2){
  temp=copy(data)
  temp=as.data.table(temp)

  axis_plot=ggplot(data=NULL, aes(x=0, y=0))+
    theme(panel.background = element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.line.x=element_line())+
    xlab(NULL)+
    scale_x_continuous(limits=xlim, expand=c(0,0))

  temp[,` `:=list(list(forest_line(data.table(estimate, lower.CL, upper.CL), xlim))), by=1:nrow(temp)]
  plots=list(temp$` `)

  tab=flextable(temp[,c(keep, " "), with=F])|>
    colformat_double(digits=digits)|>
    mk_par(j=" ", i=1:(nrow(temp)),
           value=as_paragraph(gg_chunk(value=., height=fig_height, width=fig_width)),
           use_dot = T)|>
    add_footer( ` ` = "")|>
    mk_par(part = "header", j = " ",
           value = as_paragraph(gg_chunk(value = list(axis_plot), height = .35, width = fig_width))) |>
    mk_par(part = "footer", i = 1, j = " ",
           value = as_paragraph(gg_chunk(value = list(axis_plot), height = .35, width = fig_width)))|>
    align(part="all", j=" ", align="center")|>
    align(part="header", align="center")|>
    padding(padding.top=0,
            padding.bottom=0)|>
    fontsize(size=12)|>
    font(fontname="Times New Roman")
  return(tab)
}
