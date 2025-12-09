#' km_ggsurvfit
#'
#' Returns a Kaplan-Meier curve created using ggsurvfit
#'
#' @param mytime (quoted) Survival time variable
#' @param entry (quoted) What type of risk set entry are we using? "traditional" (default) or "delayed" (implements entry risk set adjustment approach)
#' @param mytime_t0_to_entry (quoted) Variable for time from baseline to risk set entry (only use when implementing entry risk set adjustment)
#' @param mydelta (quoted) Survival status variable. Must be numeric with: `0` = censor, `1` = event of interest
#' @param group (quoted) Group variable for plotting groups
#' @param mydata (unquoted) A data frame
#' @param myxlab (quoted) x-axis label text. If `NULL`, then this will default to `labelled::var_label(mydata[[mytime]])` if `!is.null(labelled::var_label(mydata[[mytime]]))` = `TRUE`
#' @param myylab (quoted) y-axis label text
#' @param set_linetype_aes Logical for whether to use `ggplot2::scale_linetype_manual()`
#' @param legendtitle (quoted) Legend title text
#' @param mytitle (quoted) Title text
#' @param mysubtitle (quoted) Subtitle text
#' @param maxtimeset Numeric value for max value on x-axis. If `NULL`, will default to the maximum of the `mytime` arg
#' @param xaxis_max_adjust Numeric value for adding a buffer to the max value on x-axis
#' @param coord_cartesian_xmax Numeric value for clipping the x-axis at a max value
#' @param xbreaksby Numeric value for controlling x-axis breaks
#' @param print_plot Logical for whether to print the plot that gets returned. Accepts `TRUE` or `FALSE`
#' @param test Logical for whether to include a Log-Rank p-value on the plot. Accepts `0` or `1`
#' @param risk.table.set Logical for whether to add a risk table. Accepts `TRUE` or `FALSE`
#' @param conf.int.set Logical for whether to add confidence bands. Accepts `TRUE` or `FALSE`
#'
#' @returns survplot
#' @export
#' @seealso \code{\link[ggsurvfit]{ggsurvfit}}
#'
kmfun_ggsurvfit <- function(mytime,
                            entry = "traditional",
                            mytime_t0_to_entry = NULL,
                            mydelta,
                            group = NULL,
                            mydata,
                            myxlab = "Specify x axis label",
                            myylab = "Survival probability",
                            set_linetype_aes = FALSE,
                            legendtitle = NULL,
                            mytitle = NULL,
                            mysubtitle = NULL,
                            maxtimeset = NULL,
                            xaxis_max_adjust = 2,
                            coord_cartesian_xmax = NULL,
                            xbreaksby = 12,
                            print_plot = TRUE,
                            test = 0,
                            risk.table.set = TRUE,
                            conf.int.set = TRUE){

  # get max time for formatting the x-axis
  if(is.null(maxtimeset)) {
    maxtime <- round(max(mydata[[mytime]], na.rm = TRUE), 0)
  }
  if(!is.null(maxtimeset)) {
    maxtime <- maxtimeset
  }
  print(paste0("max time on x axis (without buffer)=",maxtime))
  print(paste0("max time on x axis (with buffer)=",maxtime," + ",xaxis_max_adjust))

  xmax <- maxtime+xaxis_max_adjust

  # Create string
  formula_str <- paste0(
    {if(entry == "delayed") paste0("Surv(time = ", mytime_t0_to_entry, ", time2 = ", mytime)
      else if(entry == "traditional"){ paste0("Surv(time = ", mytime)}}
    , ", event = ",
    mydelta,
    {if(is.null(group)) ") ~ 1" else paste0(") ~ ", group)}
  )
  print(paste0("formula_str = ", formula_str))


  # initialize numgrouplevels to NULL so that the object will be defined if there is no
  # grouping variable
  numgrouplevels <- NULL
  if(!is.null(group)){
    numgrouplevels <- nlevels(mydata[[group]])
    print(paste0("Number of group levels = ", numgrouplevels))

    if(!is.null(legendtitle)){
      legend_title <- legendtitle
    }
    else{
      legend_title <- labelled::var_label(mydata[[group]])
    }
  }

  km <- formula_str |>
    stats::as.formula() |>
    ggsurvfit::survfit2(data = mydata)

  # Calculate p-value if requested in the setting of delayed entry risk set adjustment
  if(test == 1 & entry == "delayed" & !is.null(group)){
    # to get logrank modified for delayed entry risk set adjustment
    x <- formula_str |>
      stats::as.formula() |>
      survival::coxph(data = mydata) |>
      summary()
    print(x)
    print(x$sctest[["pvalue"]])
    if(!is.null(x$sctest[["pvalue"]])){
      p_delayed = ggsurvfit::format_p(x$sctest[["pvalue"]])
      print(p_delayed)
    }
  }

  survplot <- km |>
    ggsurvfit::ggsurvfit(linewidth = 1.25,
              linetype_aes = set_linetype_aes) +
    {if(conf.int.set == TRUE) ggsurvfit::add_confidence_interval()} +
    {if(risk.table.set == TRUE) ggsurvfit::add_risktable(size = 4,
                                              theme = ggsurvfit::theme_risktable_default(axis.text.y.size = 12,
                                                                              plot.title.size = 12))} +
    ggsurvfit::add_censor_mark() +
    ggsurvfit::scale_ggsurvfit(x_scales = list(breaks = seq(0, xmax, xbreaksby),
                              limits = c(0, xmax))) +
    {if(!is.null(group)) ggsurvfit::add_legend_title(legend_title)} +
    {if(!is.null(group) & !is.null(numgrouplevels))
    {if(numgrouplevels > 3)
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2),
             fill = ggplot2::guide_legend(nrow = 2),
             linetype = ggplot2::guide_legend(nrow = 2))}
    } +
    # {if(!is.null(group) &
    #     !is.null(numgrouplevels) &
    #     numgrouplevels > 2)
    #   scale_linetype_manual(values=c("dotted", "twodash", "solid", "dotdash"))} +
    ggplot2::labs(title = mytitle,
         subtitle = {if(!is.null(mysubtitle)) mysubtitle
           else NULL},
         x = {if(!is.null(labelled::var_label(mydata[[mytime]]))) labelled::var_label(mydata[[mytime]])
           else myxlab},
         y = myylab) +
    {if(!is.null(coord_cartesian_xmax)) ggplot2::coord_cartesian(xlim = c(0, coord_cartesian_xmax))} +
    {if(test == 1 & !is.null(group) & entry == "traditional")
      ggsurvfit::add_pvalue(
        location = "annotation",
        caption = "Log-rank {p.value}",
        x = min(c(xmax, coord_cartesian_xmax), na.rm = TRUE)-3*xbreaksby,
        size = 5
      )} +
    {if(test == 1 & !is.null(group) & entry == "delayed")
      ggplot2::annotate(
        geom = "text",
        x = min(c(xmax, coord_cartesian_xmax), na.rm = TRUE)-3*xbreaksby,
        y = 0.5,
        label = paste0("Log-rank p=", p_delayed),
        size = 5
      )} +
    ggplot2::theme(title = ggplot2::element_text(size = 12, face = "bold"),
          axis.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 12),
          legend.title = ggplot2::element_text(size = 12),
          plot.margin = ggplot2::margin(b = 0.7,
                               r = 2,
                               unit = "cm"))

  if(print_plot==TRUE){
    print(survplot)
  }
  return(survplot)
}
