plotLift <- function (lifts = list(), ..., cuts = c(5, 15), value = "cum_lift", 
                      fancy = F) {
  safeLibrary(purrr)
  safeLibrary(ggplot2)
  safeLibrary(gridExtra)
  if (class(lifts) != "list") {
    lifts = list(model = lifts)
  }
  extra_lifts = list(...)
  for (model in names(extra_lifts)) {
    lifts[[model]] = extra_lifts[[model]]
  }
  dataset = map2_dfr(names(lifts), lifts, function(model, values) {
    values$model = model
    values[, "value"] = values[, value]
    return(values)
  })
  hline = switch(value, cum_lift = 1, lift = 1, response_rate = sum(dataset[nrow(dataset), 
                                                                            "cum_response"]), cum_response = sum(dataset[nrow(dataset), 
                                                                                                                         "cum_response"]))
  if (!fancy) {
    gr = ggplot(dataset, aes(percentil, value, color = model)) + 
      geom_line(size = 1.5) + geom_abline(slope = 0, intercept = hline, 
                                          size = 1.5) + ylab(value)
    if (length(cuts) > 0) {
      gr = gr + geom_vline(xintercept = cuts)
      tabla = filter(dataset, percentil %in% cuts)
      gr = gr + geom_label(aes(label = round(value, 2), 
                               fill = model), tabla, color = "black")
    }
  }
  else {
    gr = ggplot(dataset, aes(percentil, value, color = model)) + 
      geom_line(size = 1.5) + ggthemes::scale_color_tableau() + 
      geom_abline(slope = 0, intercept = hline, size = 1) + 
      ylab(value) + labs(color = "modelo") + theme_minimal()
    if (length(cuts) > 0) {
      gr = gr + geom_vline(xintercept = cuts, linetype = 2)
      tabla = filter(dataset, percentil %in% cuts)
      gr = gr + geom_label(aes(label = round(value, 2), 
                               fill = model), tabla, show.legend = F, color = "black") + 
        ggthemes::scale_fill_tableau()
    }
  }
  return(gr)
}