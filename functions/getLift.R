getLift <- function (score, response, groups = 100){
  tasa_promedio = mean(response)
  dataset = data.frame(score, response)
  lift = dataset %>% arrange(-score) %>% mutate(percentil = cut(rank(score, 
                                                                     ties.method = "first"), quantile(rank(score, ties.method = "first"), 
                                                                                                      seq(from = 0, to = 1, length.out = groups + 1)), include.lowest = T, 
                                                                labels = seq(from = 100, to = 1, length.out = groups))) %>% 
    group_by(percentil) %>% summarise(min_score = min(score), 
                                      max_score = max(score), mean_score = mean(score), response_rate = mean(response), 
                                      detected = sum(response), feqcuenty = n()) %>% ungroup() %>% 
    mutate(lift = response_rate/tasa_promedio, percentil = as.numeric(as.character(percentil))) %>% 
    arrange(percentil) %>% mutate(cum_response = cumsum(detected)/cumsum(feqcuenty), 
                                  cum_lift = cum_response/tasa_promedio)
  return(lift)
}