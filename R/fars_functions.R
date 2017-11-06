#function 1
perc_cis <- function(x, n) {
  p <- x / n
  standard_error <- sqrt((p * (1 - p)) / n)
  upper_ci <- (p + (1.96 * standard_error))
  lower_ci <- (p - (1.96 * standard_error))
  p_perc <- round((p * 100), digits = 1)
  upper_ci_perc <- round((upper_ci * 100), digits = 1)
  lower_ci_perc <- round((lower_ci * 100), digits = 1)
  final_results <- paste0(p_perc, "% (", lower_ci_perc, 
                          "%, ", upper_ci_perc, "%)")
  final_results
}

#function 2
test_trend_ca <- function(drug, data = clean_fars) {
  if(drug == "Nonalcohol"){
    to_test <- clean_fars %>%
      filter(drug_type != "Alcohol") %>%
      group_by(unique_id, year) %>% 
      summarize(positive_test = sum(positive_for_drug, na.rm = TRUE), 
                positive = any(positive_test > 0),
                total_tests = length(!is.na(positive_for_drug))) %>% 
      ungroup() %>% 
      group_by(year) %>%
      summarize(total_tests = sum(total_tests), positive = sum(positive)) 
  } else{
    to_test <- clean_fars %>%
      filter(drug_type == drug) %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                total_tests = sum(!is.na(positive_for_drug)))
  }
  ca_alcohol <- prop.trend.test(x = to_test$positive,
                                n = to_test$total_tests)
  Z <- round(sqrt(ca_alcohol$statistic), digits = 1)
  p.value <- round(ca_alcohol$p.value, digits = 3)
  final_results <- data.frame(Z, p.value)
  tibble::remove_rownames(final_results)
  return(final_results)
}

#function 3
test_trend_log_reg <- function(drug, data = clean_fars) {
  if(drug == "Nonalcohol"){
    to_test <- clean_fars %>%
      filter(!is.na(drug_type)) %>% 
      filter(drug_type != "Alcohol") %>% 
      group_by(unique_id, year) %>% 
      summarize(positive_for_drug = any(positive_for_drug))
    
  } else{
    to_test <- clean_fars %>%
      filter(!is.na(drug_type)) %>%
      filter(drug_type == drug)
  }
  log_reg <- glm(positive_for_drug ~ year, data = to_test,
                 family = binomial(link = "logit"))
  log_reg_sum <- slice(broom::tidy(log_reg), 2)
  Z <- round(log_reg_sum$statistic, digits = 1)
  p.value <- round(log_reg_sum$p.value, digits = 3)
  final_results <- data.frame(Z, p.value)
  tibble::remove_rownames(final_results) 
  return(final_results)
}







