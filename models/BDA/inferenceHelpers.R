# adapted from Brandon https://github.com/bwaldon/crossling_reference/blob/master/_shared/inferenceHelpers.r

# create the basic RSA model (speaker and listener functions)
makeModel <- function(header) {
  
  # return(paste(read_file(header), read_file("engine.txt"), sep = "\n"))
  return(read_file(header))
  
}

# load different models (different posteriors)
wrapInference <- function(model, modelName, samples, lag, burn) {
  
  if(modelName == "threshold_chemo") {
    inferenceCommand <- read_file("./inferenceCommands/threshold_chemo.txt")
  } else if(modelName == "threshold_qud") {
    inferenceCommand <- read_file("./inferenceCommands/threshold_qud.txt")
  }  else {
    print("Wrong modelName")
  }

  inferenceCommand <- gsub("NUM_SAMPLES", samples, inferenceCommand, fixed = TRUE)
  inferenceCommand <- gsub("LAG", lag, inferenceCommand, fixed = TRUE)
  inferenceCommand <- gsub("BURN_IN", burn, inferenceCommand, fixed = TRUE)

  return(paste(read_file(model), inferenceCommand, sep = "\n"))
  
}

estimate_mode <- function(s) {
  d <- density(s)
  return(d$x[which.max(d$y)])
}

getEstimates <- function(posteriors) {
  
  estimates <- posteriors %>%
    group_by(Parameter) %>%
    summarize(estimate = estimate_mode(value)) %>% 
    mutate(estimate = ifelse(estimate < 0, 0, estimate)) %>%
    pivot_wider(names_from = Parameter, values_from = estimate)
  
  return(estimates)
  
}

wrapPrediction = function(model, estimates, overmodifyingUtterance, targetReferent, inferenceType) {
  
  if(inferenceType == "incrementalContinuous" | inferenceType == "incremental" ) {
    
    predictionCommand <- read_file("../_shared/inferenceCommands/main/getIncrementalPredictions.txt")
    
  } else if (inferenceType == "continuous" | inferenceType == "vanilla" ) {
    
    predictionCommand <- read_file("../_shared/inferenceCommands/main/getGlobalPredictions.txt")
    
  }
  
  predictionCommand <- paste((sprintf("var estimates = %s[0]", toJSON(estimates, digits = NA))), predictionCommand, sep = "\n")
  predictionCommand <- gsub("OVERMODIFYING_UTTERANCE", overmodifyingUtterance, predictionCommand, fixed = TRUE)
  predictionCommand <- gsub("TARGET_REFERENT", targetReferent, predictionCommand, fixed = TRUE)
  
  return(paste(read_file(model), predictionCommand, sep = "\n"))
  
}