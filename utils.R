library(tidyverse)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Functions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


constructer = function(total, n_exp, r1, r2){
  # Constructor function that takes the basic parameters of a study design, return
  # a study object that has corresponding attributes.
  # Need consider EDGE CASES LATER!!!!!!!!!!
  
  this_study = list(
    total_n = "numeric",
    n_exp = "numeric",
    n_nonexp = "numeric",
    true_risks = "vector"
  )
  setClass("study", slots=this_study)
  this_study = new('study',
                   total_n = total,
                   n_exp = n_exp,
                   n_nonexp = total - n_exp,
                   true_risks = c(r1, r2)
                   )
  return(this_study)
}


true_table = function(study_obj){
  # generate a hypothetical table
  n1 = study_obj@n_exp * study_obj@true_risks[1]
  n2 = study_obj@n_nonexp * study_obj@true_risks[2]
  n3 = study_obj@n_exp - n1
  n4 = study_obj@n_nonexp - n2
  
  df = data.frame(
    matrix(c(n1, n2, n3, n4), 2, 2),
    row.names = c('exposure', 'non-exposure')
  )
  colnames(df) = c('outcome +', 'outcome -')
  
  return(df)
}
  
  
  
true_vector = function(true_risks){
  # This function takes the basic parameters of a study, returns the true RR, RD
  # OR, OD values in a vector.
  # 
  # study_obj:
  # true_risks: vector of true risks in the order c(risk_exposed, risk_nonexp).
  # view: Boolean default to FALSE, use TRUE to display the actual simulation.
  
  
  RR = true_risks[1] / true_risks[2]
  RD = true_risks[1] - true_risks[2]
  OR = true_risks[1]*(1-true_risks[2]) / (true_risks[2]*(1 - true_risks[1]))
  OD = true_risks[1]/(1-true_risks[1]) - true_risks[2]/(1-true_risks[2])
  return(round(c(RR, RD, OR, OD), 3))
}


bias_table = function(study_obj, bias) {
  
  vector = calculate_biased_matrix(
    study_obj@n_exp,
    study_obj@n_nonexp,
    study_obj@true_risks,
    bias[1],
    bias[2]
  )
  df = data.frame(
    matrix(vector, 2, 2),
    row.names = c('exposure', 'non-exposure')
  )
  colnames(df) = c('outcome +', 'outcome -')
  
  return(df)

}



bias_vector = function(study_obj, bias){
  
   vector = calculate_biased_matrix(
    study_obj@n_exp,
    study_obj@n_nonexp,
    study_obj@true_risks,
    bias[1],
    bias[2]
  )
  
  matrix = matrix(vector, 2, 2)
  
  RR = (vector[1]/study_obj@n_exp) / (vector[2]/study_obj@n_nonexp)
  RD = (vector[1]/study_obj@n_exp) - (vector[2]/study_obj@n_nonexp)
  OR = (vector[1]/vector[3]) / (vector[2]/vector[4])
  OD = (vector[1]/vector[3]) - (vector[2]/vector[4])
  
  return(round(c(RR,RD,OR,OD), 3))
  
}


calculate_biased_matrix = function(n_exp, n_nonexp, true_risks, a, b) {
  # Takes study parameters and bias numbers, return the simulation matrix
  r1 = true_risks[1]; r2 = true_risks[2]
  
  n1 = sum(sample(c(1,0), size = n_exp-a, replace = T, prob = c(r1, 1-r1))) +
    sum(sample(c(1,0), size = b, replace = T, prob = c(r2, 1-r2)))
  n2 = sum(sample(c(1,0), size = a, replace = T, prob = c(r1, 1-r1))) + 
    sum(sample(c(1,0), size = n_nonexp - b, replace = T, prob = c(r2, 1-r2)))
  n3 = n_exp - n1
  n4 = n_nonexp - n2
  
  out = c(n1, n2, n3, n4)
  return(out)
}

