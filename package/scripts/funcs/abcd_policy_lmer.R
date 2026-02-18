

# models will follow this format: lmerTest::lmer(dv ~ iv + covariates + random_effects, data)

abcd_policy_lmer = function(dv, iv=NULL, covariates=NULL, random_effects, data, increase.tol = F) {
  
  if (length(covariates) > 0) {
    covariate_structure = paste0(covariates, collapse = "+")
  } else {
    covariates_structure = ""  
  }
  
  re_structure = paste0("+", random_effects, sep = "")
  if (is.null(iv) | iv == "") {
    iv_structure = ""
  } else {
    iv_structure = paste0(iv, "+", sep = "")  
  }
  dv_structure = paste0(dv, "~", sep = "")
  model_structure =
    paste0(
      dv_structure,
      iv_structure,
      covariate_structure,
      re_structure,
      sep = ""
    )
  
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun = 2e5))
  # to keep compatible with gtLmerInterpret and pullOutcome we'll run this a bit clunky by passing the full character version of the model call
  mod_init = paste0("lmerTest::lmer(")
  mod_close = paste0(", data = data)")
  
  if (increase.tol == T) {
    tol = ", control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun = 2e5)),"
    model_paste = paste0(mod_init, model_structure, tol, mod_close)
  } else {
    model_paste = paste0(mod_init, model_structure, mod_close)
  }
  
  model <- eval(parse(text = model_paste))
  
  return_list = list("model" = model, "formula" = model_structure)
  return(return_list)
}