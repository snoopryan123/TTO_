
source("rstan3_sim1_main.R")

rmse_vec = numeric(25)
covg_vec = numeric(25)

for (i in 1:25) {
  # posterior samples & y vector
  fitB <- readRDS(paste0("job_output/fit_rstan3_sim1_bsn-",i,".R.rds")) 
  drawsB <- as.matrix(fitB)
  y <- readRDS(paste0("job_output/y_rstan3_sim1_bsn-",i,".R.rds")) 
  y_train = y[!test_idxs]
  y_test = y[test_idxs]
  
  # rmse
  alpha_draws = drawsB[,str_detect(colnames(drawsB), "^alpha")]
  eta_draws = drawsB[,str_detect(colnames(drawsB), "^eta")]
  sigma_draws = drawsB[,str_detect(colnames(drawsB), "^sigma")]
  post_pred_means = S%*%t(alpha_draws) + X%*%t(eta_draws)
  epsilon0 = matrix(sapply(sigma_draws, function(s) { rnorm(1,0,sd=s) }), nrow=1)
  epsilon = do.call(rbind, replicate(n, epsilon0, simplify=FALSE))
  post_pred = post_pred_means + epsilon
  
  pplower = apply(post_pred, 1, function(x) quantile(x,.025))
  ppmean = apply(post_pred, 1, function(x) mean(x))
  ppupper = apply(post_pred, 1, function(x) quantile(x,.975))
  df_bsn = bind_cols(y_test, pplower=pplower, ppmean=ppmean, ppupper=ppupper)
  test_tib_bsn = bind_rows(test_tib_bsn, df_bsn)
  
  # coverage
  
  
  # PLOTS
  draws <- as_tibble(as.matrix(fit))
  alpha_post <- as.matrix(draws[,2:28])
  eta_post <- as.matrix(draws[,(ncol(draws)-4):(ncol(draws)-1)])
  alpha_plot = plot_alpha_post(alpha_post)
  eta_plot = plot_eta_post(eta_post)
  #alpha_plot
  ###ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_alphaPlot.png"), alpha_plot)
  #eta_plot
  ###ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_etaPlot.png"), eta_plot)
  
}





