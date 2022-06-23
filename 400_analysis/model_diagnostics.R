get_model_resid_df <- function(lm_model){
  data.frame(id = 1:length(resid(lm_model)),
             residuals = resid(lm_model),
             standard = rstandard(lm_model),
             studend = rstudent(lm_model))
}

diagnostic_lm_plot <- function(df_model_resid){
  p1 <- ggplot(df_model_resid, aes(x = id, y = residuals)) +
    geom_point(alpha=0.2) +
    labs(y = "Residuals", x = "Index")
  p2 <- ggplot(df_model_resid, aes(x = id, y = standard)) +
    geom_point(alpha=0.2) +
    geom_hline(yintercept  =3.29, col = "red", lty=2) +
    geom_hline(yintercept  =-3.29, col = "red", lty=2)+
    labs(y = "Standardized Residuals", x = "Index")
  p3 <- ggplot(df_model_resid, aes(x = id, y = studend)) +
    geom_point(alpha=0.2) +
    labs(y = "Studentized Residuals", x = "Index")
  p1+p2+p3
}

