fun.ve.cox <- function(z.var,z) {
  #z is a cox model fitted
  z.coefs <- summary(z)$coefficients
  z.dn2 <- dimnames(z.coefs)[[2]]
  z.sel <- grepl(z.var,dimnames(z.coefs)[[1]])
  z.c <- matrix(z.coefs[z.sel,],ncol=ncol(z.coefs),dimnames=list(dimnames(z.coefs)[[1]][z.sel],z.dn2))
  z.est <- z.c[,"coef"]
  z.se <- z.c[,grepl("se\\(", dimnames(z.c)[[2]])]
  z.sel <- grepl("^p$", dimnames(z.c)[[2]]) | grepl("^Pr", dimnames(z.c)[[2]])
  z.pv <- z.c[,z.sel]
  z.out <- cbind(z.est,z.est-1.96*z.se,z.est+1.96*z.se)
#  z.out <- cbind((1-exp(z.out))*100,z.c[,"Pr(>|z|)"])
  z.out <- cbind(exp(z.out),z.pv)
  dimnames(z.out)[[2]] <- c("HR","LCL","UCL","P")
  z.out <- data.frame(z.out) %>% mutate(var=row.names(z.out)) %>% 
    mutate(var = gsub("get\\(z_var\\)","",var))
  z1 <- data.frame(HR=1,LCL=1,UCL=1, P=NA, var="uv")
  z.out <- bind_rows(z1,z.out)
  z.out
}

fun_ve_glm <- function(z_raw,z_type="raw"){
  z_coef <- cbind(z_raw$coefficients, confint.default(z_raw))
  z_sel <- grepl("vacc_status", dimnames(z_coef)[[1]])
  z_coef <- z_coef[z_sel,]
  dimnames(z_coef)[[1]] <- gsub("vacc_status", "", dimnames(z_coef)[[1]])
  z_coef <- rbind(c(0,0,0), z_coef)
  dimnames(z_coef)[[1]][1] <- "Unvaccinated"
  z_coef_or <- exp(z_coef)
  dimnames(z_coef_or)[[2]] <- c("OR","OR_LCL","OR_UCL")
  if (z_type == "adj" ) dimnames(z_coef_or)[[2]] <- paste0(dimnames(z_coef_or)[[2]], "_adj")
  if (z_type == "raw" ) dimnames(z_coef_or)[[2]] <- paste0(dimnames(z_coef_or)[[2]], "_raw")
  z_coef <- (1-exp(z_coef))*100
  z_coef <- z_coef[,c(1,3,2)]
  dimnames(z_coef)[[2]] <- c("VE","VE_LCL","VE_UCL")
  if (z_type == "adj" ) dimnames(z_coef)[[2]] <- paste0(dimnames(z_coef)[[2]], "_adj")
  if (z_type == "raw" ) dimnames(z_coef)[[2]] <- paste0(dimnames(z_coef)[[2]], "_raw")
  z_coef_raw <- as.data.frame(z_coef_or) %>% bind_cols(as.data.frame(z_coef))
  z_coef_raw <- z_coef_raw %>% mutate(vacc_status = row.names(z_coef))
  z_coef_raw
}

fun_ve_gam <- function(z_adj){
  z_sel <- grepl("vacc_status", names(z_adj$coefficients))
  z_coef <- z_adj$coefficients[z_sel]
  z_ci <- confint.default(z_adj)
  z_sel <- grepl("vacc_status", dimnames(z_ci)[[1]])
  z_ci <- z_ci[z_sel,]
  z_coef <- cbind(z_coef,z_ci)
  dimnames(z_coef)[[1]] <- gsub("vacc_status", "", dimnames(z_coef)[[1]])
  z_coef <- rbind(c(0,0,0), z_coef)
  dimnames(z_coef)[[1]][1] <- "Unvaccinated"
  z_coef_or <- exp(z_coef)
  dimnames(z_coef_or)[[2]] <- c("OR_adj","OR_LCL_adj","OR_UCL_adj")
  z_coef <- (1-exp(z_coef))*100
  z_coef <- z_coef[,c(1,3,2)]
  dimnames(z_coef)[[2]] <- c("VE_Adj","LCL_Adj","UCL_Adj")
  
  z_coef_adj <- as.data.frame(z_coef_or) %>% bind_cols(as.data.frame(z_coef))
  z_coef_adj <- z_coef_adj %>% mutate(vacc_status = row.names(z_coef))
  z_coef_adj
}
