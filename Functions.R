round2 <- function(x) {
  # round to two. Doesn't drop trailing zeros
  a <- format(round(x, 2), nsmall=2)
  return(a)
}

p_round <- function(x) {
  if(is.data.frame(x) | length(x) > 1) {
    y <- function(z) {
      if(z < .001) {
        noquote("< .001")
      }
      else {
        noquote(weights::rd(z, 3))
      }
    }
    noquote(sapply(x, y))
  }
  else {
    if(x < .001) {
      noquote("< .001")
    }
    else {
      noquote(weights::rd(x, 3))
    }
  }
  # removes leading zero. Install weights package if needed
}

# make a condition version. Reliability in each condition. And maybe missing data one. 
meas <- function(x) {
  a <- cor(x, use = "pairwise")
  b <- range(x, na.rm = T)
  c <- psych::alpha(x, check.keys = T)
  d <- psych::alpha(x, check.keys = T)$scores
  e <- shapiro.test(d)
  f <- data.frame(d)
  g <- ggplot2::ggplot(f, aes(x=d)) + 
    ggplot2::geom_histogram(aes(y=after_stat(density)), binwidth = range2(d)/14) +
    geom_density(adjust=1,kernel="gaussian",na.rm=TRUE,
                 color="red" ,linewidth=.5) + 
    stat_function(fun = dnorm, 
                  args = list(mean = mean(f$d,na.rm=TRUE),
                              sd = sd(f$d,na.rm=TRUE)), 
                  linewidth = 1, 
                  color = "blue") +
    xlab("Aggregate") +
    theme_bw()
  if(Hmisc::label(x[1]) != "") {
    h <- list("Item"=Hmisc::label(x), "Cor"=a, "ALpha"=c, "Range"=b, "Normality"=e, "Plot"=g, "Scores"=d)
    print(h[1:6])
    invisible(h)
  }
  else {
    h <- list("Cor"=a, "ALpha"=c, "Range"=b, "Normality"=e, "Plot"=g, "Scores"=d)
    print(h[1:5])
    invisible(h)
  }
}

# revisit these to have multiple predictors/formulas
histo <- function(formula, data=NULL, formula2=NULL) {
  library(ggplot2)
  a <- lm(formula = formula, data = data, na.action = "na.exclude")
  b <- data.frame(a$model[1], a$residuals)
  colnames(b) <- c("Outcome", "Residuals")
  c <- data.frame(col=grDevices::colors()) |>  dplyr::filter(!stringr::str_detect(col, "grey|gray"))
  d <- ggplot2::ggplot(b, aes(x=Outcome)) + 
    geom_histogram(aes(y=after_stat(density)), binwidth = range2(b$Outcome)/14, 
  color=sample(c$col, 1),fill=sample(c$col, 1)) +
    geom_density(adjust=1,kernel="gaussian",na.rm=TRUE,
                 color=sample(c$col, 1), linewidth=1) + 
    stat_function(fun = dnorm, 
                  args = list(mean = mean(b$Outcome,na.rm=TRUE),
                              sd = sd(b$Outcome,na.rm=TRUE)), 
                  linewidth = 1, 
                  color=sample(c$col, 1)) +
    theme_bw() |> suppressMessages() |> suppressWarnings()
  e <- ggplot2::ggplot(b, aes(x=Residuals)) + 
    geom_histogram(aes(y=after_stat(density)), binwidth = range2(b$Residuals)/14,
  color=sample(c$col, 1),fill=sample(c$col, 1)) +
    geom_density(adjust=1,kernel="gaussian",na.rm=TRUE,
                 color=sample(c$col, 1) ,linewidth=1) + 
    stat_function(fun = dnorm, 
                  args = list(mean = mean(b$Residuals,na.rm=TRUE),
                              sd = sd(b$Residuals,na.rm=TRUE)), 
                  linewidth = 1, 
                  color=sample(c$col, 1)) +
    theme_bw() |> suppressMessages() |> suppressWarnings()
  f <- ggpubr::ggarrange(d, e)
  if (length(a$coefficients) == 1) {return(d)} 
  else  
  {if (is.null(formula2)) { return(f) }
    else { 
      g <- lm(formula = formula2, data = data, na.action = "na.exclude")
      h <- data.frame(residuals(g))
      colnames(h) <- c("Residuals")
      j <- ggplot2::ggplot(h, aes(x=Residuals)) + 
        geom_histogram(aes(y=after_stat(density)), color=sample(c$col, 1),fill=sample(c$col, 1)) +
        geom_density(adjust=1,kernel="gaussian",na.rm=TRUE,
                     color=sample(c$col, 1) ,linewidth=1) + 
        stat_function(fun = dnorm, 
                      args = list(mean = mean(h$Residuals,na.rm=TRUE),
                                  sd = sd(h$Residuals,na.rm=TRUE)), 
                      linewidth = 1, 
                      color=sample(c$col, 1)) +
        theme_bw() |> suppressMessages() |> suppressWarnings()
      k <- egg::ggarrange(d, e, j)
      return(k)}
  }
}
# Tables ----

# underscores instead of comma in the CI because csvs will misinterpret it. 
ttable <- function(data, holm=T) {
  library(rstatix)
  library(tidyr)
  library(dplyr)
  library(labelled)
  c <- colnames(data)
  data <- remove_labels(data)
  x1 <- data |> 
    dplyr::rename("Group"=last_col()) |> 
    pivot_longer(-Group, values_to = "values", names_to = "variables") |> 
    group_by(variables) |> 
    rstatix::t_test(values~Group, detailed = T) |> 
    data.frame() |> 
    mutate(t=round2(statistic), p=p_round(p), df=round2(df), conf.low=round2(conf.low), 
           conf.high=round2(conf.high), meandiff=round2(estimate), est1=round2(estimate1), est2=round2(estimate2)) |> 
    unite("95%CI", conf.low:conf.high, sep = "__ ", remove = T) 
  x2 <- data |> 
    dplyr::rename("Group"=last_col()) |> 
    pivot_longer(-Group, values_to = "values", names_to = "variables") |> 
    group_by(variables, Group) |> 
    dplyr::summarise(sd=round2(sd(values, na.rm = T))) |> 
    pivot_wider(names_from = Group, values_from = sd)|>  
    data.frame() |> 
    suppressMessages() |> 
    suppressWarnings()
  x3 <- data |> 
    dplyr::rename("Group"=last_col()) |> 
    pivot_longer(-Group, values_to = "values", names_to = "variables") |> 
    dplyr::group_by(variables) |> 
    cohens_d(values~Group, hedges.correction = T) |>  
    data.frame() 
  x4 <- cbind(x2[-1], x1, "g"=round2(x3$effsize))
  colnames(x4)[1] <- "g1"
  colnames(x4)[2] <- "g2"
  x5 <- x4 |> mutate(endp1=c(")"), endp2=c(")")) |> 
    unite(col=Group1, c(est1, g1), sep = " (", remove = T) |> 
    unite(col="Group1 M (SD)", c(Group1, endp1), sep = "") |> 
    unite(col=Group2, c(est2, g2), sep = " (", remove = T) |> 
    unite(col="Group2 M (SD)", c(Group2, endp2), sep = "") |> 
    mutate(variables=factor(variables, levels =c)) |> 
    arrange(variables) |> 
    dplyr::select("Outcomes"=variables, "Group1 M (SD)", "Group2 M (SD)", t, df, p, "95%CI", g)
  x5
}

# NA ----
namean <- function(x, na.rm=T) {
  if(na.rm==T) {
    a<-mean(x, na.rm=T)
  }
 if(na.rm==F) {
    a<-mean(x, na.rm=F)
 }
  a
}

nasd <- function(x, na.rm=T) {
  if(na.rm==T) {
    a<-sd(x, na.rm=T)
  }
  if(na.rm==F) {
    a<-sd(x, na.rm=F)
  }
  a
}

navar <- function(x, na.rm=T) {
  if(na.rm==T) {
    a<-var(x, na.rm=T)
  }
  if(na.rm==F) {
    a<-var(x, na.rm=F)
  }
  a
}


narange <- function(x, na.rm=T) {
  if(na.rm==T) {
    a<-range(x, na.rm=T)
  }
  if(na.rm==F) {
    a<-range(x, na.rm=F)
  }
  a
}

range2 <- function(x, na.rm=T) {
  if(na.rm==T) {
    a<-max(x, na.rm=T)-min(x, na.rm=T)
  }
  if(na.rm==F) {
    a<-max(x, na.rm=F)-min(x, na.rm = F)
  }
  a
}

namax <- function(x, na.rm=T) {
  if(na.rm==T) {
    a<-max(x, na.rm=T)
  }
  if(na.rm==F) {
    a<-max(x, na.rm=F)
  }
  a
}

namin <- function(x, na.rm=T) {
  if(na.rm==T) {
    a<-min(x, na.rm=T)
  }
  if(na.rm==F) {
    a<-min(x, na.rm=F)
  }
  a
}

nalength <- function(x) {
  length(na.omit(x))
}


# RMarkdown APA ----
msd <- function(x, y=NULL, group=NULL, y2=NULL, group2=NULL) {
  if(is.null(y)) {
    paste("(*M* = ", format(round(mean(x, na.rm = T),2)),  ", *SD* = ", format(round(sd(x, na.rm = T),2)), ")", sep = "") 
  }
  else if(is.null(y2) & !is.null(y)) {
    x <- x[y==group]
    paste("(*M* = ", format(round(mean(x, na.rm = T),2)),  ", *SD* = ", format(round(sd(x, na.rm = T),2)), ")", sep = "") 
  }
  else {
    x <- x[y==group & y2==group2]
    paste("(*M* = ", format(round(mean(x, na.rm = T),2)),  ", *SD* = ", format(round(sd(x, na.rm = T),2)), ")", sep = "")
  } 
}

apat <- function(formula, rev=F, ...) {
  # set rev=T to reverse the t value and confidence interval
  a <- t.test(formula, ...)
  if(a$p.value < .001) {
    p <- paste(", *p* < .001") }
  else {
    p <- paste(", *p* = ", weights::rd(a$p.value, 3)) }
  if(rev==T) {
    paste("*t*(", 
          round(a$parameter,2), 
          ") = ", 
          format(round((a$statistic), 2), nsmall=2), 
          p,
          ", 95%CI: [",
          weights::rd(a$conf.int[1]),
          ", ",
          weights::rd(a$conf.int[2]),
          "]",
          sep="")
  }
  else {
    paste("*t*(", 
          round(a$parameter,2), 
          ") = ", 
          format(round((0-a$statistic), 2), nsmall=2), 
          p,
          ", 95%CI: [",
          weights::rd(0-a$conf.int[2]),
          ", ",
          weights::rd(0-a$conf.int[1]),
          "]",
          sep="") }
}

apalm <- function(mod) {
  x <- coefficients(summary(mod))
  x[,-4] <- format(round(x[,-4], 2), nsmall=2, trim=T)
  x[,4] <- p_round(as.numeric(x[,4]))
  x[,4] <- ifelse(startsWith(x[,4], "<"), x[,4], paste("=", x[,4]))
  y <- format(round(confint(mod), 2), nsmall=2, trim=T) 
  z <- model.matrix.lm(mod)
  v <- character(nrow(x)-1)
  for (i in 2:nrow(x)) {
    beta <- format(round(coefficients(lm(scale(model.frame(mod)[,1])~ residuals(lm(scale(z[,i])~scale(z[,-c(1,i)])))))[2], 2), nsmall=2, trim=T)
    v[i-1] <- paste("b = ", x[i,1], ", B = ", 
                    beta, 
                    ", t(", mod$df.residual, ") = ", x[i,3], ", p ", x[i,4], 
                    ", 95%CI: [", y[i,1], ", ", y[i,2], "]", sep = "")
  }
  names(v) <- rownames(x)[-1]
  return(v)
}



#mod <- lmer(value ~ Condition*nonconforming*I(tmen-.5)  + (1 | ID), data = al)
#rf <- unlist(strsplit(as.character(formula(mod)), "\\+"))

apalmer <- function(mod) {
  x <- coefficients(summary(mod))
  x[,-5] <- format(round(x[,-5], 2), nsmall=2, trim=T)
  x[,5] <- p_round(as.numeric(x[,5]))
  x[,5] <- ifelse(startsWith(x[,5], "<"), x[,5], paste("=", x[,5]))
  y <- format(round(confint(mod), 2), nsmall=2, trim=T) |> suppressMessages()
  leny <- length(y[,1])
  z <- data.frame(data.frame(model.matrix(mod)[,-1]), model.frame(mod)[,ncol(model.frame(mod))])
  v <- character(nrow(x)-1)
  rf <- unlist(strsplit(as.character(formula(mod)), "\\+"))
  rf <- rf[grepl(pattern =  "\\|", x=rf)]
  for (i in 1:nrow(x)) {
   # z1 <- cbind(z[,i], z[,-1])
   # DF2formula(z1)
    #pred <- residuals(lmer(scale(z[,i])~scale(z[,-c(1,i)])))
    
   # beta <- format(round(coefficients(lm(scale(model.frame(mod)[,1])~ ))[2], 2), nsmall=2, trim=T)
    v[i-1] <- paste("b = ", x[i,1], 
                    ", t(", x[i,3], ") = ", x[i,4], ", p ", x[i,5], 
                    ", 95%CI: [", y[leny-nrow(x)+i, 1], ", ", y[leny-nrow(x)+i, 2], "]", sep = "")
  }
  names(v) <- rownames(x)[-1]
  return(v)
}
#apalmer(mod)
# z <- model.matrix.lm(mod)

# mod
# coefficients(lm(scale(model.frame(mod)[,1])~ residuals(lm(scale(z[,6])~scale(z[,-c(1,6)])))))[2]
# arm::standardize(lm(mpg~I(as.factor(cyl)) + am, data=mtcars), standardize.y=T, binary.inputs="full")
# QuantPsyc::lm.beta(mod)

# library(fastDummies)
# dat <- data.frame("mpg"=mtcars$mpg, fastDummies::dummy_cols(mtcars$cyl, remove_first_dummy = T)[,-1], "am"=mtcars$am)
# dat
# dat$am6 <- dat$am*dat$.data_6
# dat$am8 <- dat$am*dat$.data_8
# mod2 <- lm(mpg~am + .data_6 + .data_8 + am6 + am8, data=dat)
# lm(mod)
# mod2
# 
# arm::standardize(mod2, standardize.y=T, binary.inputs="full")
# QuantPsyc::lm.beta(mod2)
#basically my standardization works like theirs except better


# exportlabdf <- function(df, ...) {
#   df <- df |>  dplyr::select(-c(IPAddress, ResponseId, RecipientLastName, Status, RecipientEmail, 
#                      RecipientLastName, RecipientFirstName, ExternalReference, LocationLatitude, LocationLongitude,
#                      DistributionChannel)) |> data.frame()
#   df <- rbind(Hmisc::label(df), data.frame(apply(df, 2, as.character)))
#   df <- data.frame(df)
#   write.csv(df, row.names=F, ...)
#   print("Check data for identifiers then duplicate it to erase change history")
# }

exportlabdf <- function(df, ...) {
  df <- df[,setdiff(colnames(df), c("IPAddress", "ResponseId", "Status", "RecipientEmail", 
                          "RecipientLastName", "RecipientFirstName", "ExternalReference", "LocationLatitude", "LocationLongitude",
                          "DistributionChannel", "aid"))]
  df <- rbind(Hmisc::label(df), data.frame(apply(df, 2, as.character)))
  df <- data.frame(df)
  write.csv(df, row.names=F, ...)
  print("Check data for identifiers then duplicate it to erase change history")
}
  

