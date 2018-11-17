#' TasKar – Binary Classification Performance Calculator/Dashboard (Missing Metrics Calculator) 
#' Copyright (C) 2016-2018 Gürol Canbek  
#' This file is licensed under
#' 
#'   GNU Affero General Public License v3.0, GNU AGPLv3 
#' 
#' This program is free software: you can redistribute it and/or modify
#' it under the terms of the GNU Affero General Public License as published
#' by the Free Software Foundation, either version 3 of the License, or
#' (at your option) any later version.
#'
#' This program is distributed in the hope that it will be useful,
#' but WITHOUT ANY WARRANTY; without even the implied warranty of
#' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#' GNU Affero General Public License for more details.
#'
#' You should have received a copy of the GNU Affero General Public License
#' along with this program.  If not, see <https://www.gnu.org/licenses/>.
#' 
#' See the license file in <https://github.com/gurol/taskar>
#'
#' @author Gürol Canbek, <gurol44@gmail.com> 
#' @references <http://gurol.canbek.com>  
#' @keywords utilities, common functions  
#' @title Missing Metrics Revealer
#' @version 1.2  
#' @description R functions for calculating confusion matrix (base measures)
#' from given performance instruments (e.g. P, N, TPR, and ACC) as well as
#' initializing global confusion matrix by given instruments.  
#' @note version history  
#' 1.2, 13 November 2018, Converting calculated base measures into integer  
#' 1.1, 26 March 2018, Batch processing  
#' 1.0, 19 February 2017, The first version  
#' @date 13 November 2018  

#' libraries  
# None

#' ### integerClassBaseMeasures
#' Convert the base measures of a single class into integer.  
#' **Parameters:**  
#' *fTrueClass*: Calculated float True-Class value (TP or TN)  
#' *fFalseOtherClass*: Calculated float False-Other-Class value (FN or FP)  
#' *dClass*: Given (actual) class size (P or N)  
#' **Return:**  
#' A vector first element integer True-Class and integer False-Other-Class value  
#' **Example Usage:** 
# int_values <- integerClassBaseMeasures(12.25, 12.75, 25)
integerClassBaseMeasures <- function(fTrueClass, fFalseOtherClass, dClass)
{
  tTrueClass <- trunc(fTrueClass)
  tFalseOtherClass <- trunc(fFalseOtherClass)
  tClass <- tTrueClass + tFalseOtherClass
  
  delta <- dClass - tClass
  
  if (delta == 0) {
    return (c(tTrueClass, tFalseOtherClass))
  }
  
  if (delta == 2) {
    # Increase truncated values
    return (c(tTrueClass+1, tFalseOtherClass+1))
  }
  
  if (delta == -2) {
    # Decreas truncated values
    return (c(tTrueClass-1, tFalseOtherClass-1))
  }
  
  frTrueClass <- fTrueClass - tTrueClass
  frFalseOtherClass <- fFalseOtherClass - tFalseOtherClass
  
  if (delta == 1) {
    # Increase truncated values
    if (frTrueClass >= 0.5 && frFalseOtherClass >= 0.5) {
      if (frTrueClass > frFalseOtherClass) {
        return (c(tTrueClass+1, tFalseOtherClass))
      }
      
      if (frTrueClass < frFalseOtherClass) {
        return (c(tTrueClass, tFalseOtherClass+1))
      }
      
      if (tTrueClass > tFalseOtherClass) {
        return (c(tTrueClass+1, tFalseOtherClass))
      }
      
      return (c(tTrueClass, tFalseOtherClass+1))
    }
    
    if (frTrueClass >= 0.5) {
      return (c(tTrueClass+1, tFalseOtherClass))
    }
    
    if (frFalseOtherClass >= 0.5) {
      return (c(tTrueClass, tFalseOtherClass + 1))
    }
    
    if (tTrueClass > tFalseOtherClass) {
      return (c(tTrueClass+1, tFalseOtherClass))
    }
    
    return (c(tTrueClass, tFalseOtherClass+1))
  }
  
  
  # Delta = -1, Decrease truncated values
  if (frTrueClass >= 0.5 && frFalseOtherClass >= 0.5) {
    if (frTrueClass > frFalseOtherClass) {
      return (c(tTrueClass-1, tFalseOtherClass))
    }
    
    if (frTrueClass < frFalseOtherClass) {
      return (c(tTrueClass, tFalseOtherClass-1))
    }
    
    if (tTrueClass > tFalseOtherClass) {
      return (c(tTrueClass-1, tFalseOtherClass))
    }
    
    return (c(tTrueClass, tFalseOtherClass-1))
  }
  
  if (frTrueClass >= 0.5) {
    return (c(tTrueClass-1, tFalseOtherClass))
  }
  
  if (frFalseOtherClass >= 0.5) {
    return (c(tTrueClass, tFalseOtherClass-1))
  }
  
  if (tTrueClass > tFalseOtherClass) {
    return (c(tTrueClass-1, tFalseOtherClass))
  }
  
  return (c(tTrueClass, tFalseOtherClass-1))
}

#' ### getMeasuresViaP_TPR_FPR_ACC
#' Reveal base measures (TP, FP, FN, TN) and Sn from P, TPR, FPR, ACC  
#' **Parameters:**  
#' *p*: Positive  
#' *tpr*: True Positive Rate  
#' *fpr*: False Positive Rate  
#' *acc*: Accuracy  
#' **Return:**  
#' A list with base measures and Sn  
getMeasuresViaP_TPR_FPR_ACC <- function(p, tpr, fpr, acc)
{
  # Check inputs
  p <- as.numeric(p)
  stopifnot(all(p >= 0))
  
  tpr <- as.numeric(tpr)
  stopifnot(all(tpr >= 0) && all(tpr <= 1))
  
  fpr <- as.numeric(fpr)
  stopifnot(all(fpr >= 0) && all(fpr <= 1))
  tnr <- 1 - fpr
  
  acc <- as.numeric(acc)
  stopifnot(all(acc >= 0) && all(acc <= 1))
  
  # Calculate base and 1st level measures
  tp <- p*tpr
  fn <- p - tp
  tn <- tnr*(tp - acc*p)/(acc*(fpr+tnr) - tnr)
  fp <- fpr*tn/tnr
  sn <- tp+fp+fn+tn
  # n <- p*(tpr-acc)/(acc+fpr-1)
  # sn <- p+n
  # tn <- acc*sn-tp
  # fp <- n-tn
  # fn <- p-tp
  
  return (list(TP=tp, FP=fp, FN=fn, TN=tn, Sn=sn))
}

#' ### getMeasuresViaP_N_TPR_ACC
#' Reveal base measures (TP, FP, FN, TN) and Sn from P, N, TPR, ACC  
#' **Parameters:**  
#' *p*: Positive  
#' *n*: Negative  
#' *tpr*: True Positive Rate  
#' *acc*: Accuracy  
#' **Return:**  
#' A list with base measures and Sn  
getMeasuresViaP_N_TPR_ACC<-function(p, n, tpr, acc)
{
  # Check inputs
  p <- as.numeric(p)
  stopifnot(all(p >= 0))
  
  n <- as.numeric(n)
  stopifnot(all(n >= 0))
  
  tpr <- as.numeric(tpr)
  stopifnot(all(tpr >= 0) && all(tpr <= 1))
  
  acc <- as.numeric(acc)
  stopifnot(all(acc >= 0) && all(acc <= 1))
  
  # Calculate base and 1st level measures
  tp <- p*tpr
  sn <- p+n
  tn <- acc*sn-tp
  fp <- n-tn
  fn <- p-tp
  
  return (list(TP=tp, FP=fp, FN=fn, TN=tn, Sn=sn))
}

#' ### getMeasuresViaP_N_TNR_ACC
#' Reveal base measures (TP, FP, FN, TN) and Sn from P, N, TNR, ACC  
#' **Parameters:**  
#' *p*: Positive  
#' *n*: Negative  
#' *tnr*: True Negative Rate  
#' *acc*: Accuracy  
#' **Return:**  
#' A list with base measures and Sn  
getMeasuresViaP_N_TNR_ACC<-function(p, n, tnr, acc)
{
  # Check inputs
  p <- as.numeric(p)
  stopifnot(all(p >= 0))
  
  n <- as.numeric(n)
  stopifnot(all(n >= 0))
  
  tnr <- as.numeric(tnr)
  stopifnot(all(tnr >= 0) && all(tnr <= 1))
  
  acc <- as.numeric(acc)
  stopifnot(all(acc >= 0) && all(acc <= 1))
  
  # Calculate base and 1st level measures
  tn <- n*tnr
  sn <- p+n
  tp <- acc*sn-tn
  fp <- n-tn
  fn <- p-tp
  
  return (list(TP=tp, FP=fp, FN=fn, TN=tn, Sn=sn))
}

#' ### getMeasuresViaP_N_TPR_PPV
#' Reveal base measures (TP, FP, FN, TN) and Sn from P, N, TPR, PPV  
#' **Parameters:**  
#' *p*: Positive  
#' *n*: Negative  
#' *tpr*: True Positive Rate  
#' *ppv*: Positive Predictive Value  
#' **Return:**  
#' A list with base measures and Sn  
getMeasuresViaP_N_TPR_PPV<-function(p, n, tpr, ppv)
{
  # Check inputs
  p <- as.numeric(p)
  stopifnot(all(p >= 0))
  
  n <- as.numeric(n)
  stopifnot(all(n >= 0))
  
  tpr <- as.numeric(tpr)
  stopifnot(all(tpr >= 0) && all(tpr <= 1))
  
  ppv <- as.numeric(ppv)
  stopifnot(all(ppv >= 0) && all(ppv <= 1))
  
  # Calculate base and 1st level measures
  tp <- p*tpr
  sn <- p+n
  fp <- (tp/ppv)-tp
  fn <- p-tp
  tn <- n-fp
  
  return (list(TP=tp, FP=fp, FN=fn, TN=tn, Sn=sn))
}

#' ### getMeasuresViaP_N_TPR_FPR
#' Reveal base measures (TP, FP, FN, TN) and Sn from P, N, TPR, FPR  
#' **Parameters:**  
#' *p*: Positive  
#' *n*: Negative  
#' *tpr*: True Positive Rate  
#' *fpr*: False Positive Rate  
#' **Return:**  
#' A list with base measures and Sn  
getMeasuresViaP_N_TPR_FPR <- function(p, n, tpr, fpr)
{
  # Check inputs
  p <- as.numeric(p)
  stopifnot(all(p >= 0))
  
  n <- as.numeric(n)
  stopifnot(all(n >= 0))
  
  tpr <- as.numeric(tpr)
  stopifnot(all(tpr >= 0) && all(tpr <= 1))
  
  fpr <- as.numeric(fpr)
  stopifnot(all(fpr >= 0) && all(fpr <= 1))
  
  # Calculate base and 1st level measures
  tp <- p*tpr
  sn <- p+n
  fp <- n*fpr
  fn <- p-tp
  tn <- n-fp
  
  return (list(TP=tp, FP=fp, FN=fn, TN=tn, Sn=sn))
}

#' ### getMeasuresViaP_N_TPR_TNR
#' Reveal base measures (TP, FP, FN, TN) and Sn from P, N, TPR, TNR via
#' getMeasuresViaP_N_TPR_FPR  
#' **Parameters:**  
#' *p*: Positive  
#' *n*: Negative  
#' *tpr*: True Positive Rate  
#' *tnr*: True Negative Rate  
#' **Return:**  
#' A list with base measures and Sn  
getMeasuresViaP_N_TPR_TNR <- function(p, n, tpr, tnr)
{
  fpr <- 1-as.numeric(tnr)
  
  return (getMeasuresViaP_N_TPR_FPR(p, n, tpr, fpr))
}

#' ### getMeasuresViaP_N_ACC_F1
#' Reveal base measures (TP, FP, FN, TN) and Sn from P, N, ACC, F1  
#' **Parameters:**  
#' *p*: Positive  
#' *n*: Negative  
#' *acc*: Accuracy  
#' *f1*: F1  
#' **Return:**  
#' A list with base measures and Sn  
getMeasuresViaP_N_ACC_F1 <- function(p, n, acc, f1)
{
  # Check inputs
  p <- as.numeric(p)
  stopifnot(all(p >= 0))
  
  n <- as.numeric(n)
  stopifnot(all(n >= 0))
  
  acc <- as.numeric(acc)
  stopifnot(all(acc >= 0) && all(acc <= 1))
  
  # F1 = 2TP/(2TP+FC)
  f1 <- as.numeric(f1)
  stopifnot(all(f1 >= 0) && all(f1 <= 1))
  
  # Calculate base and 1st level measures
  # ACC = TC/(P+N) => TC = ACC.(P+N)
  tc <- acc*(p+n)
  # FC = (P+N) - TC (1)
  fc <- (p+n)-tc
  
  # => 2TP.F1 + FC.F1 = 2TP
  # => 2TP.F1 - 2TP = -FC.F1
  # => 2TP(F1-1) = -FC.F1
  # => 2TP(1-F1) = FC.F1
  # TP = FC.F1/(2.(1-F1))
  tp <- fc*f1/(2*(1-f1))
  
  tpr <- tp/p
  stopifnot(all(tpr >= 0) && all(tpr <= 1))
  
  return (getMeasuresViaP_N_TPR_ACC(p, n, tpr, acc))
}

#' ### getMeasuresViaP_N_FPR_FNR
#' Reveal base measures (TP, FP, FN, TN) and Sn from P, N, FPR, FNR via
#' getMeasuresViaP_N_TPR_TNR  
#' **Parameters:**  
#' *p*: Positive  
#' *n*: Negative  
#' *fpr*: False Positive Rate  
#' *fnr*: False Negative Rate  
#' **Return:**  
#' A list with base measures and Sn  
getMeasuresViaP_N_FPR_FNR <- function(p, n, fpr, fnr)
{
  tpr <- 1-as.numeric(fnr)
  tnr <- 1-as.numeric(fpr)
  
  return (getMeasuresViaP_N_TPR_TNR(p, n, tpr, tnr))
}

#' ### getMeasuresViaP_N_FNR_ACC
#' Reveal base measures (TP, FP, FN, TN) and Sn from P, N, FNR, ACC via
#' getMeasuresViaP_N_TPR_ACC  
#' **Parameters:**  
#' *p*: Positive  
#' *n*: Negative  
#' *fnr*: False Negative Rate  
#' *acc*: Accuracy  
#' **Return:**  
#' A list with base measures and Sn  
getMeasuresViaP_N_FNR_ACC <- function(p, n, fnr, acc)
{
  tpr <- 1-as.numeric(fnr)
  
  return (getMeasuresViaP_N_TPR_ACC(p, n, tpr, acc))
}

#' ### getMeasuresViaP_N_FPR_ACC
#' Reveal base measures (TP, FP, FN, TN) and Sn from P, N, FPR, ACC via
#' getMeasuresViaP_N_TNR_ACC  
#' **Parameters:**  
#' *p*: Positive  
#' *n*: Negative  
#' *fpr*: False Positive Rate  
#' *acc*: Accuracy  
#' **Return:**  
#' A list with base measures and Sn  
getMeasuresViaP_N_FPR_ACC <- function(p, n, fpr, acc)
{
  tnr <- 1-as.numeric(fpr)
  
  return (getMeasuresViaP_N_TNR_ACC(p, n, tnr, acc))
}

#' ### getMeasuresViaBaseMeasures
#' Return base measures (TP, FP, FN, TN) and Sn from base measures  
#' **Parameters:**  
#' *tp*: True Positive  
#' *fp*: False Positive  
#' *fn*: False Negative  
#' *tn*: True Negative  
#' **Return:**  
#' A list with base measures and Sn  
getMeasuresViaBaseMeasures <- function(tp, fp, fn, tn)
{
  tp <- as.numeric(tp)
  stopifnot(all(tp >= 0))
  
  fp <- as.numeric(fp)
  stopifnot(all(fp >= 0))
  
  fn <- as.numeric(fn)
  stopifnot(all(fn >= 0))
  
  tn <- as.numeric(tn)
  stopifnot(all(tn >= 0))
  
  sn <- tp + fp + fn + tn
  
  return (list(TP=tp, FP=fp, FN=fn, TN=tn, Sn=sn))
}

#' ### initConfusionMatrixViaBaseMeasures
#' Initialize global base measures (TP, FP, FN, TN) and Sn from base measures  
#' **Parameters:**  
#' *bm*: Base Measures data frame (TP, FP, FN, TN)  
#' **Return:**  
#' None
#' **Examples:**   
# base_measures <- rclip()
# initConfusionMatrixViaBaseMeasures(base_measures)
## then
# source('TasKarMetrics.R')
## to calculate all the metrics/measures/indicators
# wclip(TasKar)
## Also
# source('TasKarSummaries.R')
## to calculate and populate all the stuff related to the instruments
## to copy values to paste into other platforms such as Excel use
# getAll()
initConfusionMatrixViaBaseMeasures<-function(base_measures)
{
  m <- getMeasuresViaBaseMeasures(base_measures$TP, base_measures$FP,
                                  base_measures$FN, base_measures$TN)
  TP <<- m$TP
  FP <<- m$FP
  FN <<- m$FN
  TN <<- m$TN
  Sn <<- m$Sn
}

#' ### initConfusionMatrixViaP_TPR_FPR_ACC
#' Initialize global base measures (TP, FP, FN, TN) and Sn from
#' give data frame with P, TPR, FPR, ACC columns  
#' **Parameters:**  
#' *metrics*: Instrument data frame with P, TPR, FPR, ACC  
#' **Return:**  
#' None
#' **Example Usage for all initConfusionMatrix...:**   
## clear all objects from the workspace
# source('TasKar.R')
## copy reported metrics from the spreadsheet
# metrics <- rclip()
# initConfusionMatrixViaP_N_TPR_ACC(metrics)
# source('TasKarMetrics.R')
# wclip(TasKar)
# source('TasKarSummaries.R')
# getAll()
initConfusionMatrixViaP_TPR_FPR_ACC<-function(metrics)
{
  m <- getMeasuresViaP_TPR_FPR_ACC(metrics$P, metrics$TPR,
                                   metrics$FPR, metrics$ACC)
  initConfusionMatrixViaBaseMeasures(m)
}

#' ### initConfusionMatrixViaP_N_TPR_ACC
#' Initialize global base measures (TP, FP, FN, TN) and Sn from
#' give data frame with P, N, TPR, ACC columns  
#' **Parameters:**  
#' *metrics*: Instrument data frame with P, N, TPR, ACC  
#' **Return:**  
#' None
initConfusionMatrixViaP_N_TPR_ACC<-function(metrics)
{
  m <- getMeasuresViaP_N_TPR_ACC(metrics$P, metrics$N,
                                 metrics$TPR, metrics$ACC)
  initConfusionMatrixViaBaseMeasures(m)
}

#' ### initConfusionMatrixViaP_N_TNR_ACC
#' Initialize global base measures (TP, FP, FN, TN) and Sn from
#' give data frame with P, N, TNR, ACC columns  
#' **Parameters:**  
#' *metrics*: Instrument data frame with P, N, TNR, ACC  
#' **Return:**  
#' None
initConfusionMatrixViaP_N_TNR_ACC<-function(metrics)
{
  m <- getMeasuresViaP_N_TNR_ACC(metrics$P, metrics$N,
                                 metrics$TNR, metrics$ACC)
  initConfusionMatrixViaBaseMeasures(m)
}

#' ### initConfusionMatrixViaP_N_TPR_PPV
#' Initialize global base measures (TP, FP, FN, TN) and Sn from
#' give data frame with P, N, TPR, PPV columns  
#' **Parameters:**  
#' *metrics*: Instrument data frame with P, N, TPR, PPV  
#' **Return:**  
#' None
initConfusionMatrixViaP_N_TPR_PPV<-function(metrics)
{
  m <- getMeasuresViaP_N_TPR_PPV(metrics$P, metrics$N,
                                 metrics$TPR, metrics$PPV)
  initConfusionMatrixViaBaseMeasures(m)
}

#' ### initConfusionMatrixViaP_N_TPR_FPR
#' Initialize global base measures (TP, FP, FN, TN) and Sn from
#' give data frame with P, N, TNR, FPR columns  
#' **Parameters:**  
#' *metrics*: Instrument data frame with P, N, TNR, FPR  
#' **Return:**  
#' None
initConfusionMatrixViaP_N_TPR_FPR<-function(metrics)
{
  m <- getMeasuresViaP_N_TPR_FPR(metrics$P, metrics$N,
                                 metrics$TPR, metrics$FPR)
  initConfusionMatrixViaBaseMeasures(m)
}

#' ### initConfusionMatrixViaP_N_TPR_TNR
#' Initialize global base measures (TP, FP, FN, TN) and Sn from
#' give data frame with P, N, TPR, TNR columns  
#' **Parameters:**  
#' *metrics*: Instrument data frame with P, N, TPR, TNR  
#' **Return:**  
#' None
initConfusionMatrixViaP_N_TPR_TNR<-function(metrics)
{
  m <- getMeasuresViaP_N_TPR_TNR(metrics$P, metrics$N,
                                 metrics$TPR, metrics$TNR)
  initConfusionMatrixViaBaseMeasures(m)
}

#' ### initConfusionMatrixViaP_N_ACC_F1
#' Initialize global base measures (TP, FP, FN, TN) and Sn from
#' give data frame with P, N, ACC, F1 columns  
#' **Parameters:**  
#' *metrics*: Instrument data frame with P, N, ACC, F1  
#' **Return:**  
#' None
initConfusionMatrixViaP_N_ACC_F1<-function(metrics)
{
  m <- getMeasuresViaP_N_ACC_F1(metrics$P, metrics$N,
                                metrics$ACC, metrics$F1)
  initConfusionMatrixViaBaseMeasures(m)
}

#' ### initConfusionMatrixViaP_N_FPR_FNR
#' Initialize global base measures (TP, FP, FN, TN) and Sn from
#' give data frame with P, N, FPR, FNR columns via
#' getMeasuresViaP_N_FPR_FNR  
#' **Parameters:**  
#' *metrics*: Instrument data frame with P, N, FPR, FNR  
#' **Return:**  
#' None
initConfusionMatrixViaP_N_FPR_FNR<-function(metrics)
{
  m <- getMeasuresViaP_N_FPR_FNR(metrics$P, metrics$N,
                                 metrics$FPR, metrics$FNR)
  initConfusionMatrixViaBaseMeasures(m)
}

#' ### initConfusionMatrixViaP_N_FNR_ACC
#' Initialize global base measures (TP, FP, FN, TN) and Sn from
#' give data frame with P, N, FNR, ACC columns via
#' getMeasuresViaP_N_FPR_FNR  
#' **Parameters:**  
#' *metrics*: Instrument data frame with P, N, FNR, ACC  
#' **Return:**  
#' None
initConfusionMatrixViaP_N_FNR_ACC<-function(metrics)
{
  m <- getMeasuresViaP_N_FNR_ACC(metrics$P, metrics$N,
                                 metrics$FNR, metrics$ACC)
  initConfusionMatrixViaBaseMeasures(m)
}

#' ### initConfusionMatrixViaP_N_FPR_ACC
#' Initialize global base measures (TP, FP, FN, TN) and Sn from
#' give data frame with P, N, FPR, ACC columns via
#' getMeasuresViaP_N_FPR_FNR  
#' **Parameters:**  
#' *metrics*: Instrument data frame with P, N, FPR, ACC  
#' **Return:**  
#' None
initConfusionMatrixViaP_N_FPR_ACC<-function(metrics)
{
  m <- getMeasuresViaP_N_FPR_ACC(metrics$P, metrics$N,
                                 metrics$FPR, metrics$ACC)
  initConfusionMatrixViaBaseMeasures(m)
}

#' ### addStudyBaseMeasures
#' Adds an instance of study of which the base measures are revealed.  
#' **Parameters:**  
#' *bm*: Revealed base measure data frane (TP, FP, FN, TN, Sn)  
#' *study*: Name of the study (e.g. s01)  
#' *config*: Classification configuration in the study (e.g. 1)  
#' *inputs*: General input data frame (TP, FP, FN, TN, TPR, FPR, FNR, ...) that
#' is used to reveal the base measures  
#' *possibility*: Internal possibility numbers indicating the combination of  
#' given inputs (i.e. Possibility: 1: NaN P, N, TPR, FPR, 2: NaN P, N, TPR,
#' ACC) (default: 1)  
#' #' *add_inconsistent_BMs*: Should inconsistent revealing results be added  
#' into the global results (e.g. revealed base measures yielding more than  
#' original Sn value (i.e. input$P + input$N or input$TP + input$FP + input$FN  
#' + input$TN). If it is TRUE, the inconsistencies are stored in global log  
#' (LogInconsistencies) (default: TRUE).
#' **Return:**  
#' None
addStudyBaseMeasures <- function(bm, study, config, inputs,
                                 instrument_combination, possibility=1,
                                 add_inconsistent_BMs=TRUE)
{
  new_study <- length(Study) + 1
  
  hasP_and_N = !is.na(inputs$N) && !is.na(inputs$P)
  consistency <- 'Mismatching Sn!'
  delta_Sn <- NA
  is_consistent <- FALSE
  
  # Check original Sn with given P and N or given BM
  # with calculated Base Measures (TP, FP, FN, TN)
  if (hasP_and_N) {
    input_Sn <- inputs$P + inputs$N
    delta_Sn <- abs(bm$Sn - input_Sn)
    if (delta_Sn < 1) {
      consistency <- 'Matching Sn'
      is_consistent <- TRUE
    }
    else if (input_Sn == bm$Sn) {
      consistency <- 'Exact matching Sn'
      is_consistent <- TRUE
    }
  }
  else {
    hasBM = !is.na(inputs$TP) && !is.na(inputs$FP) &&
      !is.na(inputs$FN) && !is.na(inputs$TNP)
    
    if (hasBM) {
      input_Sn <- inputs$TP + inputs$FP + inputs$FN + inputs$TN
      delta_Sn <- abs(bm$Sn - input_Sn)
      if (delta_Sn < 1) {
        consistency <- 'Matching Sn via BM'
        is_consistent <- TRUE
      }
      else if (inputs$P + inputs$N == bm$Sn) {
        consistency <- 'Exact matching Sn via BM'
        is_consistent <- TRUE
      }
      else {
        consistency <- 'Mismatching Sn via BM'
      }
    }
  }
  
  # Check calculated BM >= 0
  if (is_consistent && (bm$TP < 0 || bm$FP < 0 || bm$FN < 0 || bm$TN < 0)) {
    is_consistent <- FALSE
    df <- data.frame(TP=bm$TP, FP=bm$FP, FN=bm$FN, TN=bm$TN)
    df <- df[colSums(df) < 0]
    consistency <- paste('Negative Base Measure(s):',
                         paste(colnames(df), round(as.numeric(df[1, ]), 1),
                               sep=':', collapse=', '))
  }
  
  if (is_consistent || add_inconsistent_BMs) {
    reported_metrics <- inputs[!colnames(inputs) %in%
                                 c('Study', 'Config', 'N', 'P',
                                   'TP', 'FP', 'FN', 'TN')]
    reported_metrics <- removeNaColumns(reported_metrics)
    ReportedMetrics[new_study] <<-
      paste(colnames(reported_metrics), collapse=', ')
    reported_metric_values <- as.numeric(reported_metrics[1, ])
    ReportedMetricValues[new_study] <<-
      paste(colnames(reported_metrics),
            round(reported_metric_values, 4),
            sep=':', collapse=', ')
    
    has_negative_performance <- any(c('FPR', 'FNR', 'FDR', 'FOR', 'MCR')
                                        %in% names(reported_metrics))
    if (has_negative_performance) {
      reported_metric_values.positive <- as.numeric(
        reported_metrics[1,
                         -which(names(reported_metrics) %in%
                                  c('FPR', 'FNR', 'FDR', 'FOR', 'MCR'))])
      reported_metric_values.negative <- 1 - as.numeric(
        reported_metrics[1,
                         which(names(reported_metrics) %in%
                                 c('FPR', 'FNR', 'FDR', 'FOR', 'MCR'))])
    }
    else {
      reported_metric_values.positive <- as.numeric(reported_metrics[1, ])
      reported_metric_values.negative <- NA
    }
    
    AverageReportedMetricValues[new_study] <<- mean(
      c(reported_metric_values.positive, reported_metric_values.negative),
      na.rm=TRUE)
    MaxReportedMetricValues[new_study] <<- max(
      c(reported_metric_values.positive, reported_metric_values.negative),
      na.rm=TRUE)
    MinReportedMetricValues[new_study] <<- min(
      c(reported_metric_values.positive, reported_metric_values.negative),
      na.rm=TRUE)
    inputs <- appendDataFrameColumns(inputs, prefix='input')
    
    Inputs[new_study, ] <<-
      inputs[1, -which(names(inputs) %in% c('inputStudy', 'inputConfig'))]
    # TP[new_study] <<- round(bm$TP, 1)
    # FP[new_study] <<- round(bm$FP, 1)
    # FN[new_study] <<- round(bm$FN, 1)
    # TN[new_study] <<- round(bm$TN, 1)
    # Sn[new_study] <<- round(bm$Sn, 1)
    fTP[new_study] <<- bm$TP
    fFP[new_study] <<- bm$FP
    fFN[new_study] <<- bm$FN
    fTN[new_study] <<- bm$TN
    fSn[new_study] <<- bm$Sn
    
    resultsP <- integerClassBaseMeasures(bm$TP, bm$FN, inputs$inputP)
    TP[new_study] <<- resultsP[1]
    FN[new_study] <<- resultsP[2]
    
    resultsN <- integerClassBaseMeasures(bm$TN, bm$FP, inputs$inputN)
    TN[new_study] <<- resultsN[1]
    FP[new_study] <<- resultsN[2]
    
    Sn[new_study] <<- TP[new_study]+FP[new_study]+FN[new_study]+TN[new_study]
    
    # FIX HERE:
    # round(TP, 0) + round(FN, 0) + 1 == P => TP <<- ceil(TP), FN <<- ceil(FN)
    # round(TN, 0) + round(FP, 0) + 1 == N => TN <<- ceil(TN), FP <<- ceil(FP)
    Study[new_study] <<- study
    Config[new_study] <<- as.numeric(config)
    InstrumentCombination[new_study] <<- instrument_combination
    Possibilities[new_study] <<- possibility
    Consistency[new_study] <<- consistency
    DeltaSn[new_study] <<- round(delta_Sn, 1)
  }
  else {
    # Log any inconsistencies
    LogInconsistencies[length(LogInconsistencies)+1] <<-
      paste(study, as.numeric(config), possibility, instrument_combination,
            consistency, round(delta_Sn, 1), sep='; ')
  }
}

#' ### initParsedMetrics
#' Initialize global variables about base measure revealing process  
#' **Parameters:**  
#' *survey*: Classification study (survey) performance inputs  
#' *add_inconsistent_BMs*: Should inconsistent revealing results be added  
#' into the global results (e.g. revealed base measures yielding more than  
#' original Sn value (i.e. input$P + input$N or input$TP + input$FP + input$FN  
#' + input$TN). If it is TRUE, the inconsistencies are stored in global log  
#' (LogInconsistencies) (default: TRUE).
#' **Return:**  
#' None
initParsedMetrics <- function(survey)
{
  fTP <<- fFP <<- fFN <<- fTN <<- fSn <<- TP <<- FP <<- FN <<- TN <<- Sn <<-
    Possibilities <<- Config <<- DeltaSn <<-
    ReportedMetricValues <<- ReportedMetrics <<- AverageReportedMetricValues <<-
    MaxReportedMetricValues <<- MinReportedMetricValues <<- numeric()
  Study <<- InstrumentCombination <<- Consistency <<- LogInconsistencies <<-
    character()
  Inputs <<- appendDataFrameColumns(
    subset(emptyDataFrame(colnames(survey)), select=-c(Study, Config)),
    prefix='input')
}

#' ### revealConfusionMatrixes
#' Parse the (performance instrument) inputs per configuration per study and
#' reveal the confusion matrix (i.e. base measures) from possible combinations  
#' of inputs (four terms) and store the corresponding results in global
#' variables
#' **Parameters:**  
#' *survey*: Classification study (survey) performance inputs  
#' **Return:**  
#' A copy of the results in global variables in a data frame (can be used to
#' copy and paste into another medium for example a spreadsheet)
#' **Example Usage:** 
## Copy spreadsheet like:
## Study Config   N   P TP FP FN TN   TPR   TNR   FPR FNR ACC PPV NPV F1
## s01   1      261 180             0.956       0.621     
## s01   2      261 180             0.467       0.13     
#
# survey <- rclip()
## Set problematic metrics as NA
##   (for example the ones cause exceptions in initParsedMetrics)
# survey$F1[93] <- NA
## if o studies are included:
# survey$F1[120] <- NA
# survey$F1[121] <- NA
#
# parsed_base_metrics <- revealConfusionMatrixes(survey)
## or exclude mismatching Sns
# parsed_base_metrics <- revealConfusionMatrixes(survey, FALSE)
#
# wclip(parsed_base_metrics)
revealConfusionMatrixes <- function(survey, add_inconsistent_BMs=TRUE)
{
  count_survey <- nrow(survey)
  
  if (count_survey > 0) {
    initParsedMetrics(survey)
  }
  
  for (i in 1:count_survey) {
    inputs <- survey[i, ]
    study <- inputs
    hasTP <- !is.na(study$TP)
    hasFP <- !is.na(study$FP)
    hasFN <- !is.na(study$FN)
    hasTN <- !is.na(study$TN)
    hasP <- !is.na(study$P)
    hasN <- !is.na(study$N)
    hasTPR <- !is.na(study$TPR)
    hasTNR <- !is.na(study$TNR)
    hasFPR <- !is.na(study$FPR)
    hasFNR <- !is.na(study$FNR)
    hasPPV <- !is.na(study$PPV)
    hasNPV <- !is.na(study$NPV)
    hasACC <- !is.na(study$ACC)
    hasMCR <- !is.na(study$MCR)
    hasF1 <- !is.na(study$F1)
    hasCK <- !is.na(study$CK)
    hasMCC <- !is.na(study$MCC)
    study <- removeNaColumns(inputs)
    k <- 1
    cat(paste0('[', i, '/', count_survey, '] ', study$Study, '#', study$Config,
              ': '))

    if (hasTP && hasFP && hasFN && hasTN) {
      m <- getMeasuresViaBaseMeasures(study$TP, study$FP, study$FN, study$TN)
      addStudyBaseMeasures(m, study$Study, study$Config, inputs,
                           'BM', k, add_inconsistent_BMs)
      cat(paste(k, ': via Base Measures. '))
      k <- k + 1
    }
    
    if (hasP && hasTPR && hasFPR && hasACC) {
      m <- getMeasuresViaP_TPR_FPR_ACC(study$P, study$TPR, study$FPR, study$ACC)
      addStudyBaseMeasures(m, study$Study, study$Config, inputs,
                           'P,TPR,FPR,ACC', k, add_inconsistent_BMs)
      cat(paste(k, ': via P,TPR,FPR,ACC. '))
      k <- k + 1
    }
    
    if (hasP && hasN && hasTPR && hasACC) {
      m <- getMeasuresViaP_N_TPR_ACC(study$P, study$N, study$TPR, study$ACC)
      addStudyBaseMeasures(m, study$Study, study$Config, inputs,
                           'P,N,TPR,ACC', k, add_inconsistent_BMs)
      cat(paste(k, ': via P,N,TPR,ACC. '))
      k <- k + 1
    }
    
    if (hasP && hasN && hasTNR && hasACC) {
      m <- getMeasuresViaP_N_TNR_ACC(study$P, study$N, study$TNR, study$ACC)
      addStudyBaseMeasures(m, study$Study, study$Config, inputs,
                           'P,N,TNR,ACC', k, add_inconsistent_BMs)
      cat(paste(k, ': via P,N,TNR,ACC. '))
      k <- k + 1
    }
    
    if (hasP && hasN && hasTPR && hasPPV) {
      m <- getMeasuresViaP_N_TPR_PPV(study$P, study$N, study$TPR, study$PPV)
      addStudyBaseMeasures(m, study$Study, study$Config, inputs,
                           'P,N,TPR,PPV', k, add_inconsistent_BMs)
      cat(paste(k, ': via P,N,TPR,PPV. '))
      k <- k + 1
    }
    
    if (hasP && hasN && hasTPR && hasFPR) {
      m <- getMeasuresViaP_N_TPR_FPR(study$P, study$N, study$TPR, study$FPR)
      addStudyBaseMeasures(m, study$Study, study$Config, inputs,
                           'P,N,TPR,FPR', k, add_inconsistent_BMs)
      cat(paste(k, ': via P,N,TPR,FPR '))
      k <- k + 1
    }
    
    if (hasP && hasN && hasTPR && hasTNR) {
      m <- getMeasuresViaP_N_TPR_TNR(study$P, study$N, study$TPR, study$TNR)
      addStudyBaseMeasures(m, study$Study, study$Config, inputs,
                           'P,N,TPR,TNR', k, add_inconsistent_BMs)
      cat(paste(k, ': via P,N,TPR,TNR. '))
      k <- k + 1
    }
    
    if (hasP && hasN && hasACC && hasF1) {
      m <- getMeasuresViaP_N_ACC_F1(study$P, study$N, study$ACC, study$F1)
      addStudyBaseMeasures(m, study$Study, study$Config, inputs,
                           'P,N,ACC,F1', k, add_inconsistent_BMs)
      cat(paste(k, ': via P,N,ACC,F1. '))
      k <- k + 1
    }
    
    if (hasP && hasN && hasFPR && hasFNR) {
      m <- getMeasuresViaP_N_FPR_FNR(study$P, study$N, study$FPR, study$FNR)
      addStudyBaseMeasures(m, study$Study, study$Config, inputs,
                           'P,N,FPR,FNR', k, add_inconsistent_BMs)
      cat(paste(k, ': via P,N,FPR,FNR. '))
      k <- k + 1
    }
    
    if (hasP && hasN && hasFNR && hasACC) {
      m <- getMeasuresViaP_N_FNR_ACC(study$P, study$N, study$FNR, study$ACC)
      addStudyBaseMeasures(m, study$Study, study$Config, inputs,
                           'P,N,FNR,ACC', k, add_inconsistent_BMs)
      cat(paste(k, ': via P,N,FNR,ACC. '))
      k <- k + 1
    }
    
    if (hasP && hasN && hasFPR && hasACC) {
      m <- getMeasuresViaP_N_FPR_ACC(study$P, study$N, study$FPR, study$ACC)
      addStudyBaseMeasures(m, study$Study, study$Config, inputs,
                           'P,N,FPR,ACC', k, add_inconsistent_BMs)
      cat(paste(k, ': via P,N,FPR,ACC. '))
      k <- k + 1
    }
    
    if (k == 1) {
      cat(paste('Unknown combinations: ',
                paste(colnames(study), collapse=','), '\n'))
    }
    else {
      cat('[done]\n')
    }
  }
  
  if (add_inconsistent_BMs == FALSE) {
    count_inconsistencies <- length(LogInconsistencies)
    
    if (count_inconsistencies == 0) {
      cat('No inconsistencies.\n')
    }
    else {
      cat(paste0('\n', count_inconsistencies, ' inconsistencies:\n'))
      cat(paste(paste('study', 'config', 'possibility', 'instrument_combination',
                'consistency', 'delta_Sn', sep='; ')), '\n')
      cat(paste(LogInconsistencies, collapse='\n'))
    }
  }
  
  return (data.frame(Study, Config, Inputs,
                     ReportedMetrics, ReportedMetricValues,
                     MinReportedMetricValues,
                     AverageReportedMetricValues, MaxReportedMetricValues,
                     Possibilities, InstrumentCombination,
                     fTP, fFP, fFN, fTN, fSn, TP, FP, FN, TN, Sn,
                     InputSn=Inputs$inputN+Inputs$inputP,
                     Consistency, DeltaSn))
}
