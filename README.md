# TasKar – Binary-Classification Performance Dashboard/Calculator
[![Last-changedate](https://img.shields.io/badge/last%20change-2021--11--13-brightgreen.svg)](https://github.com/gurol/TasKar) [![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)  [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0002--9337--097X-green.svg)](https://orcid.org/0000-0002-9337-097X)
### A Research Compedium of "TasKar: A research and education tool for calculation and representation of binary classification performance instruments"
#### IEEE 14th International Conference on Information Security and Cryptology, 2-3 December 2021
* ![TasKar.ods](TasKar.ods) – Binary-Classification Performance Instruments Calculator/Dashboard/Visualizer\*
* with novel ![TasKarMissing.R](TasKarMissing.R) Missing Metrics Calculator (R Script to Reveal Confusion Matrix and then all the other metrics from a few given measures/metrics)

https://user-images.githubusercontent.com/14318443/142153110-7a0a22e9-5a60-430f-8963-61a230719b88.mp4

* Best viewed with free [Libre Office](https://www.libreoffice.org/download/download/) or [Apache Open Office](https://www.openoffice.org/download/index.html).

Classification performance, which is very important in machine learning, states the success state of a classifier implementing a specific algorithm. Binary classifications or two-class classifications separate a given input into two opposite classes such as 'presence' vs. 'absence' of a disease (healthy) or a condition, 'spam' vs. 'non-spam' for an e-mail, and 'malign' vs. 'benign' software.

TasKar is a handy visually supported compact calculator to see the performance of a given binary classification by entering only the True Positives (***TP***), True Negatives (***TN***), False Positives (***FP***), and False Negatives (***FN***). It is based on an academical study given in the notes below.

Just input the four measures (***TP***, ***TN***, ***FP***, ***FN***) and TasKar calculates the entire performance measures and metrics.

TasKar calculates and shows the ultimate set of (65) measures/metrics including True Positive Rate (*TPR*), Accuracy (*ACC*), Balanced Accuracy (*BACC*), *G*, *F1*, Normalized Mutual Information (*nMI*), Cohen's Kappa (*CK*), and Mathews Correlation Coefficient (*MCC*). Three new graphics were devised to visualize true/false positive/negative rates (*TPR*, *FPR*, *TNR*, *FNR*), positive/negative predictive values (*PPV*, *NPV*), and false discovery/omission rates (*FDR*, *FOR*) performance metrics.

I hope that TasKar becomes a handy tool for calculating and expressing binary classification performances as well as using in machine learning education and trainings.

Please, contact and/or inform me for further usage on TasKar.

Gürol Canbek, October 13th, 2018 - December 2nd, 2021

## Abstract
This study covers almost the ultimate set of binary-classification performance instruments derived from four dimensions of a confusion matrix, namely true positives/negatives and false positives/negatives and enhances their representation by establishing a meaningful interpretation of the dimensions. A common textual formatting scheme is provided to improve the readability and comprehensibility of performance instruments’ representation. A compact dashboard (named ‘TasKar’, the abbreviation of ‘Tasnif Karnesi’, ‘Classification Report’ in Turkish) is developed and provided online to calculate and visualize a total of 52 performance instruments (27 measures, 23 metrics, and 2 indicators) by entering confusion matrix elements only. Taking parametric, variant, and recently proposed instruments the number covered becomes 65. Despite the limited approaches in confusion matrix visualization in the literature, three new graphics were devised to visualize true/false positive/negative rates (*TPR*, *FPR*, *TNR*, *FNR*), positive/negative predictive values (*PPV*, *NPV*), and false discovery/omission rates (*FDR*, *FOR*) performance metrics. It is expected that the proposed method and tool will be used by researchers in computation, interpretation, and standardized representation of classification performance as well as by teachers and students in machine learning education.

## Keywords
binary classification, performance evaluation, performance metrics, confusion matrix, visualization, publication bias

**NOTES**:
1) TasKar is prepared as a OpenDocument Spreadsheet file and tested with LibreOffice Calc (v2.7).
2) Please, cite our study if you use TasKar in your academic studies as
   Gürol Canbek, Tugba Taskaya Temizel, and Seref Sagiroglu, *TasKar: A research and education tool for calculation and representation of binary classification performance instruments*, IEEE 14th International Conference on Information Security and Cryptology (ISCTurkey), 2-3 December 2021, Ankara.
3) Please, check this repository for updates and the references.
4) Gürol Canbek, Tugba Taskaya Temizel, and Seref Sagiroglu (2021). BenchMetrics: A systematic benchmarking method for binary-classification performance metrics. *Neural Computing and Applications*, 33(21), 14623–14650. https://doi.org/10.1007/s00521-021-06103-6
