# TasKar – Binary Classification Performance Dashboard/Calculator
### A Research Compedium of "New Means of Calculation and Representation of The Entire Binary-Classification Performance Instruments"
#### Submitted to IEEE Computation Intelligence Magazine (CIM)
* ![TasKar.ods](TasKar.ods) – Binary-Classification Performance Instruments Calculator/Dashboard/Visualizer\*
* with novel ![TasKarMissing.R](TasKarMissing.R) Missing Metrics Calculator (R Script to Reveal Confusion Matrix and then all the other metrics from a few given measures/metrics)

![](images/TasKarDashboard_Demo.gif)

\* Best viewed with [LibreOffice](https://www.libreoffice.org/download/libreoffice-fresh/).

Classification performance, which is very important in machine learning, states the success state of a classifier implementing a specific algorithm. Binary classifications or two-class classifications separate a given input into two opposite classes such as 'presence' vs. 'absence' of a disease (healthy) or a condition, 'spam' vs. 'non-spam' for an e-mail, and 'malign' vs. 'benign' software.

TasKar is a handy visually supported compact calculator to see the performance of a given binary classification by entering only the True Positives (***TP***), True Negatives (***TN***), False Positives (***FP***), and False Negatives (***FN***). It is based on an academical study given in the notes below.

Just input the four measures (***TP***, ***TN***, ***FP***, ***FN***) and TasKar calculates the entire performance measures and metrics.

TasKar calculates and shows 56 measures/metrics including True Positive Rate (*TPR*), Accuracy (*ACC*), Balanced Accuracy (*BACC*), *G*, *F1*, Normalized Mutual Information (*nMI*), Cohen's Kappa (*CK*), and Mathews Correlation Coefficient (*MCC*).

I hope that TasKar becomes a standard tool for calculating and expressing binary classification performances as well as using in machine learning education and trainings.

Please, contact and/or inform me for further usage on TasKar.

Gürol Canbek, October 13th, 2018

## Abstract
This study takes a close look at binary-classification performance instruments that are based on the four basic values in a confusion matrix, namely true positives/negatives and false positives/negatives and enhances their representation by establishing a well-grounded interpretation of the performance evaluation fundamentals. We proposed a common textual formatting scheme to improve the readability and comprehensibility of performance instruments’ representation. A compact dashboard is provided online to calculate and visualize total 56 performance instruments (26 measures, 30 metrics) by entering confusion matrix for the first time. Seeing the limited approaches in confusion matrix visualization, we also devised three new graphics to visualize true/false positive/negative rates, positive/negative predictive values, and false discovery/omission rates performance metrics. Lastly, we have introduced some equations to reveal the original confusion matrix of a classification study that reported a few metrics. We used the equations to reveal and evaluate the entire performance of the 43 academic studies (2012–2018) in Android malware classification. Calculating the performance in terms of other metrics might detect the confirmation/publication biases. We expect that the proposed methods and tools will assist the researchers in computation, interpretation, and representation of classification performance.

## Keywords
Classification, performance metrics, confusion matrix, visualization, publication bias

**NOTES**:
1) TasKar is prepared as a OpenDocument Spreadsheet file and tested with LibreOffice Calc (v2.3).
2) Please, cite our study if you use TasKar in your academic studies as
   Gürol Canbek, Seref Sagiroglu, and Tugba Taskaya Temizel, *New Means of Calculation and Representation of The Entire Binary-Classification Performance Instruments*, IEEE Computational Intelligence Magazine (CIM), (Submitted), November 2018, https://github.com/gurol/taskar
3) Please, check this repository for updates and the references.
4) See also: G. Canbek, S. Sagiroglu, T. T. Temizel, and N. Baykal, “Binary classification performance measures/metrics: A comprehensive visualized roadmap to gain new insights,” in *2017 International Conference on Computer Science and Engineering (UBMK)*, Antalya: IEEE, 2017, pp. 821–826. [Full Text](https://www.researchgate.net/publication/320829355_Binary_Classification_Performance_MeasuresMetrics_A_comprehensive_visualized_roadmap_to_gain_new_insights)
