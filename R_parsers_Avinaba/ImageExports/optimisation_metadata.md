
### Optimisation with R data structures

60 day runs, 30 days of contact matrices for 4530 people

R data structure | time | memory
--|--|--
data.frame | ~ 260s | ~ 231 MB
sparseMatrix | ~ 202s | ~ 116 MB
2D array | ~ 74s | ~ 5 GB

each 2D array is about ~ 165 MB

---
#### List based data structures vs Flat array in R


R data structure | coefficient | time  
--|--|--
sparseMatrix | 0.045 | 202.015
sparseMatrix | 0.025 | 365.147
array | 0.045 | 68.502
array | 0.015 | 78.817
array | 0.005 | 79.646
array | 0.002 | 74.098
array | 0.001 | 74.635
