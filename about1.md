Welcome to podshatteR!
======================

This is a browser tool or web app for plotting and analysing pod shatter data collected from Random Impact Tests.

This should be useful to use if you are interested in how long it takes for the pods of a certain line/variety to shatter and you have followed the standard protocol for the Random Impact Tests.
This app works for data that may have repeated samples and/or used a randomised block design.

### Required data format
- There is a specific format the data needs to be in when it is uploaded to the app
- This format follows the principles of [tidy data](http://vita.had.co.nz/papers/tidy-data.pdf):
  1. Each variable forms a column
  2. Each observation forms a row
- The file must be a CSV file
- There should 6 columns with the header `Block,Number,Line,Sample,Value,Time`
- If you only have one block just create a Block column full of 1s
- If you don't have repeated samples as above just create a Sample column full of 1s
- Enter a new measurement on a new line
- Extra 0s are not needed! Adding later timepoints with a Value of 0 is not necessary for fitting my model - the final timepoint with a Value of 0 is fine.
- Below is an example of the start of an example dataset in the prescribed table format

  Block  |  Number  |  Line   |  Sample  |  Value  |  Time  
:-------:|:--------:|:-------:|:--------:|:-------:|:------:
  1      |  110     |  10722  |  1       |  20     |  0     
  1      |  179     |  10722  |  2       |  20     |  0     
  1      |  15      |  10723  |  1       |  20     |  0     
  1      |  89      |  10723  |  2       |  20     |  0     
  1      |  110     |  10722  |  1       |  18     |  8     
  1      |  179     |  10722  |  2       |  19     |  8     
  1      |  15      |  10723  |  1       |  20     |  8     
  1      |  89      |  10723  |  2       |  20     |  8     
  1      |  110     |  10722  |  1       |  16     |  16    
  1      |  179     |  10722  |  2       |  18     |  16    
  1      |  15      |  10723  |  1       |  20     |  16    
  1      |  89      |  10723  |  2       |  20     |  16    
  1      |  110     |  10722  |  1       |  11     |  24    
  1      |  179     |  10722  |  2       |  15     |  24    
  1      |  15      |  10723  |  1       |  17     |  24    
  1      |  89      |  10723  |  2       |  19     |  24    
  1      |  110     |  10722  |  1       |  7      |  32    
  1      |  179     |  10722  |  2       |  12     |  32    
  1      |  15      |  10723  |  1       |  16     |  32    
  1      |  89      |  10723  |  2       |  15     |  32    
  1      |  110     |  10722  |  1       |  6      |  40    
  1      |  179     |  10722  |  2       |  9      |  40    
  1      |  15      |  10723  |  1       |  13     |  40    
  1      |  89      |  10723  |  2       |  12     |  40    

### Model fitting assumptions
- It's assumed you have sampled at a reasonable time resolution for your pods. If your pods shatter too quickly (i.e. the first recorded timepoint of the test has a Value of 0) then the model will not fit. 
- In a similar way if you can only record two timepoints then the fit should be OK but the confidence intervals will be large.
- If pods shatter too quickly then your experimental design should be updated (e.g. smaller ball bearings or higher temporal sampling).
