# preprocess_database
Preprocess phenotypic, sonographic and biochemical database with R to improve a Quadratic Discriminant Analysis (QDA) machine learning model for identifying foetuses with Down’s trisomy in diagnostic test.
The files with the code are contained in the Preprocess_database folder (unzip the Preprocess_database rar file).
The file "Analysis.R" contains the code necessary to analyse the relationship between weight, height, BMI and the seven explicative variables with a linear model.
The seven variables are PAPP-A (pregnancy-associated plasma protein A), hCG (human chorionic gonadotropin), NT (nuchal translucency fetal),
DBP (diameter biparietal of foetus), PER_CEFAL (Cephalic perimeter), PER_ABDOM (Abdominal perimeter) and LF (Femur length of foetus).
The file "Graphs.R" contains the code necessary to build four graphs. 
The first two represent the log-linear regression of weight and logarithms of PAPP-A and hCG levels in MoM (multiples of the median). These two graphs do not represent all the values, but the median of values of some intervals in which weight has been divided.
The last two represent the log-linear regression of weight and logarithms of PAPP-A and hCG levels in MoM (multiples of the median), but in this case the levels of the proteins are divided in intervals according to the BMI.
Finally, The file "Correction.R" contains the code necessary to correct the levels of PAPP-A and hCG using the weight, height, BMI, BMI divided by weight and weight and height combined. In addition, it calculates the standard deviation before and after the correction.
