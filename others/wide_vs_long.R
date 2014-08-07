fhs.wide <- read.csv("data/fhs.csv")
fhs.long <- read.csv("data/frmgham2.csv")

dim(fhs.long)
dim(fhs.wide)

names(fhs.long) <- toupper(names(fhs.long))
names(fhs.wide) <- toupper(names(fhs.wide))

# The variables in the two datasets are different. 
# For example, the wide set has a BMIDIFF whereas the long data set does not.
# Moreover, the long dataset has EDUC and PERIOD whereas the wide one does not.
# The wide dataset does not seem to have the difference between the recorded 
# either (column TIME in long format)
# The wide dataset uses time in years whereas the long format uses time in days.
# It is recommended to use the long format.

# The following command converts the wide format into a long format, that we 
# can use for comparison.

out <- reshape(fhs.wide, direction="long", varying=18:74, idvar="RANDID", 
  sep="", timevar="TIME.ID")

# We can see that the patient ID 6238 has not been recorded correctly in the 
# wide format.
dim(out)
# The out matrix has more rows, because each patient has 3 rows, even if there 
# were only 1 or 2 visits.




