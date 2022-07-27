
### Detecting Drift in Healthcare AI Models based on Data Availability

- Code: 
- Paper: 


#### Author:

Ylenia Rotalinti: ylenia.rotalinti@gmail.com 

## :small_red_triangle_down: Brief
* We investigated methodologies to address drift detection depending on which information is available during the monitoring process.
* We explored three different aspects of drift detection: drift based on performance (when labels are available), drift based on model structure (indicating causes of drift) and drift based on change in underlying data characteristics (distribution and correlation) when labels are not available.

# Dataset
### Simulated data 
An artificial dataset was simulated from Agrawal's data generator to capture a well-known gradual drift. We generated 60K instances which were then converted into batches of 5K consecutive data points. The location of the centre of the drift (in terms of the number of instances) is 30K and the width is 10K. 
