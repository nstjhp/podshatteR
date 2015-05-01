### Navigating the web app

The web app is split up into a number of sections, which can be navigated using the bar at the top.

#### Upload and plot data
- Click `Browse...` to find and upload the CSV file of your data in the required format.
- This tab will then read your data, show it back to you in a table, and plot the data with lines so you can eyeball for any mistakes in the data entry and know what you're dealing with.
- The plots show every line with repeated samples in different colours and symbols. 
If you have more than one block then the separate blocks will be shown in separate sub-tabs.

#### Fit models
- This is where the magic happens!
- You will need to wait a while because for every combination of block, line and sample the model must be fitted and confidence intervals calculated.
- A plot will be displayed showing the raw data points and a line for the fitted model with a semi-transparent ribbon showing the confidence intervals.

#### Calculate half-lives
- Click the button to calculate half-lives.
- A table will appear giving the result for each combination of variables at which half the initial pods are estimated to have shattered, along with half the number of initial pods.
- You may download a CSV file of these data.
- Plots will also appear building on the previous model fit plots with dark lines showing where this half-life point is.

#### Select plots
- In this tab you can easily select a subset of plots to display for presentations etc. by checking the boxes and clicking `Show plots`.
- If you want to select new plots just check different boxes and click `Show plots` again.
- You may also choose each panel to have the same axes or each one to be different. With the same axes the panels will all be scaled to the most-resistant line so you can easily select the ones that shatter at noticeably different times for instance.
- To allow you full control of how big the plots are adjust the `Plot height` and `Plot width` sliders. These are reactive so there is *NO* need to click on `Show plots` again. (NB The numbers represent inches for the downloaded figure.)
- Clicking `Download` will allow you download a PDF of the current figure (some things, particularly the legend size, may differ between screen display and saved plot so please check this and adjust the sliders as appropriate).

### About podshatteR

This web app aims to be an easy and useful way of visualising your data and automatically analysing it.
It is written in R using the `shiny` package for display in the browser.

This app was built using the following versions:
- `R` v3.1.2
- `shiny` v0.11.1
- `ggplot2` v1.0.0
- `grid` v3.1.2
- `reshape2` v1.4.1
- `plyr` v1.8.1
- `minpack.lm` v1.1-8
- `propagate` v1.0-4

Copyright Nick Pullen 2015

Current licence restricts its use to people I say are allowed to use it :-p

### Contact

All enquiries, feature requests, bug reports etc. can be sent to [Nick Pullen](mailto:nick.pullen@jic.ac.uk).
