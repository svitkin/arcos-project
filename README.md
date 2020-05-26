# An analysis of the Washington Post released ARCOS dataset

Resulting analysis can be found here: <a href="https://svitkin.rbind.io/2020/05/on-new-jerseys-opioid-problem/" target="_blank">https://svitkin.rbind.io/2020/05/on-new-jerseys-opioid-problem/</a>.

When generating data used in <code>analysis.R</code> and <code>visualizations.R</code>, first run: 
* <code>cache-data.R</code>
* <code>scrape-and-process-nj-hospital-visits.R</code> (be aware, this uses the curl command in dynamically created shell scripts to scrape the relevant data)
* <code>process-census-data.R</code>

And then run <code>combine-and-process-final-data.R</code>.
