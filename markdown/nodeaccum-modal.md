This plot shows node-level errors, and their accumulation to produce reach-level errors. Each point corresponds to a node in the selected reach, ordered upstream to downstream. 

The **top plot** shows individual node errors. If "Standardize" is checked, then these errors are scaled by the estimated uncertainty, i.e. $\epsilon_i / \sigma_i$.

The **bottom plot** shows cumulative node errors across the reach, so that the leftmost point is identical to the leftmost point in the top plot, and the rightmost point is identical to the reach-level error. If "Standardize" is unchecked, then this is the cumulative sum of raw errors, i.e.


$$
\text{(cumulative error)}_i = \sum_{j = 1}^i\epsilon_j
$$


If "Standardize" is checked, then this is the cumulative sum of errors divided by the cumulative standard deviation (uncertainty estimate), i.e.


$$
\text{(standardized cumulative error)}_i = \frac{\sum_{j = 1}^i \epsilon_j}{\sqrt{\sum_{j = 1}^i \sigma_j^2}}
$$
