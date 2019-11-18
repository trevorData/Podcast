# [Main Plot](https://i.imgur.com/NPmqiqc.jpg)

### Forecasting The Top Patreon Podcasts in 2020

**Plots:**

Thin lines on the main plot represent the 200 day forecasts. Grey areas on the individual plots represent the 95% and 80% confidence intervals.

[Not Another D&D Podcast](https://i.imgur.com/NPmqiqc.jpg)

[Last Podcast On The Left](https://i.imgur.com/AlKKQJn.jpg)

[Tiny Meat Gang](https://i.imgur.com/WbeCmwz.jpg)

[Doughboys](https://i.imgur.com/SOozR34.jpg)

[The Cum Boys](https://i.imgur.com/Qzk8cRd.jpg)

[Tell 'em Steve Dave](https://i.imgur.com/5EJQAIL.jpg)

[Mueller She Wrote](https://i.imgur.com/aHf3iNl.jpg)

[Second Captains](https://i.imgur.com/SjYMOiu.jpg)

[True Crime Obsessed](https://i.imgur.com/BvcKVDG.jpg)

[Chapo Trap House](https://i.imgur.com/uNvWGjB.jpg)

---

**Methodology:**

I scraped daily subscriber data for the top 10 Patreon podcasts, and forecasted their subscribers for the next 200 days using the [ARIMA](https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average) model.

ARIMA models forecast time-series data by smoothing out the series with a moving average and also regressing the data on its own previous values.

The model I used here is a Seasonal-ARIMA model which accounts for the regular movement of the data in each "season".

In this case, a season in one month, because subscriber counts take a dip at the beginning of each month when cancellations are processed.

---

**Results:**

At the time of this data being recorded, the top 10 Patreon podcasts are:

\#| Podcast| Subscribers
---|---|---
|1. |Chapo Trap House |30099
|2. |Last Podcast On The Left | 11624
|3. |Second Captains| 11388
|4.| True Crime Obsessed|12622
|5.| The Cum Boys| 10697
|6.| Tiny Meat Gang| 10854
|7.| Not Another D&D Podcast |8992
|8. | Doughboys| 7683
|9.|Mueller She Wrote | 7300
|10. |Tell 'em Steve Dave| 6597

The forecasts predict that after 200 days, the order of those 10 will switch to:

\#| Podcast| Subscribers
---|---|---
|1. | Chapo Trap House | 33621.30
|5. | Last Podcast On The Left | 13221.82
|7. | Second Captains| 11556.08
|2.| True Crime Obsessed| 23166.87
|6.| The Cum Boys| 11898.09
|3.| Tiny Meat Gang| 15305.11
|4.| Not Another D&D Podcast | 13828.469
|8. | Doughboys| 8628.263
|10.| Mueller She Wrote | 5782.763
|9. | Tell 'em Steve Dave| 6910.083

---
  
Techniques taken from [Robert Nau's notes on regression and time series analysis](https://people.duke.edu/~rnau/411home.htm)
