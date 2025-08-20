# Confidence Curves R Shiny App

This app is a front-end interactive tool to familiarise oneself with the `confidenceCurve` package available [here](https://CRAN.R-project.org/package=confidenceCurves) from CRAN.

## Confidence Analysis for Clinical Trials
This app demonstrates the ability to express frequentist confidence in *any treatment effect of interest*. Here is a demonstration.

1.   Enter your point estimate (in the example, odds ratio):

       <img width="355" height="468" alt="image" src="https://github.com/user-attachments/assets/51990794-cdb5-4c60-8602-2afec0668ea5" />

2.   Enter your precision estimate (here a 95% confidence interval):

      <img width="346" height="331" alt="image" src="https://github.com/user-attachments/assets/d3885432-8166-4eb4-b698-077645653d98" />

3.   Specfify two treatment effects of interest. Designate whether you want to know confidence that the treatment effect is about this, below this, and/or between the two.

      In this example, I am interested in confidence that the odds ratio is less than one, i.e. treatment benefit, and that the odds ratio is more than 0.95 i.e. lacks meaningful benefit.

      <img width="345" height="450" alt="image" src="https://github.com/user-attachments/assets/acf3c4b3-779a-4c0b-ad1c-6f603d01f435" />

4.    Specify which threshold (1 or 2) should be displayed on the Confidence Density curve (i.e. which region is colour in under the curve).

5.    Press "Generate Confidence Curves".
  
## Basic Theory

For more information see the [README](https://cran.r-project.org/web/packages/confidenceCurves/readme/README.html) from the `confidenceCurves` package.

### What is a confidence distribution?
The confidence distribution is constructed by stacking every one-sided confidence interval (from 0-100) vertically to generate a cumulative distribution function. Figure 1 shows an example from real trial data where a point estimate of -0.22 (log odds ratio) was observed. The treatment effect in this example represents the *odds of a patient in the treatment group doing poorly over the odds of a patient in the control group doing poorly*. On this scale, smaller values are better. Values less than zero represented benefit of a treatment to the patient (lying in a *Region of Benefit*). We see the stack of one-sided intervals, including the 99.89% confidence interval which intersects with zero - zero being the boundary of the Region of Benefit. So, any values within the 99.89% confidence interval represent benefit to the patient.

<figure>
  <p align="center">
      <img width="560" height="450" alt="confidence_distribution_INTERACT_annotated" src="https://github.com/user-attachments/assets/41d0d671-7832-439d-8cfd-b6d89d571bea" title="Example Confidence Distribution Function"/>
    <figcaption>Figure 1. Confidence distribution function from observed data that has a point estimate of -0.22 (a log odds ratio) made up of all one-sided confidence intervals, including the 5%, 25%, 50%, 75% 95% and 99.89% CI. The red dashed line represents the observed effect, and the blue line represents the line of no effect. Any treatment effect values less than zero represent benefit of treatment to the patient, and are in the <i>region of benefit</i>. The 99.89% CI covers the region of benefit. </figcaption>
  </p>
</figure>

### Why would we do this?
By constructing this distribution, we have the flexibility to reason about a variety of treatment effects. For example, our treatment effect of interest in this example is "treatment benefit". Given that the 99.98% confidence interval contains all values that represent treatment benefit, we may say: ***We are 99.89% confident that the treatment has benefit***.

### Flexible thresholds
We do not have to limit ourselves to statments about benefit. Say we are interested in an amount of benefit that we believe is actually meaningful: It is not enough that the magnitude of the effect is greater than zero, it had to be big enough to be clinically meaningful. That is possible! With confidenceCurves we can also specify a *meaningful benefit*. Figure 2 shows the same confidence distribution as in Figure 1, but this time we are interested in a different confidence interval: The interval covering the region of **no meaningful benefit**. In this case, the confidence interval covering the region of meaningful benefit is the 99.1% confidence interval, so the confidence interval for *lack* of meaningful benefit is $1-0.991 = 0.009$. Therefore, we may say: ***We are 1% confidence that the treatment lacks meaningful benefit***. If we were using the confidence in a lack of meaningful benefit or $LMB$ to *monitor for futility* in an adaptive trial, we would not stop the trial early on the basis of this confidence value.

<figure>
 <p align="center">
<img width="560" height="450" alt="confidence_lob_interact_annotated_github" src="https://github.com/user-attachments/assets/508e5ec8-ac27-4b93-a46a-349dfdb0238d" />
  <figcaption>
   Figure 2. Confidence distribution from observed data that has a point estimate of -0.22 (a log odds ratio) made up of all one-sided confidence intervals, including the 5%, 25%, 50%, 75% 95% and 99.1% CIs. The red dashed line represents the observed effect, and the green dashed line represents the smallest clinically meaningful benefit. The 99.1% CI covers the regions of meaningful benefit. Any treatment effect greater than this represents no meaningful benefit to the patient, and are in the region of <i>no meaningful benefit</i>. The 99.1% CI covers the regions of meaningful benefit.
  </figcaption>
  </p>
</figure>

### Confidence statments are valuable in clinical trials
The traditional frequentist p-value does not allow us to make such a useful statement, although we can still calculate it in this framework. Frequentist confidence statements are interpretable and intuitive and especially useful in the context of an adapative trial. During an adaptive trial, we can use frequentist confidence to express our confidence in a particular effect, and make interim decisions based on the strength of that evidence. We do not have to pay with control of Type I error, as we would if we used Bayesian posterior probability statements, and we do not have to construct a prior distribution either. Thats why these are "Posteriors without Priors".


## Reference
The confidence curves in this package are constructed using equations from Ian Marschner.

Marschner, I. "Confidence distributions for treatment effects in Clinical Trials: Posteriors without Priors", Statistics in Medicine, 2024;43:1271-1289 https://doi.org/10.1002/sim.10000.
