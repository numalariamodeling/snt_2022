## Creating input for Burkina SNT
The path is a bit of a nightmare that needs to be cleaned up but here are a series of files that you can use to create a set of priors for eight parameters per DS: `Habitat_Multiplier`, `CM_cov_2010`, `CM_cov_2014`, `CM_cov_2017`, `ITN_2010`, `ITN_2013`, `ITN_2016`, `ITN_2019`.

### CM
Use `make_data_for_cm_model.R` and `model_cm.R` to model CM coverage for each DHS year (2010, 2014 and 2017). I use no spatial covariate in the model since they are found to not have improved the likelihood. I modeled health seeking rate using Gaussian Process with INLA-SPDE approximation. Then I aggregate the pixels within a DS with population-weighted average to obtain one mean estimate of health seeking % per DS, and their upper and lower CI.

Then I use `extract_act_usage.R` to obtain national-level percentage of ACT usage. The CM coverage in our model is health seeking % times national-level ACT usage %.

Finally, I use `DS_CM_priors` to turn all the mean, lower and upper CI into priors (normal in logit scale).

### ITN
Use `net_use_ratio.R` to first obtain national-level ratio of ITN usage among U05, U10, U20 and A20 (A=Above), based on DHS data. Then use `DS_ITN_priors.R` to extract mean, upper and lower CI of ITN usage from Amelia's [work](https://malariaatlas.org/research-project/metrics-of-insecticide-treated-nets-distribution/). I use usage maps from 2010, 2013, 2016 and 2019. Once extracted, I turn them into priors (normal in logit scale).

### Finalize
Final final step is to run `stack_all_priors.R`. I specify priors for `Habitat_Multiplier` using a log-uniform prior for each archetype, and then stack it with CM and ITN priors.