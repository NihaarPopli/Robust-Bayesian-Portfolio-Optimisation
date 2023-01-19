# Robust Bayesian Portfolio Optimisation
Robust Bayesian Portfolio Optimisation:
Sensitivity Analysis and Estimation Algorithms

Author: Nihaar Popli, supervised by Dr. Nikolas Kantas

Robust portfolio optimisation involves finding the optimal allocation of assets while taking into account
the uncertainty in input market parameters. In this paper, we set up a Bayesian inference framework and
treat the market parameters as random variables. The posterior distributions of the market parameters
are central in determining the robust Bayesian optimal allocation. We then investigate the local sensitivity
of a conjugate prior market model using the Kullback-Leibler divergence and PAC-Bayes bounds.
This paper also extends the robust portfolio optimisation framework to intractable posterior market models.
We focus on variational inference as a posterior estimation algorithm for the Bayesian portfolio
allocation problem. We compare the computational and accuracy benefits of variational inference to conventional
Markov chain Monte Carlo methods, through theory, simulations and case studies. Our analyses
and model implementations are carried out primarily in the Stan probabilistic programming environment.
