# jeva
A jamovi module for log likelihood ratio analyses of common statistical tests

The likelihood approach is one of several approaches to making
inferences from data. The best description and justification for the
approach is given by [Edwards, A.W.F. (1992) Likelihood, Johns Hopkins
Press](https://www.amazon.co.uk/Likelihood-W-F-Edwards/dp/0801844436/). Others, such as [R. Royall (1997)](https://www.amazon.co.uk/Statistical-Evidence-Likelihood-Monographs-Probability/dp/0412044110/), [S. Goodman (1988)](https://ajph.aphapublications.org/doi/abs/10.2105/AJPH.78.12.1568), [Z. Dienes (2008)](https://www.amazon.co.uk/Understanding-Psychology-Science-Introduction-Statistical/dp/023054231X/), [S. Glover & P. Dixon (2004)](https://link.springer.com/article/10.3758/BF03196706) have subsequently made important contributions. More recently useful contributions are by [Dennis et al (2019](https://www.frontiersin.org/articles/10.3389/fevo.2019.00372/full), and [Taper et al (2022)](https://www.frontiersin.org/articles/10.3389/fevo.2022.883456/full) 

The likelihood approach focusses on the observed data, using maximum
likelihood for estimates, and calculates likelihood ratios for specific
parameter values given the collected data. The log of the likelihood
is known as the *support*. When comparing two parameter values (as might be suggested by different hypotheses), the difference
in log likelihoods represents the ratio of their likelihoods. Using support has distinct advantages. It 
represents the weight of evidence with a scale that ranges
from positive to negative (indicating support for or against a hypothetical 
parameter value). Support values obtained from independent studies
can simply be added together to give their combined support. Unlike *p* values, 
support values are insensitive to transforms.

##		S value -- Interpretation of *H*~1~ vs H2
	0	No evidence either way
	1	Weak evidence
	2	Moderate evidence
	3	Strong evidence
	4	Extremely strong evidence
**Table 1:** _Interpretation for values of S, the support, calculated as the natural logarithm of the likelihood
ratio. Negative values would represent support for hypothesis values H2 vs H1. Typically, it is
sufficient to give S to one decimal place._

Support values give the relative strength of evidence for one hypothesis 
value versus another, see Table 1. They range from negative infinity to positive infinity. 
There is no specific threshold value (unlike 0.05 in frequentist testing), and
values can be rounded to the nearest 1 decimal place, e.g. -2.161 -> -2.2.
A support of 0 shows no support for either hypothesis, while a support of 1 
represents weak evidence for the first hypothesis versus the second. 
A support of 2 represents moderate evidence, and is roughly equivalent to the 
5% significance level in the frequentist approach. A support of 3 represents
strong evidence and 4 represents extremely strong evidence, and so on. 
Negative values represent the same strength of evidence, except for the 
second hypothesis value versus the first. Support values for a given likelihood
function can be compared with each other simply by subtracting them. In the 
example below, the support for d versus the 2nd hypothesis can be obtained 
by subtracting the support for the 2nd hypothesis versus null (4.115) from the support
for d versus the null (2.191) giving -1.924, as listed in the 3rd line of output.
The support for d versus the observed mean would be 2.191 - 4.352 = -2.161, negative
as expected since d lower on the likelihood function curve from the maximum
represented by the observed mean (MLE). 

The likelihood interval can be obtained for many statistics. This represents 
the range of values that are consistent with the observed data for a given level
of support. For example, the S-2 likelihood interval includes all values which are 
not more different from the MLE by more than a support
value of 2. As noted for the figure below, the S-2 interval is numerically closely equivalent 
to the 95% confidence interval, as the support of 2 is similar to the 5% significance 
level.
An S-3 likelihood interval would include all values which are not more different 
from the MLE by more than a support value of 3, and so on. The stronger 
the interval the wider it will be. The interpretation of the likelihood interval 
is distinct from a confidence interval.
The confidence interval represents the long run probability of capturing the
population parameter and may need to be corrected for multiple testing, stopping
rule, etc. The likelihood interval is also distinct from the Bayesian credibility
interval that represents the subjective probability for a population value occurring
within it.

There are few statistical packages that implement the likelihood
approach and which calculate support. This jamovi module calculates support for a range of statistical
analyses: *t* tests, ANOVA, regression, correlation and categorical analyses. 

The analyses in the module complement my recent book: [Cahusac, P.M.B.
(2020) Evidence-Based Statistics, Wiley](https://onlinelibrary.wiley.com/doi/book/10.1002/9781119549833)  
[Amazon.co.uk](https://www.amazon.co.uk/Evidence-Based-Statistics-Introduction-Evidential-Statistical/dp/1119549809/)  

In jamovi the **.jmo** file can be sideloaded, and the module will appear among the other modules.
Example jamovi analyses with data from the book are given in the **Test Data Analyses** folder.

I would be interested in feedback <pcahusac@alfaisal.edu>          Peter Cahusac  

Below is a sample screenshot from jamovi showing the acquisition and results for an odds ratio analysis of the MRC study (1991) of neural tube
defects in babies born to mothers receiving either folic acid or placebo (double-blind randomized study), see pp 146 - 151 in Cahusac (2020) book.
<figure>
<img src="https://github.com/PeterC-alfaisal/Likelihood/blob/44eb93703f0db88530283526be9b8cc52fe5077c/jam_screen.jpg" id="id" class="class" style="width:60.0%;height:60.0%" />
</figure>
