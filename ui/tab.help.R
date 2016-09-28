tabPanel('Help', tags$h2('Help'), withMathJax(),
         'In toxicology, the median lethal dose, LD50 (abbreviation for "lethal dose, 50%"), LC50 (lethal concentration, 50%) 
         is a measure of the lethal dose of a toxin, radiation, or pathogen. The value of LD50 for a substance is the dose 
         required to kill half the members of a tested population after a specified test duration.',
         
         tags$a(href='data.xlsx', 'Sample Data File'),
         tags$h4('Citation:'),
         tags$cite(textOutput('citation')),
         tags$h4('Contact us:'),
         tags$address('Maintainer:	Khaled El-Sham\'aa <k.el-shamaa at cgiar.org>'),
         tags$h4('J. Berkson (1944), Logit function:'),
         tags$blockquote('$$Pr(x) = \\frac{1}{1+e^{-(a+b.x)}}$$'),
         tags$h4('C. I. Bliss (1934), Probit function:'),
         tags$blockquote('$$Pr(x) = \\phi{(a+b.x)}$$'),
         tags$h4('Where normal Cumulative Distribution Function (CDF):'),
         tags$blockquote('$$\\phi(x) = \\frac{1}{\\sqrt{2\\pi}} \\int_{-\\infty}^{x} e^{(-t^2/2)} dt$$'),
         tags$h4('R. A. Fisher (1922), Complementary log-log:'),
         tags$blockquote('$$Pr(x) = 1-e^{-e^{a+b.x}}$$'))