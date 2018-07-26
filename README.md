# Text Modeling for Content Validity Evidence

This is a repository for some initial work on using text modeling as an
additional source of content-related validity evidence for tests. The general
idea is to train a model on a set of content standards, and then use that model
to predict which topic (among the set of topics in the standards estimated from
the trained model) each item "belongs" to. Interestingly, in some cases the 
item includes little to no language used in the standards, and so the
probability is equally divided among all topics. In this case, then, the
content-validity evidence is called into question. Expert judgments will 
likely always weigh more heavily in the evidential basis for supporting
content-related validity, but text-modeling does have some nice features that
could either support validity evidence or screen items that may need to be
further evaluated.