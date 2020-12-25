# H1
# p(Twins|A)
p_twins.a <- 0.1
# p(Twins|B)
p_twins.b <- 0.2
# p(A) = p(B) i.e. A and B are equally common
prior <- 0.5

# p(Twins) = p(A)*p(Twins|A) + p(B)*p(Twins|B)
p_twins <- sum(prior * c(p_twins.a, p_twins.b))

# priors for calculating p(twins,twins)
# p(A|Twins) = p(Twins|A) * p(Twins,Twins|A) / p(Twins) = 0.03
p_a.twins <- prior * p_twins.a / p_twins

# p(B|Twins) = p(Twins|B) * p(Twins,Twins|B) / p(Twins)
p_b.twins <- prior * p_twins.b / p_twins

# p(Twins,Twins) = p(A|Twins) * p(Twins|A) + p(B|Twins) * p(Twins|B)
p_twins_twins <- p_a.twins * p_twins.a + p_b.twins * p_twins.b

# H2
# p(A|Twins) = p(Twins|A) * p(Twins,Twins|A) / p(Twins)
p_a.twins <- prior * p_twins.a / p_twins

# H3
# p(Twins,Singleton|A) = p(Twins|A) * p(Singleton|A)
p_twins_sing.a <- p_twins.a * (1 - p_twins.a)

# p(Twins,Singleton|B) = p(B) * p(Twins|B) * p(Singleton|B)
p_twins_sing.b <- p_twins.b * (1 - p_twins.b)

# p(Twins,Singleton) = p(Twins,Singleton|A) * p(A) + p(Twins,Singleton|B) * p(B)
p_twins_sing <- sum(prior * c(p_twins_sing.a, p_twins_sing.b))

# p(A|Twins,Singleton) = p(Twins,Singleton|A) * p(A) / p(Twins,Singleton) = 0.36
p_a.twins_sing <- p_twins_sing.a * prior / p_twins_sing

# p(B|Twins,Singleton) = p(Twins,Singleton|B) * p(B) / p(Twins,Singleton)
p_b.twins_sing <- p_twins_sing.b * prior / p_twins_sing

# H4 (a)
# p(Test_A|A)
p_test.a <- 0.8

# p(Test_B|B)
p_test.b <- 0.65

# p(TestTruePositive)
p_test <- sum(prior * c(p_test.a, p_test.b))

# p(Test_A) = p(Test_A|A) + p(Test_A|B)
p_test_a <- sum(prior * c(p_test.a, 1 - p_test.b))

# p(Test_B) = p(Test_B|B) + p(Test_B|A) = TruePositive_B + FalseNegative_A
p_test_b <- sum(prior * c(p_test.b, 1 - p_test.a))

# p(A|Test_A) = p(Test_A|A) * p(A) / p(Test_A) ~ 0.55
p_a.test <- prior * p_test.a / p_test

# p(B|Test_B) = p(Test_B|B) * p(B) / p(Test_B) ~ 0.45
p_b.test <- prior * p_test.b / p_test

# H4 (b)
prior.births_twins_sing <- c(p_a.twins_sing, p_b.twins_sing)

# Notice the prior here: it's all the ways a test can return 'A' which includes
# (a) true positives for A
# (b) false negatives for B
# p(A|Test_A) = p(Test_A|A) * p(A) / [p(Test_A|A) * p(A) + p(Test_A|B) * p(B)] = 0.5625
# where A = (A|Twins,Singleton)
p_a.births_twins_sing.test <- p_test.a * prior.births_twins_sing[1] /
  sum(prior.births_twins_sing * c(p_test.a, 1 - p_test.b))

prior_a.test <- c(p_a.test, 1 - p_a.test)
# p(A|Twins,Singleton) = p(Twins,Singleton|A) * p(A) / [p(Twins,Singleton|A) * p(A) + p(Twins,Singleton|B) * p(B)]
# where A = (A|Test_A)
p_a.test.births_twins_sing <- p_a.twins_sing * prior_a.test[1] /
  sum(prior_a.test * c(p_a.twins_sing, p_b.twins_sing))
