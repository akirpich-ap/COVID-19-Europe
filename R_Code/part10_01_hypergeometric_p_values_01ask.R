# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.08.13. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
options(scipen = 20)

# x, q	
# vector of quantiles representing the number of white balls drawn without replacement from an urn which contains both black and white balls.
# 
# m	
# the number of white balls in the urn.
# 
# n	
# the number of black balls in the urn.
# 
# k	  
# the number of balls drawn from the urn, hence must 
# 
# p	
# probability, it must be between 0 and 1.
# 
# nn	
# number of observations. If length(nn) > 1, the length is taken to be the number required.
# 
# log, log.p	
# logical; if TRUE, probabilities p are given as log(p).
# 
# lower.tail	
# logical; if TRUE (default), probabilities are 

# Here Lithuania, Latvia and Estonia are counted as ex-Soviet countries.

x_value <- 5 # Number of selected from m  
m_value <- 9 # No of post soviet countries including Lithuania, Latvia and Estonia.
n_value <- 44 - m_value
k_value <- 10

# phyper(q = q_value, m = m_value, n = n_value, k = k_value, lower.tail = FALSE, log.p = FALSE)

dhyper(x = x_value, m = m_value, n = n_value, k = k_value, log = FALSE)
phyper(q = x_value-1, m = m_value, n = n_value, k = k_value, lower.tail = FALSE, log.p = FALSE)


# Here Lithuania, Latvia and Estonia are NOT counted as ex-Soviet countries.

x_value <- 5 # Number of selected from m  
m_value <- 6 # No of post soviet countries
n_value <- 44 - m_value
k_value <- 10

# phyper(q = q_value, m = m_value, n = n_value, k = k_value, lower.tail = FALSE, log.p = FALSE)

dhyper(x = x_value, m = m_value, n = n_value, k = k_value, log = FALSE)
phyper(q = x_value-1, m = m_value, n = n_value, k = k_value, lower.tail = FALSE, log.p = FALSE)




# Here 9 out of 24 Eastern European countries.

x_value <- 9  # Number of selected from m  
m_value <- 24 # No of Eastern European Countries.
n_value <- 44 - m_value
k_value <- 10

# phyper(q = q_value, m = m_value, n = n_value, k = k_value, lower.tail = FALSE, log.p = FALSE)

dhyper(x = x_value, m = m_value, n = n_value, k = k_value, log = FALSE)
phyper(q = x_value-1, m = m_value, n = n_value, k = k_value, lower.tail = FALSE, log.p = FALSE)




# Here 3 out of 9 former Soviet countries.

x_value <- 3  # Number of selected from m  
m_value <- 9 # No of Former Soviet Countries.
n_value <- 44 - m_value
k_value <- 7

# phyper(q = q_value, m = m_value, n = n_value, k = k_value, lower.tail = FALSE, log.p = FALSE)

dhyper(x = x_value, m = m_value, n = n_value, k = k_value, log = FALSE)
phyper(q = x_value-1, m = m_value, n = n_value, k = k_value, lower.tail = FALSE, log.p = FALSE)



# Here 4 (Iceland, Finland, Norway and Denmark) out of 5 (+ Sweden) Nordic countries.
ss
x_value <- 4  # Number of selected from m  
m_value <- 5 # No of Eastern European Countries.
n_value <- 44 - m_value
k_value <- 7

# phyper(q = q_value, m = m_value, n = n_value, k = k_value, lower.tail = FALSE, log.p = FALSE)

dhyper(x = x_value, m = m_value, n = n_value, k = k_value, log = FALSE)
phyper(q = x_value-1, m = m_value, n = n_value, k = k_value, lower.tail = FALSE, log.p = FALSE)




