###########################################
##Code by Rollanz written 2019-11-23
##comb_iter() allows iteration through possible orderings of successes and failures
##where numbers of each is kept fixed
###########################################

#set.seed(100)

affs_total <- 7
affs_needed <- 5

probs <- runif(affs_total)
#probs <- c(0,0,1,1,1,1,1) #test

total_prob = 0

comb_iter <- function(state){
	length_state <- length(state)
	read0 <- 0 #no 0s to the right
	read1 <- 0 #no 1s between 0 to be moved and 0s to the right
	for(i in length_state:2){
		if(state[i]==T){
			read1 <- read1 + 1
		}else if(state[i]==F){
			if(state[i-1]==F){
				read0 <- read0 + 1
			}else if(state[i-1]==T){
				state[i] <- T
				state[i-1] <- F
				if(read0 > 0){
					state[(i+read1+1):length_state] <- F
				}
				if(read1 > 0){
					state[(i+1):(i+read1)] <- T
				}
				return(state)
			}
		}
	}
	return(FALSE)
}

for(i in affs_needed:affs_total){
	affs_success <- i
	affs_fail <- affs_total - i
	if (affs_fail == 0){
		total_prob <- total_prob + prod(probs)
	}else{
		state_vec <- c(rep(1,affs_success), rep(0, affs_fail))
		while(!identical(state_vec, FALSE)){
			#print(state_vec)
			prob_outcome <- prod((state_vec * probs) + ((!state_vec) * (1-probs)))
			#print(prob_outcome)
			total_prob <- total_prob + prob_outcome
			state_vec <- comb_iter(state_vec)
		}
	}
}

#compare with total_prob obtained from a simulation
num_runs <- 10^5
num_success <- 0

for(i in 1:num_runs){
	rolls <- runif(affs_total)
	if(sum(rolls < probs) >= affs_needed)
		num_success <- num_success + 1
}

total_prob
num_success/num_runs