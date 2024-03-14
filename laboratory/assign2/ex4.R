# probability that a student knows the answer
p <- 0.7

# function to simulate
# n_student: number of students for the simulation
simul <- function(n_students){
	student <- c(rep("K", n_students * p), rep("R", n_students * (1 - p)))
	ans <- rep(0, n_students)

	for(i in 1:n_students){
		if(student[i] == "K"){
			ans[i] <- 0				
			}
		else{
			ans[i] <- round(runif(n=1, min=1, max=5), 0)
			}  
		}
	return(ans)
}

# 0 -> student that knows the answer
# 1 -> student that picks randomly the right answer 
df <- as.data.frame(table(simul(1000)))
df

probability <- df$Freq[1] / (df$Freq[1] + df$Freq[2])

cat(paste("The probability that the student really knew the correct answer, once a correct answer is given is: ", probability * 100, "%", '\n'))