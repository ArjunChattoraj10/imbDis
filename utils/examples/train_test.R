# Creating Train/test split

# working directory is repo base
sim_dat = read.csv("utils/data/simulated_data.csv")

set.seed(343)
train_i = sample(5000, 3500)

train_dat = sim_dat[train_i,]
test_dat = sim_dat[-train_i,]
