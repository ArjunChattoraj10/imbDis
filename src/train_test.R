# Creating Train/test split

sim_dat = read.csv("../data/simulated_data_arjun.csv")

set.seed(343)
train_i = sample(5000, 3500)

train_dat = sim_dat[train_i,]
test_dat = sim_dat[-train_i,]