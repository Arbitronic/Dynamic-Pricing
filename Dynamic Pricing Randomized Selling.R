# Dynamic Pricing. Randomized Selling.
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)

rm(list = ls())

# Intialization of variables 

initializiation <- function(){
  transaction <<- c()
  price_index <<- c()
  profit <<- c()
  profit_index <<- c()
  profit_margin <<- c()
  costs <<- c()
  quantity_index <<- c()
  price <<- c()
  total_rounds <<- c()
  keep_money <<- c()
  money_saved_index <<- c()
  payoffs_buyer <<- c()
  redemption_value_index <<- c()
  cost_index <<- c()
  revenue_index <<- c()
  value_per_unit_index <<- c()
  accumulated_profit <<- c()
  critical_peak_transactions <<- c()
  q_bid <<- c()
  value_by_bot <<- c()
}
initializiation() 

# This is the seller, who sells by drawing prices from a uniform distribution. 
ZI_C_Seller <- function(){
  
  upper_limit <- 200
  lower_limit <- 1
  
  
  capacity <<- sample(lower_limit:upper_limit, size =1 , replace = TRUE)
  costs <<- sample(lower_limit:(upper_limit/capacity), size = 1, replace = TRUE) # costs are determined by upper_limit up to maximum capacity
  q_supply_critical <<- capacity*(2/3)
  profit_margin <<- sample(lower_limit:(costs*0.1), size = 1, replace = TRUE)
  
  # sunk_cost <<- sample(1:1, size =1 , replace = TRUE)
  #fix_costs <<- sample(1:1, size =1 , replace = TRUE)
  
  basic_fee <<- sample(1:1, size =1 , replace = TRUE)
  upcharge <<- sample(1:5, size = 1, replace = TRUE)
  
  
  cost_plus_pricing <<- q_bid*costs + basic_fee # cost_pricing plus basic fee price 
  #critical_peak <<- q_supply_critical < q_bid #critical peak pricing 
  print(paste("costs", costs))
  print(paste("capacity", capacity))
  print(paste("q_supply_critical", q_supply_critical))
  print(paste("profit_margin", profit_margin))
  print(paste("upcharge", upcharge))
}

# This is the buying party who draws his quantity demand from a uniform distribution
ZI_C_Buyer <- function() {
  q_bid_i <<-c()
  upper_limit <- 200
  lower_limit <- 1
  

  quantity <<- sample(lower_limit:upper_limit, size = 1, replace = TRUE) # how much do I want to buy
  redemption_value_i <<- sample(lower_limit:upper_limit, size = 1, replace = TRUE)
  
  value_per_unit <<- redemption_value_i/quantity
if(quantity*costs <  redemption_value_i){
    q_bid <<- quantity
} else {
ZI_C_Buyer()
}
  print(paste("quantity ordererd:", q_bid))
}
ZI_C_Buyer() 








# Prices get matched at the market
Dynamic_Pricing_Market <- function(){
  
Commodity <<- c("Energy")
ZI_C_Seller()

  p <- 0
  k <- 0
  z <- 0

  while(k < 100){
   ZI_C_Buyer()

    
    #cost_plus_pricing <- q_bid*costs + basic_fee
    critical_peak <- q_supply_critical < q_bid # This condition matches quantities by seller and by bidder
    
    p <- p + 1
    k <- k + 1
    

    
    if(critical_peak){
      z <- z + 1
      critical_peak_transactions <<- append(critical_peak_transactions, z)
      transaction <<- append(transaction, p)
      price <<- q_bid*costs + basic_fee + upcharge # critical_peak_pricing
      
      
      quantity_index <<- append(quantity_index, q_bid)
      price_index <<- append(price_index, price)
      profit <<- price*q_bid - (q_bid * costs) #- fix_costs - round(sunk_cost/k)
      
      profit_index <<- append(profit_index, profit)
      accumulated_profit <<- cumsum(profit_index)
      revenue <<- price*q_bid
      revenue_index <<- append(revenue_index, revenue) 
      
      cost_index <<- append(cost_index, costs*q_bid)
      
      redemption_value_index <<- append(redemption_value_index, redemption_value_i)
      realized_payoff_buyer <<- (redemption_value_i - price*q_bid)
      payoffs_buyer <<- append(payoffs_buyer, realized_payoff_buyer)
      
      print(paste("accumulated_profit", accumulated_profit))
      
    } else {
   
      transaction <<- append(transaction, p)
      
      price <<- q_bid*costs + basic_fee #cost-plus pricing
      
      quantity_index <<- append(quantity_index, q_bid)
      price_index <<- append(price_index, price)
      profit <<- price*q_bid - (q_bid * costs) #- fix_costs - round(sunk_cost/k)
      
      profit_index <<- append(profit_index, profit)
      accumulated_profit <<- cumsum(profit_index)
      revenue <<- price*q_bid
      revenue_index <<- append(revenue_index, revenue) 
      
      cost_index <<- append(cost_index, costs*q_bid)
      
      redemption_value_index <<- append(redemption_value_index, redemption_value_i)
      realized_payoff_buyer <<- (redemption_value_i - price*q_bid)
      payoffs_buyer <<- append(payoffs_buyer, realized_payoff_buyer)
      
      print(paste("accumulated_profit", accumulated_profit))
 
    }
    
  }

}

Dynamic_Pricing_Market() 



# This function calculates allocative efficiency and other market indicators.
Calculations <- function(){
  sorted_profits <<- sort(profit_index)
  sorted_quantity <<- sort(quantity_index)
  sorted_costs <<- sort(cost_index)
  sorted_redemption_values <<- sort(redemption_value_index, decreasing = TRUE)
  
  y <- any(sorted_redemption_values <= sorted_costs)
  
  # Realized Profits:
  Payoffs <<- (payoffs_buyer) + (profit_index)
  Total_Surplus <<- round(sum(Payoffs)) 
  Total_Surplus
  
  if(y == TRUE){
    for(i in transaction){
      if(sorted_redemption_values[i] <= sorted_costs[i]){
        truncate <- i 
        eq_c <<-  i 
        equilibrium_price <<- (sorted_redemption_values[i] + sorted_costs[i])/2
        break
      } else {
      }
    }
    # Theoretical Profits:
    theoretical_profits_buyers <<- (sorted_redemption_values[1:eq_c] - equilibrium_price)
    theoretical_profits_sellers <<- (equilibrium_price - sorted_costs[1:eq_c])
    
    Theoretical_Surplus <<- sum(theoretical_profits_buyers) + sum(theoretical_profits_sellers)
    Theoretical_Surplus # aka Marshallian Path
    
    # Realized Profits:
    Payoffs <<- (payoffs_buyer) + (profit_index)
    Total_Surplus <<- round(sum(Payoffs)) 
    Total_Surplus
    
    # Distribution of Profits
    share_of_seller <<- sum(profit_index)/Total_Surplus
    
    # Ex-Post Efficiency:
    allocation_efficiency <<- Total_Surplus / Theoretical_Surplus
    #  trade_ratio <<- max(transaction)/total_rounds   
    
    # Market Performance:
    print(paste("Price", price))
    print(paste("capacity", capacity))
    print(paste("mean costs", mean(sorted_costs)))
    print(paste("mean quantity", mean(sorted_quantity)))
    print(paste("profit margin", profit_margin))
    print(paste("equilibrium transaction:", eq_c))
    print(paste("equilibrium price:", equilibrium_price))
    print(paste("Theoretical Surplus:", Theoretical_Surplus))
    print(paste("Total Surplus", Total_Surplus))
    print(paste("Total Profit by Seller", sum(profit_index)))
    print(paste("Total Quantities Moved", sum(sorted_quantity)))
    print(paste("Total Revenue", sum(revenue_index)))
    print(paste("Allocation efficiency:", allocation_efficiency))
    print(paste("share of seller", share_of_seller))  
    
    
  } else {
    print("no allocative efficiency can be calculated")
    print("there is no equilibrium price")
    print("reason: no crossing of theoretical profits")
    print(paste("Price", price))
    print(paste("mean costs", mean(sorted_costs)))
    print(paste("mean quantity", mean(sorted_quantity)))
    print(paste("capacity", capacity))
    print(paste("profit margin", profit_margin))
    print(paste("Total Surplus", Total_Surplus))
    print(paste("Total Profit by Seller", sum(profit_index)))
    print(paste("Total Quantities Moved", sum(sorted_quantity)))
    print(paste("Total Revenue", sum(revenue_index)))
  }
  Profit_index <<- data.frame(profit_index, price_index, quantity_index, accumulated_profit, transaction)
  
  Price_Plot <<-ggplot(data = Profit_index, aes(x=transaction)) +  geom_line(aes(y=price_index))
  Profit_Plot <<- ggplot(data = Profit_index, aes(x=transaction)) +  geom_line(aes(y=profit_index))
  Quantity_Plot <<- ggplot(data = Profit_index, aes(x=transaction)) +  geom_line(aes(y=quantity_index))
  Break_even <<-ggplot(data = Profit_index, aes(x=transaction)) +  geom_line(aes(y=accumulated_profit))
  grid.arrange(Price_Plot, Profit_Plot, Quantity_Plot,  Break_even, nrow=4)
  
  Payoffs <<- data.frame(transaction, sorted_redemption_values, sorted_costs, sorted_quantity, quantity_index, payoffs_buyer, profit_index)
  Marshallian_Path_Plot <- ggplot(data=Payoffs, aes(x = transaction)) + geom_line(aes(y=sorted_redemption_values)) + geom_line(aes(y=sorted_costs)) 
  Marshallian_Path_Plot + labs(y="Value or Cost", x = "Transaction")
  
  grid.arrange(Marshallian_Path_Plot, Quantity_Plot, Price_Plot, Break_even, ncol=2, nrow=2)
  
}
Calculations()



