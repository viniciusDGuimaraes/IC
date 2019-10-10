calculate.fitness <- function(gene)
{
  fitness <- 0
  for(i in 1:(length(gene) - 2))
  {
    if(mean(c(gene[i], gene[i + 1], gene[i + 2])) == 3)
    {
      fitness <- fitness + 1
    }
  }
  
  return(fitness)
  
  if(FALSE)
  {
    fitness <- 0
    total_fitness <- 0
    for(i in 2:length(gene))
    {
      if(gene[i - 1] == gene[i])
      {
        fitness <- fitness + 1
      }
      else if(fitness > 0)
      {
        total_fitness <- total_fitness + fitness + 1
        fitness <- 0
      }
    }
    
    if(fitness > 0)
    {
      total_fitness <- total_fitness + fitness + 1
      fitness <- 0
    }
    
    return(total_fitness/length(gene))
  }
}

population <- 300
gene_size <- 100
individuals <- rep(list(NA), population)
for(i in 1:population)
{
  individuals[[i]] <- sample(0:9, gene_size, replace = TRUE)
}

pairs <- population/2
half_gene <- gene_size/2
iter_max <- 100
fitness_record <- numeric(iter_max)
alpha <- gene_size/10

for(iter in 1:iter_max)
{
  for(i in 1:pairs)
  {
    parent_1 <- individuals[[i]]
    parent_2 <- individuals[[i + 1]]
    child_1 <- parent_1
    child_2 <- parent_2
    # Cross over
    crossover_index <- sample(1:gene_size, gene_size/2)
    parent_1_genes <- parent_1[crossover_index]
    parent_2_genes <- parent_2[crossover_index]
    child_1[crossover_index] <- parent_2_genes
    child_2[crossover_index] <- parent_1_genes
    
    mutation_index <- sample(1:gene_size, alpha)
    child_1[mutation_index] <- sample(0:9, alpha, replace = TRUE)
    child_2[mutation_index] <- sample(0:9, alpha, replace = TRUE)
    
    fitness <- c(calculate.fitness(parent_1),
                 calculate.fitness(parent_2),
                 calculate.fitness(child_1),
                 calculate.fitness(child_2))
    
    ordered_positions <- order(fitness, decreasing = TRUE)
    
    # Seleciona os dois melhores resultados
    if(ordered_positions[1] == 1)
    {
      individuals[[i]] <- parent_1
    }
    else if(ordered_positions[1] == 2)
    {
      individuals[[i]] <- parent_2
    }
    else if(ordered_positions[1] == 3)
    {
      individuals[[i]] <- child_1
    }
    else if(ordered_positions[1] == 4)
    {
      individuals[[i]] <- child_2
    }
    
    if(ordered_positions[2] == 1)
    {
      individuals[[i + 1]] <- parent_1
    }
    else if(ordered_positions[2] == 2)
    {
      individuals[[i + 1]] <- parent_2
    }
    else if(ordered_positions[2] == 3)
    {
      individuals[[i + 1]] <- child_1
    }
    else if(ordered_positions[2] == 4)
    {
      individuals[[i + 1]] <- child_2
    }
  }
  
  individual_fitness <- numeric(population)
  for(j in 1:population)
  {
    current_individual <- individuals[[j]]
    individual_fitness[j] <- calculate.fitness(current_individual)
  }
  
  fitness_record[iter] <- mean(individual_fitness)
}

plot(fitness_record, type = "l")