# -*- coding: utf-8 -*-
"""
Created on Tue Mar 12 20:28:59 2024

@author: JYW2018
"""

def Patient_scheduling_MIP(n_samp, K, nk, DFF, c_I, c_O, c_W=1, n=16, T=240):
    
    import gurobipy as gp
    from gurobipy import GRB
    import numpy as np
    import os
    
    log_directory = "./RawOutput/Gurobi_MIP_log/"

    # Create a Gurobi model 
    model = gp.Model("Patient_scheduling_MIP")
    
    # Start logging
    log_file_name = f"K{K}_cI{c_I}_cO{c_O}_{nk}_gurobi.log"
    log_file_path = os.path.join(log_directory, log_file_name)
    model.setParam('LogFile', log_file_path)
    model.setParam('OutputFlag', 1)  # Control the detail level of the output log
    
    d = T/n  # the length of slot duration
    X_Vec=np.full(n, d)  # service time allowance
    ZERO_Vec = np.zeros((n_samp, 1))
    
    # Create x_ki(3x16) array list
    X_ki_List = []

    # Assign values from DFF to x_ki
    for nn in range(n_samp):
        x_ki = np.zeros((K, n))  # Create a new x_ki array for each iteration
        for k in range(K):
            x_ki[k] = DFF[k][nn]  # Assign the first row of each DF to the corresponding row of x_ki
        X_ki_List.append(x_ki)

    # Decision Variables
    x = model.addVars(K, n, vtype=GRB.BINARY, name="x")

    R_mat = model.addVars(n_samp, n, name="R_mat")
    
    W_Mat = model.addVars(n_samp, n, name="W_mat")  # Initialize waiting time matrix
    
    T_Vec = model.addVars(n_samp, name="T_Vec")
    O_Vec = model.addVars(n_samp, name="O_Vec")
    last_start_Vec = model.addVars(n_samp, name="last_start_Vec")
    Idle_Vec = model.addVars(n_samp, name="Idle_Vec")

    # Add constraint
    for nn in range(n_samp): 
        for i in range(n):
            model.addConstr(R_mat[nn, i] == gp.quicksum(x[k, i] * X_ki_List[nn][k, i] for k in range(K)), name=f"cnst_ServTime_{i}")
    
    for i in range(n-1):
        for nn in range(n_samp):
            model.addConstr(W_Mat[nn, i+1] >= 0, name=f"constraint_{i}_{nn}_1")
            model.addConstr(W_Mat[nn, i+1] >= W_Mat[nn, i] + R_mat[nn, i] - X_Vec[i], name=f"constraint_{i}_{nn}_2")

    for nn in range(n_samp):
        # Overtime
        model.addConstr(T_Vec[nn] == np.sum(X_Vec) + W_Mat[nn, n-1] + R_mat[nn, n-1], name=f"T_Vec_constraint_{nn}")
        model.addConstr(O_Vec[nn] >= ZERO_Vec[nn, 0], name=f"O_Vec_lower_bound_constraint_{nn}")
        model.addConstr(O_Vec[nn] >= T_Vec[nn] - T, name=f"O_Vec_upper_bound_constraint_{nn}")
        # Idle
        model.addConstr(last_start_Vec[nn] == np.sum(X_Vec) + W_Mat[nn, n-1], name=f"last_start_constraint_{nn}")
        model.addConstr(Idle_Vec[nn] >= ZERO_Vec[nn, 0], name=f"Idle_lower_bound_Constraint_{nn}")
        model.addConstr(Idle_Vec[nn] >= last_start_Vec[nn] - gp.quicksum(R_mat[nn, i] for i in range(n-1)), name=f"Idle_upper_bound_Constraint_{nn}")
    
    for k in range(K):
        model.addConstr(gp.quicksum(x[k, i] for i in range(n)) == nk[k], name=f"cnst_pat_num_consv_{k}")

    for i in range(n):
        model.addConstr(gp.quicksum(x[k, i] for k in range(K)) == 1, name=f"cnst_slot_num_consv_{i}")
    
     
    # Objective Function
    model.setObjective(
        c_W * gp.quicksum(gp.quicksum(W_Mat[nn, i] for nn in range(n_samp)) / n_samp for i in range(n)) +
        c_I * gp.quicksum(Idle_Vec[nn] for nn in range(n_samp)) / n_samp +
        c_O * gp.quicksum(O_Vec[nn] for nn in range(n_samp)) / n_samp,
        GRB.MINIMIZE)
    

    # Optimize the model
    model.optimize()

    # Create a dictionary to store the optimal solution for x
    optimal_solution = {}

    # Create an empty matrix with zeros
    x_matrix = np.zeros((K, n))

    for k in range(K):
        for i in range(n):
            optimal_solution[f"x_{k}_{i}"] = x[k, i].x
            x_matrix[k, i] = x[k, i].x
            
    # Return the optimal objective value and the optimal solution for x
    
    def generate_index_to_char(K):
        return {k: chr(ord('A') + k) for k in range(K)}

    # Create a dictionary that maps the column index to the corresponding character
    #{0: 'A', 1: 'B', 2: 'C'}
    index_to_char = generate_index_to_char(K)

    # Generate the Rule List using list comprehensions
    Rule_List = [index_to_char[np.where(col==1)[0][0]] for col in x_matrix.T]
    

    # Turn off logging
    model.setParam('LogFile', '')
    
    
    return model.objVal, Rule_List