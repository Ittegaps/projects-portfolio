import random
import numpy as np
import matplotlib.pyplot as plt

#Task 2

with open("perc_ini.txt", "r") as file:
        parts = file.read().strip().split()
        L_main = int(parts[0])
        T = int(parts[1])
        p0 = float(parts[2])
        dp = float(parts[3])
        pk = float(parts[4])

p_values = np.arange(p0, pk + dp, dp)

def simulation(current_p, current_L):
    Lattice = (np.random.rand(current_L, current_L) < current_p).astype(int)
    burning_queue = []

    for i in range(current_L):
        if (Lattice[0,i] == 1):
            Lattice[0,i] = 2 
            burning_queue.append((0,i))

    while len(burning_queue) > 0:
        curr_r, curr_c = burning_queue.pop(0)
        next_val = Lattice[curr_r, curr_c] + 1
        for dr, dc in [(1,0), (-1,0), (0,1), (0,-1)]:
            nr, nc = curr_r + dr, curr_c + dc
            if 0 <= nr < current_L and 0 <= nc < current_L and Lattice[nr, nc] == 1:
                Lattice[nr, nc] = next_val
                burning_queue.append((nr, nc))
    
    path_exists = any(Lattice[current_L-1, :] > 1)
    return path_exists, Lattice
    
    
results = []
for p_val in p_values:
    count = 0
    for _ in range(T):
        success, _ = simulation(p_val, L_main)
        if success: count += 1
    results.append((p_val, count/T))
    print(f"p = {p_val:.2f}, Successes: {count}")


filename = "Ave-L" + str(L_main) + "T" + str(T) + ".txt"
with open(filename, "w") as file:
    for p_v, p_f in results:
        file.write(f"{p_v:.4f} {p_f:.4f}\n")


#task 2a
        
for p_vis in [0.4, 0.6, 0.8]:
    _, grid = simulation(p_vis, 10)
    plt.figure()
    plt.imshow(grid == 0, cmap='binary', alpha=0.1)     #shadowed blocks
    for r in range(10):
        for c in range(10):
            if grid[r,c] >= 2:
                plt.text(c, r, str(grid[r,c]), va='center', ha='center', fontsize=8)    #numbers on grid
    plt.title(f"Visual p={p_vis}")

plt.show()
