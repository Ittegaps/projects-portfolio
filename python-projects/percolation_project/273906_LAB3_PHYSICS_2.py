import numpy as np
import matplotlib.pyplot as plt

#task2b

files = ["Ave-L10T40.txt", "Ave-L50T40.txt", "Ave-L100T40.txt"]
labels = ["L = 10", "L = 50", "L = 100"]
markers = ["o", "s", "^"]

plt.figure(figsize=(8, 6))

for i in range(len(files)):
    try:
        data = np.loadtxt(files[i])
        p_vals = data[:, 0]
        p_flow = data[:, 1]
        
        plt.plot(p_vals, p_flow, label=labels[i], marker=markers[i], 
                 markersize=5, linestyle='-', linewidth=1.5)
    except:
        print(f"Error: Could not find or read {files[i]}")

plt.xlabel("Occupancy probability p", fontsize=12)
plt.ylabel("Flow probability Pflow", fontsize=12)
plt.title("Site Percolation: Pflow vs p", fontsize=14)
plt.legend()
plt.grid(True, linestyle='--', alpha=0.7)

#plt.savefig("percolation_plot.png", dpi=300)
plt.show()