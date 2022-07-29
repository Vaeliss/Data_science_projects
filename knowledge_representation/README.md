Project for the knowledge representation and learning exam in unipd.

Minesweeper game implemented in numpy, only square grids are allowed in the implementatation considered.
A logical agent is created, which, making using of a sat solver, satisfiability and model counting solves the minesweeper game at superhuman speed and with win % close to optimal.

As long as some squares in the grid are 100% safe by logical consequence of the knowledge base, the agent probes them. New knowledge is added (as cardinality constraints in CNF) to the knowledge base when a safe square is probed.
If the agent can't determine a safe square, it uses probabilistic reasoning by problem reduction (focusing only on squares adjacent to the area explored) and model counting to determine the safest square to probe
Probabilistic reasoning is particularly useful in the early stages of the game in the case that the first moves don't allow early exploration.

Some performance metrics of the agent:

| Grid size | # mines | mines density | win % | Mean time per game |
| :-------: | :-----: | :-----------: | :---: | :----------------: |
| 4x4       | 5       | 0.3125        | 52%   | 0.15s              |
| 6x6       | 6       | 0.1667        | 72%   | 0.03s              |
| 8x8       | 10      | 0.1563        | 82%   | 0.36s              |
| 16x16     | 40      | 0.1563        | 64%   | 0.95s              |
| 22x22     | 99      | 0.2045        | 31%   | 6 s                |


Note that the 8x8 game is the classic beginner game.
The 16x16 game is the classic intermediate game.
The 22x22 is pretty much equivalent to the classic expert game in terms of area of the grid, number of mines and density.
