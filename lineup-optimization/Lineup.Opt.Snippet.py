import random as rm
import numpy as np
import scipy.stats as stats

class Player: 
    def __init__(self, name, AVG, OBP, SLG, PA):
        self.name = name
        self.AVG = AVG
        self.OBP = OBP
        self.SLG = SLG
        self.PA = PA
        self.bph = (SLG*PA) / (AVG*PA) if AVG > 0 else 0

    def probabilities(self, bph_std):
        z_scores = [(i - self.bph) / bph_std for i in range(1,5)]
        self.P_single = stats.norm.cdf(z_scores[0])
        # Additional probability logic here...

    def get_hit_type(self):
        r = rm.random()
        if r < self.out_percentage:
            return 'out'
        elif r < self.out_percentage + self.walk_percentage:
            return 'walk'
        # Additional hit type conditions...

class Team: 
    def __init__(self, players, bph_std):
        self.players = players
        self.calc_probabilities(bph_std)

    def calc_probabilities(self, bph_std):
        for player in self.players:
            player.probabilities(bph_std)

    def inning_sim(self, batter_index):
        outs = 0
        runs = 0
        bases = [None, None, None]
        while outs < 3:
            hit_type = self.players[batter_index].get_hit_type()
            # Increment outs or advance runners based on hit_type...
            batter_index = (batter_index + 1) % len(self.players)
        return runs, batter_index

def top_lineup(result, method):
    Expected_runs, lineup = result
    print(f"Best lineup ({method}) with {Expected_runs} expected runs")

def sim_permutation(permutation, bph_std):
    team = Team(list(permutation), bph_std)
    scores = [team.inning_sim(0)[0] for _ in range(1000)]
    return sum(scores) / 1000, permutation

if __name__ == "__main__":
    players = [
        Player("Jakob Schardt", .324, .398, .533, 94),
        Player("Nick Lazzara", .270, .368, .378, 88),
        # Additional players...
    ]
    bph_std = np.std([p.bph for p in players])
    for p in players:
        p.probabilities(bph_std)
    permutations = list(iter.permutations(players))
    
    # Run simulations for each lineup
    best_lineup = max((sim_permutation(p, bph_std) for p in permutations), key=lambda x: x[0])
    top_lineup(best_lineup, "Simulation")