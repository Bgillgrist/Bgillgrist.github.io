import random as rm
import itertools as iter
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
        if bph_std > 0:
            z_scores = [(i - self.bph) / bph_std for i in range(1,5)]

            self.P_single = stats.norm.cdf(z_scores[0])
            self.P_double = stats.norm.cdf(z_scores[1]) - stats.norm.cdf(z_scores[0])
            self.P_triple = stats.norm.cdf(z_scores[2]) - stats.norm.cdf(z_scores[1])
            self.P_hr = stats.norm.cdf(z_scores[3]) - stats.norm.cdf(z_scores[2])

            total_probability = self.P_single + self.P_double + self.P_triple + self.P_hr
            self.P_single /= total_probability
            self.P_double /= total_probability
            self.P_triple /= total_probability
            self.P_hr /= total_probability

        else:
            self.P_single = self.P_double = self.P_triple = self.P_hr = 0

        self.walk_percentage = self.OBP - self.AVG
        self.out_percentage = 1 - self.walk_percentage - self.AVG
    
    def get_hit_type(self):
        r = rm.random()
        if r < self.out_percentage:
            return 'out'
        elif r < self.out_percentage + self.walk_percentage:
            return 'walk'
        elif r < self.out_percentage + self.walk_percentage + self.P_single:
            return 'single'
        elif r < self.out_percentage + self.walk_percentage + self.P_single + self.P_double:
            return 'double'
        elif r < self.out_percentage + self.walk_percentage + self.P_single + self.P_double + self.P_triple:
            return 'triple'
        else:
            return 'home_run'

class Team: 
    def __init__(self, players, bph_std):
        self.players = players
        self.bph_std = bph_std
        self.calc_probabilities()

    def calc_probabilities(self):
        for player in self.players:
            player.probabilities(self.bph_std)

    def inning_sim(self, batter_index):
        outs = 0
        runs = 0
        bases = [None, None, None]
        batter = batter_index

        while outs <3:
            player = self.players[batter_index]
            batter_index = (batter_index + 1) % len(self.players)

            hit_type = player.get_hit_type()
            if hit_type == 'out':
                outs += 1
            elif hit_type == 'walk':
                if bases[0] is not None and bases[1] is not None and bases[2] is not None:
                    runs += 1
                if bases[0] is not None:
                    bases = [player] + bases[:2]
                else:
                    bases = [player, None, None]
            elif hit_type == 'single':
                if bases[2] is not None:
                    runs += 1
                bases = [player] + bases[:2]
            elif hit_type == 'double':
                if bases[2] is not None:
                    runs += 1
                if bases[1] is not None:
                    runs += 1
                bases = [None, player] + bases[:1]
            elif hit_type == 'triple':
                runs += sum(b is not None for b in bases)
                bases = [None, None, player]
            elif hit_type == 'home_run':
                runs += 1 + sum(b is not None for b in bases)
                bases = [None, None, None]

        return runs, batter_index
    
    def game_sim(self, innings=9):
        total_runs = 0
        batter_index = 0

        for _ in range(innings):
            runs, batter_index = self.inning_sim(batter_index)
            total_runs += runs

        return total_runs
    
def top_lineup(result, method):
    Expected_runs, lineup = result 
    print(f"Best lineup ({method}) so far has {Expected_runs} expected number of runs in a game")
    for player in lineup: 
        print(player.name)
    print()
    
def sim_permutation(permutation, bph_std):
    num = 1000
    team = Team(list(permutation), bph_std)
    scores = [team.game_sim() for _ in range(num)]
    Expected_runs = sum(scores) / num
    return(Expected_runs, permutation)

def regression_permutation(permutation):
    OBP = [player.OBP for player in permutation]
    SLG = [player.SLG for player in permutation]
    formula = (-7.4308 + (-5.08667*OBP[0]) + (3.6728*OBP[1]) + (8.8314*OBP[2]) +
               (5.32595*OBP[3]) + (-2.687*OBP[4]) + (-0.626644*OBP[5]) + (5.921*OBP[6]) +
               (4.31638*OBP[7]) + (8.30182*OBP[8]) + (6.3018*SLG[0]) + (-3.0225*SLG[1]) +
               (-0.720786*SLG[2]) + (-0.0043*SLG[3]) + (7.17167*SLG[4]) + (0.260923*SLG[5]) +
               (0.819067*SLG[6]) + (0.096553*SLG[7]) + (-2.266321*SLG[8]))
    return formula, permutation

def main():
    players = [
#                 Name           AVG, OBP, SLG, PA
        Player("Jakob Schardt", .324, .398, .533, 94),
        Player("Nick Lazzara", .270 , .368, .378, 88),
        Player("Easton Elliot", .313 , .411, .375, 96),
        Player("Josh Livingston", .325 , .426, .426, 94),
        Player("Joey Craig", .267 , .389, .267, 18),
        Player("Eli Watson", .280 , .514, .400, 37),
        Player("Joey Donnelly", .316 , .426, .481, 94),
        Player("Alex Birge", .308 , .478, .538, 69),
        Player("Adrian Lopez", .241 , .384, .367, 100)
    ]

    bph_values = [player.bph for player in players]
    bph_std = np.std(bph_values)

    for player in players: 
        player.probabilities(bph_std)
    
    permutations = list(iter.permutations(players))
    print(f"Processing {len(permutations)} permutations")

    best_sim_lineup = (0, [])
    best_regression_lineup = (-float('inf'), [])

    for index, permutation in enumerate(permutations):
        sim_result = sim_permutation(permutation, bph_std)
        regression_result = regression_permutation(permutation)
        if index %1000 == 0:
            print(f"Processing permutation {index} of {len(permutations)}")
        if sim_result[0] > best_sim_lineup[0]:
            best_sim_lineup = sim_result
            top_lineup(best_sim_lineup, "Simulation")
        if regression_result[0] > best_regression_lineup[0]:
            best_regression_lineup = regression_result
            top_lineup(best_regression_lineup, "Regression")

if __name__ == "__main__":
    main()