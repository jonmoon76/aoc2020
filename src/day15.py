#!/usr/bin/python3


seed = [14,1,17,0,3,20]
example_seed = [ 0, 3, 6]

ARRAY_SIZE = 1000000

class State:
    time = 0
    previousGo = 0
    previousRoundsList = []
    previousRoundsDict = {}

    def __init__ (self):
        for i in range(1, ARRAY_SIZE):
            self.previousRoundsList.append(NumberState())
    

    def update(self, n):
        if n < ARRAY_SIZE:
            numberState = self.previousRoundsList[n]
        else:
            try:
                numberState = self.previousRoundsDict[n]
            except KeyError:
                numberState = NumberState()
                self.previousRoundsDict[n] = numberState
        numberState.update(self.time)
        self.time = self.time + 1
        self.previousGo = numberState
        self.previousN = n



class NumberState:
    def __init__ (self):
        self.lastTime =  None
        self.previousTime = None

    def update(self, n):
        self.previousTime = self.lastTime
        self.lastTime = n


# makeMove : State -> State
# makeMove state =
#     case Dict.get state.previousGo state.previousRounds of
#         Just ( a, Nothing ) ->
#             updateState 0 state

#         Just ( a, Just b ) ->
#             updateState (a - b) state

#         Nothing ->
#             Debug.todo "Assert: Must have made previous move"
def makeMove (state):
    previousGo = state.previousGo
    if previousGo.previousTime == None:
        state.update(0)
    else:
        state.update(previousGo.lastTime - previousGo.previousTime)



def seedMoves (seed):
    state = State()
    for move in seed:
        state.update(move)
    return state

def playGame( maxTime, state : State):
    while state.time < maxTime:
        if state.time % 100000 == 0:
            print ("{} : {}".format(state.time, len(state.previousRoundsDict)))

        makeMove(state)


state = seedMoves(seed)    

playGame(30000000, state)

print(state.previousN)