import numpy as np
import random 

#MNK game AI LEVEL2
CHECK_O=1
CHECK_X=2
CHECK_NULL=0

#same as level 0
def initBoard(m,n):
    bo = np.zeros(shape=(m, n)) 
    return bo
        
def printBoard(board):
    m = len(board)
    n = len(board[0])
    for i in range(m):
        for j in range(n):
            if  bo[i][j] == CHECK_NULL:
                print(' ', end='')
            elif  bo[i][j] == CHECK_X:
                print('X', end='')
            elif  bo[i][j] == CHECK_O:
                print('O', end='')
              
            print('|', end = '')
        print()    
        for j in range(n):
            print('__', end='')
        print()

def user_input(board):
      move_x = int(input('Please enter position x: ')) 
      move_y = int(input('Please enter position y: ')) 
      while board[move_x][move_y] != CHECK_NULL:
          move_x = int(input('Please enter position x: '))  
          move_y = int(input('Please enter position y: ')) 
      return [move_x, move_y]
      
def AIMove(board):
    #level0
    list_of_avail_pos = get_list_avail_pos(board)
    AI_pos = random.choice(list_of_avail_pos)         
    return AI_pos

def get_list_avail_pos(board): 
    m = len(board);
    n = len(board[0])
    list_of_avail_pos = []
    for i in range(m):
        for j in range(n):
            if board[i][j] == CHECK_NULL:
                list_of_avail_pos.append([i,j])
    return list_of_avail_pos
       
def isBoardFull(board):
    l = get_list_avail_pos(board)
    if len(l)==0:
        return True
    else:
        return False

def isWinner(board, x, y, k, player_symbol):
    if isHorizontalWin(board, x, y, k, player_symbol):
        return True
    if(isVerticalWin(board, x, y, k, player_symbol)):
        return True
    if(isDiagonalWin(board, x, y, k, player_symbol)):
        return True

def isHorizontalWin(board, x, y, k, player_symbol): 
    n = len(board[0])
    curr_y = move_y
    count=0
    while curr_y >=0 and board[move_x][curr_y] == player_symbol:
        count = count +1
        curr_y = curr_y -1
        
    curr_y  = move_y + 1
    while curr_y < n and board[move_x][curr_y] == player_symbol:
        count = count +1
        curr_y = curr_y +1        
      
    if count >= k:
        return True
    else:
        return False             
        
def isVerticalWin(board, x, y, k, player_symbol):
    m = len(board)
    curr_x = move_x
    count=0
    while curr_x >=0 and board[curr_x][move_y] == player_symbol:
        count = count +1
        curr_x = curr_x -1
        
    curr_x  = move_x + 1
    while curr_x < m and board[curr_x][move_y] == player_symbol:
        count = count +1
        curr_x = curr_x +1   
    
    if count >= k:
        return True
    else:
        return False        
 
def isDiagonalWin(board, x, y, k, player_symbol): 
    m = len(board)
    n = len(board[0])
    count1=0
    count2=0
    curr_x = move_x 
    curr_y = move_y
    # 1) \ diagnonal :  top left corner diagonal
    while curr_y >=0 and curr_x>=0 and board[curr_x][curr_y] == player_symbol:
        count1 +=1
        curr_y -=1
        curr_x -=1
    # 1) \ diagnonal:  bottom right corner diagonal
    curr_x  = move_x + 1
    curr_y  = move_y + 1 
    while curr_y< n and curr_x< m and board[curr_x][curr_y] == player_symbol:
        count1 +=1
        curr_y += 1 
        curr_x += 1    
    # 2) / diagnoal : top right corner diagonal 
    curr_x = move_x 
    curr_y = move_y   
    while curr_x < m and curr_y >=0 and board[curr_x][curr_y] == player_symbol:
        count2 +=1
        curr_x +=1
        curr_y -=1
    # 2) / diagnonal: bottom left corner diagonal
    curr_x = move_x +1
    curr_y = move_y -1
    while curr_x <n and curr_y<=0 and board[curr_x][curr_y] == player_symbol:
        count2 +=1
        curr_y +=1
        curr_x -=1
        
    if count1 >= k:
        return True
    elif count2 >= k:
        return True
    else:
        return False 

#### level2 functions ### 

#get the longest chain of user's marks 
def get_best_pos(board, k, symbol): 
    best_count = 0 
    best_x=-1
    best_y=-1 
    if symbol == CHECK_O:
        opponent_symbol = CHECK_X
    elif symbol == CHECK_X:
        opponent_symbol = CHECK_O   
    for i in range(m):
        for j in range(n):
            if board[i][j] == symbol:
              pos_list = getLongSeq(board, i, j, k, opponent_symbol)
              #first check player's longest chain 
              if pos_list[0] > best_count:
                  best_count = pos_list[0]
                  best_x = pos_list[0]
                  best_y = pos_list[1]
    return [best_count, best_x, best_y]

#AI move for AI level 2
#calls function get_best_pos to determine where to set mark 
def AIMove_lv2(board, k): 
    move=[]
    user_pos_list = get_best_pos(board, k, CHECK_X)
    AI_pos_list = get_best_pos(board, k, CHECK_O)
    if user_pos_list[0] > AI_pos_list[0]: 
        move = user_pos_list[1,2]
    else:
        move= AI_pos_list[1,2]
    board[move[0], move[1]] = CHECK_O

    
#get the long list of a specific position in board
#return format: [ count, x, y]    
def getLongSeq(board, move_x, move_y, k, opponent_symbol):
    horizontal_list =  getHorizontalCount(board, move_x, move_y, k, opponent_symbol) 
    vertical_list =  getVerticalCount(board, move_x, move_y, k, opponent_symbol) 
    diagonal1_list = getDiagonal1Count(board, move_x, move_y, k, opponent_symbol)
    diagonal2_list = getDiagonal2Count(board, move_x, move_y, k, opponent_symbol)
    
    #initialize best list
    best_list=[0, -1, -1]

    if horizontal_list != None and horizontal_list[0] > best_list[0]:
            best_list = horizontal_list
            
    if vertical_list != None and vertical_list[0] > best_list[0]:
            best_list = vertical_list

    if diagonal1_list != None and diagonal1_list[0] > best_list[0]:
        best_list = diagonal1_list
        
        
    if diagonal2_list != None and diagonal2_list[0] > best_list[0]:
        best_list = diagonal2_list
        
    return best_list

#gets horizontal longest chain of marks 
def getHorizontalCount(board, move_x, move_y, k, opponent_symbol):
    n = len(board[0])
    curr_y = move_y   
    AIcount=0
    NULLcount=0
    nextMove = None
    # move left
    while curr_y >=0 and board[move_x][curr_y] != opponent_symbol: # X or NULL
        if board[move_x][curr_y] == CHECK_NULL: 
            NULLcount += 1     
            if nextMove == None:
                nextMove = [move_x, curr_y]
        if board[move_x][curr_y] == CHECK_X: 
            AIcount += 1
        curr_y -=1
    #move right        
    curr_y  = move_y + 1
    while curr_y < n and board[move_x][curr_y] != opponent_symbol: # X or NULL
        if board[move_x][curr_y] == CHECK_NULL: 
            NULLcount += 1     
            
        if board[move_x][curr_y] == CHECK_X: 
            AIcount += 1
        curr_y +=1
    if AIcount + NULLcount >= k:
        return [AIcount, nextMove[0], nextMove[1] ]
    else:
        return None
    
#gets vertical longest chain of marks
def getVerticalCount(board, move_x, move_y, k, opponent_symbol):
    m = len(board)
    curr_x = move_x
    AIcount=0
    NULLcount=0
    nextMove = None
    #move up
    while curr_x >=0 and board[curr_x][move_y] != opponent_symbol: # X or NULL
        if board[curr_x][move_y] == CHECK_NULL: 
            NULLcount += 1     
            if nextMove == None:
                nextMove = [curr_x, move_y]
        if board[curr_x][move_y] == CHECK_X: 
            AIcount += 1
        curr_x -=1
                
    #move down        
    curr_x  = move_x + 1
    while curr_x < m and board[curr_x][move_y] != opponent_symbol: # X or NULL
        if board[curr_x][move_y] == CHECK_NULL: 
            NULLcount += 1              
        if board[curr_x][move_y] == CHECK_X: 
            AIcount += 1
        curr_x +=1
    
    if AIcount + NULLcount >= k:
        return [AIcount, nextMove[0], nextMove[1] ]
    else:
        return None

#gets the '\' diagonal longest chain of marks 
def getDiagonal1Count(board, move_x, move_y, k, opponent_symbol):
    m = len(board)
    n = len(board[0])
    curr_x = move_x
    curr_y = move_y
    
    AIcount=0
    NULLcount=0
    nextMove = None
    #moves left and up 
    while curr_x >=0 and  curr_y >= 0 and board[curr_x][curr_y] != opponent_symbol: # X or NUL
        if board[curr_x][curr_y] == CHECK_NULL: 
            NULLcount += 1     
            if nextMove == None:
                nextMove = [curr_x, curr_y]
        if board[curr_x][curr_y] == CHECK_X: 
            AIcount += 1
        curr_x -=1
        curr_y -=1
                
    #move right and down         
    curr_x  = move_x + 1
    curr_y = move_y + 1
    while curr_x < m and curr_y < n and board[curr_x][curr_y] != opponent_symbol: # X or NUL
        if board[curr_x][curr_y] == CHECK_NULL: 
            NULLcount += 1              
        if board[curr_x][curr_y] == CHECK_X: 
            AIcount += 1
        curr_x +=1
        curr_y +=1
    if AIcount + NULLcount >= k:
        return [AIcount, nextMove[0], nextMove[1] ]
    else:
        return None

#gets the '/' diagonal longest chain of marks 
def getDiagonal2Count(board, move_x, move_y, k, opponent_symbol):
    m = len(board)
    n = len(board[0])
    curr_x = move_x
    curr_y = move_y
    
    AIcount=0
    NULLcount=0
    nextMove = None
    #moves right and up 
    while curr_x >=0 and  curr_y <n and board[curr_x][curr_y] != opponent_symbol: # X or NUL
        if board[curr_x][curr_y] == CHECK_NULL: 
            NULLcount += 1     
            if nextMove == None:
                nextMove = [curr_x, curr_y]
        if board[curr_x][curr_y] == CHECK_X: 
            AIcount += 1
        curr_x -=1
        curr_y +=1
            
    #move left and down         
    curr_x  = move_x + 1
    curr_y = move_y -1
    while curr_x < m and curr_y >=0 and board[curr_x][curr_y] != opponent_symbol: # X or NUL
        if board[curr_x][curr_y] == CHECK_NULL: 
            NULLcount += 1              
        if board[curr_x][curr_y] == CHECK_X: 
            AIcount += 1
        curr_x +=1
        curr_y -=1
        
    if AIcount + NULLcount >= k:
        return [AIcount, nextMove[0], nextMove[1] ]
    else:
        return None

################## LEVEL2 MAIN #######################################
m = int(input('Please enter the number of rows: '))
n = int(input('Please enter the number of columns: '))
k = int(input('Please enter the number to win: '))

bo = initBoard(m,n)
printBoard(bo)

while True:
    #read user input
    [move_x,move_y] = user_input(bo)
    bo[move_x][move_y] = CHECK_O
 
    if isWinner(bo, move_x, move_y, k, CHECK_O) == True: 
        print("Player has won! ")
        break     
    #AI move
    [move_x,move_y] = AIMove_lv2(bo)
    bo[move_x][move_y] = CHECK_X

    if isWinner(bo, move_x, move_y, k, CHECK_X) == True: 
        print("Player has lost. Try again? ")
        break     
    printBoard(bo)
    #if board is full, break 
    if isBoardFull(bo) == True:
        print('Board is full, end of game')
        break

printBoard(bo)            



################ AI2 vs. AI0 GAMES ###############
##Game2.1 AI2 vs. AI0 ##
for i in range(500):
    bo = initBoard(3, 3)
    printBoard(bo)
    while True:
        [move_x,move_y] = AIMove_lv2(bo)
        bo[move_x][move_y] = CHECK_O
        
        AI2_win = 0
        if isWinner(bo, move_x, move_y, k, CHECK_O) == True: 
            print("AI1 has won! ")
            AI2_win +=1
            break     
        #AI move
        [move_x,move_y] = AIMove(bo)
        bo[move_x][move_y] = CHECK_X
        
        AI0_win = 0
        if isWinner(bo, move_x, move_y, k, CHECK_X) == True: 
            print("AI0 has won ")
            AI0_win +=1
            break     
        printBoard(bo)
        #if board is full, break 
        if isBoardFull(bo) == True:
            print('Board is full, end of game')
            break
printBoard(bo)     
##Game2.2 AI2 vs. AI0 ##
for i in range(500):
    bo = initBoard(4, 4)
    printBoard(bo)
    while True:
        [move_x,move_y] = AIMove_lv2(bo)
        bo[move_x][move_y] = CHECK_O
        
        AI2_win = 0
        if isWinner(bo, move_x, move_y, k, CHECK_O) == True: 
            print("AI1 has won! ")
            AI2_win +=1
            break     
        #AI move
        [move_x,move_y] = AIMove(bo)
        bo[move_x][move_y] = CHECK_X
        
        AI0_win = 0
        if isWinner(bo, move_x, move_y, k, CHECK_X) == True: 
            print("AI0 has won ")
            AI0_win +=1
            break     
        printBoard(bo)
        #if board is full, break 
        if isBoardFull(bo) == True:
            print('Board is full, end of game')
            break
printBoard(bo) 

##Game2.3 AI2 vs. AI0 ##
for i in range(500):
    bo = initBoard(4, 3)
    printBoard(bo)
    while True:
        [move_x,move_y] = AIMove_lv2(bo)
        bo[move_x][move_y] = CHECK_O
        
        AI2_win = 0
        if isWinner(bo, move_x, move_y, k, CHECK_O) == True: 
            print("AI1 has won! ")
            AI2_win +=1
            break     
        #AI move
        [move_x,move_y] = AIMove(bo)
        bo[move_x][move_y] = CHECK_X
        
        AI0_win = 0
        if isWinner(bo, move_x, move_y, k, CHECK_X) == True: 
            print("AI0 has won ")
            AI0_win +=1
            break     
        printBoard(bo)
        #if board is full, break 
        if isBoardFull(bo) == True:
            print('Board is full, end of game')
            break
printBoard(bo) 
    
################ AI2 vs. AI1 GAMES ###############
##Game3.1 AI2 vs. AI1 ##
for i in range(500):
    bo = initBoard(3, 3)
    printBoard(bo)
    while True:
        [move_x,move_y] = AIMove_lv1(bo)
        bo[move_x][move_y] = CHECK_O
        
        AI1_win = 0
        if isWinner(bo, move_x, move_y, k, CHECK_O) == True: 
            print("AI1 has won! ")
            AI1_win +=1
            break     
        #AI move
        [move_x,move_y] = AIMove_lv2(bo)
        bo[move_x][move_y] = CHECK_X
        
        AI2_win = 0
        if isWinner(bo, move_x, move_y, k, CHECK_X) == True: 
            print("AI0 has won ")
            AI2_win +=1
            break     
        printBoard(bo)
        #if board is full, break 
        if isBoardFull(bo) == True:
            print('Board is full, end of game')
            break
printBoard(bo)     

##Game3.2 AI2 vs. AI1 ##
for i in range(500):
    bo = initBoard(4, 4)
    printBoard(bo)
    while True:
        [move_x,move_y] = AIMove_lv1(bo)
        bo[move_x][move_y] = CHECK_O
        
        AI1_win = 0
        if isWinner(bo, move_x, move_y, k, CHECK_O) == True: 
            print("AI1 has won! ")
            AI1_win +=1
            break     
        #AI move
        [move_x,move_y] = AIMove_lv2(bo)
        bo[move_x][move_y] = CHECK_X
        
        AI2_win = 0
        if isWinner(bo, move_x, move_y, k, CHECK_X) == True: 
            print("AI0 has won ")
            AI2_win +=1
            break     
        printBoard(bo)
        #if board is full, break 
        if isBoardFull(bo) == True:
            print('Board is full, end of game')
            break
printBoard(bo)     

##Game3.3 AI2 vs. AI1 ##
for i in range(500):
    bo = initBoard(4, 3)
    printBoard(bo)
    while True:
        [move_x,move_y] = AIMove_lv1(bo)
        bo[move_x][move_y] = CHECK_O
        
        AI1_win = 0
        if isWinner(bo, move_x, move_y, k, CHECK_O) == True: 
            print("AI1 has won! ")
            AI1_win +=1
            break     
        #AI move
        [move_x,move_y] = AIMove_lv2(bo)
        bo[move_x][move_y] = CHECK_X
        
        AI2_win = 0
        if isWinner(bo, move_x, move_y, k, CHECK_X) == True: 
            print("AI0 has won ")
            AI2_win +=1
            break     
        printBoard(bo)
        #if board is full, break 
        if isBoardFull(bo) == True:
            print('Board is full, end of game')
            break
printBoard(bo)     