get_numeric_rep(A,X):- 
	(
		A==ace->X=14;
		A==king->X=13;
		A==queen->X=12;
		A==jack->X=11;
		true->X=A
	).

is_royal_flush(Hand, FLUSH, RESULT) :-

	[X, Y, Z, T, U] = Hand,

	(
		X =:= 14, Y =:= 13, Z =:= 12, T =:= 11, U =:= 10, FLUSH = true -> RESULT = true; RESULT = false
	).

is_straight_flush(Hand, FLUSH, RESULT, X) :-
	
	[X, Y, Z, T, U] = Hand,

	(
		X-Y =:= 1, Y-Z =:= 1, Z-T =:= 1, T-U =:= 1, FLUSH = true -> RESULT = true; RESULT = false
	).

is_four_of_a_kind(Hand, RESULT, M) :-
	
	(
		Hand = [M,M,M,M,N];
		Hand = [N,M,M,M,M] -> RESULT = true; RESULT = false
	).

is_full_house(Hand, RESULT, M) :-

	(
		Hand = [M,M,M,N,N];
		Hand = [M,M,N,N,N] -> RESULT = true; RESULT = false
	).

is_straight(Hand, RESULT, X) :-
	
	[X, Y, Z, T, U] = Hand,
	(	
		X-Y =:= 1, Y-Z =:= 1, Z-T =:= 1, T-U =:= 1 -> RESULT = true; RESULT = false
	).

is_three_of_a_kind(Hand, RESULT, M) :-

	(
		Hand = [M,M,M,N,P];
		Hand = [P,M,M,M,N];
		Hand = [P,N,M,M,M] -> RESULT = true; RESULT = false
	).

is_two_pair(Hand, RESULT, M) :-

	(
		Hand = [M,M,N,N,P];
		Hand = [M,M,P,N,N];
		Hand = [P,M,M,N,N] -> RESULT = true; RESULT = false
	).

is_one_pair(Hand, RESULT, M) :-

	(
		Hand = [M,M,P,Q,R];
		Hand = [P,M,M,Q,R];
		Hand = [P,Q,M,M,R];
		Hand = [P,Q,R,M,M] -> RESULT = true; RESULT = false
	).

is_no_pair(Hand, RESULT) :-

	(
		Hand = [M,N,P,Q,R], M\=N, N\=P, P\=Q, Q\=R -> RESULT = true; RESULT = false
	).

get_flush_details(Hand, Flush) :-

	[A, B, C, D, E] = Hand,

	nth0(1, A, COL_A), nth0(1, B, COL_B),
	nth0(1, C, COL_C), nth0(1, D, COL_D), nth0(1, E, COL_E),

	((COL_A=COL_B,COL_B=COL_C,COL_C=COL_D,COL_D=COL_E) -> Flush = true; Flush = false).

high_sum_winner(P1_Hand, P2_Hand, Winner) :-

	get_hand_sum(P1_Hand, Sum1),
	get_hand_sum(P2_Hand, Sum2),

	(
		Sum1 > Sum2 -> Winner = "P1";
		Sum2 > Sum1 -> Winner = "P2";
		Sum1 =:= Sum2 -> Winner = "Tie"; true
	).

high(P1_High, P2_High, P1_Hand, P2_Hand, Winner) :-

	(
		P1_High > P2_High -> Winner = "P1";
		P1_High < P2_High -> Winner = "P2";
		P1_High =:= P2_High -> high_sum_winner(P1_Hand, P2_Hand, Winner); true
	).

decide_winner_on_rank_tie(Rank, Hand1, Hand2, Winner) :-
	
	get_sorted_list(Hand1, P1_Hand),
	get_sorted_list(Hand2, P2_Hand),

	(
		(Rank =:= 10; Rank =:= 5) -> high_sum_winner(Hand1, Hand2, Winner);

		Rank =:= 9 -> is_one_pair(P1_Hand, _, High1),
					  is_one_pair(P2_Hand, _, High2),
					  high(High1, High2, Hand1, Hand2, Winner);

		Rank =:= 8 -> is_two_pair(P1_Hand, _, High1),
					  is_two_pair(P2_Hand, _, High2),
					  high(High1, High2, Hand1, Hand2, Winner);

		Rank =:= 7 -> is_three_of_a_kind(P1_Hand, _, High1),
					  is_three_of_a_kind(P2_Hand, _, High2),
					  high(High1, High2, Hand1, Hand2, Winner);

		Rank =:= 6 -> is_straight(P1_Hand, _, High1),
					  is_straight(P2_Hand, _, High2),
					  high(High1, High2, Hand1, Hand2, Winner);

		Rank =:= 4 -> is_full_house(P1_Hand, _, High1),
					  is_full_house(P2_Hand, _, High2),
					  high(High1, High2, Hand1, Hand2, Winner);

		Rank =:= 3 -> is_four_of_a_kind(P1_Hand, _, High1),
					  is_four_of_a_kind(P2_Hand, _, High2),
					  high(High1, High2, Hand1, Hand2, Winner);

		Rank =:= 2 -> is_straight_flush(P1_Hand, true, _, High1),
					  is_straight_flush(P2_Hand, true, _, High2),
					  high(High1, High2, Hand1, Hand2, Winner);

		Rank =:= 1 -> Winner = "Tie"; true
	).

find_winner(P1_Rank, P2_Rank, P1_Hand, P2_Hand, Winner) :-
	
	(
		P1_Rank < P2_Rank -> Winner = "P1";
		P1_Rank > P2_Rank -> Winner = "P2";
		P1_Rank =:= P2_Rank -> decide_winner_on_rank_tie(P1_Rank, P1_Hand, P2_Hand, Winner); true
	).


get_numeric_hand(Hand, Numeric_Hand) :-
	
	[A, B, C, D, E] = Hand,

	nth0(0, A, NUM_A), nth0(0, B, NUM_B),
	nth0(0, C, NUM_C), nth0(0, D, NUM_D), nth0(0, E, NUM_E),

	get_numeric_rep(NUM_A,NUM_AA), get_numeric_rep(NUM_B,NUM_BB), 
	get_numeric_rep(NUM_C,NUM_CC), get_numeric_rep(NUM_D,NUM_DD), 
	get_numeric_rep(NUM_E,NUM_EE),

	Numeric_Hand = [NUM_AA, NUM_BB, NUM_CC, NUM_DD, NUM_EE].

get_sorted_list(Hand, SORTED_LIST) :-

	get_numeric_hand(Hand, X),
	sort(0, @>=,X, SORTED_LIST).

get_hand_helper(Hand, Flush, Hand_Type, Rank) :- 
	
	get_sorted_list(Hand, SORTED_LIST),

	(
		is_royal_flush(SORTED_LIST,Flush,RESULT), RESULT = true -> Hand_Type = "ROYAL FLUSH", Rank = 1; 
		is_straight_flush(SORTED_LIST,Flush,RESULT,_), RESULT = true -> Hand_Type = "STRAIGHT FLUSH", Rank = 2;
		is_four_of_a_kind(SORTED_LIST,RESULT,_), RESULT = true -> Hand_Type = "FOUR OF A KIND", Rank = 3;
		is_full_house(SORTED_LIST,RESULT,_), RESULT = true -> Hand_Type = "FULL HOUSE", Rank = 4;
		Flush = true -> Hand_Type = "FLUSH", Rank = 5;
		is_straight(SORTED_LIST,RESULT,_),RESULT=true -> Hand_Type = "STRAIGHT", Rank = 6;
		is_three_of_a_kind(SORTED_LIST,RESULT,_), RESULT = true -> Hand_Type = "THREE OF A KIND", Rank = 7;
		is_two_pair(SORTED_LIST,RESULT,_), RESULT=true -> Hand_Type = "TWO PAIR", Rank = 8;
		is_one_pair(SORTED_LIST,RESULT,_), RESULT = true -> Hand_Type = "ONE PAIR", Rank = 9;
		is_no_pair(SORTED_LIST,RESULT), RESULT = true -> Hand_Type = "NO PAIR", Rank = 10;
		true -> Hand_Type = "NO HAND FOUND"
	).

get_best_hand(Player_Hand_List, Best_Hand, Rank) :-

	get_flush_details(Player_Hand_List, Flush),
	get_hand_helper(Player_Hand_List, Flush, Best_Hand, Rank).
	%write("The hand type: "), writeln(Best_Hand).

get_rank(Player_Hand_List, _, Rank) :-

	get_flush_details(Player_Hand_List, Flush),
	get_hand_helper(Player_Hand_List, Flush, Best_Hand, Rank).

get_hand_sum(Hand, Sum) :-

	get_numeric_hand(Hand, Numeric_Hand),
	sumlist(Numeric_Hand, Sum).