comb2(_,[]).
comb2([X|T],[X|Comb]):-comb2(T,Comb).
comb2([_|T],[X|Comb]):-comb2(T,[X|Comb]).

initialize :-
	nb_setval(royal_flush_count, 0),
	nb_setval(straight_flush_count, 0),
	nb_setval(four_kind_count, 0),
	nb_setval(full_house_count, 0),
	nb_setval(flush_count, 0),
	nb_setval(straight_count, 0),
	nb_setval(three_kind_count, 0),
	nb_setval(two_pair_count, 0),
	nb_setval(one_pair_count, 0),
	nb_setval(no_pair_count, 0).

display_total_ranks :-
	write("Possible rank=1 or royal_flush_count: "), nb_getval(royal_flush_count, I), write(I), write(", Probability = (Count / 44C2) = "), format('~3f', [I rdiv 946]), nl,
	write("Possible rank=2 or straight_flush_count: "), nb_getval(straight_flush_count, J), write(J), write(", Probability = (Count / 44C2) = "), format('~3f', [J rdiv 946]), nl,
	write("Possible rank=3 or four_kind_count: "), nb_getval(four_kind_count, K), write(K), write(", Probability = (Count / 44C2) = "), format('~3f', [K rdiv 946]), nl,
	write("Possible rank=4 or full_house_count: "), nb_getval(full_house_count, L), write(L), write(", Probability = (Count / 44C2) = "), format('~3f', [L rdiv 946]), nl,
	write("Possible rank=5 or flush_count: "), nb_getval(flush_count, M), write(M), write(", Probability = (Count / 44C2) = "), format('~3f', [M rdiv 946]), nl,
	write("Possible rank=6 or straight_count: "), nb_getval(straight_count, N), write(N), write(", Probability = (Count / 44C2) = "), format('~3f', [N rdiv 946]), nl,
	write("Possible rank=7 or three_kind_count: "), nb_getval(three_kind_count, O), write(O), write(", Probability = (Count / 44C2) = "), format('~3f', [O rdiv 946]), nl,
	write("Possible rank=8 or two_pair_count: "), nb_getval(two_pair_count, P), write(P), write(", Probability = (Count / 44C2) = "), format('~3f', [P rdiv 946]), nl,
	write("Possible rank=9 or one_pair_count: "), nb_getval(one_pair_count, Q), write(Q), write(", Probability = (Count / 44C2) = "), format('~3f', [Q rdiv 946]), nl,
	write("Possible rank=10 or no_pair_count: "), nb_getval(no_pair_count, R), write(R), write(", Probability = (Count / 44C2) = "), format('~3f', [R rdiv 946]), nl.

get_numbers_only_hand([], _).
get_numbers_only_hand([One_Card|Rem_Card], [X|Y]) :-
	nth0(0, One_Card, Card_Num),
	get_numeric_rep(Card_Num, X),
	get_numbers_only_hand(Rem_Card, Y). 

get_ranks_list(Rank_List) :-
	nb_getval(royal_flush_count, I), nb_getval(straight_flush_count, J),
	nb_getval(four_kind_count, K), nb_getval(full_house_count, L),
	nb_getval(flush_count, M), nb_getval(straight_count, N),
	nb_getval(three_kind_count, O), nb_getval(two_pair_count, P),
	nb_getval(one_pair_count, Q), nb_getval(no_pair_count, R),
	Rank_List = [I, J, K, L, M, N, O, P, Q, R].

get_normal_heuristic_mean_rank(Ranks_List, Mean_Heuristic_Rank) :-

	nth1(1, Ranks_List, I), nth1(2, Ranks_List, J), nth1(3, Ranks_List, K), nth1(4, Ranks_List, L),
	nth1(5, Ranks_List, M), nth1(6, Ranks_List, N), nth1(7, Ranks_List, O), nth1(8, Ranks_List, P),
	nth1(9, Ranks_List, Q), nth1(10, Ranks_List, R),

	Total_Freq is (I*1 + J*2 + K*3 + L*4 + M*5 + N*6 + O*7 + P*8 + Q*9 + R*10),
	Total_Count is (I + J + K + L + M + N + O + P + Q + R),
	Mean_Heuristic_Rank is Total_Freq / Total_Count.

get_normal_heuristic_highest_prob_rank(Ranks_List, Highest_Prob_Heuristic_Rank) :-
	get_max_element_index(Ranks_List, Highest_Prob_Heuristic_Rank).

get_smart_heuristic_mean_rank(Ranks_List, Heuristic_Rank) :-

	get_normal_heuristic_mean_rank(Ranks_List, Mean_Rank),
	X is float_integer_part(Mean_Rank), Y is float_fractional_part(Mean_Rank),
	(Y > 0.5 -> Heuristic_Rank is X + 1; Heuristic_Rank is X).

get_avg_sum(Cards_List, Avg) :-
	sumlist(Cards_List, Sum),
	length(Cards_List, Len),
	Avg is (Sum/Len).

get_bet_amount(Coins_At_Hand, Expected_Amount, Actual_Amount) :-
	%nl, write("Coins_At_Hand: "), writeln(Coins_At_Hand),
	%write("Expected Amount: "), writeln(Expected_Amount),
	(
		Coins_At_Hand >= Expected_Amount -> Actual_Amount is Expected_Amount;
			Actual_Amount is Coins_At_Hand
	). 
	%write("Actual Amount: "), writeln(Actual_Amount).

get_smart_rank_heuristic1(P1_Hand, P1_Rank, P1_Unknown_Cards, 
	P2_Ranks_List, P2_FaceUp_Cards, P2_Heuristic_Rank, Coins_At_Hand, P2_Last_Bet, Bet_Amt) :-

	get_smart_heuristic_mean_rank(P2_Ranks_List, P2_Temp_Heuristic_Rank),

	(
		P1_Rank < P2_Temp_Heuristic_Rank -> P2_Heuristic_Rank = P2_Temp_Heuristic_Rank, 
			get_bet_amount(Coins_At_Hand, 3, Bet_Amt); 

		P1_Rank > P2_Temp_Heuristic_Rank -> P2_Heuristic_Rank = P2_Temp_Heuristic_Rank, 
			get_bet_amount(Coins_At_Hand, 0, Bet_Amt);

		P1_Rank =:= P2_Temp_Heuristic_Rank -> 
			get_numbers_only_hand(P2_FaceUp_Cards, P2_FaceUp_No_Only),
			sumlist(P2_FaceUp_No_Only, P2_3Cards_Sum), 
			get_numbers_only_hand(P1_Unknown_Cards, P1_Unknown_Cards_No_Only),
			get_avg_sum(P1_Unknown_Cards_No_Only, P2_2Cards_Avg),
			P2_Total_Expected_Sum is P2_3Cards_Sum + P2_2Cards_Avg + P2_2Cards_Avg,
			get_numbers_only_hand(P1_Hand, P1_Hand_No_Only),
			sumlist(P1_Hand_No_Only, P1_5Cards_Sum),

			(
				P2_Total_Expected_Sum >= P1_5Cards_Sum -> P2_Heuristic_Rank = P2_Temp_Heuristic_Rank, 
					get_bet_amount(Coins_At_Hand, 1, Bet_Amt); 
					P2_Heuristic_Rank = P2_Temp_Heuristic_Rank, get_bet_amount(Coins_At_Hand, 2, Bet_Amt)
			)
	),

	(
		(
			Bet_Amt =< P2_Last_Bet, 
			(P1_Rank < P2_Temp_Heuristic_Rank; P1_Rank =:= P2_Temp_Heuristic_Rank),
			Bet_Amt > Coins_At_Hand
		) -> Bet_Amt = P2_Last_Bet; true
	).

	%write("Bet_Amt: "), writeln(Bet_Amt).

get_smart_rank_heuristic2(P1_Rank, P2_Ranks_List, Threshold, P2_Heuristic_Rank, 
	Coins_At_Hand, P2_Last_Bet, Bet_Amt) :-
	
	get_normal_heuristic_highest_prob_rank(P2_Ranks_List, P2_Temp_Heuristic_Rank),

	(
		P1_Rank < P2_Temp_Heuristic_Rank -> P2_Heuristic_Rank = P2_Temp_Heuristic_Rank, get_bet_amount(Coins_At_Hand, 3, Bet_Amt);
		P1_Rank > P2_Temp_Heuristic_Rank -> P2_Heuristic_Rank = P2_Temp_Heuristic_Rank, get_bet_amount(Coins_At_Hand, 0, Bet_Amt);
		P1_Rank =:= P2_Temp_Heuristic_Rank -> 
			sort(P2_Ranks_List, Sorted_Ranks), 
			length(Sorted_Ranks, Len),
			Second_Largest is Len-1,
			nth1(Second_Largest, Sorted_Ranks, Second_Highest_Freq),
			sumlist(P2_Ranks_List, Total_Sum),
			Second_Highest_Prob is Second_Highest_Freq/Total_Sum,
			(
				Second_Highest_Prob > Threshold -> P2_Heuristic_Rank = P2_Temp_Heuristic_Rank , get_bet_amount(Coins_At_Hand, 1, Bet_Amt);
					P2_Heuristic_Rank = P2_Temp_Heuristic_Rank, get_bet_amount(Coins_At_Hand, 2, Bet_Amt)
			)
	),

	(
		(
			Bet_Amt =< P2_Last_Bet, 
			(P1_Rank < P2_Temp_Heuristic_Rank; P1_Rank =:= P2_Temp_Heuristic_Rank),
			Bet_Amt > Coins_At_Hand
		) -> Bet_Amt = P2_Last_Bet; true
	).
	%write("Bet amount:"), writeln(Bet_Amt).

get_max_element_index(List, Index) :- 
	nth1(Index, List, M), \+ (member(E, List), E > M).

update_rank(A) :-
	(
		A =:= 1 -> nb_getval(royal_flush_count, I), J is I + 1, nb_setval(royal_flush_count, J);
		A =:= 2 -> nb_getval(straight_flush_count, I), J is I + 1, nb_setval(straight_flush_count, J);
		A =:= 3 -> nb_getval(four_kind_count, I), J is I + 1, nb_setval(four_kind_count, J);
		A =:= 4 -> nb_getval(full_house_count, I), J is I + 1, nb_setval(full_house_count, J);
		A =:= 5 -> nb_getval(flush_count, I), J is I + 1, nb_setval(flush_count, J);
		A =:= 6 -> nb_getval(straight_count, I), J is I + 1, nb_setval(straight_count, J);
		A =:= 7 -> nb_getval(three_kind_count, I), J is I + 1, nb_setval(three_kind_count, J);
		A =:= 8 -> nb_getval(two_pair_count, I), J is I + 1, nb_setval(two_pair_count, J);
		A =:= 9 -> nb_getval(one_pair_count, I), J is I + 1, nb_setval(one_pair_count, J);
		A =:= 10 -> nb_getval(no_pair_count, I), J is I + 1, nb_setval(no_pair_count, J)
	).

calculate_prob(_, []).

calculate_prob(FaceUp_Cards, [Rem_2Cards | T]) :-
	append(FaceUp_Cards, Rem_2Cards, Opponents_Possible_Hand),
	get_best_hand(Opponents_Possible_Hand, _, Rank),
	update_rank(Rank),
	calculate_prob(FaceUp_Cards, T).
	
guess_opponents_rank(FaceUp_Cards, Rem_List_Cards, Ranks_List) :-
	initialize,
	findall([X,Y], comb2(Rem_List_Cards,[X,Y]), Pair),
	calculate_prob(FaceUp_Cards, Pair),
	%display_total_ranks,
	get_ranks_list(Ranks_List),
	get_normal_heuristic_mean_rank(Ranks_List, Heuristic_Rank).
	%heuristic_rank(Heuristic_Rank).


