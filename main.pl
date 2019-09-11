:- include("get_hand_helper.pl").
:- include("predict_opponents_rank_helper.pl").

deck([
 [ace,heart],[ace,diamond],[ace,club],[ace,spade],
 [2,heart],[2,diamond],[2,club],[2,spade],
 [3,heart],[3,diamond],[3,club],[3,spade],
 [4,heart],[4,diamond],[4,club],[4,spade],
 [5,heart],[5,diamond],[5,club],[5,spade],
 [6,heart],[6,diamond],[6,club],[6,spade],
 [7,heart],[7,diamond],[7,club],[7,spade],
 [8,heart],[8,diamond],[8,club],[8,spade],
 [9,heart],[9,diamond],[9,club],[9,spade],
 [10,heart],[10,diamond],[10,club],[10,spade],
 [jack,heart],[jack,diamond],[jack,club],[jack,spade],
 [queen,heart],[queen,diamond],[queen,club],[queen,spade],
 [king,heart],[king,diamond],[king,club],[king,spade]
]).

print_separators :-
	nl, writeln("==============================================================================").

print_light_separators :-
	nl, writeln("------------------------------------------------------------------------------").

initial_setup :-
	initialize_table,
	initialize_accuracy_matrices.

initialize_table:-
	set_table(10, 0, 0, 10).

initialize_accuracy_matrices :-

	nb_setval(p1_win_pos, 0), nb_setval(p1_loose_pos, 0),
	nb_setval(p2_win_pos, 0), nb_setval(p2_loose_pos, 0).

get_table(P1, T1, T2, P2) :-
	
	nb_getval(p1_total_chip, P1), nb_getval(p2_total_chip, P2),
	nb_getval(p1_table_chip, T1), nb_getval(p2_table_chip, T2).

set_table(P1, T1, T2, P2) :-
	
	nb_setval(p1_total_chip, P1), nb_setval(p2_total_chip, P2),
	nb_setval(p1_table_chip, T1), nb_setval(p2_table_chip, T2).

update_table(B1, B2) :-
	
	get_table(P1, T1, T2, P2),
	X is P1 - B1,
	Y is T1 + B1,
	Z is T2 + B2,
	W is P2 - B2,
	set_table(X, Y, Z, W).

make_winner(Player, Winner) :-

	Winner = Player,
	get_table(P1, T1, T2, P2),

	X is P1 + T1 + T2,
	Y is P2 + T1 + T2,

	(
		Player = "P1" ->
			set_table(X, 0, 0, P2); set_table(P1, 0, 0, Y)
	).
	
display_table :-

	get_table(P1, T1, T2, P2),
	nl, nl, format('~*t ~51|~n', [46]),
	format('~w ~*t ~w ~*t ~w~50|~n', ['P1', 32, 'Table', 32, 'P2']),
	format('~*t ~51|~n', [46]),
	format('~d ~*t ~d-~d ~*t ~d~50|~n', [P1, 32, T1, T2, 32, P2]),
	format('~*t ~51|~n', [46]), nl, nl.

shuffle_all_cards(Deck) :- 
	deck(P),
	%writeln("Unshuffled deck of cards:"), nl,
	%write(P), nl, nl,
	random_permutation(P, Deck).
	%print_separators,
	%writeln("Shuffled deck of cards: "), nl,
	%write(Deck), nl, nl.

deal_cards(Deck, P1_Hand, P2_Hand, Card_On_Table, 
	P1_FaceUp_Cards, P2_FaceUp_Cards) :-

	[H1_1|T1_1] = Deck,	[H2_1|T2_1] = T1_1,	[H1_2|T1_2] = T2_1,
	[H2_2|T2_2] = T1_2,	[H1_3|T1_3] = T2_2,	[H2_3|T2_3] = T1_3,
	[H1_4|T1_4] = T2_3,	[H2_4|T2_4] = T1_4,	[H1_5|T1_5] = T2_4,
	[H2_5|Card_On_Table] = T1_5,

	P1_Hand = [H1_1, H1_2, H1_3, H1_4, H1_5],
	P2_Hand = [H2_1, H2_2, H2_3, H2_4, H2_5],
	P1_FaceUp_Cards = [H1_1, H1_2, H1_3],
	P2_FaceUp_Cards = [H2_1, H2_2, H2_3].

validate_guesses(P1_Rank, P2_Rank, 
	P1_Heuristic_Rank, P2_Heuristic_Rank, Winner) :-

	(
		P1_Rank < P2_Heuristic_Rank -> 
			write("Payer1 guesses that he might win the game."), 
				P1_Winning_Guess = true, P1_Tie_Guess = false, P1_Losing_Guess = false;
		(
			P1_Rank =:= P2_Heuristic_Rank -> 
				write("P1 guesses tie."), P1_Winning_Guess = false, P1_Tie_Guess = true, P1_Losing_Guess = false; 
			write("Payer1 guesses that he may lose the game."), P1_Winning_Guess = false, P1_Tie_Guess = false, P1_Losing_Guess = true
		)
	), nl,

	(
		P2_Rank < P1_Heuristic_Rank -> 
			write("Payer2 guesses that he might win the game."), P2_Winning_Guess = true, P2_Tie_Guess = false, P2_Losing_Guess = false;
		(
			P2_Rank =:= P1_Heuristic_Rank -> 
				write("P2 guesses tie."), P2_Winning_Guess = false, P2_Tie_Guess = true, P2_Losing_Guess = false; 
			write("Payer2 guesses that he may lose the game."), P2_Winning_Guess = false, P2_Tie_Guess = false, P2_Losing_Guess = true
		)
	),

	print_light_separators,
	write("Actual winner is: "), write(Winner), write(". Hence: "),
	print_light_separators,

	nb_getval(p1_win_pos, P1_TP), nb_getval(p1_loose_pos, P1_LP),
	nb_getval(p2_win_pos, P2_TP), nb_getval(p2_loose_pos, P2_LP),

	(
		Winner = "P1" -> 
			(
				P1_Winning_Guess = true -> writeln("P1's winning guess is true"), 
					TEMP1 is P1_TP + 1, nb_setval(p1_win_pos, TEMP1);
					writeln("P1's guess is false")
			);
			(
				Winner = "Tie" ->
				(
					P1_Tie_Guess = true -> writeln("P1's tie guess is true"),
						TEMP2 is P1_TP + 1, nb_setval(p1_win_pos, TEMP2);
						writeln("P1's guess is false")
				);
				(
					P1_Losing_Guess = true -> writeln("P1's losing guess is correct"),
						TEMP3 is P1_LP + 1, nb_setval(p1_loose_pos, TEMP3);
						writeln("P1's guess is false")
				)
			)
	),

	(
		Winner = "P2" -> 
			(
				P2_Winning_Guess = true -> write("P2's winning guess is true"),
					TEMP4 is P2_TP + 1, nb_setval(p2_win_pos, TEMP4);
					write("P2's guess is false")
			);
			(
				Winner = "Tie" ->
				(
					P2_Tie_Guess = true -> write("P2's tie guess is true"),
						TEMP5 is P2_TP + 1, nb_setval(p2_win_pos, TEMP5);
						write("P2's guess is false")
				);
				(
					P2_Losing_Guess = true -> write("P2's losing guess is correct"),
						TEMP6 is P2_LP + 1, nb_setval(p2_loose_pos, TEMP6);
						write("P2's guess is false")
				)
			)
	).

display_accuracy_matrices(N) :-
	nb_getval(p1_win_pos, P1_WP), nb_getval(p1_loose_pos, P1_LP),
	nb_getval(p2_win_pos, P2_WP), nb_getval(p2_loose_pos, P2_LP),
	print_separators,
	P1_Mat is (P1_WP + P1_LP) / N,
	P2_Mat is (P2_WP + P2_LP) / N,
	write("P1's Accuracy: "), writeln(P1_Mat),
	write("P2's Accuracy: "), writeln(P2_Mat).

chance_of_winning(P1_Rank, P2_Heuristic_Rank, P1, P2) :-
	(
		P1_Rank < P2_Heuristic_Rank -> writeln(P1);
		P1_Rank > P2_Heuristic_Rank -> writeln(P2);
		P1_Rank =:= P2_Heuristic_Rank -> writeln("Cannot Say!"); true
	).

start_game :-
	
	get_table(P1, T1, T2, P2),
	((P1 < 1; P2 < 1) -> 
		write("Game Over! A player has run out of cash."),
		display_table;

	print_separators, write("Table before start of the game:"),
	display_table,
	writeln("Game about to start, each player place 1 chip on the table to participate..."),
	update_table(1, 1),
	display_table,

	%% Distribute cards to each player

	deck(P),
	shuffle_all_cards(Deck),
	deal_cards(Deck, P1_Hand, P2_Hand, Rem_Card_On_Table,
		P1_FaceUp_Cards, P2_FaceUp_Cards),
	get_best_hand(P1_Hand, Best_Hand_P1, P1_Rank),
	get_best_hand(P2_Hand, Best_Hand_P2, P2_Rank),

	append(P1_Hand, P2_FaceUp_Cards, P1_Known_Cards),
	append(P2_Hand, P1_FaceUp_Cards, P2_Known_Cards),
	subtract(P, P1_Known_Cards, P1_Unknown_Cards),
	subtract(P, P2_Known_Cards, P2_Unknown_Cards),
	guess_opponents_rank(P2_FaceUp_Cards, P1_Unknown_Cards, P2_Ranks_List),
	guess_opponents_rank(P1_FaceUp_Cards, P2_Unknown_Cards, P1_Ranks_List),

	play_game(P1_Hand, P1_Rank, P1_Unknown_Cards, P1_Ranks_List,
		P2_Hand, P2_Rank, P2_Ranks_List, P2_FaceUp_Cards, 0, 0, 0, Winner),


	print_separators, nl, write("WINNER AT THE TABLE: "), writeln(Winner),


	%% DISPLAYING MATRICES...

	get_smart_rank_heuristic1(P1_Hand, P1_Rank, P1_Unknown_Cards, 
		P2_Ranks_List, P2_FaceUp_Cards, P2_Heuristic_Rank, 10, 0, _),

	get_smart_rank_heuristic2(P2_Rank, P1_Ranks_List, 0.20, 
		P1_Heuristic_Rank, 10, 0, _),

	print_separators,
	write("P1 Full Hand: "), writeln(P1_Hand),
	write("P1 Face up Cards: "), writeln(P1_FaceUp_Cards),
	write("P1 Best Hand: "), write(Best_Hand_P1), 
	write(", Rank: "), writeln(P1_Rank),
	write("P1's estimated heuristic rank for P2: "), writeln(P2_Heuristic_Rank),
	write("Chance of winning: "), chance_of_winning(P1_Rank, P2_Heuristic_Rank, "P1", "P2"),

	print_separators,
	write("P2 Full Hand: "), writeln(P2_Hand),
	write("P2 Face up cards: "), writeln(P2_FaceUp_Cards),
	write("P2 Best Hand: "), write(Best_Hand_P2), 
	write(", Rank: "), writeln(P2_Rank),
	write("P2's estimated heuristic rank for P1: "), writeln(P1_Heuristic_Rank),
	write("Chance of winning: "), chance_of_winning(P2_Rank, P1_Heuristic_Rank, "P2", "P1"),
	print_separators, nl,
	find_winner(P1_Rank, P2_Rank, P1_Hand, P2_Hand, Winning_Player_Temp),
	write("WINNER OF THE GAME(BY HAND RANK): "), writeln(Winning_Player_Temp),
	print_separators,

	start_game
	).

play_game(P1_Hand, P1_Rank, P1_Unknown_Cards, P1_Ranks_List, 
			P2_Hand, P2_Rank, P2_Ranks_List, P2_FaceUp_Cards, Raised_Bet_Amt, 
				P1_Last_Bet, P2_Last_Bet, Winner) :-
	
	get_table(P1, T1, T2, P2),

	get_smart_rank_heuristic1(P1_Hand, P1_Rank, P1_Unknown_Cards, 
		P2_Ranks_List, P2_FaceUp_Cards, P2_Heuristic_Rank, P1, P2_Last_Bet, B1),

	get_smart_rank_heuristic2(P2_Rank, P1_Ranks_List, 0.20, P1_Heuristic_Rank, P2, P1_Last_Bet, B2),

	(

		(P1 =:= 0, P2 =:= 0) -> format("Both player ran out of chips. Time to find the winner."),
								find_winner(P1_Rank, P2_Rank, P1_Hand, P2_Hand, Winning_Player),
								make_winner(Winning_Player, Winner);

		P1 =:= 0 -> format("P1 ran out of chips."), make_winner("P2", Winner);

		P2 =:= 0 -> format("P2 ran out of chips."), make_winner("P1", Winner);		

		Raised_Bet_Amt > B1 ->  format("P1 decides not to match the increased bet and folds."),
								make_winner("P2", Winner);

		B1 =:= 0 -> writeln("P1 decides to fold."), make_winner("P2", Winner);

		B2 =:= 0 -> writeln("P2 decides to fold"), make_winner("P1", Winner);

		B1 =:= B2 -> format('P1 bets ~d coins. ', [B1]),
					 format('P2 also bets ~d coins.', [B2]),
					 update_table(B1, B2), 
					 display_table,
					 play_game(P1_Hand, P1_Rank, P1_Unknown_Cards, P1_Ranks_List, 
						P2_Hand, P2_Rank, P2_Ranks_List, P2_FaceUp_Cards, Raised_Bet_Amt, B1, B2, Winner);

		B1 > B2 ->  format('P1 bets ~d coins. ', [B1]),
					format('P2 decides to fold.'),
					make_winner("P1", Winner);

		B2 > B1 ->  format('P1 bets ~d coins. ', [B1]),
					format('P2 raises the bet and put ~d coins on table.', [B2]),
					update_table(B1, B2), 
					display_table,
					play_game(P1_Hand, P1_Rank, P1_Unknown_Cards, P1_Ranks_List, 
						P2_Hand, P2_Rank, P2_Ranks_List, P2_FaceUp_Cards, B2, B1, B2, Winner)
	).

play_with_smart_heuristic :-
	print_separators,

	nb_setval(p1_total_chip, 10),
	nb_setval(p2_total_chip, 10),
	nb_setval(p1_table_chip, 0),
	nb_setval(p2_table_chip, 0),

	display_table,
	writeln("Game about to start, each player place 1 chip on the table to participate..."),
	display_table,
	deck(P),
	shuffle_all_cards(Deck),
	deal_cards(Deck, P1_Hand, P2_Hand, Rem_Card_On_Table,
		P1_FaceUp_Cards, P2_FaceUp_Cards),
	get_best_hand(P1_Hand, Best_Hand_P1, P1_Rank),
	get_best_hand(P2_Hand, Best_Hand_P2, P2_Rank),

	print_separators,
	write("P1 Full Hand: "), writeln(P1_Hand),
	write("P1 Face up Cards: "), writeln(P1_FaceUp_Cards),
	write("P1 Best Hand: "), write(Best_Hand_P1), write(", Rank: "), writeln(P1_Rank),
	print_separators,
	write("P2 Full Hand: "), writeln(P2_Hand),
	write("P2 Face up cards: "), writeln(P2_FaceUp_Cards),
	write("P2 Best Hand: "), write(Best_Hand_P2), write(", Rank: "), writeln(P2_Rank),

	append(P1_Hand, P2_FaceUp_Cards, P1_Known_Cards),
	append(P2_Hand, P1_FaceUp_Cards, P2_Known_Cards),
	subtract(P, P1_Known_Cards, P1_Unknown_Cards),
	subtract(P, P2_Known_Cards, P2_Unknown_Cards),

	print_separators,
	writeln("P1's estimate of P2's possible card ranks:"), nl, nl,
	guess_opponents_rank(P2_FaceUp_Cards, P1_Unknown_Cards, P2_Ranks_List), nl, nl,

	get_smart_rank_heuristic1(P1_Hand, P1_Rank, P1_Unknown_Cards, 
		P2_Ranks_List, P2_FaceUp_Cards, P2_Heuristic_Rank),

	write("P1's estimated smart heuristic rank for P2: "),
	writeln(P2_Heuristic_Rank),

	print_separators,
	writeln("P2's estimate of P1's possible card ranks:"), nl, nl,
	guess_opponents_rank(P1_FaceUp_Cards, P2_Unknown_Cards, P1_Ranks_List), nl, nl,
	get_smart_rank_heuristic2(P2_Rank, P1_Ranks_List, 0.20, P1_Heuristic_Rank),
	write("P2's estimated smart heuristic rank for P1: "),
	writeln(P1_Heuristic_Rank),

	print_separators,
	find_winner(P1_Rank, P2_Rank, P1_Hand, P2_Hand, Winner),
	validate_guesses(P1_Rank, P2_Rank, 
		P1_Heuristic_Rank, P2_Heuristic_Rank, Winner).

play_poker_smart_heuristic(0).
play_poker_smart_heuristic(N) :-
	%protocol("a4part2.pl"),
	N > 0, initialize_table, start_game,
	X is N-1, play_poker_smart_heuristic(X), !.
