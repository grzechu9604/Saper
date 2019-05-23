// Learn more about F# at http://fsharp.org

open System

let closed_field_char = '#'
let mine_field_char = 'M'
let mine_field_value = -1
let third (_, _, c) = c

let rec get_int_in_range_from_input communicate min max =
    let get_int_from_input comunicate default_eror_value =
        try 
            printfn comunicate
            let x = System.Console.ReadLine(); 
            int(x)
        with 
            | :? System.FormatException -> 
                printfn "Invalid number!" 
                default_eror_value 
                in
    let value = get_int_from_input communicate (min - 1)
    if (value < min || value > max) then
        printfn "Wprowadź poprawną wartość z przedziału <%A, %A>" min max
        get_int_in_range_from_input communicate min max
    else
        value

let random_number_generator min max =
    (new System.Random()).Next(min, max)

let calculate_mines_in_neighborhood_for_board board =
    let rec _calculate_mines_in_neighborhood_for_board board index =
        let calculate_mines_in_neighborhood board x y =
            let is_it_mine board x y =
                if (x < 0 || x > (Array2D.length1 board - 1) || y < 0 || y > (Array2D.length2 board - 1) || board.[x,y] <> mine_field_value) then 
                    0 
                else  
                    1 
                in

                is_it_mine board (x - 1) (y - 1) + is_it_mine board (x - 1) y + is_it_mine board (x - 1) (y + 1) 
                + is_it_mine board x (y - 1) + is_it_mine board x (y + 1) 
                + is_it_mine board (x + 1) (y - 1) + is_it_mine board (x + 1) y + is_it_mine board (x + 1) (y + 1) 
            in

            if (index = Array2D.length1 board * Array2D.length2 board) then
                board
            else
                let x = index % (Array2D.length1 board)
                let y = index / (Array2D.length1 board)

                if (board.[x, y] <> -1) then
                    board.SetValue((calculate_mines_in_neighborhood board x y), x, y)

                _calculate_mines_in_neighborhood_for_board board (index + 1)

    in _calculate_mines_in_neighborhood_for_board board 0
 
let generate_board (x, y, mines_amount) =
    let rec assign_mines mines_amount board =
        if (mines_amount = 0) then 
            board
        else 
            let rand_x = random_number_generator 0 (Array2D.length1 board - 1)
            let rand_y = random_number_generator 0 (Array2D.length2 board - 1)
    
            if (board.[rand_x, rand_y] = mine_field_value) then
                assign_mines mines_amount board
            else
                board.SetValue(mine_field_value, rand_x, rand_y)
                assign_mines (mines_amount - 1) board
     in
        let board = assign_mines mines_amount (Array2D.zeroCreate x y)
        calculate_mines_in_neighborhood_for_board board

let print_table printing_func table =
    for x = 0 to Array2D.length1 table - 1 do
        for y = 0 to Array2D.length2 table - 1 do
            printing_func table x y
        printfn ""

let print_user_table table =
    let user_board_printer board x y =  
        printf "%O\t" (Array2D.get board x y) in
    print_table user_board_printer table

let print_mines_table table =
    let mines_board_printer board x y =
        if (Array2D.get board x y = mine_field_value) then 
            printf "*\t"
        else
            printf "%A\t" (Array2D.get board x y) in
    print_table mines_board_printer table

let choose_game_variant =
    let game_variants = [(8, 8, 16); (16, 16, 40); (30, 16, 99)]
    let game_variant = get_int_in_range_from_input "Wybierz wariant gry od 1 do 3" 1 3
    game_variants.Item(game_variant - 1)

let do_for_neighbours board user_board x y f =
    f board user_board (x - 1) (y - 1)
    f board user_board (x - 1) y
    f board user_board (x - 1) (y + 1)
    f board user_board x (y - 1)
    f board user_board x (y + 1)
    f board user_board (x + 1) (y - 1)
    f board user_board (x + 1) y
    f board user_board (x + 1) (y + 1)

let rec guess board user_board x y =
    if (x >= 0 && x < Array2D.length1 board && y >= 0 && y < Array2D.length2 board 
    && Array2D.get user_board x y = closed_field_char) then
        if (Array2D.get board x y = mine_field_value) then
            raise (System.Exception("MINA!!!"))
        else
            user_board.SetValue((char (Array2D.get board x y) + '0'), x, y)
            if (Array2D.get board x y = 0) then
                do_for_neighbours board user_board x y guess

let uncover_neighbours board user_board x y = 
    if (x >= 0 && x < Array2D.length1 board && y >= 0 && y < Array2D.length2 board 
    && Array2D.get user_board x y <> closed_field_char) then
        do_for_neighbours board user_board x y guess

let sign_as_mine _ user_board x y =
    if (x >= 0 && x < Array2D.length1 user_board && y >= 0 && y < Array2D.length2 user_board 
    && Array2D.get user_board x y = closed_field_char) then
        user_board.SetValue(mine_field_char, x, y)

let sign_as_not_mine _ user_board x y =
    if (x >= 0 && x < Array2D.length1 user_board && y >= 0 && y < Array2D.length2 user_board 
    && Array2D.get user_board x y = mine_field_char) then
        user_board.SetValue(closed_field_char, x, y)

let get_point_from_user max_x max_y =
    let x = get_int_in_range_from_input "wprowadź x" 0 max_x
    let y = get_int_in_range_from_input "wprowadź y" 0 max_y
    (x, y)

let choose_methode methodes communicate min_id max_id =
    let methode_id = get_int_in_range_from_input communicate min_id max_id
    List.item (methode_id - 1) methodes

let is_winner user_board amount_of_mines =
    let count_open_fields user_board = 
         let rec _count_open_fields user_board accum index = 
            if (index = Array2D.length1 user_board * Array2D.length2 user_board) then
                accum
            else
                let x = index % (Array2D.length1 user_board)
                let y = index / (Array2D.length1 user_board)

                if (Char.IsDigit user_board.[x, y]) then
                    _count_open_fields user_board (accum + 1) (index + 1)
                else
                    _count_open_fields user_board accum (index + 1)
         in
            _count_open_fields user_board 0 0
    in
        (Array2D.length1 user_board * Array2D.length2 user_board) - amount_of_mines = count_open_fields user_board
    

[<EntryPoint>]
let main argv =
    
    let variant = choose_game_variant
    let board = generate_board variant
    let user_board = Array2D.create<char> (Array2D.length1 board) (Array2D.length2 board) closed_field_char
    let methodes = [guess; sign_as_mine; sign_as_not_mine; uncover_neighbours]

    print_user_table user_board

    try
        while (not (is_winner user_board (third variant))) do
            let methode = choose_methode methodes "Odkryj pole (1) / Oznacz minę (2) / Odznacz minę(3) / Odkryj sąsiednie (4)" 1 4
            let point = get_point_from_user (Array2D.length1 board) (Array2D.length2 board)
            Console.Clear()
            methode board user_board (fst point) (snd point)
            print_user_table user_board

        printfn "Wygrałeś! Gratulacje!"

    with
        _ -> 
            Console.Clear()
            printfn "Przegrałeś! Wszedłeś na minę"
    
    print_mines_table board
    0
