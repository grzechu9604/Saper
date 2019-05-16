// Learn more about F# at http://fsharp.org

open System

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
                if (x < 0 || x > (Array2D.length1 board - 1) || y < 0 || y > (Array2D.length2 board - 1) || board.[x,y] <> -1) then 
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
    
            if (board.[rand_x, rand_y] = -1) then
                assign_mines mines_amount board
            else
                board.SetValue(-1, rand_x, rand_y)
                assign_mines (mines_amount - 1) board
     in
        let board = assign_mines mines_amount (Array2D.zeroCreate x y)
        calculate_mines_in_neighborhood_for_board board


let print_table table =
    for r = 0 to Array2D.length1 table - 1 do
        for c = 0 to Array2D.length2 table - 1 do
            if (table.[r, c] = -1) then 
                printf "*\t"
            else
                printf "%A\t" table.[r, c]
        printfn ""

let choose_game_variant =
    let game_variants = [(8, 8, 16); (16, 16, 40); (30, 16, 99)]
    let game_variant = get_int_in_range_from_input "Wybierz wariant gry od 1 do 3" 1 3
    game_variants.Item(game_variant - 1)

[<EntryPoint>]
let main argv =
    
    let variant = choose_game_variant
    let board = generate_board variant

    print_table board

    0 // return an integer exit code
