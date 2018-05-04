(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))
    

    (* Same functionalities as C's strcmp.
     * Move from low to high order digits tail recursively and stop at the end of the shorter list.
     * Return  0 == two lists are the same.
     * Return  1 == a value in list1 has greater ASC value than a value in list2.
     * Return -1 == the other way around.*)
    let rec stringcmp list1 list2 = match (list1, list2) with 
        | list1, []      ->  1
        | [], list2      -> -1
        | [], []         ->  0 
        | list1, list2   ->
            if (List.length list1) > (List.length list2)
            then 1
            else if (List.length list1) < (List.length list2)
            then -1
            else let r1 = reverse list1 in
                 let r2 = reverse list2 in
                    if (car r1) > (car r2)
                    then 1
                    else if (car r2) > (car r1)
                    then -1
                    else stringcmp (reverse (cdr r1)) (reverse (cdr r2))


(* ///////////////// *)
    (* Pattern Matching format:
     *      match value with
     *      | pattern   -> result
     *      | pattern   -> result
     *         ...
     *)
    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
            let result = car1 + car2 + carry
            in  result mod radix :: add' cdr1 cdr2 (result / radix)

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | list1, [], carry   -> sub' list1 [carry] 0
        | _, _, _            -> failwith "sub'"
        | car1::cdr1, car2::cdr2, carry ->
            let result = car1 - car2 - carry 
            in if result < 0
               then result + 10 :: sub' cdr1 cdr2 1
               else result :: sub' cdr1 cdr2 0
             (* then car1 + radix - car2 - carry :: sub' cdr1 cdr2 1
             else car1 - car2 - carry :: sub' cdr1 cdr2 0 *)


(* ******************************************************************************** *)
(* ******************************************************************************** *)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        (* If both numbers are +, then add them together *)
        (* If both numbers are -, it's the same as addition of both numbers and make the result negative.*)
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        (* Else if num1 > num2, +(num1 - num2). Else, +(num2 - num1) *)
        else let cmp = stringcmp value1 value2 in 
            if cmp > 0
            then Bigint (neg1, sub' value1 value2 0)
            else if cmp < 0
            then Bigint (neg2, sub' value2 value1 0)
            else zero

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        match neg1, neg2 with
        | Pos, Pos  ->
            (* If greater - less, result would be +(greater - less) *)
            let cmp = stringcmp value1 value2 in 
                if cmp > 0
                then Bigint (neg1, sub' value1 value2 0)
            (* If a less - greater, result would be -(greater - less) *)
                else if cmp < 0
                then Bigint (Neg , sub' value2 value1 0)
                else zero
        | Pos, Neg  ->
            (* a number minus any negative number also means two numbers adding each other *)
            Bigint (neg1, add' value1 value2 0)
        | Neg, Pos  ->
            (* a negative num minus a positive num means -((+num) + (+num)) *)
            Bigint (neg1, add' value1 value2 0)
        | Neg, Neg  ->
            (* Make sure the larger number is in the front. *)
            let cmp = stringcmp value1 value2 in 
                if cmp > 0
                then Bigint (Pos, sub' value2 value1 0)
                else if cmp < 0
                then Bigint (Neg, sub' value1 value2 0)
                else zero





(* ******************************************************************************** *)
    let mul = add
    let div = add
    let rem = add
    let pow = add


end
