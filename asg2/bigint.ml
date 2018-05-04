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
    
    (* Pass in a list and link each number in the whole list together
     * to form a total number *)
    let list_concat list =
        float_of_string (String.concat ""
            (List.rev_map string_of_int list))


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
        | car1::cdr1, car2::cdr2, carry ->
          let result = car1 - car2 - carry 
          in if result < 0
             then result + 10 :: sub' cdr1 cdr2 1
             else result :: sub' cdr1 cdr2 0
             (* then car1 + radix - car2 - carry :: sub' cdr1 cdr2 1
             else car1 - car2 - carry :: sub' cdr1 cdr2 0 *)
        | [], [], 0          -> failwith "sub'"

(* ******************************************************************************** *)
(* ******************************************************************************** *)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        (* If both numbers are +, then add them together *)
        (* If both numbers are -, it's the same as addition of both numbers and make the result negative.*)
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        (* Else if num1 > num2, +(num1 - num2). Else, +(num2 - num1) *)
        else if list_concat value1 > list_concat value2
        then Bigint (neg1, sub' value1 value2 0)
        else Bigint (neg2, sub' value2 value1 0)

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        match neg1, neg2 with
        | Pos, Pos  ->
            (* If greater - less, result would be +(greater - less) *)
            if list_concat value1 > list_concat value2
            then Bigint (neg1, sub' value1 value2 0)
            (* If a less - greater, result would be -(greater - less) *)
            else Bigint (Neg , sub' value2 value1 0)
        | Pos, Neg  ->
            (* a number minus any negative number also means two numbers adding each other *)
            Bigint (neg1, add' value1 value2 0)
        | Neg, Pos  ->
            (* a negative num minus a positive num means -((+num) + (+num)) *)
            Bigint (neg1, add' value1 value2 0)
        | Neg, Neg  ->
            (* Make sure the larger number is in the front. *)
            if list_concat value1 > list_concat value2
            then Bigint (Pos, sub' value2 value1 0)
            else Bigint (Neg, sub' value1 value2 0)


(*
    let rec mul' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else zero

(* ////////////////////////////////// *)

    let rec div' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else zero


(* ////////////////////////////////// *)

    let rec rem' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else zero

(* ////////////////////////////////// *)

    let rec pow' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else zero *)


(* ******************************************************************************** *)
    (* let sub = add *)
    let mul = add
    let div = add
    let rem = add
    let pow = add


end
