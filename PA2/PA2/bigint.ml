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
    
    let delete_zeros list =
        let rec delete_zeros' list' = match list' with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                 let cdr' = delete_zeros' cdr
                 in  match car, cdr' with
                    | 0, [] -> []
                    | car, cdr' -> car::cdr'
        in delete_zeros' list

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else zero

    let rec cmp' list1 list2 = match (list1, list2) with
        | [], []                 ->  0
        | list1, []              ->  1
        | [], list2              -> -1
        | car1::cdr1, car2::cdr2 ->
          let retval = cmp' cdr1 cdr2
          in if retval = 0 && car1 != car2
          then (if car1 > car2
                then 1
                else (if car1 < car2
                then -1
                else 0))
             else retval

    let cmp (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        if neg1 = neg2
        then cmp' val1 val2
        else if neg1 = Neg
             then -1
             else 1

    let rec sub' list1 list2 s = match (list1, list2, s) with
        | list1, [], 0          -> list1
        | [], list2, 0          -> failwith "sub: list1 empty"
        | car1::cdr1, [], s  ->
          if car1 = 0
          then 9 :: (sub' cdr1 [] 1)
          else let dif = car1 - s * 1
              in dif :: (sub' cdr1 [] 0)
        | [], list2, s                 -> failwith "sub: list1 empty"
        | car1::cdr1, car2::cdr2, s ->

        if car2 > (car1 - s * 1)
        then let dif = ((car1 + 10) - s * 1) - car2
            in dif :: (sub' cdr1 cdr2 1)
        else let dif = (car1 - s * 1) - car2
            in dif :: (sub' cdr1 cdr2 0)

    let sub (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        if neg1 = neg2
        then (if (cmp' val2 val1) = 1
            then Bigint (Neg, delete_zeros(sub' val2 val1 0))
            else Bigint (Pos, delete_zeros(sub' val1 val2 0)))
        else
            (if (cmp' val1 val2) = 1
            then Bigint (neg2, add' val1 val2 0)
            else Bigint (neg1, add' val2 val1 0))

    let double_bigint bi = 
        add' bi bi 0

    let rec mul' (muler, pow2, mulcand') =
        if (cmp' pow2 muler) = 1
        then muler, []
        else let rmd, pdt =
            mul' (muler, double_bigint pow2,
                        double_bigint mulcand')
        in if (cmp' pow2 rmd) = 1
           then rmd, pdt
           else (delete_zeros(sub' rmd pow2 0)),
                (add' pdt mulcand' 0)

    let mul (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        let _, pdt =
            mul' (val1, [1], val2) in
                if neg1 = neg2
                then Bigint (Pos, pdt)
                else Bigint (Neg, pdt)

    let rec divrem' (dvdd, pow2, dvs) =
        if (cmp' dvs dvdd) = 1
        then [0], dvdd
        else let q, r = divrem' (dvdd, double_bigint pow2,
                                       double_bigint dvs)
             in if (cmp' dvs r) = 1
                then q, r
                else (add' q pow2 0),
                      (delete_zeros(sub' r dvs 0))

    let divrem ((Bigint (neg1, val1)), (Bigint (neg2, val2))) =
        let q, r = divrem' (val1, [1], val2)
        in if neg1 = neg2
          then Bigint (Pos, q),Bigint (Pos, r)
          else Bigint (Neg, q),Bigint (Pos, r)

    let div (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        let q, _ = divrem ((Bigint (neg1, val1)),
                           (Bigint (neg2, val2)))
        in q

    let rem (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        let _, r = divrem ((Bigint (neg1, val1)),
                           (Bigint (neg2, val2)))
        in r

    let is_even (Bigint (sign, num)) =
        let _, r = rem (Bigint (sign, num)),
                       (Bigint (Pos, [2]))
        in (cmp r zero) = 0

    let rec pow' ((Bigint (neg1, base)), (Bigint (neg2, expt)),
        (Bigint (neg3, result))) =
        match (Bigint (neg2, expt)) with
        | (Bigint (neg2, expt)) when
          (cmp (Bigint (neg2, expt)) zero) = 0 ->
          (Bigint (neg3, result))
        | (Bigint (neg2, expt)) when
          is_even (Bigint (neg2, expt)) ->
          pow' (mul (Bigint (neg1, base)) (Bigint (neg1, base)),
                     (div (Bigint (neg2, expt)) (Bigint (Pos, [2]))),
                     (Bigint (neg3, result)))
        | (Bigint (neg2, expt)) ->
          pow' ((Bigint (neg1, base)),
              (sub (Bigint (neg2, expt)) (Bigint (Pos, [1]))),
              (mul (Bigint (neg1, base)) (Bigint (neg3, result))))

    let pow (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        if (cmp (Bigint (neg2, val2)) zero) = -1
        then pow'
            ((div (Bigint (Pos, [1])) (Bigint (neg1, val1))),
            (Bigint (neg2, val2)), (Bigint (Pos, [1])))
        else pow' ((Bigint (neg1, val1)),
            (Bigint (neg2, val2)),
            (Bigint (Pos, [1])))


end

