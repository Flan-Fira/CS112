(* $Id: bigint.ml,v 1.5 2017-11-1 15:06:24-08 - - $ *)


open List
open String
open Printf

module Bigint = struct

    type intimation = Neg | Pos
    let emptylist = []
    let incr = 0
    type bigint = Bigint of intimation * int list
    let nincr = -1
    let sint = int_of_char
    let zero = Bigint (Pos, emptylist)
    let charzero = '0'
    let stringzero = "0"
    let minus = '_'
    let listofOne = [1]
    let  radixlen = 1
    let listofTwo = [2]
    let emptystring = ""
    let gerr = "sub: firstl is greater than secl!"
    let  radix = 10
    let negative = "-"
    let boolt = true
    let boolf = false

    let strtochar inp = 
        let last = String.length inp - radixlen
        in  let rec strtochar' pos out =
            if pos >= incr
            then strtochar' (pos-radixlen) (inp.[pos]::out) 
            else out
        in  strtochar' last emptylist

     let convertToBigInt inp =
        let llength = String.length inp
        in  let convertString strf =
                let lstr = sub inp strf (llength - strf) in
                let charone char = sint char - sint charzero in
                List.map charone (List.rev (strtochar lstr)) in  
                if llength <> incr
                then if inp.[incr] <> minus
                     then Bigint (Pos, convertString incr)  
                     else Bigint (Neg, convertString radixlen)
                else zero

    let bigint_of_string = convertToBigInt

    let string_of_bigint (Bigint (pn, digit)) =
        match digit with
        | []    -> stringzero
        | digit -> let revd = List.rev digit
                   in  String.concat emptystring
                       ((
                           if pn <> Pos 
                           then negative 
                           else emptystring
                        ) ::
                        (List.map string_of_int revd))

    let rec add' secl changes firstl = match 
        (secl, changes, firstl) 
        with
        | [], secl, 0 -> secl
        | firstl, [], 0 -> firstl
        | [], secl, changes -> add' [changes] secl incr
        | firstl, [], changes -> add' firstl [changes] incr
        | fircar::seccdr, seccar::thircdr, changes
        -> let total = fircar + seccar + changes
          in  total mod (radixlen*radix) 
            ::add' seccdr thircdr(total * radixlen/(radixlen*radix))

    let rec sub' changes secl firstl =  match 
        (changes, secl, firstl) with
        | firstl,[],false->firstl
        | firstl,[],boolt->let contrast= 
              (hd firstl)+(radix*incr+(-radixlen)) in 
        if contrast >= incr then 
        contrast::(tl firstl)
        else (contrast + radix)::(sub' (tl firstl) emptylist boolt)
        | [], secl, false  -> failwith gerr
        | [], secl, boolt -> failwith gerr
        | firstcar::firstcdr, seccar::seccdr, changes -> 
            let contrast = firstcar - seccar in
                if (changes) then 
                    sub' ((firstcar - radixlen)::firstcdr) secl boolf
                else if contrast >= incr then 
                contrast::(sub' firstcdr seccdr boolf)
                else (contrast + radix)::(sub' firstcdr seccdr boolt)
                
    let rec compare flist seclist = 
        if (List.length flist) < (List.length seclist) then nincr
        else if (List.length flist) <= (List.length seclist) 
            then match (flist, seclist) with
            | [],[] -> incr
            | flist,[] -> radixlen
            | [],seclist -> nincr
            | flist,seclist ->  
                let frevd = List.rev flist in
                let secrevd = List.rev seclist in
                    if (hd frevd) > (hd secrevd) 
                    then incr
                    else if (hd frevd) < (hd secrevd) 
                    then nincr
                    else compare (rev (tl frevd))
                     (List.rev (tl secrevd))
        else radixlen

     let rec format input =
        if input <> emptylist then 
        let revd = List.rev input in
        if (hd revd) = incr then 
        format (List.rev (tl revd))
        else input
        else emptylist

    let add (Bigint (firstneg, value1)) (Bigint (secneg, value2)) =
        if firstneg = secneg
        then Bigint (firstneg, add' value1 value2 incr) 
        else let comparevec = compare value1 value2 in
        if comparevec > incr 
            then Bigint (firstneg, sub' value1 value2 boolf)
        else if comparevec >= incr then zero
        else Bigint (secneg, sub' value2 value1 boolf)

    let sub (Bigint (firstneg, firstvr)) (Bigint (secneg, secvr)) =
        if secneg <> firstneg 
            then Bigint (firstneg, add' firstvr secvr incr) 
        else 
        let comparevec = compare firstvr secvr in
        if incr < comparevec
           then Bigint (firstneg, format 
               (sub' firstvr secvr boolf))
        else if comparevec < incr then 
            let intimation = if firstneg
                <> Pos then Pos else Neg
            in  Bigint (intimation, format 
               (sub' secvr firstvr boolf))
        else zero 

    let rec helperm' firstl num changes = 
        if firstl = emptylist then [changes]
        else let prd = (hd firstl) * num + changes in
            (prd mod radix)::(helperm' (tl firstl) num (prd / radix))

    let rec helperm firstl secl =
        if (firstl = emptylist || secl = emptylist) then emptylist
        else add' (helperm' firstl (hd secl) incr) 
                    (incr::(helperm firstl (tl secl))) incr

    let mul (Bigint (firstneg, firstvr))
         (Bigint (secneg, secvr)) =
        if firstneg = secneg then Bigint 
            (Pos, format (helperm firstvr secvr))
        else Bigint (Neg, format (helperm firstvr secvr))

    let rec helperdiv divisor dividend oval newin value = 
        let vcompare = 
            compare (format (helperm (add' value newin incr) dividend))
                 divisor in
            if vcompare > incr then oval
            else if vcompare < incr 
                then helperdiv divisor dividend newin 
                    (helperm newin listofTwo) value
            else newin

    let rec helperdiv' divisor dividend value = 
        let newin = helperdiv divisor dividend
                emptylist listofOne value in
            if newin = emptylist then value
            else helperdiv' divisor dividend (add' value newin incr)

    let div (Bigint (firstneg, firstvr)) 
        (Bigint (secneg, secvr)) =  match
        (firstvr, secvr) 
        with 
            | firstvr, secvr -> 
              let intimation = if firstneg 
                <> secneg then Neg else Pos in
                  Bigint (intimation, format 
                    (helperdiv' firstvr secvr emptylist))
            | firstvr, [] -> failwith "ocamldc: divide by zero"
            | [], secvr -> zero

    let rem (Bigint (firstneg, firstvr)) (Bigint (secneg, secvr)) = 
        Bigint (firstneg,  
            (format (sub' firstvr (format (helperm 
                (helperdiv' firstvr secvr emptylist) secvr)) boolf)))

    let rec helperpow firstl secl ofirstl =
        let secl' = (format (sub' secl listofOne boolf)) in
            if secl' <> emptylist then helperpow 
                (helperm firstl ofirstl) secl' ofirstl
            else firstl

    let pow (Bigint (firstneg, firstvr)) (Bigint (secneg, secvr)) = 
        if secneg <> Neg then match
        (firstvr, secvr) 
        with | [], secvr -> zero
        | firstvr, [] -> Bigint (Pos, listofOne)
        | firstvr, secvr -> Bigint (firstneg, (format 
            (helperpow firstvr secvr firstvr)))
        else zero

end

