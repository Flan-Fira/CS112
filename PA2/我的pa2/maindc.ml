(* $Id: maindc.ml,v 1.5 2017-11-01 13:24:41-07 - - $ *)

include Scanner
include Bigint

open Lexing
open Bigint
open Printf
open Scanner

type stack_t = Bigint.bigint Stack.t
let incr = 0
let maxcount = 69
let size = 256
let boolf = false
let print1 = "%s\\\n%!"
let print2 = ""
let err2 = "no register"
let lim = 0

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

let valtable = Array.make size (boolf, Bigint.zero)

let print_digitber digitber = 
    let digit = string_of_bigint digitber in
    let length  = (String.length digit) in
    let rec pnhelper index =
        let ldigit = length - index in
            if (ldigit >= maxcount) then
                (printf "%s\\\n%!" (String.sub digit index maxcount);
                pnhelper (index + maxcount))
            else printf "%s\n%!" (String.sub digit index ldigit)
    in pnhelper incr
    
let print_stackempty () = printf "dc: stack empty\n%!"
let err1() = printf "Given funct/flag is unimplemented\n%!" 

let pullv dirs fstck =
    let out = Array.get valtable dirs in 
    match out 
    with | true,outp-> Stack.push outp fstck
         | false,_-> print_stackempty ()
let executedirs (fstck: stack_t) (oper: char) (dirs: int) =
    try match oper with
        | 's' -> Array.set valtable dirs (true, (Stack.pop fstck))
        | 'l' -> pullv dirs fstck
        | _   -> err1 ()
    with Stack.Empty -> print_stackempty()

let runop (fstck: stack_t) (oper: binop_t) =
    try let right = Stack.pop fstck
        in  try let left = Stack.pop fstck
                in  Stack.push (oper left right) fstck
            with Stack.Empty -> (print_stackempty ();
                                 Stack.push right fstck)
    with Stack.Empty -> print_stackempty ()

let execute (fstck: stack_t) (oper: char) =
    try match oper with
        | '-'->runop fstck Bigint.sub
        | '+'->runop fstck Bigint.add
        | '^'->runop fstck Bigint.pow
        | 's'->failwith err2
        | 'c'->Stack.clear fstck
        | '*'->runop fstck Bigint.mul
        | 'l'->failwith err2
        | '%'->runop fstck Bigint.rem
        | 'p'->print_digitber (Stack.top fstck)
        | '/'->runop fstck Bigint.div
        | 'q'->raise End_of_file
        | 'd'->Stack.push (Stack.top fstck) fstck
        | '\n'->()
        | ' '->()
        | 'f'->Stack.iter print_digitber fstck
        | _ ->printf "0%o is unimplemented\n%!"(ord oper)
    with Stack.Empty -> print_stackempty()

let initial (fstck: stack_t) inmethod =
    let buffer = from_channel inmethod in
    let rec initial () = 
        try  let toknex = Scanner.scanner buffer
             in  (match toknex with
                 | Number digitber-> Stack.push digitber fstck
                 | Regoper (oper, dirs)-> executedirs fstck oper dirs
                 | Operator oper-> execute fstck oper
                 );
             initial ()
        with End_of_file -> exit incr;
    in  initial ()

let input () =
    let fstck : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1 
         then try  let inp = open_in Sys.argv.(1)
                   in  initial fstck inp
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(incr) message;
                   exit lim));
        initial fstck stdin)

let interact () =
    let fstck : bigint Stack.t = Stack.create ()
    in  initial fstck stdin

let _ = if not !Sys.interactive then input ()
