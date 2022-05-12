open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an arithmetic expression in an environment *)

let rec getValue l v = match l with 
 | (s,a)::sh -> (match (s = v) with 
                | true -> a 
                | false -> (getValue sh v))
 | [] -> raise UndefinedVar;;

let rec eval_expr (e : exp) (env : environment):value =
        match e with 
        | Number n -> Int_Val n
        | True  -> Bool_Val true
        | False -> Bool_Val false
        | Var n -> getValue env n 
        | Plus (e1,e2) -> let n1 = eval_expr e1 env in
                          let n2 = eval_expr e2 env in
                          (match n1,n2 with
                          | Int_Val i, Int_Val j -> Int_Val (i + j)
                          | _ -> raise TypeError)
        | Div (e1,e2) -> let n1 = eval_expr e1 env in
                         let n2 = eval_expr e2 env in
                         (match n1,n2 with
                         | Int_Val i, Int_Val 0 -> raise DivByZeroError
                         | Int_Val i, Int_Val j -> Int_Val (i / j)
                         | _ -> raise TypeError)
        | Minus (e1,e2) -> let n1 = eval_expr e1 env in
                           let n2 = eval_expr e2 env in
                            (match n1,n2 with
                                | Int_Val i, Int_Val j -> Int_Val (i - j)
                                | _ -> raise TypeError)
        | Times (e1,e2) -> let n1 = eval_expr e1 env in
                           let n2 = eval_expr e2 env in
                              (match n1,n2 with
                                  | Int_Val i, Int_Val j -> Int_Val (i * j)
                                  | _ -> raise TypeError)
        | Mod (e1,e2) -> let n1 = eval_expr e1 env in
                                  let n2 = eval_expr e2 env in
                                  (match n1,n2 with                             
                                   | Int_Val i, Int_Val j -> Int_Val (i mod j) 
                                   | _ -> raise TypeError)
       | Not e1 -> let n1 = eval_expr e1 env in
                                (match n1 with
                                | Bool_Val true -> Bool_Val false
                                | Bool_Val false ->  Bool_Val true      
                                | _ -> raise TypeError)
        | Or (e1,e2) -> let n1 = eval_expr e1 env in
                   let n2 = eval_expr e2 env in
                      (match n1,n2 with
                         | Bool_Val i, Bool_Val j -> Bool_Val (i or j)
                          | _ -> raise TypeError)
    | And (e1,e2) -> let n1 = eval_expr e1 env in
                          let n2 = eval_expr e2 env in
                          (match n1,n2 with
                          | Bool_Val i, Bool_Val j -> Bool_Val (i & j)
                          | _ -> raise TypeError)
    | Lt (e1,e2) -> let n1 = eval_expr e1 env in
                          let n2 = eval_expr e2 env in
                          (match n1,n2 with
                          | Int_Val i, Int_Val j -> Bool_Val (i < j)
                          | _ -> raise TypeError)
    | Leq (e1,e2) -> let n1 = eval_expr e1 env in
                          let n2 = eval_expr e2 env in
                          (match n1,n2 with
                          | Int_Val i, Int_Val j -> Bool_Val (i <= j)
                          | _ -> raise TypeError)
    | Eq (e1,e2) -> let n1 = eval_expr e2 env in
                              let n2 = eval_expr e1 env in
                              (match n1,n2 with
                              | Int_Val i, Int_Val j -> Bool_Val (i == j)
                              | Bool_Val i, Bool_Val j -> Bool_Val (i == j)                                   
                              | _ -> raise TypeError)   
      | App (e1,e2) -> (let n1 = eval_expr e1 env in 
                          let n2 =  eval_expr e2 env in 
                          match (n1, n2) with
                          | Closure (e1,e2, e3), Int_Val n -> eval_expr e3 ((e2, Int_Val n)::e1)
                          | Closure (e1,e2, e3), Bool_Val n -> eval_expr e3 ((e2, Bool_Val n)::e1)
                          | Closure (e1,e2, e3), Closure (e4,e5, e6) ->(match e3 with 
                                                                        |Fun(a1, a2) -> (match (a1 = e5) with 
                                                                                         |true -> Closure(e4,e5,e6 )
                                                                                         |false -> raise TypeError )
                                                                        | _ -> raise TypeError )
                          | _ -> raise TypeError)                                    
      | Fun (e1,e2) ->  Closure(env, e1, e2)
      | _ -> raise TypeError;;
  
                    

(* evaluate a command in an environment *)

let rec changeList env l name newValue =  match l with 
       | [] -> env
       | (a,b)::ls -> match (a=name) with 
                      | true -> (a, newValue)::env
                      | false -> changeList env ls name newValue
                      | _ -> raise TypeError;;

let rec eval_command (c : com) (env : environment) : environment =
     match c with 
     | Skip -> []
     | Declare (t, x) -> (match t with 
                          | Int_Type -> (x, Int_Val 0)::env
                          | Bool_Type -> (x, Bool_Val false)::env
                          | Lambda_Type -> (x,Closure(env, x, Var x))::env
                          ) 
      | Assg (x, e) -> let d1 = eval_expr e env in changeList env env x d1
      | Comp (c1, c2) ->  let d1 = eval_command c1 env in eval_command c2 d1
      | Cond (c1,c2,c3) -> let n1 = eval_expr c1 env in (
                         match n1 with 
                         |Bool_Val true -> eval_command c2 env 
                         |Bool_Val false -> eval_command c3 env
                         | _ -> raise TypeError)
      | For(c1,c2) -> let n1 = eval_expr c1 env in(
                      match n1 with 
                      | Int_Val n -> (
                         match (n > 0) with 
                         | true -> (let rec evaluateExpressionForLoop num (ce : com) (ceenv : environment):environment = 
                                    match (num > 0) with
                                     |true -> let newEnv = eval_command ce ceenv in 
                                        evaluateExpressionForLoop (num-1) ce newEnv
                                     |false -> ceenv  
                                     in evaluateExpressionForLoop n c2 env)
                         | false -> env)
                      | _ -> raise TypeError  )
      | While (c1,c2) -> let n1 = eval_expr c1 env in(
                         match n1 with 
                         |Bool_Val true -> let newEnv = eval_command c2 env in eval_command c newEnv
                         |Bool_Val false ->  env
                         | _ -> raise TypeError
      )
      | _ -> raise TypeError;;

 
