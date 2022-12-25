(* -- FONCTION D'AFFICHAGE -- *)

(* Affiche un entier *)
let print a = Printf.printf "%8d\n" a;;

(* Affiche une chaine de charactere *)
let print_string s = Printf.printf "%s\n" s;;

(* Affiche une liste d'entier *)
let print_list l = 
  (Printf.printf "%8s%8s\n" "indice" "val";
   let rec print_list_rec l i =
     match l with 
     |[] -> []
     |h::t -> (Printf.printf "%8d%8d\n" i h; print_list_rec t (i + 1))
   in print_list_rec l 0);;

(* ================== GENERATION DE LISTE ===================== *) 

(* 
   -- liste_aleatoire --
   liste_aleatoire b n renvoie une liste de n entiers compris entre 0 et (b - 1) inclus 
*)
let rec liste_aleatoire b n =
  if n <= 0
  then []
  else let k = (Random.int b) 
    in k::(liste_aleatoire b (n - 1));;

(* ================== Tri comptage ===================== *)

(* 
   -- nombre d'occurence -- 
   Cette fonction compte le nombre d'occurences de e dans la liste l.
*) 
let rec nb_occurrences e l = 
  match l with
  |[] -> 0
  |h::t -> 
      if h = e
      then 1 + (nb_occurrences e t)
      else nb_occurrences e t;;


(* 
   -- supprimer tous -- 
   Cette fonction supprime toute les occurences de e dans la liste l.
*)

let rec supprimer_tous e l = 
  match l with
  |[] -> [] 
  |h::t ->
      if h = e
      then supprimer_tous e t
      else h::supprimer_tous e t;; 

(* 
   -- min max -- 
   Cette fonction renvoie un 3-uplet composé du minimum et du maximum de la liste l en fonction d'un ordre donné, 
   ainsi que l’opération devant être effectuée entre min et l’entier 1 pour que sa valeur se rapproche de max.
*)
let min_max ordre l = 
  let rec min_max_rec ordre min max l = 
    match l with (* Creation du 3-uplet, compose du min max et de l operateur *)
    |[] -> if min > max
        then (min, max, (+))
        else (min, max, (-))
    |h::t ->
        if ordre h min
        then min_max_rec ordre h max t 
        else if ordre max h
        then min_max_rec ordre min h t
        else min_max_rec ordre min max t
  in match l with
  |[] -> failwith "Error ! Array is empty !"
  |h::t -> min_max_rec ordre h h t;;


(* 
   -- nombre chaque -- 
   Cette fonction renvoie la liste des couples indiquant, pour chaque élément pouvant apparaître dans la liste 
   l, le nombre de fois où il apparaît. La liste est triée selon l'ordre donnée. 
*)
let nb_chaque ordre l =
  match (min_max ordre l) with |(min, max, op) ->
    let rec nb_chaque_rec res min max op l = 
      if op min 1 = max (* On fais attention de bien parcourir toute la liste *)
      then res 
      else if nb_occurrences max l = 0 (* On ignore les elements apparaissant 0 fois *)
      then nb_chaque_rec res min (op max 1) op (supprimer_tous max l)
      else  nb_chaque_rec ((max, nb_occurrences max l)::res) min (op max 1) op (supprimer_tous max l)
    in nb_chaque_rec [] min max op l;;

(* 
   -- liste element -- 
   Cette fonction renvoie une liste de nb element e
*)
let rec liste_element nb e = 
  if nb <> 0
  then e::liste_element (nb - 1) e
  else [];;

(* 
   -- concat -- 
   Cette fonction renvoie la liste concatenee de l1 et l2
*)

let rec concat l1 l2 =
  match l1 with 
  |[] -> l2 
  |h::t -> h::concat t l2;;


(* 
   -- reconstituer -- 
   Cette fonction renvoie la liste reconstituer (composee d'entier) a partir d'une liste contenant des couples, la liste des couples correspond
   au resultat de la fonction nb_chaque .
*)
let rec reconstituer l = 
  match l with
  |[] -> []
  |(e, nb)::t -> concat (liste_element nb e) (reconstituer t);; 

(* 
   -- Tri comptage -- 
   Cette fonction renvoie la liste passe en parametre trie selon l'ordre donnee.
   Elle utilise les fonctions defini precedement pour trie la liste.
*)
let tri_comptage ordre l = 
  match l with
  |[] -> failwith "La liste est vide, aucun élément à trier !"
  |_ -> reconstituer (nb_chaque ordre l);;



(* ================== Tri par sélection du minimum ================== *)

(* 
   -- min list -- 
   Cette fonction renvoie un couple contenant la position et l'element minimum de la liste en fonction 
   de l'ordre donnee.
*)
let min_list ordre l = 
  let rec min_list_rec ordre l min index index_min =
    match l with
    |[] -> (min, index_min) 
    |h::t -> 
        if ordre h min
        then min_list_rec ordre t h (index + 1) (index + 1)
        else min_list_rec ordre t min (index + 1) index_min
  in match l with
  |[] -> failwith "Erreur, la liste est vide !" 
  |h::t -> min_list_rec ordre t h 0 0;;



(* 
   -- min list indice -- 
   Cette fonction renvoie un couple contenant la position et l'element minimum a partir d'un indice de la liste en fonction 
   de l'ordre donnee.
*)
let min_list_indice ordre i l = 
  let rec min_list_indice_rec ordre i l =
    match l with 
    |[] -> []
    |h::t ->
        if i <> 0      
        then  min_list_indice_rec ordre (i - 1) t
        else t
  in if i > List.length l (* Si l'indice est superieur a la taille de la liste Erreur *)
  then failwith "Erreur, l'indice est plus grand que la taille de la liste !" 
  else if i = 0 (* indice 0 on cherche le min dans toute la liste *)
  then min_list ordre l
  else (* On recupere la partie de la liste qui nous interesse et on cherche le minimum *)
    match (min_list ordre (min_list_indice_rec ordre (i - 1) l)) with 
    |(e, pos) -> (e, pos + i);; (* On oublie pas d'ajouter l'indice pour avoir un indice referant a la liste entiere *)
  

(* 
   -- echange -- 
   Cette fonction renvoie la liste l apres avoir echanger de place les elements e1 et e2 si necessaire.
*)
let echange e1 i1 e2 i2 l =
  let rec echange_rec e1 i1 e2 i2 l index = 
    match l with 
    |[] -> []
    |h::t ->
        if i1 = index 
        then e2::echange_rec e1 i1 e2 i2 t (index + 1)
        else if i2 = index 
        then e1::echange_rec e1 i1 e2 i2 t (index + 1)
        else h::echange_rec e1 i1 e2 i2 t (index + 1)
  in if i1 < i2 (* On s'assure que l'echange est faisable *)
  then echange_rec e1 i1 e2 i2 l 0
  else if i1 = i2 (* S'il n'est pas necessaire de faire un echange on renvoie l *)
  then l
  else failwith "Erreur, i1 doit etre inferieur a i2 !";; (*S'il est impossible de faire l'echange Erreur *) 
  

(* 
   -- nieme -- 
   Cette fonction renvoie le n-ieme element de la liste l.
*)                                                        
let rec nieme n l = 
  if n > List.length l
  then failwith "Erreur l'indice est plus grand que la taille de la liste !"
  else match l with 
    |[] -> failwith "Erreur, la liste est vide"
    |h::t ->
        if n = 0
        then h
        else nieme (n - 1) t;;


(* 
   -- Tri selection min -- 
   Cette fonction renvoie la liste passe en parametre trie selon l'ordre donnee.
   Elle utilise les fonctions defini precedement pour trier la liste.
*) 
let tri_selection_min ordre l =
  let rec tri_selection_min_rec ordre l index = 
    match l with
    |[] -> l
    |h::t -> 
        if index = List.length l - 1
        then l
        else match (min_list_indice ordre index l) with 
          |(e, i) -> 
              tri_selection_min_rec ordre (echange (nieme index l) index e i l) (index + 1)
  in tri_selection_min_rec ordre l 0;;



(* ================== Tri de crêpes ================== *)


(* 
   -- Inserer spatule -- 
   Cette fonction renvoie l'indice de l'element le plus grand dans l'interval [0, n] en fonction de l'ordre donnée.
*) 
let inserer_spatule ordre l n = 
  if n > List.length l (* On verrifie que l'indice est valide *)
  then failwith "Erreur, l'indice est suppérieur à la taille de la liste !"
  else (* On recherche l'indice du plus grand element dans l'interval donnee *)
    let rec inserer_spatule_rec ordre l n min indice indice_min =
      match l with 
      |[] -> indice_min
      |h::t -> 
          if n = 0
          then indice_min
          else if ordre min h || min = h
          then inserer_spatule_rec ordre t (n - 1) h (indice + 1) (indice + 1)
          else inserer_spatule_rec ordre t (n - 1) min (indice + 1) indice_min
    in match l with 
    |[] -> failwith "Erreur, la liste est vide !"
    |h::t -> inserer_spatule_rec ordre t n h 0 0;;


(* 
   -- Retourner spatule -- 
   Cette fonction renvoie la liste retournée entre dans l'interval [0, i] ainsi que la concatenation avec la partie 
   non retourné.
*) 
let retourner_spatule l i =
  let rec retourner_spatule_rec l i res = 
    match l with 
    |[] -> res
    |h::t -> 
        if i = 0 (* Utilisation de concat pour inverser la liste obtenue (entre [0, i]) *)
        then concat (h::res) t (* Lors de cette appel recursif on remplace l par la fin de la liste  que l'on doit ajouté a la liste que l'on vient de retourner *) 
        else retourner_spatule_rec t (i - 1) (h::res)
  in retourner_spatule_rec l i [];;


(* 
   -- etape -- 
   Cette fonction renvoie la liste après avoir réalisé l’étape d’indice n de l’algorithme du tri de crêpes.
*)    
let etape ordre l n = 
  (* On verifie s'il est necessaire d'effectuer une etape *)
  if (n - (inserer_spatule ordre l n)) > 0 
  then retourner_spatule (retourner_spatule l (inserer_spatule ordre l n)) n 
  else l;;


(* 
   -- Tri crepes -- 
   Cette fonction renvoie la liste passe en parametre trie selon l'ordre donnee.
   Elle utilise les fonctions defini precedement pour trier la liste.
*) 
let tri_crepes ordre l = 
  let rec tri_crepes_rec ordre l indice = 
    match l with 
    |[] -> []
    |_::_ ->
        if indice > 0
        then tri_crepes_rec ordre (etape ordre l indice) (indice - 1)
        else l
  in tri_crepes_rec ordre l (List.length l);;



(* ================== Tri par encerclement ================== *)

(* 
   -- renverser -- 
   Cette fonction renvoie la liste passe en parametre renverser.
*) 
let renverser l =
  let rec renverser_rec l res =
    match l with 
    |[] -> res
    |h::t -> renverser_rec t (h::res)
  in renverser_rec l [];; 

(* 
   -- renverser -- 
   Cette fonction renvoie la liste passe en parametre après son encerclement selon ordre.
   Parametre : On prend en parametre la liste l et son renversement, ainsi qu'un indice sur la position du parcours,
   deux liste la et lb (ces deux liste enregisterons les valeurs echanges selon ordre) et une variable p indiquant si la liste est paire ou impaire
   Principe : On prend l et renv qu'on parcours jusqu'a (List.length l / 2) pour une liste de taille paire et (List.length l + 1 / 2) pour une liste l
   de taille impaire. 
   Le premiere objectif est de remplir les liste la et lb, pour cela on compare les liste l et renv selon ordre et on ajoute les elements aux liste la et lb
   Le second est fusionner la et lb dans le bonne ordre lorsque nous avons fini de parcours les listes l et renv (grâce a l'indice).
*) 
let encercler ordre l = 
  let rec encercler_rec ordre l renv indice la lb p = 
    match l, renv with
    |([], []) -> []
    |[], _ -> []
    |_, [] -> []
    |(h::t, hr::tr) ->
        if indice = 0 (* Si les listes l et renv on finit d'être parcouru alors, on doit ajouter la à lb *)
        then match (renverser la), lb with (* Subtiliter la liste 'la' est inverser suite à sa création *)
          |[], _ -> lb
          |_, [] -> (renverser la) 
          |ha :: ta, hb :: tb ->
              if List.length la <> 0 (* On creer la liste *)
              then ha :: encercler_rec ordre l renv indice (renverser ta) lb p
              else hb :: encercler_rec ordre l renv indice la tb p
        else if ordre h hr (* Si on a pas fini de parcourir l et renv, on remplie les listes la et lb*)
        then encercler_rec ordre t tr (indice - 1) (h::la) (hr::lb) p
        else ( (* On doit faire attention au cas ou la liste est impaire, et donc au cas ou h = hr *)
          if indice <> 1 (* Si nous ne somme pas encore a la derniere etapes du parcours on ajoute les elements aux listes *)
          then encercler_rec ordre t tr (indice - 1) (hr::la) (h::lb) p
          else ( (* Si nous somme a la derniere etapes du parcours donc indice = 1, on doit faire attention si la liste est impaire *)
            if p = "paire" (* Si la liste est paire on remplie les deux liste *)
            then encercler_rec ordre t tr (indice - 1) (hr::la) (h::lb) p
            else encercler_rec ordre t tr (indice - 1) la (h::lb) p)) (* Si la liste est impaire on ne remplie qu'une seule des deux liste (la ou lb) pour ne pas ajouter deux fois le même element *)
  in if (List.length l) mod 2 = 0 (* On verifier la parité de la liste *)
  then encercler_rec ordre l (renverser l) ((List.length l) / 2) [] [] "paire"
  else encercler_rec ordre l (renverser l) ((List.length l + 1) / 2) [] [] "impaire";; 


(* 
   -- separer -- 
   Cette fonction renvoie la liste renvoie, sous la forme d’un couple, la liste des premiers éléments de l, et la
   liste des derniers éléments de l. Si la liste d’origine contient un nombre impair d’éléments, alors la liste des premiers
   elements sera plus petite que la liste des derniers elements.
*) 
let separer l =
  (* Cette fonction recursive renvoie la liste des premiers elements *)
  let rec separer_rec l indice =
    match indice, l with 
    |0, [] -> []
    |0, _ -> []
    |_, [] -> [] 
    |indice, h::t -> h::separer_rec t (indice - 1)
  in (* Cette fonction recursive renvoie le couple de listes, elle prend en parametre la liste d'origine ainsi que la liste des permiers elements *)
  let rec creer_couple l l1 indice = 
    match l with 
    |[] -> (l1, l)
    |h::t -> 
        if indice <> 0
        then creer_couple t l1 (indice - 1)
        else (l1, l)
  in creer_couple l (separer_rec l ((List.length l) / 2)) ((List.length l) / 2);;


(* 
   -- fusion -- 
   Cette fonction renvoie la liste fusionné de l1 et l2 selon ordre.
*) 

let rec fusion ordre l1 l2 =
  match l1, l2 with 
  |[], _ -> l2
  |_, [] -> l1 
  |ha :: ta, hb :: tb ->
      if ordre ha hb
      then ha :: fusion ordre ta l2
      else hb :: fusion ordre l1 tb;;


(* 
   -- Tri encerclement -- 
   Cette fonction renvoie la liste passe en parametre trie selon l'ordre donnee.
   Elle utilise les fonctions defini precedement pour trier la liste.
*) 
let rec tri_encerclement ordre l =
  match (separer (encercler ordre l)) with
  |([], []) -> []
  |(_, []) -> l
  |([], _) -> l
  |([a], [b]) -> fusion ordre [a] [b]
  |([a], l2) -> fusion ordre  [a] (tri_encerclement ordre l2)
  |(l1, [a]) -> fusion ordre  (tri_encerclement ordre l1) [a]
  |(l1, l2) -> fusion ordre (tri_encerclement ordre l1) (tri_encerclement ordre l2);; 
  

(* ================== Generation des temps d'execution ================== *)


(* Fonction permettant d'ecrire le temps time dans le fichier file *)
let print_in_file time file =
  (* Write message to file *)
  let oc = open_out_gen [Open_append; Open_creat] 0o666 file in
  (* create or truncate file, return channel *)
  Printf.fprintf oc "%f\n" time;
  (* write something *)
  close_out oc;;
  
let ordre = (<);;

(* Fonction permettant de lancer run test sur la liste l *)
let rec start_test run ordre l b n =
  if run <> 0
  then ( 
    (let temps_debut = 
       Sys.time () in let _ = tri_comptage ordre l in
     let temps_fin = Sys.time () in
     print_in_file (temps_fin -. temps_debut) ("./data/tri_comptage/decroissant/" ^ string_of_int b ^ "/result_" ^ string_of_int n ^ "_" ^ string_of_int b ^ ".txt"));

    (let temps_debut = Sys.time () in
     let _ = tri_selection_min ordre l in
     let temps_fin = Sys.time () in
     print_in_file (temps_fin -. temps_debut) ("./data/tri_selection_min/decroissant/" ^ string_of_int b ^ "/result_" ^ string_of_int n ^ "_" ^ string_of_int b ^ ".txt"));

    (let temps_debut = Sys.time () in
     let _ = tri_crepes ordre l in
     let temps_fin = Sys.time () in
     print_in_file (temps_fin -. temps_debut) ("./data/tri_crepes/decroissant/" ^ string_of_int b ^ "/result_" ^ string_of_int n ^ "_" ^ string_of_int b ^ ".txt"));

    (let temps_debut = Sys.time () in
     let _ = tri_encerclement ordre l in
     let temps_fin = Sys.time () in
     print_in_file (temps_fin -. temps_debut) ("./data/tri_encerclement/decroissant/" ^ string_of_int b ^ "/result_" ^ string_of_int n ^ "_" ^ string_of_int b ^ ".txt"));
    start_test (run - 1) ordre l b n
  )
  else print_string "Fin test !";;


let n = 10;;
let b = 5000;;
let l = liste_aleatoire b n;;
start_test 10 ordre l b n;;



