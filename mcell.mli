
(** *)


(** Type d'une macro-cellule *)
type t = private { id : int; size : int; nw : t; ne : t; sw : t; se : t; mutable result : t }

(** Les différentes tables de hachage *)
type h =
    HCREATE | HUNION | HINTER | HDIFF
  | HMIROR | HROT90  | HROT180 | HROT270 
  | HCNTLIVE

(** Vide la table de hachage h *)
val clear_hashtbl : h -> unit

(** Renvoie la taille de la table de hachage h *)
val size_hashtbl : h -> int

(** Cellule atomique vivante *)
val on : t

(** Cellule atomique morte *)
val off : t

(** Crée une macro-cellule de taille n depuis 4 macro-cellule
    de taille n-1 *)
val create : nw:t -> ne:t -> sw:t -> se:t -> t

(** Renvoie la taille n d'une macro-cellule *)
val size : t -> int

(** Calcule et renvoie la macro-cellule résultat de taille n-1
    de la macro-cellule de taille n *)
val result : t -> t

(** Renvoie vrai si la macro-cellule n'a aucune cellule vivante *)
val is_empty : t -> bool

(** Renvoie vrai si la macro-cellule n'a que des cellules vivantes *)
val is_full : t -> bool

(** Crée une macro-cellule de cellules mortes de taille n  *)
val empty : int -> t

(** Crée une macro-cellule de cellules vivantes de taille n *)
val full : int -> t

(** Change la valeur d'une cellule atomique de coordonnée i,j 
    de la macro-cellule. Si le booléen est à vrai, alors la cellule
    deviendra vivante, sinon morte.
    Les coordonnées i,j doivent être dans la macro-cellule *)
val set : t -> int -> int -> bool -> t

(** Comme la fonction set, mais aggrandit la macro-cellule si les
    coordonnées i,j sont en dehors de la macro-cellule *)
val set_extend : t -> int -> int -> bool -> t

(** Étend une macro-cellule de taille n, en plaçant cette dernier
    au centre d'une macro-cellule de taille n+1 *)
val extend : t -> t

(** Étend une macro-cellule jusqu'à ce que les coordonnées i,j
    ne soient plus en dehors de la macro-cellule *)
val extend_rec : t -> int -> int -> t

(** Renvoie vrai si la macro-cellule a besoin d'être étendue
    avant le calcul du résultat de cette dernière *)
val need_extend : t -> bool

(** Donne la macro-cellule de taille n-1 située au centre 
    de la macro-cellule de taille n *)
val center : t -> t

(** Diminue la taille de la macro-cellule au maximum
    sans perdre de cellules vivantes *)
val simplify_max : t -> t

(** Calcule l'état d'une macro-cellule dans 2^t
    pas de temps avec t n'étant pas forcement la taille de la
    macro-cellule *)
val future : int -> t -> t

(** Calcule l'état d'une macro-cellule dans t pas de temps 
    avec t quelconque (par défaut 1) *)
val iter : ?t:int -> t -> t

(** Calcule l'union de deux macro-cellules *)
val union : t -> t -> t

(** Calcule l'intersection de deux macro-celluels *)
val inter : t -> t -> t

(** Calcules la différence entre deux macro-cellules *)
val diff : t -> t -> t

(** Applique une symétrie axiale verticale sur la 
    macro-cellule *)
val mirror : t -> t

(** Applique une rotation de 90 degrés dans le sens trigonométrique
    sur la macro-cellule *)
val rotate90 : t -> t

(** Applique une rotation de 180 degrés dans le sens trigonométrique
    sur la macro-cellule *)
val rotate180 : t -> t

(** Applique une rotation de 270 degrés dans le sens trigonométrique
    sur la macro-cellule *)
val rotate270 : t -> t

(** Donne une macro-cellule de taille n+1 à partir d'une macro-cellule
    de taille n en plaçant cette dernière sur la gauche de la macro-cellule
    résultante *)
val extend_left : t -> t

(** Donne une macro-cellule de taille n+1 à partir d'une macro-cellule
    de taille n en plaçant cette dernière sur la droite de la macro-cellule
    résultante *)
val extend_right : t -> t

(** Crée une fichier au format dot exposant la représentation en mémoire d'une
    macro-cellule *)
val dot: filename:string -> t -> unit

(** Donne la réprésentation d'un tapis de Sierpinski de taille n sous
    la forme d'une macro-cellule *)
val carpet : int -> t

(** Compte le nombre de cellules vivantes dans une macro-cellule *)
val count_live_cells : t -> int

(** Compte le nombre de macro-cellules nécessaire pour représenter
    une macro-cellule (somme de la macro-cellule mère et des macro-cellules
    filles) *)
val count_nodes : t -> int
