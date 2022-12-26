# Tris-liste

Ce projet a pour but d'observer les performances de différentes fonctions de tris developpés en ocaml. Les données des tests sont dans le dossier `data`, l'analyse dans le dossier `analyse`. Un script en python peut être retrouver dans le dossier `src` celui permet de calculer la moyenne des temps d'execution de chaque algo et d'écrire dans les fichiers de tests cette valeur.

## Algorithme de tris

Vous pourrez retrouver dans le dossier `src` le fichier `tris.ml` contenant les fonctions de tris.

Quatre algorithme sont implémenté :

- [Tri comptage](#Tri-comptage)
- [Tri par sélection du minimum](#Tri-par-sélection-du-minimum)
- [Tri crêpes](#Tri-de-crêpes)
- [Tri par encerclement](#Tri-par-encerclement)

### Tri comptage

Cette méthode de tri commence par compter, dans l’ordre, pour chaque élément pouvant apparaître dans la liste à trier, le nombre de fois où il apparaît. Ceci fait, il suffira de reconstituer la liste, en répétant chaque élément le bon nombre de fois, du plus petit au plus grand. Notez que cet algorithme nous permettra uniquement de trier des listes d’entiers, contenant au moins deux valeurs différentes, et seulement selon les ordres mathématiques < et >.

### Tri par sélection du minimum

Le principe de ce tri est simple : il s’agit de récupérer le plus petit élément de la liste, et de le placer en première position. Puis on recommence avec le second plus petit élément, qu’on place en deuxième position. Et ainsi de suite.

### Tri de crêpes

Le tri de crêpes considère les éléments à trier comme étant des crêpes de diamètre plus ou moins grand, proportionnellement à leur valeur. La liste à trier est alors représentée comme une pile de crêpes (le haut de la pile contenant le premier élément de la liste). L’idée est de retourner intelligemment, à l’aide d’une spatule, une partie des crêpes du dessus de la pile, et de répéter le processus jusqu’à obtenir une pile de crêpes triées par taille.

### Tri par encerclement

Dans cette méthode de tri, on commence par _encercler_ la liste à trier, i.e :

1. on compare le premier élément de la liste avec le dernier, et on les échange s’ils ne respectent pas l’ordre,
2. puis on fait de même avec le deuxième élément de la liste et l’avant-dernier,
3. puis avec le troisième élément de la liste et l’avant-avant-dernier, etc.

Une fois l’encerclement terminé, on _sépare_ en deux les éléments de la liste obtenue en récupérant d’une part ceux du début, et d’autre part ceux de la fin. On recommence le processus sur ces deux listes, en les enclerclant puis en les divisant en deux.

Quand la liste considérée ne contient plus qu’un seul élément, le processus s’arrête et la renvoie telle quelle (car elle est triée).
La liste triée globale est obtenue en _fusionnant_, à chaque fois, le résultat du tri par encerclement des deux listes.
