data bool : Set where
  true  : bool
  false : bool

bool-case : (P : bool → Set) → P true → P false → (b : bool) → P b
bool-case P Pt Pf true = Pt
bool-case P Pt Pf false = Pf

data nat : Set where
  zero : nat
  suc  : nat -> nat

nat-case : (P : nat → Set) → P zero → ((n : nat) → P (suc n)) → (n : nat) → P n
nat-case P Pz Ps zero = Pz
nat-case P Pz Ps (suc n) = Ps n

data vec (a : Set) : (n : nat) → Set where
  nil : vec a zero
  cons : {n : nat} → a → vec a n → vec a (suc n)

vec-case : (a : Set) → (P : (n : nat) → vec a n → Set) → P zero nil → ((n : nat) (x : a) (l : vec a n) → P (suc n) (cons x l)) → ((n : nat) (l : vec a n) → P n l)
vec-case a P Pn Pc zero nil = Pn
vec-case a P Pn Pc (suc n) (cons x l) = Pc n x l
