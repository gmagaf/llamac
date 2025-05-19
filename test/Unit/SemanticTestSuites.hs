module Unit.SemanticTestSuites (module Unit.SemanticTestSuites) where

typeDefSuite :: [(String, Bool)]
typeDefSuite =
    [
    ("type t = T", True),
    ("type t = T of int", True),
    ("type t = T of int t", True),
    ("type t = T of int -> char t ref", True),
    ("type t = T_int of int | T_float of array [*, *] of float | T_char of char", True),
    ("type t1 = T1 of t1 t2 and t2 = T2 of t2 t1", True),
    ("type t1 = T1 of t1 array [*, *] of t2 and t2 = T2 of t2 t1 ref", True),
    ("type t1 = T of t1 t2 and t2 = T of t2 t1", True),
    ("type s = K of s -> t -> s and t = L | R", True),
    -- Failing cases
    ("type t = T1 | T1", False),
    ("type t = R of r", False)
    ]

letDefSuite :: [(String, Bool)]
letDefSuite =
    [
    ("let main = 42", True),
    ("let main : int = 42", True),
    ("let f (x : int) : int -> int = x", True),
    ("let f (x : int) a : int -> char -> int = x", True),
    ("let f (x : int) a = x", True),
    ("let main = 42 and main : float = 17.0", True),
    ("let s = \"this is a string\" and c : char = 'c'", True),
    ("let main = 42 and f x = x", True),
    ("let main a b = a b", True),
    ("let main a b = a b and f x = x", True),
    ("let ar id (f : int ref) g x (ar : array of float) = let d : int = id 2 in ar", True),
    ("type t = T\nlet t : t = T", True),
    ("type t = T\nlet t = T", True),
    ("type t = T of t\nlet t (t : t) = t\nlet g : t -> t = T", True),
    ("let id id x = let c = id x in x", True),
    ("let main id x = let c = id x in c", True),
    ("let mutable f : float and f x : int -> int = 5\nlet g : int = f 42", True),
    ("let f (x : int) = let x = 'c' in x", True),
    -- Failing cases
    ("let f = g", False),
    ("let f x = g x and g x = x", False),
    ("let f (x : int) (x : int) = 5", False),
    ("let f x : int -> int = 5 and mutable f : float\nlet g : int = f 42", False),
    ("let f x : int -> int = 5.0", False),
    ("let f : t = T", False),
    ("let f x = f x", False),
    ("let f x : int -> int = let x = 'c' in x", False),
    ("let f x : int -> int = x and c = 'c'\nlet g x = f c", False)
    ]

letRecSuites :: [(String, Bool)]
letRecSuites =
    [
    ("let rec main = 42", True),
    ("let rec f = main and main a = 42 and g = main", True),
    ("let rec f x = main x and main a = 42 and g x = main x", True),
    ("let rec f x : int -> int = main x and main a = 42 and g x = main x", True),
    ("let rec f : int -> int = main and main a = 42 and g = main", True),
    ("let rec mutable x and main : int ref = x", True),
    ("let rec main : int ref = x and mutable x", True),
    ("let k x y : int -> int -> int = 5 let rec f x = k (g x) x and g y = k y (f y)", True),
    ("let rec f x = x and g = f 4", True),
    ("let rec f x = x and mutable a [f 4, 5] : float", True),
    ("let rec f x = f x", True),
    ("let rec f x = f (g x) and g x : int -> int = g (f x)", True),
    ("let rec f x = f (g x) and g x = g (f x)", True),
    ("let rec f x y = f y y and mutable ar [f 3 (f 0 42)]", True),
    ("let rec f x = x and mutable ar[f 42]", True),
    ("let id x = x\nlet c = id 3 and d = id 'a'", True),
    ("let rec id x = x and c x = id 1 and main = id", True),
    ("let rec id x = x and f : int ref = x and g x = id x and mutable x and mutable ar [id 2] : float", True),
    ("let rec id x = x and f : int ref = x and g x = id x and mutable x and mutable ar [id 2, g 3] : float", True),
    -- Failing cases
    ("let rec f = main and main a = 42 and g = main and k1 = main 'c' and k2 = main 9.0", False),
    ("let rec f = main and main a = 42 and g (a : int) = main a and k1 = main 'c' and k2 = main 9.0", False),
    ("let rec id x = x and c = id 3 and d = id 'a'", False)
    ]
