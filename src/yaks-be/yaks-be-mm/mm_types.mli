open Types_signatures


module MakeStore (K : Key) (Va : Value): Store with 
  type key=K.t and
  type value=Va.t