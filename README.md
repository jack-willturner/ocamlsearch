# ocamlsearch
While doing some route finding tasks I struggled to find many examples of A\* search being implemented in OCaml. The solution is quite elegant since it lends so cleanly to recursion. 

I've tried to keep the structure as general as possible so that people only need to change the `expand` and `strategy` functions if they want to adapt it to their own specific implementations. 
