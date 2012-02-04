(define defstruct
  Name Attributes -> (let Selectors (selectors Name Attributes)
                          Constructor (constructor Name Attributes)
                          Recognisor (recognisor Name)
                          Name))

(define selectors
    Name Attributes -> (map (/. A (selector Name A)) Attributes))

(define selector
    Name Attribute 
    -> (let SelectorName (concat Name (concat - Attribute))
           (eval [define SelectorName
                   Structure -> [let LookUp [assoc Attribute Structure]
                                     [if [empty? LookUp]
                                         [error "~A is not an attribute of ~A.~%" 
                                                    Attribute Name]
                                         [tail LookUp]]]])))

(define constructor
   Name Attributes 
   -> (let ConstructorName (concat make- Name)
           Parameters (params Attributes)
           (eval [define ConstructorName |
                    (append Parameters 
                            [-> [cons [cons structure Name] 
                                      (make-association-list Attributes
                                                             Parameters)]])])))

(define params
   [] -> []
   [_ | Attributes] -> [(gensym X) | (params Attributes)])

(define make-association-list
   [] [] -> []
   [A | As] [P | Ps] -> [cons [cons A P] (make-association-list As Ps)])

(define recognisor
  Name -> (let RecognisorName (concat Name ?)
               (eval [define RecognisorName
                       [cons [cons structure Name] _] -> true
                        _ -> false])))