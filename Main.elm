import Containers.Trie as T
import Containers.Queue as Q
import Containers.Dequeue as DQ

main = flow down [ asText . T.member "a" . T.insert "a" <| T.empty
                 , asText . Q.snoc 3 . Q.snoc 7 <| Q.empty
                 , asText . DQ.head . DQ.init . DQ.cons 3 . DQ.cons 1 . DQ.snoc 7 <| DQ.empty
                 ]
