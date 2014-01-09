import Containers.Trie as T
import Containers.Queue as Q

main = flow down [ asText . T.member "a" . T.insert "a" <| T.empty
                 , asText . Q.toList . Q.snoc 3 . Q.snoc 7 <| Q.empty
                 ]
