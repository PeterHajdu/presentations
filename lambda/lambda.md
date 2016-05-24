---
-> lambda calculus

---
-> Alonzo Church 1930
-> model of computation
-> investigate functions, function application and recursion
-> Which problems can be solved?

---
<Exp> ::= <identifier>        |
          ( <Exp> )           |
          <Exp> <Exp>         |    --application
          \\<identifier>.<Exp>      --abstraction

---
-> identifiers: free, bound
-> \\x.xy

---
-> application: beta reduction
-> (\\x.xq)(\\y.p(o y))

---
-> application: beta reduction
-> (\\x.xq)(\\y.p(o y))
-> (\\y.p(o y))q

---
-> application: beta reduction
-> (\\x.xq)(\\y.p(o y))
-> (\\y.p(o y))q
-> p(o q)

---
-> name change: alpha conversion
-> (\\x.xq)(\\q.p(o q))

---
-> name change: alpha conversion
-> (\\x.xq)(\\q.p(o q))
-> (\\x.xq)(\\y.p(o y))

---
-> normal order evaluation: left-most, outer-most
-> (\\x.7)((\\x.xx)(\\y.yy))

---
-> currying
-> \\xy.x

---
-> currying
-> \\xy.x
-> \\x.\\y.x

---
-> currying
-> Moses Schonfinkel
-> Curry

---
-> ? Curry

---
-> examples
