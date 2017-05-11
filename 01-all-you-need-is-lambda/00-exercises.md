# Chapter Exercises

## Intermission: Equivalence Exercises

Which are equivalent?

1. 𝜆𝑥𝑦.𝑥𝑧
	a) 𝜆𝑥𝑧.𝑥𝑧
	b) 𝜆𝑚𝑛.𝑚𝑧 <- this one
	c) 𝜆𝑧(𝜆𝑥.𝑥𝑧)
2. 𝜆𝑥𝑦.𝑥𝑥𝑦
	a) 𝜆𝑚𝑛.𝑚𝑛𝑝
	b) 𝜆𝑥(𝜆𝑦.𝑥𝑦)
	c) 𝜆𝑎(𝜆𝑏.𝑎𝑎𝑏) <- this one
3. 𝜆𝑥𝑦𝑧.𝑧𝑥
	a) 𝜆𝑥.(𝜆𝑦.(𝜆𝑧.𝑧))
	b) 𝜆𝑡𝑜𝑠.𝑠𝑡 <- this one
	c) 𝜆𝑚𝑛𝑝.𝑚𝑛

## 1.11 Chapter Exercises

Answers for these exercises are actually provided in the book, but in the spirit of discipline:

Combinators: Determine if each of the following are combinators or not.

1. 𝜆𝑥.𝑥𝑥𝑥 — yes
2. 𝜆𝑥𝑦.𝑧𝑥 — no
3. 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥) — yes
4. 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥𝑦) — yes
5. 𝜆𝑥𝑦.𝑥𝑦(𝑧𝑥𝑦) — no

Determine if each of the following can be reduced to a normal form or if they diverge.

1. 𝜆𝑥.𝑥𝑥𝑥 — yes (already in normal form)
2. (𝜆𝑧.𝑧𝑧)(𝜆𝑦.𝑦𝑦) — no, this is the omega term
3. (𝜆𝑥.𝑥𝑥𝑥)𝑧 — yes (zzz)

Beta reduce: Evaluate (that is, beta reduce) each of the following expressions to normal form. We strongly recommend writing out the steps on paper with a pencil or pen.

```
1. (𝜆𝑎𝑏𝑐.𝑐𝑏𝑎)𝑧𝑧(𝜆𝑤𝑣.𝑤)
(𝜆𝑏𝑐.𝑐𝑏𝑧)𝑧(𝜆𝑤𝑣.𝑤)
(𝜆𝑐.𝑐𝑧𝑧)(𝜆𝑤𝑣.𝑤)
((𝜆𝑤𝑣.𝑤)𝑧𝑧)
(𝜆𝑣.𝑧)𝑧
𝑧

2. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑎.𝑎)𝑏
(𝜆𝑦.(𝜆𝑎.𝑎)𝑦𝑦)𝑏
(𝜆𝑎.𝑎)𝑏𝑏
𝑏𝑏

3. (𝜆𝑦.𝑦)(𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)
(𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)
(𝜆𝑧.𝑧𝑞)(𝜆𝑧.𝑧𝑞)
(𝜆𝑧.𝑧𝑞)𝑞
𝑞𝑞

4. (𝜆𝑧.𝑧)(𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦) Hint: alpha equivalence.
yy
I find it good to have names for simpler functions… then you can read this out as "the identity of the self-application of \z -> zq"

5. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑦.𝑦)𝑦
(𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆a.a)b
(𝜆𝑦.(𝜆a.a)𝑦𝑦)b
(𝜆a.a)bb
bb (where b is the final y in the original expression)

6. (𝜆𝑎.𝑎𝑎)(𝜆𝑏.𝑏𝑎)𝑐
(𝜆𝑎.𝑎𝑎)(𝜆x.xy)𝑐
(𝜆x.xy)(𝜆x.xy)𝑐
((𝜆x.xy)y)𝑐
yy𝑐 (where y is the `a` in the second parens of the original expression)

7. (𝜆𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑧)(𝜆𝑥.𝑎)
(𝜆𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))(𝜆m.n)(𝜆p.q)
(𝜆𝑦𝑧.(𝜆m.n)𝑧(𝑦𝑧))(𝜆p.q)
𝜆𝑧.(𝜆m.n)𝑧((𝜆p.q)𝑧)
𝜆𝑧.n((𝜆p.q)𝑧)
𝜆𝑧.nq (where n and q are the bodies of the second and third parens in the original expression)
```
