Chapter 1. All You Need Is Lambda

> module Lambda where

Equivalence exercises.

1. \xy.xz
 = \x.(\y.xz)   -- curried form
 = \mn.mz       -- alpha equivalence
 => b)

2. \xy.xxy
 = \x.(\y.xxy)  -- curried form
 = \a.(\b.aab)  -- alpha equivalence
 => c)

3. \xyz.zx
 = \tos.st      -- alpha equivalence
 => b)

Def:
 A combinator is a lambda term with no free variables.

Combinators Determine if each of the following functions are combinators or not.
1. \x.xxx
   is a combinator
2. \xy.zx
   is not a combinator because z is a free variable
3. \xyz.xy(zx)
   is a combinator
4. \xyz.xy(zxy)
   is a combinator
5. \xy.xy(zxy)
   is not a combinator because z is free

Normal form or diverges. Determine if each of the following expressions can be reduced to a normal form or if they diverge.
1. \x.xxx
   is already in normal form
2. (\z.zz)(\y.yy)
 = (\y.yy)(\y.yy)
   diverges
3. (\x.xxx)z
 = zzz
   can be reduced to normal form

Beta reduce. Evaluate (that is, beta reduce) each of the following expressions to normal form. We strongly recommend writing out the steps on paper with a pencil or pen.
1. (\abc.cba)zz(\wv.w)
 = (\a.(\b.(\c.cba)))zz(\w.(\v.w))   -- curry
 = (\b.(\c.cbz))z(\w.(\v.w))         -- beta a
 = (\c.czz)(\w.(\v.w))               -- beta b
 = (\w.(\v.w))zz                     -- beta c
 = (\v.z)z                           -- beta w
 = z                                 -- beta v
2. (\x.\y.xyy)(\a.a)b
 = (\y.(\a.a)yy)b                    -- beta x
 = (\a.a)bb                          -- beta y
 = bb                                -- beta a
3. (\y.y)(\x.xx)(\z.zq)
 = (\x.xx)(\z.zq)                    -- beta y
 = (\z.zq)(\z.zq)                    -- beta x
 = (\z.zq)q                          -- beta z
 = qq                                -- beta z
4. (\z.z)(\z.zz)(\z.zy)
 = (\z.z)(\a.aa)(\b.bc)              -- alpha
 = (\a.aa)(\b.bc)                    -- beta z
 = (\b.bc)(\b.bc)                    -- beta a
 = cc                                -- see 3.
5. (\x.\y.xyy)(\y.y)y
 = (\x.\y.xyy)(\z.z)a                -- alpha
 = (\y.(\z.z)yy)a                    -- beta x
 = (\z.z)aa                          -- beta y
 = aa                                -- beta z
6. (\a.aa)(\b.ba)c
 = (\a.aa)(\b.bd)c                   -- alpha
 = (\b.bd)(\b.bd)c                   -- beta a
 = (\b.bd)dc                         -- beta b
 = ddc                               -- beta b
7. (\xyz.xz(yz))(\x.z)(\x.a)
 = (\xyz.xz(yz))(\a.b)(\c.d)         -- alpha
 = (\x.(\y.(\z.xz(yz))))(\a.b)(\c.d) -- curry
 = (\y.(\z.(\a.b)z(yz)))(\c.d)       -- beta x
 = (\z.(\a.b)z((\c.d)z))             -- beta y
 = (\z.b((\c.d)z))                   -- beta a
 = (\z.b(d))                         -- beta c
 = (\z.bd)                           -- unnec parens
