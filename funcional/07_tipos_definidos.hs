data Natural = Cero | Suc Natural
suma :: Natural -> Natural -> Natural
suma Cero n = n
suma (Suc m) n = Suc (suma m n)