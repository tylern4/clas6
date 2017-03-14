      Real Function user_tdep(x)
c
      real x
      real par(4)
c from Bryan's data on L(1520)
      data par/1.4253,49.005,3.8380,0./
c
      user_tdep=par(1)+par(2)/x**par(3)
c
      return
      end
