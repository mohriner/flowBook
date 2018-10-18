###--- Parameters for non-alignment---###
# Determine sensitivity of measurement
phase.increment = .5   # in minutes on the clock face
swing.increment = .0075 # in seconds of time
swing.tempo = 96       # in beats per minute
durs = t(apply(
  X = matrix(c(seq(0,.3,swing.increment),-seq(0,.3,swing.increment)),ncol = 2),
  MARGIN = 1,
  function(i) i + (60/swing.tempo/4))
)
BURs = durs[,1]/durs[,2]
BURs = round(BURs[BURs >= 1 & BURs < 4],digits=2)
tempo.increment = .01  # in percentage of beat tempo
m = expand.grid(phase = seq(-30,30,phase.increment),
                swing = BURs,
                # The values above are chosen so that, at 96bpm, these swing
                # ratios will shift weak-division syllables by increments of 
                # 5 ms.
                #swing = 1,
                tempo = seq(.9, 1.1,tempo.increment)
)
remove(BURs,phase.increment,swing.increment,swing.tempo,durs,tempo.increment)
