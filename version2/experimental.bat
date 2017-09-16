FOR /L %%A IN (1,1,1000) DO (
  swipl -s q_RRL.pl --quiet -t runbatchdefault
)
