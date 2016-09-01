return function(health, damageMultiplier, concentration, hasCoffee)
  if concentration == 1 and hasCoffee then
    return 3 -- Drink Coffee
  end

  if health > 200 and damageMultiplier < 2 then
    return 1 -- Rename
  end
  
  if health <= 50 then
    return 2 -- Capture
  end
  
  return 0 -- Reduce
end
