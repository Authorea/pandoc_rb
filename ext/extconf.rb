puts "building haskell library"

if system 'stack build'
  puts "built haskell library"
else
  raise "building haskell library failed"
end

