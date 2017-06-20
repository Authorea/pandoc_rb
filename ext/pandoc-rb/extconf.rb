
if `which stack`.blank?
  puts "installing stack"
  unless system "curl -sSL https://get.haskellstack.org/ | sh"
    raise "installing stack failed"
  end
  system "export PATH=$HOME/.local/bin:$PATH"
  puts "installed stack"
end

puts "beginning to build haskell library as extension"

puts "updating stack"
unless Dir.chdir(__dir__){ system "stack update" }
  raise "stack update failed"
end

puts "setting up ghc for pandoc-rb"
unless Dir.chdir(__dir__){ "stack setup" }
  raise "stack setup failed"
end

unless Dir.chdir(__dir__){ system "stack ghc -- -Wall -fno-warn-orphans -O3 -split-objs --make -no-hs-main -optc-O3 -optl '-shared' -o ../../lib/Text_Pandoc_C.so ../../src/Text/Pandoc/C.hs" }
  raise "stack build failed"
end

unless Dir.chdir(__dir__){ File.exist? "../../lib/Text_Pandoc_C.so" }
  raise "unfinished: the haskell library was not properly built"
end
puts "built haskell library"

# Mock regular setup with makefile
File.open('Makefile', 'w') { |f| f.write "all:\n\ninstall:\n\n" }
File.open('make', 'w') do |f|
  f.write '#!/bin/sh'
  f.chmod f.stat.mode | 0111
end
File.open('pandoc-rb.so', 'w') {}

