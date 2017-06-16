require "bundler/gem_tasks"
require "rake/testtask"

Rake::TestTask.new(:test) do |t|
  t.libs << "test"
  t.libs << "lib"
  t.test_files = FileList["test/**/*_test.rb"]
end

desc "Run tests"
task :default => :test

task :build_hs do
  puts "beginning to build haskell library"
  unless system "stack ghc -- -Wall -fno-warn-orphans -O3 -split-objs --make -no-hs-main -optc-O3 -optl '-shared' -o Text_Pandoc_C.so src/Text/Pandoc/C.hs"
    # system('stack build --force-dirty --ghc-options="-O3 -no-hs-main -dynamic -shared -o Text_Pandoc_C.so"') # --force-dirty -dynamic
    raise "stack build failed"
  end

  unless File.exist? "Text_Pandoc_C.so"
    raise "unfinished: the haskell library was not properly built"
  end
  puts "built haskell library"
end

Rake::Task["build"].enhance [:build_hs]

