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
  unless system [ "(which stack || (echo 'installing stack' && curl -sSL https://get.haskellstack.org/ | sh))",
                  "export PATH=$HOME/.local/bin:$PATH",
                  "stack update",
                  "stack setup" ].join(" && ")
    raise "setting up stack failed"
  end

  puts "beginning to build haskell library"
  unless system "stack ghc -- -Wall -fno-warn-orphans -O3 -split-objs --make -no-hs-main -optc-O3 -optl '-shared' -o Text_Pandoc_C.so src/Text/Pandoc/C.hs"
    raise "stack build failed"
  end

  unless File.exist? "Text_Pandoc_C.so"
    raise "unfinished: the haskell library was not properly built"
  end
  puts "built haskell library"
end

Rake::Task["build"].enhance [:build_hs]

