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
  unless system('stack build --force-dirty --ghc-options="-no-hs-main -dynamic -fPIC -shared -o ext/Text_Pandoc_C.so"') # --force-dirty -dynamic
    raise "stack build failed"
  end

  unless File.exist? "ext/Text_Pandoc_C.so"
    raise "unfinished: the haskell library was not properly built"
  end
  puts "built haskell library"
end

Rake::Task["build"].enhance [:build_hs]

