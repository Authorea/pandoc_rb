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
  unless system('stack build --force-dirty --ghc-options="-no-hs-main -o lib/Text.Pandoc.C.so"')
    raise "stack build failed"
  end

  unless File.exist? "lib/Text.Pandoc.C.so"
    raise "unfinished: the haskell library was not properly built"
  end
  puts "built haskell library"
end

Rake::Task["build"].enhance [:build_hs]

