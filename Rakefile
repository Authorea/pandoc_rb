require "bundler/gem_tasks"
require "rake/testtask"
require 'rake/extensiontask'

Rake::ExtensionTask.new 'pandoc_rb'

Rake::ExtensionTask.new "pandoc_rb" do |ext|
  ext.lib_dir = "lib/pandoc_rb"
end

Rake::TestTask.new(:test) do |t|
  t.libs << "test"
  t.libs << "lib"
  t.test_files = FileList["test/**/*_test.rb"]
end

desc "Run tests"
task :default => :test

