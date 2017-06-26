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

task :nuke do
  Dir.chdir(__dir__) do
    FileUtils.rm_rf 'tmp'
    FileUtils.rm_rf 'ext/pandoc_rb/.stack-work'
    FileUtils.rm_f  'ext/pandoc_rb/a.out'
    Dir['**/*.so'].each do |so|
      FileUtils.rm_f so
    end
  end
end


