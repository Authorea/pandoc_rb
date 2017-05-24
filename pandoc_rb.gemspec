Gem::Specification.new do |s|
  s.name          =  'pandoc_rb'
  s.version       =  '0.1.0.0'
  s.date          =  '2017-05-24'
  s.summary       =  "FFI bindings to Pandoc"
  s.description   =  "Performant bindings to Pandoc through C"
  s.authors       =  ["Michael Klein"]
  s.email         =  'lambdamichael@gmail.com'
  s.extensions    << "ext/extconf.rb"
  s.files         =  Dir["lib/**/*.rb"] + Dir["test/**/*.rb"] + Dir["bench/**/*.rb"]
  s.require_paths << 'ext'
  s.homepage    =    'http://rubygems.org/gems/pandoc_rb'
  s.license     =    'BSD-3-Clause'
end

