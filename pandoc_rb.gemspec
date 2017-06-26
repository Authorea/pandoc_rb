# coding: utf-8

lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "pandoc_rb/version"

$gem_spec = Gem::Specification.new do |spec|
  spec.name          = "pandoc_rb"
  spec.platform      = Gem::Platform::RUBY
  spec.version       = PandocRb::VERSION
  spec.authors       =  ["Michael Klein"]
  spec.email         =  ['lambdamichael@gmail.com']

  spec.summary       =  "FFI bindings to Pandoc"
  spec.description   =  "Fast bindings to Pandoc through Ruby's C FFI gem and Haskell's C FFI"
  spec.homepage      =  'http://rubygems.org/gems/pandoc_rb'
  spec.license       =  'BSD-3-Clause'

  # Prevent pushing this gem to RubyGems.org. To allow pushes either set the 'allowed_push_host'
  # to allow pushing to a single host or delete this section to allow pushing to any host.
  if spec.respond_to?(:metadata)
    spec.metadata["allowed_push_host"] = "TODO: Set to 'http://mygemserver.com'"
  else
    raise "RubyGems 2.0 or newer is required to protect against " \
      "public gem pushes."
  end

  spec.files         = `git ls-files -z`.split("\x0").reject do |f|
    f.match(%r{^(test|spec|features)/}) || f.match(/\.(so|o)$/)
  end
  spec.bindir        = "bin"
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]
  spec.extensions    = ['ext/pandoc_rb/extconf.rb']

  spec.add_development_dependency "bundler", "~> 1.15.1"
  spec.add_development_dependency "rake", "~> 10.0"
  spec.add_development_dependency "minitest", "~> 5.0"
  spec.add_development_dependency "pry"
  spec.add_development_dependency "pry-byebug"
  spec.add_development_dependency "rake-compiler"
end

