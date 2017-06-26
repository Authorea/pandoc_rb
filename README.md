[![Build Status](https://travis-ci.org/michaeljklein/pandoc-rb.svg?branch=master)](https://travis-ci.org/michaeljklein/pandoc-rb)

Requires haskell-stack

# PandocRb

## Installation

Add this line to your application's Gemfile:

```ruby
gem 'pandoc_rb'
```

And then execute:

    $ bundle install

Or install it yourself as:

    $ gem install pandoc_rb

## Usage

`pandoc_rb`'s main interface is contained in the `PandocRb` module.

The public interface consists of constants and a main conversion function

The constants include:
```ruby
PandocRb::Version # The current version
PandocRb::Readers # A list of allowed readers
PandocRb::Writers # A list of allowed writers
```

And the main conversion function is:
```ruby
# General conversion function
PandocRb.convert input_format, output_format, input_string, (optional) extract_media_path

# Convert `markdown` to `latex`
PandocRb.convert 'markdown', 'latex', '_italic text_'

# Convert `docx` to `latex` from file
PandocRb.convert 'docx', 'latex', File.binread('some_doc.docx'), `extract/figures/dir`

# Convert `markdown` to `docx`, writing to a `docx` file
File.open 'some_doc.docx', 'wb' do |file|
  file.write PandocRb.convert('markdown', 'docx', '_italic text_')
end
```

## Contributing

1. Fork it ( https://github.com/michaeljklein/pandoc_rb/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

