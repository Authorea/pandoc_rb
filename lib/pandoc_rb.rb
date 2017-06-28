require 'pandoc_rb/pandoc_rb'
require 'json'
require 'pandoc_rb/error'
require 'pandoc_rb/parse_failure'
require 'pandoc_rb/parsec_error'
require 'pandoc_rb/readers'
require 'pandoc_rb/version'
require 'pandoc_rb/writers'
require 'timeout'


module PandocRb
  def self.raise_exception(result)
    if    /^Unknown reader: / === result
      raise ArgumentError, result
    elsif /^Unknown writer: / === result
      raise ArgumentError, result
    elsif /^Pandoc timed out/ === result
      raise Timeout::Error
    elsif /^Pandoc internal / === result
      raise PandocRb::Error, result.sub(/^Pandoc internal error: /, '')
    end
    result = JSON.parse result
    if    result['tag'] == 'ParseFailure'
      raise PandocRb::ParseFailure.new(result['contents'])
    elsif result['tag'] == 'ParsecError'
      raise PandocRb::ParsecError.new( result['contents'])
    else
      raise "Unknown error type returned from pandoc: #{result}"
    end
  end

  def self.convert(in_format_str, out_format_str, input_str, extract_media_path='')
    unless @PandocRb_loaded
      self.convert_init
      Kernel.at_exit do
        PandocRb.convert_exit
      end
      @PandocRb_loaded = true
    end

    if in_format_str.to_s == 'docx'
      input_str.force_encoding "UTF-8"
    end

    begin
      in_format          = in_format_str.to_s.encode "UTF-8"
      out_format         = out_format_str.to_s.encode "UTF-8"
      input              = input_str.to_s.encode "UTF-8"
      extract_media_path = extract_media_path.to_s.encode "UTF-8"
      success, result    = PandocRb.convert_raw in_format, out_format, input, extract_media_path
      self.raise_exception result if (success == false)
      result
    end
  end

  def self.reader_from_ext(extension)
    {
      ".xhtml"    => "html",
      ".html"     => "html",
      ".htm"      => "html",
      ".md"       => "markdown",
      ".markdown" => "markdown",
      ".tex"      => "latex",
      ".latex"    => "latex",
      ".ltx"      => "latex",
      ".rst"      => "rst",
      ".org"      => "org",
      ".lhs"      => "markdown+lhs",
      ".db"       => "docbook",
      ".opml"     => "opml",
      ".wiki"     => "mediawiki",
      ".dokuwiki" => "dokuwiki",
      ".textile"  => "textile",
      ".native"   => "native",
      ".json"     => "json",
      ".docx"     => "docx",
      ".t2t"      => "t2t",
      ".epub"     => "epub",
      ".odt"      => "odt",
      ".pdf"      => "pdf",
      ".doc"      => "doc",
    }[extension&.downcase]
  end

  def self.reader_from_ext(extension)
    {
      ""          => "markdown",
      ".tex"      => "latex",
      ".latex"    => "latex",
      ".ltx"      => "latex",
      ".context"  => "context",
      ".ctx"      => "context",
      ".rtf"      => "rtf",
      ".rst"      => "rst",
      ".s5"       => "s5",
      ".native"   => "native",
      ".json"     => "json",
      ".txt"      => "markdown",
      ".text"     => "markdown",
      ".md"       => "markdown",
      ".markdown" => "markdown",
      ".textile"  => "textile",
      ".lhs"      => "markdown+lhs",
      ".texi"     => "texinfo",
      ".texinfo"  => "texinfo",
      ".db"       => "docbook",
      ".odt"      => "odt",
      ".docx"     => "docx",
      ".epub"     => "epub",
      ".org"      => "org",
      ".asciidoc" => "asciidoc",
      ".adoc"     => "asciidoc",
      ".pdf"      => "latex",
      ".fb2"      => "fb2",
      ".opml"     => "opml",
      ".icml"     => "icml",
      ".tei.xml"  => "tei",
      ".tei"      => "tei",
    }[extension&.downcase] || !!extension&.downcase&.match(/\.\d$/)
  end


end

