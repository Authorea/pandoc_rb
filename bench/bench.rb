require 'pandoc_rb'
require 'benchmark/ips'
require 'pandoc-ruby'


def test_files_hash(dir='test/files')
  @test_files_hash ||= begin
    hash = {}
    Dir["#{dir}/*"].each do |file|
      extension = PandocRb.reader_from_ext File.extname(file)
      hash[extension] ||= []
      hash[extension] << file
    end
    hash
  end
end


Benchmark.ips do |bench|
  10.times do
    PandocRb.convert 'markdown', 'latex', 'warmup'
  end

  10.times do
    PandocRuby.convert 'warmup', from: 'markdown', to: 'latex'
  end

  # Number of seconds to run each benchmark
  bench.time       = 5 # seconds

  bench.stats = :bootstrap
  bench.confidence = 95 # percent

  test_files_hash.each do |reader, file_paths|
    if PandocRb::Readers.include? reader
      file_paths.each do |file_path|
        file = File.binread file_path
        file.force_encoding "UTF-8"
        bench.report "from: #{reader}, to: Fixnum, file: #{File.basename(file_path)}, sum bytes" do
          file.each_byte.reduce(:+)
        end
        ['markdown', 'latex', 'html', 'docx'].each do |writer|
          GC.start(full_mark: true, immediate_sweep: true)

          # Convert by telling pandoc through a C extension to read the string
          bench.report "from: #{reader}, to: #{writer}, file: #{File.basename(file_path)}, pandoc_rb" do
            PandocRb.convert reader, writer, file
          end

          # Convert by telling pandoc through PandocRuby to read the file
          bench.report "from: #{reader}, to: #{writer}, file: #{File.basename(file_path)}, pandoc-ruby (read file)" do
            PandocRuby.convert [file_path], from: reader, to: writer
          end

          # PandocRuby is incapable of accepting a raw docx file as input
          unless file_path.match(/\.docx/)
            # Convert by telling pandoc through PandocRuby to read stdin
            bench.report "from: #{reader}, to: #{writer}, file: #{File.basename(file_path)}, pandoc-ruby" do
              PandocRuby.convert file     , from: reader, to: writer
            end
          end
        end
      end
    end
  end

  bench.compare!
end

