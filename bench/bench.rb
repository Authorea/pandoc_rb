require 'pandoc-rb'
require 'benchmark'
require 'pandoc-ruby'

lots_of_markdown = File.read("/Users/michaelklein/Coding/scratch/s22/md/markdown-here.wiki/Markdown-Cheatsheet.md")

n = 1000
puts "n = #{n}"
Benchmark.bm(11) do |x|
  x.report("empty:") do
    n.times do
      PandocRB::convert("markdown", "latex", "")
    end
  end
  x.report("empty p-r:") do
    n.times do
      PandocRuby.convert("", {:from => :markdown, :to => :latex})
    end
  end
  x.report("longer:") do
    n.times do
      PandocRB::convert("markdown", "latex", lots_of_markdown)
    end
  end
  x.report("longer p-r:") do
    n.times do
      PandocRuby.convert(lots_of_markdown, {:from => :markdown, :to => :latex})
    end
  end
end

