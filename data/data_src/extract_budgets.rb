#!/usr/bin/env ruby

require 'nokogiri'
require 'optparse'

REVENUE_HEADER  = %w{name conference year ticket_sales contributions rights student_fees school_funds other total_revenue}
EXPENSES_HEADER = %w{name conference year coaching scholarships building other total_expense}

script_dir = File.dirname(__FILE__)
REVENUE_FILE = File.join(script_dir, "revenue.tsv")
EXPENSE_FILE = File.join(script_dir, "expense.tsv")

def parse_option(argv)
  opts = {
    header: false
  }
  parser = OptionParser.new()
  parser.on('--print-header', "print header before appending rows")do|v|
    opts[:header] = true
  end
  parser.parse!
  opts
end

def textContent(elem)
  elem.children.first.to_s.strip
end

def makeRows(name, conference, trElemArray)
  result = []
  for trElem in trElemArray do
    tdElems = trElem.children.select{|elem|elem.name == 'td'}
    result << [name, conference, * tdElems.map{|elem| textContent(elem).gsub(/[,$]/,'').to_i}].join("\t")
  end
  result.join("\n") + "\n"
end

def extractRowsFromTable(html_doc, write_header)
  name = textContent(html_doc.css("p.sp-subhead-profile-schoolname").first).gsub(/&amp;/,'&')
  conference = textContent(html_doc.css("p.sp-subhead-profile-schoolname ~ p").first).split(": ")[-1]
  revenues = html_doc.css("table.Revenues tbody tr")
  expenses = html_doc.css("table.Expenses tbody tr")

  File.open(REVENUE_FILE,'a')do |f|
    if(write_header)
      f.puts REVENUE_HEADER.join("\t")
    end
    f.write(makeRows(name, conference, revenues))
  end

  File.open(EXPENSE_FILE,'a')do |f|
    if(write_header)
      f.puts EXPENSES_HEADER.join("\t")
    end
    f.write(makeRows(name, conference, expenses))
  end
end

def main(opts)
  html_doc = Nokogiri::HTML(STDIN)
  extractRowsFromTable(html_doc, opts[:header])
end

if __FILE__ == $0
  opts = parse_option(ARGV)
  main(opts)
end
