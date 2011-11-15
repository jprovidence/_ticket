module Refinery

  require 'rubygems'
  require 'xmlsimple'
  require 'net/http'
  require 'uri'

  def self.refine(url)

    uri = URI.parse(url)
    req = Net::HTTP::Get.new(uri.path)
    res = Net::HTTP.start(uri.host, uri.port) do |http|
      http.request(req)
    end

    xml     = XmlSimple.xml_in(res.body)
    trimmed = trim(xml)
    root    = root_of(trimmed)

    {:root     => root[:root], 
     :link_tag => root[:tag],
     :content  => trimmed}

  end


  def self.longest_tag(data)

    high_water = {}

    data.each do |k, v|
      length = v.length
      high_water = {:tag => k, :length => length} if v.length > high_water[:length]
    end

    high_water[:tag]

  end


  def self.root_of(data)

    key  = data.keys[0]

    tags = data[key].collect do |v|
      tags_with_links(v)
    end.flatten

    link_tag = most_common(tags)
    data[key][0][link_tag].match(/(https?:\/\/.*?)\//)

    if $1
      {:tag => link_tag, :root => $1}
    else
      {:tag => link_tag, :root => data[key][0][link_tag]}
    end

  end


  def self.most_common(tags)

    tags = disinclude(tags)

    tags.group_by do |t|
      t
    end.values.max_by {|x| x.length}.first

  end


  def self.disinclude(ary)
    malodorous = ['commentRss', 'commentrss', 'commentsRss', 'commentRss', 'rss', 'comment', 'description']
    ary.select {|x| !malodours.include?(x)}
  end


  def self.tags_with_links(hash)

    ret = []

    hash.each do |k, v|
      ret << k if v =~ /https?:\/\//
    end

    ret

  end


  def trim(simple)

    return nil unless simple.kind_of?(Array) || simple.kind_of?(Hash)            #edge case
    
    unless !simple.kind_of?(Hash) || simple.keys.grep(/(item|entry)/).empty?     #edge case
      return {"item" => simple["item"]} || {"entry" => simple["entry"]}
    end
    
    if simple.kind_of? Array
      
      simple.each do |s|
        result = trim(s)
        return result unless result == nil
      end
      
    elsif simple.kind_of? Hash
      
      simple.keys.each do |key|
        result = trim(simple[key])
        return result unless result == nil
      end
      
    end
    
    return nil
    
  end

end
