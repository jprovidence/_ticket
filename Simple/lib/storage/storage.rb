module Storage

  require 'neosql'

  def self.store(data)
    
    if is_raw? data
      store_raw data
    end

  end


  def self.store_raw(data)

    desc_tag = description_tag(data)

    # create root node
    root = ""

    unless NeoSQL::exists_node_with(:type => "root", :url  => data[:root])
      root = NeoSQL::new_node
      root.set(:type => "root", 
               :url  => data[:root])

    end

    # create content nodes
    data[:content].each do |entry|

      links = links_in(desc_tag, entry)

      if (x = NeoSQL::exists_node_with(:type => "placeholder", :url  => entry[data[:link_tag]]))
        entry.merge!({:type => "entry", :url => entry[data[:link_tag]]})
        x[0].set!(entry)
        set_relations_given(x[0], links)
        root.relationship_with(x[0], "content")
      else
        entry.merge!({:type => "entry", :url => entry[data[:link_tag]]})
        node = NeoSQL::new_node
        node.set(entry)
        set_relations_given(node, links)
        root.relationship_with(node, "content")
      end

    end

  end


  def self.set_relations_given(node, links)

    links.group_by {|l| l}.each do |link, ary|

      if (existing = NeoSQL::exists_node_with(:url => link))
        node.relationship_with(existing, "hyperlink", {:strength => ary.length.to_s})
      else
        new_node = NeoSQL::new_node
        new_node.set({:type => "placeholder", :url => link})
        node.relationship_with(new_node, "hyperlink", {:strength => ary.length.to_s})
      end
        
    end

  end


  def self.description_tag(data)
    
    key  = data.keys[0]

    tags = data[key].collect do |v|
      longest_tag(v)
    end

    tags.group_by {|e| e}.max_by {|x| x.length}.first

  end


  def self.links_in(desc_tag, entry)
    entry[desc_tag].scan(/(https?:\/\/.*?)(?:"|')/)
  end


  def self.is_raw?(data)
    if data.keys.include?(:root) then true else false end
  end

end
