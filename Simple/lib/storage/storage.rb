module Storage

  require 'neosql'

  def self.store(data)
    
    if is_raw? data
      store_raw data
    end

  end


  def self.store_raw(data)

    # create root node
    unless NeoSQL::exists_node_with(:type => "root", :url  => data[:root])
      node = NeoSQL::new_node
      node.set(:type => "root", 
               :url  => data[:root])

    end

    data[:content].each do |entry|

      if (x = NeoSQL::exists_node_with(:type => "placeholder", :url  => entry[data[:link_tag]]))
        entry.merge!({:type => "entry"})
        x[0].set!(entry)
      else
        entry.merge!({:type => "entry"})
        node = NeoSQL::new_node
        node.set(entry)
      end

    end

  end


  def self.is_raw?(data)
    if data.keys.include?(:root) then true else false end
  end

end
