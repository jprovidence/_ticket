class UniqueIndexOnDetritus < ActiveRecord::Migration
  def up
    add_index :detritus, :link, :unique => true
  end

  def down
  end
end
