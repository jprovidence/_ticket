class CreateDetritus < ActiveRecord::Migration
  def change
    create_table :detritus do |t|
      t.text :link
      t.integer :processed

      t.timestamps
    end
  end
end
