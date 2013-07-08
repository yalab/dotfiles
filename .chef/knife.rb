knife[:solo_path] = '/tmp/chef-solo'

cookbook_path ["cookbooks", "site-cookbooks"]
role_path     "roles"
data_bag_path "data_bags"
encrypted_data_bag_secret "encrypted_data_bag_secret"
