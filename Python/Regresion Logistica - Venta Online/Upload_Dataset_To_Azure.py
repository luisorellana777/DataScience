import Workspace_Experiment as we 

ds = we.load_workspace.get_default_datastore()
print(ds.datastore_type, ds.account_name, ds.container_name)

ds.upload(src_dir='./Python/Regresion Logistica - Venta Online/Social_Network_Ads.csv', target_path='mnist', overwrite=True, show_progress=True)