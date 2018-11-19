import Workspace_Experiment as we 

ds = we.load_workspace.get_default_datastore()
print(ds.datastore_type, ds.account_name, ds.container_name)

ds.upload(src_dir='./Python/Regresion Logistica - Clasificacion Imagenes/data', target_path='mnist', overwrite=True, show_progress=True)