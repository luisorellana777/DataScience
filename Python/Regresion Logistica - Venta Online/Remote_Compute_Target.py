import numpy as np
import matplotlib
import matplotlib.pyplot as plt

import azureml
from azureml.core import Workspace, Run
from azureml.core.compute import BatchAiCompute
from azureml.core.compute import ComputeTarget
import os


ws = Workspace.from_config()


# choose a name for your cluster
batchai_cluster_name = os.environ.get("BATCHAI_CLUSTER_NAME", ws.name + "gpu")
cluster_min_nodes = os.environ.get("BATCHAI_CLUSTER_MIN_NODES", 1)
cluster_max_nodes = os.environ.get("BATCHAI_CLUSTER_MAX_NODES", 3)
vm_size = os.environ.get("BATCHAI_CLUSTER_SKU", "STANDARD_NC6")
autoscale_enabled = os.environ.get("BATCHAI_CLUSTER_AUTOSCALE_ENABLED", True)



print('creating a new compute target...')
provisioning_config = BatchAiCompute.provisioning_configuration(vm_size = vm_size, # NC6 is GPU-enabled
                                                            vm_priority = 'lowpriority', # optional
                                                            autoscale_enabled = autoscale_enabled,
                                                            cluster_min_nodes = cluster_min_nodes, 
                                                            cluster_max_nodes = cluster_max_nodes)

# create the cluster
compute_target = ComputeTarget.create(ws, batchai_cluster_name, provisioning_config)

# can poll for a minimum number of nodes and for a specific timeout. 
# if no min node count is provided it will use the scale settings for the cluster
compute_target.wait_for_completion(show_output=True, min_node_count=None, timeout_in_minutes=20)

    # For a more detailed view of current BatchAI cluster status, use the 'status' property    
print(compute_target.status.serialize())