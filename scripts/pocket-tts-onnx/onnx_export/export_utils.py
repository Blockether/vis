import torch

def flatten_state(state):
    """
    Flattens a nested dictionary state into a list of tensors.
    """
    flat = []
    
    # Sort keys to ensure deterministic order
    for key in sorted(state.keys()):
        value = state[key]
        if isinstance(value, dict):
            flat.extend(flatten_state(value))
        elif isinstance(value, torch.Tensor):
            flat.append(value)
        else:
            # Skip non-tensor values or handle them if necessary
            pass
            
    return flat

def unflatten_state(flat_list, structure):
    """
    Reconstructs the nested dictionary state from a flat list of tensors.
    'structure' should be a dictionary reflecting the structure of the state,
    where leaf nodes can be anything (shapes, dummy tensors, etc.)
    """
    state = {}
    idx = 0
    
    for key in sorted(structure.keys()):
        value = structure[key]
        if isinstance(value, dict):
            sub_state, consumed = unflatten_state(flat_list[idx:], value)
            state[key] = sub_state
            idx += consumed
        else:
            # Assuming leaf node corresponds to a tensor in flat_list
            # Clone to ensure we don't modify the input tensor in-place, which ONNX dislikes for inputs
            state[key] = flat_list[idx].clone()
            idx += 1
            
    return state, idx

def get_state_structure(state):
    """
    Returns the structure of the state dict (useful for unflattening).
    """
    structure = {}
    for key in sorted(state.keys()):
        value = state[key]
        if isinstance(value, dict):
            structure[key] = get_state_structure(value)
        else:
            structure[key] = "tensor" # Placeholder
    return structure
