
import torch
import torch.nn as nn
from pocket_tts.models.flow_lm import FlowLMModel
from pocket_tts.models.mimi import MimiModel
from onnx_export.export_utils import unflatten_state, flatten_state
from pocket_tts.modules.stateful_module import increment_steps

class FlowLMWrapper(nn.Module):
    def __init__(self, flow_lm: FlowLMModel, state_structure):
        super().__init__()
        self.flow_lm = flow_lm
        self.state_structure = state_structure

        # Fixed parameters for export
        self.lsd_decode_steps = 1
        # Use temp=0.0 for deterministic output/verification
        self.temp = 0.0
        self.noise_clamp = 10.0
        self.eos_threshold = -4.0

    def forward(self, sequence, text_embeddings, flat_state):
        # unflatten state
        model_state, _ = unflatten_state(flat_state, self.state_structure)

        if torch.jit.is_tracing():
            from torch.onnx import operators
            seq_len = operators.shape_as_tensor(sequence)[1]
            text_len = operators.shape_as_tensor(text_embeddings)[1]
        else:
            seq_len = sequence.shape[1]
            text_len = text_embeddings.shape[1]

        # Replace NaN with bos_emb
        sequence = torch.where(torch.isnan(sequence), self.flow_lm.bos_emb, sequence)

        # Project sequence through input_linear
        input_ = self.flow_lm.input_linear(sequence)

        # Backbone: concatenate text_embeddings with input_
        combined = torch.cat([text_embeddings, input_], dim=1)

        # Run transformer
        transformer_out = self.flow_lm.transformer(combined, model_state)

        # Apply output norm
        if self.flow_lm.out_norm:
            transformer_out = self.flow_lm.out_norm(transformer_out)

        # CRITICAL: Match PyTorch's backbone slicing behavior exactly!
        # PyTorch does: transformer_out = transformer_out[:, -seq_len:]
        # This removes the text_embeddings positions and keeps only sequence positions.
        # For conditioning (seq_len=0), this returns an empty tensor.
        # For AR (seq_len=1), this returns just the last position.
        
        if torch.jit.is_tracing():
            from torch.onnx import operators
            T_out = operators.shape_as_tensor(transformer_out)[1]
            start = T_out - seq_len
            sliced_out = torch.narrow(transformer_out, 1, start, seq_len)
        else:
            sliced_out = transformer_out[:, transformer_out.shape[1] - seq_len:]
        
        sliced_out = sliced_out.to(torch.float32)
        
        # PyTorch: if transformer_out.shape[1] == 0, return dummy values
        # But we can't do conditional returns in ONNX tracing.
        # Instead, we need to handle both cases in a traceable way.
        
        # For ONNX, we always compute the EOS/latent from the LAST position of
        # the ORIGINAL transformer_out (before slicing). This works because:
        # - During AR (seq_len=1): last position is the sequence position (correct)
        # - During conditioning (seq_len=0): last position is last text position (wrong, but ignored)
        # The key insight is that during conditioning, the EOS/latent output is IGNORED
        # by the inference script - only the state updates matter.
        
        # So we compute from the full output's last position for traceability,
        # knowing conditioning outputs are discarded.
        last_pos = transformer_out[:, -1].to(torch.float32)  # [B, D]

        # Check EOS
        out_eos = self.flow_lm.out_eos(last_pos) > self.eos_threshold

        # Generate latent using flow_net with LSD decode
        noise_shape = last_pos.shape[:-1] + (self.flow_lm.ldim,)
        
        # Handle temp=0 (deterministic) case - use zeros instead of random
        if self.temp == 0.0:
            noise = torch.zeros(noise_shape, dtype=last_pos.dtype, device=last_pos.device)
        else:
            std = self.temp ** 0.5
            noise = torch.empty(noise_shape, dtype=last_pos.dtype, device=last_pos.device)
            if self.noise_clamp is None:
                torch.nn.init.normal_(noise, mean=0.0, std=std)
            else:
                torch.nn.init.trunc_normal_(noise, mean=0.0, std=std, a=-self.noise_clamp, b=self.noise_clamp)

        from functools import partial
        from pocket_tts.models.flow_lm import lsd_decode
        conditioned_flow = partial(self.flow_lm.flow_net, last_pos)
        result = lsd_decode(conditioned_flow, noise, self.lsd_decode_steps)

        # Calculate proper increment: sequence length + text_embeddings length
        increment = seq_len + text_len
        
        increment_steps(self.flow_lm, model_state, increment=increment)

        new_flat_state = flatten_state(model_state)

        # Return tuple of tensors including the list elements flattened
        return (result, out_eos, *new_flat_state)

class MimiWrapper(nn.Module):
    def __init__(self, mimi: MimiModel, state_structure, emb_std=None, emb_mean=None):
        super().__init__()
        self.mimi = mimi
        self.state_structure = state_structure
        
        # Buffers for un-normalization
        if emb_std is not None:
            self.register_buffer("emb_std", emb_std)
        else:
            self.register_buffer("emb_std", torch.ones(1))
            
        if emb_mean is not None:
            self.register_buffer("emb_mean", emb_mean)
        else:
            self.register_buffer("emb_mean", torch.zeros(1))

    def forward(self, latent, flat_state):
        try:
            # Un-normalize latent: scale and shift back
            mimi_decoding_input = latent * self.emb_std + self.emb_mean
            
            # Transpose: [B, T, D] -> [B, D, T]
            transposed = mimi_decoding_input.transpose(-1, -2)
            
            # Project: [B, dim, 1]
            quantized = self.mimi.quantizer(transposed)
            
            model_state, _ = unflatten_state(flat_state, self.state_structure)
            
            # Decode
            audio_frame = self.mimi.decode_from_latent(quantized, model_state)

            if torch.jit.is_tracing():
                from torch.onnx import operators
                seq_len = operators.shape_as_tensor(latent)[1]
            else:
                seq_len = latent.shape[1]
            
            # Increment by the hop factor (200Hz transformer / 12.5Hz latent = 16)
            increment = seq_len * 16
            increment_steps(self.mimi, model_state, increment=increment)
            
            new_flat_state = flatten_state(model_state)
            
            return (audio_frame, *new_flat_state)
        except Exception as e:
            print(f"Error in MimiWrapper forward: {e}")
            raise e


class MimiEncoderWrapper(nn.Module):
    """Wrapper for Mimi encoder that takes raw audio and returns latent embeddings."""
    def __init__(self, mimi: MimiModel, speaker_proj_weight=None):
        super().__init__()
        self.mimi = mimi
        if speaker_proj_weight is not None:
            self.register_buffer("speaker_proj_weight", speaker_proj_weight)
        else:
            self.speaker_proj_weight = None

    def forward(self, audio):
        # audio: [B, C, T] -> latent: [B, T', D]
        encoded = self.mimi.encode_to_latent(audio)
        # encoded is [B, D, T'], we need [B, T', D]
        latents = encoded.transpose(-1, -2)
        
        # Apply speaker projection if available
        if self.speaker_proj_weight is not None:
            latents = torch.nn.functional.linear(latents, self.speaker_proj_weight)
        
        return latents


class TextConditionerWrapper(nn.Module):
    """Wrapper for text conditioner that takes token IDs and returns embeddings."""
    def __init__(self, conditioner):
        super().__init__()
        self.conditioner = conditioner

    def forward(self, token_ids):
        # token_ids: [B, T] -> embeddings: [B, T, D]
        from pocket_tts.conditioners.base import TokenizedText
        return self.conditioner(TokenizedText(token_ids))
