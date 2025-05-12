# 🔬 Multi-Object Representation Analysis Pipeline

A general-purpose experimental framework for analyzing hidden layer structures in deep learning models trained on occluded or overlapping visual inputs. Originally developed for CNN-based digit separation, this pipeline is designed to support modern architectures like Capsule Networks, Transformers, and ResNets.

---

## 🧠 Key Capabilities

- **Custom synthetic data generation** (e.g., overlapping MNIST pairs)
- **Embedding extraction across layers** (input, conv, dense, capsule, etc.)
- **Pairwise distance computation** (same class, partial overlap, fully different)
- **Distribution visualization** (boxplot, PCA, distance histograms)
- **Generalization testing** by withholding classes during training

---

## 📁 Project Structure

```
overlapping-analysis/
├── data/
│   └── csv_exports/                ← Distance results for plotting
├── models/
│   ├── cnn_model.py               ← Base CNN model
│   ├── capsule_model.py           ← Optional: CapsuleNet
├── utils/
│   ├── pair_generator.py          ← Synthetic pair creator
│   ├── metrics.py                 ← Euclidean, cosine, etc.
│   └── visualizer.py              ← boxplot, PCA, etc.
├── notebooks/
│   └── analysis_cnn_vs_capsule.ipynb
├── report/
│   └── NNreport.pdf               ← Original paper summary
├── README.md
└── requirements.txt
```

---

## 🧪 Experimental Design

### 1. Data Pairing
- Generate `N` image pairs with:  
  - Identical digit overlap (same)  
  - One-digit shared (half)  
  - No digit shared (diff)

### 2. Model Variants
- CNN baseline
- Capsule Network
- [Optional] Vision Transformer, ConvNeXt, etc.

### 3. Layer Sampling
Extract representations from:
- Input image (flattened)
- Mid-level convolution/capsule layer
- Final latent/dense layer

### 4. Distance Computation
- Pairwise Euclidean distance between image pairs
- Classify distance by overlap category (same/half/diff)

### 5. Visualization
- Save results to CSV
- Use seaborn/matplotlib to plot boxplots per layer
- Optionally reduce to 2D with PCA or t-SNE

---

## 📊 Sample Output (Boxplot)

![boxplot](./report/figure5_boxplot_layers.png)

---

## 🧠 Why It Matters

- Provides a generalizable framework for understanding how models separate overlapping concepts
- Can be reused across vision and language models with discrete class outputs
- Demonstrates model capacity to disentangle entangled representations

---

## 🔄 Future Extensions

- Add support for embedding space trajectory over training
- Evaluate impact of attention mechanisms
- Extend to multimodal embeddings (e.g. text+image)
- Integrate with open source explainability tools (e.g., Captum, SHAP)

---

## 📜 License
MIT
