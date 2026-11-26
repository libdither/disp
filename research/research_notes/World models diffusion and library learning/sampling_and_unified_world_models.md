# Sampling from world models: diffusion and Langevin as joint-distribution samplers, and unified time × noise world models

How these notes were checked: every link below was minted on 2026-11-05 by `scripts/verify-source.sh -q '<quote>' <source>`. Every quoted string is verbatim from the fetched text, and the `#page=N` or `#anchor` fragment is where the script found it. Two fetch notes. OpenReview serves a browser challenge, so LeCun 2022 is read through the Wayback capture of its OpenReview PDF (original: https://openreview.net/pdf?id=BZ5a1r-kVsf). openai.com returns 403, so the Sora report is read through its Wayback capture. Anything not run through the script is marked UNVERIFIED. Equations without a citation are my own framing, and the Inferences bullets say so.

Overlap with sibling researchers: the Tweedie / denoising-score-matching math and the JEPA-vs-denoiser architecture comparison are left to them. They come up here only where sampling or planning needs them.

---

## Q1. Can diffusion or Langevin sampling be read as drawing a representative state from a world model, i.e. a sample from its joint distribution? (energy-based view, annealing)

### Takeaway
Yes, with conditions. Langevin dynamics turns any differentiable energy E into a sampler for the Gibbs distribution p ∝ e^{−E}. A diffusion model is annealed Langevin (or its ODE equivalent) over a family of noise-smoothed energies, so it is an amortized sampler of exactly that kind. The catches:
- The sample is "representative" only if e^{−E} is actually calibrated to data. Denoising-trained scores are calibrated (that math belongs to the sibling notes). JEPA-style energies are not: LeCun explicitly treats them as zero-temperature objects that are minimized, not sampled.
- A world model that is only conditional, E(x, y) or P(s′ | s, a), yields a joint only together with an initial-state distribution and a policy.
- Plain Langevin mixes badly between modes. Annealing over noise levels is what makes the sampler pick among modes with the right weights.

### Cited Findings

**The sampling machinery (energy → samples)**
- Discretized Langevin dynamics, x_{k+1} = x_k − (η/2) ∇_x E(x_k) + √η ξ_k with ξ_k ~ N(0, I), has the stationary law p(x) ∝ exp(−E(x)) as η → 0. SGLD shows that adding noise to stochastic gradient steps moves smoothly from optimization (finding the mode) to sampling (drawing from the posterior). Quote: "seamless transition between optimization and Bayesian posterior sampling" — [Welling & Teh 2011, p.1](https://www.stats.ox.ac.uk/~teh/research/compstats/WelTeh2011a.pdf#page=1)
- Energy-based models (EBMs) generate implicitly: a sample is drawn from x ~ e^{−E(x)} by MCMC, specifically Langevin. Quotes: "they allow the use of an implicit sample generation procedure" — [Du & Mordatch 2019, p.1](https://arxiv.org/pdf/1903.08689#page=1); "which uses gradient information for effective sampling" — [Du & Mordatch 2019, p.2](https://arxiv.org/pdf/1903.08689#page=2)
- The original diffusion model framed generation as reversing a slow corruption, borrowed from non-equilibrium physics. Quotes: "systematically and slowly destroy structure in a data distribution"; "learn a reverse diffusion process that restores structure in data" — [Sohl-Dickstein et al. 2015, p.1](https://arxiv.org/pdf/1503.03585#page=1)

**Why annealing over noise levels matters (mixing between modes)**
- Plain Langevin on the data score has two failures. Scores are badly estimated far from the data manifold, and chains cannot cross low-density gaps between modes. Quotes: "slows down the mixing of Langevin dynamics sampling"; "mixing can be difficult because of the need of traversing low density regions" — [Song & Ermon 2019, p.2](https://arxiv.org/pdf/1907.05600#page=2)
- Concretely, Langevin gets the mixture weights of separated modes wrong, and annealing fixes this. Quotes: "will not be able to correctly recover the relative weights of these two modes in reasonable time" — [Song & Ermon 2019, p.4](https://arxiv.org/pdf/1907.05600#page=4); "annealed Langevin dynamics recover the relative weights faithfully" — [Song & Ermon 2019, p.5](https://arxiv.org/pdf/1907.05600#page=5)
- Annealed Langevin: perturb the data, p_σ(x) = ∫ p(y) N(x; y, σ²I) dy, learn s_θ(x, σ) ≈ ∇_x log p_σ(x), and run Langevin at σ_1 > σ_2 > … > σ_L. Quote: "we use gradients corresponding to gradually decreasing noise levels" — [Song & Ermon 2019, p.1](https://arxiv.org/pdf/1907.05600#page=1)
- The continuous-time version has three pieces. Forward SDE: dx = f(x, τ) dτ + g(τ) dW. Reverse SDE: dx = [f − g² ∇_x log p_τ(x)] dτ + g dW̄. Deterministic probability-flow ODE: dx = [f − ½ g² ∇_x log p_τ(x)] dτ. Both reverse processes need only the score. Quotes: "the reverse-time SDE depends only on the time-dependent gradient field" — [Song et al. 2021, p.1](https://arxiv.org/pdf/2011.13456#page=1); "a deterministic process that samples from the same distribution as the SDE" — [Song et al. 2021, p.4](https://arxiv.org/pdf/2011.13456#page=4); "We name the ODE in Eq. (13) the probability flow ODE" — [Song et al. 2021, p.7](https://arxiv.org/pdf/2011.13456#page=7)
- The same paper notes that conditioning (the basis of guidance and planning, see Q2) can be added at sampling time. Quote: "We can modulate the generation process by conditioning on information not available during training" — [Song et al. 2021, p.2](https://arxiv.org/pdf/2011.13456#page=2)
- What annealing does dynamically: the reverse process first goes through a "speciation" transition, where coarse structure (which mode or class) is decided. Later comes a "collapse" toward individual data points. Quotes: "transition where the gross structure of data is unraveled"; "It is followed at later time by a" ['collapse' transition]; "the speciation time can be found from a spectral analysis of the correlation matrix" — [Biroli, Bonnaire, de Bortoli & Mézard 2024, p.1](https://arxiv.org/pdf/2402.18491#page=1)

**Energy-based world models: do they sample or minimize?**
- LeCun's JEPA (Joint-Embedding Predictive Architecture) is explicitly non-generative, and its energy is a zero-temperature object. Turning it into a probability is optional and not the intended use. Quotes: "a non-generative architecture for predictive world models" — [LeCun 2022, p.2](https://web.archive.org/web/20241231120746id_/https://openreview.net/pdf?id=BZ5a1r-kVsf#page=2); "should be called a zero-temperature free energy"; "many EBMs can easily be turned into probabilistic models, e.g. through a Gibbs distribution, this is not at all a necessity" — [LeCun 2022, p.20](https://web.archive.org/web/20241231120746id_/https://openreview.net/pdf?id=BZ5a1r-kVsf#page=20)
- LeCun does sample latent variables, not states: z is drawn from a Gibbs distribution over the latent regularizer to get multiple predictions. Quote: "Each sample leads to a different prediction" — [LeCun 2022, p.33](https://web.archive.org/web/20241231120746id_/https://openreview.net/pdf?id=BZ5a1r-kVsf#page=33)
- His "Mode-2" planning is optimization against the world model, and "Mode-1" is an amortized policy that approximates it. Quote: "The policy module can be seen as performing a form of amortized inference" — [LeCun 2022, p.12](https://web.archive.org/web/20241231120746id_/https://openreview.net/pdf?id=BZ5a1r-kVsf#page=12)
- V-JEPA 2-AC (the action-conditioned V-JEPA 2) plans by minimizing an energy in latent space, E(â_{1:T}; z_k, s_k, z_g) = ‖P(â_{1:T}; s_k, z_k) − z_g‖₁, using the cross-entropy method (CEM, a sampling-based optimizer). It does not run a Langevin sampler. Quotes: "by minimizing a goal-conditioned energy function" — [V-JEPA 2 (Meta FAIR 2025), p.10](https://arxiv.org/pdf/2506.09985#page=10); "The L1 loss is optimized with respect to the actions"; "using the cross-entropy method" — [V-JEPA 2, p.11](https://arxiv.org/pdf/2506.09985#page=11)
- Energy-Based World Models (EBWM, 2024) refine a predicted future state by MCMC, but found that the Langevin noise did not help and dropped it. That leaves pure gradient descent. Quotes: "MCMC is used to iteratively refine on the prediction of a future state" — [Gladstone et al. 2024, p.6](https://arxiv.org/pdf/2406.08862#page=6); "The model achieved a similar loss with and without Langevin dynamics" — [Gladstone et al. 2024, p.7](https://arxiv.org/pdf/2406.08862#page=7)
- Energy-Based Transformers (EBT, 2025) likewise predict by gradient descent on a learned energy, which they call "thinking". Quotes: "enabling predictions through gradient descent-based energy minimization until convergence"; "re-framing prediction problems as optimization with respect to this verifier" — [Gladstone et al. 2025, p.1](https://arxiv.org/pdf/2507.02092#page=1); "Optimizing over a learned energy landscape naturally allows for dynamic compute allocation" — [EBT, p.4](https://arxiv.org/pdf/2507.02092#page=4)
- Implicit Behavioral Cloning (IBC) shows why an energy is attractive as a policy or world model: it can represent multi-valued, discontinuous maps that a direct regressor cannot. Quotes: "potentially discontinuous and multi-valued (set-valued) functions"; "implicit behavioral cloning policies with energy-based models (EBM) often outperform" — [Florence et al. 2021, p.1](https://arxiv.org/pdf/2109.00137#page=1)
- Equilibrium Matching (EqM, 2025) closes the gap from the diffusion side. It learns one time-independent energy gradient instead of noise-conditioned velocities, and samples by gradient descent on it. Quotes: "learns the equilibrium gradient of an implicit energy landscape"; "samples are obtained by gradient descent on the learned landscape" — [Wang & Du 2025 (EqM), p.1](https://arxiv.org/pdf/2510.02300#page=1)
- Evidence from 2026 that sampling energy-defined latents is hard in practice. Quotes: "we have to resort to MCMC sampling techniques for EBMs"; "sampling approaches start to break down when handling continuous latents" — [Learning Latent Action World Models In The Wild 2026, p.19](https://arxiv.org/pdf/2601.05230#page=19)

### Inferences
- **The precise statement (my framing).** Take a world-model energy E_θ(x, y) (JEPA, EBWM, EBT, IBC) and add a temperature: p_β(y | x) ∝ exp(−β E_θ(x, y)).
  - β → ∞ (zero temperature) gives the argmin. That is what V-JEPA 2 (CEM), EBWM and EBT (gradient descent), and LeCun's Mode-2 planning compute.
  - β = 1 with Langevin gives samples.
  - A diffusion model is the β = 1 case made practical. It learns a whole family E_σ = −log p_σ (indexed by noise level σ), whose gradients are calibrated by denoising, and anneals σ → 0. So "diffusion = sampling a representative state from the world model" is accurate when the energy is the denoising-trained −log p_σ.
- **Mode vs. typical sample.** Minimizing gives the single most likely prediction. Sampling gives a typical one. For multimodal futures (a car may turn left or right) the argmin can be an atypical average or one arbitrary mode. Annealed sampling picks a mode with roughly the right probability. Biroli et al.'s "speciation" is the moment that choice gets made.
- **Joint, not just conditional.** A JEPA energy E(x, y) scores y given context x. On its own it does not define p(x) or a marginal over "states of the world". A joint sample over a trajectory requires p(s_1) · Π_t π(a_t | s_t) P(s_{t+1} | s_t, a_t), so you also need an initial-state distribution and a behaviour policy. A diffusion model trained on whole clips or trajectories (Q2) learns that joint directly, under the data-collection policy.
- **Calibration risk for JEPA energies.** Non-contrastive JEPA training only has to make the energy low on data and avoid collapse. Nothing forces e^{−E} to match data frequencies. LeCun's zero-temperature framing, EBWM dropping Langevin, and the 2026 latent-action paper's MCMC breakdown are all consistent with this. Langevin on an off-the-shelf JEPA energy would sample something, but with no guarantee it is "representative".
- **What annealing buys, in one line.** High noise makes the energy smooth and nearly convex, so chains can cross between modes and the coarse choice is made with correct weights. Lower noise then sharpens details within the chosen mode. This is also the bridge to the scale axis in Q4.

### Gaps
- I found no primary source that runs Langevin or annealed sampling on a trained I-JEPA / V-JEPA energy and checks whether the samples match the data distribution. Whether JEPA energies are usable as samplers is untested in what I found.
- The EqM author list ("Wang & Du") is inferred from the Equilibrium Forcing paper's citation and author overlap, not read off the EqM title page.

---

## Q2. Joint vs. conditional world models; planning as inference by guided sampling

### Takeaway
A one-step world model learns P(s_{t+1} | s_t, a_t) and builds trajectories by chaining (compounding error). A diffusion model over whole trajectories learns p(s_{1:T}, a_{1:T}) and gets every conditional by clamping known entries (inpainting). Planning then becomes sampling from the trajectory prior reweighted by an optimality term, which is exactly Levine's "control as inference". Diffuser states that sampling and planning "become nearly identical". The JEPA camp (and NWM, the Navigation World Model) does the zero-temperature version instead: CEM minimization of a trajectory energy.

### Cited Findings
- Control as inference: add binary optimality variables with p(O_t = 1 | s_t, a_t) = exp(r(s_t, a_t)) (their Eq. 3), and condition on them. This gives p(τ | O_{1:T}) ∝ p(s_1) Π_t p(s_{t+1} | s_t, a_t) exp(Σ_t r(s_t, a_t)). Quotes: "we condition on the optimality variables being true" — [Levine 2018, p.3](https://arxiv.org/pdf/1805.00909#page=3); "is equivalent to exact probabilistic inference in the case of deterministic dynamics" — [Levine 2018, p.1](https://arxiv.org/pdf/1805.00909#page=1)
- Diffuser models the whole state-action trajectory jointly and plans by sampling. Quotes: "sampling from the model and planning with it become nearly identical"; "classifier-guided sampling and image inpainting can be reinterpreted as coherent planning strategies" — [Janner et al. 2022, p.1](https://arxiv.org/pdf/2205.09991#page=1); "Diffuser predicts all timesteps of a plan simultaneously"; "An optional guide function J can be used to bias plans" — [Janner et al. 2022, p.2](https://arxiv.org/pdf/2205.09991#page=2)
- Guidance in Diffuser is sampling from a reweighted joint, p̃_θ(τ) ∝ p_θ(τ) h(τ). Joint modelling also gives up left-to-right generation. Quotes: "sampling from perturbed distributions of the form"; "we can no longer predict states autoregressively in temporal order" — [Janner et al. 2022, p.3](https://arxiv.org/pdf/2205.09991#page=3)
- Decision Diffuser replaces RL with conditional generation of trajectories (conditioning on return, constraints, or skills). Quote: "conditional generative modeling is a powerful tool for decision-making" — [Ajay et al. 2022, p.1](https://arxiv.org/pdf/2211.15657#page=1)
- Diffusion World Model (DWM) makes the one-step vs. joint contrast explicit for offline RL. Quote: "As opposed to traditional one-step dynamics models, DWM offers long-horizon predictions in a single forward pass" — [Ding et al. 2024, p.1](https://arxiv.org/pdf/2402.03570#page=1)
- Diffusion Forcing gets guidance while staying causal, and adds Monte Carlo Guidance for reward. Quote: "Monte Carlo Guidance (MCG), that dramatically improves the sampling of high-reward generations" — [Chen et al. 2024, p.2](https://arxiv.org/pdf/2407.01392#page=2)
- Navigation World Models (Bar, Zhou, Tran, Darrell, LeCun) is a diffusion world model that LeCun co-authored. It is used as a simulator inside an optimizer: an energy over (s_0, a_{0:T−1}, s_T) is minimized with CEM, or trajectories from another policy are ranked. Quotes: "plan navigation trajectories by simulating them and evaluating whether they achieve"; "by ranking trajectories sampled from an external policy" — [Bar et al. 2024, p.1](https://arxiv.org/pdf/2412.03572#page=1); "minimizing the energy corresponds to maximizing the unnormalized perceptual similarity score" — [NWM, p.4](https://arxiv.org/pdf/2412.03572#page=4); "we optimize it using the Cross-Entropy Method"; "a simple derivative-free and population-based optimization method" — [NWM, p.5](https://arxiv.org/pdf/2412.03572#page=5)
- Unified World Models (UWM) reads policy, forward dynamics, inverse dynamics and video prediction off one joint model by choosing per-modality noise levels. Quotes: "UWM can flexibly represent a policy, a forward dynamics, an inverse dynamics, and a video generator" — [Zhu et al. 2025, p.1](https://arxiv.org/pdf/2504.02792#page=1); "the model captures all combinations of marginal and conditional distributions" — [UWM, p.2](https://arxiv.org/pdf/2504.02792#page=2)

### Inferences
- **The equations line up (my framing).** With a trajectory diffusion prior p_σ(τ) and Levine's optimality likelihood, the guided score is ∇_τ log p̃_σ(τ) = ∇_τ log p_σ(τ) + ∇_τ log h_σ(τ), where h_σ is the noise-smoothed version of exp(Σ r). Diffuser's p̃ ∝ p·h is Levine's posterior over trajectories, with the diffusion model standing in for the dynamics-and-behaviour prior. Inpainting the start state s_0 and goal s_T is conditioning on exact observations.
- **Two ways to use the same object.** Sampling p̃ gives stochastic, mode-respecting plans (Diffuser, Decision Diffuser, Diffusion Forcing's MCG). Minimizing −log p̃ gives a single plan (V-JEPA 2-AC, NWM's CEM, DINO-WM's planner). Like Q1, these are the β = 1 and β → ∞ ends of one family.
- **The joint contains the one-step model, but not vice versa.** Denoise s_{t+1} with s_{≤t}, a_t clamped and you have P(s_{t+1} | s_t, a_t). Getting the joint from a one-step model needs rollouts, and errors compound (the motivation behind DWM). The catch for the joint model: the behaviour policy is baked in, so guidance is needed to move off it.

### Gaps
- I found no head-to-head primary comparison, on the same task and data, of guided diffusion sampling vs. CEM minimization of a JEPA latent energy.
- Diffusion Forcing's MCG mechanism is cited from the abstract only. I did not extract its exact algorithm.

---

## Q3. Does this suggest a generalized world model that does both physical (time-step) prediction and denoising prediction? (time axis × noise axis)

### Takeaway
Yes, and the literature already states it nearly verbatim. Diffusion Forcing gives every token its own noise level and describes the result as interleaving "the time axis of the sequence and the noise axis of diffusion": Bayesian filtering along time, diffusion along an "uncertainty" axis. One model trained this way contains next-step prediction, full-sequence (joint) diffusion, rolling and diagonal schedules, and (UWM, with separate per-modality noise) policy, forward dynamics and inverse dynamics. On the formal side, stochastic interpolants show that "denoise from Gaussian" and "evolve from the current state" are the same operation, transport between two distributions. The only difference is the base distribution: Gaussian noise vs. a point mass at s_t.

### Cited Findings

**Per-token noise: the two-axis model**
- Quote: "denoise a set of tokens with independent per-token noise levels" — [Chen et al. 2024 (Diffusion Forcing), p.1](https://arxiv.org/pdf/2407.01392#page=1)
- The cleanest primary statement of the time × noise framing. Quotes: "interleaves the time axis of the sequence and the noise axis of diffusion"; "Bayesian filtering along the time axis" [… and diffusion along an "uncertainty" (or noise level) axis] — [Diffusion Forcing, p.3](https://arxiv.org/pdf/2407.01392#page=3)
- Noise works as soft masking, which is why one network can serve as both predictor and denoiser. Quotes: "noising tokens is a form of partial masking" — [Diffusion Forcing, p.2](https://arxiv.org/pdf/2407.01392#page=2); "which we refer to as masking along the noise axis" — [Diffusion Forcing, p.4](https://arxiv.org/pdf/2407.01392#page=4)
- Rolling Diffusion maps one global denoising time to a per-frame local time, so later frames are noisier: a diagonal path through the (time × noise) grid. Quotes: "reparameterizing the global diffusion time to a local time for each frame"; "by assigning more noise to frames that appear later" — [Ruhe et al. 2024, p.1](https://arxiv.org/pdf/2402.09470#page=1); "The global diffusion denoising time t (vertical axis) is mapped to a local time" — [Rolling Diffusion, p.3](https://arxiv.org/pdf/2402.09470#page=3)
- FIFO-Diffusion uses the same diagonal schedule at inference time only. Quote: "consecutive frames with increasing noise levels in a queue" — [FIFO-Diffusion 2024, p.1](https://arxiv.org/pdf/2405.11473#page=1)
- History-Guided Video Diffusion (DFoT, the Diffusion Forcing Transformer) uses per-frame noise to condition on any subset of history, and on frequency bands of it, and to guide with it. Quotes: "we propose the Diffusion Forcing Transformer (DFoT)"; "variable lengths, subsets of frames, and even different image-domain frequencies" — [History-Guided Video Diffusion 2025, p.1](https://arxiv.org/pdf/2502.06764#page=1)
- UWM applies the same idea across modalities: each modality has its own diffusion timestep, and the noise-as-masking link selects which conditional is sampled. Quotes: "independent diffusion timesteps govern each modality"; "a connection between diffusion noise at different timesteps of the forward diffusion noising process and partial masking" — [Zhu et al. 2025, p.1](https://arxiv.org/pdf/2504.02792#page=1)

**Diffusion steps tied to physical time, and transport between arbitrary distributions**
- DYffusion makes the diffusion steps themselves physical-time steps. The forward process is temporal interpolation and the reverse is forecasting. Quotes: "directly coupling it with the diffusion steps in the model" — [Cachay et al. 2023, p.1](https://arxiv.org/pdf/2306.01984#page=1); "an interplay of temporal interpolation and forecasting" — [DYffusion, p.2](https://arxiv.org/pdf/2306.01984#page=2)
- Stochastic interpolants connect any two densities, not just noise and data, and recover the Schrödinger bridge when optimized. Quotes: "bridge any two probability density functions exactly in finite time"; "bridge between the two target densities when explicitly optimizing" — [Albergo, Boffi & Vanden-Eijnden 2023, p.1](https://arxiv.org/pdf/2303.08797#page=1)
- The base can be coupled to the target (dependent pairs). Quote: "a base density is produced via a coupling" — [Albergo et al. 2023 (data-dependent couplings), p.1](https://arxiv.org/pdf/2310.03725#page=1)
- Probabilistic forecasting as transport: the base is a point mass at the current state, and the stochastic interpolant / Föllmer process spreads it into the forecast distribution. Quote: "maps a point mass centered at the current state onto a probabilistic ensemble of forecasts" — [Chen, Goldstein, Hua, Albergo, Boffi & Vanden-Eijnden 2024, p.1](https://arxiv.org/pdf/2403.13724#page=1). They frame it as a Schrödinger bridge with a point-mass base. Quote: "in which the base distribution is a point mass measure" — [same, p.2](https://arxiv.org/pdf/2403.13724#page=2)

**Physics and weather forecasters built from denoising**
- GenCast runs a conditional diffusion model at each 12-hour physical step, trained with a standard denoising objective. Quotes: "generates an ensemble of stochastic 15-day global forecasts, at 12-hour steps" — [Price et al. 2023, p.1](https://arxiv.org/pdf/2312.15796#page=1); "GenCast is implemented as a conditional diffusion model" — [GenCast, p.3](https://arxiv.org/pdf/2312.15796#page=3); "using a standard diffusion model denoising objective" — [GenCast, p.4](https://arxiv.org/pdf/2312.15796#page=4)
- PDE-Refiner runs an inner denoising-style refinement loop inside each physical time step to recover neglected (often high-frequency) components. Quotes: "neglect of non-dominant spatial frequency information"; "the denoising objective implicitly induces a novel form of spectral data augmentation" — [Lippe et al. 2023, p.1](https://arxiv.org/pdf/2308.05732#page=1)
- GameNGen makes autoregressive diffusion stable by noising the context frames, a hand-set point on the time × noise grid. Quote: "stable auto-regressive setup via noise augmentation" — [Valevski et al. 2024, p.2](https://arxiv.org/pdf/2408.14837#page=2)
- Dreamer 4's world model is a flow-matching / diffusion model trained with "shortcut forcing". Quotes: "Our world model is based on the paradigm of diffusion models" — [Hafner, Yan & Lillicrap 2025, p.3](https://arxiv.org/pdf/2509.24527#page=3); "a shortcut forcing objective and an efficient transformer architecture" — [Dreamer 4, p.1](https://arxiv.org/pdf/2509.24527#page=1)
- 2026: Equilibrium Forcing keeps the Diffusion Forcing per-frame-noise lattice but drops noise conditioning, building on EqM's energy view. This merges Q1's energy picture with the time × noise world model. Quotes: "a simplified framework for video denoising generative models without noise level conditioning" — [Lillemark, Rojas et al. 2026, p.1](https://arxiv.org/pdf/2608.14706#page=1); "we operate within the Diffusion Forcing framework for video denoising"; "where each video frame has its own noise level" — [Equilibrium Forcing, p.2](https://arxiv.org/pdf/2608.14706#page=2)

### Inferences
- **The formal object (my framing, consistent with Diffusion Forcing and UWM).** Give each element of a sequence x_{1:T} (states, actions, observations) its own noise level σ_t ∈ [0, σ_max]. A generalized world model learns the score
  s_θ(x_{1:T}; σ_{1:T}) ≈ ∇ log p(x̃_{1:T}), with x̃_t = x_t + σ_t ε_t,
  for all noise vectors σ_{1:T}. Special cases on this lattice:
  - σ = (0, …, 0, σ, σ_max, …, σ_max): one-step / next-token world model P(x_{t+1} | x_{≤t}), sampled by lowering σ_{t+1} to 0 (teacher forcing, GameNGen, Genie 2, Dreamer 4).
  - σ_t ≡ σ: full-sequence joint diffusion (Diffuser, DWM, Sora-style clips).
  - σ_t increasing in t: rolling / FIFO / "far future is more uncertain".
  - Separate σ per modality: (σ_obs = max) gives the policy p(a | o); (σ_act = 0) gives forward dynamics p(o′ | o, a); clamping the future gives inverse dynamics p(a | o, o′) (UWM).
  - Guidance on the whole lattice: planning (Q2).
  "Physical prediction" moves along t. "Denoising prediction" moves along σ. Planning and inference move anywhere.
- **Both axes are transport (stochastic-interpolant statement).** Write x_τ = α(τ) x_0 + β(τ) x_1 + γ(τ) z with τ ∈ [0, 1].
  - Denoising: x_0 ~ N(0, I) independent of x_1 ~ data.
  - Physical forecasting (Chen et al. 2024): x_0 = s_t (point mass) and x_1 = s_{t+Δ} (coupled).
  The same velocity/score regression handles both. So physical time t chooses which pair of distributions to connect, and the auxiliary "noise time" τ parameterizes the path between them. DYffusion is the degenerate case where the two clocks are the same.
- **A structural asymmetry worth keeping.** Along the noise axis the forward (corrupting) process is known and simple, and only the reverse is learned. Along the physical-time axis the forward process is the unknown dynamics. That is why forecasting models still need a separate noise axis (GenCast, PDE-Refiner, Diffusion Forcing): the noise axis supplies the tractable "uncertainty dimension" along which the unknown time step is sampled.
- **Where JEPA fits.** A JEPA world model is the σ = 0 slice of this lattice, read in an abstract embedding space and evaluated at zero temperature. It predicts along t but never along σ. JEDI (Q5) and latent diffusion world models (Genie 2, Cosmos) add a σ axis inside a learned latent space.

### Gaps
- I found no theorem guaranteeing that the conditionals a single network learns across all noise vectors σ_{1:T} are mutually consistent, i.e. that they come from one joint. Diffusion Forcing claims a likelihood bound (ELBO) per noise vector. I did not extract or check that claim.
- No primary source found that formally proves "Bayesian filtering in time + diffusion in noise" is optimal under compounding error. Long-rollout drift is still handled by heuristics (noise augmentation, history guidance).
- Rolling Diffusion's authors are cited by first author only. I did not read the full author list.

---

## Q4. Diffusion models used as world models vs. JEPA world models (sampling pixels vs. optimizing latents)

### Takeaway
By 2024–2026, diffusion (or latent diffusion) is the default simulator in "generative" world models: DIAMOND, GameNGen, Genie 2, NWM, Cosmos, Sora, Dreamer 4. They draw the next observation by denoising, frame by frame. The JEPA camp (LeCun 2022, V-JEPA 2, DINO-WM) predicts in an abstract embedding and plans by minimizing an energy there, because pixel generation spends capacity on unpredictable details and costs compute. NWM (co-authored by LeCun) sits in between: a diffusion simulator inside a CEM energy-minimizing planner. The JEPA position is consistent with Q1: it prefers zero-temperature inference in a coarse space.

### Cited Findings
- DIAMOND: a diffusion world model chosen because discrete latents throw away detail. It can also run alone as a neural game engine. Quotes: "this compression into a compact discrete representation may ignore visual details"; "can stand alone as an interactive neural game engine" — [Alonso et al. 2024, p.1](https://arxiv.org/pdf/2405.12399#page=1)
- GameNGen. Quote: "the first game engine powered entirely by a neural model" — [Valevski et al. 2024, p.1](https://arxiv.org/pdf/2408.14837#page=1)
- Genie 2: an autoregressive latent diffusion model, sampled frame by frame, with classifier-free guidance for actions. Quotes: "Genie 2 is an autoregressive latent diffusion model, trained on a large video dataset"; "We use classifier-free guidance to improve action controllability" — [DeepMind 2024 (Genie 2)](https://deepmind.google/blog/genie-2-a-large-scale-foundation-world-model/#model-architecture)
- Genie 3: real-time, auto-regressive, frame by frame. DeepMind says this is harder than generating a whole video at once. Quotes: "navigate in real time at 24 frames per second" — [DeepMind 2025 (Genie 3)](https://deepmind.google/blog/genie-3-a-new-frontier-for-world-models/#text); "generating an environment auto-regressively is generally a harder technical problem than generating an entire video"; "the model has to take into account the previously generated trajectory that grows with time" — [Genie 3](https://deepmind.google/blog/genie-3-a-new-frontier-for-world-models/#text-5)
- NVIDIA Cosmos: "world foundation models" ship in both diffusion and autoregressive families, and the diffusion ones are latent diffusion. Quotes: "a digital twin of the world, the world model" — [NVIDIA 2025, p.1](https://arxiv.org/pdf/2501.03575#page=1); "For both diffusion-based and autoregressive-based WFMs" — [Cosmos, p.5](https://arxiv.org/pdf/2501.03575#page=5); "Our diffusion-based WFMs are latent diffusion models" — [Cosmos, p.19](https://arxiv.org/pdf/2501.03575#page=19)
- OpenAI Sora report: diffusion over video as a route to simulators. Quotes: "train text-conditional diffusion models jointly on videos and images"; "a promising path towards building general purpose simulators of the physical world" — [OpenAI 2024 (Wayback capture)](https://web.archive.org/web/20261013204239id_/https://openai.com/index/video-generation-models-as-world-simulators/#main)
- NWM uses a Conditional Diffusion Transformer as its simulator. Quote: "NWM employs a Conditional Diffusion Transformer (CDiT)" — [Bar et al. 2024, p.1](https://arxiv.org/pdf/2412.03572#page=1). It plans by CEM on an energy (see Q2).
- Why JEPA does not generate pixels (LeCun). Quotes: "eschewing the need to predict every detail of y" — [LeCun 2022, p.24](https://web.archive.org/web/20241231120746id_/https://openreview.net/pdf?id=BZ5a1r-kVsf#page=24); "enabling the elimination of irrelevant details by the encoders" — [LeCun 2022, p.25](https://web.archive.org/web/20241231120746id_/https://openreview.net/pdf?id=BZ5a1r-kVsf#page=25)
- Why not pixels (V-JEPA 2). Quote: "ignoring unpredictable details that generative objectives emphasize" — [V-JEPA 2, p.2](https://arxiv.org/pdf/2506.09985#page=2)
- Why not pixels (DINO-WM, co-authored by LeCun): it is compute cost, explicitly including diffusion. Quotes: "model visual dynamics without reconstructing the visual world" — [Zhou, Pan, LeCun & Pinto 2024, p.1](https://arxiv.org/pdf/2411.04983#page=1); "Predicting in pixel space, however, is computationally expensive"; "the overhead of using diffusion models" — [DINO-WM, p.2](https://arxiv.org/pdf/2411.04983#page=2)

### Inferences
- The disagreement is two separable choices: which space (pixels / VAE latents vs. learned abstract embeddings) and which inference (sample vs. minimize). Diffusion world models choose (pixels or VAE latents, sample). JEPA chooses (abstract embedding, minimize). NWM shows a third combination: (pixels or VAE latents, minimize via CEM). The fourth, (abstract embedding, sample), is what JEDI and latent-diffusion-in-JEPA-space aim at (Q5).
- JEPA's "unpredictable details" argument and diffusion's multimodality are two answers to the same problem: aleatoric detail that a single prediction cannot get right. JEPA removes the detail from the target (the encoder discards it). Diffusion keeps it and samples it. In Q3's terms, JEPA predicts at a coarse "scale" and diffusion predicts all the way to σ = 0.

### Gaps
- What I fetched of the Genie 3 page gives no architecture family (diffusion or otherwise), only "auto-regressive" and frame-by-frame generation. Treat Genie 3's architecture as undisclosed here.
- The Sora report is a short web report, not a peer-reviewed paper. Its world-simulator claims are qualitative.

---

## Q5. Scale axis: is denoising prediction along a scale axis (inverse renormalization), with world modelling as prediction along time?

### Takeaway
There is solid, though mostly analytic, support that the diffusion noise level works as a coarse-to-fine scale parameter:
- Wavelet score models factor generation across scales.
- Gaussian noise on power-law-spectrum images hides frequencies from the top down, which Dieleman calls "spectral autoregression".
- The reverse process shows a phase transition where high-level features are fixed first.
- Physicists read the renormalization group (RG, the coarse-graining flow) as a diffusion, with generation as inverse RG.

So "denoising = prediction along scale, world modelling = prediction along time, a general model does both" is well grounded. The caveat is that noise ≈ scale is a property of natural-image-like spectra under Gaussian noise, not a universal identity.

### Cited Findings
- Wavelet Score-based Generative Model (WSGM): generation as a product of conditionals across wavelet scales, p(x) = p(x_J) Π_j p(x̄_j | x_j), with x̄_j the detail coefficients at scale j given the coarser approximation x_j. Quotes: "factorizing the data distribution into a product of conditional probabilities of wavelet coefficients across scales"; "synthesizes wavelet coefficients with the same number of time steps at all scales" — [Guth, Coste, De Bortoli & Mallat 2022, p.1](https://arxiv.org/pdf/2208.05003#page=1)
- Diffusion as approximate frequency-domain autoregression (expert blog by a DeepMind researcher, a secondary source). Quote: "diffusion models of images perform approximate autoregression in the frequency domain" — [Dieleman 2024](https://sander.ai/2024/09/02/spectral-autoregression.html#main)
- A phase transition in the reverse process: high-level features (class) are decided abruptly, low-level details change smoothly. Quotes: "the probability of reconstructing high-level features"; "the reconstruction of low-level features, such as specific details of an image, evolves smoothly" — [Sclocchi et al. 2024, p.1](https://arxiv.org/pdf/2402.16991#page=1)
- RG ↔ diffusion. Quotes: "the renormalization group (RG) can be conceptualized as a diffusion process" — [Sheshmani et al. 2024, p.1](https://arxiv.org/pdf/2402.17090#page=1); "inverse RG can be viewed as a generative model" — [same, p.2](https://arxiv.org/pdf/2402.17090#page=2). Quality note: this paper is framed heavily in category theory, and its empirical content is modest.
- Speciation, as coarse structure decided first. Quote: "transition where the gross structure of data is unraveled" — [Biroli et al. 2024, p.1](https://arxiv.org/pdf/2402.18491#page=1)
- Frequency used as an explicit conditioning axis inside a world-model-style video model. Quote: "variable lengths, subsets of frames, and even different image-domain frequencies" — [History-Guided Video Diffusion, p.1](https://arxiv.org/pdf/2502.06764#page=1)
- PDE-Refiner's per-step refinement targets frequency bands a one-shot predictor neglects. Quote: "neglect of non-dominant spatial frequency information" — [Lippe et al. 2023, p.1](https://arxiv.org/pdf/2308.05732#page=1)
- LeCun proposes a hierarchy of predictors at several abstraction levels. Quote: "JEPA and Hierarchical JEPA" — [LeCun 2022, p.2](https://web.archive.org/web/20241231120746id_/https://openreview.net/pdf?id=BZ5a1r-kVsf#page=2)

### Inferences
- **The two-axis picture, refined (my framing).** For data with a decaying power spectrum S(f), adding noise of variance σ² roughly hides every frequency with S(f) < σ². So σ acts as a resolution cutoff, an RG scale. The Q3 lattice then becomes (physical time t) × (scale ℓ(σ)):
  - Denoising = predicting fine scales from coarse ones at fixed t (inverse RG).
  - World modelling = predicting the next t at some scale.
  - Diffusion Forcing–style models do both.
  - JEPA is a world model at one coarse scale chosen by the encoder.
  - H-JEPA is a world model at several discrete scales.
  - A diffusion world model covers the continuum of scales down to σ = 0.
- **Physics hint (inference, not sourced).** In chaotic and turbulent systems, fine scales decorrelate fastest in time. That suggests a natural world-model schedule where far-future frames keep only coarse scales (high σ) and near-future frames are fully resolved, which is exactly the rolling / diagonal schedule of Q3. GenCast and PDE-Refiner are domain examples where uncertainty and missing information concentrate at fine scales.
- **Caveat.** The σ ↔ scale identity relies on Gaussian noise plus power-law spectra in pixel or other linear bases. In learned latents, discrete data, or symbolic domains, "noise level" need not mean "spatial scale". There it is better read generically as "how much information has been masked" (Diffusion Forcing's partial-masking view).

### Gaps
- No primary source I found proves a general equivalence between learned diffusion noise levels and RG scale beyond Gaussian / linear-spectrum analyses. Dieleman's argument is a blog post, and Sheshmani et al. is conceptual.
- I did not verify a specific power-law statement on Dieleman's page (my grep found only general spectrum discussion), so the S(f) power-law premise above is standard image statistics, not a quote.

---

## Q6. 2025–2026 work explicitly unifying world models over (time × noise), or combining JEPA with diffusion

### Takeaway
No paper I found calls itself "world model = generative model over (time × noise level)" in those words. But the 2025–2026 "forcing" literature (Diffusion Forcing descendants) treats the per-frame noise schedule as the core design space of world models, and two lines push toward the JEPA + diffusion hybrid. One is JEDI: diffusion in a latent space learned JEPA-style, without reconstruction. The other is EqM → Equilibrium Forcing: an implicit-energy landscape sampled by gradient dynamics, used inside a per-frame-noise video model.

### Cited Findings
- UWM (RSS 2025): one diffusion transformer with independent per-modality timesteps, yielding policy, forward dynamics, inverse dynamics and video. Quote: "independent diffusion timesteps govern each modality" — [Zhu et al. 2025, p.1](https://arxiv.org/pdf/2504.02792#page=1)
- History-Guided Video Diffusion / DFoT (ICML 2025). Quote: "we propose the Diffusion Forcing Transformer (DFoT)" — [History-Guided Video Diffusion 2025, p.1](https://arxiv.org/pdf/2502.06764#page=1)
- Dreamer 4 (2025): an agent trained inside a diffusion / flow world model with shortcut forcing. Quote: "a shortcut forcing objective and an efficient transformer architecture" — [Hafner et al. 2025, p.1](https://arxiv.org/pdf/2509.24527#page=1)
- EqM (2025): implicit energy, gradient-descent sampling. Quote: "learns the equilibrium gradient of an implicit energy landscape" — [EqM, p.1](https://arxiv.org/pdf/2510.02300#page=1)
- Equilibrium Forcing (2026): Diffusion Forcing without noise conditioning. Quote: "we operate within the Diffusion Forcing framework for video denoising" — [Equilibrium Forcing, p.2](https://arxiv.org/pdf/2608.14706#page=2)
- JEDI (2026 revision of a 2025 paper): a latent diffusion world model whose latent space is learned from the denoising loss in a JEPA framework, with no reconstruction. Quotes: "the first online end-to-end latent diffusion world model"; "using denoising to learn and predict future latents rather than relying on reconstruction" — [JEDI 2026, p.1](https://arxiv.org/pdf/2605.13013#page=1)
- Next Forcing (2026): multi-chunk prediction for causal world models. Quote: "a multi-chunk prediction (MCP) framework for causal world modeling" — [Xu, Zhang & Shen 2026, p.1](https://arxiv.org/pdf/2606.11187#page=1)
- In-Context Forcing (Aug 2026): noisy context frames in autoregressive video diffusion, i.e. choosing the noise level of the past as well as the future. Quote: "explore the impact of noisy contexts on few-step autoregressive generation" — [In-Context Forcing 2026, p.1](https://arxiv.org/pdf/2608.05237#page=1)
- Latent-action world models (2026): energy-defined latents require MCMC/SGLD, which breaks down for continuous latents. Quote: "sampling approaches start to break down when handling continuous latents" — [2026, p.19](https://arxiv.org/pdf/2601.05230#page=19)
- Energy-structured latent world models (ELWM, 2026): the latent explicitly carries physical energy and momentum for physically consistent planning. This is a different sense of "energy" (physical, not EBM), and only marginally relevant. Quote: "structure the ELWM latent state to explicitly carry energy and momentum" — [ELWM 2026, p.1](https://arxiv.org/pdf/2608.09876#page=1)
- UNVERIFIED (search-result snippets only; not run through verify-source):
  - "Learning Navigable World Models via Latent Energy Shaping" (ICLR 2026): shaping the latent energy into a convex-like basin for gradient-based planning ([ICLR page](https://iclr.cc/virtual/2026/10018023)).
  - GeoWorld (CVPR 2026): hyperbolic JEPA with energy-based planning.
  - HDFlow: diffusion subgoals plus EBM guidance.
  - The 2025 v1 of JEDI (arXiv 2505.19698), under a different title.

### Inferences
- The field is converging on: one sequence model, per-element noise, read as predictor, denoiser, planner or inverse model depending on which noise levels are clamped. That is the "generalized world model" the user asked about. Diffusion Forcing (2024) is its founding statement, and UWM is its clearest multi-role demonstration.
- The JEPA side is moving toward the same object from the other direction:
  - JEDI puts diffusion inside a JEPA-style latent.
  - EqM and Equilibrium Forcing replace noise-conditioned scores with one energy landscape, which is LeCun's EBM language, sampled by gradient dynamics.
  - EBT and EBWM do energy-minimization "thinking".
  The remaining split is temperature (sample vs. minimize) and space (pixels vs. abstract latents), not the basic framework.
- A unified design implied by these papers (synthesis, not an existing system): a JEPA-like encoder defines the coarse latent. A per-frame-noise diffusion or equilibrium model over latent trajectories serves as simulator, sampler and denoiser. Planning is guidance (sampling) or CEM/gradient descent (minimizing) on the same energy, and a decoder or a σ → 0 pixel stage is optional.

### Gaps
- No primary 2025–2026 source found that explicitly proposes and tests "JEPA energy + annealed Langevin sampling" as a world-model sampler. JEDI is the closest, and it is a diffusion model in a JEPA-trained latent, not Langevin on a JEPA energy.
- Author lists for several 2026 preprints (In-Context Forcing, latent-action world models, ELWM, JEDI 2026) were not extracted. They are cited by title and arXiv id only.
- Search was not exhaustive for 2026: many "X-Forcing" variants exist (e.g. Mask Forcing 2609.09123, and AR Forcing 2605.31314 for navigation world models, both seen only in search results, UNVERIFIED). I verified only the ones listed above.
