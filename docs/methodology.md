# Methodology

This project models hypervigilance as a state-dependent decision problem. At each time step, an agent chooses between high vigilance and low vigilance while the environment may or may not contain a stressor.

## Basic Model

The Basic Model uses four state labels:

- `K`: vigilant, no stressor
- `Kd`: vigilant, stressor present
- `C`: relaxed, no stressor
- `CD`: relaxed, stressor present

The environment has one-step memory:

- If a stressor was absent, it appears with probability `LA`.
- If a stressor was present, it leaves with probability `LL`.

The dynamic programming solver compares the expected future cost of choosing high versus low vigilance at each time and state. Hypervigilance is counted during forward simulation when the policy chooses vigilance but the next realized environment contains no stressor.

## Health Model

The Health Model adds integer health to the dynamic programming state. Costs become health losses. If health reaches zero or below, the agent enters an absorbing `DEAD` state.

Terminal reward variants add value to retained final health:

- Linear reward
- Power reward
- Threshold reward

These variants test whether valuing future health changes the conditions under which vigilance and hypervigilance emerge.

## Interpretation

The main analyses compare hypervigilance across vigilance cost, stressor appearance probability, stressor leaving probability, environmental risk, autocorrelation, and health-model assumptions.
