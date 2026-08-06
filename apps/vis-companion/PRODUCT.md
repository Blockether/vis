# Product

<!-- impeccable:product-schema 1 -->

## Platform

web

## Users

Vis Companion serves developers operating Vis sessions across one or more machines. They use it from phones, tablets, and desktop browsers, including the iOS and Android Capacitor distributions, to continue work away from the terminal and coordinate sessions across a fleet of paired gateways.

## Product Purpose

Vis Companion is the responsive graphical client for Vis. It lets a developer pair with existing gateways, create and resume persistent sessions, follow streaming turns, inspect artifacts, answer human-input requests, and manage the settings and capabilities needed to keep that work moving.

Success means the user can understand which machine, project, and session owns the work; act on the right gateway without ambiguity; and continue safely when part of the fleet is slow, unavailable, or offline.

## Positioning

The Companion talks directly to paired Vis gateways instead of introducing a separate application backend. The gateway remains authoritative for sessions, projects, capabilities, and runtime state, while the client presents one coherent fleet interface across machines.

## Operating Context

- A machine owns its projects, and a project owns its sessions. The same repository on two machines is two projects and must never be merged by folder name.
- Users move between phone, tablet, and desktop widths and between touch and fine-pointer input. Width determines layout; pointer capability determines control density.
- A fleet may be partially degraded. One unreachable machine remains a degraded section in the fleet view, while a view scoped to that machine treats the failure as the screen state.
- Session and search operations are scoped to the selected gateway set. Creation is always anchored to a concrete reachable machine and project.
- The product ships as a web app and through Capacitor wrappers for iOS and Android, while sharing the same responsive interface and product model.

## Capabilities and Constraints

- Pair with one or more Vis gateways and manage the resulting machine fleet.
- Browse machine → project → session ownership, create sessions, resume persistent transcripts, and stream turns.
- Render artifacts and human-input dialogs; support voice, notifications, settings, and themes.
- Preserve gateway authority and privacy. Do not create a second backend or infer cross-machine identity from matching folder names.
- Treat offline and partial-failure states as first-class product states rather than collapsing the whole fleet into an error page.
- Keep project-relative behavior scoped to the active session workspace, including nested monorepo apps and their inherited project configuration.

## Brand Commitments

The product name is **Vis Companion**. Product language should be direct, operational, and explicit about the machine or gateway an action affects. Do not invent deployment, customer, performance, or security claims.

## Evidence on Hand

The application, fixtures, and tests under `apps/vis-companion/` are the source of truth for current behavior. The repository contains no approved testimonials, customer logos, usage benchmarks, or marketing proof; future work must not fabricate them.

## Product Principles

1. **Make ownership unmistakable.** Every session action must preserve and communicate its machine and project context.
2. **Degrade locally, not globally.** A failing gateway should impair only the scope it owns whenever the rest of the fleet remains usable.
3. **Use the right density for the input device.** Touch remains comfortably tappable at every width; compact rhythm is earned by a fine pointer, not a wide screen.
4. **Keep the gateway authoritative.** The client reflects and operates gateway state instead of creating competing identity, persistence, or capability models.
5. **Make fleet-wide work provable.** Scope, counts, search coverage, and creation targets should visibly show which machines participated.

## Accessibility & Inclusion

The same workflows must remain usable with touch, mouse or trackpad, keyboard, and assistive technology. Preserve semantic landmarks and accessible names, avoid hidden machine context in repeated controls, respect safe areas and virtual keyboards, and prevent horizontal overflow across supported phone, tablet, and desktop layouts.
