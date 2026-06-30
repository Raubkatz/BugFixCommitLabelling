# Commit Prompt Miner

Standalone tool that walks a git repository and, for every commit, produces:

- `<repo>-bugfixes.csv` — one row per commit (`hash`, `message`, `isBugfix_stemming`), where
  `isBugfix_stemming` is a keyword/stemming bugfix verdict derived from the commit message.
- `<repo>-commit-prompts.json` — one ready-to-run LLM prompt per commit (commit message + a
  per-file diff summary), for classifying commits as bug fixes later, against any model.

It makes **no** network/LLM calls; it only reads the repo and writes these two files.

## Build

Requires a JDK 23 toolchain (Gradle provisions it if missing).

```bash
./gradlew shadowJar
```

This produces a self-contained jar at `build/libs/shadow-*.jar`.

## Run

```bash
java -jar build/libs/shadow-*.jar -r <repo> -o <out-dir> [options]
```

Or directly via Gradle:

```bash
./gradlew run --args="-r <repo> -o <out-dir>"
```

## Parameters

| Flag                    | Default      | Description                                                              |
| ----------------------- | ------------ | ----------------------------------------------------------------------- |
| `--repository`, `-r`    | **required** | Git repository directory (the working tree, or one containing `.git`).  |
| `--out-dir`, `-o`       | **required** | Output directory for the CSV and prompts JSON (created if missing).     |
| `--top-files`, `-n`     | `10`         | Max changed files included per commit prompt (largest changes first).   |
| `--max-diff-lines`, `-l`| `15`         | Max diff lines per file in the prompt. `0` = only `+/-` counts, no diff.|
| `--with-reason`         | off          | Generate prompts that also ask the model for a short reason.            |
| `--hashes`              | none         | Path to a `.txt` file with one commit hash per line; mine only those.   |
