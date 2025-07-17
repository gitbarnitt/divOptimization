# Get the original function
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")

# Set environment to current for patching
environment(patched_gjamPrediction) <- environment()

# Convert body to modifiable list of expressions
body_lines <- as.list(body(patched_gjamPrediction))

# Patch 1: Force REDUCT to FALSE
body_lines[[2]] <- quote(REDUCT <- FALSE)

# Patch 2: Remove the original `if (REDUCT)` block
body_lines[[4]] <- quote({})

# ✅ Patch 3: Ensure x and xnew are defined
ensure_x_block <- quote({
  if (!exists("x")) x <- output$xdata
  if (!exists("xnew")) xnew <- newdata$xdata
})
body_lines <- append(body_lines, list(ensure_x_block), after = 6)

# ✅ Patch 4: Convert x and xnew to numeric matrices
force_numeric_x <- quote({
  message("🔁 Converting x and xnew to numeric model matrices")
  x <- model.matrix(~ . - 1, data = x)
  xnew <- model.matrix(~ . - 1, data = xnew)
})
body_lines <- append(body_lines, list(force_numeric_x), after = 7)

# ✅ Patch 5: Define y from output (now comes after x is set)
inject_y <- quote({
  y <- output$y
})
body_lines <- append(body_lines, list(inject_y), after = 8)

# ✅ Patch 6: Guard for effortMat
safe_effort_block <- quote({
  message("🔧 Checking for effortMat and rowOrder...")
  effortMat <- if (!is.null(output$effortMat)) output$effortMat else NULL
  rowOrder <- if (!is.null(output$inputs$rowOrder)) output$inputs$rowOrder else NULL
  if (!is.null(effortMat) && !is.null(rowOrder)) {
    effort <- effortMat[rowOrder, , drop = FALSE]
  } else {
    message("⚠️ Missing effortMat or rowOrder — using default effort = matrix(1)")
    effort <- matrix(1, nrow = nrow(y), ncol = ncol(y))
  }
})
body_lines <- append(body_lines, list(safe_effort_block), after = 9)

# ✅ Patch 7: Guard for holdoutIndex
safe_holdout_block <- quote({
  if (!is.null(output$inputs$holdoutIndex)) {
    holdoutIndex <- which(output$inputs$holdoutIndex == 1)
  } else {
    holdoutIndex <- integer(0)
  }
})
body_lines <- append(body_lines, list(safe_holdout_block), after = 10)

# ✅ Patch 8: Check t(x) %*% x solve()
linmod_diag <- quote({
  message("🔍 Diagnostic: attempting t(x) %*% x and solve()")
  tryCatch({
    xtx <- t(as.matrix(x)) %*% as.matrix(x)
    tmp <- solve(xtx)
    message("✅ Matrix solve successful")
  }, error = function(e) {
    message("❌ Matrix solve failed: ", e$message)
  })
})
body_lines <- append(body_lines, list(linmod_diag), after = 11)

# ✅ Patch 9: Post-effort diagnostics
next_diag <- quote({
  message("🔍 Post-effort diagnostics...")
  message("✔️ output$type: ", output$type)
  message("✔️ nrow(x): ", nrow(x))
  message("✔️ ncol(x): ", ncol(x))
  message("✔️ nrow(xnew): ", nrow(xnew))
  message("✔️ nrow(y): ", nrow(y))
  message("✔️ effort dim: ", paste(dim(effort), collapse = " x "))
  message("✔️ holdoutIndex length: ", length(holdoutIndex))
})
body_lines <- append(body_lines, list(next_diag), after = 12)

# ✅ Rebuild the patched function
body(patched_gjamPrediction) <- as.call(c(quote(`{`), body_lines))



manual_fit$y <- manual_fit$inputs$y

manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL,
    PLOT = FALSE,
    ylim = NULL,
    FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})
