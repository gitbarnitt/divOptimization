

### line 15

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

# ✅ Patch 5: Define y from output
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

# ✅ Patch 8: Check matrix solve
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
diagnostic_block <- quote({
  message("🔍 Post-effort diagnostics...")
  message("✔️ output$type: ", output$type)
  message("✔️ nrow(x): ", nrow(x))
  message("✔️ ncol(x): ", ncol(x))
  message("✔️ nrow(xnew): ", nrow(xnew))
  message("✔️ nrow(y): ", nrow(y))
  message("✔️ effort dim: ", paste(dim(effort), collapse = " x "))
  message("✔️ holdoutIndex length: ", length(holdoutIndex))
})
body_lines <- append(body_lines, list(diagnostic_block), after = 12)

# ✅ Patch 10: Guard against output$type == 'traits'
guard_traits_block <- quote({
  message("📍 REACHED LINE 14")
  message("🔍 Evaluating output$type condition...")
  if (!is.null(output$type) && output$type == 'traits') {
    stop("Trait models not currently supported in patched prediction.")
  } else {
    message("✔️ Skipping traits block")
  }
})
body_lines <- append(body_lines, list(guard_traits_block), after = 13)

# ✅ Patch 11: Next line checkpoint
checkpoint_line15 <- quote({
  message("📍 REACHED LINE 15")
})
body_lines <- append(body_lines, list(checkpoint_line15), after = 14)

line16_trace <- quote({
  message("📍 REACHED LINE 16")
  message("🔍 Inspecting key objects before linear predictor calculations...")
  
  if (!is.null(output$modelList)) {
    ml <- output$modelList
    message("✔️ modelList found")
    message("🔸 typeNames length: ", length(ml$typeNames))
    message("🔸 ng: ", ml$ng)
    message("🔸 reDraw: ", ml$reDraw)
    message("🔸 sigmaSave dims: ", paste(dim(ml$sigmaSave), collapse = " x "))
    message("🔸 betaBeta dims: ", paste(dim(ml$betaBeta), collapse = " x "))
    message("🔸 fit$chains length: ", length(output$chains))
  } else {
    stop("❌ output$modelList is NULL — cannot proceed.")
  }
})

body_lines <- append(body_lines, list(line16_trace), after = 15)

# ✅ Patch 12: Rebuild modelList if incomplete
reconstruct_modelList_block <- quote({
  message("🧩 Reconstructing missing modelList components from chains")
  
  chains <- output$chains
  modelList <- output$modelList
  
  if (!is.null(chains$bgibbs)) {
    beta_raw <- chains$bgibbs
    message("✔️ betaBeta raw dims: ", paste(dim(beta_raw), collapse = " x "))
    
    P <- ncol(x)
    S <- ncol(y)
    ng <- nrow(beta_raw)
    
    betaBeta <- array(NA, dim = c(ng, P, S))
    for (i in 1:ng) {
      betaBeta[i,,] <- matrix(beta_raw[i,], nrow = P, ncol = S)
    }
    message("✔️ betaBeta reshaped to: ", paste(dim(betaBeta), collapse = " x "))
  } else {
    stop("❌ Missing chains$bgibbs")
  }
  
  if (!is.null(chains$sgibbs)) {
    sigma_raw <- chains$sgibbs
    message("✔️ sigmaSave raw dims: ", paste(dim(sigma_raw), collapse = " x "))
    
    S <- ncol(y)
    ng <- nrow(sigma_raw)
    
    if (is.null(dim(sigma_raw)) || length(dim(sigma_raw)) != 3) {
      # Expect a flat matrix: [ng x S^2]
      if (ncol(sigma_raw) != S * S) {
        stop(paste("❌ sigma_raw has", ncol(sigma_raw), "columns but expected", S * S, "(S × S)"))
      }
      sigmaSave <- array(NA, dim = c(ng, S, S))
      for (i in 1:ng) {
        sigmaSave[i,,] <- matrix(sigma_raw[i, ], nrow = S, ncol = S)
      }
      message("✔️ sigmaSave reshaped to: ", paste(dim(sigmaSave), collapse = " x "))
    } else {
      # Check if it's already the correct shape
      if (!all(dim(sigma_raw)[2:3] == S)) {
        stop(paste("❌ sigma_raw is 3D but inner dimensions are not S x S — found:",
                   paste(dim(sigma_raw)[2:3], collapse = " x "), "expected:", S, "x", S))
      }
      sigmaSave <- sigma_raw
      message("✔️ sigmaSave already has correct 3D shape")
    }
    
  } else {
    stop("❌ Missing chains$sgibbs")
  }
  
  
  modelList$betaBeta <- betaBeta
  modelList$sigmaSave <- sigmaSave
  modelList$ng <- ng
  modelList$reDraw <- ng
  output$modelList <- modelList
})
body_lines <- append(body_lines, list(reconstruct_modelList_block), after = 16)

post_reconstruct_check <- quote({
  message("📍 REACHED LINE 17")
  message("🔎 Confirming modelList integrity after reconstruction...")
  message("🔸 modelList$reDraw: ", output$modelList$reDraw)
  message("🔸 modelList$ng: ", output$modelList$ng)
  message("🔸 betaBeta dims: ", paste(dim(output$modelList$betaBeta), collapse = " x "))
})

body_lines <- append(body_lines, list(post_reconstruct_check), after = 17)

set_model_type <- quote({
  if (is.null(output$type)) {
    output$type <- "CC"  # Continuous Cover — your actual response model
    message("🛠️ Manually set output$type to: ", output$type)
  }
})

body_lines <- append(body_lines, list(set_model_type), after = 17)

posterior_loop_wrap <- quote({
  message("📍 REACHED LINE 18")
  message("🌀 Entering posterior predictive loop...")
  
  tryCatch({
    for (k in 1:output$modelList$reDraw) {
      if (k == 1) message("🔁 Starting first iteration of posterior loop...")
      
      beta_k <- output$modelList$betaBeta[k,,]
      dim(beta_k) <- c(ncol(xnew), length(output$modelList$typeNames))
      message("✔️ Forced beta_k dims: ", paste(dim(beta_k), collapse = " x "))
      
      xb <- xnew %*% beta_k
      message("✔️ xnew %*% beta_k successful, result dim: ", paste(dim(xb), collapse = " x "))
      
      break
    }
    
    message("✅ Posterior loop structure intact")
    
  }, error = function(e) {
    message("❌ Posterior loop failed: ", e$message)
  })
})

body_lines <- append(body_lines, list(posterior_loop_wrap), after = 18)

post_xb_block <- quote({
  message("📍 REACHED LINE 19")
  message("🔍 Checking type-specific post-processing...")
  
  type <- output$type
  message("✔️ model type: ", type)
  
  if (is.null(type)) {
    stop("❌ output$type is NULL — model type must be specified.")
  }
  
  typeNames <- output$modelList$typeNames
  if (is.null(typeNames)) {
    stop("❌ modelList$typeNames is NULL — required for post-processing.")
  }
  
  message("✔️ typeNames length: ", length(typeNames))
})

body_lines <- append(body_lines, list(post_xb_block), after = 19)

# Rebuild the patched function
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
