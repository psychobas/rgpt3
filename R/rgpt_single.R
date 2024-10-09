rgpt_single <- function(prompt_role = "user", prompt_content, seed = NULL, 
                        model = "gpt-4o", output_type = "complete", max_tokens = 100, 
                        temperature = 1, top_p = 1, n = 1, stop = NULL, presence_penalty = 0, 
                        frequency_penalty = 0, logprobs = T) {
    # Define supported models
    models = c("gpt-3.5-turbo-0125", "gpt-3.5-turbo", "gpt-3.5-turbo-1106", 
               "gpt-3.5-turbo-16k", "gpt-3.5-turbo-0613", "gpt-3.5-turbo-16k-0613", 
               "gpt-4", "gpt-4-0125-preview", "gpt-4-turbo-preview", 
               "gpt-4-turbo-2024-04-09", "gpt-4-turbo", "gpt-4o", "gpt-4o-mini")
    
    # Check if model is supported
    if (!model %in% models) {
        message(paste0("The `model` is not on the list of supported models or contains a typo. For current models, have a look at: https://platform.openai.com/docs/models/gpt-4o"))
    }
    
    # If deterministic model and n > 1, set n to 1
    if (temperature == 0 & n > 1) {
        n = 1
        message("You are running the deterministic model, so `n` was set to 1 to avoid unnecessary token quota usage.")
    }
    
    # Set seed if provided
    if (is.numeric(seed)) {
        seed_info = seed
    } else {
        seed_info = NA
    }
    
    # Prepare message data frame
    messages = data.frame(role = prompt_role, content = prompt_content)
    parameter_list = list(messages = messages, model = model, 
                          seed = seed, max_tokens = max_tokens, temperature = temperature, 
                          top_p = top_p, n = n, stop = stop, presence_penalty = presence_penalty, 
                          frequency_penalty = frequency_penalty, logprobs = logprobs)
    
    # Define the retry mechanism
    max_retries <- 3
    for (retry in 1:max_retries) {
        # Send POST request to the chat completions API
        request_base <- httr::POST(url = url.chat_completions, body = parameter_list, 
                                   httr::add_headers(Authorization = paste("Bearer", api_key)), 
                                   encode = "json")
        # If request is successful, break out of the retry loop
        if (request_base$status_code == 200) {
            break
        } else {
            # Log the error message and attempt retry
            warning(paste("Request attempt", retry, "failed. Status code:", request_base$status_code))
            
            # If max retries reached, stop and show error
            if (retry == max_retries) {
                stop(paste("Request failed after", max_retries, "attempts. Status code:", request_base$status_code))
            }
            # Wait for 2 seconds before retrying
            Sys.sleep(2)
        }
    }
    
    # Extract content from the successful request
    request_content <- httr::content(request_base)
    
    # Check if response contains expected fields before proceeding
    if (is.null(request_content$choices) || length(request_content$choices) == 0) {
        stop("The API response did not contain the expected 'choices' field or it was empty. Please check the API response.")
    }
    
    # Handle single request scenario
    if (n == 1) {
        core_output <- data.table::data.table(n = 1, prompt_role = prompt_role, 
                                              prompt_content = prompt_content, gpt_role = request_content$choices[[1]]$message$role, 
                                              gpt_content = request_content$choices[[1]]$message$content)
        if (logprobs == TRUE) {
            data_logprobs <- request_content$choices[[1]]$logprobs[[1]]
            logprobs_output <- data.table::data.table(token = rep("", 
                                                                  length(data_logprobs)), logprob = rep(0, length(data_logprobs)))
            for (i in 1:length(data_logprobs)) {
                logprobs_output$token[i] <- data_logprobs[[i]]$token
                logprobs_output$logprob[i] <- data_logprobs[[i]]$logprob
            }
        }
    }
    # Handle multiple requests scenario
    else if (n > 1) {
        core_output <- data.table::data.table(n = 1:n, prompt_role = rep(prompt_role, 
                                                                         n), prompt_content = rep(prompt_content, n), gpt_role = rep("", 
                                                                                                                                      n), gpt_content = rep("", n))
        logprobs_output_list <- list()
        for (i in 1:n) {
            core_output$gpt_role[i] <- request_content$choices[[i]]$message$role
            core_output$gpt_content[i] <- request_content$choices[[i]]$message$content
            if (logprobs == TRUE) {
                data_logprobs <- request_content$choices[[i]]$logprobs[[1]]
                logprobs_output <- data.table::data.table(n = i, 
                                                          token = rep("", length(data_logprobs)), logprob = rep(0, 
                                                                                                                length(data_logprobs)))
                for (j in 1:length(data_logprobs)) {
                    logprobs_output$token[j] <- data_logprobs[[j]]$token
                    logprobs_output$logprob[j] <- data_logprobs[[j]]$logprob
                }
                logprobs_output_list[[i]] <- logprobs_output
            }
        }
        if (logprobs == TRUE) {
            logprobs_output <- data.table::rbindlist(logprobs_output_list)
        } else {
            logprobs_output <- "no logprobs requested"
        }
    }
    
    # Prepare metadata
    meta_output <- data.table::data.table(request_id = request_content$id, 
                                          object = request_content$object, model = request_content$model, 
                                          param_prompt_role = prompt_role, param_prompt_content = prompt_content, 
                                          param_seed = seed_info, param_model = model, param_max_tokens = max_tokens, 
                                          param_temperature = temperature, param_top_p = top_p, 
                                          param_n = n, param_stop = stop, param_logprobs = logprobs, 
                                          param_presence_penalty = presence_penalty, param_frequency_penalty = frequency_penalty, 
                                          tok_usage_prompt = request_content$usage$prompt_tokens, 
                                          tok_usage_completion = request_content$usage$completion_tokens, 
                                          tok_usage_total = request_content$usage$total_tokens, 
                                          system_fingerprint = request_content$system_fingerprint)
    
    # Return the desired output type
    if (output_type == "complete") {
        output <- list(core_output, meta_output, logprobs_output)
    } else if (output_type == "meta") {
        output <- meta_output
    } else if (output_type == "text") {
        output <- core_output
    } else if (output_type == "logprobs") {
        output <- logprobs_output
    }
    
    return(output)
}
