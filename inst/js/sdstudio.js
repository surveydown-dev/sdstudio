$(document).ready(function() {
  
  /* ===== FLOATING BUTTON VISIBILITY MANAGEMENT ===== */
  // Function to update floating button groups based on active tab
  function updateFloatingButtonVisibility() {
    var activeTab = $('#tabset .nav-link.active').text().trim();
    
    if (activeTab === 'Preview') {
      // Show all groups on Preview tab
      $('#refresh-group').css('display', 'flex');
      $('#view-group').css('display', 'flex');
      $('#code-group').css('display', 'flex');
      $('#separator-1').css('display', 'block');
      $('#separator-2').css('display', 'block');
      
      // Show preview refresh button, hide responses refresh button
      $('#preview_refresh_btn').css('display', 'inline-block');
      $('#responses_refresh_btn').css('display', 'none');
      
    } else if (activeTab === 'Responses') {
      // Show refresh and code groups on Responses tab
      $('#refresh-group').css('display', 'flex');
      $('#view-group').css('display', 'none');
      $('#code-group').css('display', 'flex');
      $('#separator-1').css('display', 'block');
      $('#separator-2').css('display', 'none');
      
      // Show responses refresh button, hide preview refresh button
      $('#preview_refresh_btn').css('display', 'none');
      $('#responses_refresh_btn').css('display', 'inline-block');
      
    } else {
      // Show only code group on Build tab
      $('#refresh-group').css('display', 'none');
      $('#view-group').css('display', 'none');
      $('#code-group').css('display', 'flex');
      $('#separator-1').css('display', 'none');
      $('#separator-2').css('display', 'none');
    }
  }
  
  // Update visibility on tab change
  $(document).on('shown.bs.tab', '#tabset a[data-bs-toggle="tab"]', function(e) {
    updateFloatingButtonVisibility();
  });
  
  // Initial visibility update (with delay to ensure UI is loaded)
  setTimeout(updateFloatingButtonVisibility, 500);
  
  /* ===== SHINY MESSAGE HANDLERS ===== */
  // Ace editor undo/redo commands
  Shiny.addCustomMessageHandler('aceUndo', function(editorId) {
    var editor = ace.edit(editorId);
    editor.undo();
    editor.clearSelection();
    editor.navigateTo(0, 0);
    editor.scrollToLine(0, true, true, function() {});
    editor.focus();
  });

  Shiny.addCustomMessageHandler('aceRedo', function(editorId) {
    var editor = ace.edit(editorId);
    editor.redo();
    editor.clearSelection();
    editor.navigateTo(0, 0);
    editor.scrollToLine(0, true, true, function() {});
    editor.focus();
  });
  
  // Modal control handlers
  Shiny.addCustomMessageHandler('showModal', function(modalId) {
    $('#' + modalId).modal('show');
  });

  Shiny.addCustomMessageHandler('hideModal', function(modalId) {
    $('#' + modalId).modal('hide');
  });

  Shiny.addCustomMessageHandler('updateModalTitle', function(data) {
    $('#' + data.modalId).text(data.title);
  });

  // Path display update handler
  Shiny.addCustomMessageHandler('updatePathDisplay', function(data) {
    $('#path_display').text(data.display).attr('title', data.path);
  });

  // Path button update handler
  Shiny.addCustomMessageHandler('updatePathButton', function(data) {
    $('#path_display_btn').text(data.display + '/').attr('title', 'Click to edit: ' + data.path);
  });

  // Connection state indicator update handler
  Shiny.addCustomMessageHandler('updateConnectionIndicator', function(data) {
    $('#connection_icon').html(data.icon);
    $('#connection_text').text(data.text).attr('class', data.textClass);
  });

  // Code mode buttons update handler
  Shiny.addCustomMessageHandler('updateCodeModeButtons', function(data) {
    var localBtn = $('#code_local_btn');
    var liveBtn = $('#code_live_btn');
    
    // Only update if buttons exist
    if (localBtn.length && liveBtn.length) {
      if (data.mode === 'local') {
        localBtn.addClass('active');
        liveBtn.removeClass('active');
      } else {
        liveBtn.addClass('active');
        localBtn.removeClass('active');
      }
    }
  });

  // Mode UI update handler (hide/show database connection section)
  Shiny.addCustomMessageHandler('updateModeUI', function(data) {
    var dbConnectionCard = $('#database_connection_card');
    var tableSelectLabel = $('label[for="table_select"]');
    
    if (data.mode === 'local') {
      // Local mode: hide database connection, update table select label
      dbConnectionCard.hide();
      if (tableSelectLabel.length) {
        tableSelectLabel.text('Choose a CSV file to view:');
      }
    } else {
      // Live mode: show database connection, restore original label
      dbConnectionCard.show();
      if (tableSelectLabel.length) {
        tableSelectLabel.text('Choose a table to view:');
      }
    }
  });

  // Function to update button states based on current app.R content
  function updateCodeModeFromContent() {
    // Only run this if we're on the app.R tab and the editor exists
    if ($('#code_tabs .nav-link.active').text().trim() === 'app.R' && typeof ace !== 'undefined') {
      try {
        var editor = ace.edit('app_editor');
        if (editor) {
          var content = editor.getValue();
          var isLocal = /sd_db_connect\s*\(\s*ignore\s*=\s*TRUE/.test(content);
          var hasConnect = /sd_db_connect\s*\(/.test(content);
          
          var mode = 'live'; // default
          if (hasConnect && isLocal) {
            mode = 'local';
          } else if (hasConnect) {
            mode = 'live';
          }
          
          // Update buttons directly
          var localBtn = $('#code_local_btn');
          var liveBtn = $('#code_live_btn');
          
          if (localBtn.length && liveBtn.length) {
            if (mode === 'local') {
              localBtn.addClass('active');
              liveBtn.removeClass('active');
            } else {
              liveBtn.addClass('active');
              localBtn.removeClass('active');
            }
          }
        }
      } catch (e) {
        // Silently handle errors
      }
    }
  }

  // Preview view switching functionality
  function switchPreviewView(viewMode) {
    var container = $('#preview_container');
    var widescreenBtn = $('#preview_widescreen_btn');
    var mobileBtn = $('#preview_mobile_btn');
    
    if (viewMode === 'mobile') {
      // Switch to mobile view (375px width)
      container.css({
        'width': '375px',
        'max-width': '375px',
        'margin': '0 auto'
      });
      
      // Update button states
      widescreenBtn.removeClass('active');
      mobileBtn.addClass('active');
      
    } else {
      // Switch to widescreen view (full width)
      container.css({
        'width': '100%',
        'max-width': '100%',
        'margin': '0'
      });
      
      // Update button states
      mobileBtn.removeClass('active');
      widescreenBtn.addClass('active');
    }
  }
  
  // Code mode switching functionality
  function switchCodeMode(mode) {
    var localBtn = $('#code_local_btn');
    var liveBtn = $('#code_live_btn');
    
    // Update button highlights IMMEDIATELY
    if (mode === 'local') {
      localBtn.addClass('active');
      liveBtn.removeClass('active');
    } else {
      liveBtn.addClass('active');
      localBtn.removeClass('active');
    }
    
    // Send message to Shiny to update app.R content (this happens in background)
    Shiny.setInputValue('code_mode_switch', {
      mode: mode,
      timestamp: new Date().getTime()
    });
  }
  
  // Preview button click handlers
  $(document).on('click', '#preview_widescreen_btn', function() {
    switchPreviewView('widescreen');
  });
  
  $(document).on('click', '#preview_mobile_btn', function() {
    switchPreviewView('mobile');
  });
  
  // Code mode button click handlers
  $(document).on('click', '#code_local_btn', function() {
    switchCodeMode('local');
  });
  
  $(document).on('click', '#code_live_btn', function() {
    switchCodeMode('live');
  });

  // Auto-refresh preview handler
  Shiny.addCustomMessageHandler('triggerAutoRefresh', function(data) {
    setTimeout(function() {
      console.log('Auto-refreshing preview now!');
      
      // Remove loading overlay
      $('.rendering-overlay').remove();
      console.log('Loading overlay removed');
      
      // Directly trigger preview refresh via custom message
      Shiny.setInputValue('auto_refresh_trigger', {timestamp: new Date().getTime()});
      console.log('Direct refresh triggered');
    }, data.delay);
  });


  // Show rendering message handler
  Shiny.addCustomMessageHandler('showRenderingMessage', function(data) {
    // Handle single preview frame
    var previewContainer = $('#preview_container');
    
    // Make sure the container has relative positioning for the overlay
    previewContainer.css('position', 'relative');
    
    // Add loading overlay (remove if already exists)
    previewContainer.find('.rendering-overlay').remove();
    previewContainer.append(
      '<div class="rendering-overlay" style="position: absolute; top: 0; left: 0; width: 100%; height: 100%; display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center; background-color: rgba(248, 249, 250, 0.95); border: 1px solid #ddd; border-radius: 5px; z-index: 10;">' +
        '<div style="margin-bottom: 20px;">' +
          '<div class="spinner-border text-primary" role="status" style="width: 3rem; height: 3rem;">' +
            '<span class="visually-hidden">Loading...</span>' +
          '</div>' +
        '</div>' +
        '<h4 style="color: #495057; margin-bottom: 10px;">Updating Survey</h4>' +
        '<p style="color: #6c757d; font-size: 1.1em;">Please wait while the survey is being rendered...</p>' +
      '</div>'
    );
  });

  /* ===== MODAL MANAGEMENT ===== */
  // Function to reset modal content
  function resetModifyModal() {
    // Clear any cached form values
    $('#modify-content-modal .modal-body input, #modify-content-modal .modal-body textarea, #modify-content-modal .modal-body select').val('');
    
    // Reset modal title
    $('#modify-content-modal-title').text('Modify Content');
  }

  // Modal event handlers
  $('#modify-content-modal').on('hidden.bs.modal', function() {
    resetModifyModal();
  });

  $('#modify-content-modal').on('show.bs.modal', function() {
    resetModifyModal();
  });

  // Function to reset add page modal
  function resetAddPageModal() {
    // Clear the page ID input
    $('#add_page_id_input').val('');
    
    // Reset the position dropdown to the last option if it exists
    if ($('#add_page_below').length) {
      var $dropdown = $('#add_page_below');
      var lastOption = $dropdown.find('option:last').val();
      $dropdown.val(lastOption);
    }
  }

  // Add page modal event handlers
  $('#add-page-modal').on('hidden.bs.modal', function() {
    resetAddPageModal();
  });

  $('#add-page-modal').on('show.bs.modal', function() {
    resetAddPageModal();
  });

  // Function to reset add content modal
  function resetAddContentModal() {
    // Clear any cached form values
    $('#add-content-modal .modal-body input, #add-content-modal .modal-body textarea, #add-content-modal .modal-body select').val('');
    
    // Reset content type to default
    $('#add_content_type').val('text').trigger('change');
  }

  // Add content modal event handlers
  $('#add-content-modal').on('hidden.bs.modal', function() {
    resetAddContentModal();
  });

  $('#add-content-modal').on('show.bs.modal', function() {
    resetAddContentModal();
  });

  // Show/hide options input based on question type
  $(document).off('change', '#add_question_type, #modify_question_type').on('change', '#add_question_type, #modify_question_type', function() {
    updateOptionsVisibility();
  });

  // Function to update options visibility
  function updateOptionsVisibility() {
    // For add question modal
    var addQuestionType = $('#add_question_type').val();
    if (addQuestionType) {
      var choiceTypes = ['mc', 'mc_buttons', 'mc_multiple', 'mc_multiple_buttons', 'select', 'slider'];
      var $optionsDiv = $('#add_question_options_div');
      var $sliderNumericDiv = $('#add_slider_numeric_div');
      
      if (choiceTypes.includes(addQuestionType)) {
        $optionsDiv.show();
      } else {
        $optionsDiv.hide();
      }
      
      if (addQuestionType === 'slider_numeric') {
        $sliderNumericDiv.show();
      } else {
        $sliderNumericDiv.hide();
      }
    }
    
    // For modify question modal
    var modifyQuestionType = $('#modify_question_type').val();
    if (modifyQuestionType) {
      var choiceTypes = ['mc', 'mc_buttons', 'mc_multiple', 'mc_multiple_buttons', 'select', 'slider'];
      var $optionsInput = $('#modify_question_options');
      var $optionsContainer = $optionsInput.closest('div');
      var $sliderMinInput = $('#modify_slider_min');
      var $sliderMaxInput = $('#modify_slider_max');
      var $sliderRangeInput = $('#modify_slider_range');
      var $sliderContainer = $sliderMinInput.closest('div').parent();
      
      if (choiceTypes.includes(modifyQuestionType)) {
        $optionsContainer.show();
      } else {
        $optionsContainer.hide();
      }
      
      if (modifyQuestionType === 'slider_numeric') {
        $sliderContainer.show();
      } else {
        $sliderContainer.hide();
      }
    }
  }

  // Initialize options visibility on modal show and when DOM updates
  $('#add-content-modal, #modify-content-modal').on('shown.bs.modal', function() {
    // Small delay to ensure DOM is ready
    setTimeout(updateOptionsVisibility, 100);
  });

  // Also trigger when Shiny updates the form content
  $(document).on('shiny:value', function(event) {
    if (event.target.id === 'add_content_form' || event.target.id === 'modify_content_form') {
      setTimeout(updateOptionsVisibility, 100);
    }
  });

  /* ===== BUTTON EVENT HANDLERS ===== */
  // Modify button handlers
  $(document).off('click', '.modify-page-btn').on('click', '.modify-page-btn', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    var pageId = $(this).attr('data-page-id');
    Shiny.setInputValue('modify_page_btn', {
      pageId: pageId,
      timestamp: new Date().getTime()
    });
    return false;
  });

  $(document).off('click', '.modify-content-btn').on('click', '.modify-content-btn', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    var pageId = $(this).attr('data-page-id');
    var contentId = $(this).attr('data-content-id');
    var contentType = $(this).attr('data-content-type');
    
    Shiny.setInputValue('modify_content_btn', { 
      pageId: pageId, 
      contentId: contentId, 
      contentType: contentType,
      timestamp: new Date().getTime()
    });
    return false;
  });

  // Add content button handler
  $(document).off('click', '.add-text-btn').on('click', '.add-text-btn', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    var pageId = $(this).attr('data-page-id');
    Shiny.setInputValue('add_text_btn', {
      pageId: pageId,
      timestamp: new Date().getTime()
    });
    return false;
  });

  $(document).off('click', '.add-question-btn').on('click', '.add-question-btn', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    var pageId = $(this).attr('data-page-id');
    Shiny.setInputValue('add_question_btn', {
      pageId: pageId,
      timestamp: new Date().getTime()
    });
    return false;
  });

  // Add page button handler
  $(document).off('click', '#add_page_btn').on('click', '#add_page_btn', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    // This will trigger the Shiny input
    return true;
  });

  // Delete confirmation handlers
  $(document).off('click', '.delete-page-btn').on('click', '.delete-page-btn', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    var pageId = $(this).attr('data-page-id');
    if (confirm('Are you sure you want to delete page "' + pageId + '"? This action cannot be undone.')) {
      Shiny.setInputValue('delete_page_btn', pageId);
    }
    return false;
  });
  
  $(document).off('click', '.delete-content-btn').on('click', '.delete-content-btn', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    var pageId = $(this).attr('data-page-id');
    var contentId = $(this).attr('data-content-id');
    var contentType = $(this).closest('[data-content-type]').attr('data-content-type');
    
    if (confirm('Are you sure you want to delete this ' + contentType + '? This action cannot be undone.')) {
      Shiny.setInputValue('delete_content_btn', { 
        pageId: pageId, 
        contentId: contentId, 
        contentType: contentType 
      });
    }
    return false;
  });

  /* ===== UI INTERACTION HANDLERS ===== */
  // Page toggle functionality
  function initToggle() {
    $('.page-header').off('click').on('click', function(e) {
      // Don't toggle if clicking on drag handle or action buttons
      if (!$(e.target).hasClass('drag-handle') && 
          !$(e.target).closest('.drag-handle').length &&
          !$(e.target).hasClass('delete-page-btn') &&
          !$(e.target).closest('.delete-page-btn').length &&
          !$(e.target).closest('.page-actions').length) {
        
        var $questions = $(this).next('.questions-container');
        var pageId = $(this).closest('.page-wrapper').attr('data-page-id');
        var willBeExpanded = $questions.is(':hidden'); // Will be expanded after toggle
        
        $questions.slideToggle();
        
        // Toggle icon
        var $icon = $(this).find('.toggle-icon i');
        if ($icon.hasClass('fa-chevron-down')) {
          $icon.removeClass('fa-chevron-down').addClass('fa-chevron-right');
        } else {
          $icon.removeClass('fa-chevron-right').addClass('fa-chevron-down');
        }
        
        // Send toggle state to Shiny
        Shiny.setInputValue('page_toggled', {
          pageId: pageId,
          isExpanded: willBeExpanded
        });
      }
    });
  }

  /* ===== DRAG & DROP FUNCTIONALITY ===== */
  // Initialize drag and drop functionality
  function initSortable() {
    // Pages sortable
    if (document.getElementById('pages-container')) {
      new Sortable(document.getElementById('pages-container'), {
        animation: 150,
        handle: '.page-drag-handle',
        ghostClass: 'sortable-ghost',
        filter: '.delete-page-btn, .page-actions, .modify-page-btn',
        preventOnFilter: true,
        onEnd: function(evt) {
          // Gather the new page order
          var pageOrder = [];
          $('#pages-container > div.page-wrapper').each(function() {
            pageOrder.push($(this).attr('data-page-id'));
          });
          
          // Send page order to Shiny
          Shiny.setInputValue('page_drag_completed', {
            order: pageOrder,
            timestamp: new Date().getTime()
          });
        }
      });
    }
    
    // Content (questions and text) sortable
    $('.questions-container').each(function() {
      var pageId = $(this).attr('data-page-id');
      new Sortable(this, {
        group: 'content-items', // This enables cross-container dragging
        animation: 150,
        handle: '.drag-handle',
        ghostClass: 'sortable-ghost',
        draggable: '.question-item, .text-item',
        filter: '.delete-content-btn, .content-actions, .modify-content-btn, .add-content-btn',
        preventOnFilter: true,
        
        // Expand collapsed pages when dragging starts
        onStart: function(evt) {
          // Add a class to indicate we're dragging
          $('body').addClass('content-dragging');
          
          // Set up hover handlers for page headers during drag
          $('.page-header').on('dragenter.sortable dragover.sortable', function(e) {
            e.preventDefault();
            var $pageHeader = $(this);
            var $questionsContainer = $pageHeader.next('.questions-container');
            
            // If the page is collapsed, expand it immediately
            if ($questionsContainer.is(':hidden')) {
              $questionsContainer.show(); // Use show() instead of slideDown() for immediate effect
              var $icon = $pageHeader.find('.toggle-icon i');
              $icon.removeClass('fa-chevron-right').addClass('fa-chevron-down');
              
              // Add visual feedback
              $pageHeader.addClass('drag-hover');
            }
          });
          
          $('.page-header').on('dragleave.sortable', function(e) {
            $(this).removeClass('drag-hover');
          });
        },
        
        // Clean up when dragging ends
        onEnd: function(evt) {
          // Remove dragging class and clean up event handlers
          $('body').removeClass('content-dragging');
          $('.page-header').off('.sortable').removeClass('drag-hover');
          
          var fromPageId = $(evt.from).attr('data-page-id');
          var toPageId = $(evt.to).attr('data-page-id');
          
          // Create an array of content order for the target page
          var contentOrder = [];
          
          $(evt.to).children('.question-item, .text-item').each(function() {
            var $element = $(this);
            var type = $element.attr('data-content-type');
            var id;
            
            if (type === 'question') {
              id = $element.attr('data-question-id');
            } else if (type === 'text') {
              id = $element.attr('data-text-id');
            }
            
            if (id && type) {
              contentOrder.push({
                type: type,
                id: id
              });
            }
          });
          
          // Convert to serialized format
          var serializedOrder = [];
          for (var i = 0; i < contentOrder.length; i++) {
            serializedOrder.push(contentOrder[i].type);
            serializedOrder.push(contentOrder[i].id);
          }
          
          // Send event to Shiny with both source and target page info
          Shiny.setInputValue('content_drag_completed', {
            fromPageId: fromPageId,
            toPageId: toPageId,
            order: serializedOrder,
            isCrossPage: fromPageId !== toPageId,
            timestamp: new Date().getTime()
          }, {priority: 'event'});
        }
      });
    });
  }

  /* ===== DOM MANAGEMENT & INITIALIZATION ===== */
  // Initialize all functionality together
  function initializeAll() {
    initToggle();
    initSortable();
  }

  // Ensure functionality is reinitialized whenever the DOM changes
  $(document).on('shiny:idle', function(event) {
    initializeAll();
  });
  
  // Watch for changes to the structure output
  var observer = new MutationObserver(function(mutations) {
    initializeAll();
  });
  
  // Function to start observing when survey structure becomes available
  function startObservingSurveyStructure() {
    var target = document.getElementById('survey_structure');
    if (target) {
      observer.observe(target, { childList: true, subtree: true });
      return true;
    }
    return false;
  }
  
  // Try to start observing immediately, and if not successful, retry periodically
  if (!startObservingSurveyStructure()) {
    var retryInterval = setInterval(function() {
      if (startObservingSurveyStructure()) {
        clearInterval(retryInterval);
      }
    }, 500);
  }
  
  // Initialize everything on document ready
  initializeAll();
  
  // Monitor tab changes to update button states
  $(document).on('shown.bs.tab', '#code_tabs a[data-bs-toggle="tab"]', function(e) {
    if ($(e.target).text().trim() === 'app.R') {
      // Delay to ensure ACE editor is ready
      setTimeout(updateCodeModeFromContent, 300);
    }
  });
  
  // Monitor ACE editor changes for real-time updates
  $(document).on('shiny:idle', function() {
    // Set up ACE editor change listener if not already set
    if (typeof ace !== 'undefined' && $('#app_editor').length) {
      try {
        var editor = ace.edit('app_editor');
        if (editor && !editor._codeModeListerSet) {
          editor.on('change', function() {
            // Debounce the update
            clearTimeout(window.codeModeUpdateTimeout);
            window.codeModeUpdateTimeout = setTimeout(updateCodeModeFromContent, 500);
          });
          editor._codeModeListerSet = true;
        }
      } catch (e) {
        // Silently handle errors
      }
    }
    
    // Also update button states when idle
    updateCodeModeFromContent();
  });
  
  // Also initialize after a short delay to catch any late-loading content
  setTimeout(function() {
    initializeAll();
    updateCodeModeFromContent();
  }, 1000);
  
  // Update button states periodically to catch any missed changes
  setInterval(updateCodeModeFromContent, 2000);
});