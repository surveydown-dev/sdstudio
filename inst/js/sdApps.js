$(document).ready(function() {
  
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
      if (choiceTypes.includes(addQuestionType)) {
        $optionsDiv.show();
      } else {
        $optionsDiv.hide();
      }
    }
    
    // For modify question modal
    var modifyQuestionType = $('#modify_question_type').val();
    if (modifyQuestionType) {
      var choiceTypes = ['mc', 'mc_buttons', 'mc_multiple', 'mc_multiple_buttons', 'select', 'slider'];
      var $optionsInput = $('#modify_question_options');
      var $optionsContainer = $optionsInput.closest('div');
      if (choiceTypes.includes(modifyQuestionType)) {
        $optionsContainer.show();
      } else {
        $optionsContainer.hide();
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
  
  // Start observing changes to the survey structure
  var target = document.getElementById('survey_structure');
  if (target) {
    observer.observe(target, { childList: true, subtree: true });
  }
  
  // Initialize everything on document ready
  initializeAll();
});