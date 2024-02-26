// Define JavaScript function to reset radio group buttons
function resetRadioGroup(id) {
  var radioGroupId = id.replace('reset', 'radio');
  $("input[name='" + radioGroupId + "']").prop('checked', false);
}
