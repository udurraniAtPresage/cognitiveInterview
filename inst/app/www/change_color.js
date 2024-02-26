change_color = function (btn_id, color = "#113f35") {
  // find the closest parent with class 'accordion-item':
  let accordion_item = $(btn_id).closest(".accordion-item");

  // change its background color:
  //accordion_item.css("background-color", color);

  // find the sibling accordion-button div:
  let accordion_btn = accordion_item.find(".accordion-button");

  // change its background color:
  accordion_btn.css("background-color", color);
};
