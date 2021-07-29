// cite: https://stackoverflow.com/a/49668515/9708266
// note: could not get this to
$('document').ready(function(){
  $('#threshold_slider').change(function(){
    console.log("HERE!!");
    var new_val = $(this).val();
    $('#threshold_numeric').val(new_val);
  });
});
