
<script>
  $(".toggle").click(function() {
    $(this).toggleClass("open");
  });

function myFunction() {
  var x = document.getElementById("toggleDIV");
  if (x.style.display === "none") {
    x.style.display = "block";
  } else {
    x.style.display = "none";
  }
}

</script>
