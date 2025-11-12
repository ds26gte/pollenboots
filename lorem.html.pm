#lang pollen

◊h1{Lorem Ipsum}

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec
eget mi arcu. Nunc sagittis in eros ac venenatis. Donec placerat
sodales lobortis. Fusce viverra ipsum vitae est tincidunt,
a blandit magna ornare. Maecenas fermentum egestas purus nec
hendrerit. Curabitur augue odio, malesuada ac egestas ut,
cursus et mauris. Phasellus porttitor, tellus nec rhoncus
tempor, lectus urna placerat nibh, sed condimentum magna arcu
eget felis.

This is ◊strong{strong masquerading as emphasized}.

This is ◊strong-og{the original strong}.

Use "smart quotes".

Use dashes --- smart dashes.

◊h2{Use Pollen markup lang directly}

◊div[#:class "red" #:style "font-size:150%"]{Important ◊em{News}}

◊h2{Use Bootstrap classes implicitly (via pollen.rkt), and explicitly}

◊ul{
  ◊li[#:class "active"]{One, active}
  ◊li{Two}
  ◊li{Three}
  }

◊h2{Use defined tag}

◊new-em{This is emphasized.}

◊new-em[#:style "color: red"]{This is emphasized.}


◊h2{Bootstrap classes directly}

◊div[#:class "text-center"]{Center aligned text}

◊div[#:class "w-25 p-3" #:style "background-color: green"]{Width 25%}
