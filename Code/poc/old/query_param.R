# That will ensure you are only retrieving observations annotated as Dead.


query_params <- glue(
  "annotation_term_id=17&",   # "Alive or Dead"
  "annotation_value_id=19&",  # "Dead"
  "taxon_id={taxon_id}&",     # 3 for birds, 40151 for mammals
  "verifiable=true&",
  "d1={start_date}&d2={end_date}&",
  "order=desc&order_by=created_at&",
  "per_page={per_page}"
)