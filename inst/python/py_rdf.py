# Enable python SPARQL queries

import rdflib
import pandas as pd
from os.path import exists


def read(path):
    """
    Read RDF/OWL files into Python for querying/manipulation.

    Keyword arguments:
    path -- the path to the RDF/OWL file
    """
    g = rdflib.Graph()
    g.parse(path, format = "application/rdf+xml")
    return g


def sparql_query(rdf_graph, query):
    """
    Query a loaded RDF graph with SPARQL.

    Keyword arguments:
    rdf_graph -- the RDF graph object
    query -- the SPARQL query, as a string or the path to a sparql file
    """
    if not isinstance(query, str):
        raise TypeError('query must be a string/path.')

    if exists(query):
        with open(query) as f:
            q = f.read()
    else:
        q = query

    res = rdf_graph.query(q)
    df = pd.DataFrame(res, columns = res.vars)
    # df = df_raw.convert_dtypes()
    return df
