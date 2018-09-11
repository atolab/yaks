class Path(object):
    def __init__(self, path):
        self.validate_selector_path(path)
        if not path.startswith('//'):
            raise ValueError('Path should start with //')
        self.path = path

    @staticmethod
    def validate_selector_path(p):
        return True

    def get_query(self):
        q = self.path.split('?')[-1]
        return self.args2dict(q)

    def dot2dict(self, dot_notation, value=None):
        ld = []
        tokens = dot_notation.split('.')
        n_tokens = len(tokens)
        for i in range(n_tokens, 0, -1):
            if i == n_tokens and value is not None:
                ld.append({tokens[i - 1]: value})
            else:
                ld.append({tokens[i - 1]: ld[-1]})
        return ld[-1]

    def args2dict(self, values):
        data = {}
        uri_values = values.split('&')
        for tokens in uri_values:
            v = tokens.split('=')[-1]
            k = tokens.split('=')[0]
            if len(k.split('.')) < 2:
                data.update({k: v})
            else:
                d = self.dot2dict(k, v)
                data.update(d)
        return data

    def is_prefix(self, prefix):
        return self.path.startswith(prefix)
