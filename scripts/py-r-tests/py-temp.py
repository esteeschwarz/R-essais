import csv
import tempfile

text = "testtexttowrite"

# def output_to_file(newfilepath):
#         with open(newfilepath, 'w') as outfile:
#             outfile.write(self.tree_to_write)
#             self.outputname = newfilepath

#def write_csv(csvfile):
    
    # writer = csv.DictWriter(r.csvfile, fieldnames=['foo', 'bar'])
    # writer.writeheader()
    # writer.writerow({'foo': 1, 'bar': 2})

def test_write_csv():
    with tempfile.NamedTemporaryFile(mode='w', delete=False) as csvfile:
    #    write_csv(csvfile)
         csvfile.write(text)
