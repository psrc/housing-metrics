import os
import urllib
import requests

def download_ofm_apr1_est(dataset, **inter_years):
    """
     This function will download either post-censal or intercensal April 1 data from OFM. 
     Intercensal data requires a starting and ending census year because of how the filename is constructed.
     File will download to the current working directory
    """
    main_path = 'https://www.ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1'
    if dataset == 'post':
        dls = main_path + '/ofm_april1_population_final.xlsx'
        filename = 'ofm_april1_population_final.xlsx'
    elif dataset == 'inter':
        if ('start' in inter_years) & ('end' in inter_years):
            dls = main_path + '/hseries/'+ 'ofm_april1_intercensal_estimates_' + inter_years['start'] + '-' + inter_years['end'] + '.xlsx'
            filename = 'ofm_april1_population_intercensal.xlsx'
        else:
            print("You're missing either start or end (or both) keyword arguments.\n")
            return
    else:
        print("Use either 'post' or 'inter' for dataset argument\n")
        return
    resp = requests.get(dls)
    output = open(filename, 'wb')
    output.write(resp.content)
    output.close()

#download_ofm_apr1_est('inter', start = '2000', end = '2010')
#download_ofm_apr1_est('post')






