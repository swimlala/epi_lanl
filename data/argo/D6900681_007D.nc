CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      history       X2009-03-26T08:46:58Z creation; 2015-10-19T16:06:18Z last update (coriolis COFC software)   comment_dmqc_operator         DPRIMARY | https://orcid.org/0000-0002-3512-2070 | Saout-Grit, Glazeo      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    8   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    8   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8$   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  8,   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  8l   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     9   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     90   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     9P   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9p   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        ?q   comment_on_resolution         �JULD resolution is 6 minutes, except when JULD = JULD_LOCATION or when JULD = JULD_FIRST_MESSAGE (TRAJ file variable); in that case, JULD resolution is 1 second        9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9|   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >��	4E�        9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          :�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  <�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          =T   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  ?l   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       ?�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       B   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  D$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       D�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  F�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       GL   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       Id   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  K|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  N   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       N�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  P�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    P�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    S�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    V�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  Y�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    Z   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    Z   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    Z    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    Z$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  Z(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    Zh   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    Zx   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Z|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         Z�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         Z�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        Z�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    Z�Argo profile    3.1 1.2 19500101000000  20090326084658  20200417103158  6900681 BIOArgo                                                         Antoine POTEAU                                                  PRES            TEMP            PSAL               D   IF  10680438                        2C  D   PROVOR_II                       n/a                             n/a                             841 @��N��>�1   @��N��>�@6�W���'�c�k��~(1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   B   B   A��A!��AS33A�ffA�33A�33Aə�AᙚA���B	��B��B!��B.  B933BE33BP��B\��Bi��Bu33B�ffB���B�33B���B���B�  B�33B�33B���B�  B�  B�B�  B���B�ffBڙ�B�  B�ffB���B�33B�ffB���CffCL�C33CffCL�C33CffCffCL�CL�C ffC#ffC&33C)33C,33C/33C2�C5ffC8� C;ffC>� CAffCD33CG33CJffCM33CPffCS� CVL�CY� C\ffC_33CbffCeffChL�Ck33CnL�Cq� CtffCw� Cz�3C}� C�33C��3C��C��fC�33C��3C�33C��3C�33C�� C�&fC���C�33C��3C�33C�� C�@ C��3C�33C��3C�&fC���C�33C���C�&fC��fC��C���C�@ C��3C�&fC��3C��C��3C�33C��fC��C���C�&fC��fC�33C��3C�&fC��3C�33CæfC��Cƙ�C�33C�L�D|  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A!��AS33A�ffA�33A�33Aə�AᙚA���B	��B��B!��B.  B933BE33BP��B\��Bi��Bu33B�ffB���B�33B���B���B�  B�33B�33B���B�  B�  B�B�  B���B�ffBڙ�B�  B�ffB���B�33B�ffB���CffCL�C33CffCL�C33CffCffCL�CL�C ffC#ffC&33C)33C,33C/33C2�C5ffC8� C;ffC>� CAffCD33CG33CJffCM33CPffCS� CVL�CY� C\ffC_33CbffCeffChL�Ck33CnL�Cq� CtffCw� Cz�3C}� C�33C��3C��C��fC�33C��3C�33C��3C�33C�� C�&fC���C�33C��3C�33C�� C�@ C��3C�33C��3C�&fC���C�33C���C�&fC��fC��C���C�@ C��3C�&fC��3C��C��3C�33C��fC��C���C�&fC��fC�33C��3C�&fC��3C�33CæfC��Cƙ�C�33C�L�D|  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��BA�BJ��BA�B@�BC�BB�B@�B>wB;dB;dB<jB=qB=qB=qB>wB>wB=qB<jB.B�B�B,B49BQ�Bl�Br�Bs�Bz�B� B�B�JB�bB�\B�JB�\B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B�B�-B�9B�FB�RB�qBǮB��B��B�B�B��BŢB�dB�LB�9B�\Bv�B_;BJ�B6FB#�B!�B�B�B�yB�;B��BǮB�^B�?B�B��B��B�PBv�BiyBM�BE�B�B
��B
�ZB
ĜB
��B
u�B
^5B
$�B
B	��B	�/B	��B	�{B	�1B	�JB	�B	�B	x�B	N�B	:^B	'�B	�B	uB	+B	B��B�B�B�fB��BB�jB�RB�LB�-B��B��B�DB|�B� B�=B�{BE��14111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114  BA�G�O�BA�B@�BC�BB�B@�B>wB;dB;dB<jB=qB=qB=qB>wB>wB=qB<jB.B�B�B,B49BQ�Bl�Br�Bs�Bz�B� B�B�JB�bB�\B�JB�\B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B�B�-B�9B�FB�RB�qBǮB��B��B�B�B��BŢB�dB�LB�9B�\Bv�B_;BJ�B6FB#�B!�B�B�B�yB�;B��BǮB�^B�?B�B��B��B�PBv�BiyBM�BE�B�B
��B
�ZB
ĜB
��B
u�B
^5B
$�B
B	��B	�/B	��B	�{B	�1B	�JB	�B	�B	x�B	N�B	:^B	'�B	�B	uB	+B	B��B�B�B�fB��BB�jB�RB�LB�-B��B��B�DB|�B� B�=B�{G�O�14111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114  <#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�Aѥ�B]�dAљ�A�\)A�l�A� �A�%A��TAϴ9Aϣ�Aϝ�Aϕ�AύPAω7Aω7AυA�v�AΕ�Aǧ�AŬA���A��A�v�A�C�A���A��A�(�A�\)A��`A�;dA��\A���A���A��DA�XA�A��wA�&�A�A���A�7LA��9A��PA�dZA�A��7A���A�|�A�C�A��A���A��-A�l�A�  A���A��!A��A�?}A��A��uA�5?A���A�JA��mA���A��A��FA���A�bNA��A�r�A���A�bA��#A�S�A��A�;dA�5?A�x�A��#A��A�^5A�{A���A�p�A���A���A��DA��HA���A�A��A��A���A�  A�Q�A�ffA�  Az�yAv�+At�`ApJAk�-AeƨAal�A`VA^�A]�-A[dZAS;dAQ7LAM�AK�AH�uAFz�AEXAB$�A@�/A>�A<  A6�HA3S�A2bA1+A/;dA.(�A+C�A(��A'C�A%ƨA%/A%�PA#�^A�S�14111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114  Aѥ�G�O�Aљ�A�\)A�l�A� �A�%A��TAϴ9Aϣ�Aϝ�Aϕ�AύPAω7Aω7AυA�v�AΕ�Aǧ�AŬA���A��A�v�A�C�A���A��A�(�A�\)A��`A�;dA��\A���A���A��DA�XA�A��wA�&�A�A���A�7LA��9A��PA�dZA�A��7A���A�|�A�C�A��A���A��-A�l�A�  A���A��!A��A�?}A��A��uA�5?A���A�JA��mA���A��A��FA���A�bNA��A�r�A���A�bA��#A�S�A��A�;dA�5?A�x�A��#A��A�^5A�{A���A�p�A���A���A��DA��HA���A�A��A��A���A�  A�Q�A�ffA�  Az�yAv�+At�`ApJAk�-AeƨAal�A`VA^�A]�-A[dZAS;dAQ7LAM�AK�AH�uAFz�AEXAB$�A@�/A>�A<  A6�HA3S�A2bA1+A/;dA.(�A+C�A(��A'C�A%ƨA%/A%�PA#�^G�O�14111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114  ;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 1.1,  -CTD2018V01 & ARGO2018V01 -                                                                                                                                    202004171031582020041710315820200417103158  IF  ARGQSCOO1.2                                                                 20091001150923  CF  TEMP            A��A��@�                  IF  ARGQSCOO1.2                                                                 20091001150923  CF  TEMP            AS33AS33@�                  IF  ARGQSCOO1.2                                                                 20091001150923  CF  TEMP            D|  D|  ?�                  IF  ARGQSCOO1.2                                                                 20091001150923  CF  PSAL            A��A��@�                  IF  ARGQSCOO1.2                                                                 20091001150923  CF  PSAL            AS33AS33@�                  IF  ARGQSCOO1.2                                                                 20091001150923  CF  PSAL            C�L�C�L�@�                  IF  ARGQCOAR1.0                                                                 20111010073740  QCP$                G�O�G�O�G�O�00840           IF  ARGQCOAR1.0                                                                 20111010073740  QCF$                G�O�G�O�G�O�00000           IF      SCOO1.4                                                                 20130109163605  QC                  G�O�G�O�G�O�                        CORA                                                                    20090812124006  SVP                 G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818103410  QCP$TEMP            G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818103518  QCP$TEMP            G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818104348  QCP$PSAL            G�O�G�O�G�O�                IF      COFC2.7                                                                 20151019160618                      G�O�G�O�G�O�                IF  ARSQOW  1.1 CTD2018V01 & ARGO2018V01                                        20200417103158  IP  PSAL            A��D|  G�O�                