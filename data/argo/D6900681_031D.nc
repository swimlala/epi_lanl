CDF   	   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      history       X2009-04-29T16:08:07Z creation; 2015-10-19T16:06:49Z last update (coriolis COFC software)   comment_dmqc_operator         DPRIMARY | https://orcid.org/0000-0002-3512-2070 | Saout-Grit, Glazeo      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        =���   axis      Z        �  :�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  <�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  <�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ?4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  A    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  E   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  E�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  GL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  I   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  KX   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  K�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  M�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    P�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    S�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  V�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    V�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    V�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    V�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    W    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  W   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    WD   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    WT   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    WX   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         Wh   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         Wl   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        Wp   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    WtArgo profile    3.1 1.2 19500101000000  20090429160807  20200417103158  6900681 BIOArgo                                                         Antoine POTEAU                                                  PRES            TEMP            PSAL               D   IF  10680535                        2C  D   PROVOR_II                       n/a                             n/a                             841 @�&K�8�1   @�&K�8�@8���$u�cہ���1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   A   A   AY��A���A�ffA���A�33A噚A���B
  B��B ��B,ffB8ffBD  BQ��B_33Bj��Bv  B���B�ffB�  B���B�33B���B�  B�33B�  B���B�33B���B�33B�33B�33B�ffB�ffB���B�33B�ffB���B�  C��C��C��CffC� CffC33CffC� CL�C L�C#��C&��C)� C,� C/� C2� C5� C8L�C;L�C>ffCAL�CDL�CG33CJ�CMffCPL�CS� CVffCY33C\L�C_L�CbL�CeL�Ch33Ck�Cn� CqffCt� CwL�CzffC}33C�@ C���C�L�C���C�L�C��3C�&fC��3C�Y�C�ٚC�Y�C�� C�&fC��fC�33C��fC�&fC�� C��C��fC�33C��3C��C���C�33C���C�&fC��fC�33C��3C��C��fC�ffC�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AY��A���A�ffA���A�33A噚A���B
  B��B ��B,ffB8ffBD  BQ��B_33Bj��Bv  B���B�ffB�  B���B�33B���B�  B�33B�  B���B�33B���B�33B�33B�33B�ffB�ffB���B�33B�ffB���B�  C��C��C��CffC� CffC33CffC� CL�C L�C#��C&��C)� C,� C/� C2� C5� C8L�C;L�C>ffCAL�CDL�CG33CJ�CMffCPL�CS� CVffCY33C\L�C_L�CbL�CeL�Ch33Ck�Cn� CqffCt� CwL�CzffC}33C�@ C���C�L�C���C�L�C��3C�&fC��3C�Y�C�ٚC�Y�C�� C�&fC��fC�33C��fC�&fC�� C��C��fC�33C��3C��C���C�33C���C�&fC��fC�33C��3C��C��fC�ffC�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��B�oB�VB�PB�PB�VB�VB�VB�VB�VB�VB�\B�VB�VB�VB�PB�JB�=B�1B�%B�+B�%B�%B�%B�B�B�B�B�B�B�Bz�By�Bx�By�By�Bx�Bw�Bw�Bw�Bw�Bv�Bv�Bu�Bu�Bt�Bs�Bp�Bo�Bl�BjBhsBhsBaHB\)BS�B?}B49B�B��B�
B�VBN�B�Bq�BR�BH�B?}B33B/B#�B
=B
�B
��B
�B
�VB
�=B
�+B
v�B
:^B
�B
\B
B	��B	��B	�B	�fB	�5B	�B	��B	B	�}B	�wB	�XB	�-B	��B	��B	aHB	9XB	33B	-B	$�B	�B	VB	%B��B�B�B�ZB�5B�/B�#B�B�BŢB��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�oB�VB�PB�PB�VB�VB�VB�VB�VB�VB�\B�VB�VB�VB�PB�JB�=B�1B�%B�+B�%B�%B�%B�B�B�B�B�B�B�Bz�By�Bx�By�By�Bx�Bw�Bw�Bw�Bw�Bv�Bv�Bu�Bu�Bt�Bs�Bp�Bo�Bl�BjBhsBhsBaHB\)BS�B?}B49B�B��B�
B�VBN�B�Bq�BR�BH�B?}B33B/B#�B
=B
�B
��B
�B
�VB
�=B
�+B
v�B
:^B
�B
\B
B	��B	��B	�B	�fB	�5B	�B	��B	B	�}B	�wB	�XB	�-B	��B	��B	aHB	9XB	33B	-B	$�B	�B	VB	%B��B�B�B�ZB�5B�/B�#B�B�BŢB��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
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
A�$�A�1A��A��mA��A���A���A��RA��A���A�z�A�ffA�ZA�G�A�{A��HA���A���A�v�A�ffA�^5A�I�A�33A��A��#A��A�33A�ƨA�"�A�A�E�A�"�A�1A��A��yA���A�ĜA���A��-A���A���A��7A�r�A�`BA�E�A�JA��
A��!A��PA�dZA�-A��A���A�O�A��
A�$�A�~�A�9XA��FA���A�hsA�`BA�XA���A�x�A�$�A�dZA�  A���A�S�A�33A�bNA��A��A���A�n�A�9XA��/Ax�+Asx�Aq�AnffAmK�Ak��Aip�Ah�Ag&�Ad�Acl�Ab��Ab=qAa�PA`M�A^�9A]
=A[K�AQ\)AK��AJr�AI?}AF~�AC��A@��A>�A<^5A:(�A8ĜA6ZA5�TA5\)A4r�A3�A2�uA,ȴA"�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�$�A�1A��A��mA��A���A���A��RA��A���A�z�A�ffA�ZA�G�A�{A��HA���A���A�v�A�ffA�^5A�I�A�33A��A��#A��A�33A�ƨA�"�A�A�E�A�"�A�1A��A��yA���A�ĜA���A��-A���A���A��7A�r�A�`BA�E�A�JA��
A��!A��PA�dZA�-A��A���A�O�A��
A�$�A�~�A�9XA��FA���A�hsA�`BA�XA���A�x�A�$�A�dZA�  A���A�S�A�33A�bNA��A��A���A�n�A�9XA��/Ax�+Asx�Aq�AnffAmK�Ak��Aip�Ah�Ag&�Ad�Acl�Ab��Ab=qAa�PA`M�A^�9A]
=A[K�AQ\)AK��AJr�AI?}AF~�AC��A@��A>�A<^5A:(�A8ĜA6ZA5�TA5\)A4r�A3�A2�uA,ȴA"�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 1.1,  -CTD2018V01 & ARGO2018V01 -                                                                                                                                    202004171031582020041710315820200417103158  TC      SCOO1.2                                                                 20091001150942  QC                  G�O�G�O�G�O�                IF  CODMCOOA5.1 DMQCGL01                                                        20091102165936  QCP$PSAL            G�O�G�O�G�O�                IF  CODMCOOA5.1 DMQCGL01                                                        20091102164021  QCP$TEMP            G�O�G�O�G�O�                IF  ARGQCOAR1.0                                                                 20111010073937  QCP$                G�O�G�O�G�O�00840           IF  ARGQCOAR1.0                                                                 20111010073937  QCF$                G�O�G�O�G�O�00000           IF      SCOO1.4                                                                 20130109163600  QC                  G�O�G�O�G�O�                        CORA                                                                    20101009053754  SVP                 G�O�G�O�G�O�                IF      COFC2.7                                                                 20151019160649                      G�O�G�O�G�O�                IF  ARSQOW  1.1 CTD2018V01 & ARGO2018V01                                        20200417103158  IP  PSAL            AY��C�L�G�O�                