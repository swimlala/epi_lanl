CDF   	   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   j   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      history       X2009-03-26T08:47:48Z creation; 2015-10-19T16:06:25Z last update (coriolis COFC software)   comment_dmqc_operator         DPRIMARY | https://orcid.org/0000-0002-3512-2070 | Saout-Grit, Glazeo      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                  l  <\   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  <�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  >p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  B,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  D@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  FT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  G�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Hh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  J   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  L$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    LT   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    OT   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    RT   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  UT   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    U�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    U�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    U�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    U�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  U�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    U�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    U�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    U�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         U�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         U�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        U�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    V Argo profile    3.1 1.2 19500101000000  20090326084748  20200417103158  6900681 BIOArgo                                                         Antoine POTEAU                                                  PRES            TEMP            PSAL               D   IF  10680458                        2C  D   PROVOR_II                       n/a                             n/a                             841 @���8�91   @���8�9@7~ߤ?���c��1���1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   A   A   A+33AY��A�ffA�  A���A�33A�  A�ffB
  B33B ffB-��B:��BF  BQ��B]��Bi33Bu33B���B���B���B���B���B���B�ffB�ffB�ffB���B���B�ffB�  B�ffB�ffB�  B���B�33B�  B�  B���B���CL�C33CL�CL�C� CL�C33CL�C33CffC 33C#L�C&ffC)ffC,��C/� C2L�C5� C8ffC;L�C>� CA��CD� CGffCJL�CM33CP33CSffCV33CY�C\33C_ffCb� CeffCh33Ck33CnL�CqL�CtL�Cw33CzffC}ffC�&fC��3C�  C�� C�L�C�� C�@ C�� C�@ C�� C�33C��3C�33C��3C�&fC��fC�&fC��fC�33C��3C�@ C�� C�&fC�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A+33AY��A�ffA�  A���A�33A�  A�ffB
  B33B ffB-��B:��BF  BQ��B]��Bi33Bu33B���B���B���B���B���B���B�ffB�ffB�ffB���B���B�ffB�  B�ffB�ffB�  B���B�33B�  B�  B���B���CL�C33CL�CL�C� CL�C33CL�C33CffC 33C#L�C&ffC)ffC,��C/� C2L�C5� C8ffC;L�C>� CA��CD� CGffCJL�CM33CP33CSffCV33CY�C\33C_ffCb� CeffCh33Ck33CnL�CqL�CtL�Cw33CzffC}ffC�&fC��3C�  C�� C�L�C�� C�@ C�� C�@ C�� C�33C��3C�33C��3C�&fC��fC�&fC��fC�33C��3C�@ C�� C�&fC�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��BÖB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�}B�dB��B}�B�bB�DB��B��B�bB��B��B��B��B��B�B�!B�3B�wBĜBǮB��B��B��BɺBɺB��B��BɺBɺBÖB�qB�XB�?B�3B�?B��B�#B�HB��BB�FB�B�7B�Bl�BXB.BVB  B�B��B��B��B�BdZB6FBhBB
�B
�B
��B
��B
�{B
�bB
�PB
e`B
>wB
;dB
2-B
'�B
+B	�B	�B	�BB	�B	�#B	�wB	��B	�hB	�1B	gmB	A�B	,B	&�B	$�B	�B�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BÖB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�}B�dB��B}�B�bB�DB��B��B�bB��B��B��B��B��B�B�!B�3B�wBĜBǮB��B��B��BɺBɺB��B��BɺBɺBÖB�qB�XB�?B�3B�?B��B�#B�HB��BB�FB�B�7B�Bl�BXB.BVB  B�B��B��B��B�BdZB6FBhBB
�B
�B
��B
��B
�{B
�bB
�PB
e`B
>wB
;dB
2-B
'�B
+B	�B	�B	�BB	�B	�#B	�wB	��B	�hB	�1B	gmB	A�B	,B	&�B	$�B	�B�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
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
A�|�A��A�A���A��A��yA��TA��/A��A��
A���A���A���A���A���A���A�ȴA�ĜA���Aӝ�AЧ�A�
=A�ZA�n�A��yA�n�A�9XA��+A��A�ȴA�/A�  A�v�A�A�t�A��PA��FA��A���A�ƨA�t�A�9XA���A�~�A�bA��-A�O�A��HA��/A�7LA��DA��A��uA�&�A�dZA�"�A�K�A�\)A�XA�A���A�=qA�dZA�bA�(�A��/A��uA�K�A���A�=qA��A��jA�-A���A�t�A��7A�~�A�C�A��A�VA�~�A���A���A�C�A�=qA�S�A�hA~ĜAz�Av�At��Ar��Ap5?AnĜAl1'Ai��Ag��Ad��Aa�A[ƨAW7LASO�AR^5AQp�ANĜAH��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�|�A��A�A���A��A��yA��TA��/A��A��
A���A���A���A���A���A���A�ȴA�ĜA���Aӝ�AЧ�A�
=A�ZA�n�A��yA�n�A�9XA��+A��A�ȴA�/A�  A�v�A�A�t�A��PA��FA��A���A�ƨA�t�A�9XA���A�~�A�bA��-A�O�A��HA��/A�7LA��DA��A��uA�&�A�dZA�"�A�K�A�\)A�XA�A���A�=qA�dZA�bA�(�A��/A��uA�K�A���A�=qA��A��jA�-A���A�t�A��7A�~�A�C�A��A�VA�~�A���A���A�C�A�=qA�S�A�hA~ĜAz�Av�At��Ar��Ap5?AnĜAl1'Ai��Ag��Ad��Aa�A[ƨAW7LASO�AR^5AQp�ANĜAH��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 1.1,  -CTD2018V01 & ARGO2018V01 -                                                                                                                                    202004171031582020041710315820200417103158  IF  ARGQSCOO1.2                                                                 20091001150902  CF  TEMP            A+33C�@ @@                  IF  ARGQCOAR1.0                                                                 20111010073804  QCP$                G�O�G�O�G�O�00840           IF  ARGQCOAR1.0                                                                 20111010073804  QCF$                G�O�G�O�G�O�00000           IF      SCOO1.4                                                                 20130109163604  QC                  G�O�G�O�G�O�                        CORA                                                                    20090817133727  SVP                 G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818103651  QCP$TEMP            G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818104840  QCP$PSAL            G�O�G�O�G�O�                IF      COFC2.7                                                                 20151019160625                      G�O�G�O�G�O�                IF  ARSQOW  1.1 CTD2018V01 & ARGO2018V01                                        20200417103158  IP  PSAL            A+33C�@ G�O�                