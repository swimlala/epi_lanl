CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   u   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      history       X2010-12-29T04:39:32Z creation; 2015-10-19T16:08:03Z last update (coriolis COFC software)   comment_dmqc_operator         DPRIMARY | https://orcid.org/0000-0002-3512-2070 | Saout-Grit, Glazeo      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                  x  <�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  =    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  >�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ?L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  A    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Cl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  E@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  E�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  G�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  I`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  K�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  L$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  M�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    N(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    Q(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    T(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  W(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    WT   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    WX   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    W\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    W`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  Wd   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    W�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    W�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    W�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         W�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         W�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        W�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    W�Argo profile    3.1 1.2 19500101000000  20101229043932  20200417103158  6900681 BIOArgo                                                         Antoine POTEAU                                                  PRES            TEMP            PSAL               \D   IF  15254460                        2C  D   PROVOR_II                       n/a                             n/a                             841 @վ͈'qf1   @վ͈'qf@6<w�kP��d˰��'S1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   A   A   AffA&ffAP  A���A���A�33A�33AᙚA�33B
ffB��B"  B-33B9��BF  BQ��B]33Bj  Bu33B���B�ffB�  B�ffB���B���B�ffB�  B���B���B�33B���B�  B�ffB�ffB�ffB���B�33B왚B���B���B���C� C��C� CffC��CffCL�CffCffCL�C L�C#L�C&ffC)� C,ffC/L�C233C5ffC8ffC;� C>� CAffCD�CG� CJL�CMffCP��CSffCVL�CY� C\� C_ffCbffCeffChL�CkL�CnffCq�CtL�Cw33CzffC}33C�&fC��3C��C���C�33C���C��C���C�33C�� C�&fC��3C�33C���C�@ C��3C�&fC��3C�&fC��fC�@ C��3C�&fC��3C�&fC���C�33C�� C�33C��fC��C�� C�Y�C�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AffA&ffAP  A���A���A�33A�33AᙚA�33B
ffB��B"  B-33B9��BF  BQ��B]33Bj  Bu33B���B�ffB�  B�ffB���B���B�ffB�  B���B���B�33B���B�  B�ffB�ffB�ffB���B�33B왚B���B���B���C� C��C� CffC��CffCL�CffCffCL�C L�C#L�C&ffC)� C,ffC/L�C233C5ffC8ffC;� C>� CAffCD�CG� CJL�CMffCP��CSffCVL�CY� C\� C_ffCbffCeffChL�CkL�CnffCq�CtL�Cw33CzffC}33C�&fC��3C��C���C�33C���C��C���C�33C�� C�&fC��3C�33C���C�@ C��3C�&fC��3C�&fC��fC�@ C��3C�&fC��3C�&fC���C�33C�� C�33C��fC��C�� C�Y�C�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��B^5B^5B^5B]/B^5B^5B^5B^5B]/B^5B]/B^5B^5B^5B^5B^5B^5B^5B^5B^5B]/B^5B^5B^5B^5B_;B�JB��B��B��B�B�BJB�B�?B�PBiyBP�BI�BN�BD�B;dB<jB:^B?}BF�BYBM�BC�B9XB0!B&�B�BhB  B�mB�)B��B��BƨB�3B�XB�qB��BdZBI�B:^B+B%�B�BB�B�TB�)B��B�9B|�BhsBG�B49B#�B
��B
�BB
��B
�LB
��B
��B
|�B
\)B
M�B
B�B
:^B
-B
B	�;B	B	��B	�wB	��B	hsB	`BB	]/B	\)B	XB	S�B	C�B	7LB	)�B	oB	%B	B��B��B��B��B�HB��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B^5B^5B^5B]/B^5B^5B^5B^5B]/B^5B]/B^5B^5B^5B^5B^5B^5B^5B^5B^5B]/B^5B^5B^5B^5B_;B�JB��B��B��B�B�BJB�B�?B�PBiyBP�BI�BN�BD�B;dB<jB:^B?}BF�BYBM�BC�B9XB0!B&�B�BhB  B�mB�)B��B��BƨB�3B�XB�qB��BdZBI�B:^B+B%�B�BB�B�TB�)B��B�9B|�BhsBG�B49B#�B
��B
�BB
��B
�LB
��B
��B
|�B
\)B
M�B
B�B
:^B
-B
B	�;B	B	��B	�wB	��B	hsB	`BB	]/B	\)B	XB	S�B	C�B	7LB	)�B	oB	%B	B��B��B��B��B�HB��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
A�&�A�$�A�$�A�1'A�+A�&�A�+A�1'A�5?A�9XA�?}A�C�A�=qA�;dA�=qA�?}A�C�A�A�A�A�A�E�A�O�A�O�A�O�A�K�A�K�A�G�AΟ�A�v�Ḁ�A�{A��A�
=A�x�AîA�  A�7LA���A�A�A�S�A�5?A��/A�"�A���A�JA� �A�G�A���A��-A���A���A�JA�7LA���A�hsA�jA�oA��A�-A�A���A�A���A���A�XA���A���A��yA�x�A��A�jA�A�A�ȴA�n�A��RA��#A��TA�ffA�(�A�l�A���A�p�A�?}A���A��uA��hA��yA~n�Az�HAy;dAw��Av��At�An�!Ag�FAd=qAc�-Ac/A^AT�AS�AQ��AQ��AQ&�AO�TAL��AJ�AFz�A@^5A<�9A;�A;%A9��A8�!A8^5A3�-A/��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�&�A�$�A�$�A�1'A�+A�&�A�+A�1'A�5?A�9XA�?}A�C�A�=qA�;dA�=qA�?}A�C�A�A�A�A�A�E�A�O�A�O�A�O�A�K�A�K�A�G�AΟ�A�v�Ḁ�A�{A��A�
=A�x�AîA�  A�7LA���A�A�A�S�A�5?A��/A�"�A���A�JA� �A�G�A���A��-A���A���A�JA�7LA���A�hsA�jA�oA��A�-A�A���A�A���A���A�XA���A���A��yA�x�A��A�jA�A�A�ȴA�n�A��RA��#A��TA�ffA�(�A�l�A���A�p�A�?}A���A��uA��hA��yA~n�Az�HAy;dAw��Av��At�An�!Ag�FAd=qAc�-Ac/A^AT�AS�AQ��AQ��AQ&�AO�TAL��AJ�AFz�A@^5A<�9A;�A;%A9��A8�!A8^5A3�-A/��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 1.1,  -CTD2018V01 & ARGO2018V01 -                                                                                                                                    202004171031582020041710315820200417103158  IF  ARGQCOAR1.0                                                                 20101229042740  QCP$                G�O�G�O�G�O�DEBEC           IF  ARGQCOAR1.0                                                                 20101229042740  QCF$                G�O�G�O�G�O�02000           IF  CORTCOOA5.2 RTQCGL01                                                        20101230052748  QCP$PSAL            G�O�G�O�G�O�                IF  CORTCOOA5.2 RTQCGL01                                                        20101230052055  QCP$TEMP            G�O�G�O�G�O�                IF      SCOO1.3                                                                 20101229114833  QC                  G�O�G�O�G�O�                IF      SCOO1.3                                                                 20101229115348  QC                  G�O�G�O�G�O�                IF  ARGQCOAR1.0                                                                 20111010074454  QCP$                G�O�G�O�G�O�00840           IF  ARGQCOAR1.0                                                                 20111010074454  QCF$                G�O�G�O�G�O�00000           IF      SCOO1.4                                                                 20130109163546  QC                  G�O�G�O�G�O�                        CORA                                                                    20110317010410  SVP                 G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818110804  QCP$TEMP            G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818111143  QCP$TEMP            G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818112229  QCP$PSAL            G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818112450  QCP$PSAL            G�O�G�O�G�O�                IF      COFC2.7                                                                 20151019160803                      G�O�G�O�G�O�                IF  ARSQOW  1.1 CTD2018V01 & ARGO2018V01                                        20200417103158  IP  PSAL            AffC�  G�O�                