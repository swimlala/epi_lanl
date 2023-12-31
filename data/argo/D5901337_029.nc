CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:31Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112819  20190522121836  1901_5055_029                   2C  D   APEX                            2140                            040306                          846 @�qd�q�1   @�qe��@@-ix����cL�hr�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A~ffA�33A�33A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0ffB8  B@  BH  BO��BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B�  B���B�  B�  B���B�  B�33B�33C �C�C�C�C  C	�fC  C  C�C  C  C  C  C  C  C�fC�fC"  C$  C%�fC(�C*�C,  C.  C0  C2  C4  C6�C8  C9�fC<  C>�C@  CB  CD  CF�CH�CJ�CL�CN  CP  CQ�fCT  CV�CX  CZ  C[�fC^  C`�Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C{�fC~  C�  C�  C�  C��C��C�  C��3C�  C�  C��C��C��C��C��C��C��C��C��C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C��C�  C��3C��3C�  C��C�  C��3D � D  D�fD  Dy�D  D� D  Dy�D  D�fDfD� D  D� DfD� D��D	� D
  D
� DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD  Dy�D��Dy�D  D� D  D�fDfD�fDfD�fD  Dy�D  D� D  D� D  Dy�D  D�fD  D� D��D� DfD� D   D � D ��D!� D"  D"y�D#  D#� D$  D$�fD%  D%y�D&  D&� D'  D'y�D'��D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-�fD.  D.y�D/  D/� D0  D0y�D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D:��D;y�D<  D<�fD=fD=�fD>fD>�fD?fD?�fD@  D@y�DA  DA� DB  DB� DCfDC�fDD  DD� DE  DE�fDFfDF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DLy�DM  DM� DN  DNy�DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT�fDU  DUy�DV  DV�fDWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D[��D\y�D\��D]y�D]��D^� D_  D_� D`  D`� DafDa� Da��Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Do��Dp� Dq  Dq� Dr  Dr� DsfDs�fDtfDt� Du  Duy�Dv  Dv� DyY�D�3D�C3D�|�D��fD�3D�0 D�|�D��3D��fD�  D�` DǦfD��3D�,�DچfDਗ਼D��3D��D�ffD�l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�33@�33A��A9��AY��Ax  A�  A�  A���A���A���A���A���A���BffBffBffB��B&��B.��B6ffB>ffBFffBN  BVffB^ffBfffBnffBv  B~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�ffB�33B�33B�  B�33B�33B�  B�33B�ffB�ffB�ffC�3C�3C�3C��C	� C��C��C�3C��C��C��C��C��C��C� C� C!��C#��C%� C'�3C)�3C+��C-��C/��C1��C3��C5�3C7��C9� C;��C=�3C?��CA��CC��CE�3CG�3CI�3CK�3CM��CO��CQ� CS��CU�3CW��CY��C[� C]��C_�3Ca�3Cc�3Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu�3Cw�3Cy��C{� C}��C��C���C���C�ٚC�ٚC���C�� C���C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C���C���C���C�� C�� C�� C�� C�� C���C�ٚC�ٚC�ٚC�ٚC���C���C���C���C���C���C���C���C���C�ٚC���C�� C�� C���C���C���C���C���C���C�� C���C���C�� C���C���C���C�ٚC���C���C���C�� C���C���C�� C���C���C���C�� C���C�ٚC���C���C�ٚC���C���C���C�� C���C���C�� C�� C���C���C���C�� C���C�ٚC���C���C���C���C�ٚC���C���C�ٚC���C���C���C�� C���C���C�� C���C���C�� C���C���C���C���C���C���C�� C���C���C���C�ٚC���C�� C���C�ٚC���C�� C�� C���C�ٚC���C�� D ffD �fDl�D�fD` D�fDffD�fD` D�fDl�D��DffD�fDffD��DffD� D	ffD	�fD
ffD
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D�fD` D� D` D�fDffD�fDl�D��Dl�D��Dl�D�fD` D�fDffD�fDffD�fD` D�fDl�D�fDffD� DffD��DffD�fD ffD � D!ffD!�fD"` D"�fD#ffD#�fD$l�D$�fD%` D%�fD&ffD&�fD'` D'� D(ffD(�fD)ffD)�fD*` D*�fD+ffD+�fD,ffD,�fD-l�D-�fD.` D.�fD/ffD/�fD0` D0�fD1ffD1�fD2` D2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7� D8ffD8�fD9ffD9�fD:ffD:� D;` D;�fD<l�D<��D=l�D=��D>l�D>��D?l�D?�fD@` D@�fDAffDA�fDBffDB��DCl�DC�fDDffDD�fDEl�DE��DFl�DF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKl�DK�fDL` DL�fDMffDM�fDN` DN�fDOffDO�fDPffDP��DQffDQ�fDRffDR�fDSffDS�fDTl�DT�fDU` DU�fDVl�DV��DWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[� D\` D\� D]` D]� D^ffD^�fD_ffD_�fD`ffD`��DaffDa� DbffDb�fDcffDc�fDdffDd�fDeffDe� DfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl� DmffDm�fDnffDn�fDoffDo� DpffDp�fDqffDq�fDrffDr��Dsl�Ds��DtffDt�fDu` Du�fDvffDy@ D��fD�6fD�p D���D��fD�#3D�p D��fD�ٚD�3D�S3DǙ�D��fD�  D�y�D���D�fD� D�Y�D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A̛�A̧�A̩�A̬A̮A̰!A̸RA���A���A�ĜA�A̼jA̼jA�ƨA�ĜA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��#A��A��#A��A���A���A���A��A��A��
A��#A��/A̗�A��AĮA�/A��A�%A�dZA��A� �A�$�A�/A��wA�7LA��wA���A�M�A���A�|�A�(�A���A�
=A�`BA�1A�ȴA�ffA�`BA�A�A��/A���A�ȴA��
A�A��`A�=qA�$�A��PA���A���A���A�z�A��FA�A��A�I�A���A�1A�&�A�A|Q�Az�DAwC�As|�Ap��Al�RAh��Af^5Ac��Ab1A`�A_A]oAXn�AT-AQ7LAO��ANI�AL�`ALjAJA�AFv�AC�AB��AAA>�9A=��A<�jA;��A:9XA933A8r�A4^5A0-A.�!A,��A+K�A*E�A'�
A&��A&�`A&�A%hsA$�`A#��A"��A �AbNA�-A��AAVAr�A�+AdZA33A�uAbA
=A�/A�Ar�AZAM�A9XAbA�#A�FA�PAp�AS�A�jAjA�#A��AAffAJA��A�PA9XA�^A��AS�A�A��A�AE�A��A\)A&�A��A�A
=A�wAXA�/A9XA��A�;AAS�AoA�Av�A��A
�A
A�A	�A	��A	x�A	C�A�A�+AZA�A�hA33Av�AA�mA�-A&�A��A�+A�A�wAVA�/A�Az�AI�A�A33AoA�A ��A �HA�A �/A n�@�|�@���@�K�@�ff@�-@��h@��@��-@��@�-@�&�@�(�@�7L@��@��@���@�%@���@�5?@���@�@��@��`@�j@��@���@�+@�7@��@�j@��y@��@��y@���@�!@�ȴ@�7L@�-@�bN@⟾@��;@�@��`@�1'@�(�@�b@�ƨ@� �@���@�t�@ܛ�@�?}@�I�@�^5@�/@ج@�  @�K�@�+@���@ָR@և+@�V@��@�x�@�(�@�dZ@�^5@���@Ь@�dZ@ΰ!@�{@�7L@�1@��@�ff@��@�E�@��`@ȴ9@�7L@�1'@�ff@ũ�@�hs@�V@�b@ÍP@�K�@�dZ@�@§�@�n�@��@�&�@���@��@�j@���@�33@�V@�=q@�hs@��@�V@���@�9X@�t�@�ȴ@��+@�{@�x�@�7L@��@��@��@��@�+@�v�@�@�`B@��j@�A�@���@���@�C�@�o@��y@�M�@��@��#@��#@���@��T@��#@�X@�z�@�1@���@�S�@�bN@�b@�dZ@��!@�+@�\)@�S�@�+@�ƨ@���@��P@�
=@�ȴ@�~�@�p�@��/@���@��@���@��9@�z�@�z�@�Z@�bN@��D@�V@�I�@�l�@��@��@���@�O�@�?}@��`@���@�j@��w@�o@�n�@�$�@���@��^@�@�p�@�p�@�/@�hs@�?}@���@�t�@�ȴ@�~�@�-@�-@�@��@��@��T@��7@�%@���@��/@���@�r�@��@��
@���@�\)@��w@��m@��w@�33@�E�@�-@�V@�n�@���@��\@��@�hs@�p�@��@��@���@�A�@�b@�ƨ@�;d@��y@�^5@�=q@�E�@�^5@��@���@��-@���@��9@�%@���@���@��
@��w@��@��P@�\)@�+@�~�@�5?@��@���@�x�@�G�@���@���@���@�`B@��7@��+@�A�@�+@{��@u?}@hQ�@[�F@S"�@J^5@B�\@<9X@5�-@/l�@&�y@�R@��@��@9X@1'@�@	�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A̛�A̧�A̩�A̬A̮A̰!A̸RA���A���A�ĜA�A̼jA̼jA�ƨA�ĜA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��#A��A��#A��A���A���A���A��A��A��
A��#A��/A̗�A��AĮA�/A��A�%A�dZA��A� �A�$�A�/A��wA�7LA��wA���A�M�A���A�|�A�(�A���A�
=A�`BA�1A�ȴA�ffA�`BA�A�A��/A���A�ȴA��
A�A��`A�=qA�$�A��PA���A���A���A�z�A��FA�A��A�I�A���A�1A�&�A�A|Q�Az�DAwC�As|�Ap��Al�RAh��Af^5Ac��Ab1A`�A_A]oAXn�AT-AQ7LAO��ANI�AL�`ALjAJA�AFv�AC�AB��AAA>�9A=��A<�jA;��A:9XA933A8r�A4^5A0-A.�!A,��A+K�A*E�A'�
A&��A&�`A&�A%hsA$�`A#��A"��A �AbNA�-A��AAVAr�A�+AdZA33A�uAbA
=A�/A�Ar�AZAM�A9XAbA�#A�FA�PAp�AS�A�jAjA�#A��AAffAJA��A�PA9XA�^A��AS�A�A��A�AE�A��A\)A&�A��A�A
=A�wAXA�/A9XA��A�;AAS�AoA�Av�A��A
�A
A�A	�A	��A	x�A	C�A�A�+AZA�A�hA33Av�AA�mA�-A&�A��A�+A�A�wAVA�/A�Az�AI�A�A33AoA�A ��A �HA�A �/A n�@�|�@���@�K�@�ff@�-@��h@��@��-@��@�-@�&�@�(�@�7L@��@��@���@�%@���@�5?@���@�@��@��`@�j@��@���@�+@�7@��@�j@��y@��@��y@���@�!@�ȴ@�7L@�-@�bN@⟾@��;@�@��`@�1'@�(�@�b@�ƨ@� �@���@�t�@ܛ�@�?}@�I�@�^5@�/@ج@�  @�K�@�+@���@ָR@և+@�V@��@�x�@�(�@�dZ@�^5@���@Ь@�dZ@ΰ!@�{@�7L@�1@��@�ff@��@�E�@��`@ȴ9@�7L@�1'@�ff@ũ�@�hs@�V@�b@ÍP@�K�@�dZ@�@§�@�n�@��@�&�@���@��@�j@���@�33@�V@�=q@�hs@��@�V@���@�9X@�t�@�ȴ@��+@�{@�x�@�7L@��@��@��@��@�+@�v�@�@�`B@��j@�A�@���@���@�C�@�o@��y@�M�@��@��#@��#@���@��T@��#@�X@�z�@�1@���@�S�@�bN@�b@�dZ@��!@�+@�\)@�S�@�+@�ƨ@���@��P@�
=@�ȴ@�~�@�p�@��/@���@��@���@��9@�z�@�z�@�Z@�bN@��D@�V@�I�@�l�@��@��@���@�O�@�?}@��`@���@�j@��w@�o@�n�@�$�@���@��^@�@�p�@�p�@�/@�hs@�?}@���@�t�@�ȴ@�~�@�-@�-@�@��@��@��T@��7@�%@���@��/@���@�r�@��@��
@���@�\)@��w@��m@��w@�33@�E�@�-@�V@�n�@���@��\@��@�hs@�p�@��@��@���@�A�@�b@�ƨ@�;d@��y@�^5@�=q@�E�@�^5@��@���@��-@���@��9@�%@���@���@��
@��w@��@��P@�\)@�+@�~�@�5?@��@���@�x�@�G�@���@���@���@�`B@��7@��+@�A�@�+@{��@u?}@hQ�@[�F@S"�@J^5@B�\@<9X@5�-@/l�@&�y@�R@��@��@9X@1'@�@	�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�'B	�'B	�'B	�'B	�'B	�'B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�B	�!B	�!B	�B	�B	�!B	�B	�B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B
6FB
L�B
�3B
��BB
�B
�B
��B	7B+B{BZBl�Br�Bu�Bt�B�%B�\B�hB�JB��BĜB�dB�LB��B��B��B�\B�B��B�bBv�B[#B1'B �B{B
�sB
��B
��B
� B
I�B
&�B
+B
hB
B	�ZB	ɺB	�XB	��B	�B	k�B	Q�B	;dB	.B	 �B	�B	PB	B�B�TB�B	B		7B	{B	�B	�B	�B	�B	�B	�B	�B	(�B	+B	-B	1'B	6FB	5?B	,B	�B	B	  B	  B��B��B	  B	�B	#�B	&�B	&�B	$�B	 �B	�B	�B	�B	%�B	+B	8RB	H�B	P�B	e`B	�1B	��B	��B	��B	��B	�9B	�wB	ŢB	��B	��B	��B	�B	�B	�/B	�HB	�TB	�TB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
B
B
B
B
B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B

=B

=B
	7B

=B
DB
DB
	7B
+B
%B
B
B
B
B
B
B
%B
	7B
PB
{B
�B
�B
{B
VB
+B
%B
%B
+B
1B
DB
\B
\B
JB
1B
  B	��B	��B
1B
VB
VB
	7B
+B
1B

=B
DB
	7B
B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	�B	�fB	�;B	�5B	�)B	�/B	�5B	�BB	�fB	�sB	�sB	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�B	�sB	�mB	�ZB	�HB	�/B	�5B	�/B	�#B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�5B	�BB	�NB	�NB	�TB	�TB	�TB	�TB	�NB	�TB	�ZB	�ZB	�TB	�TB	�NB	�NB	�TB	�ZB	�`B	�fB	�mB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
1B
1B
1B
1B
+B
B
B
B
B	��B	��B	��B	��B	��B	��B
B
+B
%B
B
B
B
B
B
+B
+B
+B
B
B
B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
PB
\B
bB
VB
VB
VB
\B
bB
uB
uB
hB
bB
bB
uB
oB
\B
VB
PB
PB
VB
VB
VB
\B
bB
hB
hB
oB
uB
hB
hB
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
�B
%�B
-B
49B
7LB
<jB
A�B
F�B
N�B
T�B
[#B
]/B
cTB
hsB
q�B
v�B
x�B
z�B
}�B
�B
�%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�'B	�'B	�'B	�'B	�'B	�'B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�B	�!B	�!B	�B	�B	�!B	�B	�B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
\B
L�B
gmB
ƨBJB�BVBDB�B�B�B�BcTBt�B{�B�B�B�PB��B��B�hB��B��B��B��B�!B��B��B��B�oB�B��B�oBw�B=qB2-B2-BB
�)B
��B
�B
r�B
G�B
?}B
$�B
�B	��B	�B	��B	�jB	��B	�%B	l�B	L�B	?}B	.B	"�B	�B	�B	VB	  B	B	JB	{B	�B	"�B	+B	7LB	2-B	$�B	%�B	+B	1'B	33B	7LB	<jB	A�B	D�B	M�B	0!B	VB	\B	DB	1B	JB	%B	�B	)�B	-B	-B	/B	,B	+B	)�B	!�B	%�B	'�B	6FB	G�B	O�B	bNB	�DB	��B	��B	��B	��B	�FB	��B	ƨB	��B	��B	�B	�B	�B	�;B	�TB	�`B	�mB	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
1B

=B
+B
B
+B
	7B
\B
B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
%B
B
B
B
B
%B
%B
B
B
B
%B
%B
B
B
JB
VB
JB
PB
VB
\B
PB
	7B
1B
+B
%B
+B
B
B
B
+B

=B
PB
�B
�B
�B
�B
�B

=B
+B
1B
+B
1B
DB
hB
uB
hB
hB
B	��B	��B
	7B
uB
{B
PB
+B
1B
DB
PB
PB
B
B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B
B	�B	�B	�NB	�BB	�)B	�/B	�5B	�BB	�fB	�sB	�sB	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�sB	�mB	�ZB	�HB	�/B	�TB	�/B	�5B	�/B	�#B	�B	��B	��B	��B	��B	��B	��B	�B	�)B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�HB	�HB	�TB	�ZB	�TB	�fB	�`B	�ZB	�ZB	�`B	�ZB	�`B	�TB	�`B	�ZB	�ZB	�TB	�fB	�mB	�sB	�mB	�yB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
1B

=B
	7B

=B

=B
B
B
+B
+B	��B	��B	��B	��B	��B	��B
B

=B
	7B
+B
1B
%B
%B
B
1B
+B
1B
+B
B
B
  B	��B	��B	��B	��B
  B
B
B
%B
+B
%B
B
B
B
B
%B
%B
+B
1B
1B

=B
1B
1B
	7B

=B

=B
	7B
	7B
	7B
	7B
PB
bB
oB
bB
VB
VB
\B
bB
uB
�B
oB
bB
\B
{B
�B
hB
VB
VB
\B
\B
bB
\B
\B
bB
oB
oB
uB
�B
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
�B
%�B
-B
49B
7LB
<jB
A�B
F�B
N�B
VB
[#B
^5B
cTB
iyB
q�B
v�B
y�B
z�B
}�B
�B
�%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��=C�<�9X<���<���<���<ě�<�/<�j<�h<u<e`B<#�
<#�
<#�
<#�
<e`B<T��<#�
<#�
<#�
<#�
<#�
<#�
<u<�9X<e`B<#�
<49X<D��<T��<�C�<ě�<���<�`B<D��<�C�<�`B<���<���<�`B=8Q�=�w=o<��
<�t�<�h<�j<u<���<���<�j<���<���<�C�<�C�<T��<D��<u<��
<�<�/<�t�<49X<49X<#�
<#�
<���<���<�C�<#�
<T��<e`B<#�
<#�
<#�
<49X<49X<u=o<���<T��<e`B<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<49X<e`B<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250172012011312501720120113125017  AO  ARGQ                                                                        20111205112819  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112819  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125017  IP                  G�O�G�O�G�O�                