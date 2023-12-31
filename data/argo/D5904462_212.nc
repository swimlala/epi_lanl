CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-09-04T17:02:25Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20170904170225  20190405100807  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�#�&�F1   @�#����2@.$�/�d����m1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   B   @�ff@�  A   A   A@  A`  A~ffA�  A�33A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�33B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3y�D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy� D�  D�,�D�|�D��fD�fD�@ D�y�D���D�fD�0 D��fD�ɚD��D�S3Dڙ�D��3D�� D�<�D�p D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@ҏ\A	G�A)G�AIG�AiG�A��
A���A��
A��
Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BBQ�BJ�RBRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�B�\)B���B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�{C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CV�{CX�{CZ�{C\�{C^�{C`�{Cb�{Cd�Cf�{Ch�{Cj�{Cl�{Cn�{Cpz�Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�W
C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3��D4%D4��D5%D5�D6%D6�D7%D7�D8%D8�D9%D9��D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�Dy�D��D�?\D��\D���D�(�D�R�D��)D��\D��D�B�D���D��)D�\D�e�Dڬ)D���D��D�O\D�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�+A�-A�-A�-A�(�A�-A�33A�5?A�5?A�5?A�1'A�$�A�5?A�5?A�/A�(�A�{A�
=A�A��yA��HA��A���A���A���A���A�ȴA�ƨA�A���Aܺ^AܸRAܴ9Aܰ!Aܩ�A܅A�VA��
AۑhA���Aٗ�A���A�33AԴ9A� �AӼjA�
=A�oA�l�A��AʑhA��A�r�Aȕ�A�bA�ȴA�ĜA�hsA��A�^5A�bA���A��HA��A���A��FA��DA�33A��;A� �A�oA��A�?}A�oA�|�A�+A�-A��A�ĜA���A�bNA���A�I�A�XA�I�A�I�A��DA�+A�A�A��7A���A�S�A�bNA��A��A�C�A��A��PA�|�A�x�A�oA�x�A��\A�VA�hsAz��Av�`Ar�Am�AjJAh��Ael�A^�DAX  AR�yAP1'ANM�AL�yAI��AIhsAH��AE�FAD�AC�PAB��A?�^A=A8�/A7��A7K�A6JA57LA4ffA4�A3�mA3O�A2��A1�wA0�A.��A.{A.�yA.ffA.1'A-ƨA-O�A,��A,{A*��A)��A)ƨA)�PA)O�A(1'A&n�A%��A%�FA%hsA%ƨA'VA&�!A&JA%dZA%p�A$��A$��A$Q�A#��A"9XA!�A z�A�;A?}A ��A!
=A��A�jA?}A��A(�A`BA��A�#A��A\)A�AE�A�PA��A�A�A/A��A  AS�A�\AVA{A�A�A��A=qA�TA�PAVA�/Av�A=qA�;A�-A�hAl�At�A��A�7A;dA
��A
�\A
Q�A
(�A	�A	�A	O�A��A��AQ�A=qA=qA-A�A\)A��A^5AM�A1'A-A$�A��A�-AO�A�yA��A�9Ar�AE�A�AA�^A;dA��A�A�9A1'A��A+AoA�A&�A�A �yA ��A 1'@���@��@�E�@�p�@�7L@��@��y@��\@�ff@�X@��F@�x�@�z�@��@��H@�v�@��@�p�@�Z@�t�@���@��T@�/@���@�@��m@��H@�V@�@�X@�z�@�;d@�ȴ@�!@�V@���@�-@�hs@���@��;@��@�5?@��@��`@�|�@�+@�o@��@��@���@ݺ^@ݡ�@�x�@�`B@�%@�Z@�1@�|�@��@ڇ+@�$�@���@٩�@�X@�7L@���@�Z@׾w@�"�@�ȴ@֗�@և+@և+@֏\@�ff@��#@�G�@�r�@���@��@ҏ\@�ff@�=q@�J@�/@�9X@ϝ�@�S�@�@θR@�=q@�7L@��@��/@̃@��;@�+@�~�@�$�@ɲ-@�`B@��@ȋD@��@�33@�@�ȴ@�ȴ@�~�@�ff@ŉ7@�&�@�A�@��m@ÍP@�+@°!@�-@�`B@���@�Z@�b@��m@�ƨ@�l�@���@�V@�-@��@���@��@���@�ƨ@�S�@�33@��@�=q@�hs@��j@�(�@�ƨ@�@��\@�J@�G�@�Ĝ@���@�I�@�ƨ@�33@���@���@�V@��T@���@� �@�t�@��+@���@���@�%@��9@��u@�1'@�"�@���@��!@�n�@�{@�?}@�%@��@���@��@��@��w@�t�@�;d@�+@�@�5?@�@�-@�-@�@��7@��@�Ĝ@��`@��`@���@�9X@�b@��P@��@�ȴ@��!@���@�~�@�M�@���@��T@���@�O�@�/@���@��`@��9@�z�@�9X@�j@��@�j@� �@��F@�S�@�
=@��@��H@��!@�-@��7@��@�9X@���@���@���@�Q�@�^5@��@r-@h�@b�!@X  @Pb@Kƨ@C��@;ƨ@3dZ@+33@"��@�j@�P@Z@��@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�+A�+A�-A�-A�-A�(�A�-A�33A�5?A�5?A�5?A�1'A�$�A�5?A�5?A�/A�(�A�{A�
=A�A��yA��HA��A���A���A���A���A�ȴA�ƨA�A���Aܺ^AܸRAܴ9Aܰ!Aܩ�A܅A�VA��
AۑhA���Aٗ�A���A�33AԴ9A� �AӼjA�
=A�oA�l�A��AʑhA��A�r�Aȕ�A�bA�ȴA�ĜA�hsA��A�^5A�bA���A��HA��A���A��FA��DA�33A��;A� �A�oA��A�?}A�oA�|�A�+A�-A��A�ĜA���A�bNA���A�I�A�XA�I�A�I�A��DA�+A�A�A��7A���A�S�A�bNA��A��A�C�A��A��PA�|�A�x�A�oA�x�A��\A�VA�hsAz��Av�`Ar�Am�AjJAh��Ael�A^�DAX  AR�yAP1'ANM�AL�yAI��AIhsAH��AE�FAD�AC�PAB��A?�^A=A8�/A7��A7K�A6JA57LA4ffA4�A3�mA3O�A2��A1�wA0�A.��A.{A.�yA.ffA.1'A-ƨA-O�A,��A,{A*��A)��A)ƨA)�PA)O�A(1'A&n�A%��A%�FA%hsA%ƨA'VA&�!A&JA%dZA%p�A$��A$��A$Q�A#��A"9XA!�A z�A�;A?}A ��A!
=A��A�jA?}A��A(�A`BA��A�#A��A\)A�AE�A�PA��A�A�A/A��A  AS�A�\AVA{A�A�A��A=qA�TA�PAVA�/Av�A=qA�;A�-A�hAl�At�A��A�7A;dA
��A
�\A
Q�A
(�A	�A	�A	O�A��A��AQ�A=qA=qA-A�A\)A��A^5AM�A1'A-A$�A��A�-AO�A�yA��A�9Ar�AE�A�AA�^A;dA��A�A�9A1'A��A+AoA�A&�A�A �yA ��A 1'@���@��@�E�@�p�@�7L@��@��y@��\@�ff@�X@��F@�x�@�z�@��@��H@�v�@��@�p�@�Z@�t�@���@��T@�/@���@�@��m@��H@�V@�@�X@�z�@�;d@�ȴ@�!@�V@���@�-@�hs@���@��;@��@�5?@��@��`@�|�@�+@�o@��@��@���@ݺ^@ݡ�@�x�@�`B@�%@�Z@�1@�|�@��@ڇ+@�$�@���@٩�@�X@�7L@���@�Z@׾w@�"�@�ȴ@֗�@և+@և+@֏\@�ff@��#@�G�@�r�@���@��@ҏ\@�ff@�=q@�J@�/@�9X@ϝ�@�S�@�@θR@�=q@�7L@��@��/@̃@��;@�+@�~�@�$�@ɲ-@�`B@��@ȋD@��@�33@�@�ȴ@�ȴ@�~�@�ff@ŉ7@�&�@�A�@��m@ÍP@�+@°!@�-@�`B@���@�Z@�b@��m@�ƨ@�l�@���@�V@�-@��@���@��@���@�ƨ@�S�@�33@��@�=q@�hs@��j@�(�@�ƨ@�@��\@�J@�G�@�Ĝ@���@�I�@�ƨ@�33@���@���@�V@��T@���@� �@�t�@��+@���@���@�%@��9@��u@�1'@�"�@���@��!@�n�@�{@�?}@�%@��@���@��@��@��w@�t�@�;d@�+@�@�5?@�@�-@�-@�@��7@��@�Ĝ@��`@��`@���@�9X@�b@��P@��@�ȴ@��!@���@�~�@�M�@���@��T@���@�O�@�/@���@��`@��9@�z�@�9X@�j@��@�j@� �@��F@�S�@�
=@��@��H@��!@�-@��7@��@�9X@���@���@���@�Q�@�^5@��@r-@h�@b�!@X  @Pb@Kƨ@C��@;ƨ@3dZ@+33@"��@�j@�P@Z@��@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
uB
oB
bB
JB
JB
hB
33B
�RB
�B  BE�BgmBw�BffBuB
�yB
ȴB
�B
�fB
��B�B7LBM�BcTB�B��BŢB��B��BB�fB��B��B�B �B&�BC�BQ�BO�BK�BK�BG�B@�B6FB(�B�B�B\BB��B�B�HB�B��BƨB�dB�!B��B�BhsB8RB+B
ǮB
��B
�B
n�B
l�B
M�B
0!B
 �B
�B
B	��B	��B	s�B	D�B	&�B	�B	\B��B�B�B�B�B��B�B�B�B�B�B�yB�fB�sB�B��B��B	B	PB	�B	 �B	$�B	&�B	-B	:^B	L�B	YB	iyB	t�B	�oB	�?B	�dB	��B	ŢB	ȴB	��B	�TB	�B	�B	��B	��B	�B	�`B	�5B	�5B	�5B	�B
B
JB
DB
DB
oB
oB
oB
hB
JB
B	��B	��B	�B	�B
\B
!�B
�B
�B
PB
JB
hB
�B
�B
�B
�B
{B
hB
VB
bB
JB
B	��B
B
\B
\B
\B
hB
oB
oB
hB
hB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
-B
/B
.B
/B
1'B
5?B
7LB
8RB
8RB
7LB
7LB
7LB
7LB
7LB
7LB
6FB
33B
.B
'�B
%�B
%�B
%�B
%�B
%�B
$�B
#�B
#�B
#�B
"�B
"�B
!�B
 �B
#�B
"�B
$�B
#�B
#�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB

=B
+B
%B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
1B
+B
+B
%B
%B
%B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
%B
B
%B
+B
1B
1B
+B
%B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
VB
\B
\B
bB
bB
bB
hB
hB
bB
bB
hB
hB
hB
oB
�B
{B
�B
�B
$�B
0!B
6FB
@�B
E�B
K�B
P�B
W
B
\)B
^5B
e`B
jB
p�B
t�B
u�B
y�B
� B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
`B
fB
]B
fB
dB
^B
`B
fB
^B
`B
^B
`B
^B
`B
`B
^B
^B
YB
RB
VB
OB
MB
NB
KB
IB
IB
KB
KB
OB
NB
NB
OB
OB
NB
HB
DB
6B
B
B
<B
3B
�"B
�VB
��BEtBg=Bw�Bf7BEB
�MB
ȅB
��B
�8B
��BzB7BM�Bc#B��B��B�qB��B��B�B�8B�B��B�B �B&�BCeBQ�BO�BK�BK�BG}B@OB6B(�B�B^B'B�B��B�hB�B��B̙B�tB�-B��B�rB��Bh>B8B�B
�uB
��B
��B
n]B
lSB
M�B
/�B
 �B
NB
�B	ΞB	��B	szB	D^B	&�B	tB	!B��B�BB�uB�RB�]B��B�PB�FB�?B�MB�DB�8B�$B�2B�@B��B��B	�B	B	EB	 �B	$�B	&�B	,�B	:B	L�B	X�B	i5B	tzB	�/B	��B	�"B	�?B	�^B	�nB	͐B	�B	�KB	�sB	�zB	�yB	�tB	�B	��B	��B	��B	�FB
�B
B
B
 B
+B
+B
+B
$B
B
�B	��B	��B	�oB	�lB
B
!�B
sB
JB

B
B
#B
WB
VB
BB
>B
4B
#B
B
B
B
�B	��B
 �B
B
B
B
!B
+B
)B
!B
$B
,B
0B
4B
2B
9B
;B
FB
NB
LB
UB
`B
fB
rB
,�B
.�B
-�B
.�B
0�B
4�B
7B
8	B
8B
7B
7B
7B
7B
7B
7B
6 B
2�B
-�B
'�B
%�B
%�B
%�B
%�B
%�B
$�B
#�B
#�B
#�B
"�B
"�B
!�B
 B
#�B
"�B
$�B
#�B
#�B
"�B
!�B
 |B
sB
lB
kB
qB
pB
xB
oB
kB
vB
 ~B
oB
dB
XB
RB
FB
:B
>B
FB
FB
-B
	�B
�B
�B
�B
�B	��B	��B	��B	��B	��B	�~B	�{B	�tB	�}B	�yB	��B	��B	��B	�B	�yB	�zB	�|B	�uB	�rB	�sB	�tB	�uB	�{B	�yB	�uB	�oB	�jB	�aB	�[B	�]B	�eB	�sB	�nB	�lB	�sB	�sB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�rB	�nB	�hB	�cB	�dB	�eB	�dB	�uB	��B	��B	�{B	�rB	�nB	�oB	�sB	�sB	�uB	�nB	�mB	�^B	�gB	�aB	�`B	�[B	�]B	�TB	�cB	�tB	�uB	�uB	�uB	�rB	�lB	�fB	�bB	�aB	�fB	�hB	�hB	�`B	�fB	�zB	�tB	�zB	�rB	�sB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B

�B

�B
�B
�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%G�O�B
/B
QB
tB
$�B
/�B
5�B
@7B
EXB
KzB
P�B
V�B
[�B
]�B
eB
j1B
pYB
tqB
uzB
y�B
�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.58 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008072019040510080720190405100807  AO  ARCAADJP                                                                    20170904170225    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170904170225  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170904170225  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100807  IP                  G�O�G�O�G�O�                