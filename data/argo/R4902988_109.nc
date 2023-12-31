CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-10-05T06:50:39Z creation;2022-10-05T06:50:39Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20221005065039  20221005070503  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               mA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @���N��1   @���[�ޠ@:`     �c�=p��
1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A���A���A�  A�  A���A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C�fC  C  C�fC  C�C  C  C  C �C"  C#�fC&  C(  C)�fC,  C.  C/�fC2  C4  C6  C8  C:  C<  C>�C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C��3C�  C��C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C��3C�  C�  C��3C�  C��C��C��3C��3C��C��C�  C�  C��3C��3C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3D   D � DfD�fDfD� D  D�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D fD � D!  D!� D"  D"y�D"��D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5�fD6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDXfDX� DY  DY� DZ  DZ�fD[fD[�fD\fD\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db�fDc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dl��Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dry�Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�|�D���D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�|�D���D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�C3D��3D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D D�� D�3D�@ DÀ D�� D�  D�@ DĀ D��3D�  D�@ D�|�D�� D�  D�@ Dƀ D�� D���D�@ Dǃ3D�� D�  D�@ DȀ D��3D�  D�@ Dɀ D�� D���D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�C3D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D��3D�  D�@ Dр D�� D�  D�@ DҀ D�� D���D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�@ Dڀ D�� D���D�@ Dۀ D�� D�  D�@ D܃3D��3D�  D�@ D݀ D�� D���D�@ Dހ D�� D�  D�@ D߃3D��3D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D��3D�3D�@ D� D�� D�3D�C3D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�|�D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�fg@���A ��A ��A@��A`��A�33A�33A�ffA�ffA�33A�ffA�ffA�33B 33B33B33B33B 33B(33B033B833B@33BH��BP33BX33B`33Bh��Bp33Bx33B��B��B��B��B��B��B��B��B��B��B�L�B��B��B��B��B��B��B��B��B��B��B��B��B�L�B��B��B��B��B��B��B��B��C �C�C�C�C�C	�3C�C�3C�C�C�3C�C&gC�C�C�C &gC"�C#�3C&�C(�C)�3C,�C.�C/�3C2�C4�C6�C8�C:�C<�C>&gC@�CB�CC�3CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�3C�fC�fC���C�fC�3C�fC�fC�fC�3C�fC�fC�fC�fC�3C�fC�fC�3C�fC�fC�fC���C�fC�fC���C�fC�3C�3C���C���C�3C�3C�fC�fC���C���C�fC�fC�fC�3C�fC���C���C�fC�fC�fC���C�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC���C���C�fC�fC�fC�3C�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C���C���C���C���D 3D �3D	�D��D	�D�3D3D��D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D��D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D��D�3D3D�3D3D�3D 	�D �3D!3D!�3D"3D"|�D"��D#|�D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+|�D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4��D53D5��D63D6�3D7	�D7�3D83D8�3D93D9�3D:3D:�3D;3D;|�D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB��DC3DC|�DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW��DX	�DX�3DY3DY�3DZ3DZ��D[	�D[��D\	�D\�3D]	�D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db��Dc3Dc|�Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl|�Dl��Dm|�Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq��Dr3Dr|�Ds3Ds��Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx	�Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~gD���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~gD��gD��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~gD���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��gD�>gD�~gD��gD��D�A�D���D���D��D�D�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D��gD��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�D�D���D���D��D�>gD���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�D�D���D���D��D�A�D���D���D��D�A�D���D���D��D�>gD���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~gD��gD��gD�>gD���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D��gD��D�D�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D��gD��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�>gD���D���D��D�A�D���D���D��D�A�D���D���D��D�>gD�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�D�~gD���D��D�A�DƁ�D���D��gD�A�DǄ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��gD�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�D�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��gD�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��gD�A�Dہ�D���D��D�A�D܄�D���D��D�A�D݁�D���D��gD�A�Dށ�D���D��D�A�D߄�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD�gD��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D�gD��D�A�D灚D���D��D�A�D聚D���D��D�D�D遚D���D��D�A�DꁚD���D��D�>gD끚D���D��D�A�D�~gD�gD��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�~gD���D��D�A�D�~gD���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D��g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��DA���A���A���Aت0A�k�A�W�A�M�A�F�A�>�A�;dA�8A�1�A�.�A�+�A�)�A�'�A�$@A� 'A�A�PA׷�A�~�A�oiA�/OA��A�U�A�[�AǶzA��[A�7A�c�A��A�+A���A��8A��xA��"A�Y�A��A�\A���A�~A��A�I�A��A��A�eA��A�&�A�-�A��A�@�A���A�G�A���A��@A�{A��A��2A��hA�kQA��	A��A���A���A�8�A��|A��A� \A��A��/A��A�k�A���A�9$A��cA��"A��KA�MA���A�c�A��A�XEA�cA��oA�dZA��#A�R�A�_A���A�L�A�8RA��rA��A��A���A�`vA��UA��A���A�J�A�%FA~�A{�TAz8�Ay�AAxJAu�eAs�<Ar��ApG�An{Am��Al�TAk�'Aj�AiE�Ae%FAbѷAb1A^MA]H�A\rGAZ��AY��AY�YAYVmAX�+AV��AT�pAR�AP�AO5�AN�$AMe,AL�jALL�AKGAJAIAH�AH��AG�tAF�EAFzAE��AD*�AC8AA��A@��A@}VA@8A?�pA>q�A=�A;�UA:��A:p;A:1�A9��A8�mA7��A6�	A5��A5^5A5A�A4ϫA4�oA4?�A3�0A2�A1g�A0'�A.�1A-~(A-�A,�DA+	lA)9�A(~(A&�ZA&<�A&	A$s�A#�=A#n�A#N�A#�A"��A"w�A!�A 33A�LA5�AA�A(�A	�A�XA�A�AQA��A�}A��AFATaA�xA<�A�A�\AS&A��A�mA��A�]A�A.IA
��A
hsA	+A|�Ae�A�@��X@�)�@��X@�T�@�8�@�6z@�ں@�V�@���@���@�)_@�Xy@��A@��@�j@���@�9�@�4@�<�@퐗@�h
@�y�@�,=@���@�^@�R�@��@�f�@�d�@�G@�Dg@�IR@�'R@�y>@�&�@�_@ץ�@�tT@��W@՚k@Ԟ@�?}@���@��A@��E@͖S@́�@�m]@̴9@�f�@ʣ�@�`B@��@�c@�A�@��K@�҉@�˒@��@���@�,=@���@�33@���@��A@�iD@�+@�A�@�O�@�@@��[@�GE@��)@���@�G�@�L�@��@�rG@���@�ߤ@���@��@�M�@���@�@���@��D@�g�@��@���@��@���@��k@�a�@��@��@��,@���@�C�@�}�@��b@���@���@��M@���@�q@�=q@��@���@�S&@���@��b@�� @�N�@�$t@��.@�h
@�H�@�1�@�	�@���@�6z@���@�c�@��@�W?@�Q@���@�l�@�*0@��p@�l�@���@�qv@�]�@�.I@�q�@�^�@���@�_@���@�]�@���@�q�@�D�@��#@���@�8�@�V�@�Dg@��@�1'@�ƨ@�Mj@��@��@��@��@�@�q@�(�@�/�@�(�@���@��D@�i�@�Ta@�%�@���@���@��m@�خ@�˒@��6@���@���@�33@��e@�h�@��3@���@�|@�4�@��_@�,=@��#@��@�T�@�ں@�6@�IR@�L�@��A@��f@�c @�Q�@�PH@�M�@�O@���@�e,@�>�@�'�@�q@���@�^5@�+k@�$�@�~@�@��@��@���@��@�r�@�H�@�'R@�#:@��@��@�@��@�ԕ@��k@�]�@�/@�ی@�w�@�	@P�@~h
@}w2@}&�@|Ɇ@|I�@{�@{�F@{l�@z��@zYK@zJ@y�#@xy>@wݘ@w�q@w��@wH�@w!-@v�m@v5?@v)�@v
�@ue,@t�P@t֡@t[�@s��@s�	@s�	@s�@r�!@r{�@rxl@rd�@r=q@rO@q�@q�@p��@p �@o��@oE9@n�6@n�1@ns�@m��@ma�@l�@l��@l��@l��@loi@lZ@lK^@l>B@l%�@l�@k�+@k��@kP�@k�@k@k i@j��@j�y@j��@j�@i��@h�f@h$@g��@g_p@ej@c�m@ct�@c+@c�@b�@b��@b�@b��@b}V@bZ�@bM�@b+k@a�@a��@a��@ac�@aDg@a�@`�.@`e�@`[�@`V�@`9X@`%�@_��@_�{@^��@^\�@^($@]�t@]zx@\�@[�$@Z��@Zu@Xl"@X�@X�@W�r@W�]@X�@W�@W�Q@W�@@V�B@V@U��@U��@U�=@U}�@Uk�@UT�@UN<@UA @U(�@U+@U�@U+@T�K@T�@S��@R�"@R�@R=q@R&�@R�@R�@R�@R �@Qϫ@P��@PQ�@O�P@N6�@M�#@M�-@M��@MQ�@M�@L�|@L�@L��@L��@L:�@L �@L�@K��@K@O@K�@J�2@J��@J!�@I��@If�@H��@H�?@H�@G��@F�B@F� @FOv@FO@Ep�@E!�@E@@E@D��@D��@D�|@D�@D��@D�@Dy>@DK^@Db@C�;@C��@C�k@CMj@C�@B҉@Bz@B;�@B($@A@A4@@�@@�9@@�e@@��@@Xy@?��@?��@?��@?��@?F�@?�@?�@?@?@>ߤ@>҉@>�R@>�1@>C�@>J@=�@=��@=c@=-w@<��@<�@<��@<�z@<�I@<��@<l"@<I�@</�@<�@;�Q@;��@;W?@;"�@:ں@:��@:J�@:�@9�H@9�^@9�^@9@9��@9�@8y>@8S�@82�@8�@7�]@7� @7ƨ@7��@7E9@6�@6��@6p;@6Ta@6?@5��@5%@4֡@4�9@4_@4�@3�4@3P�@333@2�@2)�@1�@1�j@1��@1%@0�[@0�j@0�@0Ft@0  @/|�@/C@/o@.�M@.n�@.H�@-��@-�@-hs@-q@,�5@,Ĝ@,�@,q@,~@+�r@+��@+�a@+�@+��@+s@+j�@+Z�@+Mj@++@+/�@+�@*�c@*�<@*��@*:*@)��@)�S@)��@)�@(��@(�e@(��@(j@(C-@((�@(@(	�@'�@'l�@&�}@&GE@%�@%e,@%Dg@$��@$�@$�E@$�@$%�@$�@#�r@#�A@#�Q@#�K@#�F@#�0@#�[@#��@#�@@#��@#��@#��@#\)@#F�@#@O@"��@"��@"L0@"�@!��@!��@!�C@!o @!0�@!*0@!#�@! \@!�@ ��@�@��@�K@�D@I�@,=@7@�]@��@��@�A@�@��@�6@�w@�[@�V@\)@)_@�@��@��@(�@�@��@bN@�@��@F�@'�@&@)_@$t@@"�@!-@o@�"@�B@�m@�<@��@��@c @@�@�9@��@u�@rG@a�@[W@5�@V@�@�j@��@H@�@X�@$t@��@�,@�F@~�@d�@u@��@�@6@�@@��@��@n�@Z�@Q@;�@@�9@��@��@��@w2@^�@IR@2a@��@��@��@��@u�@`�@U2@I�@9X@,=@ݘ@��@��@��@t�@
�@
H�@
.�@	�@	ϫ@	��@	e,@	0�@	%F@	 \@	�@	V@�	@��@�`@�E@�[@Ɇ@��@��@��@��@�@�O@�@z�@H@7�@�@b@�@  @�+@�@��@�6@��@��@s@;d@S@�@ i@��@�2@�R@s�@W�@@�@1�@&�@�@�9@�@��@IR@�@��@��@�Y@j@PH@<�@1'@~@�Q@��@iD@4�@$t@�@��@�c@�2@ں@�B@�@�@s�@\�@W�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��DA���A���A���Aت0A�k�A�W�A�M�A�F�A�>�A�;dA�8A�1�A�.�A�+�A�)�A�'�A�$@A� 'A�A�PA׷�A�~�A�oiA�/OA��A�U�A�[�AǶzA��[A�7A�c�A��A�+A���A��8A��xA��"A�Y�A��A�\A���A�~A��A�I�A��A��A�eA��A�&�A�-�A��A�@�A���A�G�A���A��@A�{A��A��2A��hA�kQA��	A��A���A���A�8�A��|A��A� \A��A��/A��A�k�A���A�9$A��cA��"A��KA�MA���A�c�A��A�XEA�cA��oA�dZA��#A�R�A�_A���A�L�A�8RA��rA��A��A���A�`vA��UA��A���A�J�A�%FA~�A{�TAz8�Ay�AAxJAu�eAs�<Ar��ApG�An{Am��Al�TAk�'Aj�AiE�Ae%FAbѷAb1A^MA]H�A\rGAZ��AY��AY�YAYVmAX�+AV��AT�pAR�AP�AO5�AN�$AMe,AL�jALL�AKGAJAIAH�AH��AG�tAF�EAFzAE��AD*�AC8AA��A@��A@}VA@8A?�pA>q�A=�A;�UA:��A:p;A:1�A9��A8�mA7��A6�	A5��A5^5A5A�A4ϫA4�oA4?�A3�0A2�A1g�A0'�A.�1A-~(A-�A,�DA+	lA)9�A(~(A&�ZA&<�A&	A$s�A#�=A#n�A#N�A#�A"��A"w�A!�A 33A�LA5�AA�A(�A	�A�XA�A�AQA��A�}A��AFATaA�xA<�A�A�\AS&A��A�mA��A�]A�A.IA
��A
hsA	+A|�Ae�A�@��X@�)�@��X@�T�@�8�@�6z@�ں@�V�@���@���@�)_@�Xy@��A@��@�j@���@�9�@�4@�<�@퐗@�h
@�y�@�,=@���@�^@�R�@��@�f�@�d�@�G@�Dg@�IR@�'R@�y>@�&�@�_@ץ�@�tT@��W@՚k@Ԟ@�?}@���@��A@��E@͖S@́�@�m]@̴9@�f�@ʣ�@�`B@��@�c@�A�@��K@�҉@�˒@��@���@�,=@���@�33@���@��A@�iD@�+@�A�@�O�@�@@��[@�GE@��)@���@�G�@�L�@��@�rG@���@�ߤ@���@��@�M�@���@�@���@��D@�g�@��@���@��@���@��k@�a�@��@��@��,@���@�C�@�}�@��b@���@���@��M@���@�q@�=q@��@���@�S&@���@��b@�� @�N�@�$t@��.@�h
@�H�@�1�@�	�@���@�6z@���@�c�@��@�W?@�Q@���@�l�@�*0@��p@�l�@���@�qv@�]�@�.I@�q�@�^�@���@�_@���@�]�@���@�q�@�D�@��#@���@�8�@�V�@�Dg@��@�1'@�ƨ@�Mj@��@��@��@��@�@�q@�(�@�/�@�(�@���@��D@�i�@�Ta@�%�@���@���@��m@�خ@�˒@��6@���@���@�33@��e@�h�@��3@���@�|@�4�@��_@�,=@��#@��@�T�@�ں@�6@�IR@�L�@��A@��f@�c @�Q�@�PH@�M�@�O@���@�e,@�>�@�'�@�q@���@�^5@�+k@�$�@�~@�@��@��@���@��@�r�@�H�@�'R@�#:@��@��@�@��@�ԕ@��k@�]�@�/@�ی@�w�@�	@P�@~h
@}w2@}&�@|Ɇ@|I�@{�@{�F@{l�@z��@zYK@zJ@y�#@xy>@wݘ@w�q@w��@wH�@w!-@v�m@v5?@v)�@v
�@ue,@t�P@t֡@t[�@s��@s�	@s�	@s�@r�!@r{�@rxl@rd�@r=q@rO@q�@q�@p��@p �@o��@oE9@n�6@n�1@ns�@m��@ma�@l�@l��@l��@l��@loi@lZ@lK^@l>B@l%�@l�@k�+@k��@kP�@k�@k@k i@j��@j�y@j��@j�@i��@h�f@h$@g��@g_p@ej@c�m@ct�@c+@c�@b�@b��@b�@b��@b}V@bZ�@bM�@b+k@a�@a��@a��@ac�@aDg@a�@`�.@`e�@`[�@`V�@`9X@`%�@_��@_�{@^��@^\�@^($@]�t@]zx@\�@[�$@Z��@Zu@Xl"@X�@X�@W�r@W�]@X�@W�@W�Q@W�@@V�B@V@U��@U��@U�=@U}�@Uk�@UT�@UN<@UA @U(�@U+@U�@U+@T�K@T�@S��@R�"@R�@R=q@R&�@R�@R�@R�@R �@Qϫ@P��@PQ�@O�P@N6�@M�#@M�-@M��@MQ�@M�@L�|@L�@L��@L��@L:�@L �@L�@K��@K@O@K�@J�2@J��@J!�@I��@If�@H��@H�?@H�@G��@F�B@F� @FOv@FO@Ep�@E!�@E@@E@D��@D��@D�|@D�@D��@D�@Dy>@DK^@Db@C�;@C��@C�k@CMj@C�@B҉@Bz@B;�@B($@A@A4@@�@@�9@@�e@@��@@Xy@?��@?��@?��@?��@?F�@?�@?�@?@?@>ߤ@>҉@>�R@>�1@>C�@>J@=�@=��@=c@=-w@<��@<�@<��@<�z@<�I@<��@<l"@<I�@</�@<�@;�Q@;��@;W?@;"�@:ں@:��@:J�@:�@9�H@9�^@9�^@9@9��@9�@8y>@8S�@82�@8�@7�]@7� @7ƨ@7��@7E9@6�@6��@6p;@6Ta@6?@5��@5%@4֡@4�9@4_@4�@3�4@3P�@333@2�@2)�@1�@1�j@1��@1%@0�[@0�j@0�@0Ft@0  @/|�@/C@/o@.�M@.n�@.H�@-��@-�@-hs@-q@,�5@,Ĝ@,�@,q@,~@+�r@+��@+�a@+�@+��@+s@+j�@+Z�@+Mj@++@+/�@+�@*�c@*�<@*��@*:*@)��@)�S@)��@)�@(��@(�e@(��@(j@(C-@((�@(@(	�@'�@'l�@&�}@&GE@%�@%e,@%Dg@$��@$�@$�E@$�@$%�@$�@#�r@#�A@#�Q@#�K@#�F@#�0@#�[@#��@#�@@#��@#��@#��@#\)@#F�@#@O@"��@"��@"L0@"�@!��@!��@!�C@!o @!0�@!*0@!#�@! \@!�@ ��@�@��@�K@�D@I�@,=@7@�]@��@��@�A@�@��@�6@�w@�[@�V@\)@)_@�@��@��@(�@�@��@bN@�@��@F�@'�@&@)_@$t@@"�@!-@o@�"@�B@�m@�<@��@��@c @@�@�9@��@u�@rG@a�@[W@5�@V@�@�j@��@H@�@X�@$t@��@�,@�F@~�@d�@u@��@�@6@�@@��@��@n�@Z�@Q@;�@@�9@��@��@��@w2@^�@IR@2a@��@��@��@��@u�@`�@U2@I�@9X@,=@ݘ@��@��@��@t�@
�@
H�@
.�@	�@	ϫ@	��@	e,@	0�@	%F@	 \@	�@	V@�	@��@�`@�E@�[@Ɇ@��@��@��@��@�@�O@�@z�@H@7�@�@b@�@  @�+@�@��@�6@��@��@s@;d@S@�@ i@��@�2@�R@s�@W�@@�@1�@&�@�@�9@�@��@IR@�@��@��@�Y@j@PH@<�@1'@~@�Q@��@iD@4�@$t@�@��@�c@�2@ں@�B@�@�@s�@\�@W�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B� B�.B��B��B�B�~B�B�DB��B�	B�RB�1B��B�YB�SB��B�aB��B�;B~wB{B�{BmwBiyBeFB]BFB+BTB�zB��B�SB��B��B��B�eB��B��B�:B��B��B~�Bq�Bh�Ba|B]dBKxBG�BB�B9�B'�B�B�B�qB�B�`B�;B��BیB��BյB��B�[B��B�B�DB��B�9B��Bx�Bk�BgBY�BL0BF�BA�B4nB#�BEB4B	�B�B�B�\B��B͹BƨB��B�XB�B�)B�B�.B��Bp;Be,B_pBV�BK�B:xB4nB%�B�B	RB
��B
��B
�kB
�B
уB
�B
� B
��B
�B
��B
�HB
��B
�TB
~B
k�B
gB
XEB
L�B
H1B
@�B
:�B
8�B
72B
5ZB
,�B
#nB
sB
�B
+B
gB	�BB	��B	��B	�B	��B	�kB	�B	��B	�B	�BB	�)B	ٚB	бB	�pB	�^B	��B	żB	�9B	ĜB	�B	�VB	��B	��B	��B	�B	�B	��B	�B	�$B	�B	�TB	�NB	�|B	��B	��B	��B	�<B	�DB	��B	�B	|B	y>B	u�B	rB	h�B	d�B	a|B	[�B	ZB	X�B	S@B	R�B	R B	Q�B	PbB	N�B	K�B	I7B	B�B	@�B	?�B	>]B	<B	8lB	0�B	0;B	/�B	-�B	+�B	*B	)yB	%`B	"NB	~B	�B	qB	
B	�B	FB	B	�B	�B	�B	�B	�B	
	B	�B	tB��B�B��B��B�B�lB��B��B�8B�LB�2B�B��B��B�?B��B�B�B�;B�B�B�cB�IB�WB��B�B�8B��B�mB�
B��B�B�*B��B��B�[B��B�B�'B�MB��B��B�lB�	B�B�B�BB��B��B�6B�jB�B�B	;B	;B	AB	 4B��B	B	GB	AB	B	3B	�B	9B	KB	�B	�B	�B		�B	6B	\B	 B	�B	�B	CB	�B	�B	# B	&�B	&�B	&�B	(>B	)�B	*�B	.�B	2�B	3�B	4�B	7�B	7�B	9�B	;dB	@�B	CGB	C�B	EB	E�B	E�B	E�B	G�B	J=B	K�B	L�B	Q B	X�B	]B	]�B	_�B	aHB	cnB	h
B	kB	k�B	k�B	mB	r�B	u?B	u�B	v�B	v�B	wfB	xB	z^B	|jB	|�B	~(B	�uB	�+B	��B	�#B	�JB	��B	��B	�2B	��B	�SB	��B	�B	��B	�WB	�5B	�-B	��B	�$B	�*B	��B	�)B	�wB	��B	��B	�B	��B	��B	�%B	�`B	��B	�jB	�B	�HB	��B	ªB	ðB	�B	ňB	�RB	��B	�JB	�~B	�B	��B	бB	өB	�aB	ՁB	�
B	خB	�+B	�qB	�B	�TB	�B	�B	��B	�LB	��B	�B	�B	��B	�CB	�IB	�B	�[B	�3B	�B	��B	��B	�xB	��B	�0B	��B	�B
 �B
uB
GB
B
�B
�B

rB
B
^B
�B
�B
xB
PB
�B
�B
dB
B
!B
VB
�B
 �B
!�B
"4B
#TB
#nB
#�B
%�B
(�B
+kB
,qB
4�B
8�B
9�B
;�B
>]B
@�B
A B
B�B
E�B
I�B
K�B
OB
Q�B
R�B
S�B
T�B
V�B
W�B
Y�B
\�B
]IB
]�B
_�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bhB
c�B
d�B
h�B
j�B
kkB
mwB
o�B
pB
p�B
r�B
t�B
wB
xB
xlB
yrB
z�B
{0B
{�B
{�B
|jB
|�B
}VB
B
��B
�[B
��B
��B
��B
��B
��B
�%B
�EB
�B
�DB
�JB
��B
��B
�1B
�=B
��B
��B
�IB
��B
�B
�jB
�!B
�pB
��B
��B
�vB
�-B
��B
��B
�4B
��B
�&B
��B
��B
��B
��B
�,B
�zB
��B
�yB
�KB
��B
��B
�WB
��B
�vB
��B
�B
�>B
��B
�*B
�^B
�DB
�*B
��B
��B
�JB
�VB
�cB
��B
� B
��B
��B
�B
�;B
� B
�;B
�oB
��B
��B
��B
�[B
��B
��B
�tB
ǮB
ȚB
ȴB
��B
�B
��B
��B
�lB
�B
��B
�B
�:B
��B
�@B
�[B
�B
ԯB
��B
��B
�2B
�B
ևB
ּB
��B
�+B
��B
�B
�B
��B
�=B
�qB
ܬB
��B
��B
�B
�'B
�B
�B
� B
�:B
�FB
��B
��B
��B
�2B
�2B
�2B
�LB
�B
�B
�8B
�B
�$B
�B
��B
��B
�B
��B
�B
�QB
��B
�B
��B
��B
�B
�B
�B
��B
�iB
�!B
�UB
��B
��B
�vB
��B
��B
��B
��B
�GB
�aB
�B
��B
�B
��B
�B
�B
��B
��B
��B
��B
�`B
��B
��B
��B
��B
�LB
�LB
��B
��B
��B
��B
�>B
��B
�DB
��B
�B
��B
��B
��B
�B
�JB
�B
��B
�(B
�BB
�wB
��B
��B
��B
�B
��B OB �B BoB�BB�B�B�BgB9BB%B?B�B1BKBKB�B	�B	�B	�B	�B
rB
�B�B�B�BBB6B�B�BpB�BBBvB�BB�B�B BNBNB�B�B�B�B�B:BB:B�B�B�B�B,BFBB2BgB�B�B�BB9BSBSBmB?BB�BeB�BBkB�B�B�B�B�B�B�BBB)B)BCB]B]BxBCBxB�B�B�BdB�BOB�B�BB!B�B�B�B�B�B�B�B"�B$�B%`B%�B&LB&�B&�B&�B&�B&�B&�B&�B'B'B'B'B'RB'�B'�B(
B(�B)yB*�B*�B+B+kB+�B,=B-B-)B-CB-CB-)B-]B-CB-CB-]B-]B-�B-�B-�B-�B-�B.IB.IB/5B/�B/�B/�B/�B/�B0B0!B0UB0�B0�B1B1�B2aB2�B2�B2�B3hB3MB3�B4TB4�B5tB6FB7B8B8�B8�B8�B8�B8�B9$B9XB9rB9�B9�B9�B9�B:B:B:�B:�B:�B:�B;0B;JB;dB;B;B;�B<B<6B<PB<PB<�B=<B>B>B>�B>wB>�B?HB?cB?}B?�B?�B?�B?�B?�B?�B@ B@ B@B@OB@OB@OB@OB@OB@4B@OB@�B@�B@�BA BA BAUBA;BAUBA;BAoBA�BA�BA�BA�BB[BB�BB�BB�BB�BB�BB�BCaBC{BC�BC�BC�BC�BDBDBD�BD�BE�BE�BE�BE�BFBE�BF%BF?BF?BF�BGBG+BG�BG�BG�BG�BG�BG�BG�BG�BH1BHfBH�BH�BH�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B��B� B�.B��B��B�B�~B�B�DB��B�	B�RB�1B��B�YB�SB��B�aB��B�;B~wB{B�{BmwBiyBeFB]BFB+BTB�zB��B�SB��B��B��B�eB��B��B�:B��B��B~�Bq�Bh�Ba|B]dBKxBG�BB�B9�B'�B�B�B�qB�B�`B�;B��BیB��BյB��B�[B��B�B�DB��B�9B��Bx�Bk�BgBY�BL0BF�BA�B4nB#�BEB4B	�B�B�B�\B��B͹BƨB��B�XB�B�)B�B�.B��Bp;Be,B_pBV�BK�B:xB4nB%�B�B	RB
��B
��B
�kB
�B
уB
�B
� B
��B
�B
��B
�HB
��B
�TB
~B
k�B
gB
XEB
L�B
H1B
@�B
:�B
8�B
72B
5ZB
,�B
#nB
sB
�B
+B
gB	�BB	��B	��B	�B	��B	�kB	�B	��B	�B	�BB	�)B	ٚB	бB	�pB	�^B	��B	żB	�9B	ĜB	�B	�VB	��B	��B	��B	�B	�B	��B	�B	�$B	�B	�TB	�NB	�|B	��B	��B	��B	�<B	�DB	��B	�B	|B	y>B	u�B	rB	h�B	d�B	a|B	[�B	ZB	X�B	S@B	R�B	R B	Q�B	PbB	N�B	K�B	I7B	B�B	@�B	?�B	>]B	<B	8lB	0�B	0;B	/�B	-�B	+�B	*B	)yB	%`B	"NB	~B	�B	qB	
B	�B	FB	B	�B	�B	�B	�B	�B	
	B	�B	tB��B�B��B��B�B�lB��B��B�8B�LB�2B�B��B��B�?B��B�B�B�;B�B�B�cB�IB�WB��B�B�8B��B�mB�
B��B�B�*B��B��B�[B��B�B�'B�MB��B��B�lB�	B�B�B�BB��B��B�6B�jB�B�B	;B	;B	AB	 4B��B	B	GB	AB	B	3B	�B	9B	KB	�B	�B	�B		�B	6B	\B	 B	�B	�B	CB	�B	�B	# B	&�B	&�B	&�B	(>B	)�B	*�B	.�B	2�B	3�B	4�B	7�B	7�B	9�B	;dB	@�B	CGB	C�B	EB	E�B	E�B	E�B	G�B	J=B	K�B	L�B	Q B	X�B	]B	]�B	_�B	aHB	cnB	h
B	kB	k�B	k�B	mB	r�B	u?B	u�B	v�B	v�B	wfB	xB	z^B	|jB	|�B	~(B	�uB	�+B	��B	�#B	�JB	��B	��B	�2B	��B	�SB	��B	�B	��B	�WB	�5B	�-B	��B	�$B	�*B	��B	�)B	�wB	��B	��B	�B	��B	��B	�%B	�`B	��B	�jB	�B	�HB	��B	ªB	ðB	�B	ňB	�RB	��B	�JB	�~B	�B	��B	бB	өB	�aB	ՁB	�
B	خB	�+B	�qB	�B	�TB	�B	�B	��B	�LB	��B	�B	�B	��B	�CB	�IB	�B	�[B	�3B	�B	��B	��B	�xB	��B	�0B	��B	�B
 �B
uB
GB
B
�B
�B

rB
B
^B
�B
�B
xB
PB
�B
�B
dB
B
!B
VB
�B
 �B
!�B
"4B
#TB
#nB
#�B
%�B
(�B
+kB
,qB
4�B
8�B
9�B
;�B
>]B
@�B
A B
B�B
E�B
I�B
K�B
OB
Q�B
R�B
S�B
T�B
V�B
W�B
Y�B
\�B
]IB
]�B
_�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bhB
c�B
d�B
h�B
j�B
kkB
mwB
o�B
pB
p�B
r�B
t�B
wB
xB
xlB
yrB
z�B
{0B
{�B
{�B
|jB
|�B
}VB
B
��B
�[B
��B
��B
��B
��B
��B
�%B
�EB
�B
�DB
�JB
��B
��B
�1B
�=B
��B
��B
�IB
��B
�B
�jB
�!B
�pB
��B
��B
�vB
�-B
��B
��B
�4B
��B
�&B
��B
��B
��B
��B
�,B
�zB
��B
�yB
�KB
��B
��B
�WB
��B
�vB
��B
�B
�>B
��B
�*B
�^B
�DB
�*B
��B
��B
�JB
�VB
�cB
��B
� B
��B
��B
�B
�;B
� B
�;B
�oB
��B
��B
��B
�[B
��B
��B
�tB
ǮB
ȚB
ȴB
��B
�B
��B
��B
�lB
�B
��B
�B
�:B
��B
�@B
�[B
�B
ԯB
��B
��B
�2B
�B
ևB
ּB
��B
�+B
��B
�B
�B
��B
�=B
�qB
ܬB
��B
��B
�B
�'B
�B
�B
� B
�:B
�FB
��B
��B
��B
�2B
�2B
�2B
�LB
�B
�B
�8B
�B
�$B
�B
��B
��B
�B
��B
�B
�QB
��B
�B
��B
��B
�B
�B
�B
��B
�iB
�!B
�UB
��B
��B
�vB
��B
��B
��B
��B
�GB
�aB
�B
��B
�B
��B
�B
�B
��B
��B
��B
��B
�`B
��B
��B
��B
��B
�LB
�LB
��B
��B
��B
��B
�>B
��B
�DB
��B
�B
��B
��B
��B
�B
�JB
�B
��B
�(B
�BB
�wB
��B
��B
��B
�B
��B OB �B BoB�BB�B�B�BgB9BB%B?B�B1BKBKB�B	�B	�B	�B	�B
rB
�B�B�B�BBB6B�B�BpB�BBBvB�BB�B�B BNBNB�B�B�B�B�B:BB:B�B�B�B�B,BFBB2BgB�B�B�BB9BSBSBmB?BB�BeB�BBkB�B�B�B�B�B�B�BBB)B)BCB]B]BxBCBxB�B�B�BdB�BOB�B�BB!B�B�B�B�B�B�B�B"�B$�B%`B%�B&LB&�B&�B&�B&�B&�B&�B&�B'B'B'B'B'RB'�B'�B(
B(�B)yB*�B*�B+B+kB+�B,=B-B-)B-CB-CB-)B-]B-CB-CB-]B-]B-�B-�B-�B-�B-�B.IB.IB/5B/�B/�B/�B/�B/�B0B0!B0UB0�B0�B1B1�B2aB2�B2�B2�B3hB3MB3�B4TB4�B5tB6FB7B8B8�B8�B8�B8�B8�B9$B9XB9rB9�B9�B9�B9�B:B:B:�B:�B:�B:�B;0B;JB;dB;B;B;�B<B<6B<PB<PB<�B=<B>B>B>�B>wB>�B?HB?cB?}B?�B?�B?�B?�B?�B?�B@ B@ B@B@OB@OB@OB@OB@OB@4B@OB@�B@�B@�BA BA BAUBA;BAUBA;BAoBA�BA�BA�BA�BB[BB�BB�BB�BB�BB�BB�BCaBC{BC�BC�BC�BC�BDBDBD�BD�BE�BE�BE�BE�BFBE�BF%BF?BF?BF�BGBG+BG�BG�BG�BG�BG�BG�BG�BG�BH1BHfBH�BH�BH�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221005065030  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221005065039  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221005065039  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221005065039                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221005155044  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221005155044  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20221005070503                      G�O�G�O�G�O�                