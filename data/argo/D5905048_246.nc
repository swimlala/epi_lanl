CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-01T00:35:41Z creation;2018-06-01T00:35:45Z conversion to V3.1;2019-12-19T07:36:59Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        x  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `L   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ̜   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180601003541  20200116231515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_246                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�f���}�1   @�f��[�@4;�l�C��dO+��a1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�C3DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D���D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @���@���A   A>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BH  BP  BW��B_��Bg33Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D� D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da�3Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�9�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�@ D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D�� D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�S�A�?}A�5?A�1'A� �A�$�A��A��A��A�oA�oA�bA�VA�
=A�A�A�A�A���A���A���A���A��A��A��`A���Aɡ�A�VA�=qA��/Aã�AÅA��A�?}A��mA�&�A���A�(�A��A�"�A�^5A���A�33A�t�A�E�A��TA��hA�G�A���A�l�A�p�A��wA��-A�?}A��wA���A�A�A�K�A�+A���A���A�r�A���A�5?A���A�=qA�jA���A�K�A��hA�&�A�+A�dZA��FA�G�A��`A��A�?}A���A�r�A��9A��A���A��A���A�`BA�^5A��yA�I�A���A��A�9XA��A�hsA�1'A���A�(�A��
A�E�A�hsA���A}��A{��Az�\Ay�-AyO�Ay�Ax��Aw�At-As\)Ar�!Ar9XAq��ApI�Am��Ak��Ai�#AgS�Af�HAe�;Ac�;Ab�uAb9XA`I�A_O�A^��A\��A[�wAZ{AX�DAV�uATAS�AR��APVAM�mAK��AIƨAG�hAF �AE�AC`BAA�mA@^5A>z�A:v�A8��A7�7A6^5A4��A3�mA3�^A3XA1��A0r�A/��A.�A.bNA-`BA,�9A+��A*�A*��A*�A)��A(��A(�A(�A(�uA(ZA'��A&�!A&  A$�A#t�A#C�A"E�A (�A�A�\A��A�!AbA�-A��A(�A�;A�jA&�A�mAjA(�A��A\)AĜA�A�^Al�A�yA��A�A7LAdZA
�A	��A	?}A	33A	?}A	?}AĜA�HA	A��A�A�#A�A�^A�mA\)A1'A&�A ff@�ƨ@�@�$�@�p�@�b@���@�v�@�Q�@�?}@��@���@�S�@�X@�1@ꟾ@��@���@��;@�n�@�r�@�  @�F@�@�@��@�j@�z�@�A�@�  @ߝ�@�E�@��/@�1@���@���@�&�@���@أ�@�bN@�A�@֧�@�p�@ӝ�@���@���@Ь@�Q�@��@�
=@�$�@��@�Z@��;@�S�@��T@�I�@���@�C�@��y@�=q@�X@�/@���@ă@��
@�"�@�M�@��7@��`@���@���@���@��h@��7@�hs@�7L@�9X@�;d@�M�@�G�@��@���@�+@��+@��T@��`@�Q�@���@�S�@���@��@�7L@���@���@��j@�bN@���@���@�o@�ff@�=q@���@��@��@��@���@��F@�;d@��@��R@�@���@�X@�Ĝ@�z�@�r�@�r�@�1'@��m@��@�S�@�n�@���@��T@���@��-@��-@��@�G�@��@���@�1'@���@�\)@�S�@�+@���@���@��+@�~�@�M�@��@��#@��#@���@��-@��h@�p�@�/@��@���@�j@�1'@��
@��@�K�@��@���@��\@��-@��7@�7L@�&�@���@�Ĝ@��@�j@�I�@�1'@�  @���@�33@���@�M�@���@���@���@�@��@���@��@�I�@��@�|�@�\)@�+@��y@���@��R@�E�@���@��h@�hs@�&�@��@��/@���@�Ĝ@���@�j@�A�@�1'@�  @���@���@��@�t�@�K�@��@��y@��\@�M�@�{@��T@��-@��7@�`B@�7L@���@��@��D@�Q�@��@���@��;@��F@���@�|�@�S�@�K�@�;d@��@�M�@��#@��^@���@�/@�%@���@�%@�%@��`@��9@���@��@�Ĝ@�bN@�b@�  @�ƨ@�;d@��@��@�v�@��@��^@�G�@��@�%@��/@��9@�1'@���@��@��
@��w@��P@�"�@���@���@��!@���@���@��+@���@��\@��@��#@��-@���@��-@���@�O�@��u@�j@�b@��m@��;@��
@��w@�|�@�S�@�C�@�33@�
=@��y@���@�M�@��@��@���@�@��7@�`B@�/@���@��j@��@��u@�I�@�@l�@~�y@~E�@}�-@}V@|j@|9X@{��@{C�@z��@z��@z�\@zn�@zJ@y��@y��@yx�@y7L@y%@x��@x��@xbN@x1'@w��@wK�@v��@vȴ@v��@vv�@v{@u�@u�@t�/@tj@tI�@t�@sdZ@rn�@r�@rJ@q��@p��@o��@o�@oK�@nȴ@n@m��@mp�@m/@l�@l�D@lj@l�@k��@k@j�@i��@i�@i��@i��@iX@i&�@h�@g��@g
=@fff@e�h@e/@d��@d�@d��@dZ@dZ@c�
@c"�@b=q@a��@a��@a�@`��@`A�@`  @_��@_|�@_;d@_�@^��@^@]�-@]�@]p�@]p�@]p�@]�@\��@\Z@\(�@\�@[�m@[dZ@Z�!@Y��@Y��@Yhs@YG�@Y7L@Y%@X�9@XA�@X �@W�@V��@Vȴ@V�R@VE�@U�-@UO�@T�/@T��@T�j@TI�@T�@S��@S�F@S��@So@R��@R��@R�\@Rn�@R^5@R-@Q�@QG�@Q�@P�`@P��@P�u@PbN@PA�@Pb@O�@Ol�@O;d@O�@N��@Nv�@NV@N{@M�h@M?}@L�/@L��@Lj@L1@K�
@K��@KC�@Ko@J�H@J��@J��@Jn�@J=q@I��@I��@I�7@I%@H�@G�w@G|�@G+@FV@E�T@E�-@E��@E�h@EO�@EV@D�/@Dz�@C��@C33@Co@B�!@B�@A�@A��@AX@A&�@@Ĝ@@��@@�u@@b@?�P@>ȴ@>$�@=�-@=p�@=?}@=�@<�/@<��@<z�@<1@;�@;dZ@;"�@:�@:��@:�\@:M�@:=q@:�@:�@9��@9x�@8Ĝ@8A�@8  @7�P@6ȴ@6ff@6{@5��@5�-@5�-@5��@5`B@5/@4��@4�/@4�@4z�@49X@41@3��@3dZ@3C�@3@2�!@2�!@2n�@2=q@2-@1�@1��@1�7@1hs@1&�@0��@0��@0bN@0 �@0  @/�@/�w@/��@/�P@/|�@/\)@.��@.�@.�R@.5?@.@-�T@-��@-�@-�@,�@,�j@,�D@+��@+�@*�@*�\@*n�@*^5@*=q@*-@)�@)hs@)�@(��@(�u@(Q�@(  @'��@'�@'�P@'|�@'l�@';d@'�@&�@&��@&��@&��@&5?@%��@%��@%O�@$��@$�D@$9X@#�m@#��@#dZ@#C�@"�@"��@"�\@"=q@!��@!�^@!hs@!&�@ ��@ �9@ �@ bN@ bN@ A�@  �@�@�@�P@�@ȴ@��@ff@�@�T@��@@p�@O�@/@��@��@9X@�m@ƨ@dZ@C�@@��@�!@�\@�\@n�@=q@��@�@�#@��@G�@��@Q�@  @�;@�;@�@l�@;d@�@
=@�@��@$�@{@�@�@�@�T@��@�h@`B@?}@V@�@�/@�@z�@I�@9X@(�@1@�F@��@dZ@dZ@S�@C�@33@33@��@^5@-@�@��@�#@��@�^@�^@��@��@�7@�7@�7@hs@X@G�@%@�`@Ĝ@�9@�u@bN@1'@�@�@�@�;@��@�@�P@\)@+@
=@�y@��@�y@�@��@�+@E�@5?@$�@$�@{@�T@@�@`B@O�@?}@�@�/@�j@��@�D@�D@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�S�A�?}A�5?A�1'A� �A�$�A��A��A��A�oA�oA�bA�VA�
=A�A�A�A�A���A���A���A���A��A��A��`A���Aɡ�A�VA�=qA��/Aã�AÅA��A�?}A��mA�&�A���A�(�A��A�"�A�^5A���A�33A�t�A�E�A��TA��hA�G�A���A�l�A�p�A��wA��-A�?}A��wA���A�A�A�K�A�+A���A���A�r�A���A�5?A���A�=qA�jA���A�K�A��hA�&�A�+A�dZA��FA�G�A��`A��A�?}A���A�r�A��9A��A���A��A���A�`BA�^5A��yA�I�A���A��A�9XA��A�hsA�1'A���A�(�G�O�G�O�A�hsA���A}��A{��Az�\Ay�-AyO�Ay�Ax��Aw�At-As\)Ar�!Ar9XAq��ApI�Am��Ak��Ai�#AgS�Af�HAe�;Ac�;Ab�uAb9XA`I�A_O�A^��A\��A[�wAZ{AX�DAV�uATAS�AR��APVAM�mAK��AIƨAG�hAF �AE�AC`BAA�mA@^5A>z�A:v�A8��A7�7A6^5A4��A3�mA3�^A3XA1��A0r�A/��A.�A.bNA-`BA,�9A+��A*�A*��A*�A)��A(��A(�A(�A(�uA(ZA'��A&�!A&  A$�A#t�A#C�A"E�A (�A�A�\A��A�!AbA�-A��A(�A�;A�jA&�A�mAjA(�A��A\)AĜA�A�^Al�A�yA��A�A7LAdZA
�A	��A	?}A	33A	?}A	?}AĜA�HA	A��A�A�#A�A�^A�mA\)A1'A&�A ff@�ƨ@�@�$�@�p�@�b@���@�v�@�Q�@�?}@��@���@�S�@�X@�1@ꟾ@��@���@��;@�n�@�r�@�  @�F@�@�@��@�j@�z�@�A�@�  @ߝ�@�E�@��/@�1@���@���@�&�@���@أ�@�bN@�A�@֧�@�p�@ӝ�@���@���@Ь@�Q�@��@�
=@�$�@��@�Z@��;@�S�@��T@�I�@���@�C�@��y@�=q@�X@�/@���@ă@��
@�"�@�M�@��7@��`@���@���@���@��h@��7@�hs@�7L@�9X@�;d@�M�@�G�@��@���@�+@��+@��T@��`@�Q�@���@�S�@���@��@�7L@���@���@��j@�bN@���@���@�o@�ff@�=q@���@��@��@��@���@��F@�;d@��@��R@�@���@�X@�Ĝ@�z�@�r�@�r�@�1'@��m@��@�S�@�n�@���@��T@���@��-@��-@��@�G�@��@���@�1'@���@�\)@�S�@�+@���@���@��+@�~�@�M�@��@��#@��#@���@��-@��h@�p�@�/@��@���@�j@�1'@��
@��@�K�@��@���@��\@��-@��7@�7L@�&�@���@�Ĝ@��@�j@�I�@�1'@�  @���@�33@���@�M�@���@���@���@�@��@���@��@�I�@��@�|�@�\)@�+@��y@���@��R@�E�@���@��h@�hs@�&�@��@��/@���@�Ĝ@���@�j@�A�@�1'@�  @���@���@��@�t�@�K�@��@��y@��\@�M�@�{@��T@��-@��7@�`B@�7L@���@��@��D@�Q�@��@���@��;@��F@���@�|�@�S�@�K�@�;d@��@�M�@��#@��^@���@�/@�%@���@�%@�%@��`@��9@���@��@�Ĝ@�bN@�b@�  @�ƨ@�;d@��@��@�v�@��@��^@�G�@��@�%@��/@��9@�1'@���@��@��
@��w@��P@�"�@���@���@��!@���@���@��+@���@��\@��@��#@��-@���@��-G�O�G�O�@��u@�j@�b@��m@��;@��
@��w@�|�@�S�@�C�@�33@�
=@��yG�O�@�M�@��@��@���@�@��7@�`B@�/@���@��j@��G�O�@�I�@�@l�@~�y@~E�@}�-@}V@|j@|9X@{��@{C�@z��@z��@z�\@zn�@zJ@y��@y��@yx�@y7L@y%@x��@x��@xbN@x1'@w��@wK�@v��@vȴ@v��@vv�@v{@u�@u�@t�/@tj@tI�@t�@sdZ@rn�@r�@rJG�O�@p��@o��@o�@oK�@nȴ@n@m��@mp�@m/@l�@l�D@lj@l�@k��@k@j�@i��@i�@i��@i��@iXG�O�@h�@g��@g
=@fff@e�h@e/@d��@d�@d��@dZG�O�@c�
@c"�@b=q@a��@a��@a�@`��@`A�@`  @_��@_|�@_;d@_�@^��@^@]�-@]�@]p�@]p�@]p�@]�@\��@\Z@\(�@\�@[�m@[dZ@Z�!@Y��@Y��@Yhs@YG�@Y7L@Y%@X�9@XA�@X �@W�@V��@Vȴ@V�R@VE�@U�-@UO�@T�/@T��@T�j@TI�@T�@S��@S�F@S��@So@R��@R��@R�\@Rn�@R^5@R-@Q�@QG�@Q�@P�`@P��@P�u@PbN@PA�@Pb@O�@Ol�@O;d@O�@N��@Nv�@NV@N{@M�h@M?}@L�/@L��@Lj@L1@K�
@K��@KC�@Ko@J�H@J��@J��@Jn�@J=q@I��@I��@I�7@I%@H�@G�w@G|�G�O�@FV@E�T@E�-@E��@E�h@EO�@EV@D�/@Dz�@C��@C33@Co@B�!@B�@A�@A��@AX@A&�@@Ĝ@@��@@�u@@b@?�P@>ȴ@>$�@=�-@=p�@=?}@=�@<�/@<��@<z�@<1@;�@;dZ@;"�@:�@:��@:�\@:M�@:=q@:�@:�@9��@9x�@8Ĝ@8A�@8  @7�P@6ȴ@6ff@6{@5��@5�-@5�-@5��@5`B@5/@4��@4�/@4�@4z�@49X@41@3��@3dZ@3C�@3@2�!@2�!@2n�@2=q@2-@1�@1��@1�7@1hs@1&�@0��@0��@0bN@0 �@0  @/�@/�w@/��@/�P@/|�@/\)@.��@.�@.�R@.5?@.@-�T@-��@-�@-�@,�@,�j@,�D@+��@+�@*�@*�\@*n�@*^5@*=q@*-@)�@)hs@)�@(��@(�u@(Q�@(  @'��@'�@'�P@'|�@'l�@';d@'�@&�@&��@&��@&��@&5?@%��@%��@%O�@$��@$�D@$9X@#�m@#��@#dZ@#C�@"�@"��@"�\@"=q@!��@!�^@!hs@!&�@ ��@ �9@ �@ bN@ bN@ A�@  �@�@�@�P@�@ȴ@��@ff@�@�T@��@@p�@O�@/@��@��@9X@�m@ƨ@dZ@C�@@��@�!@�\@�\@n�@=q@��@�@�#@��@G�@��@Q�@  @�;@�;@�@l�@;d@�@
=@�@��@$�@{@�@�@�@�T@��@�h@`B@?}@V@�@�/@�@z�@I�@9X@(�@1@�F@��@dZ@dZ@S�@C�@33@33@��@^5@-@�@��@�#@��@�^@�^@��@��@�7@�7@�7@hs@X@G�@%@�`@Ĝ@�9@�u@bN@1'@�@�@�@�;@��@�@�P@\)@+@
=@�y@��@�y@�@��@�+@E�@5?@$�@$�@{@�T@@�@`B@O�@?}@�@�/@�j@��@�D@�D@�D111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111131144131111111111111111111111111111111113111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111411111111111411111111111111111111111111111111111111111141111111111111111111114111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
�B
�B
!�B
 �B
 �B
 �B
 �B
!�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
(�B
��B
�B{�B�jB��B��BǮB�NB�BB�B+B,BJ�BK�BD�B6FB;dB>wBD�BG�B<jB0!B�B�mB�)B��B��B��B1BK�Bu�BffBm�BZBC�BE�B=qB1'B<jBA�B:^B49B,B	7B�mB�B�/BȴB��B��BǮB��B��BȴB�FB��B�bBt�BK�BD�B,B+BuB
��B
�B
��B
ŢB
��B
o�B
l�B
VB

=B	�B	��B	�B	�qB	�wB	ÖB	ƨB	��B	�B	��B	�NB	�NB	�/B	��B	B	��B	��B	��B	� B	�{B	�7B	t�B	x�B	}�B	k�B	e`B	dZB	R�B	N�B	D�B	:^B	-B	 �B	.B	&�B		7B��B�B�yB�HB�BB�BB�B��BȴB�}B��B��B��B��B��B�B�!B��B�uB�uB��B�{B��B�VB�\B�JB�JB�uB�hB�bB�=B�{B�oB�hB�DB�Bv�Bx�Bt�Bk�Bz�Bo�B[#Bt�BgmBm�Bo�Bo�Bq�BgmBiyBhsB]/BO�BZBT�Bk�BiyBgmBe`B`BBk�Bk�Be`B\)BN�B\)BO�B^5BYBbNBp�Br�Bw�Bq�B~�B~�B}�Bw�Bp�BiyB_;BVBcTB\)B\)BbNBdZBgmBe`BgmB`BBffB^5BQ�BL�BJ�BXBN�BW
B^5BaHBcTBe`BcTB`BB^5Bo�Bo�Bm�Bk�Bm�Bq�Br�Br�Bp�Bl�Be`BdZBk�BjBl�Bp�Bu�Bv�Bt�Bq�BhsBjBk�Bu�Bw�Bx�B�B�B|�Bz�B}�B�B�%B�B�B�B�\B�bB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B�XB�^B�^B�XB�LB�9B�FB�qB��BǮB��B��B��B�B�B�5B�HB�ZB�mB�B��B��B��B	  B	  B	B	%B	1B	
=B	bB	\B	hB	�B	�B	"�B	#�B	"�B	&�B	%�B	&�B	-B	1'B	49B	:^B	>wB	?}B	?}B	B�B	C�B	D�B	C�B	I�B	P�B	P�B	P�B	Q�B	S�B	XB	XB	XB	W
B	[#B	`BB	cTB	cTB	dZB	ffB	m�B	m�B	l�B	m�B	o�B	t�B	t�B	w�B	x�B	z�B	z�B	}�B	}�B	}�B	� B	� B	�B	�B	�B	�=B	�JB	�7B	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�'B	�B	�B	�B	��B	�B	�'B	�-B	�LB	�LB	�RB	�^B	�^B	�RB	�RB	��B	��B	��B	B	ĜB	ŢB	ŢB	ĜB	ŢB	ƨB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�)B	�/B	�5B	�BB	�HB	�HB	�NB	�TB	�NB	�TB	�ZB	�HB	�BB	�ZB	�yB	�yB	�mB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
%B
%B
%B
+B
+B
%B
%B
+B
1B
1B
+B
B
  B
%B
+B
1B
	7B
	7B
	7B
1B

=B
DB
DB
DB
DB
DB
	7B
JB
PB
VB
VB
PB
PB
VB
VB
\B
bB
bB
PB
PB
PB
\B
\B
bB
bB
bB
uB
uB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
�B
�B
"�B
#�B
"�B
 �B
 �B
%�B
$�B
$�B
$�B
'�B
(�B
(�B
(�B
)�B
)�B
(�B
'�B
'�B
'�B
,B
,B
+B
+B
)�B
+B
(�B
&�B
)�B
)�B
)�B
,B
-B
.B
-B
-B
-B
+B
)�B
+B
.B
.B
-B
.B
/B
0!B
0!B
1'B
0!B
1'B
0!B
/B
2-B
33B
33B
33B
33B
2-B
2-B
2-B
33B
49B
33B
2-B
2-B
2-B
5?B
6FB
6FB
6FB
6FB
5?B
5?B
6FB
6FB
5?B
7LB
8RB
7LB
7LB
8RB
9XB
:^B
:^B
9XB
:^B
;dB
:^B
;dB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
=qB
>wB
>wB
>wB
=qB
?}B
>wB
?}B
@�B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
A�B
A�B
A�B
A�B
A�B
@�B
@�B
?}B
A�B
A�B
?}B
A�B
C�B
C�B
C�B
B�B
B�B
C�B
C�B
B�B
D�B
E�B
D�B
C�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
E�B
E�B
E�B
F�B
G�B
H�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
K�B
K�B
K�B
L�B
K�B
L�B
M�B
M�B
M�B
L�B
K�B
K�B
L�B
M�B
L�B
L�B
M�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
R�B
R�B
S�B
S�B
S�B
T�B
S�B
S�B
T�B
VB
T�B
VB
VB
VB
W
B
W
B
XB
XB
W
B
XB
XB
XB
W
B
XB
XB
W
B
XB
YB
YB
YB
XB
YB
YB
YB
XB
YB
YB
ZB
\)B
\)B
\)B
\)B
[#B
[#B
\)B
]/B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
^5B
^5B
^5B
_;B
_;B
_;B
^5B
^5B
_;B
_;B
^5B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
dZB
dZB
e`B
e`B
dZB
e`B
ffB
e`B
e`B
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
iyB
iyB
k�B
jB
jB
iyB
iyB
iyB
jB
l�B
l�B
l�B
l�B
m�B
m�B
n�B
m�B
m�B
m�B
o�B
o�B
o�B
p�B
o�B
o�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
p�B
o�B
o�B
q�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
u�B
v�B
v�B
v�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
�B
�B
!�B
 �B
 �B
 �B
 �B
!�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
 �B
 �B
 �B
 �B
 �B
 B
�B
�B
.�B
�mB
�B}�B��B�B��B�RB�TB�5BSB�B+�B.BLdBM�BG+B7�B;�B?cBESBH�B=�B1�BjB�B�OB��B OB�B	BK�Bv`Bi�Bo�B]IBF�BG�B@iB4�B>BBB�B<B5�B-�B�B�B�|B�vBˬB��B�dB�B��BΥBʌB�XB��B��By�BP}BG_B-wB,"B�B
��B
�CB
�~B
��B
��B
t9B
o�G�O�G�O�B	��B	�B	�;B	��B	��B	�B	�EB	οB	�B	�mB	�:B	� B	��B	�B	ĜB	��B	�B	��B	�B	�gB	��B	w2B	z^B	~�B	m�B	f�B	e`B	UMB	P�B	F�B	<�B	/�B	#�B	/B	(XB	�B	 B�vB�B��B�B��B�QB�B�DB�[B�7B�B��B��B��B�B��B��B��B��B��B��B��B��B�}B��B�PB�B�:B�B�^B��B��B��B��B��Bx�BzBv`BmwB{dBq[B]~BuZBiyBn�BqBp�BraBh�BjKBiDB^�BR:B[�BV�Bk�BjBh>BfLBabBlBl"Bf�B^BQB]IBRTB_!BZ�Bc Bp�Br�BxBrGB~�B.B~]Bx�Bq�Bj�Ba-BX_BdtB]�B]�BcTBeBh$BfBh
BabBgB_pBS�BN�BLJBX�BP�BXyB_;BbhBdBfBd&BabB_�Bo�Bp!BnIBlWBnIBrBr�BsBp�BmBf�BezBlWBkQBm]Bq[BvBwBuBr-Bi�Bk�Bl�Bv`Bx�By�B�UB�[B}�B{�B~�B��B��B��B�'B�'B��B� B��B�,B�B��B�B�=B�7B�WB�xB�VB�hB��B� B�XB�xB��B��B��B�B�2B�(B�AB�KB�jBΊB�uBؓB��BޞB��B��B�$B�!B�`B�6B�HB	 4B	 OB	uB	tB	�B	
�B	�B	�B	 B	B	/B	#B	$B	#TB	'8B	&fB	'mB	-]B	1vB	4�B	:�B	>�B	?�B	?�B	B�B	C�B	EB	DMB	J	B	P�B	Q B	Q B	R B	T,B	XEB	XEB	XyB	W�B	[�B	`vB	c�B	c�B	d�B	f�B	m�B	m�B	l�B	m�B	o�B	t�B	t�B	xB	y	B	z�B	{B	~(B	~BB	~]B	�4B	�OB	�GB	�SB	�mB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�B	�bB	�2B	��B	�AB	�wB	�iB	��B	��B	�cB	�vB	��B	��B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�BB	� B	�&B	�B	�2B	�?B	�EB	�_B	�KB	�B	�CB	�dB	�jB	�vB	�bB	�bB	�hB	�B	�B	�B	�tB	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�	B	�>B	�B	��B	�B	�B	�BB	�VB	�]B
B
?B
?B
?B
YB
+B
_B
�B
YB
EB
KB
fB
�G�O�G�O�B
YB
_B
fB
	RB
	RB
	lB
fB

XB
xB
xB
^B
�B
�G�O�B
�B
�B
pB
pB
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
 �B
 B
!�B
 �B
 B
 B
#B
#�B
# G�O�B
!HB
%�B
%B
%,B
%FB
($B
)B
)B
)DB
*0B
*0B
)*B
(>B
(>B
(XB
,B
,"B
+B
+B
*KB
+6G�O�B
'mB
*KB
*eB
*KB
,=B
-CB
./B
-)B
-CB
-CG�O�B
*eB
+kB
.IB
.IB
-CB
.cB
/OB
0UB
0UB
1[B
0;B
1[B
0UB
/iB
2aB
3hB
3MB
33B
3MB
2aB
2GB
2|B
3hB
4TB
3MB
2|B
2�B
2|B
5ZB
6zB
6zB
6zB
6`B
5ZB
5tB
6zB
6zB
5�B
7fB
8lB
7�B
7�B
8�B
9�B
:^B
:�B
9�B
:xB
;B
:�B
;B
:�B
;B
<�B
<�B
<�B
<�B
<�B
;�B
;�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
=�B
>�B
>�B
>�B
=�B
?�B
>�B
?�B
@�B
?�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
A�B
A�B
A�B
A�B
A�B
@�B
@�B
?�B
A�B
A�G�O�B
A�B
C�B
C�B
C�B
B�B
B�B
C�B
C�B
B�B
D�B
E�B
D�B
C�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
E�B
E�B
E�B
F�B
G�B
H�B
I�B
I�B
I�B
I�B
J�B
I�B
J	B
K�B
K�B
K�B
L�B
K�B
L�B
M�B
M�B
M�B
MB
K�B
LB
MB
M�B
MB
M6B
NB
N�B
O�B
Q B
P�B
QB
Q B
Q B
R B
R B
RB
RB
RB
SB
S&B
S&B
TB
S&B
S&B
TB
T,B
TB
T�B
T,B
T,B
UB
VB
UB
VB
V9B
V9B
W$B
W?B
XEB
X+B
W?B
X+B
X+B
XEB
W?B
X+B
X+B
W?B
XEB
Y1B
YKB
Y1B
XEB
YKB
YKB
YKB
XEB
YKB
YKB
ZQB
\CB
\CB
\CB
\]B
[=B
[WB
\]B
]dB
\]B
]IB
]IB
^OB
^OB
^jB
^OB
_;B
^jB
^OB
^OB
_pB
_VB
_pB
^jB
^OB
_pB
_pB
^jB
`vB
`\B
`vB
`\B
a|B
a|B
abB
bhB
b�B
a|B
b�B
b�B
bhB
c�B
cnB
cnB
d�B
dtB
e`B
e�B
dtB
d�B
e�B
ezB
d�B
e�B
f�B
ezB
ezB
g�B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
f�B
h�B
h�B
h�B
h�B
i�B
jB
j�B
j�B
j�B
i�B
i�B
k�B
j�B
j�B
i�B
i�B
i�B
j�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
m�B
m�B
m�B
o�B
o�B
o�B
p�B
o�B
o�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
p�B
o�B
o�B
q�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
u�B
v�B
v�B
v�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
xB
w�B
x�B
x�B
x�B
y�B
y�B
y�111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111131144131111111111111111111111111111111113111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111411111111111411111111111111111111111111111111111111111141111111111111111111114111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.1(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806050039332018060500393320180605003933201806221331222018062213312220180622133122201806060027002018060600270020180606002700  JA  ARFMdecpA19c                                                                20180601093525  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180601003541  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180601003543  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180601003544  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180601003544  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180601003544  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180601003544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180601003544  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180601003544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180601003544  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180601003545  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180601003545                      G�O�G�O�G�O�                JA  ARUP                                                                        20180601005537                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180601153731  CV  JULD            G�O�G�O�F�7�                JM  ARSQJMQC2.0                                                                 20180604000000  CF  PSAL_ADJUSTED_QCC  D�  G�O�                JM  ARSQJMQC2.0                                                                 20180604000000  CF  TEMP_ADJUSTED_QCC  D�  G�O�                JM  ARCAJMQC2.0                                                                 20180604153933  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180604153933  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180605152700  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622043122  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231515                      G�O�G�O�G�O�                