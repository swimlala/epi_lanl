CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-21T00:35:28Z creation;2016-10-21T00:35:30Z conversion to V3.1;2019-12-19T08:23:40Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20161021003528  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               2A   JA  I2_0577_050                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @����fǀ1   @���}'Ҁ@3��4֡b�d�_ح��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@���A   A   A@  A^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���@���AffA>ffA\��A|��A�33A�33A�33A�33A�33A�33A�33A�33B��B33B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCL  CM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Ds3D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�Df  Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D�� D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�9�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�� D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D�� D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D� D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�@ D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��mA��mA��`A��`A��mA��mA��TA��HA��;A��HA��HA��TA��TA��;A��/A��
A�ȴAܛ�A�XA�-A��
A���A�AۓuA��Aک�A�VA���A�O�A��A��mA؅A�A�~�A�9XA�;dA�9XAȉ7AǬA��;A��
A���A���A�33A��+A�A�A���A���A��A�dZA�?}A�ƨA���A��A�|�A��
A�ffA���A�bA�r�A���A���A�Q�A��#A��uA�~�A��A��A��A�{A���A�A�A�
=A�&�A���A�~�A���A���A�ffA���A�oA��A�VA��mA���A�v�A��A���A�A�=qA�/A��!A���A�I�A��A��A���A���A�`BA��DA�XA�5?A�ĜA��A�jA�$�A��A}K�A{ƨA{�Ay�^Au��At�ArȴAp �Ao��An��An1'Ak�Ag;dAd�Ab��Aa�wA`bNA]A[33AX-AQp�APA�AOx�AN��AM��AJQ�AF��AD��ADQ�AA��A@�A>Q�A<5?A;7LA:E�A9%A7;dA6�DA6Q�A5ƨA4bA3�A2��A1VA/\)A.1'A-��A-S�A,z�A*�\A)�hA)
=A(�uA(bA'��A'�mA'��A'�^A'�7A&�A&^5A%l�A$ �A#�;A#A#�-A#��A#��A#t�A#O�A#;dA"�`A��AĜA�AƨA5?A/AbA�A$�A�A�^A�#A��A��A�PAr�A�A?}A5?A|�A=qA�A
ffA	dZA�+A%A�A+A��AdZA�\A�TA bN@���@���@�$�@��@���@���@�D@�S�@���@���@�P@�V@�j@��y@��@�  @��@��@�u@�u@�Z@�dZ@�E�@�7L@�bN@�  @� �@��H@�^5@ݩ�@�b@�S�@ڟ�@�ff@�M�@��`@�9X@أ�@؃@�@���@ԛ�@ӥ�@�dZ@�C�@��@ҏ\@ёh@�1'@�@�ȴ@ΰ!@�V@�z�@��@ˍP@���@�5?@�p�@�Z@Ǯ@�o@Ɨ�@��T@�Ĝ@þw@Õ�@�C�@�@��@��H@�@��^@� �@�C�@��!@�-@��^@���@�/@�dZ@��@�M�@��@�@���@�p�@�V@���@�Ĝ@�Ĝ@�/@���@��h@�Ĝ@�r�@���@�t�@��@�v�@�M�@�=q@�5?@�$�@���@���@�?}@�&�@�%@�Ĝ@��@��@�Ĝ@��/@�Q�@�ƨ@�33@�n�@��+@�@�J@��@���@�O�@��u@�A�@�  @��
@���@���@�|�@���@�1@��j@�?}@�ff@���@�{@���@���@�?}@��/@��/@���@��@��@��F@�dZ@�+@�o@��R@��T@��^@��h@��@�Q�@��
@�;d@�+@�33@�C�@�+@��+@���@��@��@��j@��u@�b@�|�@�+@��@�@��@�n�@��@���@�G�@�%@��@��@�Z@�  @�ƨ@�t�@�C�@��@���@��R@���@���@���@��+@�n�@���@�X@��@��9@�Z@��@���@��
@��w@���@���@�|�@�l�@�33@�
=@��@��@��y@��-@�`B@�?}@�7L@��@���@�Ĝ@�z�@�9X@�|�@�n�@��@���@��7@�`B@�?}@��@��@��@���@��@� �@��
@��@��P@�|�@�;d@��H@���@�ȴ@�V@��@�@��-@���@�hs@���@�Ĝ@��9@�bN@���@�;d@���@���@��R@��!@��+@�n�@�M�@�=q@��@��@�V@���@���@��D@��@�bN@�9X@��@��m@���@��@�t�@�l�@�dZ@��H@��-@��@�hs@�O�@���@��@��m@�K�@��@��+@��@��-@���@��h@�hs@��j@��u@��P@�ȴ@���@���@�V@�-@���@�&�@��@���@�1'@�1@��m@��P@�\)@�;d@���@��R@���@���@�n�@�^5@��@��@�?}@���@��@��@�j@�Z@�A�@� �@� �@�@�P@K�@~ȴ@~5?@}��@}�h@}`B@}?}@}�@|�@|�j@|�D@|(�@{ƨ@{ƨ@{�F@{��@{C�@z�@z��@y��@yx�@yX@y�@x  @w\)@vȴ@v{@v@u��@u�@uO�@uV@t�@t�D@tZ@tI�@t(�@sƨ@st�@st�@st�@s�@sC�@r~�@r�@q��@q&�@p�u@p  @o��@o\)@nȴ@n��@n�+@nv�@n@m`B@m�@l�@l��@lz�@k�@kS�@ko@j�H@j��@j�\@jM�@j-@j�@jJ@i�^@i&�@hĜ@hA�@g�;@g|�@g
=@f�y@f�@f��@e�T@e`B@e�@d��@d��@dI�@c�@b��@b��@b�\@b~�@b~�@bJ@a��@aX@`��@`Ĝ@`b@_�@_\)@_
=@^�@]�@]��@]p�@]/@\�@\I�@[t�@[o@[@Z��@ZJ@Y��@Y��@YX@X�u@XbN@XQ�@X �@W�@W�;@W�w@W��@Wl�@W\)@W�@Vȴ@U@T��@T��@Tz�@Tj@TZ@T9X@T�@S�m@S�F@S��@S�@SS�@S33@R��@R-@Q��@PĜ@O�@O��@O�w@O�P@O;d@N��@N�@Nȴ@Nv�@M�@M�-@M/@L�@K��@K�m@Kt�@K@J�!@J=q@I%@Hr�@H  @G��@G|�@G+@F��@Fv�@Fff@F5?@F{@E�@E�T@E�T@E��@E�-@E��@Ep�@Dz�@C�m@C��@C��@Ct�@CC�@B�H@BJ@A�#@A�^@A��@A��@A��@Ahs@A7L@@��@@bN@@A�@?�@?�@?�P@?l�@?K�@?+@>��@>ȴ@>ff@=�@=p�@=�@<�/@<j@;��@;33@:^5@9�^@9�@8��@8Q�@8 �@8b@8b@8  @7�;@7�@7+@6�+@6@5�@4(�@3��@3dZ@2��@2��@2M�@2J@1��@1��@1G�@1%@0��@0Q�@01'@/�@/�;@/��@/�w@/�w@/�w@/�w@/\)@/l�@/|�@.�+@.�+@.�+@.�+@.ff@-�h@,�j@,9X@+�
@+t�@+S�@+@+@*�@*�H@*��@*J@)��@(�`@(�9@(bN@'�@'�P@'K�@'+@&��@&ȴ@&v�@%�T@%@%�-@%�@%V@$�@$�/@$�@$j@$1@#t�@"��@"�\@"~�@"^5@"=q@"J@!�#@!�^@!hs@!X@!X@!7L@ ��@ ��@ �u@ 1'@�@��@��@|�@K�@�@
=@
=@��@�@�+@ff@ff@V@{@�@�T@@O�@�j@j@(�@ƨ@dZ@C�@33@"�@o@@��@�!@~�@^5@M�@�@�#@hs@7L@&�@&�@&�@��@r�@A�@ �@  @  @�@�@\)@;d@+@�y@��@�+@ff@E�@{@@@@�T@�-@��@�h@�@`B@?}@/@V@�/@�j@��@j@�@ƨ@dZ@C�@33@o@@��@��@J@��@��@�7@hs@X@7L@�`@�u@bN@ �@�@��@K�@�y@�@��@�+@v�@V@5?@$�@�T@@�-@��@�h@�h@�@`B@V@�@�/@��@��@�j@j@9X@�m@��@�@t�@dZ@C�@33@o@
�H@
��@
�\@
^5@
=q@	�@	�^@	��@	G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��mA��mA��`A��`A��mA��mA��TA��HA��;A��HA��HA��TA��TA��;A��/A��
A�ȴAܛ�A�XA�-A��
A���A�AۓuA��Aک�A�VA���A�O�A��A��mA؅A�A�~�A�9XA�;dA�9XAȉ7AǬA��;A��
A���A���A�33A��+A�A�A���A���A��A�dZA�?}A�ƨA���A��A�|�A��
A�ffA���A�bA�r�A���A���A�Q�A��#A��uA�~�A��A��A��A�{A���A�A�A�
=A�&�A���A�~�A���A���A�ffA���A�oA��A�VA��mA���A�v�A��A���A�A�=qA�/A��!A���A�I�A��A��A���A���A�`BA��DA�XA�5?A�ĜA��A�jA�$�A��A}K�A{ƨA{�Ay�^Au��At�ArȴAp �Ao��An��An1'Ak�Ag;dAd�Ab��Aa�wA`bNA]A[33AX-AQp�APA�AOx�AN��AM��AJQ�AF��AD��ADQ�AA��A@�A>Q�A<5?A;7LA:E�A9%A7;dA6�DA6Q�A5ƨA4bA3�A2��A1VA/\)A.1'A-��A-S�A,z�A*�\A)�hA)
=A(�uA(bA'��A'�mA'��A'�^A'�7A&�A&^5A%l�A$ �A#�;A#A#�-A#��A#��A#t�A#O�A#;dA"�`A��AĜA�AƨA5?A/AbA�A$�A�A�^A�#A��A��A�PAr�A�A?}A5?A|�A=qA�A
ffA	dZA�+A%A�A+A��AdZA�\A�TA bN@���@���@�$�@��@���@���@�D@�S�@���@���@�P@�V@�j@��y@��@�  @��@��@�u@�u@�Z@�dZ@�E�@�7L@�bN@�  @� �@��H@�^5@ݩ�@�b@�S�@ڟ�@�ff@�M�@��`@�9X@أ�@؃@�@���@ԛ�@ӥ�@�dZ@�C�@��@ҏ\@ёh@�1'@�@�ȴ@ΰ!@�V@�z�@��@ˍP@���@�5?@�p�@�Z@Ǯ@�o@Ɨ�@��T@�Ĝ@þw@Õ�@�C�@�@��@��H@�@��^@� �@�C�@��!@�-@��^@���@�/@�dZ@��@�M�@��@�@���@�p�@�V@���@�Ĝ@�Ĝ@�/@���@��h@�Ĝ@�r�@���@�t�@��@�v�@�M�@�=q@�5?@�$�@���@���@�?}@�&�@�%@�Ĝ@��@��@�Ĝ@��/@�Q�@�ƨ@�33@�n�@��+@�@�J@��@���@�O�@��u@�A�@�  @��
@���@���@�|�@���@�1@��j@�?}@�ff@���@�{@���@���@�?}@��/@��/@���@��@��@��F@�dZ@�+@�o@��R@��T@��^@��h@��@�Q�@��
@�;d@�+@�33@�C�@�+@��+@���@��@��@��j@��u@�b@�|�@�+@��@�@��@�n�@��@���@�G�@�%@��@��@�Z@�  @�ƨ@�t�@�C�@��@���@��R@���@���@���@��+@�n�@���@�X@��@��9@�Z@��@���@��
@��w@���@���@�|�@�l�@�33@�
=@��@��@��y@��-@�`B@�?}@�7L@��@���@�Ĝ@�z�@�9X@�|�@�n�@��@���@��7@�`B@�?}@��@��@��@���@��@� �@��
@��@��P@�|�@�;d@��H@���@�ȴ@�V@��@�@��-@���@�hs@���@�Ĝ@��9@�bN@���@�;d@���@���@��R@��!@��+@�n�@�M�@�=q@��@��@�V@���@���@��D@��@�bN@�9X@��@��m@���@��@�t�@�l�@�dZ@��H@��-@��@�hs@�O�@���@��@��m@�K�@��@��+@��@��-@���@��h@�hs@��j@��u@��P@�ȴ@���@���@�V@�-@���@�&�@��@���@�1'@�1@��m@��P@�\)@�;d@���@��R@���@���@�n�@�^5@��@��@�?}@���@��@��@�j@�Z@�A�@� �@� �@�@�P@K�@~ȴ@~5?@}��@}�h@}`B@}?}@}�@|�@|�j@|�D@|(�@{ƨ@{ƨ@{�F@{��@{C�@z�@z��@y��@yx�@yX@y�@x  @w\)@vȴ@v{@v@u��@u�@uO�@uV@t�@t�D@tZ@tI�@t(�@sƨ@st�@st�@st�@s�@sC�@r~�@r�@q��@q&�@p�u@p  @o��@o\)@nȴ@n��@n�+@nv�@n@m`B@m�@l�@l��@lz�@k�@kS�@ko@j�H@j��@j�\@jM�@j-@j�@jJ@i�^@i&�@hĜ@hA�@g�;@g|�@g
=@f�y@f�@f��@e�T@e`B@e�@d��@d��@dI�@c�@b��@b��@b�\@b~�@b~�@bJ@a��@aX@`��@`Ĝ@`b@_�@_\)@_
=@^�@]�@]��@]p�@]/@\�@\I�@[t�@[o@[@Z��@ZJ@Y��@Y��@YX@X�u@XbN@XQ�@X �@W�@W�;@W�w@W��@Wl�@W\)@W�@Vȴ@U@T��@T��@Tz�@Tj@TZ@T9X@T�@S�m@S�F@S��@S�@SS�@S33@R��@R-@Q��@PĜ@O�@O��@O�w@O�P@O;d@N��@N�@Nȴ@Nv�@M�@M�-@M/@L�@K��@K�m@Kt�@K@J�!@J=q@I%@Hr�@H  @G��@G|�@G+@F��@Fv�@Fff@F5?@F{@E�@E�T@E�T@E��@E�-@E��@Ep�@Dz�@C�m@C��@C��@Ct�@CC�@B�H@BJ@A�#@A�^@A��@A��@A��@Ahs@A7L@@��@@bN@@A�@?�@?�@?�P@?l�@?K�@?+@>��@>ȴ@>ff@=�@=p�@=�@<�/@<j@;��@;33@:^5@9�^@9�@8��@8Q�@8 �@8b@8b@8  @7�;@7�@7+@6�+@6@5�@4(�@3��@3dZ@2��@2��@2M�@2J@1��@1��@1G�@1%@0��@0Q�@01'@/�@/�;@/��@/�w@/�w@/�w@/�w@/\)@/l�@/|�@.�+@.�+@.�+@.�+@.ff@-�h@,�j@,9X@+�
@+t�@+S�@+@+@*�@*�H@*��@*J@)��@(�`@(�9@(bN@'�@'�P@'K�@'+@&��@&ȴ@&v�@%�T@%@%�-@%�@%V@$�@$�/@$�@$j@$1@#t�@"��@"�\@"~�@"^5@"=q@"J@!�#@!�^@!hs@!X@!X@!7L@ ��@ ��@ �u@ 1'@�@��@��@|�@K�@�@
=@
=@��@�@�+@ff@ff@V@{@�@�T@@O�@�j@j@(�@ƨ@dZ@C�@33@"�@o@@��@�!@~�@^5@M�@�@�#@hs@7L@&�@&�@&�@��@r�@A�@ �@  @  @�@�@\)@;d@+@�y@��@�+@ff@E�@{@@@@�T@�-@��@�h@�@`B@?}@/@V@�/@�j@��@j@�@ƨ@dZ@C�@33@o@@��@��@J@��@��@�7@hs@X@7L@�`@�u@bN@ �@�@��@K�@�y@�@��@�+@v�@V@5?@$�@�T@@�-@��@�h@�h@�@`B@V@�@�/@��@��@�j@j@9X@�m@��@�@t�@dZ@C�@33@o@
�H@
��@
�\@
^5@
=q@	�@	�^@	��@	G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�?B�9B�9B�?B�?B�?B�FB�LB�LB�LB�LB�LB�LB�^B�jB��BǮB�5B�B��BB+B	7BbB+B49B>wBK�BT�BhsBk�BjBgmBm�B~�B�%B��B�'B�RB�qBĜB��B��B�/B�/B�5B�;B�;B�yB�B�B�B�B�mB�ZB��BǮBB�dB��B��B�uB�bB�=B�+B|�Bw�Bu�Bq�Bl�BVB>wB0!B#�BuBDB��B��B�B�mB��B�3B��B�uB�Bk�B^5BS�BF�B6FB33B-B�BuBB
��B
��B
�B
�B
�NB
�/B
�#B
��B
ɺB
�qB
��B
��B
�B
q�B
jB
^5B
>wB
.B
%�B
VB
	7B
B	��B	�B	��B	�wB	�!B	��B	��B	�PB	w�B	jB	@�B	5?B	/B	+B	#�B	uB	  B��B�B�yB�HB�)B��B��B��B��BŢBÖBÖBƨBǮBB��B�^B�9B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�PB�1B�B�B�B}�B|�Bw�Bu�Bs�Bq�Bl�BjBiyBhsBe`BcTBcTBaHB`BB_;B]/B^5BZBYB\)BZB\)Bq�Br�Bp�Bl�Bk�BffB\)BYBW
BYBaHBcTBe`BgmBq�Bu�By�Bx�Bt�Bk�BhsBgmBl�Bw�Bx�Bx�B{�B~�B�B�%B�\B��B��B��B��B�B�B�!B�B�B�B�-B�FB�FB�FB�XBB��B��B��B��B��B�B�5B�NB�NB�HB�NB�yB�B�B�B�B��B��B��B	B	B	B		7B	PB	VB	\B	hB	hB	hB	hB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	'�B	,B	-B	-B	.B	33B	5?B	9XB	;dB	B�B	J�B	N�B	S�B	T�B	W
B	YB	ZB	\)B	\)B	]/B	]/B	]/B	^5B	_;B	aHB	aHB	bNB	cTB	cTB	dZB	hsB	m�B	n�B	l�B	o�B	r�B	w�B	x�B	x�B	x�B	w�B	w�B	w�B	v�B	v�B	w�B	x�B	z�B	}�B	� B	�B	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�FB	�RB	�XB	�jB	�}B	��B	B	ÖB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�ZB	�fB	�fB	�fB	�fB	�fB	�mB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�fB	�fB	�fB	�fB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
JB
PB
JB
JB
JB
PB
VB
\B
\B
\B
\B
\B
\B
\B
bB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
"�B
"�B
"�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
%�B
&�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
)�B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
.B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
XB
XB
XB
XB
YB
XB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
YB
YB
YB
YB
ZB
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�ZB�9B�9B�?B�?B�?B�`B�LB�LB�LB�LB�LB�LB�^B��B��B�KB��B��B�fBBzB	�B�B+�B5B?�BL�BU�Bi*BmCBn}BmwBt�B��B�6B�B�3B�xB�4BȀB�oB��B�jB��B�VB�bB�B��B��B��B�FB�IB�}B�B��B�XB�+B�'B�*B�B��B��B�xB�B}�BxlBv�BtBqBYBA;B2aB&LB�B�B��B�zB�B�QB�sB��B��B�mB��Bm�B_�BVBH1B6�B4�B.�BB�B%B
��B
��B
��B
�;B
��B
��B
�xB
ּB
��B
�iB
��B
�B
��B
sB
l�B
a�B
@iB
0�B
(sB
vB

rB
�B	�qB	�MB	��B	��B	��B	�*B	�B	�B	|�B	p�B	BuB	6zB	0oB	-]B	(
B	�B	[B�B��B�B��BބBևB�hBϫB��BƎB�3B��BȚB��B�{B��B�jB��B�B� B��B�"B�0B��B�zB�tB��B��B��B�B�\B��B��B�B��B��B��B��B��B��B��B��B�B��B��B��B�RB�?B�B��B�B~�Bx�Bw�Bu�Bs�Bm�Bk�Bj�Bi�BfLBd�Bd�Bb�Ba�B`\B_;B_�B\CB\�B]~B[	B\BrGBs�BrBo5BoBhsB]IBY�BWYBYKBa�Bd@BffBhXBr�Bv�B{0BzDBu�Bl�Bi_BhXBmCBw�ByXBy�B|�B�B��B��B��B�SB�1B�qB��B��B��B�UB��B��B�}B�-B��B�LB�2B�DB�GB�B�(B�HB�uB��B�B�B�B�B��B�B��B��B�B�IB�MB��B��B��B	�B	�B	�B		�B	�B	�B	�B	�B	�B	�B	TB	�B	?B	�B	�B	�B	�B	1B	�B	"4B	#TB	(>B	,=B	-]B	-]B	.cB	3MB	5tB	9XB	;0B	BuB	K)B	O\B	TFB	U�B	WYB	YB	ZkB	\CB	\CB	]dB	]IB	]~B	^jB	_�B	a|B	a�B	b�B	cnB	cTB	dtB	h�B	m�B	oB	mB	pB	r�B	xB	x�B	y$B	y>B	xB	xRB	xB	v�B	v�B	w�B	y	B	z�B	}�B	�B	��B	��B	��B	�B	�;B	�B	�B	�&B	�LB	�B	�WB	�QB	�kB	�]B	�hB	��B	��B	��B	��B	��B	��B	�B	�MB	�B	�	B	��B	��B	��B	�B	҉B	�uB	�_B	�yB	�B	�eB	ڠB	چB	�]B	�IB	�dB	�~B	ބB	ߊB	ߤB	��B	�vB	�\B	��B	��B	�B	�|B	�|B	�B	�B	�tB	�tB	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�8B	��B	�B	�B	�B	�B	��B	��B	��B	�B	�)B	��B	��B	��B	�B	�B	��B	�B	��B	�B	�8B	�B	��B	��B	��B	��B	�B	��B	��B	��B	�8B	�$B	��B	��B	�	B	�	B	�DB	��B	��B	�0B	�jB	�.B
 4B
 4B
 4B
 B
;B
 B
;B
;B
UB
�B
aB
gB
mB
YB
YB
?B
?B
_B
zB
_B
KB
KB
fB
�B
�B
	�B

rB
^B
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
.B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
B
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
!B
!�B
!�B
!�B
# B
#B
#B
"NB
#B
# B
# B
#B
"�B
"�B
$B
$B
#�B
$B
%B
%B
%B
$�B
&B
%�B
%�B
%�B
&2B
&LB
&B
'8B
&B
'B
&2B
'B
'B
(>B
(
B
(
B
($B
($B
(>B
)*B
)*B
)*B
)*B
*KB
*B
*B
*B
*B
+B
*0B
+B
+B
+6B
+6B
+QB
,=B
,WB
-CB
-CB
-]B
.IB
-)B
-]B
-]B
.cB
.IB
/5B
/OB
/iB
/�B
0oB
1[B
1[B
2GB
2GB
2|B
2aB
2GB
2aB
3MB
3�B
4nB
4TB
4nB
5ZB
5�B
6zB
6`B
6`B
6zB
7�B
7�B
7�B
7�B
7fB
7�B
8�B
8lB
8�B
9�B
9�B
9�B
9rB
9rB
9rB
9�B
9rB
9rB
9rB
9�B
9�B
9�B
:�B
;B
;�B
;B
;B
;�B
;B
;B
;B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FB
F�B
G�B
G�B
G�B
G�B
G�B
IB
H�B
H�B
I�B
I�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
MB
MB
L�B
MB
MB
N"B
N"B
O(B
P.B
P.B
O�B
Q B
P�B
Q B
QB
Q B
Q B
QB
R:B
R B
S[B
TaB
UMB
UB
U2B
VB
V9B
VB
VB
V9B
W?B
W$B
XEB
XEB
X+B
XEB
YB
X+B
YB
Y1B
YB
YB
YKB
YB
Y1B
YeB
Z7B
YB
YKB
YKB
YB
ZkB
[WB
\]B
\CB
]IB
]IB
]IB
]IB
]IB
]dB
]~B
^�B
^�B
_pB
_pB
_pB
`vB
`\B
`vB
`\B
`\B
`vB
a|B
a|B
abB
a|B
a|B
bhB
b�B
bhB
bhB
b�B
c�B
c�B
d�B
dZB
d�B
dtB
dtB
dtB
ezB
ezB
ezB
ezB
ezB
e�B
f�B
f�B
f�B
f�B
g�B
gmB
g�B
g�B
g�B
g�B
gmB
g�B
g�B
h�B
h�B
hsB
h�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
vB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
x�B
y	B
y	B
x�B
x�B
x�B
y�B
zB
zB
y�B
y�B
zB
z�B
z�B
z�B
{B
z�B
{B
z�B
z�B
z�B
|B
|B
|B
|B
|B
|B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.1(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610250034402016102500344020161025003440201806221303542018062213035420180622130354201804050703342018040507033420180405070334  JA  ARFMdecpA19c                                                                20161021093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161021003528  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161021003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161021003529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161021003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161021003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161021003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161021003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161021003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161021003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20161021012137                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161021153812  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161024153440  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161024153440  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220334  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040354  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                