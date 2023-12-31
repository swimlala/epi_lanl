CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:54:02Z creation;2022-06-04T17:54:02Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175402  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               5A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�)��	{B1   @�)�Ib��@/���l�D�c#l�C��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�ffB�  B�33B˙�B�  B�  B�  B�ffB�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C�C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0�C1�fC3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO�fDPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  Dy�D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @34@s34@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B�  B�  B���B���B���B���B���B���B���B���B���B���B���B�33B�33B���B�  B�fgB���B���B���B�33B���B���B���B뙚B���B���B���B���B���C�fC�fC�fC�fC	�fC  C�fC  C�fC��C�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC.  C0  C1��C3��C5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#� D$  D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DO� DP  DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dms4Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Ds4D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�vg1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aъ	Aц%AцYAэAь~AюVAя�Aѐ�AѓAѐ.Aщ�A�~]A��AсAј+A�sMA�7A��QA�k�A�oA��WA��BA�֡A��6AϿ�Aϲ-AϥFAϐ�A�u�A�MA���A�8�A��]A˗�A�	Aȣ:A���A�bA�E�A��pA�N�A�h>A���A���A���A�5?A��LA� �A���A��xA�� A��A�	�A�9�A�DgA�{A��A��EA���A���A��A�W
A��A�qA��A���A�~A�ԕA�j�A�.�A�x8A�=�A�8A�͟A�S[A��A���A���A��A�{A�7A�خA��:A}�*A{�CAw5�Asz�Apa|AnIRAl�Ai�Ae8�Aa�A]��A[��AX��AW"�AS7�AP�AO~�AOSALϫAKm]AI�AGAE�4AC�{A?�A=G�A<tTA<5?A:��A9�A8��A6�rA4�A3S&A1�4A/dZA-}�A*$�A(�]A(��A'�rA&�aA%��A%OvA#�A! \A �DA�/A��A�mA[WA��A�3A��A�A \AK^A��A��A�UA��AѷADgAe�A$tA6�A��A|A$tA�9A�A�A�A��A��A�AɆAB[AVA��A�A�TA?A��A�AAԕA�A�*A?�A�A�AیA��AZ�A�HA��A=qA�#AZA_A
�mA
�A
�CA
͟A
�pA
��A	֡A	��A	|A	j�A	D�A	�AZ�A�EA��A8�A�A� An�A�A�ARTA�rA�9A>�A,=AoA�zAh
AMA�zA\�AA�mAc�A ��A �nA �jA sA �A �@���@��X@�0�@��I@���@�O�@���@�Mj@���@���@�G@�=@�/�@��f@�!�@���@�R�@��@�RT@���@�$@�/�@�$@���@@��}@��@�Y@�y�@�֡@�d�@��@�c�@�l"@�(�@���@��@�@�E9@�T�@�qv@�[W@���@�a|@��A@�g�@���@��@���@�X@��|@��@�w2@���@�Mj@�U2@۩�@ڛ�@�7@��@�|�@��y@ؑ @�M�@�3�@��@�8�@�[�@��@�Vm@�҉@��@Ӑ�@�}�@���@�u%@�2�@�ԕ@�.I@�u�@�7@�hs@��@΀�@�7@ͬq@�;@̑�@�H@˟V@��M@ʷ�@�oi@��Z@ɗ�@�A�@��@�kQ@�1'@��>@Ǔ@�=@ƕ@�[�@�3�@��@�e,@��@Ĺ�@�~�@�+k@��N@î@�33@��@���@I@�y>@�=q@��@���@�S�@��@�w�@��m@�dZ@�$t@���@�@�@��o@��@��@�u%@�1'@���@�P�@� i@��x@�c @��;@���@�rG@�Y@���@���@�kQ@���@���@�Z�@�(�@��_@�p;@�J@�8�@���@��@��[@�j@��@���@��@�%�@���@�o�@��@��O@� �@��@�x�@�=@� \@��p@�xl@�`�@���@���@���@� i@���@���@�  @���@��@�͟@���@�h
@�($@��j@���@�iD@�;d@�+@���@��@�Q@��+@��6@�hs@��@��8@�tT@���@���@�7L@�Ɇ@���@�u%@�"h@���@��@��W@���@���@�c�@��@��?@���@���@��F@���@�2�@�	@��@��{@�^�@�V@�l"@�~@��Z@��@���@�w2@�!-@���@���@�8�@��m@��-@��M@�"�@��@���@��@�'R@���@���@��	@�7L@��8@��H@��R@��.@�,=@��.@���@��"@�N<@�!�@���@�@�@��>@���@���@��7@�g�@�E9@���@�c�@�7�@�@���@��S@�7L@�@�Ɇ@��Y@�Ft@��@��#@�m]@�33@��`@���@���@�?@�	@��#@���@�u�@�C�@��@�ȴ@��A@�;�@�M@��d@�|�@�5�@�Y@��9@�n�@�*�@���@�ƨ@��@@��"@�rG@��@��@��U@���@�q�@�&�@��@��S@�F@��@��I@�l�@�E�@�|�@��K@��M@��v@���@�c�@�+k@��@�j�@��@���@���@�tT@�q@��@���@��H@���@�c@�&�@��P@��@�֡@��U@��@���@�Q�@��@��>@��}@��@��V@�j�@���@���@�YK@��@���@��3@���@�A�@��@��@���@���@�1'@��@�@E9@�@~�b@}�@}�=@}u�@}%F@}�@|ی@|��@{�m@{dZ@{!-@{�@z�@z��@z��@y��@y-w@x�@x��@x��@xQ�@x<�@x�@w�*@wg�@w!-@v��@v�@v��@vu%@u�z@t��@t��@t�@s��@s i@r�@r6�@q�N@qk�@p�@p�D@p �@o�a@oS�@n�@n��@n��@nl�@n1�@n�@mϫ@l��@lx@k��@k�V@ky�@k+@k�@j�M@jTa@j�@iԕ@i��@ihs@i*0@hĜ@hD�@hb@g�6@gO@f��@fTa@f�@e��@em]@e�@dی@d�@d/�@c��@co@b�1@bH�@aϫ@a2a@`�K@`��@`m�@_�;@_C@^��@^�!@^l�@^�@]�-@]c@]:�@\�@\`�@\b@[��@Z��@Z��@ZQ@Z?@Z4@Y�3@Y�n@Ya�@Y%@X��@XK^@X�@Wj�@V�@VkQ@V$�@U�@U�d@U�=@Uc@UB�@T��@T�j@T%�@S�Q@Sqv@R�@R��@R��@R5?@Q��@QT�@P��@P%�@O��@O�w@O��@O~�@ORT@N�"@N� @M�@Mp�@L�/@L_@K��@K�@Kx@KC�@K�@J��@J�@I��@IA @I+@HĜ@HFt@G�@G��@G�P@GZ�@G4�@G�@Fu%@F�@E��@E%F@D1'@C�a@Cj�@C8@C!-@B�H@B�A@BW�@A�9@AVm@@��@@��@@��@@H@?�;@?~�@>͟@>^5@>E�@=�@=��@=�S@=O�@<�K@<��@<��@<u�@<K^@<�@;��@:�@:@9��@9c�@9=�@9;@8��@8:�@7�}@76z@6��@6ȴ@6~�@65?@5�D@5�>@5�-@5x�@5q@4�@4�@47@3�@3�g@3�k@3_p@38@3 i@2J�@1�@1�d@1��@1u�@1B�@1;@0z�@0@/��@/�K@/t�@/4�@.��@.l�@-��@-�@-��@-u�@-L�@-+@,�/@,��@,��@,m�@,1'@,@+�A@+��@+�}@+�[@+g�@+o@*�H@*��@*�+@*8�@)�-@)N<@(��@(ی@(��@(�e@(��@'�r@'��@'P�@'"�@'�@&��@&�X@&u%@& �@%��@%��@%zx@%O�@%�@$��@$��@$bN@$�@#�6@#�f@#=@"��@"�'@"i�@"	@!��@!�"@!B�@!�@ ��@ ��@ Xy@��@��@o�@\)@X�@,�@�'@p;@Ov@($@��@�t@��@Q�@	l@�@��@�@h�@H@<�@�@ݘ@��@_p@1�@��@�@��@��@u%@a|@^5@c @R�@-@��@=�@�@֡@��@�9@��@_@ �@��@g�@33@�@ i@��@��@xl@\�@@�@1�@&�@�D@�#@�@e,@(�@�E@�@�u@S�@��@�&@�@�k@�	@�{@H�@"�@�h@q�@c @W�@R�@:*@)�@�@u@��@�@�@�-@��@�~@�@�[@��@��@�@��@�4@c�@PH@>B@G@�m@�@��@��@�*@�{@j�@4�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aъ	Aц%AцYAэAь~AюVAя�Aѐ�AѓAѐ.Aщ�A�~]A��AсAј+A�sMA�7A��QA�k�A�oA��WA��BA�֡A��6AϿ�Aϲ-AϥFAϐ�A�u�A�MA���A�8�A��]A˗�A�	Aȣ:A���A�bA�E�A��pA�N�A�h>A���A���A���A�5?A��LA� �A���A��xA�� A��A�	�A�9�A�DgA�{A��A��EA���A���A��A�W
A��A�qA��A���A�~A�ԕA�j�A�.�A�x8A�=�A�8A�͟A�S[A��A���A���A��A�{A�7A�خA��:A}�*A{�CAw5�Asz�Apa|AnIRAl�Ai�Ae8�Aa�A]��A[��AX��AW"�AS7�AP�AO~�AOSALϫAKm]AI�AGAE�4AC�{A?�A=G�A<tTA<5?A:��A9�A8��A6�rA4�A3S&A1�4A/dZA-}�A*$�A(�]A(��A'�rA&�aA%��A%OvA#�A! \A �DA�/A��A�mA[WA��A�3A��A�A \AK^A��A��A�UA��AѷADgAe�A$tA6�A��A|A$tA�9A�A�A�A��A��A�AɆAB[AVA��A�A�TA?A��A�AAԕA�A�*A?�A�A�AیA��AZ�A�HA��A=qA�#AZA_A
�mA
�A
�CA
͟A
�pA
��A	֡A	��A	|A	j�A	D�A	�AZ�A�EA��A8�A�A� An�A�A�ARTA�rA�9A>�A,=AoA�zAh
AMA�zA\�AA�mAc�A ��A �nA �jA sA �A �@���@��X@�0�@��I@���@�O�@���@�Mj@���@���@�G@�=@�/�@��f@�!�@���@�R�@��@�RT@���@�$@�/�@�$@���@@��}@��@�Y@�y�@�֡@�d�@��@�c�@�l"@�(�@���@��@�@�E9@�T�@�qv@�[W@���@�a|@��A@�g�@���@��@���@�X@��|@��@�w2@���@�Mj@�U2@۩�@ڛ�@�7@��@�|�@��y@ؑ @�M�@�3�@��@�8�@�[�@��@�Vm@�҉@��@Ӑ�@�}�@���@�u%@�2�@�ԕ@�.I@�u�@�7@�hs@��@΀�@�7@ͬq@�;@̑�@�H@˟V@��M@ʷ�@�oi@��Z@ɗ�@�A�@��@�kQ@�1'@��>@Ǔ@�=@ƕ@�[�@�3�@��@�e,@��@Ĺ�@�~�@�+k@��N@î@�33@��@���@I@�y>@�=q@��@���@�S�@��@�w�@��m@�dZ@�$t@���@�@�@��o@��@��@�u%@�1'@���@�P�@� i@��x@�c @��;@���@�rG@�Y@���@���@�kQ@���@���@�Z�@�(�@��_@�p;@�J@�8�@���@��@��[@�j@��@���@��@�%�@���@�o�@��@��O@� �@��@�x�@�=@� \@��p@�xl@�`�@���@���@���@� i@���@���@�  @���@��@�͟@���@�h
@�($@��j@���@�iD@�;d@�+@���@��@�Q@��+@��6@�hs@��@��8@�tT@���@���@�7L@�Ɇ@���@�u%@�"h@���@��@��W@���@���@�c�@��@��?@���@���@��F@���@�2�@�	@��@��{@�^�@�V@�l"@�~@��Z@��@���@�w2@�!-@���@���@�8�@��m@��-@��M@�"�@��@���@��@�'R@���@���@��	@�7L@��8@��H@��R@��.@�,=@��.@���@��"@�N<@�!�@���@�@�@��>@���@���@��7@�g�@�E9@���@�c�@�7�@�@���@��S@�7L@�@�Ɇ@��Y@�Ft@��@��#@�m]@�33@��`@���@���@�?@�	@��#@���@�u�@�C�@��@�ȴ@��A@�;�@�M@��d@�|�@�5�@�Y@��9@�n�@�*�@���@�ƨ@��@@��"@�rG@��@��@��U@���@�q�@�&�@��@��S@�F@��@��I@�l�@�E�@�|�@��K@��M@��v@���@�c�@�+k@��@�j�@��@���@���@�tT@�q@��@���@��H@���@�c@�&�@��P@��@�֡@��U@��@���@�Q�@��@��>@��}@��@��V@�j�@���@���@�YK@��@���@��3@���@�A�@��@��@���@���@�1'@��@�@E9@�@~�b@}�@}�=@}u�@}%F@}�@|ی@|��@{�m@{dZ@{!-@{�@z�@z��@z��@y��@y-w@x�@x��@x��@xQ�@x<�@x�@w�*@wg�@w!-@v��@v�@v��@vu%@u�z@t��@t��@t�@s��@s i@r�@r6�@q�N@qk�@p�@p�D@p �@o�a@oS�@n�@n��@n��@nl�@n1�@n�@mϫ@l��@lx@k��@k�V@ky�@k+@k�@j�M@jTa@j�@iԕ@i��@ihs@i*0@hĜ@hD�@hb@g�6@gO@f��@fTa@f�@e��@em]@e�@dی@d�@d/�@c��@co@b�1@bH�@aϫ@a2a@`�K@`��@`m�@_�;@_C@^��@^�!@^l�@^�@]�-@]c@]:�@\�@\`�@\b@[��@Z��@Z��@ZQ@Z?@Z4@Y�3@Y�n@Ya�@Y%@X��@XK^@X�@Wj�@V�@VkQ@V$�@U�@U�d@U�=@Uc@UB�@T��@T�j@T%�@S�Q@Sqv@R�@R��@R��@R5?@Q��@QT�@P��@P%�@O��@O�w@O��@O~�@ORT@N�"@N� @M�@Mp�@L�/@L_@K��@K�@Kx@KC�@K�@J��@J�@I��@IA @I+@HĜ@HFt@G�@G��@G�P@GZ�@G4�@G�@Fu%@F�@E��@E%F@D1'@C�a@Cj�@C8@C!-@B�H@B�A@BW�@A�9@AVm@@��@@��@@��@@H@?�;@?~�@>͟@>^5@>E�@=�@=��@=�S@=O�@<�K@<��@<��@<u�@<K^@<�@;��@:�@:@9��@9c�@9=�@9;@8��@8:�@7�}@76z@6��@6ȴ@6~�@65?@5�D@5�>@5�-@5x�@5q@4�@4�@47@3�@3�g@3�k@3_p@38@3 i@2J�@1�@1�d@1��@1u�@1B�@1;@0z�@0@/��@/�K@/t�@/4�@.��@.l�@-��@-�@-��@-u�@-L�@-+@,�/@,��@,��@,m�@,1'@,@+�A@+��@+�}@+�[@+g�@+o@*�H@*��@*�+@*8�@)�-@)N<@(��@(ی@(��@(�e@(��@'�r@'��@'P�@'"�@'�@&��@&�X@&u%@& �@%��@%��@%zx@%O�@%�@$��@$��@$bN@$�@#�6@#�f@#=@"��@"�'@"i�@"	@!��@!�"@!B�@!�@ ��@ ��@ Xy@��@��@o�@\)@X�@,�@�'@p;@Ov@($@��@�t@��@Q�@	l@�@��@�@h�@H@<�@�@ݘ@��@_p@1�@��@�@��@��@u%@a|@^5@c @R�@-@��@=�@�@֡@��@�9@��@_@ �@��@g�@33@�@ i@��@��@xl@\�@@�@1�@&�@�D@�#@�@e,@(�@�E@�@�u@S�@��@�&@�@�k@�	@�{@H�@"�@�h@q�@c @W�@R�@:*@)�@�@u@��@�@�@�-@��@�~@�@�[@��@��@�@��@�4@c�@PH@>B@G@�m@�@��@��@�*@�{@j�@4�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	jKB	jeB	jB	jKB	jB	j0B	jeB	j0B	j0B	jKB	i�B	i�B	i�B	i�B	j0B	h�B	h
B	fB	c:B	^�B	\�B	\B	\�B	\�B	[�B	[	B	Z7B	YB	X�B	WsB	TB	TFB	P�B	N�B	I�B	@iB	A�B	m�B	�[B	��B	��B	�aB	��B	�B
 �B	��B
�B
\B
oB
E�B
��B
ǔB
ߤB	�BdB%zBz^B}"Bp�By�BVB=�BK�BHfB6+B+�B*�B@�B�MB�B�hB��B�9BnB:B	�B
�<B
i�B
2aB	�FB	�HB	��B	�B	�2B	�sB	��B	��B	w�B	l�B	cnB	OvB	<B	+�B	B	�B	B��B��B�kB�B�B�~BچB��B�&B�B��B�-B��B��B�iB��B�]B��B��B�LB��B��B�3B�5B��B�;B� B̈́B��B�6B�B�cB��B��B�
B�B�*B��B��B�aB�B	 �B	B	�B	�B	"4B	-CB	,"B	F�B	^�B	j�B	s�B	u�B	y$B	sB	t9B	w2B	l�B	c�B	bNB	^�B	bB	q�B	eB	_�B	`�B	d�B	s�B	��B	��B	��B	�UB	��B	�ZB	��B	��B	ǮB	�B	�"B	�6B	ǔB	ȀB	�DB	��B	�=B	ȴB	ʦB	�~B	�oB	��B	��B	�1B	�KB	��B	�	B	�xB	�]B	�~B	ݲB	ݲB	�WB	�kB	�B	�B	�/B	�/B	�dB	�~B	ܒB	ںB	��B	�B	��B	ںB	�WB	��B	��B	ݲB	�IB	یB	�B	רB	ҽB	ӏB	��B	چB	��B	��B	��B	�=B	�WB	�#B	ڠB	�B	�9B	уB	̘B	�1B	��B	�xB	�^B	��B	�DB	�DB	�jB	�(B	�HB	�}B	οB	�BB	�"B	͟B	�VB	ԕB	�gB	�B	��B	��B	� B	ЗB	� B	��B	�HB	�{B	��B	�TB	ΥB	��B	�,B	��B	��B	�B	�B	ԯB	�{B	ԕB	�{B	�,B	�uB	�bB	�HB	��B	ɠB	�B	��B	�gB	�3B	�%B	��B	�zB	�XB	��B	ϫB	�vB	��B	�B	�vB	�B	��B	�(B	��B	�2B	�B	��B	ևB	׍B	�+B	�KB	��B	��B	�=B	�qB	��B	�B	��B	�/B	�IB	�OB	�!B	�!B	�VB	��B	��B	�BB	�B	��B	��B	��B	�B	��B	�4B	�hB	�B	�B	��B	��B	��B	��B	�:B	�&B	�B	�B	��B	�zB	�zB	�zB	�B	�B	�B	��B	��B	�2B	�B	�B	�RB	�B	��B	�
B	�$B	�XB	��B	�yB	�0B	�0B	�0B	�KB	�B	�QB	�QB	�6B	�WB	�B	�B	�CB	��B	��B	�IB	�}B	�;B	�GB	�GB	�;B	�iB	�iB	�B	��B	�!B	�oB	��B	�AB	�'B	�AB	�B	�B	�B	��B	��B	�3B	�3B	��B	�ZB	�fB	��B	�B	�lB	��B	�*B	��B	�B	�B	��B	�jB	��B	��B	��B	��B	��B	�B	�"B	�<B	�qB	��B	��B	��B	�B	��B	�VB	�}B
 B
 4B
 iB
 �B
�B
[B
�B
[B
'B
B
B
AB
[B
uB
�B
GB
-B
-B
-B
MB
MB
B
�B
gB
gB
�B
�B
�B
?B
�B
�B
�B
B
zB
�B
�B
fB
�B
�B
	�B

	B
	�B

=B
B
DB
B

�B
B

�B

�B

�B
DB
�B
�B
JB
�B
B
B
�B
<B
�B
�B
�B
(B
(B
BB
�B
bB
}B
}B
 B
B
�B
�B
B
TB
oB
�B
&B
�B
�B
{B
�B
B
�B
�B
�B
�B
�B
B
�B
sB
�B
B
�B
EB
yB
yB
�B
B
�B
kB
B
�B
�B
�B
xB
dB
�B
�B
�B
�B
�B
�B
�B
 B
 �B
 �B
 �B
 vB
�B
�B
 'B
 BB
 B
 B
 'B
 �B
 vB
�B
;B
�B
!B
"NB
"NB
"�B
"�B
"4B
"B
"B
"B
"�B
#TB
$�B
%`B
%�B
%zB
%�B
%�B
%�B
%zB
%�B
&2B
%zB
&�B
&�B
'�B
'�B
'mB
'�B
'�B
'mB
'mB
'�B
)B
(XB
'�B
'�B
'B
'B
'�B
(>B
(>B
(>B
(sB
(XB
(XB
(�B
(�B
)*B
)B
)*B
)*B
)DB
)*B
*eB
*�B
*�B
+kB
+�B
+�B
+�B
+�B
+�B
,qB
,qB
,�B
-wB
.}B
.�B
/ B
./B
./B
./B
.cB
/OB
/OB
/�B
/�B
0UB
0UB
0UB
0�B
0�B
1B
0�B
1[B
2B
2GB
2aB
2aB
2GB
33B
3�B
3�B
4�B
4�B
5B
5�B
6zB
5�B
5�B
5�B
6B
7�B
7�B
7�B
7�B
7�B
7�B
8�B
9$B
9XB
9rB
9�B
9�B
9�B
:DB
:xB
:�B
:�B
;B
;0B
;B
;�B
;�B
;B
;B
;dB
;�B
;�B
<B
<6B
<PB
<�B
="B
=�B
=�B
=�B
>wB
>BB
>�B
?.B
?cB
?�B
?�B
?�B
@ B
@ B
@4B
@iB
@�B
@�B
A B
A�B
BB
B'B
A�B
A�B
BB
B'B
B'B
B�B
BAB
B�B
CaB
C{B
C�B
D3B
D3B
DMB
D�B
D�B
EB
E9B
FB
F?B
F%B
F?B
F?B
FYB
F�B
F�B
G�B
G�B
HB
H�B
H�B
IB
I7B
I7B
IRB
I�B
J=B
J�B
KxB
K�B
K�B
K�B
LB
L�B
L~B
L�B
MB
L�B
M�B
NB
NVB
N�B
O(B
OBB
O\B
O�B
O�B
P.B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
R�B
S[B
S�B
T�B
T�B
T�B
UB
U�B
U�B
U�B
VB
V�B
V�B
V�B
V�B
W?B
WsB
WYB
W?B
W�B
W�B
W�B
W�B
XB
XyB
X�B
YB
Y1B
YB
YKB
YeB
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[#B
[�B
\CB
]B
]/B
]/B
]B
]�B
]�B
]�B
]�B
]�B
]�B
^B
^�B
_!B
_!B
_!B
_�B
_�B
_�B
`\B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aB
`�B
a|B
a�B
a�B
a�B
a�B
a�B
b4B
bhB
b�B
b4B
a�B
b4B
bhB
b�B
cB
cB
c:B
c B
c B
c�B
d@B
d@B
dZB
dtB
dtB
d�B
d�B
e`B
ezB
e�B
e�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h>B
hsB
hXB
h�B
h�B
iB
iB
i*B
iyB
i�B
i�B
j0B
jB
j�B
kB
kB
j�B
kB
k�B
k�B
k�B
lB
l"B
lqB
l�B
l�B
mCB
m]B
m]B
m�B
m�B
m�B
m�B
m�B
nB
nIB
n�B
n�B
o5B
oOB
oOB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
pUB
poB
p�B
qAB
q�B
qAB
q'B
qvB
q�B
r-B
r�B
r�B
r�B
s3B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tnB
t�B
t�B
t�B
u%B
u�B
u�B
u�B
vB
v+B
v+B
v+B
vzB
vFB
v�B
wLB
wfB
wfB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
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
y$B
y$B
y$B
yrB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
zDB
zx1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	jKB	jeB	jB	jKB	jB	j0B	jeB	j0B	j0B	jKB	i�B	i�B	i�B	i�B	j0B	h�B	h
B	fB	c:B	^�B	\�B	\B	\�B	\�B	[�B	[	B	Z7B	YB	X�B	WsB	TB	TFB	P�B	N�B	I�B	@iB	A�B	m�B	�[B	��B	��B	�aB	��B	�B
 �B	��B
�B
\B
oB
E�B
��B
ǔB
ߤB	�BdB%zBz^B}"Bp�By�BVB=�BK�BHfB6+B+�B*�B@�B�MB�B�hB��B�9BnB:B	�B
�<B
i�B
2aB	�FB	�HB	��B	�B	�2B	�sB	��B	��B	w�B	l�B	cnB	OvB	<B	+�B	B	�B	B��B��B�kB�B�B�~BچB��B�&B�B��B�-B��B��B�iB��B�]B��B��B�LB��B��B�3B�5B��B�;B� B̈́B��B�6B�B�cB��B��B�
B�B�*B��B��B�aB�B	 �B	B	�B	�B	"4B	-CB	,"B	F�B	^�B	j�B	s�B	u�B	y$B	sB	t9B	w2B	l�B	c�B	bNB	^�B	bB	q�B	eB	_�B	`�B	d�B	s�B	��B	��B	��B	�UB	��B	�ZB	��B	��B	ǮB	�B	�"B	�6B	ǔB	ȀB	�DB	��B	�=B	ȴB	ʦB	�~B	�oB	��B	��B	�1B	�KB	��B	�	B	�xB	�]B	�~B	ݲB	ݲB	�WB	�kB	�B	�B	�/B	�/B	�dB	�~B	ܒB	ںB	��B	�B	��B	ںB	�WB	��B	��B	ݲB	�IB	یB	�B	רB	ҽB	ӏB	��B	چB	��B	��B	��B	�=B	�WB	�#B	ڠB	�B	�9B	уB	̘B	�1B	��B	�xB	�^B	��B	�DB	�DB	�jB	�(B	�HB	�}B	οB	�BB	�"B	͟B	�VB	ԕB	�gB	�B	��B	��B	� B	ЗB	� B	��B	�HB	�{B	��B	�TB	ΥB	��B	�,B	��B	��B	�B	�B	ԯB	�{B	ԕB	�{B	�,B	�uB	�bB	�HB	��B	ɠB	�B	��B	�gB	�3B	�%B	��B	�zB	�XB	��B	ϫB	�vB	��B	�B	�vB	�B	��B	�(B	��B	�2B	�B	��B	ևB	׍B	�+B	�KB	��B	��B	�=B	�qB	��B	�B	��B	�/B	�IB	�OB	�!B	�!B	�VB	��B	��B	�BB	�B	��B	��B	��B	�B	��B	�4B	�hB	�B	�B	��B	��B	��B	��B	�:B	�&B	�B	�B	��B	�zB	�zB	�zB	�B	�B	�B	��B	��B	�2B	�B	�B	�RB	�B	��B	�
B	�$B	�XB	��B	�yB	�0B	�0B	�0B	�KB	�B	�QB	�QB	�6B	�WB	�B	�B	�CB	��B	��B	�IB	�}B	�;B	�GB	�GB	�;B	�iB	�iB	�B	��B	�!B	�oB	��B	�AB	�'B	�AB	�B	�B	�B	��B	��B	�3B	�3B	��B	�ZB	�fB	��B	�B	�lB	��B	�*B	��B	�B	�B	��B	�jB	��B	��B	��B	��B	��B	�B	�"B	�<B	�qB	��B	��B	��B	�B	��B	�VB	�}B
 B
 4B
 iB
 �B
�B
[B
�B
[B
'B
B
B
AB
[B
uB
�B
GB
-B
-B
-B
MB
MB
B
�B
gB
gB
�B
�B
�B
?B
�B
�B
�B
B
zB
�B
�B
fB
�B
�B
	�B

	B
	�B

=B
B
DB
B

�B
B

�B

�B

�B
DB
�B
�B
JB
�B
B
B
�B
<B
�B
�B
�B
(B
(B
BB
�B
bB
}B
}B
 B
B
�B
�B
B
TB
oB
�B
&B
�B
�B
{B
�B
B
�B
�B
�B
�B
�B
B
�B
sB
�B
B
�B
EB
yB
yB
�B
B
�B
kB
B
�B
�B
�B
xB
dB
�B
�B
�B
�B
�B
�B
�B
 B
 �B
 �B
 �B
 vB
�B
�B
 'B
 BB
 B
 B
 'B
 �B
 vB
�B
;B
�B
!B
"NB
"NB
"�B
"�B
"4B
"B
"B
"B
"�B
#TB
$�B
%`B
%�B
%zB
%�B
%�B
%�B
%zB
%�B
&2B
%zB
&�B
&�B
'�B
'�B
'mB
'�B
'�B
'mB
'mB
'�B
)B
(XB
'�B
'�B
'B
'B
'�B
(>B
(>B
(>B
(sB
(XB
(XB
(�B
(�B
)*B
)B
)*B
)*B
)DB
)*B
*eB
*�B
*�B
+kB
+�B
+�B
+�B
+�B
+�B
,qB
,qB
,�B
-wB
.}B
.�B
/ B
./B
./B
./B
.cB
/OB
/OB
/�B
/�B
0UB
0UB
0UB
0�B
0�B
1B
0�B
1[B
2B
2GB
2aB
2aB
2GB
33B
3�B
3�B
4�B
4�B
5B
5�B
6zB
5�B
5�B
5�B
6B
7�B
7�B
7�B
7�B
7�B
7�B
8�B
9$B
9XB
9rB
9�B
9�B
9�B
:DB
:xB
:�B
:�B
;B
;0B
;B
;�B
;�B
;B
;B
;dB
;�B
;�B
<B
<6B
<PB
<�B
="B
=�B
=�B
=�B
>wB
>BB
>�B
?.B
?cB
?�B
?�B
?�B
@ B
@ B
@4B
@iB
@�B
@�B
A B
A�B
BB
B'B
A�B
A�B
BB
B'B
B'B
B�B
BAB
B�B
CaB
C{B
C�B
D3B
D3B
DMB
D�B
D�B
EB
E9B
FB
F?B
F%B
F?B
F?B
FYB
F�B
F�B
G�B
G�B
HB
H�B
H�B
IB
I7B
I7B
IRB
I�B
J=B
J�B
KxB
K�B
K�B
K�B
LB
L�B
L~B
L�B
MB
L�B
M�B
NB
NVB
N�B
O(B
OBB
O\B
O�B
O�B
P.B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
R�B
S[B
S�B
T�B
T�B
T�B
UB
U�B
U�B
U�B
VB
V�B
V�B
V�B
V�B
W?B
WsB
WYB
W?B
W�B
W�B
W�B
W�B
XB
XyB
X�B
YB
Y1B
YB
YKB
YeB
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[#B
[�B
\CB
]B
]/B
]/B
]B
]�B
]�B
]�B
]�B
]�B
]�B
^B
^�B
_!B
_!B
_!B
_�B
_�B
_�B
`\B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aB
`�B
a|B
a�B
a�B
a�B
a�B
a�B
b4B
bhB
b�B
b4B
a�B
b4B
bhB
b�B
cB
cB
c:B
c B
c B
c�B
d@B
d@B
dZB
dtB
dtB
d�B
d�B
e`B
ezB
e�B
e�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h>B
hsB
hXB
h�B
h�B
iB
iB
i*B
iyB
i�B
i�B
j0B
jB
j�B
kB
kB
j�B
kB
k�B
k�B
k�B
lB
l"B
lqB
l�B
l�B
mCB
m]B
m]B
m�B
m�B
m�B
m�B
m�B
nB
nIB
n�B
n�B
o5B
oOB
oOB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
pUB
poB
p�B
qAB
q�B
qAB
q'B
qvB
q�B
r-B
r�B
r�B
r�B
s3B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tnB
t�B
t�B
t�B
u%B
u�B
u�B
u�B
vB
v+B
v+B
v+B
vzB
vFB
v�B
wLB
wfB
wfB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
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
y$B
y$B
y$B
yrB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
zDB
zx1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104956  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175402  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175402  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175402                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025410  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025410  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                