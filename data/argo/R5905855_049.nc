CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:19:06Z creation;2022-06-04T19:19:07Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604191906  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               1A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @� ���1   @� ���s�@/���l��cc
=p��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���A   A   A@  A^ffA�  A�  A�  A���A���A�  A�  A�  B   B  BffB  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B���B���B�  B���B�  B�  B�  B���B�  Bܙ�B�  B���B�  B�  B�  B�  B�  B�  C   C  C�C�CffC	�fC�fC  C  C  C  C  C  C  C  C  C   C"33C$  C&  C'�fC)�fC+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ33CL  CM�fCO�fCR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D���D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @34@y��@���@���AffA>ffA\��A~ffA�33A�33A�  A�  A�33A�33A�33A�33B��B  B��B��B'34B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B�fgB�  B�33B���B���B���B���B���B���B���B���B���B�fgB���BÙ�B���B���B���Bә�B���B�fgB���B㙚B���B���B���B���B���B���B���C�fC  C  CL�C	��C��C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC"�C#�fC%�fC'��C)��C+��C-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCH  CJ�CK�fCM��CO��CQ�fCS��CU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKs4DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�	A�~A��A��A��A�uA��A��A��A�	A�	A�=A��A�#nA�&LA�&�A�&�A�(XA�(�A�)_A�*eA�,�A�,A�.A�/�A�%zA�`�A̽A̻�A̽�A�یA��cA��cA̪�A˂uA���A�_�A�-Aȋ�A�R�A�&�A�a�A���A�`�A�OA��A���A��A���A�-�A�ffA�
�A� \A���A�t�A��%A�7�A��A��@A��<A���A���A��*A���A�0�A�!-A��A���A�R A�YA�^jA�9�A���A���A�jA��A���A��ZA�B'A�8�A~IRAw�Av�?Av/Au�At�Ao�FAm  AjRTAf�5Ac�A`m]AX�fAT�nAR��AP͟AK#�AEN�AB�uA?Z�A<	lA>4nA>��A<��A87A5"hA1z�A.��A-sA,�+A+uA)��A)%A'�+A&�?A%��A$��A$�zA$�$A$�QA$�A$�A$~A#oA"�A!�`A!C�A �A 1A��A��A!�A!��A!2aA �4Av�A33A��A��A�7A�]A�A��A�AOA��A[�A6�A'RA#�ASA�A� A��ARTA?AeA�DA�A�AVmA�A��Aq�A�8AIRA�A@�A�zAPHA,�A �A��A�AA A��A�XA��AxAq�A*0A
�A
kQA
A	o�A	oA��AM�A%A�cAh
AA!-A�&A{�AY�A6A�A��A�7A7LA�}A�7A+kA��An/A��A��A�uA~�Am�AD�AA˒A�+AVmA�A ��A S�A �@���@�@��E@���@�<�@��-@�$t@��5@��@���@�� @��@���@���@��@��\@�\�@��)@�@���@�|�@��@�L0@��@��[@�9X@��@�{@��]@�q@�2�@�@@�a�@��@�6@��o@��@�4@��@�B�@�@���@�O@쉠@�u@�w2@�f@�Z�@ꅈ@�v�@��@�	�@�h@�~@��d@��@��+@㦵@�7@�S&@�j@�K^@�ƨ@ḻ@��]@�D�@��@ߠ'@�qv@ެ�@�!@��@�&�@܇+@�_@�X@��M@ڨ�@�M@�@ٗ$@�u�@�L�@��@�c�@�s@�P�@�(�@�S@��y@֮}@��@տH@�O�@�&@��'@�p;@�	@��@�E9@ҫ6@��.@���@��K@ю�@�J�@��@У�@��@ϱ[@�\�@���@�xl@�"h@͟V@�?}@�,�@̈�@�YK@� �@��@ˣn@�H�@ʷ�@�YK@���@ɧ�@ɀ4@�'�@Ȏ�@�_�@���@ǩ*@Ǘ�@ǣn@Ǭq@Ǔ�@�ں@ƃ@ƃ�@�`�@�:*@�e@ű[@ŝ�@Ł�@�iD@�%F@���@č�@�J�@�b@��9@íC@ç�@�"�@���@�1�@���@�W?@��B@���@���@�~(@�u@��=@��@�@��5@��A@�GE@���@���@�F�@�ں@��1@�_�@���@��@�Vm@��@���@�j@�)�@��T@��[@�s@�W?@�$t@��9@�h
@�!�@���@���@��@�^�@��B@��R@���@�@�@���@�qv@�@��@��@���@��:@�U�@���@�D�@��@��	@���@�x�@�o @�Z�@�@���@���@�J�@��@�o�@��@��I@�'R@��A@���@��z@��n@�T�@�4@��@��@���@�|�@�j@�?�@��@��@��@���@�g�@�C@�ѷ@�a|@��@�  @��@���@�E9@�@@��@��e@�C-@��@���@�~�@�#�@��E@�9X@��K@���@���@�v�@�C-@�x@��@���@��w@��'@�K�@��@�H�@�l�@�dZ@�RT@�@@��O@�~(@�`�@��@���@�l�@���@�ff@�.�@��@��@�0�@��`@��@���@�q�@�x@��m@�c@�]�@��@���@�	@���@���@�rG@�+@��@���@���@�$�@��@�zx@� i@�ff@�$@��t@�^�@�%F@��@�͟@�($@��@��t@�U�@��@��v@�~(@�Z�@��@�U�@��c@�ȴ@���@��@�V�@���@�b�@�>�@�+�@���@��.@�GE@�@�@�=q@� �@��@���@��@��p@��R@��F@�c�@�;�@��g@�zx@�@��@��?@�xl@�M�@��Q@��*@��'@�l�@���@���@�M@��@��
@���@�zx@�^�@�#�@���@��.@�GE@��@�l�@�4�@���@���@�r�@�Q�@�D�@˒@,�@~�@~v�@~?@~{@}�@|�P@|�_@|(�@{��@{��@{~�@{iD@{]�@{@O@z�2@z@�@z�@zJ@z@y�@y\�@x�@xm�@x(�@w��@w;d@v��@uԕ@u�n@uS&@u�@t��@t�@t[�@t!@t  @sƨ@sg�@s(@r��@rh
@r+k@r�@q��@qm]@q+�@p�|@p�p@p��@p��@p!@o��@oo�@oK�@o=@o�@o
=@n�8@n��@n+k@l��@l9X@k��@k�:@k��@kC@j��@j~�@i��@h�.@h[�@hA�@hM@g��@gA�@f�+@fB[@f$�@f	@e��@e�j@e�H@erG@eY�@eT�@e�@d��@c��@cv`@c�@bL0@a��@a(�@`�@`|�@`,=@_خ@^��@^�!@^�r@^_�@]�n@]4@\��@\�/@\M@[�6@[�:@[Mj@[&@Z��@ZV@Z�@Y��@Ym]@Y \@Xy>@XG@WW?@WE9@V�y@V�s@Vxl@Uԕ@U�H@U�n@U�h@UO�@Ty>@T,=@T�@So�@R��@R�B@Ra|@R@Q��@P��@PC-@O�W@Og�@O@N�,@N�!@M��@MN<@MA @L�P@L�@L�U@L�u@L-�@K�&@K��@KdZ@KF�@K�@J�b@JYK@J($@I��@I��@I�S@IDg@I4@H�@H�.@Gƨ@F�@F�@Fa|@F?@F($@F@E�t@E��@E�@D��@D��@Dg8@D>B@D1'@D$@C�]@C��@C�k@C=@C�@Bں@B�m@B�R@B�\@BW�@A��@A��@Ap�@A0�@A�@A+@A	l@@�@@�@@��@@��@@"h@?�6@?�V@?;d@>�2@>�h@>xl@>B[@>_@=�>@=��@=Y�@<�/@<��@<Z@<,=@;�@;~�@;!-@; i@;@:5?@9�.@9�@9��@9c@9Q�@9%F@9�@8��@8�I@8|�@8Xy@81'@7�@7��@7�q@7v`@733@7o@6��@6�H@6��@6��@6�F@6��@60U@5s�@5(�@4��@4D�@4	�@3��@3�F@3��@3t�@3A�@3�@2�X@2�6@2�1@2q�@2-@2�@1��@1o @1?}@1	l@0�v@0��@0�D@0�@0~(@0u�@0m�@0D�@/�@/y�@/1�@.ȴ@-�@-f�@-?}@-?}@,�@,�@,��@,Q�@,!@+�6@+�@*��@*5?@)�~@)rG@)4@(�5@(��@(��@(4n@'�@'��@'��@'�4@'8@&�h@&_@&�@&�@&�@%�9@%�@%a�@%=�@%�@$�`@$�@$u�@$`�@$G@#�0@#_p@#U�@#�@"�6@"��@"E�@"$�@"_@!@ ��@ �z@ �@\)@&@�c@��@�@�-@k�@5�@�@��@�9@m�@!@�w@Y@�F@xl@n�@J�@	@�9@�@��@rG@;@��@`�@:�@2�@*�@~@	�@�W@�6@~�@W?@F�@@O@C@�@�H@�B@�@�@��@GE@4@�@�M@<6@�@@@�P@�$@�.@S�@1'@�@�}@�*@b�@P�@A�@)_@�M1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�	A�~A��A��A��A�uA��A��A��A�	A�	A�=A��A�#nA�&LA�&�A�&�A�(XA�(�A�)_A�*eA�,�A�,A�.A�/�A�%zA�`�A̽A̻�A̽�A�یA��cA��cA̪�A˂uA���A�_�A�-Aȋ�A�R�A�&�A�a�A���A�`�A�OA��A���A��A���A�-�A�ffA�
�A� \A���A�t�A��%A�7�A��A��@A��<A���A���A��*A���A�0�A�!-A��A���A�R A�YA�^jA�9�A���A���A�jA��A���A��ZA�B'A�8�A~IRAw�Av�?Av/Au�At�Ao�FAm  AjRTAf�5Ac�A`m]AX�fAT�nAR��AP͟AK#�AEN�AB�uA?Z�A<	lA>4nA>��A<��A87A5"hA1z�A.��A-sA,�+A+uA)��A)%A'�+A&�?A%��A$��A$�zA$�$A$�QA$�A$�A$~A#oA"�A!�`A!C�A �A 1A��A��A!�A!��A!2aA �4Av�A33A��A��A�7A�]A�A��A�AOA��A[�A6�A'RA#�ASA�A� A��ARTA?AeA�DA�A�AVmA�A��Aq�A�8AIRA�A@�A�zAPHA,�A �A��A�AA A��A�XA��AxAq�A*0A
�A
kQA
A	o�A	oA��AM�A%A�cAh
AA!-A�&A{�AY�A6A�A��A�7A7LA�}A�7A+kA��An/A��A��A�uA~�Am�AD�AA˒A�+AVmA�A ��A S�A �@���@�@��E@���@�<�@��-@�$t@��5@��@���@�� @��@���@���@��@��\@�\�@��)@�@���@�|�@��@�L0@��@��[@�9X@��@�{@��]@�q@�2�@�@@�a�@��@�6@��o@��@�4@��@�B�@�@���@�O@쉠@�u@�w2@�f@�Z�@ꅈ@�v�@��@�	�@�h@�~@��d@��@��+@㦵@�7@�S&@�j@�K^@�ƨ@ḻ@��]@�D�@��@ߠ'@�qv@ެ�@�!@��@�&�@܇+@�_@�X@��M@ڨ�@�M@�@ٗ$@�u�@�L�@��@�c�@�s@�P�@�(�@�S@��y@֮}@��@տH@�O�@�&@��'@�p;@�	@��@�E9@ҫ6@��.@���@��K@ю�@�J�@��@У�@��@ϱ[@�\�@���@�xl@�"h@͟V@�?}@�,�@̈�@�YK@� �@��@ˣn@�H�@ʷ�@�YK@���@ɧ�@ɀ4@�'�@Ȏ�@�_�@���@ǩ*@Ǘ�@ǣn@Ǭq@Ǔ�@�ں@ƃ@ƃ�@�`�@�:*@�e@ű[@ŝ�@Ł�@�iD@�%F@���@č�@�J�@�b@��9@íC@ç�@�"�@���@�1�@���@�W?@��B@���@���@�~(@�u@��=@��@�@��5@��A@�GE@���@���@�F�@�ں@��1@�_�@���@��@�Vm@��@���@�j@�)�@��T@��[@�s@�W?@�$t@��9@�h
@�!�@���@���@��@�^�@��B@��R@���@�@�@���@�qv@�@��@��@���@��:@�U�@���@�D�@��@��	@���@�x�@�o @�Z�@�@���@���@�J�@��@�o�@��@��I@�'R@��A@���@��z@��n@�T�@�4@��@��@���@�|�@�j@�?�@��@��@��@���@�g�@�C@�ѷ@�a|@��@�  @��@���@�E9@�@@��@��e@�C-@��@���@�~�@�#�@��E@�9X@��K@���@���@�v�@�C-@�x@��@���@��w@��'@�K�@��@�H�@�l�@�dZ@�RT@�@@��O@�~(@�`�@��@���@�l�@���@�ff@�.�@��@��@�0�@��`@��@���@�q�@�x@��m@�c@�]�@��@���@�	@���@���@�rG@�+@��@���@���@�$�@��@�zx@� i@�ff@�$@��t@�^�@�%F@��@�͟@�($@��@��t@�U�@��@��v@�~(@�Z�@��@�U�@��c@�ȴ@���@��@�V�@���@�b�@�>�@�+�@���@��.@�GE@�@�@�=q@� �@��@���@��@��p@��R@��F@�c�@�;�@��g@�zx@�@��@��?@�xl@�M�@��Q@��*@��'@�l�@���@���@�M@��@��
@���@�zx@�^�@�#�@���@��.@�GE@��@�l�@�4�@���@���@�r�@�Q�@�D�@˒@,�@~�@~v�@~?@~{@}�@|�P@|�_@|(�@{��@{��@{~�@{iD@{]�@{@O@z�2@z@�@z�@zJ@z@y�@y\�@x�@xm�@x(�@w��@w;d@v��@uԕ@u�n@uS&@u�@t��@t�@t[�@t!@t  @sƨ@sg�@s(@r��@rh
@r+k@r�@q��@qm]@q+�@p�|@p�p@p��@p��@p!@o��@oo�@oK�@o=@o�@o
=@n�8@n��@n+k@l��@l9X@k��@k�:@k��@kC@j��@j~�@i��@h�.@h[�@hA�@hM@g��@gA�@f�+@fB[@f$�@f	@e��@e�j@e�H@erG@eY�@eT�@e�@d��@c��@cv`@c�@bL0@a��@a(�@`�@`|�@`,=@_خ@^��@^�!@^�r@^_�@]�n@]4@\��@\�/@\M@[�6@[�:@[Mj@[&@Z��@ZV@Z�@Y��@Ym]@Y \@Xy>@XG@WW?@WE9@V�y@V�s@Vxl@Uԕ@U�H@U�n@U�h@UO�@Ty>@T,=@T�@So�@R��@R�B@Ra|@R@Q��@P��@PC-@O�W@Og�@O@N�,@N�!@M��@MN<@MA @L�P@L�@L�U@L�u@L-�@K�&@K��@KdZ@KF�@K�@J�b@JYK@J($@I��@I��@I�S@IDg@I4@H�@H�.@Gƨ@F�@F�@Fa|@F?@F($@F@E�t@E��@E�@D��@D��@Dg8@D>B@D1'@D$@C�]@C��@C�k@C=@C�@Bں@B�m@B�R@B�\@BW�@A��@A��@Ap�@A0�@A�@A+@A	l@@�@@�@@��@@��@@"h@?�6@?�V@?;d@>�2@>�h@>xl@>B[@>_@=�>@=��@=Y�@<�/@<��@<Z@<,=@;�@;~�@;!-@; i@;@:5?@9�.@9�@9��@9c@9Q�@9%F@9�@8��@8�I@8|�@8Xy@81'@7�@7��@7�q@7v`@733@7o@6��@6�H@6��@6��@6�F@6��@60U@5s�@5(�@4��@4D�@4	�@3��@3�F@3��@3t�@3A�@3�@2�X@2�6@2�1@2q�@2-@2�@1��@1o @1?}@1	l@0�v@0��@0�D@0�@0~(@0u�@0m�@0D�@/�@/y�@/1�@.ȴ@-�@-f�@-?}@-?}@,�@,�@,��@,Q�@,!@+�6@+�@*��@*5?@)�~@)rG@)4@(�5@(��@(��@(4n@'�@'��@'��@'�4@'8@&�h@&_@&�@&�@&�@%�9@%�@%a�@%=�@%�@$�`@$�@$u�@$`�@$G@#�0@#_p@#U�@#�@"�6@"��@"E�@"$�@"_@!@ ��@ �z@ �@\)@&@�c@��@�@�-@k�@5�@�@��@�9@m�@!@�w@Y@�F@xl@n�@J�@	@�9@�@��@rG@;@��@`�@:�@2�@*�@~@	�@�W@�6@~�@W?@F�@@O@C@�@�H@�B@�@�@��@GE@4@�@�M@<6@�@@@�P@�$@�.@S�@1'@�@�}@�*@b�@P�@A�@)_@�M1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�]B�(B��B�B��B�(B�(B��B�B��B�B�B�B��B��B�B�(B�B�(B�(B��B�B�BB�(B��B��B	oB	CB	 'B	#�B	/5B	5�B	8�B	6�B	*�B	 'B	�B	B	YB	X_B	S�B	�%B	�	B	��B	�B	��B	��B
�B
�B
[	B
��B
��B
�VB�B
��B
��BEB
��B
�B
�$B
�B
��B
ѷB
��B
�B
��B
�+B
��B
{�B
e�B
k6B
KDB
B
�B

�B
�B
�B
 �B	�NB	�hB	�B	�_B	��B	�BB	�xB	�%B	y�B	o�B	h�B	\]B	O�B	E�B	*KB	B	}B		�B�tB�B��B�B��B	$@B	BAB	8B	#B	�B�LB�B�7B�kB��B�RB��B	-B	[B	�B	�B	�B	B	./B	72B	?�B	HB	R�B	XB	_�B	_;B	bhB	d@B	q'B	HB	��B	�lB	��B	��B	��B	��B	��B	�mB	�B	��B	�hB	��B	��B	�B	�vB	��B	�aB	�gB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	� B	�&B	��B	��B	�UB	��B	��B	��B	��B	�OB	�cB	�}B	��B	��B	��B	��B	�ZB	�zB	��B	�^B	��B	��B	��B	�B	�B	��B	�LB	�^B	��B	��B	�.B	��B	�UB	B	�9B	�fB	ȴB	�B	��B	��B	̘B	��B	��B	�jB	�<B	͟B	бB	ϫB	ҽB	�sB	�B	چB	��B	�xB	߾B	��B	ޞB	�)B	��B	�jB	�pB	��B	��B	߾B	��B	�5B	ބB	�HB	��B	�DB	�qB	��B	�B	�B	�qB	�cB	�-B	��B	�nB	��B	�B	�B	�!B	�B	�$B	�XB	��B	��B	�B	��B	��B	�6B	�B	�$B	�B	�`B	��B	��B	��B	�;B	�B	�aB	�nB	�B	�B	�B	�LB	��B	��B	��B	��B	�B	�B	��B	��B	�B	�[B	��B	�B	�B	�B	�UB	�vB	��B	�B	�aB	�aB	��B	�MB	��B	�MB	��B	��B	��B	�zB	�B	�2B	�2B	�B	�zB	�zB	�`B	�FB	�FB	��B	�2B	��B	��B	��B	�zB	��B	�B	�fB	��B	��B	�RB	��B	�XB	�rB	��B	��B	��B	�>B	�>B	��B	��B	��B	��B	�rB	��B	��B	��B	��B	�DB	��B	��B	�xB	�B	��B	��B	�xB	�DB	�*B	�*B	�B	�*B	�B	��B	��B	��B	�B	��B	�dB	��B	��B	�"B	�VB	�BB
 B
 �B
[B
{B
�B
gB
�B
B
mB
�B
+B
B
�B
�B
�B
�B
B
�B
�B
tB
9B
%B
�B
{B
[B
[B
�B
oB
UB
�B
�B
[B
AB
AB
AB
AB
uB
AB
[B
�B
�B
�B
�B
B
�B
B
-B
-B
aB
�B
�B
�B
�B
�B
�B
�B
B
B
�B
mB
9B
9B
B
mB
�B
�B
?B
%B
�B
?B
B
B
�B
�B
�B
�B
�B
�B
YB
YB
�B
�B
B
zB
�B
fB
	RB
	RB
	7B
	7B
	7B
	�B
	�B
	�B
	�B

rB

�B

�B

�B
)B
DB
)B
)B
^B
xB
�B
�B
B
�B
�B
�B
�B
�B
JB
0B
B
�B
xB
xB
�B
�B
�B

�B
xB
�B
�B
�B
�B
�B
�B
B
�B
jB
�B
�B
�B
�B
�B
(B
�B
�B
�B
B
�B
�B
�B
�B
�B
bB
B
NB
�B
�B
�B
�B
�B
 B
B
�B
�B
�B
�B
2B
gB
B
�B
�B
�B
MB
gB
�B
�B
�B
�B
B
yB
�B
�B
�B
�B
KB
�B
7B
�B
B
]B
�B
�B
IB
�B
�B
;B
pB
�B
 'B
!�B
"B
"hB
"hB
"�B
#�B
#�B
#�B
#�B
#�B
$ZB
%zB
%�B
%zB
%�B
%�B
%�B
&B
&�B
'B
'�B
'�B
(>B
(�B
(�B
)yB
)DB
)*B
)�B
*0B
*�B
+B
+6B
+�B
,B
,B
,"B
,qB
,�B
-B
-B
-�B
.�B
.cB
.�B
/5B
/�B
/�B
/iB
0UB
0�B
1B
1AB
1vB
1vB
1�B
2GB
2aB
2�B
2�B
2�B
33B
33B
33B
33B
3MB
4B
4B
49B
4B
4B
4B
4B
4B
4B
4�B
4�B
5ZB
5�B
5�B
5�B
5�B
6B
6FB
6�B
6�B
7B
7B
7�B
8B
88B
8�B
9	B
9	B
9�B
:^B
:�B
:�B
;dB
;�B
;�B
<�B
<�B
<�B
<�B
<�B
="B
<�B
<�B
<�B
<B
<6B
<�B
=qB
=�B
=�B
>wB
>�B
>(B
?�B
@�B
@�B
@�B
@�B
@�B
A B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
AoB
BuB
B'B
B[B
BAB
B'B
A�B
A�B
A�B
AUB
A�B
BB
A�B
AoB
A�B
AoB
AoB
A B
A B
A�B
A�B
BB
BAB
B'B
B[B
C-B
CGB
C�B
D�B
EB
E�B
E�B
EmB
ESB
E9B
E9B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G+B
G+B
G�B
H�B
H�B
IB
IB
I�B
JXB
J�B
J�B
K)B
KDB
KDB
KB
K�B
K�B
K�B
LB
LB
L0B
LJB
L�B
L�B
M6B
M6B
MPB
M�B
NB
NVB
NVB
N�B
N�B
N�B
N�B
N�B
OBB
O(B
O�B
P�B
P�B
QB
QNB
Q4B
QhB
Q�B
Q�B
RoB
R�B
R�B
R�B
SB
SB
SB
SB
S&B
S[B
S�B
TaB
T{B
T{B
T{B
T�B
T�B
T�B
T�B
T�B
UgB
UMB
UMB
UgB
U�B
U�B
U�B
U�B
VB
VmB
V�B
W�B
W�B
X+B
XyB
X�B
X�B
X�B
Y1B
YKB
Y�B
Y�B
ZB
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
\B
\B
\)B
\]B
\�B
\�B
\�B
\�B
]/B
]IB
]IB
]�B
]�B
]�B
]�B
^B
^B
^B
^B
^B
^OB
^�B
_B
_�B
_�B
_�B
`B
`B
`'B
`'B
`'B
`'B
`BB
`vB
`vB
`�B
`�B
`�B
`�B
`�B
aHB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a|B
abB
a�B
a�B
a�B
bB
c B
c:B
cnB
c�B
dB
c�B
d&B
dtB
d@B
d�B
eFB
eFB
e�B
fLB
fLB
ffB
f�B
f�B
gB
g8B
g8B
g�B
g�B
g�B
g�B
hXB
h�B
h�B
h�B
h�B
h�B
i*B
i_B
i_B
iyB
i�B
i�B
i�B
i�B
i�B
j0B
jeB
jeB
jB
kB
kB
k6B
k6B
kQB
k�B
l"B
lWB
l�B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
oB
o5B
oiB
oOB
o�B
o�B
p;B
qB
q�B
q�B
q�B
q�B
r-B
rGB
r|B
r|B
r�B
sB
sB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
u%B
u?B
u%B
uZB
uZB
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
w2B
wLB
w�B
w�B
xB
xB
x8B
xlB
x�B
xlB
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�]B�(B��B�B��B�(B�(B��B�B��B�B�B�B��B��B�B�(B�B�(B�(B��B�B�BB�(B��B��B	oB	CB	 'B	#�B	/5B	5�B	8�B	6�B	*�B	 'B	�B	B	YB	X_B	S�B	�%B	�	B	��B	�B	��B	��B
�B
�B
[	B
��B
��B
�VB�B
��B
��BEB
��B
�B
�$B
�B
��B
ѷB
��B
�B
��B
�+B
��B
{�B
e�B
k6B
KDB
B
�B

�B
�B
�B
 �B	�NB	�hB	�B	�_B	��B	�BB	�xB	�%B	y�B	o�B	h�B	\]B	O�B	E�B	*KB	B	}B		�B�tB�B��B�B��B	$@B	BAB	8B	#B	�B�LB�B�7B�kB��B�RB��B	-B	[B	�B	�B	�B	B	./B	72B	?�B	HB	R�B	XB	_�B	_;B	bhB	d@B	q'B	HB	��B	�lB	��B	��B	��B	��B	��B	�mB	�B	��B	�hB	��B	��B	�B	�vB	��B	�aB	�gB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	� B	�&B	��B	��B	�UB	��B	��B	��B	��B	�OB	�cB	�}B	��B	��B	��B	��B	�ZB	�zB	��B	�^B	��B	��B	��B	�B	�B	��B	�LB	�^B	��B	��B	�.B	��B	�UB	B	�9B	�fB	ȴB	�B	��B	��B	̘B	��B	��B	�jB	�<B	͟B	бB	ϫB	ҽB	�sB	�B	چB	��B	�xB	߾B	��B	ޞB	�)B	��B	�jB	�pB	��B	��B	߾B	��B	�5B	ބB	�HB	��B	�DB	�qB	��B	�B	�B	�qB	�cB	�-B	��B	�nB	��B	�B	�B	�!B	�B	�$B	�XB	��B	��B	�B	��B	��B	�6B	�B	�$B	�B	�`B	��B	��B	��B	�;B	�B	�aB	�nB	�B	�B	�B	�LB	��B	��B	��B	��B	�B	�B	��B	��B	�B	�[B	��B	�B	�B	�B	�UB	�vB	��B	�B	�aB	�aB	��B	�MB	��B	�MB	��B	��B	��B	�zB	�B	�2B	�2B	�B	�zB	�zB	�`B	�FB	�FB	��B	�2B	��B	��B	��B	�zB	��B	�B	�fB	��B	��B	�RB	��B	�XB	�rB	��B	��B	��B	�>B	�>B	��B	��B	��B	��B	�rB	��B	��B	��B	��B	�DB	��B	��B	�xB	�B	��B	��B	�xB	�DB	�*B	�*B	�B	�*B	�B	��B	��B	��B	�B	��B	�dB	��B	��B	�"B	�VB	�BB
 B
 �B
[B
{B
�B
gB
�B
B
mB
�B
+B
B
�B
�B
�B
�B
B
�B
�B
tB
9B
%B
�B
{B
[B
[B
�B
oB
UB
�B
�B
[B
AB
AB
AB
AB
uB
AB
[B
�B
�B
�B
�B
B
�B
B
-B
-B
aB
�B
�B
�B
�B
�B
�B
�B
B
B
�B
mB
9B
9B
B
mB
�B
�B
?B
%B
�B
?B
B
B
�B
�B
�B
�B
�B
�B
YB
YB
�B
�B
B
zB
�B
fB
	RB
	RB
	7B
	7B
	7B
	�B
	�B
	�B
	�B

rB

�B

�B

�B
)B
DB
)B
)B
^B
xB
�B
�B
B
�B
�B
�B
�B
�B
JB
0B
B
�B
xB
xB
�B
�B
�B

�B
xB
�B
�B
�B
�B
�B
�B
B
�B
jB
�B
�B
�B
�B
�B
(B
�B
�B
�B
B
�B
�B
�B
�B
�B
bB
B
NB
�B
�B
�B
�B
�B
 B
B
�B
�B
�B
�B
2B
gB
B
�B
�B
�B
MB
gB
�B
�B
�B
�B
B
yB
�B
�B
�B
�B
KB
�B
7B
�B
B
]B
�B
�B
IB
�B
�B
;B
pB
�B
 'B
!�B
"B
"hB
"hB
"�B
#�B
#�B
#�B
#�B
#�B
$ZB
%zB
%�B
%zB
%�B
%�B
%�B
&B
&�B
'B
'�B
'�B
(>B
(�B
(�B
)yB
)DB
)*B
)�B
*0B
*�B
+B
+6B
+�B
,B
,B
,"B
,qB
,�B
-B
-B
-�B
.�B
.cB
.�B
/5B
/�B
/�B
/iB
0UB
0�B
1B
1AB
1vB
1vB
1�B
2GB
2aB
2�B
2�B
2�B
33B
33B
33B
33B
3MB
4B
4B
49B
4B
4B
4B
4B
4B
4B
4�B
4�B
5ZB
5�B
5�B
5�B
5�B
6B
6FB
6�B
6�B
7B
7B
7�B
8B
88B
8�B
9	B
9	B
9�B
:^B
:�B
:�B
;dB
;�B
;�B
<�B
<�B
<�B
<�B
<�B
="B
<�B
<�B
<�B
<B
<6B
<�B
=qB
=�B
=�B
>wB
>�B
>(B
?�B
@�B
@�B
@�B
@�B
@�B
A B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
AoB
BuB
B'B
B[B
BAB
B'B
A�B
A�B
A�B
AUB
A�B
BB
A�B
AoB
A�B
AoB
AoB
A B
A B
A�B
A�B
BB
BAB
B'B
B[B
C-B
CGB
C�B
D�B
EB
E�B
E�B
EmB
ESB
E9B
E9B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G+B
G+B
G�B
H�B
H�B
IB
IB
I�B
JXB
J�B
J�B
K)B
KDB
KDB
KB
K�B
K�B
K�B
LB
LB
L0B
LJB
L�B
L�B
M6B
M6B
MPB
M�B
NB
NVB
NVB
N�B
N�B
N�B
N�B
N�B
OBB
O(B
O�B
P�B
P�B
QB
QNB
Q4B
QhB
Q�B
Q�B
RoB
R�B
R�B
R�B
SB
SB
SB
SB
S&B
S[B
S�B
TaB
T{B
T{B
T{B
T�B
T�B
T�B
T�B
T�B
UgB
UMB
UMB
UgB
U�B
U�B
U�B
U�B
VB
VmB
V�B
W�B
W�B
X+B
XyB
X�B
X�B
X�B
Y1B
YKB
Y�B
Y�B
ZB
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
\B
\B
\)B
\]B
\�B
\�B
\�B
\�B
]/B
]IB
]IB
]�B
]�B
]�B
]�B
^B
^B
^B
^B
^B
^OB
^�B
_B
_�B
_�B
_�B
`B
`B
`'B
`'B
`'B
`'B
`BB
`vB
`vB
`�B
`�B
`�B
`�B
`�B
aHB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a|B
abB
a�B
a�B
a�B
bB
c B
c:B
cnB
c�B
dB
c�B
d&B
dtB
d@B
d�B
eFB
eFB
e�B
fLB
fLB
ffB
f�B
f�B
gB
g8B
g8B
g�B
g�B
g�B
g�B
hXB
h�B
h�B
h�B
h�B
h�B
i*B
i_B
i_B
iyB
i�B
i�B
i�B
i�B
i�B
j0B
jeB
jeB
jB
kB
kB
k6B
k6B
kQB
k�B
l"B
lWB
l�B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
oB
o5B
oiB
oOB
o�B
o�B
p;B
qB
q�B
q�B
q�B
q�B
r-B
rGB
r|B
r|B
r�B
sB
sB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
u%B
u?B
u%B
uZB
uZB
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
w2B
wLB
w�B
w�B
xB
xB
x8B
xlB
x�B
xlB
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105237  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191906  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191906  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191907                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041914  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041914  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                