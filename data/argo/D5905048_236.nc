CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-02T00:35:38Z creation;2018-05-02T00:35:43Z conversion to V3.1;2019-12-19T07:39:20Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180502003538  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_236                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�_s�F�1   @�_t��-�@4ȭ��U��dMS&�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�3D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B(  B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCP  CQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D)  D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�� D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D�  D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D��fD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aȏ\Aț�A���A�\)A�x�A�K�A�;dA�|�A��/Aǝ�AǗ�AǕ�AǑhA�x�A�l�A�VA�"�A��A�$�A�=qA�jA�VA���A�K�A�+A�ƨAś�A�ȴA�5?A���Aź^A�~�A�M�Ać+A�5?A�{A�ffA��A�`BA�ȴA�9XA��^A��A�33A�`BA�ƨA���A�1'A��A��RA�ȴA�jA�ĜA��hA�?}A�M�A�\)A�7LA��A��RA��\A�K�A��A�VA�C�A��7A�VA���A�XA�r�A�oA��`A�|�A�|�A�VA��+A��uA�A�jA�JA��#A�"�A��TA��A���A���A��wA�I�A��^A���A�G�A� �A�v�A�(�A��;A�A�A��A�;dA�^5A�v�A���A��PA��A�?}A�p�A��/A�S�A��FA���A�bNA���A���A�ffA�S�A�1A��A���A�z�A���A�hA~{A|�Az�DAxZAv�+Au�
At-Ar�AqK�Aq%Ao�TAn��Am��Al�jAl^5AkoAh�Ae��Ac�
Aa|�A_�A]�^A\��A[�AY/AV��AR��AQK�AO7LAM&�ALr�AKoAH��AF��AFz�AF{AD�HAD �AC�AA��A@bA?G�A>ĜA>(�A=�TA=XA<��A:�RA8^5A5��A4^5A3�
A2ffA0$�A.�jA-��A*ffA)O�A(��A'A& �A%��A%|�A$^5A"n�A (�AC�A��A�A�A�AO�A�AZA�AJA&�A�TA��A{A�AM�A�A�wA��A�A
�jA
9XA��AE�AdZA�A��A�Av�AVAt�A ��A ��A -@��!@�?}@��@��y@�p�@���@�~�@�V@��`@�z�@�;d@�ȴ@��@�j@�9@�$�@�%@��;@�^@�`B@䛦@�P@�ff@��#@��@�hs@�j@���@��@݁@��`@�9X@�|�@�+@��H@�=q@��@��T@ف@�V@���@�C�@֟�@�G�@�(�@Ӿw@�dZ@���@�O�@�V@�j@���@�v�@͙�@���@̋D@�"�@ʏ\@���@ȣ�@���@őh@���@�1'@�
=@���@�ȴ@���@�$�@��@��@��D@�t�@��\@��^@���@�Z@�  @��@�t�@�C�@�o@�M�@�`B@���@��@���@��-@�Ĝ@�  @�"�@�~�@�^5@�V@�M�@��@�G�@�I�@�ƨ@�C�@��@�^5@��@�@���@�1'@�  @�S�@�ȴ@���@�M�@��T@�p�@���@�z�@�9X@��@�t�@�;d@�+@���@��@���@���@���@�v�@�$�@�`B@�7L@��@�Ĝ@���@�r�@�Z@�I�@�1'@� �@�b@�  @��;@��w@��P@�\)@���@���@���@���@��R@�v�@�M�@��@�X@�/@��@��@��9@�(�@���@��
@��
@�ƨ@��@��P@��y@��+@�$�@��7@�`B@�?}@�/@��@���@��/@���@�A�@���@�9X@��9@��/@�%@�p�@���@��7@�X@�%@���@�Z@� �@�t�@��@��9@��u@�Q�@��@���@��m@���@���@�K�@�
=@���@�ȴ@��+@�M�@�^5@��\@��@��^@���@�X@���@�Ĝ@���@��D@�bN@��D@�z�@��;@��@��F@���@�t�@�K�@�"�@�@���@�v�@�J@�@���@��@��-@�O�@�1'@��F@���@�;d@�;d@�33@�"�@��!@�@��@���@�5?@�5?@�-@�{@��@��h@��@�G�@���@���@�j@�r�@�bN@�Q�@�I�@�A�@�9X@��@��P@�+@��H@��+@�=q@�{@��^@�`B@�?}@��@�%@��`@��j@�Z@��;@��@�|�@�l�@�K�@���@���@�~�@�E�@�$�@��@���@���@�p�@�G�@��@���@��@��@��@��@�dZ@�dZ@�C�@�33@�@�ȴ@���@���@���@���@��!@��+@�~�@�ff@�=q@�J@��-@�hs@�G�@�V@��@��/@�Ĝ@��u@��D@��u@��@�9X@�b@�1@�1@��@��@��@�w@K�@~�y@~��@~��@~�+@~{@}�@}�T@}p�@}�@|��@|��@|I�@|1@{�m@{�
@{ƨ@{��@{�@{dZ@{"�@z^5@y�#@yhs@yhs@y&�@x��@xbN@xb@w�;@w��@w+@v��@u��@st�@r��@r��@r��@r�\@r-@q��@q�7@qhs@q%@pQ�@o�w@o�P@o\)@o�@n��@n��@nv�@m�T@l�@lZ@l9X@l1@k��@kdZ@j�@j��@jM�@i��@i7L@hA�@g|�@f�y@fff@f@e��@e�-@e`B@e�@d��@dz�@dj@dj@d�@c��@c�m@c�m@cƨ@c�F@c�@cC�@co@c@b�@b~�@a��@aX@a7L@`�`@`�9@_�;@_��@_K�@^v�@^{@]�h@]/@\�@\��@\j@\9X@\(�@[��@[�@[S�@[33@Z��@Z^5@Z-@ZJ@Y�@Yhs@X�9@XbN@W�@W�P@W+@V�@V��@VV@U�T@U�h@Up�@UO�@T�@Tj@T(�@S�m@S��@S��@S�
@S33@R~�@R�H@S@R��@Rn�@RM�@Q��@Q��@QX@Q%@P��@P��@Pr�@O��@O��@O;d@N�@N5?@M�h@L��@L�@L�/@Lz�@LZ@L9X@L9X@L9X@L1@K�
@K��@Ko@J��@I�#@I��@IG�@I&�@H��@H�@HA�@G�@G+@F�@F$�@E��@E@E��@E`B@D��@Dj@C��@C"�@B��@B�\@B^5@B�@A��@@��@@ �@?��@?+@>�R@>V@>@=�-@=/@<�/@<�j@<�@<��@<�D@<Z@<1@;�@;33@:�H@:^5@:�@9��@9X@8��@8�@8b@8  @7��@7��@7l�@7K�@7;d@7+@6��@6��@6E�@5�@5�h@5/@4j@3ƨ@3dZ@3S�@3S�@3dZ@3dZ@3dZ@3S�@3o@2�@2��@2�\@2-@1�^@1G�@1�@0��@0�`@0��@0��@0bN@01'@0  @/�@/
=@.�+@.5?@-@-O�@,�/@,��@,�@,��@,z�@,j@,j@,(�@+ƨ@+�@+C�@+"�@*�@*~�@*M�@*J@)x�@)%@(�@(bN@(bN@(Q�@(A�@(b@'�@'��@'�w@'l�@&��@&ȴ@&v�@&E�@&$�@%�@%�T@%��@%`B@$��@$��@$�D@$z�@$z�@$I�@#��@#"�@#o@"��@"^5@"M�@"J@!�#@!��@!��@!hs@ ��@ �9@ ��@ �u@ �@ bN@ A�@  �@ b@ b@ b@ b@�@�@��@�w@��@|�@;d@�y@��@ff@E�@$�@�@��@`B@O�@?}@V@��@�@�/@�@j@(�@(�@(�@(�@1@�
@��@S�@o@��@^5@�@J@J@J@��@��@��@-@��@J@J@�@J@�@��@��@x�@X@&�@%@��@��@bN@Q�@�@��@��@�P@�P@|�@l�@l�@K�@�@��@��@�@ȴ@v�@V@$�@{@@��@�@O�@/@��@�@I�@�@�m@��@S�@"�@@��@�\@M�@-@J@�^@�7@X@��@��@�@b@�;@�@l�@;d@��@ff@E�@5?@5?@$�@�@�T@�T@�T@��@p�@/@�/@�@z�@j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aȏ\Aț�A���A�\)A�x�A�K�A�;dA�|�A��/Aǝ�AǗ�AǕ�AǑhA�x�A�l�A�VA�"�A��A�$�A�=qA�jA�VA���A�K�A�+A�ƨAś�A�ȴA�5?A���Aź^A�~�A�M�Ać+A�5?A�{A�ffA��A�`BA�ȴA�9XA��^A��A�33A�`BA�ƨA���A�1'A��A��RA�ȴA�jA�ĜA��hA�?}A�M�A�\)A�7LA��A��RA��\A�K�A��A�VA�C�A��7A�VA���A�XA�r�A�oA��`A�|�A�|�A�VA��+A��uA�A�jA�JA��#A�"�A��TA��A���A���A��wA�I�A��^A���A�G�A� �A�v�A�(�A��;A�A�A��A�;dA�^5A�v�A���A��PA��A�?}A�p�A��/A�S�A��FA���A�bNA���A���A�ffA�S�A�1A��A���A�z�A���A�hA~{A|�Az�DAxZAv�+Au�
At-Ar�AqK�Aq%Ao�TAn��Am��Al�jAl^5AkoAh�Ae��Ac�
Aa|�A_�A]�^A\��A[�AY/AV��AR��AQK�AO7LAM&�ALr�AKoAH��AF��AFz�AF{AD�HAD �AC�AA��A@bA?G�A>ĜA>(�A=�TA=XA<��A:�RA8^5A5��A4^5A3�
A2ffA0$�A.�jA-��A*ffA)O�A(��A'A& �A%��A%|�A$^5A"n�A (�AC�A��A�A�A�AO�A�AZA�AJA&�A�TA��A{A�AM�A�A�wA��A�A
�jA
9XA��AE�AdZA�A��A�Av�AVAt�A ��A ��A -@��!@�?}@��@��y@�p�@���@�~�@�V@��`@�z�@�;d@�ȴ@��@�j@�9@�$�@�%@��;@�^@�`B@䛦@�P@�ff@��#@��@�hs@�j@���@��@݁@��`@�9X@�|�@�+@��H@�=q@��@��T@ف@�V@���@�C�@֟�@�G�@�(�@Ӿw@�dZ@���@�O�@�V@�j@���@�v�@͙�@���@̋D@�"�@ʏ\@���@ȣ�@���@őh@���@�1'@�
=@���@�ȴ@���@�$�@��@��@��D@�t�@��\@��^@���@�Z@�  @��@�t�@�C�@�o@�M�@�`B@���@��@���@��-@�Ĝ@�  @�"�@�~�@�^5@�V@�M�@��@�G�@�I�@�ƨ@�C�@��@�^5@��@�@���@�1'@�  @�S�@�ȴ@���@�M�@��T@�p�@���@�z�@�9X@��@�t�@�;d@�+@���@��@���@���@���@�v�@�$�@�`B@�7L@��@�Ĝ@���@�r�@�Z@�I�@�1'@� �@�b@�  @��;@��w@��P@�\)@���@���@���@���@��R@�v�@�M�@��@�X@�/@��@��@��9@�(�@���@��
@��
@�ƨ@��@��P@��y@��+@�$�@��7@�`B@�?}@�/@��@���@��/@���@�A�@���@�9X@��9@��/@�%@�p�@���@��7@�X@�%@���@�Z@� �@�t�@��@��9@��u@�Q�@��@���@��m@���@���@�K�@�
=@���@�ȴ@��+@�M�@�^5@��\@��@��^@���@�X@���@�Ĝ@���@��D@�bN@��D@�z�@��;@��@��F@���@�t�@�K�@�"�@�@���@�v�@�J@�@���@��@��-@�O�@�1'@��F@���@�;d@�;d@�33@�"�@��!@�@��@���@�5?@�5?@�-@�{@��@��h@��@�G�@���@���@�j@�r�@�bN@�Q�@�I�@�A�@�9X@��@��P@�+@��H@��+@�=q@�{@��^@�`B@�?}@��@�%@��`@��j@�Z@��;@��@�|�@�l�@�K�@���@���@�~�@�E�@�$�@��@���@���@�p�@�G�@��@���@��@��@��@��@�dZ@�dZ@�C�@�33@�@�ȴ@���@���@���@���@��!@��+@�~�@�ff@�=q@�J@��-@�hs@�G�@�V@��@��/@�Ĝ@��u@��D@��u@��@�9X@�b@�1@�1@��@��@��@�w@K�@~�y@~��@~��@~�+@~{@}�@}�T@}p�@}�@|��@|��@|I�@|1@{�m@{�
@{ƨ@{��@{�@{dZ@{"�@z^5@y�#@yhs@yhs@y&�@x��@xbN@xb@w�;@w��@w+@v��@u��@st�@r��@r��@r��@r�\@r-@q��@q�7@qhs@q%@pQ�@o�w@o�P@o\)@o�@n��@n��@nv�@m�T@l�@lZ@l9X@l1@k��@kdZ@j�@j��@jM�@i��@i7L@hA�@g|�@f�y@fff@f@e��@e�-@e`B@e�@d��@dz�@dj@dj@d�@c��@c�m@c�m@cƨ@c�F@c�@cC�@co@c@b�@b~�@a��@aX@a7L@`�`@`�9@_�;@_��@_K�@^v�@^{@]�h@]/@\�@\��@\j@\9X@\(�@[��@[�@[S�@[33@Z��@Z^5@Z-@ZJ@Y�@Yhs@X�9@XbN@W�@W�P@W+@V�@V��@VV@U�T@U�h@Up�@UO�@T�@Tj@T(�@S�m@S��@S��@S�
@S33@R~�@R�H@S@R��@Rn�@RM�@Q��@Q��@QX@Q%@P��@P��@Pr�@O��@O��@O;d@N�@N5?@M�h@L��@L�@L�/@Lz�@LZ@L9X@L9X@L9X@L1@K�
@K��@Ko@J��@I�#@I��@IG�@I&�@H��@H�@HA�@G�@G+@F�@F$�@E��@E@E��@E`B@D��@Dj@C��@C"�@B��@B�\@B^5@B�@A��@@��@@ �@?��@?+@>�R@>V@>@=�-@=/@<�/@<�j@<�@<��@<�D@<Z@<1@;�@;33@:�H@:^5@:�@9��@9X@8��@8�@8b@8  @7��@7��@7l�@7K�@7;d@7+@6��@6��@6E�@5�@5�h@5/@4j@3ƨ@3dZ@3S�@3S�@3dZ@3dZ@3dZ@3S�@3o@2�@2��@2�\@2-@1�^@1G�@1�@0��@0�`@0��@0��@0bN@01'@0  @/�@/
=@.�+@.5?@-@-O�@,�/@,��@,�@,��@,z�@,j@,j@,(�@+ƨ@+�@+C�@+"�@*�@*~�@*M�@*J@)x�@)%@(�@(bN@(bN@(Q�@(A�@(b@'�@'��@'�w@'l�@&��@&ȴ@&v�@&E�@&$�@%�@%�T@%��@%`B@$��@$��@$�D@$z�@$z�@$I�@#��@#"�@#o@"��@"^5@"M�@"J@!�#@!��@!��@!hs@ ��@ �9@ ��@ �u@ �@ bN@ A�@  �@ b@ b@ b@ b@�@�@��@�w@��@|�@;d@�y@��@ff@E�@$�@�@��@`B@O�@?}@V@��@�@�/@�@j@(�@(�@(�@(�@1@�
@��@S�@o@��@^5@�@J@J@J@��@��@��@-@��@J@J@�@J@�@��@��@x�@X@&�@%@��@��@bN@Q�@�@��@��@�P@�P@|�@l�@l�@K�@�@��@��@�@ȴ@v�@V@$�@{@@��@�@O�@/@��@�@I�@�@�m@��@S�@"�@@��@�\@M�@-@J@�^@�7@X@��@��@�@b@�;@�@l�@;d@��@ff@E�@5?@5?@$�@�@�T@�T@�T@��@p�@/@�/@�@z�@j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
ffB
t�B
��B
��B
��BuB+B
��B
�B
�yB
�sB
�sB
�mB
�sB
�yB
�sB
�B
��B
��BoB#�B(�B.B/B.B2-BI�Bo�B�uB�-BĜB��B��B�BB�B��B+BJB{B�B!�B&�B(�B2-B0!B9XB5?B.B(�B(�B33B0!B6FB6FB<jBG�B=qBI�BM�BYBXBS�BM�BK�BT�BW
BW
BO�BE�BG�BJ�BB�B33B:^B49B%�B�B%�B�B1B
=B�B\BB��B�B��B�B�NB�TB�TBǮB�bB�JB��B��B�oBe`Bl�B_;BA�B�B{B	7B
��B
��B
�wB
�qB
�^B
��B
��B
��B
�=B
r�B
^5B
G�B
?}B
>wB
+B
$�B
{B

=B	��B	�B	�B	�TB	�/B	��B	��B	ǮB	�XB	�3B	�B	�!B	��B	�JB	�1B	{�B	l�B	^5B	_;B	T�B	J�B	+B	�B��B	1B	B�B��B�yB��B��B�B�sB�B�B�
B��BɺBƨB��BȴBɺB��B�9B��B�\B�\B�bB��B�=By�Bx�B|�BbNBu�B~�Br�Bw�B� Bx�BjBZB\)Bk�Br�BffBo�Bt�Bs�Bk�B^5B^5B_;B_;B[#BVB[#BVBR�BJ�BJ�BK�BF�BVBQ�BG�BK�BF�B9XB7LBM�BP�BS�BL�BJ�B[#BT�BN�BP�BT�BVBR�BR�BYBZBdZBaHB\)B^5BYBN�BD�BQ�BhsBgmBe`Bu�Bs�Bs�Bu�B{�B}�B� B{�Bv�B|�B�B�B�B�+B�JB�PB�PB�hB�hB�VB�=B�1B�JB�JB�1B�JB��B��B�\B��B��B��B��B��B��B��B��B��B�B��B��B��B�B�?B�FB�LBĜBǮBŢBB��B��BɺBƨBɺB��B��B��B�B�#B�5B�;B�;B�)B�/B�BB�/B�B�HB�mB�B�B��B	B	%B	B	B��B	B	DB	\B	{B	{B	�B	�B	�B	�B	�B	�B	!�B	)�B	+B	.B	0!B	33B	7LB	:^B	<jB	=qB	A�B	E�B	D�B	E�B	E�B	H�B	H�B	G�B	G�B	G�B	O�B	R�B	R�B	W
B	ZB	[#B	\)B	\)B	]/B	^5B	^5B	^5B	_;B	aHB	bNB	cTB	hsB	jB	iyB	iyB	hsB	jB	jB	iyB	r�B	t�B	t�B	q�B	r�B	y�B	z�B	|�B	{�B	|�B	|�B	z�B	}�B	�B	�B	�1B	�=B	�PB	�\B	�bB	�hB	�uB	�{B	��B	��B	�B	�B	�B	�B	�'B	�!B	�-B	�-B	�9B	�'B	�'B	�9B	�B	�B	�FB	�FB	�LB	�^B	��B	��B	��B	B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�;B	�HB	�;B	�NB	�ZB	�`B	�`B	�fB	�fB	�fB	�`B	�sB	�sB	�B	�B	�B	�yB	�fB	�HB	�mB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
  B	��B
  B
B
B
B
+B
%B
%B
1B
	7B

=B
	7B
1B
+B
%B

=B
DB
JB
JB
JB
JB
VB
VB
\B
\B
\B
bB
\B
\B
\B
VB
VB
PB
DB

=B
JB
\B
VB
VB
VB
VB
bB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
"�B
!�B
!�B
 �B
!�B
"�B
"�B
 �B
�B
�B
�B
!�B
#�B
$�B
%�B
$�B
%�B
%�B
&�B
$�B
#�B
$�B
&�B
&�B
&�B
&�B
%�B
%�B
$�B
"�B
%�B
'�B
'�B
&�B
'�B
&�B
'�B
&�B
&�B
%�B
%�B
&�B
(�B
(�B
)�B
-B
-B
,B
,B
,B
.B
/B
/B
.B
/B
0!B
0!B
/B
/B
/B
.B
/B
/B
/B
-B
,B
.B
.B
.B
-B
,B
.B
.B
,B
.B
.B
/B
0!B
0!B
0!B
1'B
1'B
1'B
0!B
1'B
1'B
0!B
0!B
1'B
2-B
1'B
/B
/B
1'B
1'B
2-B
33B
33B
49B
33B
33B
49B
5?B
5?B
49B
49B
5?B
5?B
6FB
7LB
7LB
6FB
5?B
:^B
:^B
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
:^B
;dB
;dB
;dB
:^B
;dB
;dB
>wB
>wB
=qB
>wB
?}B
?}B
?}B
>wB
=qB
=qB
;dB
<jB
;dB
>wB
>wB
?}B
?}B
?}B
?}B
>wB
?}B
@�B
?}B
A�B
C�B
B�B
B�B
@�B
B�B
A�B
C�B
D�B
E�B
E�B
E�B
D�B
C�B
E�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
K�B
L�B
L�B
K�B
K�B
J�B
J�B
K�B
K�B
K�B
L�B
K�B
M�B
L�B
M�B
M�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
O�B
N�B
N�B
O�B
N�B
N�B
M�B
O�B
R�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
T�B
T�B
S�B
S�B
S�B
S�B
VB
VB
W
B
XB
W
B
VB
W
B
W
B
VB
T�B
VB
W
B
VB
W
B
XB
ZB
ZB
ZB
ZB
ZB
ZB
YB
YB
ZB
[#B
[#B
[#B
ZB
[#B
[#B
ZB
\)B
]/B
_;B
`BB
_;B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
`BB
_;B
bNB
bNB
cTB
bNB
aHB
_;B
aHB
cTB
bNB
bNB
dZB
dZB
dZB
dZB
e`B
dZB
dZB
dZB
ffB
ffB
ffB
e`B
ffB
ffB
ffB
gmB
gmB
ffB
ffB
gmB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
hsB
hsB
hsB
iyB
iyB
iyB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
jB
k�B
k�B
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
m�B
n�B
o�B
n�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
r�B
r�B
q�B
r�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
u�B
v�B
v�B
v�B
u�B
v�B
v�B
v�B
u�B
u�B
u�B
u�B
v�B
w�B
x�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
f�B
uB
�TB
�{B
��B�B�B
��B
�B
�B
�B
�B
�B
�B
�B
��B
�B
��B
��BTB$&B)�B/ B/�B.�B2aBI�BoB��B��B�9BΥB�hB��B�/B�>B1B�B�B �B# B(sB+B3�B1AB9�B6FB/�B+B*�B4B1vB6�B6�B<�BHB?�BKDBOBBYBX�BT�BOvBN"BV�BXBW�BQBGzBH�BK^BC�B5ZB;dB5�B'�B vB&�B�B
�B�BB}BB��B��B��B�%B�ZB�tB�&B��B��B��B�KB��B�{Bi_Bn�Ba|BD�B!�B�BjB
� B
�RB
��B
�OB
�"B
�B
��B
��B
�6B
v+B
a�B
K�B
BB
@OB
-�B
&�B
�B
dB	�}B	�B	��B	�`B	޸B	ҽB	ӏB	�7B	��B	�nB	�UB	�'B	�$B	�B	��B	~�B	o�B	`�B	`�B	V�B	L�B	./B	 �B��B	
=B	�B�GB��B�B��B�B��B�*BٚB�YB�yB͹BˬB��BϫBɠB�=B��B��B��B��B�TB�TB��B�~B|�Bz�B~�BfLBw2B� Bt�Bx�B��By�BlqB\�B^�Bl�Bs�Bh�Bp�ButBt�BmB`�B`B`�B`�B\�BW�B\)BW�BTaBL�BL~BMjBH�BV�BSBIlBL�BHKB;�B9�BN�BQ�BT{BNBK�B[=BU�BP.BQ�BVBW
BTBTFBY�B[#Bd�Ba�B]B^�BZBP}BGBS�BiBhXBf�Bv+Bt�Bt�Bv�B|PB~BB�iB|�Bw�B}�B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�RB�6B��B�B��B��B�B�IB�CB��B�jB�HB�fB��B�kB��B��B�8B�B��B��B�B��B��B��B�B�[B��B�=BǔBʌB�pB҉B�{B�BیBބBߊBߤB��B�B��B�B�#B�4B�>B�5B�hB�dB	3B	?B	SB	�B��B	�B	�B	�B	�B	B	�B	#B	EB	+B	�B	OB	"4B	*KB	+QB	.cB	0�B	3�B	7�B	:�B	<�B	=�B	A�B	E�B	D�B	E�B	E�B	H�B	H�B	G�B	G�B	HKB	PB	S&B	S[B	W?B	ZQB	[=B	\CB	\CB	]dB	^OB	^OB	^jB	_�B	a|B	b�B	c�B	h�B	j�B	i�B	i�B	h�B	j�B	j�B	jB	r�B	t�B	t�B	rB	s3B	zB	{B	|�B	|B	}"B	}<B	{dB	~]B	�oB	�uB	�fB	�XB	�jB	��B	�}B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�'B	�UB	�aB	�aB	�nB	��B	��B	��B	�!B	�B	�`B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�\B	�B	�,B	�@B	�@B	�SB	�?B	�EB	�QB	�;B	�|B	߾B	�hB	�tB	�zB	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	�B	��B	��B	�B	��B	��B	�B	�)B	��B	��B	��B	��B	��B	��B	�	B	�$B	�B	�6B	�B	�<B
 4B
B
AB
AB
'B
'B
AB
 OB	�HB
 OB
AB
uB
gB
EB
�B
tB
fB
	RB

rB
	RB
�B
zB
�B

rB
^B
dB
~B
~B
~B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
�B
\B
pB
pB
�B
�B
}B
oB
gB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
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
B
B
�B
 �B
"�B
!�B
!�B
 �B
!�B
"�B
"�B
 �B
 B
OB
]B
"B
$B
$�B
%�B
%B
%�B
&B
'B
%B
$@B
%B
'B
'B
'B
'B
&2B
%�B
%,B
#TB
&B
($B
($B
'B
(
B
'8B
($B
'B
'B
&LB
&LB
'8B
)*B
)*B
*0B
-)B
-)B
,=B
,=B
,=B
.IB
/B
/5B
.IB
/5B
0;B
0!B
/5B
/OB
/OB
./B
/OB
/OB
/5B
-]B
,WB
.IB
./B
.IB
-CB
,WB
./B
.IB
,qB
.IB
.cB
/5B
0;B
0;B
0UB
1[B
1[B
1[B
0UB
1AB
1AB
0oB
0UB
1AB
2aB
1[B
/iB
/�B
1AB
1vB
2|B
3hB
3MB
4nB
3hB
3�B
4nB
5tB
5ZB
4�B
4nB
5tB
5tB
6`B
7LB
7LB
6�B
5�B
:DB
:^B
9�B
9rB
:�B
:�B
:�B
;�B
;B
;B
;B
;B
:�B
;�B
;�B
;�B
:�B
;�B
;�B
>�B
>�B
=�B
>�B
?�B
?}B
?�B
>�B
=�B
=�B
;�B
<�B
;�B
>�B
>�B
?�B
?�B
?�B
?�B
>�B
?�B
@�B
?�B
A�B
C�B
B�B
B�B
@�B
B�B
A�B
C�B
D�B
E�B
E�B
E�B
D�B
DB
E�B
F�B
G�B
G�B
IB
I�B
I�B
J	B
J�B
K�B
L�B
L�B
K�B
K�B
KB
J�B
K�B
K�B
LB
MB
LB
NB
MB
NB
M�B
O�B
O�B
O�B
O�B
O�B
P�B
PB
O�B
O(B
N�B
O�B
OB
OB
N"B
PB
S&B
UB
T�B
UB
UB
UB
UB
TB
U2B
U2B
TB
T,B
T,B
TFB
VB
VB
W
B
XB
W$B
VB
W?B
W?B
V9B
UMB
V9B
W?B
V9B
W?B
XEB
Z7B
Z7B
ZB
ZQB
ZB
Z7B
YKB
YKB
Z7B
[=B
[WB
[=B
ZQB
[=B
[=B
ZQB
\]B
]~B
_VB
`BB
_VB
_pB
_VB
_VB
_pB
_VB
^jB
^jB
_VB
_VB
`\B
abB
abB
a|B
abB
`�B
_pB
bNB
b�B
cTB
bhB
abB
_�B
a|B
cnB
bhB
b�B
dtB
dtB
dtB
dtB
ezB
d�B
d�B
d�B
ffB
f�B
ffB
e�B
f�B
f�B
f�B
gmB
g�B
f�B
f�B
g�B
f�B
ffB
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
hsB
h�B
h�B
h�B
h�B
h�B
iyB
iyB
iyB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
jB
j�B
j�B
j�B
jB
k�B
k�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
m�B
n�B
o�B
n�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
r�B
r�B
q�B
r�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
u�B
v�B
v�B
v�B
u�B
v�B
v�B
v�B
u�B
u�B
u�B
u�B
v�B
xB
x�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.1(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805060036372018050600363720180506003637201806221329552018062213295520180622132955201806042132162018060421321620180604213216  JA  ARFMdecpA19c                                                                20180502093520  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180502003538  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180502003541  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180502003541  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180502003542  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180502003542  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180502003542  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180502003542  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180502003543  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180502003543                      G�O�G�O�G�O�                JA  ARUP                                                                        20180502005659                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180502153623  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20180505153637  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180505153637  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604123216  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042955  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                