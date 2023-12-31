CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-20T21:35:14Z creation;2016-09-20T21:35:16Z conversion to V3.1;2019-12-19T08:29:56Z update;     
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
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160920213514  20200115111516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               'A   JA  I2_0576_039                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��s0*z 1   @��s��� @;"T`�d��dqݗ�+k1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�<�DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D���D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ Dڼ�D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�D�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC  C�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%��C'��C)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� D���D�<�D�|�D�� D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�9�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڹ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�fD�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�S�A�S�A�VA�VA�S�A�O�A�M�A�M�A�M�A�O�A�O�A�K�A�C�A�(�A�K�AϮA��yA�5?A�\)A�Q�A�ĜAß�AËDA�r�AA��9A�M�A��A�ƨA�9XA��DA�JA��A�E�A�K�A�  A�p�A���A�&�A�1A�{A�A�A�/A�;dA��A��!A�A��^A���A�|�A�Q�A�9XA�hsA���A�M�A���A��A��9A�hsA��wA�dZA�;dA���A��7A�(�A��A�5?A�5?A���A�G�A��A�XA���A�/A�v�A�v�A���A�A��
A�/A�A~v�A|�DAzI�Axn�Aw�#Aw��Aw&�AuƨAt�9As�Ar~�ApA�An��AmXAl�Ai�Ai�AgAe�AcAahsA`9XA^��A^  A]�A\�AZ^5AY
=AYS�AW�mAV1AUS�AT�AS+ASO�AS%AR��ARZAQ��AP5?AO�-AOhsAN�uAN{ALJAJ1'AI�hAG�AG�AH�AG�;AF��AF{AE7LADbAB�HAA�A?�7A>�+A>{A=33A<{A;��A;XA:��A:^5A:�A933A7��A5�hA4$�A3�
A2�A1%A09XA/dZA.ĜA.A,�/A,E�A+�A*�\A*9XA*�A*  A)��A)�PA(��A(v�A(5?A'�A'/A&�+A%��A%��A%"�A$�`A$�!A$ �A#G�A"�jA!��A!�A ĜA v�A��AAt�A"�A��A�#A��A�PAl�AXA?}AVA�`A��A��AZA�TA+A��Ar�A
=A{A�AffAI�A��A��AXA�yA�jA�PAt�A�A��A�\Ar�A �A;dAȴA��A�A1'A+A�wA�A
�A	��AI�AȴAdZA^5A\)AA ��A  �@�\)@���@��@�ȴ@���@�9X@�ƨ@�dZ@��y@�5?@�x�@���@���@���@��@�7@��@�33@�9X@���@�@�ȴ@��@� �@ݙ�@ܛ�@���@��T@�%@ج@��@�K�@�v�@���@��@Гu@�Z@϶F@Ο�@�O�@̼j@˕�@��@�p�@�Q�@�-@���@�9X@� �@ÍP@�-@��^@�p�@��/@���@�@�ƨ@�5?@�p�@��@�r�@��;@�l�@��@��#@� �@��
@��@��#@���@��u@�Z@�1'@��
@��P@�o@���@��!@�X@���@��@���@�=q@�t�@��9@��D@�\)@���@��#@��@�G�@��@���@���@� �@���@�=q@��#@�r�@��@�(�@�Q�@�j@���@��/@�b@�C�@��R@�v�@�X@�r�@�1@��\@���@�hs@�`B@�G�@��D@�C�@���@���@��#@��@���@�r�@�A�@��F@��\@��@�J@���@���@�@��@��D@�9X@�(�@�Z@���@�Ĝ@��/@��`@��@�Q�@��@�o@�x�@���@� �@�S�@�C�@�C�@�
=@�ȴ@�=q@��-@��j@���@�S�@�K�@�;d@�33@��@�5?@���@�&�@��j@��D@�I�@��w@�t�@���@��T@�X@��@���@���@�Z@� �@�  @��@��m@��
@��F@���@�dZ@�+@�o@��@�\)@��@�t�@�C�@�K�@�C�@��y@��!@�n�@�{@���@�@�{@�@�p�@�/@���@��u@�9X@�@~�@}��@}p�@}/@}�@|��@|�j@|z�@|�@{��@{33@{o@zn�@z~�@z^5@z�\@z�\@z�!@z�@yX@y&�@y�@x�@xQ�@x �@w�;@w��@w�@v��@vv�@v5?@u�-@u`B@t��@t�j@t�@t��@tz�@t�@sC�@rJ@q�#@q�^@q7L@p�9@p�@pr�@p�@p��@q&�@qG�@qhs@p��@p�9@pA�@o�;@o��@o;d@n�R@n��@n��@n��@nv�@m�T@m�-@m?}@l�/@l��@k��@kt�@ko@j��@jM�@i�#@ix�@h��@h��@hbN@g�;@gK�@g
=@fȴ@f{@e@e��@eO�@d�@d�@c�m@ct�@b��@bn�@a�#@a��@ax�@aX@aG�@a�@`�`@`�9@`r�@`A�@_�@_��@_�P@_|�@_
=@^E�@^$�@^{@]�T@]�h@]O�@]V@\��@\�@\�@\�@\�/@\�D@\j@\1@[��@[�@[S�@[o@Z�@Y��@Y�7@YX@Y&�@XĜ@X�u@Xr�@XQ�@W�w@W
=@V��@Vff@Vff@VE�@V{@U�@U��@U��@UV@Sƨ@R�@Rn�@R�@R�@R�@Q�@Q��@Q�@P1'@O�P@O
=@Nȴ@NE�@M�@M`B@MV@L�D@LI�@L(�@L1@Kt�@K33@Ko@J�H@J�!@J�\@Jn�@J^5@J=q@I��@I7L@H�`@H��@H�@HQ�@G�;@G��@G��@G�P@G\)@G+@F��@Fȴ@Fff@E��@E`B@E/@D�j@D�@DZ@C�
@C�F@C��@Ct�@CS�@B�\@A��@A��@A�7@A7L@@�u@@Q�@@A�@@A�@@1'@@ �@@b@@  @?�@?�@?;d@?
=@>�+@>$�@=�-@=/@<�@<Z@;�m@;��@;�@:�@:��@:�\@:~�@:^5@:J@9��@9X@9%@8�9@8bN@8b@7��@7�w@7�@7��@7l�@7;d@6�@6E�@5�@5�T@5@5�-@5��@5�h@5p�@5`B@5/@4�@4j@4j@4j@4j@4Z@4Z@4Z@4Z@4j@4Z@4I�@41@3�m@3ƨ@3�F@3��@3t�@3S�@2�H@2��@2�!@2^5@1�#@1G�@0�u@0  @/�P@/;d@.�y@.ȴ@.��@.��@.�+@.v�@.V@.5?@.5?@.@-@-O�@-�@,��@,�@,��@,�@,Z@,9X@,�@,1@+��@+�m@+ƨ@+�F@+��@+��@+�@+33@*�H@*��@*�!@*�\@*M�@*-@*J@)��@)X@)�@(�`@(��@'�w@';d@'
=@&�@&5?@%�@$z�@#�F@#��@#�@#�@#t�@#S�@#33@#"�@#o@#o@"�@"�H@"�H@"�!@"M�@"J@!�@!�#@!�#@!�^@!��@!��@!hs@!�@ �`@ Ĝ@ r�@ �u@ Q�@  �@   @�;@�w@|�@l�@l�@\)@\)@K�@��@E�@@��@?}@/@��@Z@�@�m@�F@��@dZ@33@�@��@M�@�@�@��@�^@��@��@��@�7@x�@hs@hs@hs@X@&�@��@�9@r�@A�@ �@�@�;@��@��@|�@
=@
=@�@ȴ@�+@E�@{@�-@p�@O�@/@��@�@�@�D@(�@�m@�
@ƨ@�F@�F@�@o@�@�!@��@~�@J@�^@�^@��@��@x�@�9@r�@bN@bN@Q�@A�@��@l�@�@��@��@�@�+@ff@5?@�@�T@�h@p�@`B@O�@/@��@��@j@(�@�F@��@dZ@"�@
�@
��@
~�@
=q@	��@	�^@	�^@	�^@	�^@	��@	�7@	X@�9@��@bN@  @  @  @  @�;@��@l�@;d@
=@�y@ȴ@��@V@{@@��@�-@��@��@�h@�@�@p�@p�@V@�j@��@I�@(�@�@�@�@�@��@�
@ƨ@��@��@�@S�@C�@@��@�\@^5@^5@^5@=q@-@�@J@��@��@��@�7@G�@�@%@ ��@ ��@ �9@ �911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�S�A�S�A�VA�VA�S�A�O�A�M�A�M�A�M�A�O�A�O�A�K�A�C�A�(�A�K�AϮA��yA�5?A�\)A�Q�A�ĜAß�AËDA�r�AA��9A�M�A��A�ƨA�9XA��DA�JA��A�E�A�K�A�  A�p�A���A�&�A�1A�{A�A�A�/A�;dA��A��!A�A��^A���A�|�A�Q�A�9XA�hsA���A�M�A���A��A��9A�hsA��wA�dZA�;dA���A��7A�(�A��A�5?A�5?A���A�G�A��A�XA���A�/A�v�A�v�A���A�A��
A�/A�A~v�A|�DAzI�Axn�Aw�#Aw��Aw&�AuƨAt�9As�Ar~�ApA�An��AmXAl�Ai�Ai�AgAe�AcAahsA`9XA^��A^  A]�A\�AZ^5AY
=AYS�AW�mAV1AUS�AT�AS+ASO�AS%AR��ARZAQ��AP5?AO�-AOhsAN�uAN{ALJAJ1'AI�hAG�AG�AH�AG�;AF��AF{AE7LADbAB�HAA�A?�7A>�+A>{A=33A<{A;��A;XA:��A:^5A:�A933A7��A5�hA4$�A3�
A2�A1%A09XA/dZA.ĜA.A,�/A,E�A+�A*�\A*9XA*�A*  A)��A)�PA(��A(v�A(5?A'�A'/A&�+A%��A%��A%"�A$�`A$�!A$ �A#G�A"�jA!��A!�A ĜA v�A��AAt�A"�A��A�#A��A�PAl�AXA?}AVA�`A��A��AZA�TA+A��Ar�A
=A{A�AffAI�A��A��AXA�yA�jA�PAt�A�A��A�\Ar�A �A;dAȴA��A�A1'A+A�wA�A
�A	��AI�AȴAdZA^5A\)AA ��A  �@�\)@���@��@�ȴ@���@�9X@�ƨ@�dZ@��y@�5?@�x�@���@���@���@��@�7@��@�33@�9X@���@�@�ȴ@��@� �@ݙ�@ܛ�@���@��T@�%@ج@��@�K�@�v�@���@��@Гu@�Z@϶F@Ο�@�O�@̼j@˕�@��@�p�@�Q�@�-@���@�9X@� �@ÍP@�-@��^@�p�@��/@���@�@�ƨ@�5?@�p�@��@�r�@��;@�l�@��@��#@� �@��
@��@��#@���@��u@�Z@�1'@��
@��P@�o@���@��!@�X@���@��@���@�=q@�t�@��9@��D@�\)@���@��#@��@�G�@��@���@���@� �@���@�=q@��#@�r�@��@�(�@�Q�@�j@���@��/@�b@�C�@��R@�v�@�X@�r�@�1@��\@���@�hs@�`B@�G�@��D@�C�@���@���@��#@��@���@�r�@�A�@��F@��\@��@�J@���@���@�@��@��D@�9X@�(�@�Z@���@�Ĝ@��/@��`@��@�Q�@��@�o@�x�@���@� �@�S�@�C�@�C�@�
=@�ȴ@�=q@��-@��j@���@�S�@�K�@�;d@�33@��@�5?@���@�&�@��j@��D@�I�@��w@�t�@���@��T@�X@��@���@���@�Z@� �@�  @��@��m@��
@��F@���@�dZ@�+@�o@��@�\)@��@�t�@�C�@�K�@�C�@��y@��!@�n�@�{@���@�@�{@�@�p�@�/@���@��u@�9X@�@~�@}��@}p�@}/@}�@|��@|�j@|z�@|�@{��@{33@{o@zn�@z~�@z^5@z�\@z�\@z�!@z�@yX@y&�@y�@x�@xQ�@x �@w�;@w��@w�@v��@vv�@v5?@u�-@u`B@t��@t�j@t�@t��@tz�@t�@sC�@rJ@q�#@q�^@q7L@p�9@p�@pr�@p�@p��@q&�@qG�@qhs@p��@p�9@pA�@o�;@o��@o;d@n�R@n��@n��@n��@nv�@m�T@m�-@m?}@l�/@l��@k��@kt�@ko@j��@jM�@i�#@ix�@h��@h��@hbN@g�;@gK�@g
=@fȴ@f{@e@e��@eO�@d�@d�@c�m@ct�@b��@bn�@a�#@a��@ax�@aX@aG�@a�@`�`@`�9@`r�@`A�@_�@_��@_�P@_|�@_
=@^E�@^$�@^{@]�T@]�h@]O�@]V@\��@\�@\�@\�@\�/@\�D@\j@\1@[��@[�@[S�@[o@Z�@Y��@Y�7@YX@Y&�@XĜ@X�u@Xr�@XQ�@W�w@W
=@V��@Vff@Vff@VE�@V{@U�@U��@U��@UV@Sƨ@R�@Rn�@R�@R�@R�@Q�@Q��@Q�@P1'@O�P@O
=@Nȴ@NE�@M�@M`B@MV@L�D@LI�@L(�@L1@Kt�@K33@Ko@J�H@J�!@J�\@Jn�@J^5@J=q@I��@I7L@H�`@H��@H�@HQ�@G�;@G��@G��@G�P@G\)@G+@F��@Fȴ@Fff@E��@E`B@E/@D�j@D�@DZ@C�
@C�F@C��@Ct�@CS�@B�\@A��@A��@A�7@A7L@@�u@@Q�@@A�@@A�@@1'@@ �@@b@@  @?�@?�@?;d@?
=@>�+@>$�@=�-@=/@<�@<Z@;�m@;��@;�@:�@:��@:�\@:~�@:^5@:J@9��@9X@9%@8�9@8bN@8b@7��@7�w@7�@7��@7l�@7;d@6�@6E�@5�@5�T@5@5�-@5��@5�h@5p�@5`B@5/@4�@4j@4j@4j@4j@4Z@4Z@4Z@4Z@4j@4Z@4I�@41@3�m@3ƨ@3�F@3��@3t�@3S�@2�H@2��@2�!@2^5@1�#@1G�@0�u@0  @/�P@/;d@.�y@.ȴ@.��@.��@.�+@.v�@.V@.5?@.5?@.@-@-O�@-�@,��@,�@,��@,�@,Z@,9X@,�@,1@+��@+�m@+ƨ@+�F@+��@+��@+�@+33@*�H@*��@*�!@*�\@*M�@*-@*J@)��@)X@)�@(�`@(��@'�w@';d@'
=@&�@&5?@%�@$z�@#�F@#��@#�@#�@#t�@#S�@#33@#"�@#o@#o@"�@"�H@"�H@"�!@"M�@"J@!�@!�#@!�#@!�^@!��@!��@!hs@!�@ �`@ Ĝ@ r�@ �u@ Q�@  �@   @�;@�w@|�@l�@l�@\)@\)@K�@��@E�@@��@?}@/@��@Z@�@�m@�F@��@dZ@33@�@��@M�@�@�@��@�^@��@��@��@�7@x�@hs@hs@hs@X@&�@��@�9@r�@A�@ �@�@�;@��@��@|�@
=@
=@�@ȴ@�+@E�@{@�-@p�@O�@/@��@�@�@�D@(�@�m@�
@ƨ@�F@�F@�@o@�@�!@��@~�@J@�^@�^@��@��@x�@�9@r�@bN@bN@Q�@A�@��@l�@�@��@��@�@�+@ff@5?@�@�T@�h@p�@`B@O�@/@��@��@j@(�@�F@��@dZ@"�@
�@
��@
~�@
=q@	��@	�^@	�^@	�^@	�^@	��@	�7@	X@�9@��@bN@  @  @  @  @�;@��@l�@;d@
=@�y@ȴ@��@V@{@@��@�-@��@��@�h@�@�@p�@p�@V@�j@��@I�@(�@�@�@�@�@��@�
@ƨ@��@��@�@S�@C�@@��@�\@^5@^5@^5@=q@-@�@J@��@��@��@�7@G�@�@%@ ��@ ��@ �9@ �911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�bB�hB�hB�hB�hB�bB�bB�bB�bB�bB�bB�\B�\B�PB�Bp�BhsB]/B\)BXBH�BF�BF�BD�BF�BB�B+B�B�B��B�HB��B��BĜB�XB��B�7BjB_;Bk�BcTBO�B(�B��B��B��B�9B�B�B��B��B��B��B�=B�%Bx�BffB`BB[#BN�BI�BF�B?}B:^B5?B.B)�B�BhB
��B
�B
�sB
�)B
��B
ȴB
�dB
�B
��B
��B
�hB
�JB
�B
q�B
aHB
M�B
K�B
L�B
W
B
O�B
G�B
@�B
49B
�B
hB
B
  B	�mB	�;B	��B	ɺB	�!B	��B	��B	�VB	�7B	�B	y�B	n�B	hsB	w�B	s�B	gmB	iyB	ffB	[#B	_;B	aHB	^5B	\)B	ZB	VB	T�B	R�B	J�B	E�B	:^B	"�B	�B	bB	�B	#�B	"�B	�B	{B	bB	+B��B��B�yB�yB�yB�`B�;B�B�B�
B�B��B��BƨBÖB�dB�XB�RB�3B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�VB�PB�JB�DB�7B�+B�%B�+B�B�B�B� B� B�%B�+B�B�B�B�+B�1B�7B�7B�1B�1B�+B�%B�%B�B�B~�B}�Bw�Bv�Bs�Br�Bq�Bo�Bn�Bm�Bk�BhsBcTB`BB_;B^5B]/B]/B[#BXBXBXBVBR�BP�BM�BK�BH�BG�BB�B>wB<jB:^B7LB5?B33B1'B0!B0!B-B,B+B)�B(�B(�B'�B&�B%�B$�B"�B�B�B�B�B�B�B{BuBhBoBhBbBbBhB\BVB\B\B\B\B\B\B\B\BVBVBVBPBDB
=B
=BJBJBJBJBPB\BVB\B\BhBoB�B�B�B�B�B�B�B�B�B�B�B�B"�B%�B&�B&�B'�B)�B+B,B-B6FB;dB;dB>wB>wB>wBH�BVBW
BXBYB\)B^5B^5B^5B_;BbNBe`BdZBcTBdZBjBl�Bt�Bw�B}�B�B�%B�=B�JB�PB�PB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�9B�?B�?B�LB�dB��BŢBɺB��B��B��B��B�B�/B�BB�NB�ZB�ZB�`B�`B�fB�mB�sB�B�B��B��B��B��B��B��B��B��B	  B	  B	B	B	  B	B	B		7B	\B	bB	hB	hB	oB	uB	uB	uB	{B	�B	�B	�B	�B	�B	�B	#�B	&�B	(�B	)�B	.B	2-B	49B	8RB	9XB	<jB	>wB	B�B	F�B	G�B	H�B	I�B	P�B	T�B	VB	W
B	W
B	ZB	[#B	\)B	\)B	\)B	\)B	\)B	]/B	aHB	dZB	e`B	iyB	l�B	m�B	p�B	s�B	w�B	z�B	}�B	~�B	� B	�B	�B	�%B	�+B	�1B	�=B	�JB	�PB	�VB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�-B	�?B	�LB	�XB	�XB	�XB	�^B	�dB	��B	��B	ÖB	ĜB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�/B	�;B	�HB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
1B
	7B

=B

=B
DB
PB
PB
VB
\B
\B
\B
bB
hB
hB
hB
oB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
%�B
&�B
'�B
'�B
(�B
)�B
)�B
+B
+B
+B
,B
,B
,B
-B
-B
-B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
49B
49B
49B
49B
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
;dB
<jB
<jB
<jB
=qB
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
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
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
iyB
iyB
iyB
iyB
jB
jB
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
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
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�}B�hB��B�hB�hB�}B�}B�bB�bB�}B��B��B�bB�HB�7Bt�Bl�BcnBb4B\BIRBG+BG�BHBMBI7B/�B#TB!bB�^B�tB�FB�gB��B��B��B��Bm�BbNBn�Bh�BW$B0�B�>B�B��B��B�wB�QB�yB��B��B�IB��B��Bz�Bg�BaHB\�BO�BJXBG�B@OB;JB6FB0!B,=BYB�B
�$B
�AB
�0B
ݲB
��B
��B
�<B
�}B
��B
�B
��B
�pB
��B
tB
c B
N�B
LdB
M�B
XyB
QNB
IB
B�B
6�B
!�B
�B
�B
�B	�B	�HB	յB	��B	�GB	�nB	�qB	�\B	�rB	��B	{�B	pB	h�B	y�B	u�B	h�B	j�B	g�B	[qB	_�B	a�B	^�B	]IB	[�B	V�B	U�B	TB	L0B	H1B	<jB	$&B	jB	�B	�B	$�B	$&B	�B	�B	B	�B��B�B��B�KB�B�B��BخB��B��B��BևB�FB�B�9B�PB�B�^B��B�GB�;B�OB�qB��B�$B�B�bB��B�!B�;B�OB��B�5B�/B�IB��B��B�aB�B��B��B��B�PB�~B�#B�KB�B�1B��B��B�oB��B��B��B�B��B�mB�SB�zB�fB��B��B��B��B��B�B�B��B�B��BHBy	Bw�BtBsMBr-Bp;BoOBn}BmwBj�Bc�B`�B_�B^�B]�B^5B[�BX_BX�BYBW�BT�BRBOBBL�BJ�BI�BD�B@B>B<PB8�B6FB4B2B1[B1�B./B,�B+�B*eB)�B)�B(�B'�B'B&�B$@B�B�B�B�B�BSBBFB�B�B B4B�B B.B.B�B�B�B�B�B�B�BHB(B�B(B<B�B)B�B6B�B~B�B"B�B�BB}B�B�B�B7B7BBBBCB�B�B5BpB �B#�B&2B'B'RB(XB*eB+kB,qB-�B7B;�B<B?}B>�B=�BHKBVSBW�BX�BY�B\�B^jB^�B^jB_�BcBfLBd�Bc�BeFBj�Bl�Bt�Bw�B}�B�gB��B��B��B��B�"B�:B�B��B�B��B��B��B�EB�yB�B��B��B�mB�B�0B�eB��B��B�cB�]B�wB�/B�UB��B��B��B�tB�2B�JB��BżB��B��B�pB�oBԯB�
B��B��B��B�tB�tB�B�B�B�
B�*B�cB��B��B��B��B�FB�lB�JB�wB�cB	 4B	 iB	oB	oB	 �B	�B	�B		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	'B	)*B	*B	.IB	2|B	4nB	8�B	9�B	<�B	>]B	B�B	F�B	G�B	H�B	J	B	Q4B	UMB	VSB	W�B	W�B	Z7B	[WB	\]B	\CB	\CB	\]B	\]B	]dB	a|B	dtB	e�B	iyB	l�B	m�B	p�B	s�B	xB	{JB	~(B	B	�OB	�-B	�9B	�?B	�zB	��B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�/B	��B	�B	�B	�,B	�B	�B	�
B	��B	��B	�B	�B	�OB	�;B	�oB	�UB	�aB	��B	��B	�rB	�rB	�rB	�xB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�4B	�4B	�@B	�2B	�2B	�SB	�$B	�EB	�_B	�eB	�WB	�]B	�dB	�dB	�pB	�B	�B	�nB	�B	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�	B	��B	�	B	�$B	�B	�B	�B	��B	�"B	�"B	�B	�B	�BB	�]B	�}B
[B
aB
3B
B
3B
MB
MB
�B
tB
�B
	lB

rB

rB
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 B
"B
!�B
#B
#B
$&B
%B
$�B
$�B
$�B
$�B
$�B
$�B
%B
$�B
&2B
'B
&B
'8B
(>B
(>B
)*B
*0B
*0B
+6B
+B
+QB
,=B
,B
,=B
-CB
-CB
-)B
.cB
.IB
/OB
/5B
0UB
0UB
0;B
0;B
1AB
1AB
1AB
1[B
2aB
3MB
3hB
49B
49B
4TB
4TB
4TB
4nB
4TB
5�B
6`B
6`B
6FB
6FB
6`B
6FB
6`B
6FB
6FB
6FB
6`B
6zB
6zB
6zB
7fB
7fB
7fB
7fB
7�B
8lB
8�B
8�B
8�B
9�B
9�B
:�B
;�B
<�B
<�B
<�B
=�B
=�B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
D�B
EB
FB
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
NB
OB
OB
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P.B
PB
Q4B
QB
QB
R B
R B
SB
SB
S&B
S&B
S&B
SB
S&B
S&B
TB
T,B
T,B
TB
TB
T�B
UB
T�B
T�B
UB
T�B
VB
VB
VB
V9B
VB
W?B
W?B
W?B
W$B
W$B
W?B
W$B
W
B
W?B
W?B
W?B
W$B
XB
XEB
YKB
YKB
Z7B
ZQB
[WB
[WB
[=B
[=B
[#B
[WB
[qB
[WB
[WB
\)B
\CB
\CB
\)B
\]B
\]B
\]B
\CB
]IB
]IB
]dB
^OB
^OB
^5B
^OB
^jB
^�B
^OB
^5B
^OB
^OB
^OB
^�B
_VB
^jB
_VB
_;B
_VB
_VB
_pB
_VB
`vB
`vB
`\B
abB
abB
abB
a|B
abB
abB
bhB
bhB
b�B
c�B
c�B
c�B
c�B
c�B
d�B
dtB
e�B
ezB
e`B
e`B
ezB
e�B
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
g�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
jB
jB
jB
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
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
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<<lR<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.1(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609250033172016092500331720160925003317201806221214142018062212141420180622121414201804050406562018040504065620180405040656  JA  ARFMdecpA19c                                                                20160921063505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160920213514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160920213514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160920213515  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160920213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160920213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160920213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160920213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160920213516  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160920213516                      G�O�G�O�G�O�                JA  ARUP                                                                        20160920223103                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160921153443  CV  JULD            G�O�G�O�F�c�                JM  ARCAJMQC2.0                                                                 20160924153317  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160924153317  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190656  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031414  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111516                      G�O�G�O�G�O�                