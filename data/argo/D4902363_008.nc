CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-24T09:48:30Z creation;2016-06-24T09:48:31Z conversion to V3.1;2019-12-19T08:37:51Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20160624094830  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_008                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׵3Q���1   @׵4�[ @;��t�j�do��ߤ1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�C3DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�  @���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe��Cg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�@ D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�9�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D� D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�FfD�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AöFAöFAô9Aô9AöFAöFAöFAöFAöFAöFAô9Aô9Aò-Aò-Aò-Aò-Að!Að!Að!Að!A�bNA�bA�E�A�;dA��!A�\)A��yA���A�z�A��^A�bA�-A���A�z�A�%A��A��!A�/A�+A��A��A���A���A��^A�-A��jA�A�A�\)A�
=A�"�A�|�A���A��DA�M�A��A�t�A�?}A��A�/A��A���A�+A���A���A���A��A���A�?}A��-A��A���A�bNA��jA�-A�VA��
A�-A���A�l�A�ZA��A�x�A�|�A�\)A�l�A�;dA��A�1A�E�A���A��yA���A�O�A���A�?}A�~�A�  A�%A�z�A~1A|ȴA{��Az=qAyƨAx�HAxr�Aw�;Av�Au��At�`As�
AsC�Ar��Aq��Ap�ApA�Ao�#Ao�Aot�An��AlI�AihsAf��AfbAe"�Ad�\AdA�Ac\)Ab~�Aa�TAa�A`v�A_�7A^�DA]�^A]O�A\ĜA\A[;dAY7LAW��AV��AV  AT�\AS�mAS
=AQ�TAQ�APȴAOAN��ANVAM�ALbNAK�PAJ��AJ�\AI�FAHI�AG�mAG�#AG��AG7LAF$�AF(�AE`BAD��ADbACƨACt�AB��A@9XA?��A?��A>�A>JA=p�A<9XA;S�A:M�A9
=A8A7hsA5/A4�9A3�A2��A2jA2{A09XA-��A,��A+�A*��A)�;A(I�A&��A&��A&bNA&VA$��A$=qA"�`A"A�A A�A �A|�A�uA1A�7At�A�PA�`A$�A�hAjA�HA��A��A1'A�mAhsA�A{A�`A��A~�A-A�FA~�A�;A��A\)A$�A�^A&�A
�A	�7A��AbNA�A�AhsA�yAt�AI�A��A�A�A�A�Al�A�/AG�A �uA j@�;d@�p�@��`@�;d@�n�@���@��7@���@���@�$�@��m@�G�@�@�A�@�S�@���@���@�&�@�9X@�^5@�w@���@�%@䛦@�Q�@�K�@�ff@�@��@߅@�1'@�^5@ؓu@�ƨ@��@�=q@�G�@ӥ�@�o@�@���@�ƨ@�@�-@̣�@˝�@�$�@��`@ư!@�j@�;d@���@�@�M�@��-@�x�@�p�@�Q�@��@�33@���@�~�@��@�p�@��@��D@��;@�"�@���@�^5@�E�@�$�@��T@�`B@��
@��R@�n�@���@�hs@���@��@�`B@��@���@�|�@�o@���@��@���@�Ĝ@��y@�V@��#@�x�@��@���@���@�=q@���@�?}@��@��u@�9X@�"�@���@�@�G�@���@�A�@�@��+@�M�@�@���@���@��7@�/@�Ĝ@��D@�1@�l�@��y@�v�@�=q@��T@�`B@���@��@���@�K�@��\@���@���@�A�@��m@���@���@���@���@��@�o@�E�@�V@��P@�v�@�=q@�$�@�@���@���@��@��/@�I�@�ƨ@��@���@��\@�~�@�~�@�v�@�v�@�V@�E�@�{@�@��j@�1'@�l�@��y@�v�@�@��7@�X@�/@��@�V@�%@���@��u@��
@���@�|�@�;d@�o@�ȴ@�M�@�$�@�{@��@���@��7@��7@�x�@�?}@��@�1@+@~v�@~{@}@}�h@|z�@z�!@z-@y�7@y%@x��@x�9@x�u@x�@x �@w�;@w�w@w�P@wK�@v�y@vv�@vV@v{@u@u�@up�@uO�@t�j@tZ@t9X@s�m@s33@r�H@q�^@pr�@o��@o��@o|�@o�@o�@o��@o�@nȴ@n�R@nE�@m@m��@m`B@m/@mV@l�@l��@k�F@ko@j�H@j�!@j-@i�@i��@i�^@i�7@ihs@iG�@i�@h��@h��@h��@h��@h�`@h��@hQ�@g�;@g�P@gl�@gK�@g�@f�y@f�R@fV@e�T@e�@eO�@e/@eV@d�@d�@dz�@dI�@c�m@cƨ@cƨ@c�F@ct�@c33@b�H@b�!@b�\@b-@aG�@`��@`��@`A�@` �@_�@_�@_�@^��@^�+@^�+@^E�@^{@]��@]p�@]?}@]V@\��@\j@\Z@\Z@\9X@\1@[�@[33@Z��@Z~�@Y�^@Y7L@X��@X�u@Xr�@XbN@XQ�@XQ�@X1'@X  @W�;@W�w@W��@W+@V@UV@T�j@TZ@T�@S�m@S�F@S��@S��@St�@St�@SS�@SC�@SdZ@St�@SdZ@SdZ@SdZ@SS�@SS�@SC�@S@R�H@R��@R~�@R=q@R�@RJ@Q�@Q�#@Q��@QG�@PĜ@P �@O�w@O\)@OK�@O
=@N�R@N�+@N$�@M�@M@M�@MO�@M/@L��@L��@L��@L��@L�j@L�j@L�@L�@L��@Lz�@LZ@L�@K�
@KdZ@K"�@J��@JJ@I��@I�7@Ihs@IG�@I&�@I&�@I&�@I�@H�`@H��@HĜ@H�u@H�@HbN@HbN@H1'@G�@G�w@G;d@Fff@F{@F@E�@E��@E��@E?}@D�@D�j@C��@Ct�@CS�@CC�@CC�@C33@C"�@C@B��@B^5@A�#@A�@@��@@bN@@  @?K�@>�R@>��@>ff@>V@>5?@=�T@=��@=�@=?}@<��@<z�@<1@;�
@;ƨ@;��@;��@;t�@;o@:�H@:�H@:�!@:n�@:n�@:�@9��@9�7@9hs@9G�@9%@8��@8 �@7�w@7�@7|�@7;d@7
=@6��@6�y@6ff@65?@5@5��@5`B@5/@4�/@4��@4j@4(�@41@3�m@3�
@3ƨ@3��@3t�@3o@2��@2~�@2-@1�^@1x�@1&�@0��@0��@0r�@0Q�@0 �@0 �@0 �@/�;@/�@/K�@/+@.��@.�@.�+@.E�@-��@-?}@,�@,(�@,1@,1@+��@+�m@+��@*��@*�\@*M�@*M�@*=q@)��@)�^@)G�@)&�@)%@(��@(��@(Ĝ@(��@'��@'|�@'l�@'K�@'
=@&�y@&ȴ@&��@&V@&@%�-@%��@%�@%`B@%?}@%�@$�@$��@$�j@$�j@$�@$�D@$j@$I�@$(�@$�@#��@$1@#��@#�@#t�@#@"��@"�!@"^5@"-@"J@!��@!��@!X@!7L@!%@ �9@ bN@   @�;@��@��@\)@
=@��@E�@@�@�T@��@/@��@z�@z�@j@I�@��@t�@C�@��@^5@-@J@��@��@�7@hs@X@G�@�@��@�9@�u@1'@�@�;@�;@��@��@��@�w@��@l�@\)@+@ȴ@�R@��@�+@ff@V@$�@�h@O�@?}@V@�@�/@�/@�/@�/@��@��@�@j@1@�F@t�@C�@"�@@�@��@�\@^5@-@��@�@��@hs@G�@&�@�@%@�`@��@��@�@r�@Q�@b@�@�;@�@�P@|�@l�@;d@+@+@
=@�y@��@�+@v�@E�@5?@{@��@@�-@O�@/@��@(�@�
@��@�@dZ@S�@S�@C�@33@33@o@
�!@
�\@
~�@
n�@
~�@
n�@
^5@
-@
J@	��@	�#@	��@	�^@	��@	hs@	&�@	%@��@1'@  @�;@��@�w@�P@|�@\)@�@��@�y@�R@�R@�R@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AöFAöFAô9Aô9AöFAöFAöFAöFAöFAöFAô9Aô9Aò-Aò-Aò-Aò-Að!Að!Að!Að!A�bNA�bA�E�A�;dA��!A�\)A��yA���A�z�A��^A�bA�-A���A�z�A�%A��A��!A�/A�+A��A��A���A���A��^A�-A��jA�A�A�\)A�
=A�"�A�|�A���A��DA�M�A��A�t�A�?}A��A�/A��A���A�+A���A���A���A��A���A�?}A��-A��A���A�bNA��jA�-A�VA��
A�-A���A�l�A�ZA��A�x�A�|�A�\)A�l�A�;dA��A�1A�E�A���A��yA���A�O�A���A�?}A�~�A�  A�%A�z�A~1A|ȴA{��Az=qAyƨAx�HAxr�Aw�;Av�Au��At�`As�
AsC�Ar��Aq��Ap�ApA�Ao�#Ao�Aot�An��AlI�AihsAf��AfbAe"�Ad�\AdA�Ac\)Ab~�Aa�TAa�A`v�A_�7A^�DA]�^A]O�A\ĜA\A[;dAY7LAW��AV��AV  AT�\AS�mAS
=AQ�TAQ�APȴAOAN��ANVAM�ALbNAK�PAJ��AJ�\AI�FAHI�AG�mAG�#AG��AG7LAF$�AF(�AE`BAD��ADbACƨACt�AB��A@9XA?��A?��A>�A>JA=p�A<9XA;S�A:M�A9
=A8A7hsA5/A4�9A3�A2��A2jA2{A09XA-��A,��A+�A*��A)�;A(I�A&��A&��A&bNA&VA$��A$=qA"�`A"A�A A�A �A|�A�uA1A�7At�A�PA�`A$�A�hAjA�HA��A��A1'A�mAhsA�A{A�`A��A~�A-A�FA~�A�;A��A\)A$�A�^A&�A
�A	�7A��AbNA�A�AhsA�yAt�AI�A��A�A�A�A�Al�A�/AG�A �uA j@�;d@�p�@��`@�;d@�n�@���@��7@���@���@�$�@��m@�G�@�@�A�@�S�@���@���@�&�@�9X@�^5@�w@���@�%@䛦@�Q�@�K�@�ff@�@��@߅@�1'@�^5@ؓu@�ƨ@��@�=q@�G�@ӥ�@�o@�@���@�ƨ@�@�-@̣�@˝�@�$�@��`@ư!@�j@�;d@���@�@�M�@��-@�x�@�p�@�Q�@��@�33@���@�~�@��@�p�@��@��D@��;@�"�@���@�^5@�E�@�$�@��T@�`B@��
@��R@�n�@���@�hs@���@��@�`B@��@���@�|�@�o@���@��@���@�Ĝ@��y@�V@��#@�x�@��@���@���@�=q@���@�?}@��@��u@�9X@�"�@���@�@�G�@���@�A�@�@��+@�M�@�@���@���@��7@�/@�Ĝ@��D@�1@�l�@��y@�v�@�=q@��T@�`B@���@��@���@�K�@��\@���@���@�A�@��m@���@���@���@���@��@�o@�E�@�V@��P@�v�@�=q@�$�@�@���@���@��@��/@�I�@�ƨ@��@���@��\@�~�@�~�@�v�@�v�@�V@�E�@�{@�@��j@�1'@�l�@��y@�v�@�@��7@�X@�/@��@�V@�%@���@��u@��
@���@�|�@�;d@�o@�ȴ@�M�@�$�@�{@��@���@��7@��7@�x�@�?}@��@�1@+@~v�@~{@}@}�h@|z�@z�!@z-@y�7@y%@x��@x�9@x�u@x�@x �@w�;@w�w@w�P@wK�@v�y@vv�@vV@v{@u@u�@up�@uO�@t�j@tZ@t9X@s�m@s33@r�H@q�^@pr�@o��@o��@o|�@o�@o�@o��@o�@nȴ@n�R@nE�@m@m��@m`B@m/@mV@l�@l��@k�F@ko@j�H@j�!@j-@i�@i��@i�^@i�7@ihs@iG�@i�@h��@h��@h��@h��@h�`@h��@hQ�@g�;@g�P@gl�@gK�@g�@f�y@f�R@fV@e�T@e�@eO�@e/@eV@d�@d�@dz�@dI�@c�m@cƨ@cƨ@c�F@ct�@c33@b�H@b�!@b�\@b-@aG�@`��@`��@`A�@` �@_�@_�@_�@^��@^�+@^�+@^E�@^{@]��@]p�@]?}@]V@\��@\j@\Z@\Z@\9X@\1@[�@[33@Z��@Z~�@Y�^@Y7L@X��@X�u@Xr�@XbN@XQ�@XQ�@X1'@X  @W�;@W�w@W��@W+@V@UV@T�j@TZ@T�@S�m@S�F@S��@S��@St�@St�@SS�@SC�@SdZ@St�@SdZ@SdZ@SdZ@SS�@SS�@SC�@S@R�H@R��@R~�@R=q@R�@RJ@Q�@Q�#@Q��@QG�@PĜ@P �@O�w@O\)@OK�@O
=@N�R@N�+@N$�@M�@M@M�@MO�@M/@L��@L��@L��@L��@L�j@L�j@L�@L�@L��@Lz�@LZ@L�@K�
@KdZ@K"�@J��@JJ@I��@I�7@Ihs@IG�@I&�@I&�@I&�@I�@H�`@H��@HĜ@H�u@H�@HbN@HbN@H1'@G�@G�w@G;d@Fff@F{@F@E�@E��@E��@E?}@D�@D�j@C��@Ct�@CS�@CC�@CC�@C33@C"�@C@B��@B^5@A�#@A�@@��@@bN@@  @?K�@>�R@>��@>ff@>V@>5?@=�T@=��@=�@=?}@<��@<z�@<1@;�
@;ƨ@;��@;��@;t�@;o@:�H@:�H@:�!@:n�@:n�@:�@9��@9�7@9hs@9G�@9%@8��@8 �@7�w@7�@7|�@7;d@7
=@6��@6�y@6ff@65?@5@5��@5`B@5/@4�/@4��@4j@4(�@41@3�m@3�
@3ƨ@3��@3t�@3o@2��@2~�@2-@1�^@1x�@1&�@0��@0��@0r�@0Q�@0 �@0 �@0 �@/�;@/�@/K�@/+@.��@.�@.�+@.E�@-��@-?}@,�@,(�@,1@,1@+��@+�m@+��@*��@*�\@*M�@*M�@*=q@)��@)�^@)G�@)&�@)%@(��@(��@(Ĝ@(��@'��@'|�@'l�@'K�@'
=@&�y@&ȴ@&��@&V@&@%�-@%��@%�@%`B@%?}@%�@$�@$��@$�j@$�j@$�@$�D@$j@$I�@$(�@$�@#��@$1@#��@#�@#t�@#@"��@"�!@"^5@"-@"J@!��@!��@!X@!7L@!%@ �9@ bN@   @�;@��@��@\)@
=@��@E�@@�@�T@��@/@��@z�@z�@j@I�@��@t�@C�@��@^5@-@J@��@��@�7@hs@X@G�@�@��@�9@�u@1'@�@�;@�;@��@��@��@�w@��@l�@\)@+@ȴ@�R@��@�+@ff@V@$�@�h@O�@?}@V@�@�/@�/@�/@�/@��@��@�@j@1@�F@t�@C�@"�@@�@��@�\@^5@-@��@�@��@hs@G�@&�@�@%@�`@��@��@�@r�@Q�@b@�@�;@�@�P@|�@l�@;d@+@+@
=@�y@��@�+@v�@E�@5?@{@��@@�-@O�@/@��@(�@�
@��@�@dZ@S�@S�@C�@33@33@o@
�!@
�\@
~�@
n�@
~�@
n�@
^5@
-@
J@	��@	�#@	��@	�^@	��@	hs@	&�@	%@��@1'@  @�;@��@�w@�P@|�@\)@�@��@�y@�R@�R@�R@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B:^B:^B:^B:^B:^B:^B:^B;dB;dB;dB:^B:^B:^B:^B;dB;dB;dB;dB;dB:^BG�B�%B�^B�!B�hB�JBw�BT�B49B>wB49B+B!�BbBVB%B��B�B�sB�BȴBĜBÖB�dB�B�B�B��B��B�oB�1B�B|�By�Bt�Bp�Bl�BhsBbNBaHB`BBYBL�BC�B6FB33B&�B�BbBB��B��B�B�NB�;B�)B��BƨB�qB�dB�FB�B��B�+Bw�B^5BZBJ�B8RB0!B!�B�BuBDB
�B
�NB
��B
�RB
�B
��B
�JB
�B
|�B
|�B
t�B
q�B
l�B
bNB
[#B
XB
O�B
K�B
G�B
@�B
:^B
5?B
1'B
0!B
-B
)�B
�B
B	�B	�fB	�;B	�B	�
B	��B	ɺB	ĜB	��B	�jB	�dB	�3B	�B	��B	��B	��B	��B	�uB	�PB	�%B	�B	{�B	{�B	v�B	o�B	ffB	cTB	]/B	T�B	Q�B	YB	T�B	O�B	J�B	F�B	@�B	33B	0!B	49B	6FB	5?B	,B	9XB	8RB	1'B	,B	)�B	&�B	!�B	%�B	"�B	"�B	 �B	�B	�B	hB	DB	B��B�B�B�ZB�TB�;B��B�B�B��B�dB�XB�FB�'B�B��B��B��B��B��B��B��B�bB�JB�By�Bx�B|�B|�Bx�Bw�Bx�Bt�Bo�Bn�BhsBbNB_;B\)BZBYBYBW
BW
BS�BR�BQ�BP�BN�BN�BM�BL�BL�BI�BG�BF�BC�BA�B@�B@�B>wB>wB=qB;dB:^B6FB49B33B33B33B33B2-B2-B1'B/B-B/B,B,B,B+B)�B)�B(�B(�B&�B%�B%�B#�B#�B"�B"�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B#�B$�B%�B%�B&�B&�B&�B&�B&�B)�B(�B)�B+B+B+B,B,B-B.B/B/B/B/B/B/B0!B2-B33B33B49B5?B6FB;dB>wB>wB?}BC�BD�BE�BG�BG�BI�BL�BK�BK�BL�BM�BQ�BW
BYBZB\)B^5B_;B`BBffBhsBl�Bo�Bp�Br�Bu�Bu�Bv�Bw�Bw�By�Bz�B}�B� B�B�B�1B�7B�PB�PB�PB�hB�uB�{B��B��B��B��B��B��B�B�B�B�B�B�B�!B�3B�RB�wBŢBǮBɺB��B��B��B��B��B�B�/B�;B�ZB�`B�fB�fB�fB�mB�sB�sB�yB�B�B��B��B��B��B	B	B	+B	1B	1B		7B		7B	
=B	DB	bB	hB	oB	{B	�B	�B	�B	�B	�B	!�B	#�B	$�B	$�B	%�B	'�B	.B	49B	7LB	:^B	;dB	<jB	<jB	@�B	G�B	I�B	L�B	O�B	P�B	P�B	P�B	Q�B	R�B	S�B	T�B	VB	W
B	XB	ZB	[#B	\)B	^5B	_;B	_;B	`BB	cTB	gmB	hsB	iyB	m�B	n�B	q�B	u�B	w�B	w�B	y�B	{�B	|�B	|�B	� B	�B	�B	�B	�B	�%B	�1B	�7B	�7B	�=B	�DB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�-B	�3B	�9B	�?B	�FB	�FB	�RB	�qB	�wB	�}B	��B	B	B	ÖB	ĜB	ƨB	ƨB	ƨB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�5B	�BB	�BB	�HB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
+B
1B
1B
	7B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
PB
\B
\B
bB
bB
hB
oB
uB
uB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
/B
/B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
>wB
>wB
@�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
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
I�B
I�B
I�B
I�B
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
J�B
J�B
K�B
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
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
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
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
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
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
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
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
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
k�B
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
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B:^B:xB:^B:^B:^B:^B:^B;dB;dB;B:xB:^B:^B:^B;dB;B;�B;B;�B;�BKDB��B�B�ZB��B��B�B[=B5�B@4B6�B.B$ZB�B�B�B��B��B��B��B�#BżBňB��B�oB��B�oB��B��B��B�RB�B}�Bz�Bu�BqABm�Bi�Bc Bb4Ba�BZ�BOBBE�B88B5�B(XB/B�B�B��B�0B�B��B�'B�dB��BǔB��B�jB�B�}B�CB��Bz*B_�B\)BL~B9�B1�B"�B�BgB�B
�B
�B
�B
�^B
�B
�QB
��B
��B
}�B
}�B
u�B
r�B
nB
cnB
\CB
Y1B
P�B
L�B
H�B
A�B
;0B
5�B
1�B
0�B
.�B
-)B
�B
�B	�B	�mB	�B	��B	�EB	��B	��B	ŢB	��B	��B	��B	�9B	��B	��B	��B	�-B	��B	�B	��B	��B	��B	}B	}"B	xB	p�B	g8B	d�B	^5B	VB	R�B	Z�B	VB	P�B	K�B	G�B	BB	3�B	0UB	4�B	7B	6`B	,qB	:^B	9>B	1�B	,�B	*�B	(sB	$tB	&�B	#TB	#�B	!�B	�B	$B	�B	�B	�B�VB�B�B�zB��B��BյB�QBڠB�oB��B��B��B��B�B�:B�QB�)B�pB��B�'B�OB��B��B��B{BzB}�B}�By	Bx8By�Bu�Bp�BpoBjeBdB`\B\�BZ�BY�BZBX_BXyBT�BSuBR�BQ�BPbBO�BNVBM�BNVBJ�BH�BHBD�BBuBAoBAUB?.B>�B>�B=VB;�B7B4�B3MB3MB3�B3�B3hB4B2B/�B.IB0UB,�B-B,�B+�B*B*�B*B*0B(�B'mB&fB$ZB$�B#�B#TB"�B"�B"NB!|BBIB)BB�B]B=B�B)B�BB�B=BQBWB�B�BWB�B]B�BxB�B�B�B!�B"�B$tB%`B%�B&fB&B'8B'mB'RB'RB'�B*�B)yB*eB+QB+�B+kB,qB,�B-�B.�B/�B/OB/OB/OB/�B/�B1AB2�B3�B3�B4�B6B7�B<PB>�B?B@4BC�BEBF?BH1BH�BJ�BMPBL0BLJBMjBN�BR�BWsBYBZ�B\�B^jB_�BaBf�BiBm)Bp!BqvBs�BvFBvBw2BxBxBz*B{JB~BB�OB��B��B��B��B��B��B��B��B�B��B�B�KB�~B�NB�zB�DB�B�WB�)B�IB�IB��B��B�9B�XB�HB��B��B�	B�B�DB�<B�@BՁBٚB�~B߾B�B�B�B�B�B�B�B��B��B�QB�'B�`B�dB�VB�}B	�B	SB	EB	KB	KB		lB		�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 B	!�B	$B	$�B	%B	&2B	(sB	.�B	4�B	7�B	:�B	;�B	<�B	<�B	A B	G�B	J#B	MB	O�B	QB	QB	QB	R:B	SB	T,B	UB	VSB	WYB	XEB	ZQB	[=B	\]B	^jB	_VB	_VB	`�B	c�B	g�B	h�B	i�B	m�B	o5B	rB	vB	w�B	w�B	y�B	|B	}"B	}<B	�B	� B	�UB	�AB	�3B	�?B	�KB	�lB	�lB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�B	��B	�2B	�$B	�DB	�B	�B	�=B	�=B	�CB	�IB	�5B	�OB	�[B	�-B	�aB	�GB	�MB	�nB	�tB	�`B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�.B	�:B	�@B	�&B	�gB	�_B	�QB	�CB	�OB	�BB	�vB	�bB	�bB	�hB	�B	�B	�tB	�B	��B	��B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	��B	��B	��B	�B	�<B	�.B
 OB
AB
-B
3B
MB
9B
?B
_B
fB
fB
	RB

rB
^B
^B
JB
dB
JB
JB
JB
JB
JB
~B
dB
~B
jB
vB
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
!B
"B
!�B
#B
# B
%,B
%�B
%�B
%�B
&B
'B
'B
'B
($B
($B
)B
)DB
*B
+B
+B
+6B
+B
+B
,"B
,B
,=B
-CB
-)B
-CB
-CB
./B
./B
.IB
.IB
/5B
/OB
0UB
1[B
1AB
1AB
1[B
1[B
1AB
1vB
2GB
2aB
3hB
3MB
3hB
3hB
4TB
4nB
4nB
5ZB
5ZB
5ZB
5ZB
6`B
6`B
6zB
7fB
7�B
7�B
8�B
8�B
9rB
9�B
9�B
9�B
:�B
:xB
:xB
:xB
:xB
:xB
:xB
;B
;B
<�B
<�B
<�B
=�B
>�B
>�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
FB
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
I�B
I�B
I�B
I�B
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
J�B
J�B
K�B
K�B
K�B
K�B
MB
L�B
MB
L�B
MB
M�B
M�B
NB
M�B
M�B
N�B
OB
PB
O�B
PB
PB
Q B
QB
QB
R B
RB
RB
RB
S&B
SB
T,B
TB
TB
T,B
T,B
U2B
V9B
VSB
VSB
V9B
W$B
W?B
X+B
XEB
XEB
XB
X+B
XEB
YKB
YKB
YKB
Y1B
Z7B
Z7B
Z7B
ZB
Z7B
Z7B
Z7B
Z7B
[=B
[=B
[WB
[WB
\CB
\CB
\CB
\CB
\]B
]IB
]dB
^OB
^jB
^jB
^jB
_VB
_;B
_;B
_VB
_VB
_;B
_VB
_pB
_pB
`\B
`vB
abB
abB
a|B
abB
a|B
a|B
bhB
b�B
b�B
b�B
bhB
cnB
c�B
c�B
cTB
c�B
dtB
dtB
dtB
dtB
dZB
dtB
dtB
ezB
ezB
e�B
e�B
e`B
e�B
f�B
ffB
ffB
f�B
f�B
f�B
ffB
f�B
f�B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
iyB
iyB
iyB
i�B
i�B
i�B
j�B
jB
jB
j�B
j�B
j�B
j�B
j�B
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
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.1(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606240036372016062400363720160624003637201806221209502018062212095020180622120950201804050402042018040504020420180405040204  JA  ARFMdecpA19c                                                                20160624183518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160624094830  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160624094830  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160624094830  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160624094831  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160624094831  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160624094831  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160624094831  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20160624094831                      G�O�G�O�G�O�                JA  ARUP                                                                        20160624102547                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160620153313  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20160623153637  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160623153637  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190204  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622030950  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                