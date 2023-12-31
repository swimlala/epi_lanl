CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-26T00:35:32Z creation;2016-12-26T00:35:34Z conversion to V3.1;2019-12-19T08:18:52Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161226003532  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               HA   JA  I2_0577_072                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��r�_� 1   @��sQ�n @2�j��f��d�vȴ9X1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A!��A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�33B���B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ D�|�D�� D�  D�@ Dʀ D�� D�  D�<�Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@���@���A   A>ffA\��A~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B�  B���B�  B㙚B癚B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC��C!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCV  CW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCn  Co�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DT� DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�y�Dɼ�D���D�<�D�|�Dʼ�D���D�9�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D�  D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�9�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�@ D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D� D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�\)A�^5A�`BA�hsA�`BA�\)A�^5A�dZA�ffA�jA�hsA�t�A�~�A�ffAџ�AѲ-A���A��yA��A��A�~�A�AҾwAҸRAҶFAҴ9AҼjA��A�ƨAҲ-AҰ!Aҡ�AҴ9A�ȴA��;A���A���A���A�%A�VAӃAӁA�v�A�l�A�jA�ffA�XA�1'A�ƨAґhAѮAЛ�A�?}AϺ^Aδ9A��yAɰ!AȓuA�oAǉ7A��A���A�1'A���A�ȴA�G�A�ĜA��DA�=qA��A�M�A���A��+A���A���A��A��hA���A���A���A�ĜA���A��hA��PA�hsA���A�ȴA���A�33A�ƨA��^A�ȴA���A��uA��A�p�A�=qA��A�A�t�A��A�M�A��`A�M�A�^5A�ffA���A��RA���A��^A�7LA��A�(�A��A���A��`A�  A�/A��jA�C�A��;A���A��A���Ap�A+A~�A}33AzAy�PAw��Avr�AsG�Ap(�AlE�Ajz�Ah1'Ae�-Ab��A_33A]�AZQ�AX �AW�AW%AU�;AS�FARbNAP��AO��AM�mAK�AJ�AH�+AFbAD=qAA��A?��A=�wA<  A;�7A;oA:��A8�A6A4ĜA3S�A1�A0~�A/"�A.r�A-��A,�A,bA*��A)�hA(1A({A'|�A&bA$Q�A#��A#S�A"�A!+A �A bNAC�A�A�AbA�7A�\A�A�A�7AbA�9A33A��A�yA
�DA	"�A�/A�9A�A�/A��A �AXA9XAp�A�DA�FAO�Ap�A��A�#A{AM�Ar�A�TA�PA �HA M�@�\)@��h@��@���@�o@�J@���@��!@��@�+@�z�@�hs@�^@�S�@�r�@�=q@���@�u@�l�@�V@�G�@��@��@�-@��@�I�@��@�&�@�9X@��;@�ff@�(�@��#@ϝ�@�ff@�\)@Ͼw@�~�@���@ˍP@���@�X@�z�@��@�V@�n�@�J@�`B@��@�bN@�C�@�v�@�`B@���@�bN@�(�@��@�\)@�33@��y@�$�@��;@�ƨ@���@�ƨ@��@�dZ@��@���@���@��u@�^5@��^@��@�`B@��^@��@��`@��9@�1@�  @�dZ@��y@�@��y@��y@���@�ff@��@���@��@�%@��@��9@� �@���@�+@�^5@�X@�&�@�%@��/@��u@�  @���@�dZ@�33@��@��T@�%@��9@�z�@�j@�Q�@�A�@��@�ƨ@�C�@��@�E�@���@�G�@�7L@�&�@�&�@��/@�%@�Ĝ@�bN@�1@���@���@� �@�j@���@��P@�+@��@��@���@�S�@�=q@���@��-@���@���@�^5@��!@��y@��@�"�@�S�@��@���@�1'@�ff@��@��j@���@��/@���@��/@��@�bN@�b@��
@���@�|�@���@�1@���@��F@�dZ@���@��T@�p�@�@��@���@��@���@��D@���@��9@���@���@���@���@��/@��/@���@���@��/@��D@�r�@�j@�(�@�b@���@��@��^@���@��7@�G�@��@��@�Q�@��;@���@���@�|�@�t�@�|�@��@���@�"�@�o@��H@���@�n�@��@�X@�%@�1@��\@�-@��!@�+@�C�@��R@��7@�O�@�Z@�t�@�+@�o@���@�v�@���@��!@��!@��R@��\@�=q@��-@��7@��7@�x�@�/@�%@��D@�bN@�I�@�9X@�1@�ƨ@�t�@��@��+@��+@���@�ff@�M�@�J@�^5@�n�@�@�M�@�ff@���@��#@���@��@�?}@�bN@��F@�|�@�33@��H@���@��R@���@�$�@��@�X@�?}@�/@�%@��@��@��`@��`@��/@���@�r�@��;@���@�l�@�"�@���@�E�@�@��T@��-@�O�@�?}@�%@��@��9@�I�@� �@� �@�w@�@~�@~��@~@}�h@}p�@|��@|�D@|(�@|�@{t�@z��@y�#@yG�@y&�@x��@xA�@v�R@v$�@uV@tI�@s��@s�
@s��@s33@rJ@q�^@q��@q�7@qX@p��@pQ�@pb@o�P@nȴ@nE�@m�T@m��@m��@m�@mO�@m/@l�@k��@k"�@j��@j��@j=q@i�@h�u@h1'@g�@g��@g�@g�P@gl�@g+@f�y@f��@f5?@e�h@d�j@d(�@c��@b��@b^5@a��@a��@`�`@`A�@_��@_+@^�R@^E�@^@]�T@]@]�-@]�@]O�@]�@\�/@\��@\Z@[�F@[33@Z�!@Zn�@Y��@Yx�@YG�@Y�@X�`@XbN@Xb@W�w@W|�@W\)@W
=@V�@V�@Vȴ@V�R@V��@V�+@VV@U�h@TI�@Sƨ@SS�@S@R��@R�\@R^5@Q��@Q��@Qx�@Q%@P��@P�9@P�u@P�@P1'@O�;@Ol�@N�+@Nff@NV@N{@M@M��@M�@MO�@M?}@L��@L�/@L�@L�@L�@L�D@Lj@LZ@L9X@L(�@L�@K��@K�m@K�m@K�F@K33@J~�@J�@I�@I��@Ix�@Ix�@IX@IG�@I7L@I%@HĜ@H�u@H �@G|�@G\)@G+@F�@F��@F��@Fv�@FV@F@E�T@E�-@EO�@D�/@D��@DZ@D�@C��@C��@C�m@C�
@C�@CC�@B��@B~�@Bn�@B�@A�#@A�^@AX@@��@@��@@A�@@ �@@b@@  @?�@?�@?�P@>��@>ff@=��@=��@=`B@<��@<�@;�
@;ƨ@;��@;"�@:^5@:J@9�^@9�^@9��@97L@8��@8��@8bN@81'@7�@7+@6�@6v�@6V@6$�@4��@4z�@4j@4Z@4I�@4(�@4�@41@3�m@3�F@3��@3�@3"�@2��@2�@1��@1��@1��@1hs@1�@0Ĝ@0b@/�@/l�@/;d@/�@.��@.��@.�y@.�@.�@.�R@.��@.�+@.v�@.v�@.V@.5?@-�@-@,�@,�@+��@+��@+33@*��@*^5@)�@)��@)X@(��@(�u@(�u@(�u@(�@(�@(r�@(A�@(b@'�@'l�@&�R@&ff@%�@%p�@%V@$�@$Z@$Z@$9X@$1@#�
@#ƨ@#��@#33@"��@"�\@"^5@!�@!�^@!�7@!%@ ��@ �@ A�@��@��@\)@+@+@�@
=@v�@5?@@�T@�-@`B@�@��@z�@�D@z�@j@I�@1@�m@��@��@��@C�@"�@��@��@^5@��@��@7L@%@��@�u@�@Q�@bN@r�@Q�@ �@�@�@��@��@��@�P@l�@\)@;d@��@��@ff@V@$�@�T@��@��@�-@�h@�@p�@p�@`B@?}@/@��@�@j@Z@I�@9X@(�@��@�m@�F@33@��@�!@��@�\@n�@^5@��@��@��@hs@G�@&�@&�@&�@��@�@A�@�@��@�@�@�@�@��@|�@;d@�@�y@��@��@E�@{@@�T@��@@��@��@�@p�@/@�@�@�D@j@9X@�@�m@��@S�@33@
�H@
�H@
��@
n�@
=q@
J@	��@	��@	��@	x�@	G�@	&�@	�@	%@�`@��@�@A�@b@�;@�w@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�\)A�^5A�`BA�hsA�`BA�\)A�^5A�dZA�ffA�jA�hsA�t�A�~�A�ffAџ�AѲ-A���A��yA��A��A�~�A�AҾwAҸRAҶFAҴ9AҼjA��A�ƨAҲ-AҰ!Aҡ�AҴ9A�ȴA��;A���A���A���A�%A�VAӃAӁA�v�A�l�A�jA�ffA�XA�1'A�ƨAґhAѮAЛ�A�?}AϺ^Aδ9A��yAɰ!AȓuA�oAǉ7A��A���A�1'A���A�ȴA�G�A�ĜA��DA�=qA��A�M�A���A��+A���A���A��A��hA���A���A���A�ĜA���A��hA��PA�hsA���A�ȴA���A�33A�ƨA��^A�ȴA���A��uA��A�p�A�=qA��A�A�t�A��A�M�A��`A�M�A�^5A�ffA���A��RA���A��^A�7LA��A�(�A��A���A��`A�  A�/A��jA�C�A��;A���A��A���Ap�A+A~�A}33AzAy�PAw��Avr�AsG�Ap(�AlE�Ajz�Ah1'Ae�-Ab��A_33A]�AZQ�AX �AW�AW%AU�;AS�FARbNAP��AO��AM�mAK�AJ�AH�+AFbAD=qAA��A?��A=�wA<  A;�7A;oA:��A8�A6A4ĜA3S�A1�A0~�A/"�A.r�A-��A,�A,bA*��A)�hA(1A({A'|�A&bA$Q�A#��A#S�A"�A!+A �A bNAC�A�A�AbA�7A�\A�A�A�7AbA�9A33A��A�yA
�DA	"�A�/A�9A�A�/A��A �AXA9XAp�A�DA�FAO�Ap�A��A�#A{AM�Ar�A�TA�PA �HA M�@�\)@��h@��@���@�o@�J@���@��!@��@�+@�z�@�hs@�^@�S�@�r�@�=q@���@�u@�l�@�V@�G�@��@��@�-@��@�I�@��@�&�@�9X@��;@�ff@�(�@��#@ϝ�@�ff@�\)@Ͼw@�~�@���@ˍP@���@�X@�z�@��@�V@�n�@�J@�`B@��@�bN@�C�@�v�@�`B@���@�bN@�(�@��@�\)@�33@��y@�$�@��;@�ƨ@���@�ƨ@��@�dZ@��@���@���@��u@�^5@��^@��@�`B@��^@��@��`@��9@�1@�  @�dZ@��y@�@��y@��y@���@�ff@��@���@��@�%@��@��9@� �@���@�+@�^5@�X@�&�@�%@��/@��u@�  @���@�dZ@�33@��@��T@�%@��9@�z�@�j@�Q�@�A�@��@�ƨ@�C�@��@�E�@���@�G�@�7L@�&�@�&�@��/@�%@�Ĝ@�bN@�1@���@���@� �@�j@���@��P@�+@��@��@���@�S�@�=q@���@��-@���@���@�^5@��!@��y@��@�"�@�S�@��@���@�1'@�ff@��@��j@���@��/@���@��/@��@�bN@�b@��
@���@�|�@���@�1@���@��F@�dZ@���@��T@�p�@�@��@���@��@���@��D@���@��9@���@���@���@���@��/@��/@���@���@��/@��D@�r�@�j@�(�@�b@���@��@��^@���@��7@�G�@��@��@�Q�@��;@���@���@�|�@�t�@�|�@��@���@�"�@�o@��H@���@�n�@��@�X@�%@�1@��\@�-@��!@�+@�C�@��R@��7@�O�@�Z@�t�@�+@�o@���@�v�@���@��!@��!@��R@��\@�=q@��-@��7@��7@�x�@�/@�%@��D@�bN@�I�@�9X@�1@�ƨ@�t�@��@��+@��+@���@�ff@�M�@�J@�^5@�n�@�@�M�@�ff@���@��#@���@��@�?}@�bN@��F@�|�@�33@��H@���@��R@���@�$�@��@�X@�?}@�/@�%@��@��@��`@��`@��/@���@�r�@��;@���@�l�@�"�@���@�E�@�@��T@��-@�O�@�?}@�%@��@��9@�I�@� �@� �@�w@�@~�@~��@~@}�h@}p�@|��@|�D@|(�@|�@{t�@z��@y�#@yG�@y&�@x��@xA�@v�R@v$�@uV@tI�@s��@s�
@s��@s33@rJ@q�^@q��@q�7@qX@p��@pQ�@pb@o�P@nȴ@nE�@m�T@m��@m��@m�@mO�@m/@l�@k��@k"�@j��@j��@j=q@i�@h�u@h1'@g�@g��@g�@g�P@gl�@g+@f�y@f��@f5?@e�h@d�j@d(�@c��@b��@b^5@a��@a��@`�`@`A�@_��@_+@^�R@^E�@^@]�T@]@]�-@]�@]O�@]�@\�/@\��@\Z@[�F@[33@Z�!@Zn�@Y��@Yx�@YG�@Y�@X�`@XbN@Xb@W�w@W|�@W\)@W
=@V�@V�@Vȴ@V�R@V��@V�+@VV@U�h@TI�@Sƨ@SS�@S@R��@R�\@R^5@Q��@Q��@Qx�@Q%@P��@P�9@P�u@P�@P1'@O�;@Ol�@N�+@Nff@NV@N{@M@M��@M�@MO�@M?}@L��@L�/@L�@L�@L�@L�D@Lj@LZ@L9X@L(�@L�@K��@K�m@K�m@K�F@K33@J~�@J�@I�@I��@Ix�@Ix�@IX@IG�@I7L@I%@HĜ@H�u@H �@G|�@G\)@G+@F�@F��@F��@Fv�@FV@F@E�T@E�-@EO�@D�/@D��@DZ@D�@C��@C��@C�m@C�
@C�@CC�@B��@B~�@Bn�@B�@A�#@A�^@AX@@��@@��@@A�@@ �@@b@@  @?�@?�@?�P@>��@>ff@=��@=��@=`B@<��@<�@;�
@;ƨ@;��@;"�@:^5@:J@9�^@9�^@9��@97L@8��@8��@8bN@81'@7�@7+@6�@6v�@6V@6$�@4��@4z�@4j@4Z@4I�@4(�@4�@41@3�m@3�F@3��@3�@3"�@2��@2�@1��@1��@1��@1hs@1�@0Ĝ@0b@/�@/l�@/;d@/�@.��@.��@.�y@.�@.�@.�R@.��@.�+@.v�@.v�@.V@.5?@-�@-@,�@,�@+��@+��@+33@*��@*^5@)�@)��@)X@(��@(�u@(�u@(�u@(�@(�@(r�@(A�@(b@'�@'l�@&�R@&ff@%�@%p�@%V@$�@$Z@$Z@$9X@$1@#�
@#ƨ@#��@#33@"��@"�\@"^5@!�@!�^@!�7@!%@ ��@ �@ A�@��@��@\)@+@+@�@
=@v�@5?@@�T@�-@`B@�@��@z�@�D@z�@j@I�@1@�m@��@��@��@C�@"�@��@��@^5@��@��@7L@%@��@�u@�@Q�@bN@r�@Q�@ �@�@�@��@��@��@�P@l�@\)@;d@��@��@ff@V@$�@�T@��@��@�-@�h@�@p�@p�@`B@?}@/@��@�@j@Z@I�@9X@(�@��@�m@�F@33@��@�!@��@�\@n�@^5@��@��@��@hs@G�@&�@&�@&�@��@�@A�@�@��@�@�@�@�@��@|�@;d@�@�y@��@��@E�@{@@�T@��@@��@��@�@p�@/@�@�@�D@j@9X@�@�m@��@S�@33@
�H@
�H@
��@
n�@
=q@
J@	��@	��@	��@	x�@	G�@	&�@	�@	%@�`@��@�@A�@b@�;@�w@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B
B
\B
�B
�B
(�B
R�B
�B
�DB
�JB
�JB
�PB
�\B
��B
��B
��B
��B
��B
��B
��B
�dB
��B
�
B
�B
�/B�B5?B8RB:^B?}BA�BC�BE�BC�BG�BR�BQ�BR�BVBK�B8RB>wB�TB%B�B&�B0!B9XBE�BJ�BaHBm�BffBe`BffBdZBgmBjBl�Bn�Bm�BVBQ�BM�BK�BJ�BD�BC�BB�BB�BA�B?}B<jB7LB-B �B�BbB
=BuBbBB�B�sB�;B�qB�9B��B�1Bm�BffB[#BO�BA�B2-B�B\BVBbBuBJB  B
��B
�B
�B
�yB
�NB
��B
�dB
��B
~�B
{�B
x�B
o�B
XB
Q�B
G�B
9XB
'�B
\B	��B	�sB	�#B	ɺB	�XB	��B	��B	�B	u�B	p�B	m�B	hsB	aHB	YB	O�B	G�B	B�B	9XB	1'B	)�B	 �B	�B	PB	B��B��B��B��B�B�B�NB�;B�B�B��B��BǮBĜBÖB��B�jB�qB�?BƨB��BɺBƨB��BȴBȴBɺBǮBǮBĜB�qB�'B��B��B��B�B�?B�9B�'B�B��B�{B�%B{�Bn�Bm�Bm�Bl�BiyBgmBffBe`BcTBbNBaHBbNBjBp�Bu�Bz�B� B�B�%B�PB�VB�bB�hB�oB�hB�oB�uB�=B�B�uB��B��B��B�Bs�Bw�B�B�\B�oB�oB�oB�oB�uB�bB�VB�\B�JB�\B�hB�hB��B��B��B��B��B��B��B��B�3B�RB�XB�FB�3B�!B�!B�9B�qB��B�B�B�B�B�)B�5B�BB�TB�ZB�fB�fB�mB�B�B�B�B�B��B��B��B��B��B	B	B		7B	DB		7B	
=B	\B	uB	�B	�B	�B	 �B	'�B	+B	+B	0!B	2-B	=qB	@�B	@�B	@�B	B�B	D�B	F�B	G�B	H�B	N�B	T�B	W
B	XB	XB	VB	T�B	VB	T�B	XB	]/B	^5B	_;B	aHB	bNB	e`B	e`B	ffB	jB	jB	k�B	l�B	n�B	p�B	p�B	p�B	q�B	s�B	r�B	r�B	q�B	q�B	r�B	x�B	}�B	}�B	}�B	}�B	� B	�B	�1B	�7B	�=B	�DB	�hB	�oB	�\B	�PB	�VB	�PB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�?B	�dB	�wB	�}B	��B	B	ǮB	��B	ɺB	ȴB	ǮB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�;B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�NB	�HB	�HB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
+B
+B
	7B
JB
PB
PB
JB
JB
JB
PB
JB
JB
JB
JB
JB
PB
PB
\B
\B
\B
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
#�B
#�B
#�B
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
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
,B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
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
8RB
9XB
:^B
:^B
:^B
:^B
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
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
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
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
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
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
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
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
XB
XB
YB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
]/B
]/B
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
aHB
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
cTB
cTB
cTB
cTB
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
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
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
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
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
r�B
s�B
s�B
s�B
r�B
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
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
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
y�B
y�B
y�B
y�B
y�B
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
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�wB	��B
B
(B
SB
KB
(sB
R�B
��B
�DB
�JB
�JB
�6B
�vB
��B
��B
��B
��B
��B
��B
��B
�JB
ϫB
��B
��B
��BB4�B8lB:xB?}BA�BC�BF%BD�BH�BT�BS�BTBW�BN�B=<BE9B�FBEB�B'�B1B:�BGBM�BfBp;Bg�Bh>Bh�Be�Bh�Bl=BpoBx�Br|BW?BT�BSuBS&BLJBD�BC�BB�BCBB�B@�B?B;�B2GB$�BB�BpB�B�B�B�'B�B�&B��B��B�/B�xBpBh�B]IBR BC�B3�B�B�BpB B�B<B�B
�(B
��B
�B
�QB
��B
�yB
��B
��B
�B
|�B
{0B
r�B
YKB
TFB
J	B
=qB
+�B
�B	�B	�B	ޞB	͹B	�"B	��B	�B	�_B	v�B	q�B	o�B	kB	c B	Z�B	RB	J=B	ESB	;B	3�B	-B	#�B	�B	�B	�B	 �B��B��B�+B��B��B�@B�-B�#B��BңB��B�BżB��B�[B�(B��B��B��BοBˬB��B�xB��BʦB��B�fBɠB�B�oB�B��B��B�5B� B�FB�FB�3B�wB�B�
B�B}�BoOBnBn�BnBj0BhXBg�Bf�Bd�Bc�BbNBb�BjeBpoBu�Bz�B� B�MB��B�B�\B�NB�oB��B�oB�B�MB��B��B�FB��B�pB��B��Bs�Bw2B�B��B�uB��B�[B�[B�FB�hB��B�NB�B�.B� B�&B�EB�OB�B�tB�mB� B�NB��B�3B�$B�*B��B�9B��B��B��B�B��B�mBخBٚB��B�B��B��B��B��B�B��B��B��B��B�WB��B��B��B��B�B�PB�}B	�B	�B	
=B	~B		�B	
rB	�B	�B	)B	�B	B	!HB	($B	+�B	+�B	0�B	1�B	=�B	@�B	AB	A;B	CB	D�B	F�B	G�B	IB	OBB	U�B	WsB	X�B	X�B	VSB	U2B	VSB	UgB	X�B	]dB	^jB	_pB	a�B	c:B	e�B	e�B	f�B	j�B	j�B	k�B	l�B	n�B	qB	qB	q'B	r-B	s�B	r�B	r�B	q�B	q�B	r�B	y$B	~]B	~BB	~BB	}�B	�B	�B	��B	��B	��B	��B	��B	��B	�HB	�"B	��B	�PB	�pB	�bB	� B	�YB	��B	��B	��B	��B	�B	��B	�RB	�pB	��B	�5B	�B	�/B	�/B	�OB	�UB	�[B	�tB	��B	��B	��B	�oB	�[B	��B	��B	�#B	�RB	�B	��B	ňB	ȚB	��B	��B	�~B	�B	��B	��B	��B	�+B	�1B	�KB	�1B	�=B	�IB	�VB	ߊB	�vB	�|B	�bB	�|B	�B	�B	��B	�B	�B	�nB	�B	�B	�B	��B	�B	�B	�bB	�B	�B	�mB	�sB	�B	��B	��B	��B	��B	�B	�;B	��B	�-B	�B	�iB	�B	�>B	�wB	��B	�`B	��B	�B	�UB	�/B	�B	�B	��B	�B	�B	�B	�B	��B	�B	�+B	�B	��B	��B	��B	�B	�B	�DB	��B	�B	�B	�6B	�<B	�<B	�VB	�(B	��B
 B
'B
AB
AB
�B
B
zB
+B
	RB
�B
jB
�B
~B
�B
�B
�B
�B
�B
�B
dB
dB
�B
�B
�B
vB
�B
}B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
!B
 B
�B
�B
 'B
!bB
"4B
#:B
$&B
#�B
#�B
%B
%,B
%FB
%�B
%�B
%�B
&B
&2B
&2B
%�B
&2B
&2B
&2B
&B
%�B
&B
&B
&B
&B
&B
&2B
'RB
(
B
(
B
($B
)_B
*0B
*B
*0B
*0B
+B
+6B
+6B
+6B
+6B
+QB
,WB
,WB
,WB
-]B
.IB
.}B
/OB
/iB
/OB
/�B
0oB
0UB
0oB
1vB
1[B
1AB
2GB
1AB
2GB
2aB
2aB
2aB
2aB
2GB
2|B
3�B
3hB
3hB
3MB
3hB
4nB
4TB
4TB
4TB
4nB
5tB
6zB
6zB
7fB
7fB
7�B
7fB
7fB
8RB
8�B
8�B
8�B
8�B
9�B
:�B
:�B
:�B
:xB
;�B
;�B
;�B
;B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
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
?}B
?�B
?�B
?�B
?�B
?�B
?}B
?�B
?�B
?}B
?�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
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
I�B
J	B
J�B
J�B
J�B
K)B
K�B
K�B
MB
L�B
MB
MB
NB
NB
M�B
NB
NB
N�B
OB
N�B
N�B
OB
P.B
PB
QB
Q B
QB
QNB
R B
R�B
SB
SB
SB
SB
SB
S&B
S&B
S&B
SB
S@B
TFB
T,B
UB
UB
UB
U2B
UB
UMB
U2B
VB
VB
VB
W?B
W$B
W
B
W
B
W$B
W
B
W?B
W$B
W$B
W
B
W$B
W$B
X+B
XEB
XEB
X_B
YB
ZQB
ZQB
ZQB
ZkB
[WB
[qB
\CB
\CB
\xB
]IB
]/B
]IB
]IB
]/B
]dB
]IB
]IB
]dB
^OB
^jB
_pB
_pB
_pB
`vB
`vB
abB
abB
abB
a|B
a|B
a|B
abB
bhB
bhB
b�B
bhB
c�B
cnB
cnB
c�B
d�B
d�B
dtB
d�B
d�B
e�B
ezB
e`B
ezB
e�B
e�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
hsB
h�B
h�B
h�B
h�B
h�B
i�B
iyB
i�B
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
l�B
l�B
m�B
m�B
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
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
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
r�B
s�B
s�B
s�B
r�B
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
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
xB
w�B
xB
w�B
w�B
w�B
w�B
w�B
x�B
x�B
y	B
x�B
y	B
y	B
y	B
zB
y�B
y�B
zB
y�B
z�B
{B
z�B
z�B
|B
|B
|B
|B
{�B
{�B
|B
}B
}"B
}"B
}B
}B
}B
}"B
~B
~(B
~(B
~(B
~B
~G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.1(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612300032492016123000324920161230003249201806221306552018062213065520180622130655201804050707182018040507071820180405070718  JA  ARFMdecpA19c                                                                20161226093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161226003532  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161226003532  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161226003533  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161226003534  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161226003534  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161226003534  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161226003534  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161226003534  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161226003534                      G�O�G�O�G�O�                JA  ARUP                                                                        20161226012929                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161226153446  CV  JULD            G�O�G�O�F�#�                JM  ARSQJMQC2.0                                                                 20161227000000  CF  PSAL_ADJUSTED_QCD���D���G�O�                JM  ARCAJMQC2.0                                                                 20161229153249  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161229153249  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220718  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040655  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                