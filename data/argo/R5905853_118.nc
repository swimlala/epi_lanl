CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:43:58Z creation;2022-06-04T17:43:59Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174358  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               vA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��Ԡ"�1   @���!�n@01hr� ��cy�-V1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BY33B_33Bh  Bp  Bx  B��B�  B���B�  B�33B���B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C � C�fC  C  C  C
  C  C  C  C  C  C�fC�fC�fC  C  C   C!�fC$  C&  C(  C*�C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^fD^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ Dۼ�D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�3D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @34@i��@���@���AffA:ffAZffAzffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B&��B.��B6��B>��BF��BN��BW��B]��Bf��Bn��Bv��B~34B�L�B��B�L�B�� B��gB�L�B�L�B�L�B�L�B�L�B�� B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC��C�fC�fC�fC	�fC�fC�fC�fC�fC�fC��C��C��C�fC�fC�fC!��C#�fC%�fC'�fC)� C+� C-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�� C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D i�D �Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�D	i�D	�D
i�D
�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�D i�D �D!i�D!�D"i�D"�D#i�D#�D$i�D$�D%i�D%�D&i�D&�D'i�D'�D(i�D(�D)i�D)�D*i�D*�D+i�D+�D,i�D,�D-i�D-�D.i�D.�D/i�D/�D0i�D0�D1i�D1�D2i�D2�D3i�D3�D4i�D4�D5i�D5�D6i�D6�D7i�D7�D8i�D8�D9p D9�D:i�D:�D;i�D;�D<i�D<�D=i�D=�D>i�D>�D?i�D?�D@i�D@�DAi�DA�DBi�DB�DCi�DC�DDi�DD�DEi�DE�DFi�DF�DGi�DG�DHi�DH�DIi�DI�DJi�DJ�DKi�DK�DLi�DL�DMi�DM�DNi�DN�DOi�DO�DPi�DP�DQi�DQ�DRi�DR�DSi�DS�DTi�DT�DUi�DU�DVi�DV�DWi�DW�DXi�DX�DYi�DY�DZi�DZ�D[i�D[�D\i�D\�D]p D]� D^i�D^�D_i�D_�D`i�D`�Dai�Da�Dbi�Db�Dci�Dc�Ddi�Dd�Dei�De�Dfi�Df�Dgi�Dg�Dhi�Dh�Dii�Di�Dji�Dj�Dki�Dk�Dli�Dl�Dmi�Dm�Dni�Dn�Doi�Do�Dpi�Dp�Dqi�Dq�4Dri�Dr�Dsi�Ds�Dti�Dt�Dui�Du�Dvi�Dv�Dwi�Dw�Dxi�Dx�Dyi�Dy�Dzi�Dz�D{i�D{�D|i�D|�D}i�D}�D~i�D~�Di�D�D�4�D�t�D���D���D�4�D�x D�� D���D�4�D�t�D���D���D�8 D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D�� D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D�� D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D´�D���D�4�D�t�Dô�D���D�4�D�t�DĴ�D���D�4�D�t�DŴ�D���D�4�D�t�Dƴ�D���D�4�D�t�DǴ�D�� D�4�D�t�Dȴ�D���D�4�D�t�Dɴ�D���D�4�D�t�Dʴ�D���D�4�D�t�D˴�D���D�4�D�t�D̴�D���D�4�D�t�Dʹ�D���D�4�D�t�Dδ�D���D�4�D�t�Dϴ�D���D�4�D�t�Dд�D���D�4�D�t�DѴ�D���D�4�D�t�DҴ�D���D�4�D�t�DӴ�D���D�4�D�t�DԴ�D���D�4�D�t�Dմ�D���D�4�D�t�Dִ�D���D�4�D�t�D״�D���D�4�D�t�Dش�D���D�4�D�t�Dٴ�D���D�4�D�t�Dڴ�D���D�4�D�t�D۱�D���D�4�D�t�Dܴ�D���D�4�D�t�Dݴ�D�� D�4�D�t�D޴�D���D�4�D�t�Dߴ�D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D���D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AΟVAΥzAά=Aζ�Aθ�AιXAθ�AζFAζ�Aκ�AξAξwA���A��UA�A���A���A���A���A���A���A��3A�ÖAι$A΍PA�[�A�dZA�PA���ÁA�IRA�[WA�	�A��AȾBA�hA�CaA�!�A��A��cA�ҽA���A�PA��ZA��VA�xA���A��>A�h�A�YA��A�k�A�A�uZA���A�[�A���A��JA��AA���A���A���A���A�t�A��A�(�A��pA��A���A�m�A��}A��A���A���A�FtA�%FA�u�A�?}A�W?A���A���A�!�A�B�A�ÖA�[#A|C-A{>�Aw��Ar�BAm�xAk�cAg]�Ab�}A]�AZ�AW�AS�7AP�aAL��AIJ�AI!�AJ�TAH��AE�AC3�A@-A>��A=w2A<e�A;A8�rA7'�A6ȴA6��A6Z�A6%�A5��A5TaA4�[A4�AA3҉A3�A1�QA0��A01�A/$�A.8�A-u�A,�SA,T�A,
=A+�$A*یA*V�A*YA)��A(�dA(t�A'�}A'/�A&��A&iDA&GEA%�2A$�>A$!�A#��A#�A!�A�"A��Au%AB�A,=A�A�@A�AȴA6zA�A�A�A0UA�CA/A��AIRA�DA��A�AM�A|�A�A�hA�vAn/A�A͟AqA�A҉A�APHA�AںA�AN<A�SA%FA��AYKA�Ax�A �A
��A
m]A
-wA	ȴA	�uA	O�A�An�A1'A�^AZAS�AYKA�A�tA|�AS�A(�AϫAq�As�A3�A�AoiA��A(�A*�AJA��A��A�A�A �\A 5?@��=@���@�\�@���@��K@�6z@���@���@��Q@�+@�)_@��`@��.@��@���@���@��3@��@�>B@��}@�y�@��@�L@�A�@� i@���@��@�l�@�7L@��.@�$t@��@�c�@�ԕ@�B�@��@�Ĝ@�e�@���@�@��.@��@�j�@�F@�A�@�}V@���@㯸@�F�@�+@��8@��@�z@�"h@��@�@�@���@��@ߥ�@߆�@��@�7�@��}@��@ܒ�@�"h@�u@��@� �@ۨX@�a@�8�@���@ڶ�@ڛ�@��W@٘�@�/�@��2@؝I@� �@�X@�{�@Յ@�H�@Ԅ�@��@Ӏ4@�)_@��v@ҹ�@ҝI@�`�@��@Ѵ�@�v`@Ь@�2a@΋D@ξ�@�1'@ͷ@̠�@�X@ȧ@���@ǡ�@�ߤ@�-�@�V@��	@��f@��p@�c @Ê	@�%F@�@�[�@�1'@��Q@�J�@��@��?@�y>@�2�@��j@�j�@�@��@��@�1'@���@��m@��@�8�@��@��@��b@�+k@���@�Vm@�*0@�+@��@��[@���@�c @�M@�L0@�I�@�=q@�b@��K@���@��4@�|�@�A�@���@�l�@�#:@���@��d@��^@��C@��@@���@��@���@���@�S�@�/�@�q�@�7@���@��@�|�@��?@���@�7@�n/@�U�@�K�@�#�@��P@�ں@�\�@�rG@��m@�M�@���@��@�Y�@���@���@�s�@�:�@��@��'@���@�>�@���@��@�h
@��
@���@�ݘ@��@���@�bN@�!�@�@�	@�G@��@��)@���@�o@�2�@��!@��@�c@�*0@���@�u�@���@��@�X�@�+�@��<@�O@��a@��@���@��W@�|�@��@�-@�,=@��z@�q@�'�@�F�@�J�@��@�ی@��U@�s�@�9X@��@��]@��W@��K@��h@�a�@���@�u�@�0U@�	@���@��d@�s@�Q�@���@���@�tT@�J�@�@�T�@�C�@�@O@�A�@�&@��@���@��v@��\@�p;@�:�@��@���@�9�@�@��L@�@�ϫ@�s�@�;d@��@�~(@�J�@�O@�خ@��f@�^�@�A�@��@��,@���@��@�h�@�0U@��@���@�B�@��@���@���@�<�@��m@�8@��@��D@�?@��@���@���@��a@���@�c@�iD@�7L@���@�s�@�M�@�B[@��@�o @�@���@�]d@�#:@���@�dZ@�4@���@���@��d@���@���@���@�w2@��]@�Q�@��6@��{@��@�a|@�1�@�ƨ@�\)@�F@��@��@��@���@��@��j@�z@�a|@�?@�A@F�@}�)@}/@|�@|�@{��@z�!@y}�@y5�@yO�@yVm@yS&@x��@x`�@w�K@w,�@v��@vz@v#:@u��@u��@us�@uX@u�@v5?@v5?@u��@u=�@t�@t��@tx@s�Q@sl�@s
=@r��@r{@q�9@q��@qa�@q[W@qq@q8�@p֡@p2�@px@ol�@n��@nH�@m�o@m�@l��@lPH@l�@ka@j��@j�h@j_�@i��@i�~@h�@h��@h�Y@h  @g��@g�k@g�f@g+@f�@fR�@f;�@f	@d��@d�@dM@c��@d�@c��@c��@cA�@c
=@b�@b�h@b��@a�H@a�7@aA @a+�@`�5@`�9@`��@``�@`9X@_��@_|�@_@^�L@]��@]�@]+@\�j@\4n@[��@[{J@Z�8@Z҉@Z�B@Z��@Z}V@Z\�@Y�o@Y^�@Y	l@X��@X�@XbN@X1'@WJ#@V��@VkQ@V.�@Ve@U��@U�@U�@U��@U��@U��@Uj@U+@T��@T��@TM@S�w@S��@S!-@Rl�@Q��@Q�C@Q��@Q0�@Q	l@P�f@P�`@P��@Pz�@P2�@O�@O��@O�	@Og�@N�s@Nn�@N$�@M�N@M��@MT�@M7L@LZ@K��@KdZ@K+@KS@Jں@J�b@JYK@J
�@Iϫ@I��@I�=@I��@Izx@Ia�@IB�@I \@H��@G�]@Gl�@G]�@GO@F��@FC�@E��@E7L@E;@D�[@D��@D�.@D��@Doi@D?�@Cخ@Ct�@C(@B�'@B�!@B��@B��@Bxl@Ba|@A�.@A��@A�@@�@@�@@tT@?�W@?j�@>�@>�s@>�B@>�@>q�@>�@=o @<�@<�u@;˒@;iD@;&@;�@:�"@:�6@:~�@:0U@9��@9@9�-@9��@9��@9}�@9k�@9O�@9!�@8��@8V�@8!@7�]@7g�@6�!@6-@5�H@5��@5!�@4ѷ@3�A@3��@3S�@3J#@3J#@38@3�@2��@2h
@2J�@20U@2u@1�N@1��@1�~@1s�@1<6@0�f@/�@/b�@.ȴ@.v�@.i�@.Ta@.@-��@-ϫ@-�@,�@+a@*�8@*��@*҉@*��@*�1@*�A@*xl@*_�@*�@)=�@(��@(�@(M@(<�@(2�@(	�@'�@'��@'Z�@'@O@'@&�@&��@&��@&�@%��@%�n@%@$�@$��@$��@$g8@$�@#�}@#��@#@"�]@"z@"($@!��@!��@!��@!|@ �5@ ��@ ��@ _@ ~@ 7@ �@�@iD@�@ں@�X@��@��@�b@� @l�@�.@�@��@c�@�@�@�@��@u�@[�@N�@K^@<�@��@��@t�@33@�@��@�R@��@6�@	@@p�@G�@�v@�p@��@��@j@7�@ �@�@��@�@��@��@x@9�@�@��@�s@�m@�!@�@�A@C�@�@�@��@�h@j@�p@�@D�@J#@ i@��@�,@��@.�@��@B�@��@c�@PH@!@�@�m@�0@��@��@g�@�@a|@E�@($@u@�@�C@c�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AΟVAΥzAά=Aζ�Aθ�AιXAθ�AζFAζ�Aκ�AξAξwA���A��UA�A���A���A���A���A���A���A��3A�ÖAι$A΍PA�[�A�dZA�PA���ÁA�IRA�[WA�	�A��AȾBA�hA�CaA�!�A��A��cA�ҽA���A�PA��ZA��VA�xA���A��>A�h�A�YA��A�k�A�A�uZA���A�[�A���A��JA��AA���A���A���A���A�t�A��A�(�A��pA��A���A�m�A��}A��A���A���A�FtA�%FA�u�A�?}A�W?A���A���A�!�A�B�A�ÖA�[#A|C-A{>�Aw��Ar�BAm�xAk�cAg]�Ab�}A]�AZ�AW�AS�7AP�aAL��AIJ�AI!�AJ�TAH��AE�AC3�A@-A>��A=w2A<e�A;A8�rA7'�A6ȴA6��A6Z�A6%�A5��A5TaA4�[A4�AA3҉A3�A1�QA0��A01�A/$�A.8�A-u�A,�SA,T�A,
=A+�$A*یA*V�A*YA)��A(�dA(t�A'�}A'/�A&��A&iDA&GEA%�2A$�>A$!�A#��A#�A!�A�"A��Au%AB�A,=A�A�@A�AȴA6zA�A�A�A0UA�CA/A��AIRA�DA��A�AM�A|�A�A�hA�vAn/A�A͟AqA�A҉A�APHA�AںA�AN<A�SA%FA��AYKA�Ax�A �A
��A
m]A
-wA	ȴA	�uA	O�A�An�A1'A�^AZAS�AYKA�A�tA|�AS�A(�AϫAq�As�A3�A�AoiA��A(�A*�AJA��A��A�A�A �\A 5?@��=@���@�\�@���@��K@�6z@���@���@��Q@�+@�)_@��`@��.@��@���@���@��3@��@�>B@��}@�y�@��@�L@�A�@� i@���@��@�l�@�7L@��.@�$t@��@�c�@�ԕ@�B�@��@�Ĝ@�e�@���@�@��.@��@�j�@�F@�A�@�}V@���@㯸@�F�@�+@��8@��@�z@�"h@��@�@�@���@��@ߥ�@߆�@��@�7�@��}@��@ܒ�@�"h@�u@��@� �@ۨX@�a@�8�@���@ڶ�@ڛ�@��W@٘�@�/�@��2@؝I@� �@�X@�{�@Յ@�H�@Ԅ�@��@Ӏ4@�)_@��v@ҹ�@ҝI@�`�@��@Ѵ�@�v`@Ь@�2a@΋D@ξ�@�1'@ͷ@̠�@�X@ȧ@���@ǡ�@�ߤ@�-�@�V@��	@��f@��p@�c @Ê	@�%F@�@�[�@�1'@��Q@�J�@��@��?@�y>@�2�@��j@�j�@�@��@��@�1'@���@��m@��@�8�@��@��@��b@�+k@���@�Vm@�*0@�+@��@��[@���@�c @�M@�L0@�I�@�=q@�b@��K@���@��4@�|�@�A�@���@�l�@�#:@���@��d@��^@��C@��@@���@��@���@���@�S�@�/�@�q�@�7@���@��@�|�@��?@���@�7@�n/@�U�@�K�@�#�@��P@�ں@�\�@�rG@��m@�M�@���@��@�Y�@���@���@�s�@�:�@��@��'@���@�>�@���@��@�h
@��
@���@�ݘ@��@���@�bN@�!�@�@�	@�G@��@��)@���@�o@�2�@��!@��@�c@�*0@���@�u�@���@��@�X�@�+�@��<@�O@��a@��@���@��W@�|�@��@�-@�,=@��z@�q@�'�@�F�@�J�@��@�ی@��U@�s�@�9X@��@��]@��W@��K@��h@�a�@���@�u�@�0U@�	@���@��d@�s@�Q�@���@���@�tT@�J�@�@�T�@�C�@�@O@�A�@�&@��@���@��v@��\@�p;@�:�@��@���@�9�@�@��L@�@�ϫ@�s�@�;d@��@�~(@�J�@�O@�خ@��f@�^�@�A�@��@��,@���@��@�h�@�0U@��@���@�B�@��@���@���@�<�@��m@�8@��@��D@�?@��@���@���@��a@���@�c@�iD@�7L@���@�s�@�M�@�B[@��@�o @�@���@�]d@�#:@���@�dZ@�4@���@���@��d@���@���@���@�w2@��]@�Q�@��6@��{@��@�a|@�1�@�ƨ@�\)@�F@��@��@��@���@��@��j@�z@�a|@�?@�A@F�@}�)@}/@|�@|�@{��@z�!@y}�@y5�@yO�@yVm@yS&@x��@x`�@w�K@w,�@v��@vz@v#:@u��@u��@us�@uX@u�@v5?@v5?@u��@u=�@t�@t��@tx@s�Q@sl�@s
=@r��@r{@q�9@q��@qa�@q[W@qq@q8�@p֡@p2�@px@ol�@n��@nH�@m�o@m�@l��@lPH@l�@ka@j��@j�h@j_�@i��@i�~@h�@h��@h�Y@h  @g��@g�k@g�f@g+@f�@fR�@f;�@f	@d��@d�@dM@c��@d�@c��@c��@cA�@c
=@b�@b�h@b��@a�H@a�7@aA @a+�@`�5@`�9@`��@``�@`9X@_��@_|�@_@^�L@]��@]�@]+@\�j@\4n@[��@[{J@Z�8@Z҉@Z�B@Z��@Z}V@Z\�@Y�o@Y^�@Y	l@X��@X�@XbN@X1'@WJ#@V��@VkQ@V.�@Ve@U��@U�@U�@U��@U��@U��@Uj@U+@T��@T��@TM@S�w@S��@S!-@Rl�@Q��@Q�C@Q��@Q0�@Q	l@P�f@P�`@P��@Pz�@P2�@O�@O��@O�	@Og�@N�s@Nn�@N$�@M�N@M��@MT�@M7L@LZ@K��@KdZ@K+@KS@Jں@J�b@JYK@J
�@Iϫ@I��@I�=@I��@Izx@Ia�@IB�@I \@H��@G�]@Gl�@G]�@GO@F��@FC�@E��@E7L@E;@D�[@D��@D�.@D��@Doi@D?�@Cخ@Ct�@C(@B�'@B�!@B��@B��@Bxl@Ba|@A�.@A��@A�@@�@@�@@tT@?�W@?j�@>�@>�s@>�B@>�@>q�@>�@=o @<�@<�u@;˒@;iD@;&@;�@:�"@:�6@:~�@:0U@9��@9@9�-@9��@9��@9}�@9k�@9O�@9!�@8��@8V�@8!@7�]@7g�@6�!@6-@5�H@5��@5!�@4ѷ@3�A@3��@3S�@3J#@3J#@38@3�@2��@2h
@2J�@20U@2u@1�N@1��@1�~@1s�@1<6@0�f@/�@/b�@.ȴ@.v�@.i�@.Ta@.@-��@-ϫ@-�@,�@+a@*�8@*��@*҉@*��@*�1@*�A@*xl@*_�@*�@)=�@(��@(�@(M@(<�@(2�@(	�@'�@'��@'Z�@'@O@'@&�@&��@&��@&�@%��@%�n@%@$�@$��@$��@$g8@$�@#�}@#��@#@"�]@"z@"($@!��@!��@!��@!|@ �5@ ��@ ��@ _@ ~@ 7@ �@�@iD@�@ں@�X@��@��@�b@� @l�@�.@�@��@c�@�@�@�@��@u�@[�@N�@K^@<�@��@��@t�@33@�@��@�R@��@6�@	@@p�@G�@�v@�p@��@��@j@7�@ �@�@��@�@��@��@x@9�@�@��@�s@�m@�!@�@�A@C�@�@�@��@�h@j@�p@�@D�@J#@ i@��@�,@��@.�@��@B�@��@c�@PH@!@�@�m@�0@��@��@g�@�@a|@E�@($@u@�@�C@c�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	_�B	e�B	g�B	gRB	gRB	g8B	gRB	gRB	g�B	g�B	h$B	g�B	g�B	g�B	hXB	hsB	i�B	i�B	kB	lB	l�B	l�B	m�B	s�B	~]B	��B	��B	�?B	�eB	�=B	�qB	��B	��B	��B	��B
��B
��BpUBs�ByXB�XB�:B�B�B��B�B�B	BPB�B�BB�*B��B�+B�B��B�iB�kB�6B��B��B�BҽB�;B�!B�4B�`B��B�B��B�7B��B��Bc�B&�B
�B
��B
g�B
VSB
BuB
-CB
�B

	B
AB	�MB	��B	бB	�JB	�B	�B	t�B	T�B	8�B	%B	 �B	/B	�B	%B�B�>B	�B	WsB	jB	UgB	T,B	VB	Q�B	T�B	ZB	a�B	\�B	^5B	cnB	e�B	gB	jB	n�B	r�B	v�B	yXB	~wB	��B	�?B	�'B	�B	��B	��B	��B	��B	�1B	��B	�B	�TB	�B	�LB	�wB	�B	��B	�$B	�jB	��B
  B
 �B
�B
9B
?B
B
AB	��B	�B	�B	�	B	�qB
�B
	�B	��B	�^B	�]B	�B
^B
gB
%B
�B
�B
�B
�B
uB
[B
�B
UB
 �B
 �B
 B
 �B
oB
uB
gB
_B
�B
�B
�B
SB
9B
�B
SB
�B
9B
�B
�B
�B
�B
?B
B
?B
�B
�B
_B
B
B

�B
�B
	�B
	�B
fB
�B
�B
�B
�B
�B
?B
YB
tB
fB
fB
	�B
xB

rB
	RB
�B
�B
�B
�B
�B
�B
B
-B
oB
UB
 OB
  B	��B
 �B
uB
{B
�B
�B	��B	�B
B
SB

�B
DB

�B
	�B
	7B
�B
	�B
	�B
�B
�B
�B
3B
MB
B
AB
oB
{B
�B
%B
SB
MB
B
gB
B
�B
B
�B
 �B	�}B	��B	�(B	��B	�dB	��B	��B	��B	��B	�xB	�xB	�xB	�DB	��B	�>B	��B	��B	��B	�jB	��B	��B	��B	��B	�*B	��B	��B	�	B	�B	��B	��B	��B	��B	��B	�2B	��B	��B	��B	��B	��B	�B	�B	��B	�tB	�9B	��B	��B	�'B	��B	�!B	�B	��B	��B	��B	�oB	�B	�B	�'B	��B	�IB	�/B	�`B	�LB	��B	�2B	�B	�B	�KB	��B	��B	��B	�=B	�=B	�=B	�B	��B	�qB	��B	��B	�qB	��B	�CB	��B	�/B	�B	� B	�B	�B	��B	��B	�AB	�B	��B	�B	�B	�B	�B	��B	��B	�9B	�9B	�`B	�fB	��B	��B	�RB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�jB	�PB	�B	��B	�jB	�6B	�jB	�6B	�jB	�PB	�PB	�6B	�6B	�B	�6B	��B	��B	��B	��B	��B	�"B	��B	�(B	��B	�B	��B
  B	��B	��B	��B	��B	�cB	�.B	�.B	��B	��B	��B	��B	��B	�.B	�B	�cB	��B	�HB	�B	��B	��B	��B	��B	�]B	�]B	��B	��B	��B
 B
UB
�B
�B
�B
�B
�B
�B
[B
3B
-B
 �B	�.B	�]B	��B	�B	�qB	��B	��B	��B	��B	��B	�MB	�hB	�%B	��B	�jB
oB
�B
B
?B
_B
YB
B
1B
�B
	7B

�B
�B
�B
"B
<B
�B
�B
B
B
}B
B
�B
�B
&B
&B
�B
MB
�B
�B

B
YB
YB
�B
�B
�B
�B
�B
KB
KB
1B
KB
�B
�B
B
B
�B
qB
WB
)B
�B
�B
�B
dB
�B
B
�B
�B
B
�B
�B
�B
�B
�B
B
pB
�B
 BB
 �B
!-B
!bB
"B
"NB
"hB
"�B
# B
#�B
#B
#�B
#�B
#�B
$&B
$@B
$@B
$@B
$@B
$B
$&B
$ZB
$�B
$�B
%�B
%�B
&LB
%,B
%�B
&�B
&�B
'RB
(>B
)*B
*0B
*eB
+kB
*�B
+6B
+�B
+QB
+6B
+�B
+�B
+�B
+QB
+B
+B
,"B
,qB
,�B
,�B
,�B
,�B
,�B
,�B
-)B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/OB
/�B
/B
0!B
1'B
1AB
1[B
1AB
1�B
1�B
1�B
1[B
1AB
1vB
1[B
1vB
1�B
1�B
3�B
5ZB
5�B
4�B
4B
3�B
3hB
3MB
3�B
5ZB
5�B
6zB
6B
6B
5�B
6`B
6�B
6�B
72B
9XB
9�B
;B
;�B
;�B
;�B
;�B
<6B
<�B
<�B
<�B
<�B
<�B
=VB
=qB
>B
?}B
@4B
@ B
?�B
?.B
?B
@�B
A;B
BuB
B�B
BB
BAB
CGB
CGB
B�B
CB
CaB
D�B
D�B
E�B
FtB
F�B
GB
G_B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
IB
IB
IB
IRB
I�B
I�B
I�B
J=B
J	B
JXB
J�B
KDB
KDB
KxB
K�B
K�B
K�B
K�B
K�B
K�B
L0B
L�B
L�B
L�B
L�B
MB
MB
M�B
M�B
NVB
NpB
NpB
NpB
NpB
NpB
N�B
N�B
N�B
N�B
N�B
N�B
O(B
O\B
O�B
OvB
O�B
P}B
P�B
P�B
P�B
Q4B
Q4B
Q4B
QNB
QNB
Q�B
Q�B
Q�B
Q�B
RB
RB
R�B
R�B
R�B
R�B
SB
S&B
R�B
S�B
S�B
S�B
TFB
TFB
TaB
T{B
T�B
T�B
T�B
T�B
UB
UB
U2B
U2B
UMB
UgB
U2B
V�B
VmB
V�B
VmB
V�B
WYB
W�B
W�B
W�B
X+B
X+B
XyB
X�B
XyB
X�B
YB
Y1B
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
[	B
[�B
\B
\B
\B
\)B
\CB
\xB
\�B
]IB
]�B
^B
^OB
^jB
^jB
^�B
^�B
^�B
_;B
_VB
_pB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`�B
`�B
`�B
aB
a|B
a�B
a�B
a�B
b�B
bhB
b�B
c B
c:B
cB
cB
c B
c:B
c�B
c�B
c�B
c�B
d&B
dZB
dtB
d�B
d�B
d�B
d�B
e�B
e�B
fB
fB
e�B
e�B
f2B
fB
f2B
gB
h$B
hsB
h�B
h�B
h�B
h�B
i*B
i*B
iB
iB
i_B
i�B
jeB
j�B
j�B
j�B
j�B
j�B
j�B
k6B
kQB
kQB
k�B
k�B
k�B
k�B
l=B
l=B
l�B
mB
m)B
mCB
mCB
m]B
m�B
m�B
m�B
ncB
n}B
n�B
oB
oiB
oOB
o�B
o�B
p;B
p;B
p;B
p�B
p�B
p�B
p�B
p�B
qvB
q�B
q�B
q�B
q�B
q�B
rB
rB
r-B
r�B
r�B
r�B
sB
sMB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
t9B
t�B
t�B
t�B
uB
uB
uZB
utB
u�B
u�B
vFB
vzB
v�B
wB
wB
v�B
w2B
wfB
w�B
w�B
w�B
w�B
w�B
xB
xB
x8B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
yXB
yXB
y�B
y�B
y�B
y�B
z�B
zxB
z�B
|B
|B
|B
|B
|B
|PB
}"B
}<B
}�B
~(B
~(B
~]B
~wB
~wB
~�B
~�B
~�B
~�B
}B
�B
�B
�B
�4B
�OB
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	_�B	e�B	g�B	gRB	gRB	g8B	gRB	gRB	g�B	g�B	h$B	g�B	g�B	g�B	hXB	hsB	i�B	i�B	kB	lB	l�B	l�B	m�B	s�B	~]B	��B	��B	�?B	�eB	�=B	�qB	��B	��B	��B	��B
��B
��BpUBs�ByXB�XB�:B�B�B��B�B�B	BPB�B�BB�*B��B�+B�B��B�iB�kB�6B��B��B�BҽB�;B�!B�4B�`B��B�B��B�7B��B��Bc�B&�B
�B
��B
g�B
VSB
BuB
-CB
�B

	B
AB	�MB	��B	бB	�JB	�B	�B	t�B	T�B	8�B	%B	 �B	/B	�B	%B�B�>B	�B	WsB	jB	UgB	T,B	VB	Q�B	T�B	ZB	a�B	\�B	^5B	cnB	e�B	gB	jB	n�B	r�B	v�B	yXB	~wB	��B	�?B	�'B	�B	��B	��B	��B	��B	�1B	��B	�B	�TB	�B	�LB	�wB	�B	��B	�$B	�jB	��B
  B
 �B
�B
9B
?B
B
AB	��B	�B	�B	�	B	�qB
�B
	�B	��B	�^B	�]B	�B
^B
gB
%B
�B
�B
�B
�B
uB
[B
�B
UB
 �B
 �B
 B
 �B
oB
uB
gB
_B
�B
�B
�B
SB
9B
�B
SB
�B
9B
�B
�B
�B
�B
?B
B
?B
�B
�B
_B
B
B

�B
�B
	�B
	�B
fB
�B
�B
�B
�B
�B
?B
YB
tB
fB
fB
	�B
xB

rB
	RB
�B
�B
�B
�B
�B
�B
B
-B
oB
UB
 OB
  B	��B
 �B
uB
{B
�B
�B	��B	�B
B
SB

�B
DB

�B
	�B
	7B
�B
	�B
	�B
�B
�B
�B
3B
MB
B
AB
oB
{B
�B
%B
SB
MB
B
gB
B
�B
B
�B
 �B	�}B	��B	�(B	��B	�dB	��B	��B	��B	��B	�xB	�xB	�xB	�DB	��B	�>B	��B	��B	��B	�jB	��B	��B	��B	��B	�*B	��B	��B	�	B	�B	��B	��B	��B	��B	��B	�2B	��B	��B	��B	��B	��B	�B	�B	��B	�tB	�9B	��B	��B	�'B	��B	�!B	�B	��B	��B	��B	�oB	�B	�B	�'B	��B	�IB	�/B	�`B	�LB	��B	�2B	�B	�B	�KB	��B	��B	��B	�=B	�=B	�=B	�B	��B	�qB	��B	��B	�qB	��B	�CB	��B	�/B	�B	� B	�B	�B	��B	��B	�AB	�B	��B	�B	�B	�B	�B	��B	��B	�9B	�9B	�`B	�fB	��B	��B	�RB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�jB	�PB	�B	��B	�jB	�6B	�jB	�6B	�jB	�PB	�PB	�6B	�6B	�B	�6B	��B	��B	��B	��B	��B	�"B	��B	�(B	��B	�B	��B
  B	��B	��B	��B	��B	�cB	�.B	�.B	��B	��B	��B	��B	��B	�.B	�B	�cB	��B	�HB	�B	��B	��B	��B	��B	�]B	�]B	��B	��B	��B
 B
UB
�B
�B
�B
�B
�B
�B
[B
3B
-B
 �B	�.B	�]B	��B	�B	�qB	��B	��B	��B	��B	��B	�MB	�hB	�%B	��B	�jB
oB
�B
B
?B
_B
YB
B
1B
�B
	7B

�B
�B
�B
"B
<B
�B
�B
B
B
}B
B
�B
�B
&B
&B
�B
MB
�B
�B

B
YB
YB
�B
�B
�B
�B
�B
KB
KB
1B
KB
�B
�B
B
B
�B
qB
WB
)B
�B
�B
�B
dB
�B
B
�B
�B
B
�B
�B
�B
�B
�B
B
pB
�B
 BB
 �B
!-B
!bB
"B
"NB
"hB
"�B
# B
#�B
#B
#�B
#�B
#�B
$&B
$@B
$@B
$@B
$@B
$B
$&B
$ZB
$�B
$�B
%�B
%�B
&LB
%,B
%�B
&�B
&�B
'RB
(>B
)*B
*0B
*eB
+kB
*�B
+6B
+�B
+QB
+6B
+�B
+�B
+�B
+QB
+B
+B
,"B
,qB
,�B
,�B
,�B
,�B
,�B
,�B
-)B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/OB
/�B
/B
0!B
1'B
1AB
1[B
1AB
1�B
1�B
1�B
1[B
1AB
1vB
1[B
1vB
1�B
1�B
3�B
5ZB
5�B
4�B
4B
3�B
3hB
3MB
3�B
5ZB
5�B
6zB
6B
6B
5�B
6`B
6�B
6�B
72B
9XB
9�B
;B
;�B
;�B
;�B
;�B
<6B
<�B
<�B
<�B
<�B
<�B
=VB
=qB
>B
?}B
@4B
@ B
?�B
?.B
?B
@�B
A;B
BuB
B�B
BB
BAB
CGB
CGB
B�B
CB
CaB
D�B
D�B
E�B
FtB
F�B
GB
G_B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
IB
IB
IB
IRB
I�B
I�B
I�B
J=B
J	B
JXB
J�B
KDB
KDB
KxB
K�B
K�B
K�B
K�B
K�B
K�B
L0B
L�B
L�B
L�B
L�B
MB
MB
M�B
M�B
NVB
NpB
NpB
NpB
NpB
NpB
N�B
N�B
N�B
N�B
N�B
N�B
O(B
O\B
O�B
OvB
O�B
P}B
P�B
P�B
P�B
Q4B
Q4B
Q4B
QNB
QNB
Q�B
Q�B
Q�B
Q�B
RB
RB
R�B
R�B
R�B
R�B
SB
S&B
R�B
S�B
S�B
S�B
TFB
TFB
TaB
T{B
T�B
T�B
T�B
T�B
UB
UB
U2B
U2B
UMB
UgB
U2B
V�B
VmB
V�B
VmB
V�B
WYB
W�B
W�B
W�B
X+B
X+B
XyB
X�B
XyB
X�B
YB
Y1B
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
[	B
[�B
\B
\B
\B
\)B
\CB
\xB
\�B
]IB
]�B
^B
^OB
^jB
^jB
^�B
^�B
^�B
_;B
_VB
_pB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`�B
`�B
`�B
aB
a|B
a�B
a�B
a�B
b�B
bhB
b�B
c B
c:B
cB
cB
c B
c:B
c�B
c�B
c�B
c�B
d&B
dZB
dtB
d�B
d�B
d�B
d�B
e�B
e�B
fB
fB
e�B
e�B
f2B
fB
f2B
gB
h$B
hsB
h�B
h�B
h�B
h�B
i*B
i*B
iB
iB
i_B
i�B
jeB
j�B
j�B
j�B
j�B
j�B
j�B
k6B
kQB
kQB
k�B
k�B
k�B
k�B
l=B
l=B
l�B
mB
m)B
mCB
mCB
m]B
m�B
m�B
m�B
ncB
n}B
n�B
oB
oiB
oOB
o�B
o�B
p;B
p;B
p;B
p�B
p�B
p�B
p�B
p�B
qvB
q�B
q�B
q�B
q�B
q�B
rB
rB
r-B
r�B
r�B
r�B
sB
sMB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
t9B
t�B
t�B
t�B
uB
uB
uZB
utB
u�B
u�B
vFB
vzB
v�B
wB
wB
v�B
w2B
wfB
w�B
w�B
w�B
w�B
w�B
xB
xB
x8B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
yXB
yXB
y�B
y�B
y�B
y�B
z�B
zxB
z�B
|B
|B
|B
|B
|B
|PB
}"B
}<B
}�B
~(B
~(B
~]B
~wB
~wB
~�B
~�B
~�B
~�B
}B
�B
�B
�B
�4B
�OB
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104932  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174358  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174359  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174359                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024406  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024406  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                