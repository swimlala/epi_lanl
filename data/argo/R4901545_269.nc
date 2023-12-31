CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-09-16T14:00:40Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  px   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ވ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޸   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20200916140040  20200916140040  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL              A   AO  4741                            2B  A   NAVIS_A                         0185                            052512                          863 @�8����1   @�8�lі@7r� Ĝ�dƧ1   GPS     Primary sampling: mixed [deep: discrete, shallow: averaged]                                                                                                                                                                                                       A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @vff@���@���AffA:ffAZffAzffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo� Cq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D i�D �Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�D	i�D	� D
i�D
�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Dc4D�Di�D�Di�D�D i�D �D!i�D!�D"i�D"�D#i�D#�D$i�D$�D%i�D%�D&i�D&�D'i�D'�D(i�D(�D)i�D)�D*i�D*�D+i�D+�D,i�D,�D-i�D-�D.i�D.�D/i�D/�D0i�D0�D1i�D1�D2i�D2�D3i�D3�D4i�D4�D5i�D5�D6i�D6�D7i�D7�D8i�D8�D9i�D9�D:i�D:�D;i�D;�D<i�D<�D=i�D=�D>i�D>�D?i�D?�D@i�D@�DAi�DA�DBi�DB�DCi�DC�DDi�DD�DEi�DE�DFi�DF�DGi�DG�DHi�DH�DIi�DI�DJi�DJ�DKi�DK�DLi�DL�DMi�DM�DNi�DN�DOi�DO�DPi�DP�DQi�DQ�DRi�DR�DSi�DS�DTi�DT�DUi�DU�DVi�DV�DWi�DW�DXi�DX�DYi�DY�DZi�DZ�D[i�D[�D\i�D\�D]i�D]�D^i�D^�D_i�D_�D`i�D`�Dai�Da�Dbi�Db�Dci�Dc�Ddi�Dd�Dei�De�Dfi�Df�Dgi�Dg�Dhi�Dh�Dii�Di�Dji�Dj�Dki�Dk�Dli�Dl�Dmi�Dm�Dni�Dn�Doi�Do�Dpi�Dp�Dqi�Dq�Dri�Dr�Dsi�Ds�Dti�Dt�Dui�Du�Dvi�Dv�Dwi�Dw�Dxi�Dx�Dyi�Dy�Dzi�Dz�D{i�D{�D|i�D|�D}i�D}�D~i�D~�Di�D�D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�1�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D´�D���D�4�D�t�Dô�D���D�4�D�t�DĴ�D���D�4�D�t�DŴ�D���D�4�D�t�Dƴ�D���D�4�D�t�DǴ�D���D�4�D�t�Dȴ�D���D�4�D�t�Dɴ�D���D�4�D�t�Dʴ�D���D�4�D�t�D˴�D���D�4�D�t�D̴�D���D�4�D�t�Dʹ�D���D�4�D�t�Dδ�D���D�4�D�t�Dϴ�D���D�4�D�t�Dд�D���D�4�D�t�DѴ�D���D�4�D�t�DҴ�D���D�4�D�t�DӴ�D���D�4�D�t�DԴ�D���D�4�D�t�Dմ�D���D�4�D�t�Dִ�D���D�4�D�t�D״�D���D�4�D�t�Dش�D���D�4�D�t�Dٴ�D���D�4�D�t�Dڴ�D���D�4�D�t�D۴�D���D�4�D�t�Dܴ�D���D�4�D�t�Dݴ�D���D�4�D�t�D޴�D���D�4�D�t�Dߴ�D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�x D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D���D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D��D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�t�D���D���D�4�D�x D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AڍPAڏ\AڋDAڍPAڏ\AڍPAڍPAڑhAڏ\AځA�ffA�hsA�ffA�^5A�ZA�XA�O�A�A�A�9XA�1'A�/A��A�%A���A�ƨA�jA�oAظRA���A�%A�A�;dA���A��mA��A��A�S�AʶFA�"�A�-A�VA�I�A�S�A���A���A�
=A��uA���A��A�x�A��A���A��/A���A�A���A��A��A��/A�XA�oA�n�A�VA�ffA���A��A�t�A�A�VA���A�9XA��A�n�A�  A��hA�?}A�ZA���A�t�A��FA���A��!A�`BA�^5A��A�$�A��A�?}A��jA�Q�A�v�A��A�l�A���A���A��A���A��A���A~  A{Ay|�Ax�!Avv�As�Ar��Ar�+Ap�Al��Aj~�Ag`BAf�Ae�wAdbNAa?}A^I�A\��A[��A[oAY��AX�+AW/AU��AS%ARA�AQ�AO��AM�ALE�AK�mAK�AJ{AHM�AF�`AEO�ACG�ABz�AB^5ABbAAp�A?�A=�A<�uA<9XA;�A;��A;�PA9|�A8�A7t�A6�yA6ZA5�hA3��A2�A1x�A0��A0bNA/�;A/C�A.$�A-��A-?}A+G�A)�FA(n�A'�;A'%A%t�A#��A#oA"5?A!�
A!��A ��A�PA~�AbA�A�jAJAdZAbNAt�AA�A�AZA
=AƨAI�A�A`BAr�A��A%A�DA"�A
jA
A	�A�At�A;dAffA�hA�A �A�
A�A�RAr�AM�A �A\)A ��@��!@��@�  @�+@��^@���@�r�@�@��R@�J@��@� �@�E�@�h@��@�Z@��#@�ff@�&�@�bN@��@��@���@��@�X@��@�bN@���@އ+@�{@݁@ܬ@�@�ƨ@��@Լj@�r�@�b@�ƨ@��;@҇+@�/@��@�C�@�E�@��`@�Z@��y@��@�A�@��@�V@�n�@�n�@���@�o@�ȴ@�5?@őh@�/@��@�ƨ@���@�M�@�J@�C�@���@�O�@�r�@���@�K�@��R@�@���@��`@�Q�@�+@�J@���@�@���@�O�@���@�Q�@�1@��P@�@�5?@�x�@���@�ƨ@��@�C�@��H@�n�@�=q@�=q@���@��9@�bN@�b@��m@���@�=q@��7@��j@�Q�@�b@�l�@�|�@�C�@��@�@���@�M�@��@�O�@�Q�@�~�@��@��@��h@�X@��/@��@�A�@�S�@��y@�ȴ@���@�^5@�{@�@���@�@�G�@�%@���@���@��D@�  @�
=@��@�X@���@���@�9X@���@��@�~�@�G�@��@��j@�b@���@�|�@�@���@�M�@��@���@��7@�p�@�`B@�7L@��9@�I�@�1@���@���@��F@�|�@�33@�ff@���@��@���@��#@�{@�V@�E�@���@���@��u@�1'@�K�@�"�@�ȴ@�n�@��@��@��@��@��@���@�@��-@�hs@�V@��u@��@���@�l�@�l�@�\)@��@���@���@���@���@�ff@�$�@�{@�{@���@���@��-@���@�x�@�`B@�G�@��@��@�V@��@���@���@�Ĝ@��9@��D@�bN@�Z@�Q�@�I�@�A�@� �@� �@��@���@��w@��F@��F@��@���@�|�@�dZ@�;d@��@���@��R@�{@��@��#@��-@��^@��-@��@�7L@�%@��`@��9@���@��D@� �@�P@~�@;d@\)@;d@~�y@~5?@}`B@|��@|(�@{C�@z��@z=q@zJ@yX@xbN@x1'@w�;@w��@wl�@w\)@w+@v��@u�@u�h@t��@t�D@t1@sƨ@st�@sdZ@sS�@so@r�\@r~�@r^5@q��@q�^@qx�@q%@p�9@pQ�@p  @o�;@ol�@nȴ@nff@n@m�-@m�h@m/@m�@l�@l�D@lZ@l1@k�@j�@j~�@j�@i��@i�#@i�7@i&�@h��@h�@hA�@gl�@gK�@g;d@g�@g
=@f�+@e��@e@ep�@e?}@e�@eV@d��@d�j@dZ@d1@c�
@c�F@cS�@c@b��@b-@b�@a��@a��@a�7@a7L@`��@`�9@`bN@`b@_�@^ȴ@^E�@]@\�j@\j@\�@[�
@[�F@[�F@[��@[��@[t�@[dZ@[S�@[33@Z��@Z�\@Zn�@Z^5@Z=q@ZJ@Y�@Y��@Y��@Y&�@XQ�@Xb@WK�@V$�@U�h@UO�@UV@T�D@T9X@T1@S��@S�@S"�@S@R�@R�H@R�\@Q�#@Qhs@P��@P��@PA�@P  @O�w@Ol�@O+@N�y@N�R@Nv�@N5?@N@M�@M�-@MO�@L�@L�@L��@Lj@K��@K�F@KC�@K"�@K"�@Ko@J�@J�\@JM�@J-@J�@J�@J�@J�@I��@IX@I7L@H�@G�@Gl�@Fȴ@Fv�@F$�@E�@E�@E`B@EO�@EV@D��@C��@C��@C��@C�
@CdZ@CS�@CS�@CC�@C@B��@B-@A��@A��@Ax�@A7L@@�@@Q�@@ �@?�@?;d@?�@>�y@>�+@>E�@>{@=@=`B@<�j@<z�@<Z@<I�@<9X@<(�@<(�@<�@<1@<1@;ƨ@;C�@:�@:��@:n�@:-@9��@9��@9�#@9�^@9hs@8Q�@7�P@7K�@7;d@6��@6ff@6$�@5�@5V@4�D@4j@3�
@3S�@3"�@2�@2��@2��@2��@2~�@2n�@2M�@2-@2J@1�@1��@1%@0�`@0��@0��@0A�@0  @/�@/�@/�;@/�w@/��@/�P@/K�@.�y@.��@.�R@/
=@.ȴ@.�R@.v�@.E�@.@-�-@-O�@,��@,��@,�@,�/@,�@,��@,z�@,Z@,(�@,1@+�m@+��@+o@*�H@*��@*�!@*~�@*^5@*M�@*J@)��@)X@)X@)X@)G�@(��@(r�@( �@(  @'�;@'��@'|�@'K�@&��@&��@&��@&v�@&ff@&V@&5?@%��@%�h@%�@$�@$��@$1@#�@#S�@#"�@"�@"��@"=q@"J@!��@!��@!hs@!G�@!G�@!G�@ ��@ Ĝ@ r�@ A�@�@��@��@�@��@��@�R@��@�+@v�@ff@5?@@�@��@��@O�@/@/@V@��@�j@I�@�@��@�m@�F@�@t�@dZ@33@o@@�H@�\@^5@J@�^@�7@x�@%@Ĝ@��@�u@r�@Q�@b@�@�w@K�@��@��@�y@�+@{@@��@@�-@�-@��@`B@O�@O�@/@/@�/@z�@9X@1@�
@��@t�@t�@t�@C�@��@�\@^5@=q@�@�#@x�@7L@%@��@Ĝ@�9@bN@Q�@1'@  @�w@�w@��@l�@+@
=@��@�y@�@ȴ@��@ff@5?@{@�@�T@�-@��@�h@�@�@�@O�@V@V@V@��@�@��@�D@j@9X@(�@�@��@t�@dZ@33@"�@@
�H@
��@
��@
^5@
-@
�@	�@	��@	��@	�^@	�^@	hs@	&�@	%@��@�`@�u@  @�;@��@|�@|�@l�@\)@;d@��@�@�R@��@v�@ff@V@5?@@�@��@��@/@V@�/@�@9X@�@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AڍPAڏ\AڋDAڍPAڏ\AڍPAڍPAڑhAڏ\AځA�ffA�hsA�ffA�^5A�ZA�XA�O�A�A�A�9XA�1'A�/A��A�%A���A�ƨA�jA�oAظRA���A�%A�A�;dA���A��mA��A��A�S�AʶFA�"�A�-A�VA�I�A�S�A���A���A�
=A��uA���A��A�x�A��A���A��/A���A�A���A��A��A��/A�XA�oA�n�A�VA�ffA���A��A�t�A�A�VA���A�9XA��A�n�A�  A��hA�?}A�ZA���A�t�A��FA���A��!A�`BA�^5A��A�$�A��A�?}A��jA�Q�A�v�A��A�l�A���A���A��A���A��A���A~  A{Ay|�Ax�!Avv�As�Ar��Ar�+Ap�Al��Aj~�Ag`BAf�Ae�wAdbNAa?}A^I�A\��A[��A[oAY��AX�+AW/AU��AS%ARA�AQ�AO��AM�ALE�AK�mAK�AJ{AHM�AF�`AEO�ACG�ABz�AB^5ABbAAp�A?�A=�A<�uA<9XA;�A;��A;�PA9|�A8�A7t�A6�yA6ZA5�hA3��A2�A1x�A0��A0bNA/�;A/C�A.$�A-��A-?}A+G�A)�FA(n�A'�;A'%A%t�A#��A#oA"5?A!�
A!��A ��A�PA~�AbA�A�jAJAdZAbNAt�AA�A�AZA
=AƨAI�A�A`BAr�A��A%A�DA"�A
jA
A	�A�At�A;dAffA�hA�A �A�
A�A�RAr�AM�A �A\)A ��@��!@��@�  @�+@��^@���@�r�@�@��R@�J@��@� �@�E�@�h@��@�Z@��#@�ff@�&�@�bN@��@��@���@��@�X@��@�bN@���@އ+@�{@݁@ܬ@�@�ƨ@��@Լj@�r�@�b@�ƨ@��;@҇+@�/@��@�C�@�E�@��`@�Z@��y@��@�A�@��@�V@�n�@�n�@���@�o@�ȴ@�5?@őh@�/@��@�ƨ@���@�M�@�J@�C�@���@�O�@�r�@���@�K�@��R@�@���@��`@�Q�@�+@�J@���@�@���@�O�@���@�Q�@�1@��P@�@�5?@�x�@���@�ƨ@��@�C�@��H@�n�@�=q@�=q@���@��9@�bN@�b@��m@���@�=q@��7@��j@�Q�@�b@�l�@�|�@�C�@��@�@���@�M�@��@�O�@�Q�@�~�@��@��@��h@�X@��/@��@�A�@�S�@��y@�ȴ@���@�^5@�{@�@���@�@�G�@�%@���@���@��D@�  @�
=@��@�X@���@���@�9X@���@��@�~�@�G�@��@��j@�b@���@�|�@�@���@�M�@��@���@��7@�p�@�`B@�7L@��9@�I�@�1@���@���@��F@�|�@�33@�ff@���@��@���@��#@�{@�V@�E�@���@���@��u@�1'@�K�@�"�@�ȴ@�n�@��@��@��@��@��@���@�@��-@�hs@�V@��u@��@���@�l�@�l�@�\)@��@���@���@���@���@�ff@�$�@�{@�{@���@���@��-@���@�x�@�`B@�G�@��@��@�V@��@���@���@�Ĝ@��9@��D@�bN@�Z@�Q�@�I�@�A�@� �@� �@��@���@��w@��F@��F@��@���@�|�@�dZ@�;d@��@���@��R@�{@��@��#@��-@��^@��-@��@�7L@�%@��`@��9@���@��D@� �@�P@~�@;d@\)@;d@~�y@~5?@}`B@|��@|(�@{C�@z��@z=q@zJ@yX@xbN@x1'@w�;@w��@wl�@w\)@w+@v��@u�@u�h@t��@t�D@t1@sƨ@st�@sdZ@sS�@so@r�\@r~�@r^5@q��@q�^@qx�@q%@p�9@pQ�@p  @o�;@ol�@nȴ@nff@n@m�-@m�h@m/@m�@l�@l�D@lZ@l1@k�@j�@j~�@j�@i��@i�#@i�7@i&�@h��@h�@hA�@gl�@gK�@g;d@g�@g
=@f�+@e��@e@ep�@e?}@e�@eV@d��@d�j@dZ@d1@c�
@c�F@cS�@c@b��@b-@b�@a��@a��@a�7@a7L@`��@`�9@`bN@`b@_�@^ȴ@^E�@]@\�j@\j@\�@[�
@[�F@[�F@[��@[��@[t�@[dZ@[S�@[33@Z��@Z�\@Zn�@Z^5@Z=q@ZJ@Y�@Y��@Y��@Y&�@XQ�@Xb@WK�@V$�@U�h@UO�@UV@T�D@T9X@T1@S��@S�@S"�@S@R�@R�H@R�\@Q�#@Qhs@P��@P��@PA�@P  @O�w@Ol�@O+@N�y@N�R@Nv�@N5?@N@M�@M�-@MO�@L�@L�@L��@Lj@K��@K�F@KC�@K"�@K"�@Ko@J�@J�\@JM�@J-@J�@J�@J�@J�@I��@IX@I7L@H�@G�@Gl�@Fȴ@Fv�@F$�@E�@E�@E`B@EO�@EV@D��@C��@C��@C��@C�
@CdZ@CS�@CS�@CC�@C@B��@B-@A��@A��@Ax�@A7L@@�@@Q�@@ �@?�@?;d@?�@>�y@>�+@>E�@>{@=@=`B@<�j@<z�@<Z@<I�@<9X@<(�@<(�@<�@<1@<1@;ƨ@;C�@:�@:��@:n�@:-@9��@9��@9�#@9�^@9hs@8Q�@7�P@7K�@7;d@6��@6ff@6$�@5�@5V@4�D@4j@3�
@3S�@3"�@2�@2��@2��@2��@2~�@2n�@2M�@2-@2J@1�@1��@1%@0�`@0��@0��@0A�@0  @/�@/�@/�;@/�w@/��@/�P@/K�@.�y@.��@.�R@/
=@.ȴ@.�R@.v�@.E�@.@-�-@-O�@,��@,��@,�@,�/@,�@,��@,z�@,Z@,(�@,1@+�m@+��@+o@*�H@*��@*�!@*~�@*^5@*M�@*J@)��@)X@)X@)X@)G�@(��@(r�@( �@(  @'�;@'��@'|�@'K�@&��@&��@&��@&v�@&ff@&V@&5?@%��@%�h@%�@$�@$��@$1@#�@#S�@#"�@"�@"��@"=q@"J@!��@!��@!hs@!G�@!G�@!G�@ ��@ Ĝ@ r�@ A�@�@��@��@�@��@��@�R@��@�+@v�@ff@5?@@�@��@��@O�@/@/@V@��@�j@I�@�@��@�m@�F@�@t�@dZ@33@o@@�H@�\@^5@J@�^@�7@x�@%@Ĝ@��@�u@r�@Q�@b@�@�w@K�@��@��@�y@�+@{@@��@@�-@�-@��@`B@O�@O�@/@/@�/@z�@9X@1@�
@��@t�@t�@t�@C�@��@�\@^5@=q@�@�#@x�@7L@%@��@Ĝ@�9@bN@Q�@1'@  @�w@�w@��@l�@+@
=@��@�y@�@ȴ@��@ff@5?@{@�@�T@�-@��@�h@�@�@�@O�@V@V@V@��@�@��@�D@j@9X@(�@�@��@t�@dZ@33@"�@@
�H@
��@
��@
^5@
-@
�@	�@	��@	��@	�^@	�^@	hs@	&�@	%@��@�`@�u@  @�;@��@|�@|�@l�@\)@;d@��@�@�R@��@v�@ff@V@5?@@�@��@��@/@V@�/@�@9X@�@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�=B�7B�7B�=B�7B�7B�7B�7B�7B�7B�DB�PB�\B�hB�oB�uB�{B��B��B��B��B��B��B��B��B�bB�DB�Bx�Bm�Bp�Bt�Bo�Bm�Bl�BhsBp�B�Bw�Bl�Bk�Bq�Bt�Bv�Bx�B�=B�{B��B�B�B�B�wB��B��B��B�
B�B�`B�B�B  BB  B��B�B�B�TB��B�^B�dBB��B��B�qB�uB}�Bn�BffBVBG�B8RB$�B�BDBB
��B
�TB
��B
��B
�B
��B
��B
�oB
�+B
{�B
s�B
m�B
YB
D�B
/B
�B
VB
%B	��B	�sB	�HB	�)B	��B	�^B	�B	��B	�uB	�JB	�B	r�B	dZB	[#B	VB	Q�B	K�B	D�B	=qB	5?B	+B	%�B	!�B	�B	\B	
=B	+B	B��B��B�B�B�`B�NB�HB�;B�)B��B��B��B��B��B��B��BƨBĜBB��B�wB�dB�LB�3B�'B�!B�B�B�B��B��B��B��B��B�\B�DB�%B~�Bx�Bu�Bs�Bq�Bo�Bl�BiyBgmBgmBe`BdZBcTBbNB`BB^5B\)BYBW
BVBT�BS�BR�BP�BP�BO�BP�BO�BP�BO�BN�BM�BP�BP�BQ�BR�BR�BR�BQ�BQ�BP�BR�BR�BR�BQ�BQ�BP�BQ�BR�BR�BR�BT�BT�BT�BVBVBVBW
BT�BQ�BW
BW
BT�BO�BF�BA�B@�BD�BG�BI�BH�BI�BN�BM�BL�BL�BL�BL�BJ�BI�BH�BG�BG�BG�BH�BL�BN�BQ�BQ�BYB`BBbNBcTBbNBdZBe`BdZBgmBl�Bo�Br�By�B}�B�B�B�1B�=B�=B�hB��B��B��B��B��B�B�B�-B�-B�3B�9B�3B�'B�!B�9B�RB�^B�qB��BÖBȴB��B��B��B��B��B��B��B�B�B�
B�B�B�B�B�B�B�B�B�B�B�B�#B�5B�BB�HB�`B�B�B�B��B��B��B��B��B��B	+B	1B		7B	DB	JB	\B	hB	oB	�B	�B	�B	�B	�B	 �B	 �B	 �B	!�B	$�B	&�B	&�B	'�B	(�B	+B	0!B	7LB	<jB	?}B	@�B	C�B	D�B	F�B	F�B	F�B	F�B	G�B	G�B	F�B	F�B	G�B	I�B	K�B	K�B	K�B	M�B	N�B	O�B	P�B	S�B	XB	ZB	ZB	\)B	]/B	_;B	bNB	hsB	k�B	l�B	m�B	q�B	x�B	}�B	~�B	�B	�B	�B	�+B	�VB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�?B	�FB	�RB	�jB	�wB	�}B	��B	B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�HB	�NB	�ZB	�ZB	�ZB	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B
	7B

=B
DB
DB
DB
DB
DB
JB
PB
VB
\B
\B
\B
\B
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
{B
{B
{B
{B
{B
{B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
#�B
#�B
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
%�B
%�B
$�B
%�B
&�B
'�B
(�B
)�B
+B
+B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
33B
2-B
2-B
2-B
33B
33B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
9XB
:^B
:^B
:^B
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
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
D�B
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
G�B
F�B
E�B
E�B
E�B
E�B
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
H�B
I�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
O�B
O�B
P�B
Q�B
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
VB
VB
VB
VB
VB
VB
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
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
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
dZB
dZB
dZB
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
hsB
hsB
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
iyB
jB
jB
iyB
iyB
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
o�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
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
z�B
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
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�=B�7B�7B�=B�7B�7B�7B�7B�7B�7B�DB�PB�\B�hB�oB�uB�{B��B��B��B��B��B��B��B��B�bB�DB�Bx�Bm�Bp�Bt�Bo�Bm�Bl�BhsBp�B�Bw�Bl�Bk�Bq�Bt�Bv�Bx�B�=B�{B��B�B�B�B�wB��B��B��B�
B�B�`B�B�B  BB  B��B�B�B�TB��B�^B�dBB��B��B�qB�uB}�Bn�BffBVBG�B8RB$�B�BDBB
��B
�TB
��B
��B
�B
��B
��B
�oB
�+B
{�B
s�B
m�B
YB
D�B
/B
�B
VB
%B	��B	�sB	�HB	�)B	��B	�^B	�B	��B	�uB	�JB	�B	r�B	dZB	[#B	VB	Q�B	K�B	D�B	=qB	5?B	+B	%�B	!�B	�B	\B	
=B	+B	B��B��B�B�B�`B�NB�HB�;B�)B��B��B��B��B��B��B��BƨBĜBB��B�wB�dB�LB�3B�'B�!B�B�B�B��B��B��B��B��B�\B�DB�%B~�Bx�Bu�Bs�Bq�Bo�Bl�BiyBgmBgmBe`BdZBcTBbNB`BB^5B\)BYBW
BVBT�BS�BR�BP�BP�BO�BP�BO�BP�BO�BN�BM�BP�BP�BQ�BR�BR�BR�BQ�BQ�BP�BR�BR�BR�BQ�BQ�BP�BQ�BR�BR�BR�BT�BT�BT�BVBVBVBW
BT�BQ�BW
BW
BT�BO�BF�BA�B@�BD�BG�BI�BH�BI�BN�BM�BL�BL�BL�BL�BJ�BI�BH�BG�BG�BG�BH�BL�BN�BQ�BQ�BYB`BBbNBcTBbNBdZBe`BdZBgmBl�Bo�Br�By�B}�B�B�B�1B�=B�=B�hB��B��B��B��B��B�B�B�-B�-B�3B�9B�3B�'B�!B�9B�RB�^B�qB��BÖBȴB��B��B��B��B��B��B��B�B�B�
B�B�B�B�B�B�B�B�B�B�B�B�#B�5B�BB�HB�`B�B�B�B��B��B��B��B��B��B	+B	1B		7B	DB	JB	\B	hB	oB	�B	�B	�B	�B	�B	 �B	 �B	 �B	!�B	$�B	&�B	&�B	'�B	(�B	+B	0!B	7LB	<jB	?}B	@�B	C�B	D�B	F�B	F�B	F�B	F�B	G�B	G�B	F�B	F�B	G�B	I�B	K�B	K�B	K�B	M�B	N�B	O�B	P�B	S�B	XB	ZB	ZB	\)B	]/B	_;B	bNB	hsB	k�B	l�B	m�B	q�B	x�B	}�B	~�B	�B	�B	�B	�+B	�VB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�?B	�FB	�RB	�jB	�wB	�}B	��B	B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�HB	�NB	�ZB	�ZB	�ZB	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B
	7B

=B
DB
DB
DB
DB
DB
JB
PB
VB
\B
\B
\B
\B
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
{B
{B
{B
{B
{B
{B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
#�B
#�B
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
%�B
%�B
$�B
%�B
&�B
'�B
(�B
)�B
+B
+B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
33B
2-B
2-B
2-B
33B
33B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
9XB
:^B
:^B
:^B
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
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
D�B
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
G�B
F�B
E�B
E�B
E�B
E�B
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
H�B
I�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
O�B
O�B
P�B
Q�B
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
VB
VB
VB
VB
VB
VB
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
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
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
dZB
dZB
dZB
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
hsB
hsB
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
iyB
jB
jB
iyB
iyB
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
o�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
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
z�B
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
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.35 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20200916140040                              AO  ARCAADJP                                                                    20200916140040    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200916140040  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200916140040  QCF$                G�O�G�O�G�O�0               