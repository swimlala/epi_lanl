CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:23:02Z creation;2022-06-04T19:23:03Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192302  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               FA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�Ud���1   @�Ud�S��@,@�n���c�fffff1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@y��@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B���B�33B���B���B���B�  B�  B�  B�ffB���B�33B�  B�  B�  B�  BÙ�B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~fD~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ Dă3D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D���D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@&fg@ffg@�ff@�ffA33A;33A[33A{33A���A�fgA���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B/33B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B���B�  B���B�33B�  B�  B�ffB�ffB�ffB���B�  B���B�ffB�ffB�ffB�ffB�  B�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9��C;�3C=�3C?�3CA��CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��gC��gC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��gC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dw��Dxl�Dx��Dyl�Dy��Dzl�Dz��D{l�D{��D|l�D|��D}l�D}�3D~l�D~��Dl�D��D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��3D��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD���D��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD¶fD��fD�6fD�vfDöfD��fD�6fD�y�DĶfD��fD�6fD�vfDŶfD��fD�6fD�vfDƶfD��fD�6fD�vfDǶfD��fD�6fD�vfDȶfD��fD�6fD�vfDɶfD��fD�6fD�vfDʶfD��fD�6fD�vfD˶fD��fD�6fD�vfD̶fD��fD�6fD�vfDͶfD��fD�6fD�vfDζfD��fD�6fD�vfD϶fD��fD�6fD�vfDжfD��fD�6fD�vfDѶfD��fD�6fD�vfDҶfD��fD�6fD�vfDӶfD��fD�6fD�vfDԶfD��fD�6fD�vfDնfD��fD�6fD�vfDֶfD��fD�6fD�vfD׶fD��fD�6fD�vfDضfD��fD�6fD�vfDٶfD��fD�6fD�vfDڶfD��fD�6fD�vfD۶fD��fD�6fD�vfDܶfD��fD�6fD�vfDݶfD��fD�6fD�vfD޶fD��fD�6fD�vfD߶fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��3D�6fD�vfD��3D��fD�6fD�vfD��fD�ٙ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�D�A�U2A�W?A�WsA�[#A�\]A�]/A�\�A�\�A�]�A�^�A�^�A�aA�a�A�aA�bNA�c�A�cTA�d&A�c�A�f2A�g8A�h
A�jA�jA�i�A�iA�jA�i�A�iyA�hsA�gmA�uAƋA���A��"A�r|A�|�A�f�A���A�v�A���A�A A���A��]A�x�A��A��8A�2�A�W�A�%A~�A{�?AzS&Aw�-As�Ak�~Ah��Af��Ac��A`�6A[rGAY�rAYB[AV�@AR�AQ��AP�~AOs�AMy�AK+�AI�!AG�AB��A=.�A9�A7MA4��A3(�A2C�A0��A.�A-+A+�A+e,A*)�A)�}A)�A(��A(,�A(SA'zA&��A&ϫA'TaA)m�A)��A(�\A'�A%
=A!�hA4nA�'A�A��A��AT�A��A�$A!�A8A�aA�A=qA~A\�A�hA�*AȴA,�AkQA;dA�]A8�A�fA2�A�KAw2A�DA��A�9A0�AqA��A�KA��A��A��A	��AxA��A��A��A�/A�FAoiAc�A�8Al"A)_A4AOAE�Ae�A�A�[AȴARTA +k@�z�@�?@��@���@�C�@���@���@�J#@�Z�@�!�@��;@�<6@�b@�o�@�0U@���@�� @�O�@���@�Z@��g@��@︻@�x@�Z@�J@�iD@�j@ꗍ@�~@�h
@�|@�;@�@�!-@�2�@䠐@�;d@�!-@��)@�9X@��@�u�@�!@��
@�>�@���@�w2@��@��@ⶮ@�z@�3�@�˒@�P@��/@��@��y@ބ�@�J@݂�@��@�@�g8@�C�@�7@ۧ�@�=@�(�@��d@���@�h�@�A�@�!@���@���@և+@�.�@Վ"@��@ԣ�@�Q@�	�@Ӳ-@�IR@��p@���@�F�@��v@Ш�@��T@�:�@��2@�~�@��@�RT@���@�6@˽�@�j�@ʅ�@���@�o�@�{�@���@ǥ@�s�@�C@��K@�j@�:�@�&�@ū�@�(�@���@�M�@��;@Õ�@�8@� \@�S@���@+@�N�@�k�@�U�@�4�@�ߤ@�e@�~�@��/@�I�@��g@��@�Ɇ@�B[@���@�X@�33@��\@��m@���@��4@�U�@��D@�,=@��j@��@�b�@��@���@��
@��n@�s@�K�@��@�	�@�W?@�>�@�#�@��@��j@���@�w�@�h
@�.�@��D@���@�zx@��<@�*�@���@��[@���@�@O@��8@���@�Ta@�Ft@�'R@�b@��j@���@��@���@�9X@�#:@��@�qv@��@��@�Xy@�=q@�)�@��@�@��@�ff@�($@�@��{@�ߤ@��$@�U2@���@�0�@�͟@�kQ@�4n@�G@���@��"@�9�@��@��e@�bN@�(�@�	�@�o @��@��@��@��@��z@�s�@�]d@�˒@�k�@�q@��@���@���@��9@��.@�A�@�O@���@�X@�J�@��@��@��6@�a|@�
�@���@���@�p�@��@���@�[�@��@�m]@�o@�ں@���@�u�@�B[@�($@���@���@��P@�\)@��@��@���@�v�@�Z�@�H@�6@�x@�ϫ@�w2@�+@��5@��@���@�H�@��@��H@�O�@��@��@���@���@��h@�=�@�$t@�@�@�;@��@���@�R�@�J@��@�u�@��@���@��,@��@�b@��[@�qv@�C�@�%@��!@�z�@�U2@�@���@�k�@�?}@�4�@�q@���@���@���@���@�?@�m]@�C�@�!�@���@��h@�g8@�I�@��@��X@�dZ@��@��2@���@��+@�Z�@�5?@�-@�$�@���@�=�@��@��@�Y@���@��1@�:*@��@���@�[W@�Q�@�F�@�)_@�ߤ@��r@��@���@�6z@�(@���@��@�1�@�g@\)@~��@~	@}�M@|�I@|x@{��@{x@{_p@{(@z��@z�h@zH�@y7L@x�e@xPH@w�r@w��@wMj@wRT@w6z@vs�@u�D@u��@u7L@tɆ@t��@t]d@s+@r�@q��@q�h@q��@qo @q7L@q%@p�@p�_@p9X@ob�@n�}@nJ@m��@mo @m\�@m+�@l�5@l6@k�[@kiD@k�@j��@j)�@i�t@im]@i&�@hc�@g�a@gH�@g�@f�X@f�A@fOv@fO@e��@d��@d�p@d�p@d�.@dV�@dH@d  @cX�@b�R@a�3@a&�@`��@`�Y@_�@_��@_��@_O@_@^��@^�@^~�@^p;@^M�@]��@\��@\w�@\bN@\:�@[�V@[�@Z5?@Y?}@X9X@X�@Wƨ@W��@W�:@W�:@WiD@W�@V��@V{@UDg@T�`@T��@Tc�@TU2@T�@Sv`@S.I@R�m@R($@Q�t@QV@P��@O�@O�
@O�@OU�@N��@Nq�@Nh
@N=q@N�@M�=@Mq@K�@K�f@K@J_�@I�@I�7@IT�@I:�@H��@Hm�@G�F@Gj�@G1�@G$t@F��@E��@E*0@D�)@D�D@Dm�@D:�@D  @C�F@B�@B��@BE�@B�@A�3@A�-@A��@A�C@A�'@A�'@A��@ArG@AX@A/@@�@?C�@?33@?1�@?+@?$t@>��@>��@>ȴ@>��@>Z�@>?@>�@=�o@=��@=��@=4@<�@<2�@< �@<  @;�a@;W?@;1�@;(@:�h@:}V@:u%@:ff@:Ta@9�@9(�@8��@8g8@8M@7'�@6�y@6Q@5�@5c@5�@4�@4w�@4bN@4I�@3�+@3��@36z@3�@2�"@2�@2l�@2e@1�o@1�h@15�@0��@0tT@04n@0	�@/�@/خ@/�
@/Mj@.�<@.d�@-�@-�"@-N<@-+�@,�9@,e�@,<�@+خ@+Y@*�@*�@*_�@*.�@*�@)�N@)Y�@(�f@(�/@(Ɇ@(�e@(�Y@(h�@(/�@'��@'
=@&��@&a|@&6�@&�@%�T@%�@%2a@$��@$�Y@$Xy@$�@#8@"��@"�@"��@"�x@"~�@"d�@"?@!�@!��@!�@!^�@!A @!-w@!;@ ֡@ �?@ ��@ bN@ x@   @��@_p@U�@Mj@ȴ@s�@J�@6�@1�@-@�@�N@��@e,@S&@Ɇ@S�@6@-�@��@x@!-@�@�b@q�@Q@�@�H@�=@u�@L�@�@��@�/@�@��@�@��@iD@dZ@1�@��@~�@a|@GE@��@��@��@c�@8�@%F@�@�P@�`@��@�4@��@l"@	�@�@@v`@X�@!-@�6@ff@M�@0U@@�Z@�9@�t@��@`B@&�@!�@q@�|@�4@?�@!@�@��@��@a@�@��@ȴ@�}@��@n�@B[@{@�n@O�@2a@@@�P@��@Ɇ@�j@�@oi@c�@7�@b@�W@�*@�4@_p@K�@;d@o@
��@
��@
�L@
��@
��@
�@
}V@
\�@
#:@
{@
u@	ԕ@	�H@	�~@	m]@	2a@��@֡@��@�@g8@I�@*�@�@	�@��@� @�0@�@_p@F�@+@�@�@�8@ߤ@��@��@p;@_�@Ov@B[@8�@($@	@��@��@�@�@�@�N@�@��@�n@��@x�@7L@(�@�@��@�@��@Ĝ@Ĝ@�e@<�@(�@�@�@�@��@�W@�
@��@�[@��@�0@��@dZ@U�@W?@S�@6z@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�D�A�U2A�W?A�WsA�[#A�\]A�]/A�\�A�\�A�]�A�^�A�^�A�aA�a�A�aA�bNA�c�A�cTA�d&A�c�A�f2A�g8A�h
A�jA�jA�i�A�iA�jA�i�A�iyA�hsA�gmA�uAƋA���A��"A�r|A�|�A�f�A���A�v�A���A�A A���A��]A�x�A��A��8A�2�A�W�A�%A~�A{�?AzS&Aw�-As�Ak�~Ah��Af��Ac��A`�6A[rGAY�rAYB[AV�@AR�AQ��AP�~AOs�AMy�AK+�AI�!AG�AB��A=.�A9�A7MA4��A3(�A2C�A0��A.�A-+A+�A+e,A*)�A)�}A)�A(��A(,�A(SA'zA&��A&ϫA'TaA)m�A)��A(�\A'�A%
=A!�hA4nA�'A�A��A��AT�A��A�$A!�A8A�aA�A=qA~A\�A�hA�*AȴA,�AkQA;dA�]A8�A�fA2�A�KAw2A�DA��A�9A0�AqA��A�KA��A��A��A	��AxA��A��A��A�/A�FAoiAc�A�8Al"A)_A4AOAE�Ae�A�A�[AȴARTA +k@�z�@�?@��@���@�C�@���@���@�J#@�Z�@�!�@��;@�<6@�b@�o�@�0U@���@�� @�O�@���@�Z@��g@��@︻@�x@�Z@�J@�iD@�j@ꗍ@�~@�h
@�|@�;@�@�!-@�2�@䠐@�;d@�!-@��)@�9X@��@�u�@�!@��
@�>�@���@�w2@��@��@ⶮ@�z@�3�@�˒@�P@��/@��@��y@ބ�@�J@݂�@��@�@�g8@�C�@�7@ۧ�@�=@�(�@��d@���@�h�@�A�@�!@���@���@և+@�.�@Վ"@��@ԣ�@�Q@�	�@Ӳ-@�IR@��p@���@�F�@��v@Ш�@��T@�:�@��2@�~�@��@�RT@���@�6@˽�@�j�@ʅ�@���@�o�@�{�@���@ǥ@�s�@�C@��K@�j@�:�@�&�@ū�@�(�@���@�M�@��;@Õ�@�8@� \@�S@���@+@�N�@�k�@�U�@�4�@�ߤ@�e@�~�@��/@�I�@��g@��@�Ɇ@�B[@���@�X@�33@��\@��m@���@��4@�U�@��D@�,=@��j@��@�b�@��@���@��
@��n@�s@�K�@��@�	�@�W?@�>�@�#�@��@��j@���@�w�@�h
@�.�@��D@���@�zx@��<@�*�@���@��[@���@�@O@��8@���@�Ta@�Ft@�'R@�b@��j@���@��@���@�9X@�#:@��@�qv@��@��@�Xy@�=q@�)�@��@�@��@�ff@�($@�@��{@�ߤ@��$@�U2@���@�0�@�͟@�kQ@�4n@�G@���@��"@�9�@��@��e@�bN@�(�@�	�@�o @��@��@��@��@��z@�s�@�]d@�˒@�k�@�q@��@���@���@��9@��.@�A�@�O@���@�X@�J�@��@��@��6@�a|@�
�@���@���@�p�@��@���@�[�@��@�m]@�o@�ں@���@�u�@�B[@�($@���@���@��P@�\)@��@��@���@�v�@�Z�@�H@�6@�x@�ϫ@�w2@�+@��5@��@���@�H�@��@��H@�O�@��@��@���@���@��h@�=�@�$t@�@�@�;@��@���@�R�@�J@��@�u�@��@���@��,@��@�b@��[@�qv@�C�@�%@��!@�z�@�U2@�@���@�k�@�?}@�4�@�q@���@���@���@���@�?@�m]@�C�@�!�@���@��h@�g8@�I�@��@��X@�dZ@��@��2@���@��+@�Z�@�5?@�-@�$�@���@�=�@��@��@�Y@���@��1@�:*@��@���@�[W@�Q�@�F�@�)_@�ߤ@��r@��@���@�6z@�(@���@��@�1�@�g@\)@~��@~	@}�M@|�I@|x@{��@{x@{_p@{(@z��@z�h@zH�@y7L@x�e@xPH@w�r@w��@wMj@wRT@w6z@vs�@u�D@u��@u7L@tɆ@t��@t]d@s+@r�@q��@q�h@q��@qo @q7L@q%@p�@p�_@p9X@ob�@n�}@nJ@m��@mo @m\�@m+�@l�5@l6@k�[@kiD@k�@j��@j)�@i�t@im]@i&�@hc�@g�a@gH�@g�@f�X@f�A@fOv@fO@e��@d��@d�p@d�p@d�.@dV�@dH@d  @cX�@b�R@a�3@a&�@`��@`�Y@_�@_��@_��@_O@_@^��@^�@^~�@^p;@^M�@]��@\��@\w�@\bN@\:�@[�V@[�@Z5?@Y?}@X9X@X�@Wƨ@W��@W�:@W�:@WiD@W�@V��@V{@UDg@T�`@T��@Tc�@TU2@T�@Sv`@S.I@R�m@R($@Q�t@QV@P��@O�@O�
@O�@OU�@N��@Nq�@Nh
@N=q@N�@M�=@Mq@K�@K�f@K@J_�@I�@I�7@IT�@I:�@H��@Hm�@G�F@Gj�@G1�@G$t@F��@E��@E*0@D�)@D�D@Dm�@D:�@D  @C�F@B�@B��@BE�@B�@A�3@A�-@A��@A�C@A�'@A�'@A��@ArG@AX@A/@@�@?C�@?33@?1�@?+@?$t@>��@>��@>ȴ@>��@>Z�@>?@>�@=�o@=��@=��@=4@<�@<2�@< �@<  @;�a@;W?@;1�@;(@:�h@:}V@:u%@:ff@:Ta@9�@9(�@8��@8g8@8M@7'�@6�y@6Q@5�@5c@5�@4�@4w�@4bN@4I�@3�+@3��@36z@3�@2�"@2�@2l�@2e@1�o@1�h@15�@0��@0tT@04n@0	�@/�@/خ@/�
@/Mj@.�<@.d�@-�@-�"@-N<@-+�@,�9@,e�@,<�@+خ@+Y@*�@*�@*_�@*.�@*�@)�N@)Y�@(�f@(�/@(Ɇ@(�e@(�Y@(h�@(/�@'��@'
=@&��@&a|@&6�@&�@%�T@%�@%2a@$��@$�Y@$Xy@$�@#8@"��@"�@"��@"�x@"~�@"d�@"?@!�@!��@!�@!^�@!A @!-w@!;@ ֡@ �?@ ��@ bN@ x@   @��@_p@U�@Mj@ȴ@s�@J�@6�@1�@-@�@�N@��@e,@S&@Ɇ@S�@6@-�@��@x@!-@�@�b@q�@Q@�@�H@�=@u�@L�@�@��@�/@�@��@�@��@iD@dZ@1�@��@~�@a|@GE@��@��@��@c�@8�@%F@�@�P@�`@��@�4@��@l"@	�@�@@v`@X�@!-@�6@ff@M�@0U@@�Z@�9@�t@��@`B@&�@!�@q@�|@�4@?�@!@�@��@��@a@�@��@ȴ@�}@��@n�@B[@{@�n@O�@2a@@@�P@��@Ɇ@�j@�@oi@c�@7�@b@�W@�*@�4@_p@K�@;d@o@
��@
��@
�L@
��@
��@
�@
}V@
\�@
#:@
{@
u@	ԕ@	�H@	�~@	m]@	2a@��@֡@��@�@g8@I�@*�@�@	�@��@� @�0@�@_p@F�@+@�@�@�8@ߤ@��@��@p;@_�@Ov@B[@8�@($@	@��@��@�@�@�@�N@�@��@�n@��@x�@7L@(�@�@��@�@��@Ĝ@Ĝ@�e@<�@(�@�@�@�@��@�W@�
@��@�[@��@�0@��@dZ@U�@W?@S�@6z@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B�,B�`B�FB�,B�,B�FB�,B�FB�FB�FB�`B�,B�FB�zB�zB�`B�zB�`B�B�B�B��B��B��B�B�fB�fB�B�B��B�B�By�B��B��B	�B	��B	�$B
0B
TB
2B
�B
B
KB
  B
�B	�B	��B	��B	��B	��B	��B	}B	z�B	o�B	RoB	H�B	CGB	<PB	2GB	/�B	&�B	$�B	&fB	&�B	&LB	&�B	&�B	,�B	0oB	/�B	&LB	'BרB��B�
B�B�dB�WB��B�HB��B��B�nB�&B�)B��B�B�B	�B	
�B	�B	�B	?B	JXB	`\B	X�B	OBB	=�B	)�B	)_B	:^B	B�B	G�B	N�B	W�B	\�B	ezB	[	B	R�B	OvB	U�B	V9B	R�B	y>B	��B	��B	�(B	��B	�B	��B	��B	��B	��B	��B	�-B	�oB	��B	�B	��B	�
B	��B	�
B	�>B	��B	��B	�yB	�=B	n�B	h�B	h>B	g�B	cB	`vB	bhB	e�B	e�B	ffB	g�B	mwB	shB	|6B	��B	�%B	��B	��B	�B	�B	�AB	��B	�-B	�;B	�SB	��B	�B	�uB	�vB	�B	��B	��B	�EB	��B	��B	�B	��B	��B	��B	��B	�'B	�vB	��B	�nB	�8B	��B	�mB	�B	��B	�B	��B	��B	��B	�aB	��B	�-B	��B	� B	��B	�TB	�+B	��B	��B	�UB	�iB	��B	��B	�jB	��B	��B	�tB	�B	�B	�LB	�B	��B	�nB	�\B	ߤB	�-B	�B	�fB	�B	��B	�B	�2B	��B	�RB	�sB	��B	�B	�kB	�B	�B	�B	��B	�B	�B	�B	�B	�QB	�B	�6B	�kB	�kB	�B	��B	�B	�B	�B	�B	�6B	�kB	�QB	��B	�B	��B	�0B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�6B	��B	�"B	��B	��B	�B	��B	��B	��B	��B	�cB	�/B	�IB	�cB	�B	�iB	�[B	�B	�B	�GB	�B	�3B	�B	�|B	�B	�B	��B	��B	��B	��B	�lB	��B	�8B	�RB	�RB	�RB	�xB	�*B	��B	��B	��B	�JB	�B	��B	�PB	��B	��B	��B	�<B	��B	��B	�.B	�HB
  B
B
;B
;B
�B
B
uB
[B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
_B
�B
�B
	B
	RB
	B
�B
�B
JB
0B
�B
�B
�B
�B
6B
B
<B
�B
pB
VB
pB
�B
�B
B
B
4B
B
}B
.B
�B
}B
�B
�B
aB
�B
�B
�B
�B
B
SB
SB
mB
�B
�B
�B
?B
�B
$B
�B
sB
�B
�B
B
�B
B
KB
B
B
B
B
QB
kB
�B
=B
WB
WB
�B
�B
B
�B
�B
�B
�B
/B
IB
~B
�B
�B
�B
�B
5B
5B
�B
�B
jB
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
;B
B
B
B
�B
�B
!B
!B
!B
!B
VB
B
;B
!B
B
!B
�B
VB
�B
 \B
 �B
 �B
 'B
 'B
 �B
!�B
!�B
"4B
"B
"NB
"�B
"�B
"�B
"�B
#�B
$@B
$@B
$tB
$�B
$�B
%zB
%`B
%�B
&fB
&�B
'B
'RB
'�B
'�B
'�B
'�B
'�B
'mB
'�B
(�B
(�B
(�B
(�B
(�B
)_B
*B
*�B
+B
+6B
+6B
+B
+6B
+kB
+QB
+�B
+B
+6B
+B
+B
+B
+�B
+�B
+�B
+kB
+�B
,�B
,qB
,�B
-)B
-)B
-CB
./B
.�B
.�B
/OB
/�B
/�B
0�B
0�B
0�B
1'B
2�B
2�B
3�B
4B
4nB
4�B
4�B
4�B
4�B
5�B
6�B
6�B
6�B
6�B
6�B
7B
7B
72B
72B
7fB
7�B
8B
8�B
8�B
8�B
8�B
9rB
9�B
:�B
:�B
:�B
;0B
;�B
;�B
<B
<B
<6B
="B
<�B
<jB
<�B
=<B
=B
<�B
<�B
=<B
=�B
=�B
>�B
?.B
?}B
@�B
@�B
A;B
A�B
B[B
B�B
B�B
B�B
CaB
CGB
CGB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
E9B
D�B
D�B
D�B
EmB
E�B
FYB
F�B
G�B
G�B
G�B
H1B
HB
G�B
HB
HKB
H�B
IRB
I�B
J#B
J�B
J�B
J�B
J�B
J�B
KB
K^B
K�B
K�B
L~B
L�B
MjB
MjB
M�B
M�B
N"B
NpB
N<B
NVB
NVB
NVB
N�B
O�B
O�B
PB
P�B
Q4B
Q4B
Q4B
QB
QhB
Q�B
RTB
R�B
R�B
R�B
S[B
T,B
TaB
T�B
T�B
T�B
T�B
U2B
UMB
VB
VB
VSB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
XEB
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
YeB
YeB
Y�B
Y�B
ZB
ZQB
Z�B
[�B
[�B
\B
\)B
\xB
\�B
\�B
\�B
]IB
]/B
]/B
]/B
]/B
]~B
^B
^5B
^OB
^OB
_;B
_;B
`'B
`\B
`�B
`�B
abB
abB
abB
a|B
a�B
a�B
bhB
bhB
bhB
b�B
b�B
cB
c B
cnB
c�B
c�B
dB
d&B
dZB
dZB
dtB
d@B
d�B
eB
ezB
e�B
e�B
fB
fB
f�B
f�B
f�B
f�B
gRB
g�B
g�B
g�B
g�B
g�B
g�B
h>B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
i_B
i�B
jB
jKB
jeB
jeB
jB
jB
k6B
kkB
k�B
k�B
k�B
lqB
l�B
l�B
l�B
mB
m)B
m)B
mCB
m�B
m�B
m�B
nB
n/B
n/B
ncB
n}B
n}B
ncB
n�B
o B
n�B
o5B
oiB
oOB
o5B
o�B
pB
p;B
p;B
p;B
p;B
p;B
poB
p�B
q'B
qB
q�B
r-B
rGB
r-B
r�B
sB
shB
s�B
s�B
s�B
s�B
tB
t9B
tTB
tnB
t�B
t�B
t�B
u%B
uB
u%B
u�B
v+B
v+B
v+B
v`B
v�B
v�B
v�B
v�B
w2B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
xlB
xlB
x�B
y	B
yrB
y�B
y�B
y�B
zDB
z^B
zxB
zxB
z�B
z�B
z�B
z�B
{B
{JB
{dB
{dB
{dB
{�B
{�B
|B
|6B
|6B
|6B
|�B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
~(B
~(B
~wB
~�B
~�B
~�B
~�B
.B
.B
.B
}B
}B
}B
�B
�B
� B
�iB
�iB
�iB
��B
��B
��B
��B
�B
� B
�B
� B
�;B
�;B
�UB
�oB
��B
��B
��B
�B
�AB
�'B
�uB
��B
��B
��B
��B
��B
�-B
�GB
�{B
��B
��B
�B
�B
�gB
��B
��B
��B
��B
��B
�B
�B
�9B
�mB
��B
��B
��B
��B
��B
��B
��B
�?B
�%B
�%B
�%B
�?B
�?B
�?B
�?B
�tB
��B
��B
��B
��B
��B
�EB
�+B
�zB
�_B
�_B
��B
�B
��B
�B
��B
�B
�1B
�B
�1B
�fB
��B
��B
�KB
��B
�B
�B
�B
�B
�RB
�	111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B�,B�`B�FB�,B�,B�FB�,B�FB�FB�FB�`B�,B�FB�zB�zB�`B�zB�`B�B�B�B��B��B��B�B�fB�fB�B�B��B�B�By�B��B��B	�B	��B	�$B
0B
TB
2B
�B
B
KB
  B
�B	�B	��B	��B	��B	��B	��B	}B	z�B	o�B	RoB	H�B	CGB	<PB	2GB	/�B	&�B	$�B	&fB	&�B	&LB	&�B	&�B	,�B	0oB	/�B	&LB	'BרB��B�
B�B�dB�WB��B�HB��B��B�nB�&B�)B��B�B�B	�B	
�B	�B	�B	?B	JXB	`\B	X�B	OBB	=�B	)�B	)_B	:^B	B�B	G�B	N�B	W�B	\�B	ezB	[	B	R�B	OvB	U�B	V9B	R�B	y>B	��B	��B	�(B	��B	�B	��B	��B	��B	��B	��B	�-B	�oB	��B	�B	��B	�
B	��B	�
B	�>B	��B	��B	�yB	�=B	n�B	h�B	h>B	g�B	cB	`vB	bhB	e�B	e�B	ffB	g�B	mwB	shB	|6B	��B	�%B	��B	��B	�B	�B	�AB	��B	�-B	�;B	�SB	��B	�B	�uB	�vB	�B	��B	��B	�EB	��B	��B	�B	��B	��B	��B	��B	�'B	�vB	��B	�nB	�8B	��B	�mB	�B	��B	�B	��B	��B	��B	�aB	��B	�-B	��B	� B	��B	�TB	�+B	��B	��B	�UB	�iB	��B	��B	�jB	��B	��B	�tB	�B	�B	�LB	�B	��B	�nB	�\B	ߤB	�-B	�B	�fB	�B	��B	�B	�2B	��B	�RB	�sB	��B	�B	�kB	�B	�B	�B	��B	�B	�B	�B	�B	�QB	�B	�6B	�kB	�kB	�B	��B	�B	�B	�B	�B	�6B	�kB	�QB	��B	�B	��B	�0B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�6B	��B	�"B	��B	��B	�B	��B	��B	��B	��B	�cB	�/B	�IB	�cB	�B	�iB	�[B	�B	�B	�GB	�B	�3B	�B	�|B	�B	�B	��B	��B	��B	��B	�lB	��B	�8B	�RB	�RB	�RB	�xB	�*B	��B	��B	��B	�JB	�B	��B	�PB	��B	��B	��B	�<B	��B	��B	�.B	�HB
  B
B
;B
;B
�B
B
uB
[B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
_B
�B
�B
	B
	RB
	B
�B
�B
JB
0B
�B
�B
�B
�B
6B
B
<B
�B
pB
VB
pB
�B
�B
B
B
4B
B
}B
.B
�B
}B
�B
�B
aB
�B
�B
�B
�B
B
SB
SB
mB
�B
�B
�B
?B
�B
$B
�B
sB
�B
�B
B
�B
B
KB
B
B
B
B
QB
kB
�B
=B
WB
WB
�B
�B
B
�B
�B
�B
�B
/B
IB
~B
�B
�B
�B
�B
5B
5B
�B
�B
jB
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
;B
B
B
B
�B
�B
!B
!B
!B
!B
VB
B
;B
!B
B
!B
�B
VB
�B
 \B
 �B
 �B
 'B
 'B
 �B
!�B
!�B
"4B
"B
"NB
"�B
"�B
"�B
"�B
#�B
$@B
$@B
$tB
$�B
$�B
%zB
%`B
%�B
&fB
&�B
'B
'RB
'�B
'�B
'�B
'�B
'�B
'mB
'�B
(�B
(�B
(�B
(�B
(�B
)_B
*B
*�B
+B
+6B
+6B
+B
+6B
+kB
+QB
+�B
+B
+6B
+B
+B
+B
+�B
+�B
+�B
+kB
+�B
,�B
,qB
,�B
-)B
-)B
-CB
./B
.�B
.�B
/OB
/�B
/�B
0�B
0�B
0�B
1'B
2�B
2�B
3�B
4B
4nB
4�B
4�B
4�B
4�B
5�B
6�B
6�B
6�B
6�B
6�B
7B
7B
72B
72B
7fB
7�B
8B
8�B
8�B
8�B
8�B
9rB
9�B
:�B
:�B
:�B
;0B
;�B
;�B
<B
<B
<6B
="B
<�B
<jB
<�B
=<B
=B
<�B
<�B
=<B
=�B
=�B
>�B
?.B
?}B
@�B
@�B
A;B
A�B
B[B
B�B
B�B
B�B
CaB
CGB
CGB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
E9B
D�B
D�B
D�B
EmB
E�B
FYB
F�B
G�B
G�B
G�B
H1B
HB
G�B
HB
HKB
H�B
IRB
I�B
J#B
J�B
J�B
J�B
J�B
J�B
KB
K^B
K�B
K�B
L~B
L�B
MjB
MjB
M�B
M�B
N"B
NpB
N<B
NVB
NVB
NVB
N�B
O�B
O�B
PB
P�B
Q4B
Q4B
Q4B
QB
QhB
Q�B
RTB
R�B
R�B
R�B
S[B
T,B
TaB
T�B
T�B
T�B
T�B
U2B
UMB
VB
VB
VSB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
XEB
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
YeB
YeB
Y�B
Y�B
ZB
ZQB
Z�B
[�B
[�B
\B
\)B
\xB
\�B
\�B
\�B
]IB
]/B
]/B
]/B
]/B
]~B
^B
^5B
^OB
^OB
_;B
_;B
`'B
`\B
`�B
`�B
abB
abB
abB
a|B
a�B
a�B
bhB
bhB
bhB
b�B
b�B
cB
c B
cnB
c�B
c�B
dB
d&B
dZB
dZB
dtB
d@B
d�B
eB
ezB
e�B
e�B
fB
fB
f�B
f�B
f�B
f�B
gRB
g�B
g�B
g�B
g�B
g�B
g�B
h>B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
i_B
i�B
jB
jKB
jeB
jeB
jB
jB
k6B
kkB
k�B
k�B
k�B
lqB
l�B
l�B
l�B
mB
m)B
m)B
mCB
m�B
m�B
m�B
nB
n/B
n/B
ncB
n}B
n}B
ncB
n�B
o B
n�B
o5B
oiB
oOB
o5B
o�B
pB
p;B
p;B
p;B
p;B
p;B
poB
p�B
q'B
qB
q�B
r-B
rGB
r-B
r�B
sB
shB
s�B
s�B
s�B
s�B
tB
t9B
tTB
tnB
t�B
t�B
t�B
u%B
uB
u%B
u�B
v+B
v+B
v+B
v`B
v�B
v�B
v�B
v�B
w2B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
xlB
xlB
x�B
y	B
yrB
y�B
y�B
y�B
zDB
z^B
zxB
zxB
z�B
z�B
z�B
z�B
{B
{JB
{dB
{dB
{dB
{�B
{�B
|B
|6B
|6B
|6B
|�B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
~(B
~(B
~wB
~�B
~�B
~�B
~�B
.B
.B
.B
}B
}B
}B
�B
�B
� B
�iB
�iB
�iB
��B
��B
��B
��B
�B
� B
�B
� B
�;B
�;B
�UB
�oB
��B
��B
��B
�B
�AB
�'B
�uB
��B
��B
��B
��B
��B
�-B
�GB
�{B
��B
��B
�B
�B
�gB
��B
��B
��B
��B
��B
�B
�B
�9B
�mB
��B
��B
��B
��B
��B
��B
��B
�?B
�%B
�%B
�%B
�?B
�?B
�?B
�?B
�tB
��B
��B
��B
��B
��B
�EB
�+B
�zB
�_B
�_B
��B
�B
��B
�B
��B
�B
�1B
�B
�1B
�fB
��B
��B
�KB
��B
�B
�B
�B
�B
�RB
�	111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105242  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192302  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192303  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192303                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042310  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042310  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                