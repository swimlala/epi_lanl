CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:47:02Z creation;2022-06-04T17:47:02Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174702  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��"0��B1   @��"��l�@-"��`B�c�XbM�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8ffB?33BG��BO��BX  B`  Bh  Bo��Bx  B�  B�33B�  B�  B�ffB���B�  B�  B�  B���B�  B�  B�  B���B���B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�ffB�ffB뙚B�  B�33B���B���C   C  C  C  C�fC
  C�C�C�fC  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C633C8  C:  C;�fC=�fC?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CY�fC\  C^  C_�fCb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DF��DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� DyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ DǼ�D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�3D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @\)@��H@��HAp�A!p�AAp�Aap�A��RA��RA��RA��RA��RAиRA�RA�RB \)B\)B\)B\)B \)B(B0\)B8B?�\BG��BO��BX\)B`\)Bh\)Bo��Bx\)B�.B�aGB�.B�.B��zB���B�.B�.B�.B���B�.B�.B�.B���B�ǮB�.B�aGB���B���B�.B�.B�.B�.B�.B�.B�zB�zB�ǮB�.B�aGB�ǮB���C 
C
C
C
C�pC

C0�C0�C�pC
C�pC
C
C
C
C
C 
C"
C$
C&
C(
C*
C,
C.
C0
C2
C4
C6J=C8
C:
C;�pC=�pC?�pCB
CD
CF
CH
CJ
CL
CN
CP
CR
CS�pCV
CX
CY�pC\
C^
C_�pCb
Cd
Cf
Ch0�Cj
Cl
Cn
Cp
Cr
Ct
Cv
Cx
Cz
C|
C~
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D]D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF]DF�]DG��DH�DH��DI)DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy)Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�FD���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D���D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D��D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D�D���D��D�B�DÂ�D���D��D�B�DĂ�D���D��D�B�Dł�D���D��D�B�DƂ�D���D��D�B�Dǂ�Dǿ�D��D�B�DȂ�D���D��D�B�Dɂ�D���D��D�B�Dʂ�D���D��D�B�D˂�D���D��D�B�D̂�D���D��D�B�D͂�D���D��D�B�D΂�D���D��D�B�Dς�D���D��D�B�DЂ�D���D��D�B�Dт�D���D��D�B�D҂�D���D��D�B�Dӂ�D���D��D�B�DԂ�D���D��D�B�DՂ�D���D��D�B�Dւ�D���D��D�B�Dׂ�D���D��D�B�D؂�D���D��D�B�Dق�D���D��D�B�Dڂ�D���D�D�B�Dۂ�D���D��D�B�D܂�D���D��D�B�D݂�D���D��D�B�Dނ�D���D��D�B�D߂�D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D��D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D�r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��(A�SA��A�SA�	7A�1A��A�(A��A��A�:A�A�$A�A�A��A��]AܼA�r|A���A�n�Aٛ�A�o5A�+A���A�+A�YA���A��A�uZA��AР'A�>wA��AξBA;�Aɦ�A�r�A�U�A�AA�J�A��TAż6A�xA��A�e�A�+�A��WA��}A� �A���A��OA��A��lA���A��zA��A��A��.A���A���A��/A� 'A�J�A��xA��A��A�E9A��FA���A�tTA��{A�PA���A���A��ZA�-wA��A���A��A��5A���A�pA��A��fA��A�$@A�S[A��A�f2A���A�R A~oAx�AqJ#Aj��Ae�Ac~�A`8A\cA[��AZخAW�AOH�AJ�`AJP�AI"�AGX�AF�*AE�"AE:*AD��AC��AC;�AB�6AA�NAA8�A@0�A=�A<A::*A81�A6��A5\�A4r�A3�A3xA2�MA2X�A1��A/|�A-�A+�.A*��A)�A'��A&}�A%M�A$�EA$HA#��A#�HA$�A#I�A"L0A"7�A"
�A!��A!K^A ��A2�A�AF�AĜA��A��A��AXAxlAQ�A`BA�>A�+A�9A�A($A�AK�AqA��A�hA�}Ar�AA�A#:A��A�cAĜA�Ao�A%�A��A�<A��A5?A�A�@A)_A�.A҉A��Ac�A
��A
9XA
+kA	��A	�qA��AC-AS�AoA��A\�A�"A�AA�AA�|AVAxA�&A��A�yA�gA'�A�A*0Au�A�jA��A��A�A�A<6A�A;dA �zA VmA @A J@���A �@��@��@�tT@���@���@�u�@���@��U@�C-@��@���@��H@��@�Z�@��<@�	�@�C�@�a|@��X@�`B@��@�}V@�\@�S@�I@�~@�6@�q@��@�w2@�+@���@�{@줩@�^@�l�@��@���@��@�7L@���@�j@�!@���@�^�@��@俱@�kQ@��@�t�@��@��]@�kQ@ᴢ@�33@��u@�ݘ@��@��@ݲ�@�w2@�E9@܇�@��N@�p�@��'@�C-@٬q@�j�@�
=@ؒ�@�?�@�	�@�	@׸�@׎"@�,�@�D�@��@�RT@�c�@���@�1�@��@Қ�@�V�@��@�(@��+@�=�@Ό@�@�@��@͇�@��@��@ʾ�@��;@���@ɝ�@�>�@ȦL@ȆY@���@��@��@��[@ȔF@�@ǅ�@��@�[�@�(�@Ū�@�Ov@���@�K�@��?@�B[@��@��H@�J�@���@�1�@���@���@���@�:*@��@��z@���@��2@���@�:�@��F@�ѷ@�s�@�-�@���@��@���@�w�@�%�@�o @��@��$@�<�@��@��}@�{J@�!-@���@�u@���@���@���@�8�@���@�Ta@�&�@��@�:�@��,@��+@��Z@�خ@�m]@��,@���@�i�@��@�qv@�8@�'�@�#�@��@��h@�c�@�)�@��@��@�E9@���@�w�@�S�@��@���@�w2@�>�@��H@��L@�1'@�s@�E9@���@��@�l�@�;�@���@���@�m]@�@��h@�H�@��W@���@�a@���@���@��@���@�d�@�1�@���@���@��M@�G�@��5@��@�kQ@�J@���@�֡@�C-@�ݘ@���@�Y�@�Y@���@�U2@�{@��o@���@�S@���@���@�bN@�:*@�-@��@��@�c@�[W@�,�@��]@�xl@��@��}@��f@�5�@��1@�9X@��@��#@�N<@��	@�<�@��@�$t@��E@���@��4@�@�@��@��@�5�@��@���@��+@�B[@�@���@���@�u�@�F�@�%@�ѷ@���@��L@�ff@��o@���@��P@�J#@��@���@�YK@�0U@�	@���@���@�w2@�
=@��6@�4@��@��@@��S@��:@���@��f@��	@��{@�j�@��@���@�u%@�[�@�*�@��@��&@��N@��q@��@�Dg@��@��$@��@�m�@�-@���@�ƨ@���@�k�@�'�@���@��A@�a|@�_@��@��6@���@���@�s�@�\)@��@��}@�YK@�	@���@�~�@�dZ@�B�@�q@��@���@�}V@�Ft@�{@�G@�W@�a@{J@W?@
=@~��@~+k@}�@}!�@|b@{��@{Z�@z��@z_@y�~@y4@x�@x��@xw�@x/�@w�$@w@O@v�h@v\�@v#:@u�@u5�@t��@t'R@s�$@ss@sC@r��@rL0@q�M@q%F@p�$@p|�@o�;@o�@nz@nu%@n^5@m�.@m��@m!�@lu�@l6@lG@kqv@k>�@k&@j�,@j�A@j4@i�@i�j@i��@iq@h�@hj@g��@g�@f�}@f� @fc @f
�@e��@ezx@e(�@d��@d��@dS�@c��@c9�@b��@b{�@bi�@b6�@b4@a��@a�~@aX@a \@`�5@`��@`�D@`�@`|�@`oi@_��@_)_@^�@^�L@^($@]�)@]�S@]S&@]8�@]�@\ی@\@[��@[/�@Zߤ@Z��@ZOv@Yc�@Y@@X��@Xz�@X%�@W�V@V�c@Vs�@U�#@Uw2@UL�@U�@Tѷ@Tr�@T]d@T7�@S�6@SH�@SC@R�@R��@RB[@Q�@Q�@Pu�@O�@O�V@O$t@N�m@N-@M�S@M/@L�v@L��@L7�@K��@Kt�@K�@J��@J��@J-@I�C@IV@H��@H�@Hc�@H[�@H<�@G��@GRT@G@FOv@E`B@D��@De�@D'R@C�@C�@C&@B�,@B��@Bc @B?@B�@Aa�@@��@@PH@@@?��@?�{@?�@>�6@>�b@>v�@>L0@>
�@=�-@=7L@<��@<'R@;�
@;�	@;v`@;C�@;/�@:��@:	@9��@9�@9+�@9V@8֡@8m�@8�@7�+@7��@7��@7]�@7E9@76z@6�c@6��@6R�@5�@54@5�@4�	@4�[@4�z@4�.@4y>@4V�@3��@3X�@2ߤ@2Ta@2u@1��@1��@1��@1^�@0֡@0��@0�@0�o@0Z@0~@/�@/�P@/+@.�b@.�@-�N@-�X@-k�@-*0@,�@,��@,"h@+�Q@+��@+t�@*�y@*ff@)�@)��@(�)@(]d@(�@(b@'�@'_p@&�X@&�A@&C�@&�@%�j@%�7@%^�@%q@$֡@$_@$@#��@#e�@#Mj@#.I@"�@"{�@"#:@!��@!X@!@@ �K@ N�@ƨ@�$@��@Z�@�B@�6@�F@=q@��@��@j@N<@=�@�@��@��@�z@w�@PH@-�@�m@�@��@�:@�@��@��@��@;�@��@�@�@�@��@��@��@|@G�@=�@!�@�@��@~(@q@	�@�W@�K@��@j�@>�@/�@�@�X@��@GE@=q@.�@�z@��@��@f�@/@�@�@�@S�@"h@�A@�@�@��@S�@�@�@S@��@�'@�\@l�@0U@$�@O@O@O@�@�Z@��@j@IR@%F@�|@��@�@��@]d@9X@1@��@��@j�@+@��@�B@�\@d�@Z�@Ta@J�@�@�>@�@�S@e,@B�@!�@V@��@��@�/@�@�@��@��@��@�@z�@c�@K^@�@�m@�K@�@{J@@O@�@
�]@
�}@
��@
H�@
0U@
e@	�D1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��(A�SA��A�SA�	7A�1A��A�(A��A��A�:A�A�$A�A�A��A��]AܼA�r|A���A�n�Aٛ�A�o5A�+A���A�+A�YA���A��A�uZA��AР'A�>wA��AξBA;�Aɦ�A�r�A�U�A�AA�J�A��TAż6A�xA��A�e�A�+�A��WA��}A� �A���A��OA��A��lA���A��zA��A��A��.A���A���A��/A� 'A�J�A��xA��A��A�E9A��FA���A�tTA��{A�PA���A���A��ZA�-wA��A���A��A��5A���A�pA��A��fA��A�$@A�S[A��A�f2A���A�R A~oAx�AqJ#Aj��Ae�Ac~�A`8A\cA[��AZخAW�AOH�AJ�`AJP�AI"�AGX�AF�*AE�"AE:*AD��AC��AC;�AB�6AA�NAA8�A@0�A=�A<A::*A81�A6��A5\�A4r�A3�A3xA2�MA2X�A1��A/|�A-�A+�.A*��A)�A'��A&}�A%M�A$�EA$HA#��A#�HA$�A#I�A"L0A"7�A"
�A!��A!K^A ��A2�A�AF�AĜA��A��A��AXAxlAQ�A`BA�>A�+A�9A�A($A�AK�AqA��A�hA�}Ar�AA�A#:A��A�cAĜA�Ao�A%�A��A�<A��A5?A�A�@A)_A�.A҉A��Ac�A
��A
9XA
+kA	��A	�qA��AC-AS�AoA��A\�A�"A�AA�AA�|AVAxA�&A��A�yA�gA'�A�A*0Au�A�jA��A��A�A�A<6A�A;dA �zA VmA @A J@���A �@��@��@�tT@���@���@�u�@���@��U@�C-@��@���@��H@��@�Z�@��<@�	�@�C�@�a|@��X@�`B@��@�}V@�\@�S@�I@�~@�6@�q@��@�w2@�+@���@�{@줩@�^@�l�@��@���@��@�7L@���@�j@�!@���@�^�@��@俱@�kQ@��@�t�@��@��]@�kQ@ᴢ@�33@��u@�ݘ@��@��@ݲ�@�w2@�E9@܇�@��N@�p�@��'@�C-@٬q@�j�@�
=@ؒ�@�?�@�	�@�	@׸�@׎"@�,�@�D�@��@�RT@�c�@���@�1�@��@Қ�@�V�@��@�(@��+@�=�@Ό@�@�@��@͇�@��@��@ʾ�@��;@���@ɝ�@�>�@ȦL@ȆY@���@��@��@��[@ȔF@�@ǅ�@��@�[�@�(�@Ū�@�Ov@���@�K�@��?@�B[@��@��H@�J�@���@�1�@���@���@���@�:*@��@��z@���@��2@���@�:�@��F@�ѷ@�s�@�-�@���@��@���@�w�@�%�@�o @��@��$@�<�@��@��}@�{J@�!-@���@�u@���@���@���@�8�@���@�Ta@�&�@��@�:�@��,@��+@��Z@�خ@�m]@��,@���@�i�@��@�qv@�8@�'�@�#�@��@��h@�c�@�)�@��@��@�E9@���@�w�@�S�@��@���@�w2@�>�@��H@��L@�1'@�s@�E9@���@��@�l�@�;�@���@���@�m]@�@��h@�H�@��W@���@�a@���@���@��@���@�d�@�1�@���@���@��M@�G�@��5@��@�kQ@�J@���@�֡@�C-@�ݘ@���@�Y�@�Y@���@�U2@�{@��o@���@�S@���@���@�bN@�:*@�-@��@��@�c@�[W@�,�@��]@�xl@��@��}@��f@�5�@��1@�9X@��@��#@�N<@��	@�<�@��@�$t@��E@���@��4@�@�@��@��@�5�@��@���@��+@�B[@�@���@���@�u�@�F�@�%@�ѷ@���@��L@�ff@��o@���@��P@�J#@��@���@�YK@�0U@�	@���@���@�w2@�
=@��6@�4@��@��@@��S@��:@���@��f@��	@��{@�j�@��@���@�u%@�[�@�*�@��@��&@��N@��q@��@�Dg@��@��$@��@�m�@�-@���@�ƨ@���@�k�@�'�@���@��A@�a|@�_@��@��6@���@���@�s�@�\)@��@��}@�YK@�	@���@�~�@�dZ@�B�@�q@��@���@�}V@�Ft@�{@�G@�W@�a@{J@W?@
=@~��@~+k@}�@}!�@|b@{��@{Z�@z��@z_@y�~@y4@x�@x��@xw�@x/�@w�$@w@O@v�h@v\�@v#:@u�@u5�@t��@t'R@s�$@ss@sC@r��@rL0@q�M@q%F@p�$@p|�@o�;@o�@nz@nu%@n^5@m�.@m��@m!�@lu�@l6@lG@kqv@k>�@k&@j�,@j�A@j4@i�@i�j@i��@iq@h�@hj@g��@g�@f�}@f� @fc @f
�@e��@ezx@e(�@d��@d��@dS�@c��@c9�@b��@b{�@bi�@b6�@b4@a��@a�~@aX@a \@`�5@`��@`�D@`�@`|�@`oi@_��@_)_@^�@^�L@^($@]�)@]�S@]S&@]8�@]�@\ی@\@[��@[/�@Zߤ@Z��@ZOv@Yc�@Y@@X��@Xz�@X%�@W�V@V�c@Vs�@U�#@Uw2@UL�@U�@Tѷ@Tr�@T]d@T7�@S�6@SH�@SC@R�@R��@RB[@Q�@Q�@Pu�@O�@O�V@O$t@N�m@N-@M�S@M/@L�v@L��@L7�@K��@Kt�@K�@J��@J��@J-@I�C@IV@H��@H�@Hc�@H[�@H<�@G��@GRT@G@FOv@E`B@D��@De�@D'R@C�@C�@C&@B�,@B��@Bc @B?@B�@Aa�@@��@@PH@@@?��@?�{@?�@>�6@>�b@>v�@>L0@>
�@=�-@=7L@<��@<'R@;�
@;�	@;v`@;C�@;/�@:��@:	@9��@9�@9+�@9V@8֡@8m�@8�@7�+@7��@7��@7]�@7E9@76z@6�c@6��@6R�@5�@54@5�@4�	@4�[@4�z@4�.@4y>@4V�@3��@3X�@2ߤ@2Ta@2u@1��@1��@1��@1^�@0֡@0��@0�@0�o@0Z@0~@/�@/�P@/+@.�b@.�@-�N@-�X@-k�@-*0@,�@,��@,"h@+�Q@+��@+t�@*�y@*ff@)�@)��@(�)@(]d@(�@(b@'�@'_p@&�X@&�A@&C�@&�@%�j@%�7@%^�@%q@$֡@$_@$@#��@#e�@#Mj@#.I@"�@"{�@"#:@!��@!X@!@@ �K@ N�@ƨ@�$@��@Z�@�B@�6@�F@=q@��@��@j@N<@=�@�@��@��@�z@w�@PH@-�@�m@�@��@�:@�@��@��@��@;�@��@�@�@�@��@��@��@|@G�@=�@!�@�@��@~(@q@	�@�W@�K@��@j�@>�@/�@�@�X@��@GE@=q@.�@�z@��@��@f�@/@�@�@�@S�@"h@�A@�@�@��@S�@�@�@S@��@�'@�\@l�@0U@$�@O@O@O@�@�Z@��@j@IR@%F@�|@��@�@��@]d@9X@1@��@��@j�@+@��@�B@�\@d�@Z�@Ta@J�@�@�>@�@�S@e,@B�@!�@V@��@��@�/@�@�@��@��@��@�@z�@c�@K^@�@�m@�K@�@{J@@O@�@
�]@
�}@
��@
H�@
0U@
e@	�D1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	~�B	~�B	~�B	~�B	~wB	~�B	~�B	~wB	~wB	~�B	~�B	~�B	~wB	~�B	~�B	~�B	}B	�4B	��B	x�B	Y�B	N�B	W�B	Z�B	RoB	YB	��B	�mB	��B	�CB	��B	��B	��B	�B	�KB	��B	�pB	��B	�ZB	��B
 �B
�B
K�B
JXB
�B
��B
�B
�=B
�IB
ܒB)B)�B2�B@iBR�BZ�B_�Bi�Bl�BuB�B��B��B��B�B�`B�B��B�B� B�?B�PB��B�tBl�BKxBA�B/�B�B
��B
�B
�*B
�mB
�4B
dZB
L0B
>�B
0�B
'RB
 B
B
tB	�`B	�B	�fB	�B	i�B	\�B	N�B	;�B	4�B	/OB	B	
�B�B�B�MB�nB�.B	
rB	
XB	BB	�B	�B	!HB	'�B	9XB	>�B	D3B	EB	E�B	J�B	P.B	VB	^B	kB	|�B	�=B	��B	��B	�7B	�B	�pB	�/B	��B	�SB	��B	��B	�B	�7B	�B	�8B	�AB	�4B	��B	�hB	ЗB	�(B	�B	�(B	ǮB	�uB	��B	�B	�B	ɠB	�fB	�<B	��B	�`B	��B	�`B	�B	��B	��B	��B	�jB	��B	��B	�?B	ȚB	ˬB	�B	��B	��B	�gB	�SB	ϫB	ٴB	ٚB	�{B	��B	��B	ҽB	��B	ϑB	�"B	ΥB	�hB	��B	�7B	��B	��B	��B	خB	�~B	�OB	یB	��B	�CB	�IB	�5B	��B	�B	�`B	�QB	�qB	�vB	�DB	�B
  B
B	��B	�AB	��B	��B	�nB	�B
�B
�B
%B
 4B	�BB	�DB	�2B	�B	�-B	��B	��B	��B	�ZB	�^B	�VB	�>B	��B	�zB	�fB	��B	�$B	�B	�^B	�*B	�0B	��B	�	B	�fB	�tB	�B	��B	�B	��B	��B	�B	��B	�B	�B	�B	��B	�qB	�wB	��B	�B	�;B	��B	�B	�!B	�!B	�B	��B	�B	�+B	�TB	��B	�B	�B	��B	�-B	�B	�QB	�B	�6B	�0B	�*B	�WB	�B	��B	�wB	��B	�B	��B	�B	��B	�eB	�B	�eB	�B	�yB	�sB	��B	�B	�XB	�>B	�*B	�B	�B	��B	�yB	�0B	��B	�B	��B	�B	��B	�B	��B	�wB	�CB	��B	��B	�B	�B	�*B	�B	�B	��B	�0B	��B	�$B	��B	�zB	�_B	�B	�B	�fB	�DB	�B	��B	�B	�OB	��B	�}B	�5B	�B	�]B	��B	�CB	�kB	�eB	��B	�B	�B	�)B	�)B	��B	�"B	��B	��B	�B	�B	�B	�iB	�B	�UB	��B	�AB	�B	�B	�B	�!B	�B	�B	�oB	�UB	�B	�B	�B	��B	� B	�B	��B	��B	�iB	�B	��B	�'B	�AB	�AB	�'B	�vB	�aB	�-B	��B	�B	��B	��B	�B	�%B	�?B	�FB	��B	��B	��B	�8B	�lB	�8B	�B	��B	��B	�RB	�lB	��B	�DB	��B	��B	�B	��B	��B	�JB	�B	��B	�B	��B	��B	��B	�wB	�.B	��B
 B
 iB
 iB
 4B
 B
 iB
 �B
 B
 iB
 �B
oB
oB
B
aB
3B
�B
�B
B
mB
�B
�B
�B
B
B
�B
�B
B
	B
�B
	�B

=B

=B

rB
B
B
B

�B
)B
�B
�B
�B
JB
dB
dB
~B
B
jB
�B
�B
VB
�B
�B
�B
 B
 B
4B
B
�B
uB
�B
@B
:B
�B
�B
�B
uB
�B
&B
�B
&B
[B
�B
�B
B
�B
B
�B
?B
�B
�B
EB
KB
�B
KB
�B
�B
#B
WB
�B
)B
~B
OB
�B
�B
�B
�B
�B
5B
B
B
VB
�B
�B
 B
 vB
!�B
#B
#�B
$@B
$ZB
#�B
#�B
$�B
%B
%,B
%,B
%B
%,B
%,B
%FB
%�B
%�B
%�B
&�B
'�B
(>B
(�B
(�B
(�B
)_B
)�B
)�B
*B
*�B
*�B
+B
+B
+QB
+6B
+6B
+6B
+6B
+kB
+�B
,�B
,�B
,�B
-B
-B
-�B
-�B
.IB
.�B
.�B
.�B
.�B
.�B
/ B
/ B
/OB
/�B
0oB
0oB
0�B
1vB
1�B
1�B
2�B
3B
3MB
33B
3�B
3�B
3�B
3�B
4�B
4�B
5tB
5�B
6B
6zB
6�B
72B
7�B
7�B
7�B
7�B
88B
8�B
8�B
9	B
9�B
9�B
:*B
:�B
;B
;B
:�B
;dB
;dB
;�B
;�B
;�B
;�B
<jB
<PB
<PB
<�B
<�B
<�B
<�B
<�B
<�B
=B
=<B
=qB
>BB
>]B
>BB
>BB
>BB
?B
?HB
?}B
@�B
@�B
@�B
A B
A�B
A�B
A�B
A�B
A�B
BAB
B[B
B�B
B�B
B�B
B�B
C-B
CGB
CaB
CaB
CaB
C-B
C�B
DMB
DgB
D�B
EmB
EmB
E�B
E�B
E�B
E�B
E�B
FYB
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G+B
GzB
HKB
H�B
I7B
IRB
IlB
I�B
I�B
J	B
J	B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
JXB
J�B
KxB
KxB
KxB
K�B
LJB
L~B
L�B
L�B
L�B
MB
M�B
NB
N<B
N�B
O�B
PB
PB
PB
O�B
O�B
O�B
O�B
PB
PB
PbB
Q4B
QNB
Q�B
RB
R B
RTB
R�B
R�B
R�B
R�B
R�B
R�B
SuB
S�B
TB
T,B
T,B
T�B
T�B
UgB
UgB
U�B
U�B
V9B
V�B
V�B
W$B
WsB
W�B
W�B
W�B
W�B
W�B
XyB
X�B
YB
Y1B
YKB
YKB
YB
Y�B
Y�B
ZB
ZkB
ZkB
Z�B
Z�B
Z�B
Z�B
[#B
[WB
[�B
\)B
\CB
\CB
\]B
\�B
\�B
\�B
\xB
]B
]�B
]�B
^jB
^�B
^�B
^�B
_B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`'B
`\B
`�B
aHB
abB
a|B
a�B
a�B
b4B
b�B
b�B
c B
c B
cTB
c�B
d&B
dZB
d�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
gB
gB
gB
gB
gB
gB
gRB
gRB
g�B
g�B
h>B
hXB
hXB
hXB
h�B
h�B
iDB
i�B
i�B
i�B
i�B
j�B
kB
k6B
k6B
k6B
k�B
k�B
lB
mB
mCB
m]B
mwB
mwB
m�B
m�B
m�B
nB
n/B
ncB
n}B
n�B
n�B
n�B
n�B
n�B
o5B
oiB
oiB
oOB
o�B
poB
p�B
qB
q'B
q'B
qAB
q'B
qvB
q�B
q�B
q�B
r-B
r�B
r�B
r�B
s3B
sMB
shB
s�B
s�B
s�B
s�B
s�B
tB
tB
t�B
t�B
t�B
u%B
u?B
uZB
uZB
u�B
u�B
u�B
vB
vFB
v`B
vzB
vzB
v�B
v�B
w2B
wfB
wfB
wfB
w�B
w�B
w�B
xB
xRB
x8B
xRB
x8B
x8B
xRB
xRB
x�B
y	B
y$B
yXB
y�B
y�B
y�B
y�B
z*B
z^B
z�B
z�B
z�B
{0B
{B
{�B
{�B
|6B
|PB
|PB
|jB
|PB
|�B
|�B
|�B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~B
~B
~B
~B
~(B
~(B
~wB
~wB
~�B
~�B
~�B
B
cB
cB
}B
�B
�B
�OB
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	~�B	~�B	~�B	~�B	~wB	~�B	~�B	~wB	~wB	~�B	~�B	~�B	~wB	~�B	~�B	~�B	}B	�4B	��B	x�B	Y�B	N�B	W�B	Z�B	RoB	YB	��B	�mB	��B	�CB	��B	��B	��B	�B	�KB	��B	�pB	��B	�ZB	��B
 �B
�B
K�B
JXB
�B
��B
�B
�=B
�IB
ܒB)B)�B2�B@iBR�BZ�B_�Bi�Bl�BuB�B��B��B��B�B�`B�B��B�B� B�?B�PB��B�tBl�BKxBA�B/�B�B
��B
�B
�*B
�mB
�4B
dZB
L0B
>�B
0�B
'RB
 B
B
tB	�`B	�B	�fB	�B	i�B	\�B	N�B	;�B	4�B	/OB	B	
�B�B�B�MB�nB�.B	
rB	
XB	BB	�B	�B	!HB	'�B	9XB	>�B	D3B	EB	E�B	J�B	P.B	VB	^B	kB	|�B	�=B	��B	��B	�7B	�B	�pB	�/B	��B	�SB	��B	��B	�B	�7B	�B	�8B	�AB	�4B	��B	�hB	ЗB	�(B	�B	�(B	ǮB	�uB	��B	�B	�B	ɠB	�fB	�<B	��B	�`B	��B	�`B	�B	��B	��B	��B	�jB	��B	��B	�?B	ȚB	ˬB	�B	��B	��B	�gB	�SB	ϫB	ٴB	ٚB	�{B	��B	��B	ҽB	��B	ϑB	�"B	ΥB	�hB	��B	�7B	��B	��B	��B	خB	�~B	�OB	یB	��B	�CB	�IB	�5B	��B	�B	�`B	�QB	�qB	�vB	�DB	�B
  B
B	��B	�AB	��B	��B	�nB	�B
�B
�B
%B
 4B	�BB	�DB	�2B	�B	�-B	��B	��B	��B	�ZB	�^B	�VB	�>B	��B	�zB	�fB	��B	�$B	�B	�^B	�*B	�0B	��B	�	B	�fB	�tB	�B	��B	�B	��B	��B	�B	��B	�B	�B	�B	��B	�qB	�wB	��B	�B	�;B	��B	�B	�!B	�!B	�B	��B	�B	�+B	�TB	��B	�B	�B	��B	�-B	�B	�QB	�B	�6B	�0B	�*B	�WB	�B	��B	�wB	��B	�B	��B	�B	��B	�eB	�B	�eB	�B	�yB	�sB	��B	�B	�XB	�>B	�*B	�B	�B	��B	�yB	�0B	��B	�B	��B	�B	��B	�B	��B	�wB	�CB	��B	��B	�B	�B	�*B	�B	�B	��B	�0B	��B	�$B	��B	�zB	�_B	�B	�B	�fB	�DB	�B	��B	�B	�OB	��B	�}B	�5B	�B	�]B	��B	�CB	�kB	�eB	��B	�B	�B	�)B	�)B	��B	�"B	��B	��B	�B	�B	�B	�iB	�B	�UB	��B	�AB	�B	�B	�B	�!B	�B	�B	�oB	�UB	�B	�B	�B	��B	� B	�B	��B	��B	�iB	�B	��B	�'B	�AB	�AB	�'B	�vB	�aB	�-B	��B	�B	��B	��B	�B	�%B	�?B	�FB	��B	��B	��B	�8B	�lB	�8B	�B	��B	��B	�RB	�lB	��B	�DB	��B	��B	�B	��B	��B	�JB	�B	��B	�B	��B	��B	��B	�wB	�.B	��B
 B
 iB
 iB
 4B
 B
 iB
 �B
 B
 iB
 �B
oB
oB
B
aB
3B
�B
�B
B
mB
�B
�B
�B
B
B
�B
�B
B
	B
�B
	�B

=B

=B

rB
B
B
B

�B
)B
�B
�B
�B
JB
dB
dB
~B
B
jB
�B
�B
VB
�B
�B
�B
 B
 B
4B
B
�B
uB
�B
@B
:B
�B
�B
�B
uB
�B
&B
�B
&B
[B
�B
�B
B
�B
B
�B
?B
�B
�B
EB
KB
�B
KB
�B
�B
#B
WB
�B
)B
~B
OB
�B
�B
�B
�B
�B
5B
B
B
VB
�B
�B
 B
 vB
!�B
#B
#�B
$@B
$ZB
#�B
#�B
$�B
%B
%,B
%,B
%B
%,B
%,B
%FB
%�B
%�B
%�B
&�B
'�B
(>B
(�B
(�B
(�B
)_B
)�B
)�B
*B
*�B
*�B
+B
+B
+QB
+6B
+6B
+6B
+6B
+kB
+�B
,�B
,�B
,�B
-B
-B
-�B
-�B
.IB
.�B
.�B
.�B
.�B
.�B
/ B
/ B
/OB
/�B
0oB
0oB
0�B
1vB
1�B
1�B
2�B
3B
3MB
33B
3�B
3�B
3�B
3�B
4�B
4�B
5tB
5�B
6B
6zB
6�B
72B
7�B
7�B
7�B
7�B
88B
8�B
8�B
9	B
9�B
9�B
:*B
:�B
;B
;B
:�B
;dB
;dB
;�B
;�B
;�B
;�B
<jB
<PB
<PB
<�B
<�B
<�B
<�B
<�B
<�B
=B
=<B
=qB
>BB
>]B
>BB
>BB
>BB
?B
?HB
?}B
@�B
@�B
@�B
A B
A�B
A�B
A�B
A�B
A�B
BAB
B[B
B�B
B�B
B�B
B�B
C-B
CGB
CaB
CaB
CaB
C-B
C�B
DMB
DgB
D�B
EmB
EmB
E�B
E�B
E�B
E�B
E�B
FYB
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G+B
GzB
HKB
H�B
I7B
IRB
IlB
I�B
I�B
J	B
J	B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
JXB
J�B
KxB
KxB
KxB
K�B
LJB
L~B
L�B
L�B
L�B
MB
M�B
NB
N<B
N�B
O�B
PB
PB
PB
O�B
O�B
O�B
O�B
PB
PB
PbB
Q4B
QNB
Q�B
RB
R B
RTB
R�B
R�B
R�B
R�B
R�B
R�B
SuB
S�B
TB
T,B
T,B
T�B
T�B
UgB
UgB
U�B
U�B
V9B
V�B
V�B
W$B
WsB
W�B
W�B
W�B
W�B
W�B
XyB
X�B
YB
Y1B
YKB
YKB
YB
Y�B
Y�B
ZB
ZkB
ZkB
Z�B
Z�B
Z�B
Z�B
[#B
[WB
[�B
\)B
\CB
\CB
\]B
\�B
\�B
\�B
\xB
]B
]�B
]�B
^jB
^�B
^�B
^�B
_B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`'B
`\B
`�B
aHB
abB
a|B
a�B
a�B
b4B
b�B
b�B
c B
c B
cTB
c�B
d&B
dZB
d�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
gB
gB
gB
gB
gB
gB
gRB
gRB
g�B
g�B
h>B
hXB
hXB
hXB
h�B
h�B
iDB
i�B
i�B
i�B
i�B
j�B
kB
k6B
k6B
k6B
k�B
k�B
lB
mB
mCB
m]B
mwB
mwB
m�B
m�B
m�B
nB
n/B
ncB
n}B
n�B
n�B
n�B
n�B
n�B
o5B
oiB
oiB
oOB
o�B
poB
p�B
qB
q'B
q'B
qAB
q'B
qvB
q�B
q�B
q�B
r-B
r�B
r�B
r�B
s3B
sMB
shB
s�B
s�B
s�B
s�B
s�B
tB
tB
t�B
t�B
t�B
u%B
u?B
uZB
uZB
u�B
u�B
u�B
vB
vFB
v`B
vzB
vzB
v�B
v�B
w2B
wfB
wfB
wfB
w�B
w�B
w�B
xB
xRB
x8B
xRB
x8B
x8B
xRB
xRB
x�B
y	B
y$B
yXB
y�B
y�B
y�B
y�B
z*B
z^B
z�B
z�B
z�B
{0B
{B
{�B
{�B
|6B
|PB
|PB
|jB
|PB
|�B
|�B
|�B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~B
~B
~B
~B
~(B
~(B
~wB
~wB
~�B
~�B
~�B
B
cB
cB
}B
�B
�B
�OB
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104940  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174702  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174702  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174702                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024710  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024710  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                