CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:47:24Z creation;2022-06-04T17:47:25Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174724  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��(|5�1   @��(��Ն@-e`A�7L�c�
=p��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX��B^ffBh  BpffBw��B33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�33B�33B���B���B�  B���B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C033C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�3D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @\)@��H@��HAp�A!p�AAp�Aap�A��RA��RA��RA��RA��RAиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BPBY(�B^Bh\)BpBw��B�\B�.B�.B�ǮB�.B�.B�.B�.B�.B�.B�.B�.B���B�ǮB�ǮB�.B�.B�.B�.B�.B�.B�aGB�aGB���B���B�.B���B�.B�.B�.B�.B�.C 
C
C
C0�C
C

C
C
C
C
C
C
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
C0J=C2
C4
C6
C8
C:
C<
C>
C@
CB
CD
CF
CH
CJ
CL
CN
CP
CR
CT
CV
CX
CZ
C\
C^
C`
Cb
Cd
Cf
Ch
Cj0�Cl
Cn
Cp
Cr
Ct
Cv
Cx
Cz
C|
C~
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC�RC�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C�RC�RD )D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D")D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC�)DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx�)Dy)Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D���D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D��D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D��D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D��D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D�D���D��D�B�DÂ�D���D��D�B�DĂ�D���D��D�B�Dł�D���D�D�B�DƂ�D���D��D�B�Dǂ�D���D��D�B�DȂ�D���D��D�B�Dɂ�D���D��D�B�Dʂ�D���D��D�B�D˂�D���D��D�B�D̂�D���D��D�B�D͂�D���D��D�B�D΂�D���D��D�B�Dς�D���D��D�B�DЂ�D���D��D�B�Dт�D���D��D�B�D҂�D���D��D�B�Dӂ�D���D��D�B�DԂ�D���D��D�B�DՂ�D���D��D�B�Dւ�D���D��D�B�Dׂ�D���D��D�B�D؂�D���D��D�B�Dق�D���D��D�B�Dڂ�D���D��D�B�Dۂ�D���D��D�B�D܂�D���D��D�B�D݂�D���D��D�B�Dނ�D���D��D�B�D߂�D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D��D�D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D���D�B�D���D���D��D�B�D���D���D��D�B�D�|{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�OBA�R�A�YKA�[�A�Z�A�[WA�\]A�Z�A�[�A�[�A�`�A�b�A�c�A�e,A�e�A�e�A�g�A�g8A�e,A�c�A�f�A�c�A�X�A�A�y�A��Aڑ4A�_A��A�w2Aؒ�A�M�A��2Å�A�R�A��A�i�A�FtA��EA���A��A���A��A��A�M6A��1A�($A�b�A��A���A�v�A�DgA���A��A��A�:A��A��<A�K�A�h
A�aA�ZQA��wA���A�A��A��A�A��MA���A�J�A�X�A�GA�OA���A���A���A�kA���A��]A���A���A��gA�%�A���A�G�A~x�Ay��At�AqN<Ao2aAmc�Ak|�Ai�Af{�Ab�A`A\iDAV��AT��ATB[AS�*AR��AP�}AN�zALB[AG4�AB�AA:*A@u%A@�A?�qA?7LA>�A<��A;:*A8�ZA8_A7�A6JA4�A3�hA2a|A1��A0�YA/�A/:*A.�OA._�A-7A+TaA)jA'�vA'.IA'!�A'�A&�A&�zA&`�A$��A$@�A#|A#,�A#[�A"N<A �yA T�A/�A|�A�AcA�HAO�A�A��A;AݘA֡A��A�0A�XA4�A:*A�AU�A�cA_A-�A�A��A�A7LA��AB�A$�A�BA9XA��A�AMA@�A:�A*�AuAخA�+AAMA�cA�$A��AQAp�A
�=A	�A	�_A	{A�vA��A��Ad�A�AbNA�{A:*A��A�#AX�A͟AoiA�=A%FA�$AK^A ��A 	@�!�@��@��D@���@�Ta@���@�ff@��-@��U@��@�(�@�@�ȴ@���@���@�~@���@�%@��<@�C-@�*@��@�@�9�@�@�t@�Ta@��@�@��@�/�@�I@��@�J�@��y@�7@�n@���@�c @���@�e,@�o@�j@���@�"�@�q@���@�0@�n/@��@�<�@�rG@�F�@��@ޮ}@�x@��@�r�@��@�RT@�Ĝ@�e@٬q@�C@�}V@��Z@�H�@־�@Ոf@�҉@�3�@�o�@�@ҁo@��@��X@Љ�@�g8@���@Ϻ^@�\)@�;@Ε@�1�@��@ͷ@�}�@��8@�W�@���@�@O@ʦL@�3�@�
�@ɘ�@���@�]d@���@ǘ�@�hs@�C�@���@Ƶ�@ƀ�@�� @��@�~(@�$�@Ó@�!�@¥z@�M@��D@��@�4�@�ѷ@�p;@��a@�s�@���@�I�@���@�iD@��B@�@�@��@���@���@��7@��P@���@��@���@�A @���@�=q@���@��M@��@���@�C-@�~@� �@�P�@��@�)�@���@�+@��j@��@�V@�.�@�  @���@���@��9@��@��K@��$@�@�r�@��@��@��@��O@�g8@�($@���@�1�@��`@���@�� @�5?@��@���@���@���@���@�W?@�%@��/@��R@���@�7�@���@�f�@�)_@���@�_@��@�y�@�6z@��@���@��o@�0U@��@�@���@�u�@�A�@��@���@�_�@��;@��$@�iD@�1�@��@���@�V�@�{@��3@�H�@�o@���@��h@�kQ@�/�@�{@��@���@���@�A�@��$@�($@���@�0�@��@�ȴ@�e�@�;�@���@��@���@�c�@���@���@��+@�s�@�Xy@�7@��j@��X@�{J@�F@��s@�\�@��j@���@���@�i�@�0U@��T@�c@��f@���@�U2@��@���@��~@�e,@�J�@�&@��@���@��.@�u�@�M@��@���@�e�@���@��2@�ȴ@�0U@���@���@��-@���@�^�@�%@�YK@�$@��@���@��@@��'@���@��P@��{@�`B@�.I@��@��@�
=@���@��?@��@�ff@�!�@���@���@���@�C@��@���@��F@�|�@�Z@��@��j@���@�qv@�Mj@��@��E@���@���@���@�h�@�L0@���@��q@�]�@�@@��@���@��'@�z@�:�@���@���@�T�@��	@���@��4@�z@�@�@� �@�;@X�@"�@�@~ں@~E�@}��@}@|Ɇ@|u�@{�V@{W?@{'�@z��@z�2@z�6@z\�@y�@yhs@y%@x�@x��@x�@x~(@x'R@w!-@v�x@v)�@u�@u@u�@u��@uf�@u:�@t��@th�@s��@rOv@p�|@oqv@o�@n��@n��@n�1@ni�@m�Z@m�@mA @l��@l�@l�@ll"@lC-@l�@k�&@k�@@kx@kE9@k
=@j�@j�8@j��@iԕ@h�@h'R@g�6@g�:@gv`@g>�@f��@f��@f�L@f8�@e��@e�@d�Y@c�w@c��@c~�@c�@bz@b@`�v@`��@`h�@`M@_�A@_�w@_W?@_
=@^}V@]�@]��@]�M@]o @]\�@]+@\Ĝ@\M@[�:@[&@Z�H@ZYK@Y�X@YL�@Y@@X֡@X�z@X��@Xq@X?�@W�F@W8@Vߤ@V��@V��@V~�@VTa@V&�@V �@U�C@T�@Tq@S�A@S+@R��@Rff@RE�@Ru@Qhs@P��@PtT@O�W@O\)@N��@N��@M�o@MT�@M;@L�9@L6@K��@Ks@J�@J3�@I�T@I#�@Hr�@G�V@GK�@G&@F��@F��@E�Z@EO�@E*0@E;@D9X@C�w@C�V@CU�@C�@B�@B�y@B�<@B8�@A��@A�@@e�@?�+@?��@?��@?��@?�@>��@>�@=�@=��@=o @=�@<��@<�U@<w�@<7�@;��@;�[@;��@;4�@:�@:�s@:�h@:L0@9��@9%F@8Ĝ@8��@8H@7�@7�w@7o�@7RT@71�@6�@6�1@6xl@6:*@6�@5�@5�@5�=@5^�@5�@4�@4�p@4Ĝ@4��@46@3ݘ@3�$@3|�@3a@3S@2�R@2�x@1��@1�n@1�@0��@0��@0g8@0I�@/�r@/��@/��@/�	@/s@/b�@/\)@/Mj@.��@.�@.L0@.u@-�@-�#@-�@-��@-�7@-s�@-�@,��@,�o@,N�@+�
@+��@+4�@*��@*��@*�h@)��@)�@)x�@)Vm@)5�@)�@(�5@(�@(�p@(Ĝ@(ѷ@(�U@(�O@(�@(:�@'��@'��@'�@@'��@'�@&�s@&Ta@%�S@%A @%O�@%A @$��@$�)@$h�@#��@#qv@#6z@"�c@"�@"�@"J�@"@"
�@!��@!�@!�@!�t@!�~@!|@!m]@!^�@!7L@ �`@ Q�@ �@�@�K@��@t�@,�@$t@��@��@��@��@;�@1�@#:@�@�o@�=@S&@+@�p@��@��@/�@�@�a@l�@_p@=@�@�c@�R@��@u%@�@�z@��@rG@Q�@%@�5@�/@�9@e�@�Q@��@��@��@��@�@ƨ@��@��@�@�P@6z@�\@-@�@�@�@�@��@�M@j@X@8�@@�@�@�)@�z@~(@$@��@v`@l�@4�@�@�@��@�b@{�@p;@R�@�.@�z@�^@��@��@m]@2a@��@�@Xy@%�@�@�m@�g@��@n/@>�@/�@�@�]@�\@��@c @&�@�"@hs@Dg@��@�Y@!@��@��@˒@��@�P@j�@b�@_p@_p@S�@A�@1�@!-@�@�@_p@t�@{J@a@E9@1�@�@
��@
��@
a|@
:*@
-@
($1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�OBA�R�A�YKA�[�A�Z�A�[WA�\]A�Z�A�[�A�[�A�`�A�b�A�c�A�e,A�e�A�e�A�g�A�g8A�e,A�c�A�f�A�c�A�X�A�A�y�A��Aڑ4A�_A��A�w2Aؒ�A�M�A��2Å�A�R�A��A�i�A�FtA��EA���A��A���A��A��A�M6A��1A�($A�b�A��A���A�v�A�DgA���A��A��A�:A��A��<A�K�A�h
A�aA�ZQA��wA���A�A��A��A�A��MA���A�J�A�X�A�GA�OA���A���A���A�kA���A��]A���A���A��gA�%�A���A�G�A~x�Ay��At�AqN<Ao2aAmc�Ak|�Ai�Af{�Ab�A`A\iDAV��AT��ATB[AS�*AR��AP�}AN�zALB[AG4�AB�AA:*A@u%A@�A?�qA?7LA>�A<��A;:*A8�ZA8_A7�A6JA4�A3�hA2a|A1��A0�YA/�A/:*A.�OA._�A-7A+TaA)jA'�vA'.IA'!�A'�A&�A&�zA&`�A$��A$@�A#|A#,�A#[�A"N<A �yA T�A/�A|�A�AcA�HAO�A�A��A;AݘA֡A��A�0A�XA4�A:*A�AU�A�cA_A-�A�A��A�A7LA��AB�A$�A�BA9XA��A�AMA@�A:�A*�AuAخA�+AAMA�cA�$A��AQAp�A
�=A	�A	�_A	{A�vA��A��Ad�A�AbNA�{A:*A��A�#AX�A͟AoiA�=A%FA�$AK^A ��A 	@�!�@��@��D@���@�Ta@���@�ff@��-@��U@��@�(�@�@�ȴ@���@���@�~@���@�%@��<@�C-@�*@��@�@�9�@�@�t@�Ta@��@�@��@�/�@�I@��@�J�@��y@�7@�n@���@�c @���@�e,@�o@�j@���@�"�@�q@���@�0@�n/@��@�<�@�rG@�F�@��@ޮ}@�x@��@�r�@��@�RT@�Ĝ@�e@٬q@�C@�}V@��Z@�H�@־�@Ոf@�҉@�3�@�o�@�@ҁo@��@��X@Љ�@�g8@���@Ϻ^@�\)@�;@Ε@�1�@��@ͷ@�}�@��8@�W�@���@�@O@ʦL@�3�@�
�@ɘ�@���@�]d@���@ǘ�@�hs@�C�@���@Ƶ�@ƀ�@�� @��@�~(@�$�@Ó@�!�@¥z@�M@��D@��@�4�@�ѷ@�p;@��a@�s�@���@�I�@���@�iD@��B@�@�@��@���@���@��7@��P@���@��@���@�A @���@�=q@���@��M@��@���@�C-@�~@� �@�P�@��@�)�@���@�+@��j@��@�V@�.�@�  @���@���@��9@��@��K@��$@�@�r�@��@��@��@��O@�g8@�($@���@�1�@��`@���@�� @�5?@��@���@���@���@���@�W?@�%@��/@��R@���@�7�@���@�f�@�)_@���@�_@��@�y�@�6z@��@���@��o@�0U@��@�@���@�u�@�A�@��@���@�_�@��;@��$@�iD@�1�@��@���@�V�@�{@��3@�H�@�o@���@��h@�kQ@�/�@�{@��@���@���@�A�@��$@�($@���@�0�@��@�ȴ@�e�@�;�@���@��@���@�c�@���@���@��+@�s�@�Xy@�7@��j@��X@�{J@�F@��s@�\�@��j@���@���@�i�@�0U@��T@�c@��f@���@�U2@��@���@��~@�e,@�J�@�&@��@���@��.@�u�@�M@��@���@�e�@���@��2@�ȴ@�0U@���@���@��-@���@�^�@�%@�YK@�$@��@���@��@@��'@���@��P@��{@�`B@�.I@��@��@�
=@���@��?@��@�ff@�!�@���@���@���@�C@��@���@��F@�|�@�Z@��@��j@���@�qv@�Mj@��@��E@���@���@���@�h�@�L0@���@��q@�]�@�@@��@���@��'@�z@�:�@���@���@�T�@��	@���@��4@�z@�@�@� �@�;@X�@"�@�@~ں@~E�@}��@}@|Ɇ@|u�@{�V@{W?@{'�@z��@z�2@z�6@z\�@y�@yhs@y%@x�@x��@x�@x~(@x'R@w!-@v�x@v)�@u�@u@u�@u��@uf�@u:�@t��@th�@s��@rOv@p�|@oqv@o�@n��@n��@n�1@ni�@m�Z@m�@mA @l��@l�@l�@ll"@lC-@l�@k�&@k�@@kx@kE9@k
=@j�@j�8@j��@iԕ@h�@h'R@g�6@g�:@gv`@g>�@f��@f��@f�L@f8�@e��@e�@d�Y@c�w@c��@c~�@c�@bz@b@`�v@`��@`h�@`M@_�A@_�w@_W?@_
=@^}V@]�@]��@]�M@]o @]\�@]+@\Ĝ@\M@[�:@[&@Z�H@ZYK@Y�X@YL�@Y@@X֡@X�z@X��@Xq@X?�@W�F@W8@Vߤ@V��@V��@V~�@VTa@V&�@V �@U�C@T�@Tq@S�A@S+@R��@Rff@RE�@Ru@Qhs@P��@PtT@O�W@O\)@N��@N��@M�o@MT�@M;@L�9@L6@K��@Ks@J�@J3�@I�T@I#�@Hr�@G�V@GK�@G&@F��@F��@E�Z@EO�@E*0@E;@D9X@C�w@C�V@CU�@C�@B�@B�y@B�<@B8�@A��@A�@@e�@?�+@?��@?��@?��@?�@>��@>�@=�@=��@=o @=�@<��@<�U@<w�@<7�@;��@;�[@;��@;4�@:�@:�s@:�h@:L0@9��@9%F@8Ĝ@8��@8H@7�@7�w@7o�@7RT@71�@6�@6�1@6xl@6:*@6�@5�@5�@5�=@5^�@5�@4�@4�p@4Ĝ@4��@46@3ݘ@3�$@3|�@3a@3S@2�R@2�x@1��@1�n@1�@0��@0��@0g8@0I�@/�r@/��@/��@/�	@/s@/b�@/\)@/Mj@.��@.�@.L0@.u@-�@-�#@-�@-��@-�7@-s�@-�@,��@,�o@,N�@+�
@+��@+4�@*��@*��@*�h@)��@)�@)x�@)Vm@)5�@)�@(�5@(�@(�p@(Ĝ@(ѷ@(�U@(�O@(�@(:�@'��@'��@'�@@'��@'�@&�s@&Ta@%�S@%A @%O�@%A @$��@$�)@$h�@#��@#qv@#6z@"�c@"�@"�@"J�@"@"
�@!��@!�@!�@!�t@!�~@!|@!m]@!^�@!7L@ �`@ Q�@ �@�@�K@��@t�@,�@$t@��@��@��@��@;�@1�@#:@�@�o@�=@S&@+@�p@��@��@/�@�@�a@l�@_p@=@�@�c@�R@��@u%@�@�z@��@rG@Q�@%@�5@�/@�9@e�@�Q@��@��@��@��@�@ƨ@��@��@�@�P@6z@�\@-@�@�@�@�@��@�M@j@X@8�@@�@�@�)@�z@~(@$@��@v`@l�@4�@�@�@��@�b@{�@p;@R�@�.@�z@�^@��@��@m]@2a@��@�@Xy@%�@�@�m@�g@��@n/@>�@/�@�@�]@�\@��@c @&�@�"@hs@Dg@��@�Y@!@��@��@˒@��@�P@j�@b�@_p@_p@S�@A�@1�@!-@�@�@_p@t�@{J@a@E9@1�@�@
��@
��@
a|@
:*@
-@
($1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B�B�DB�*B�*B�B��B��B��B��B�B��B�B��B��B��B��B�B��B��B�$B�B��B�B	9	B	I�B	CaB	B�B	6�B	B'B	(>B	B	+B	5�B	fLB	m�B	l�B	�EB
<PB
N�B
MjB
�zB
��B
�kB
��BAB�BWB�B1�BE�B[�Bv�Bv�B�sB�=B��B�mB�|B�8B��B�]B�
B�BB}BpBcTBW$BKxBxRB}�Bd�BG�B vB
��B
��B
ƎB
��B
��B
��B
.B
K^B
,WB
kB
mB	�XB	�kB	��B	��B	�B	��B	�B	��B	qAB	ZQB	M�B	<�B	$�B	�B	sB	B	�B	dB	B��B�B�*B�B�B��B߾BޞBݘBݲB�B��B�xB�B�jB�B�B�B��B�}B	aB	 �B��B	�B	�B	�B	MB	�B	uB	�B	�B	B	�B	�B	�B	B	"�B	-�B	;�B	GEB	I7B	LJB	Q4B	RoB	T,B	T�B	\)B	iDB	r-B	s�B	w2B	�oB	�B	�B	�<B	�B	�B	�;B	��B	�`B	��B	�B	�CB	�qB	��B	��B	��B	��B	��B	�MB	�hB	��B	��B	�B	��B	��B	��B	��B	��B	��B	�^B	�<B	��B	�<B	�B	�4B	�uB	�B	�?B	�mB	��B	�uB	�?B	ǮB	�B	��B	�^B	�B	ΊB	��B	�pB	̈́B	�<B	�PB	�B	�B	�\B	��B	�pB	�B	�B	�B	ϫB	�HB	�B	�B	�{B	��B	�aB	ҽB	��B	��B	ЗB	�}B	οB	͟B	��B	�B	�BB	��B	��B	��B	��B	ѷB	��B	�bB	�vB	ϫB	�.B	�}B	�HB	�bB	� B	бB	ЗB	�B	� B	�TB	�B	�:B	�B	�B	��B	��B	�oB	�:B	҉B	�@B	��B	�FB	�{B	��B	��B	ևB	ևB	�mB	��B	�sB	�$B	��B	�EB	ٴB	�_B	��B	��B	��B	خB	�_B	ؓB	�KB	ٴB	ٚB	��B	�+B	�KB	چB	��B	ںB	�kB	�kB	�B	�7B	�QB	ںB	�	B	��B	ڠB	ںB	�=B	��B	�B	�OB	�OB	�B	ޞB	�!B	ߤB	ߊB	��B	�B	�B	�B	�BB	�'B	�'B	��B	�|B	�B	��B	�B	��B	� B	�:B	�B	�B	�B	�ZB	�B	�B	�FB	�fB	�B	��B	�B	�B	�>B	�sB	�B	�B	�B	�6B	�B	�B	�WB	�B	�wB	��B	�IB	�5B	�OB	�iB	��B	�B	�B	��B	�[B	�B	��B	�hB	�B	��B	�B	�9B	�TB	��B	��B	��B	�`B	�FB	�zB	��B	��B	�B	�lB	�XB	�>B	��B	��B	�DB	��B	�0B	�0B	��B	�B	�B	�6B	�6B	��B	�"B	�VB	��B	�B	��B	�(B	��B	�B	��B	�.B	��B	��B
 4B
 B
UB
�B
uB
�B
�B
�B
3B
�B
�B
9B
�B
%B
�B
B
tB
�B
B
_B
�B
�B
�B
	�B

rB

�B

�B

�B
)B
^B
DB
)B
)B
^B
�B
�B
�B
�B
"B
B
B
�B
�B
pB
VB
<B
(B
.B
4B
 B
4B
NB
�B
B
�B
�B
@B
,B
�B
2B
gB
�B

B
�B
�B
�B
$B
�B
�B
�B
1B
�B
B
KB
�B
QB
#B
	B
�B
�B
#B
qB
�B
)B
�B
=B
qB
qB
B
�B
jB
jB
5B
~B
�B
B
�B
�B
 B
 \B
 �B
 �B
!HB
!|B
!�B
!�B
!�B
"4B
"hB
"�B
#B
"�B
"�B
"4B
!�B
"�B
#B
#:B
#�B
#�B
$&B
$�B
%,B
%�B
%�B
&LB
'B
'�B
(
B
(>B
)B
)yB
)�B
*B
*�B
*�B
+6B
+6B
+B
*�B
*�B
*�B
+B
+B
+kB
,�B
,�B
,�B
-B
-)B
-]B
-�B
-�B
-�B
-�B
-�B
.cB
.�B
/OB
/�B
0UB
0�B
1'B
1'B
1AB
1[B
1�B
33B
4TB
5B
5B
5B
5B
5%B
4�B
5B
4�B
4nB
4�B
5�B
5�B
6�B
7�B
7�B
8B
7�B
7�B
7fB
7�B
7fB
88B
8B
88B
8RB
8RB
8lB
9>B
9�B
9�B
:DB
:^B
:xB
:�B
:�B
:�B
:�B
;B
;0B
;B
;�B
;�B
;�B
;dB
<6B
<jB
=<B
=�B
=�B
=�B
>BB
>�B
>�B
>wB
>�B
?B
?HB
?�B
?cB
?HB
?HB
?�B
?�B
@OB
@�B
@�B
@�B
@�B
A B
AB
A B
A B
AoB
A�B
A�B
BB
BB
BB
B[B
B�B
C{B
D�B
E�B
E�B
F�B
G+B
G_B
GzB
G�B
G�B
G�B
G�B
HB
H�B
H�B
H1B
H�B
IB
IRB
I�B
J#B
J�B
J�B
K�B
K�B
K�B
LdB
L~B
L~B
L�B
L�B
MjB
M�B
M�B
N"B
N<B
N"B
NVB
N�B
N�B
O�B
O�B
P.B
PB
PHB
P}B
Q B
Q�B
R�B
S@B
S�B
S�B
S�B
TB
TFB
T�B
U2B
U2B
U2B
VB
VB
VB
V9B
VmB
VSB
V9B
V9B
VmB
V�B
W?B
WsB
WYB
W�B
XB
XEB
X�B
YB
ZB
ZQB
ZQB
ZQB
ZQB
Z�B
[#B
[=B
[�B
[�B
[�B
[�B
\)B
\CB
\]B
\]B
\�B
\�B
]�B
]�B
]�B
^B
^5B
^jB
^�B
^�B
^�B
^�B
_;B
_;B
_�B
_�B
_�B
_�B
_�B
`B
`BB
`\B
`\B
`BB
`vB
`�B
aHB
a|B
a�B
a�B
b�B
c:B
c B
dZB
d@B
d&B
eB
d�B
d�B
eB
eFB
e,B
eFB
ezB
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
gB
f�B
g8B
g�B
h
B
hsB
h�B
h�B
hXB
hXB
hXB
h$B
h�B
h�B
iDB
h�B
h�B
h�B
h�B
h�B
i_B
i_B
iyB
i�B
jB
kkB
l"B
l=B
l�B
l�B
l�B
l�B
l�B
m]B
m)B
m�B
n/B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
ncB
m�B
m�B
m�B
m�B
n}B
n}B
n�B
oB
oB
oiB
o�B
p!B
pB
o�B
o�B
o�B
o�B
o�B
p!B
pUB
pUB
poB
poB
p�B
p�B
p�B
q'B
q[B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
r-B
rGB
r|B
r�B
r�B
sB
shB
s�B
s�B
t�B
uB
t�B
t�B
u%B
t�B
t�B
t�B
uZB
u�B
u�B
u�B
u�B
vB
vB
v`B
v`B
v�B
wLB
w�B
xB
xB
x�B
x�B
y	B
y	B
y	B
y>B
y�B
y�B
z^B
zxB
zxB
zxB
zxB
zxB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{dB
{�B
|B
|�B
}"B
}<B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~BB
~BB
~�B
HB
�B
� B
� B
�B
��B
��B
�B
��B
�B
�;B
� B
� B
�B
� B
�UB
� B
� B
�B
��B
�;B
�;B
�oB
�oB
��B
�B
�AB
��B
��B
��B
��B
��B
��B
�B
�aB
��B
��B
��B
�B
��B
��B
��B
�B
�%B
�tB
��B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B�B�DB�*B�*B�B��B��B��B��B�B��B�B��B��B��B��B�B��B��B�$B�B��B�B	9	B	I�B	CaB	B�B	6�B	B'B	(>B	B	+B	5�B	fLB	m�B	l�B	�EB
<PB
N�B
MjB
�zB
��B
�kB
��BAB�BWB�B1�BE�B[�Bv�Bv�B�sB�=B��B�mB�|B�8B��B�]B�
B�BB}BpBcTBW$BKxBxRB}�Bd�BG�B vB
��B
��B
ƎB
��B
��B
��B
.B
K^B
,WB
kB
mB	�XB	�kB	��B	��B	�B	��B	�B	��B	qAB	ZQB	M�B	<�B	$�B	�B	sB	B	�B	dB	B��B�B�*B�B�B��B߾BޞBݘBݲB�B��B�xB�B�jB�B�B�B��B�}B	aB	 �B��B	�B	�B	�B	MB	�B	uB	�B	�B	B	�B	�B	�B	B	"�B	-�B	;�B	GEB	I7B	LJB	Q4B	RoB	T,B	T�B	\)B	iDB	r-B	s�B	w2B	�oB	�B	�B	�<B	�B	�B	�;B	��B	�`B	��B	�B	�CB	�qB	��B	��B	��B	��B	��B	�MB	�hB	��B	��B	�B	��B	��B	��B	��B	��B	��B	�^B	�<B	��B	�<B	�B	�4B	�uB	�B	�?B	�mB	��B	�uB	�?B	ǮB	�B	��B	�^B	�B	ΊB	��B	�pB	̈́B	�<B	�PB	�B	�B	�\B	��B	�pB	�B	�B	�B	ϫB	�HB	�B	�B	�{B	��B	�aB	ҽB	��B	��B	ЗB	�}B	οB	͟B	��B	�B	�BB	��B	��B	��B	��B	ѷB	��B	�bB	�vB	ϫB	�.B	�}B	�HB	�bB	� B	бB	ЗB	�B	� B	�TB	�B	�:B	�B	�B	��B	��B	�oB	�:B	҉B	�@B	��B	�FB	�{B	��B	��B	ևB	ևB	�mB	��B	�sB	�$B	��B	�EB	ٴB	�_B	��B	��B	��B	خB	�_B	ؓB	�KB	ٴB	ٚB	��B	�+B	�KB	چB	��B	ںB	�kB	�kB	�B	�7B	�QB	ںB	�	B	��B	ڠB	ںB	�=B	��B	�B	�OB	�OB	�B	ޞB	�!B	ߤB	ߊB	��B	�B	�B	�B	�BB	�'B	�'B	��B	�|B	�B	��B	�B	��B	� B	�:B	�B	�B	�B	�ZB	�B	�B	�FB	�fB	�B	��B	�B	�B	�>B	�sB	�B	�B	�B	�6B	�B	�B	�WB	�B	�wB	��B	�IB	�5B	�OB	�iB	��B	�B	�B	��B	�[B	�B	��B	�hB	�B	��B	�B	�9B	�TB	��B	��B	��B	�`B	�FB	�zB	��B	��B	�B	�lB	�XB	�>B	��B	��B	�DB	��B	�0B	�0B	��B	�B	�B	�6B	�6B	��B	�"B	�VB	��B	�B	��B	�(B	��B	�B	��B	�.B	��B	��B
 4B
 B
UB
�B
uB
�B
�B
�B
3B
�B
�B
9B
�B
%B
�B
B
tB
�B
B
_B
�B
�B
�B
	�B

rB

�B

�B

�B
)B
^B
DB
)B
)B
^B
�B
�B
�B
�B
"B
B
B
�B
�B
pB
VB
<B
(B
.B
4B
 B
4B
NB
�B
B
�B
�B
@B
,B
�B
2B
gB
�B

B
�B
�B
�B
$B
�B
�B
�B
1B
�B
B
KB
�B
QB
#B
	B
�B
�B
#B
qB
�B
)B
�B
=B
qB
qB
B
�B
jB
jB
5B
~B
�B
B
�B
�B
 B
 \B
 �B
 �B
!HB
!|B
!�B
!�B
!�B
"4B
"hB
"�B
#B
"�B
"�B
"4B
!�B
"�B
#B
#:B
#�B
#�B
$&B
$�B
%,B
%�B
%�B
&LB
'B
'�B
(
B
(>B
)B
)yB
)�B
*B
*�B
*�B
+6B
+6B
+B
*�B
*�B
*�B
+B
+B
+kB
,�B
,�B
,�B
-B
-)B
-]B
-�B
-�B
-�B
-�B
-�B
.cB
.�B
/OB
/�B
0UB
0�B
1'B
1'B
1AB
1[B
1�B
33B
4TB
5B
5B
5B
5B
5%B
4�B
5B
4�B
4nB
4�B
5�B
5�B
6�B
7�B
7�B
8B
7�B
7�B
7fB
7�B
7fB
88B
8B
88B
8RB
8RB
8lB
9>B
9�B
9�B
:DB
:^B
:xB
:�B
:�B
:�B
:�B
;B
;0B
;B
;�B
;�B
;�B
;dB
<6B
<jB
=<B
=�B
=�B
=�B
>BB
>�B
>�B
>wB
>�B
?B
?HB
?�B
?cB
?HB
?HB
?�B
?�B
@OB
@�B
@�B
@�B
@�B
A B
AB
A B
A B
AoB
A�B
A�B
BB
BB
BB
B[B
B�B
C{B
D�B
E�B
E�B
F�B
G+B
G_B
GzB
G�B
G�B
G�B
G�B
HB
H�B
H�B
H1B
H�B
IB
IRB
I�B
J#B
J�B
J�B
K�B
K�B
K�B
LdB
L~B
L~B
L�B
L�B
MjB
M�B
M�B
N"B
N<B
N"B
NVB
N�B
N�B
O�B
O�B
P.B
PB
PHB
P}B
Q B
Q�B
R�B
S@B
S�B
S�B
S�B
TB
TFB
T�B
U2B
U2B
U2B
VB
VB
VB
V9B
VmB
VSB
V9B
V9B
VmB
V�B
W?B
WsB
WYB
W�B
XB
XEB
X�B
YB
ZB
ZQB
ZQB
ZQB
ZQB
Z�B
[#B
[=B
[�B
[�B
[�B
[�B
\)B
\CB
\]B
\]B
\�B
\�B
]�B
]�B
]�B
^B
^5B
^jB
^�B
^�B
^�B
^�B
_;B
_;B
_�B
_�B
_�B
_�B
_�B
`B
`BB
`\B
`\B
`BB
`vB
`�B
aHB
a|B
a�B
a�B
b�B
c:B
c B
dZB
d@B
d&B
eB
d�B
d�B
eB
eFB
e,B
eFB
ezB
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
gB
f�B
g8B
g�B
h
B
hsB
h�B
h�B
hXB
hXB
hXB
h$B
h�B
h�B
iDB
h�B
h�B
h�B
h�B
h�B
i_B
i_B
iyB
i�B
jB
kkB
l"B
l=B
l�B
l�B
l�B
l�B
l�B
m]B
m)B
m�B
n/B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
ncB
m�B
m�B
m�B
m�B
n}B
n}B
n�B
oB
oB
oiB
o�B
p!B
pB
o�B
o�B
o�B
o�B
o�B
p!B
pUB
pUB
poB
poB
p�B
p�B
p�B
q'B
q[B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
r-B
rGB
r|B
r�B
r�B
sB
shB
s�B
s�B
t�B
uB
t�B
t�B
u%B
t�B
t�B
t�B
uZB
u�B
u�B
u�B
u�B
vB
vB
v`B
v`B
v�B
wLB
w�B
xB
xB
x�B
x�B
y	B
y	B
y	B
y>B
y�B
y�B
z^B
zxB
zxB
zxB
zxB
zxB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{dB
{�B
|B
|�B
}"B
}<B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~BB
~BB
~�B
HB
�B
� B
� B
�B
��B
��B
�B
��B
�B
�;B
� B
� B
�B
� B
�UB
� B
� B
�B
��B
�;B
�;B
�oB
�oB
��B
�B
�AB
��B
��B
��B
��B
��B
��B
�B
�aB
��B
��B
��B
�B
��B
��B
��B
�B
�%B
�tB
��B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104941  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174724  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174725  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174725                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024733  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024733  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                