CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:55:43Z creation;2022-06-04T17:55:43Z conversion to V3.1      
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604175543  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               >A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�@Kc-!�1   @�@K��7@0^�Q��b�z�G�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB ��B(  B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���BÙ�B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C�CffC��C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8�3C9��C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ33C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @(�@�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B�\B ��B((�B/B7B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�G�B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B��B��HBîB��HB�{B�{B�{B�{B�{B��HB�{B�{B�{B�{B��HB�{B�{C 
=C
=C
=C
=C
=C

=C
=C#�Cp�C�
C
=C
=C
=C
=C
=C
=C 
=C"
=C$
=C&
=C(
=C*
=C,
=C.
=C0
=C2#�C4
=C6
=C8�pC9�
C<
=C>
=C@
=CB
=CD
=CF
=CH
=CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX
=CZ=pC[�C^
=C`
=Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Cp
=Cr
=Ct
=Cv
=Cw�Cz
=C|
=C}�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C��C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�{D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�D{D��HD��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�YA�[#A�o�A�qvA�pA�gA�f2A�y>A�z�A�}�A�}�A��A�y�A�`A�RTA�5�A�$A�VA�	A�	7A�$A��A��A��A�Z�A֬A֩�A�ƨAց;A�+A��KA՘�A�TaA҆YA�+A��A�.A�d&A�a�A�|PA� �A�A�33A�e,A��Aë�A�2-A��rA��A��FA�S�A�?A�Q�A�%A�K�A��A��A�MA�qA��A��A��A�|�A�EmA��*A���A�N�A���A���A��dA���A��+A�!�A�=A���A�I�A��sA�H�A�P}A�-�A��?A�
rA��xA��^A���A�(XA���A���A�/A��xA��dA�{A�Y�Az��Aw�Au�Ar�Ao��Amm]Ai�BAcL�A_خA\v�AY"hAV�jAS�eAP�AM�AKE�AI4nAG� AF�UAEhsAC��AB�AB�AA"�A?oA<��A;��A;�A;^�A;YKA;V�A:�A:T�A:!�A9��A9��A7��A6��A6>�A6GA5�A4�YA2�A1z�A0خA/�PA.c A-ߤA-C�A.GA.�.A.OA-Z�A,�A*6A(M�A(�A(��A(��A(�[A(�A'��A&��A$�6A#�gA#��A##:A"��A"VmA"@OA"/�A"�A"1A!��A!��A ��A ��A�AVA��A($A��AϫAOvA"�A�HA �A��A�A4�A�/AS�A�WA��Ag8A��A�A��A_�Av�A�AXyAcAn/An/AW�A��AF�A	lA�*Aq�A{JA`�A�A=A>BA�^AL�A�oA�$A�A_pA�A��A�mA�A?}Az�A�A��Au�A!-A	YKAN�A$�AA�A�\A�AG�A��Ag8A�nArGAA�FA�+A�A �FA _pA ?A $�A V@���@�^�@��e@�_@���@�a|@�2�@��@��>@�|�@�B�@��2@��@� �@�:�@�.I@���@�"h@��@���@�@�3�@�-w@�z�@�=@��@�N�@�o @�R@��6@��[@��@��@ꛦ@�h@�#�@��v@�2�@��@�e�@� i@思@�-�@�=@�d�@�s@��y@��@�$@�t@࿱@��@�Q�@���@�C�@ݙ�@�Mj@�@@��@�|�@��@��K@�`B@�@@ڨ�@�	@ي	@�4@�^�@�C�@��)@���@��@ԩ�@ԝI@�H�@���@�qv@�4@ҵ�@�($@�;d@�A�@�G@�}�@��B@�\�@��@��@ʹ�@�Q�@�ی@�z@�.I@���@�ی@ʨ�@ʆY@�h�@ɶF@�C@��]@Ȗ�@Ǯ@�:�@ƶ�@��;@��N@��6@��z@�@�2�@ÖS@��@A@��Z@��C@�A�@��@��@�@�P�@��@���@���@��)@���@��[@�j@�o@�R�@��h@�b�@�/@���@�~�@��0@�\�@��8@���@�z�@��@��W@���@��[@��@�Ɇ@��@���@��"@�0�@���@�~(@���@�f�@�>�@�V@��}@�YK@��@��#@��z@�@���@��:@���@��@�m�@��@��]@���@��;@��"@��@��@���@�?�@��-@��f@�8�@��@��@�z@�C�@�!@�b@��#@���@�~�@�X�@�Y@�Ɇ@�z�@�@���@�b�@�@��?@��+@�_@�(�@���@�@�͟@�R�@���@���@�Vm@�,�@�@���@�-@��@��q@�x@�S�@��@��9@��I@�V�@��'@�IR@�;@��6@�Z�@�#:@��D@��@��H@�|�@�>�@�'�@��f@�q@�>B@�~@��@�a@��K@���@�}V@��@��&@�� @��d@��*@�]�@��8@��?@��o@�Ft@�(�@�v`@� \@���@���@��@�|�@�0U@��o@��@@�_p@�%F@��R@���@�}V@�$@��@��@�n/@��"@��9@�Xy@��@��.@��>@���@�^�@�#�@���@��@��@���@�R�@�%�@��@��@���@�^�@�@��X@��I@�c�@���@���@�o�@�F@�!-@���@���@�Z@�8�@���@���@�o�@�C�@��@��@���@���@�u�@�_@�@�@��@���@��V@�F�@��@��I@���@�R�@��@���@��n@�k�@�:�@��@���@�0U@�@���@�ϫ@���@���@�v`@�F�@�'�@��@���@�n�@��>@���@��;@���@�hs@���@��.@�V�@���@��F@���@�{J@��@��F@���@���@�]d@�PH@�Q�@�!@4�@~��@~��@~ȴ@~=q@}��@}�'@}=�@|ی@|I�@{�@{� @{��@{g�@z�R@z$�@yԕ@y�M@y+@x��@xw�@w�@wS�@wC�@w@O@v��@u�Z@uj@tl"@s@O@r��@r��@q�@q�h@q=�@p��@p�j@pC-@o�W@oRT@n��@nYK@n!�@m`B@m;@l�v@lѷ@l�j@lS�@k�@@j��@j
�@i�C@i`B@h��@hy>@h  @g�4@g9�@g�@f�}@fL0@f
�@e��@eX@eG�@e7L@e	l@d��@dc�@d�@c�{@c8@b�@b=q@a��@a0�@`bN@_~�@^��@^ں@^�,@^�@^��@]�@]�@]hs@]%F@\Ɇ@\]d@[خ@[U�@[Y@Z�R@ZR�@Y�T@YVm@Y@@X��@Xr�@W��@W_p@V��@V}V@U�z@Tی@T�@T2�@S�q@R�"@R��@R\�@R@�@Q�>@Q�~@Q@P��@P��@P�@P��@P�.@Px@O�{@O"�@N��@M��@M�S@Ma�@Le�@L$@K�@K�@K�{@K.I@J�2@J�@Je@I8�@H�@H?�@G{J@F��@F1�@E�>@Em]@E2a@D��@D�D@D2�@Cݘ@C� @C�a@C�q@CJ#@B��@Bd�@A�)@Ac@A^�@A/@A�@@�`@@��@?ƨ@?��@?S�@>��@>H�@=��@=�-@=[W@<Ĝ@<w�@<S�@<,=@;��@;˒@;�*@;s@:҉@:J�@:�@9�)@9��@9�S@9}�@9B�@8�P@8��@7��@7�4@7t�@76z@6��@6#:@5�C@5o @5G�@4�5@4��@4��@49X@47@3�@3��@3��@3�*@3A�@2�m@2��@2_@1�9@1�-@1��@1�M@1f�@15�@0�?@0��@0��@0 �@/�a@/g�@/;d@/
=@.�@.s�@.W�@.8�@-�.@-�#@-�t@-�S@-p�@-@,�v@,�p@,��@,��@,�D@,Xy@+��@+��@+dZ@+=@+'�@*�M@*B[@*)�@)��@)��@)��@)��@)e,@)q@(�I@(u�@(A�@(7@'�@'�
@'�}@'��@'F�@'4�@'�@&�B@&��@&	@%�o@%��@%L�@%@%�@$��@$ی@$l"@$�@#ƨ@#S�@"�2@"��@"L0@"�@!�o@!��@![W@ �O@ Z@ K^@ *�@ �@خ@�@@o�@RT@�@��@��@}V@_�@V@H�@$�@�Z@��@��@�~@p�@X@!�@��@�/@��@S�@'R@�@��@iD@O@6z@�@��@ں@��@ff@5?@{@�@�t@c�@2a@�@;@��@oi@$@�Q@�*@��@x@O@&@�L@��@��@�@<6@:�@%@ی@��@w�@6@�@��@�6@�P@b�@6z@Y@@�2@�b@n�@?@�@�@��@�X@�"@`B@Dg@+�@�@��@��@�@�@j@Q�@2�@�r@�;@�@iD@4�@�@��@��@��@�+@3�@@�j@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�YA�[#A�o�A�qvA�pA�gA�f2A�y>A�z�A�}�A�}�A��A�y�A�`A�RTA�5�A�$A�VA�	A�	7A�$A��A��A��A�Z�A֬A֩�A�ƨAց;A�+A��KA՘�A�TaA҆YA�+A��A�.A�d&A�a�A�|PA� �A�A�33A�e,A��Aë�A�2-A��rA��A��FA�S�A�?A�Q�A�%A�K�A��A��A�MA�qA��A��A��A�|�A�EmA��*A���A�N�A���A���A��dA���A��+A�!�A�=A���A�I�A��sA�H�A�P}A�-�A��?A�
rA��xA��^A���A�(XA���A���A�/A��xA��dA�{A�Y�Az��Aw�Au�Ar�Ao��Amm]Ai�BAcL�A_خA\v�AY"hAV�jAS�eAP�AM�AKE�AI4nAG� AF�UAEhsAC��AB�AB�AA"�A?oA<��A;��A;�A;^�A;YKA;V�A:�A:T�A:!�A9��A9��A7��A6��A6>�A6GA5�A4�YA2�A1z�A0خA/�PA.c A-ߤA-C�A.GA.�.A.OA-Z�A,�A*6A(M�A(�A(��A(��A(�[A(�A'��A&��A$�6A#�gA#��A##:A"��A"VmA"@OA"/�A"�A"1A!��A!��A ��A ��A�AVA��A($A��AϫAOvA"�A�HA �A��A�A4�A�/AS�A�WA��Ag8A��A�A��A_�Av�A�AXyAcAn/An/AW�A��AF�A	lA�*Aq�A{JA`�A�A=A>BA�^AL�A�oA�$A�A_pA�A��A�mA�A?}Az�A�A��Au�A!-A	YKAN�A$�AA�A�\A�AG�A��Ag8A�nArGAA�FA�+A�A �FA _pA ?A $�A V@���@�^�@��e@�_@���@�a|@�2�@��@��>@�|�@�B�@��2@��@� �@�:�@�.I@���@�"h@��@���@�@�3�@�-w@�z�@�=@��@�N�@�o @�R@��6@��[@��@��@ꛦ@�h@�#�@��v@�2�@��@�e�@� i@思@�-�@�=@�d�@�s@��y@��@�$@�t@࿱@��@�Q�@���@�C�@ݙ�@�Mj@�@@��@�|�@��@��K@�`B@�@@ڨ�@�	@ي	@�4@�^�@�C�@��)@���@��@ԩ�@ԝI@�H�@���@�qv@�4@ҵ�@�($@�;d@�A�@�G@�}�@��B@�\�@��@��@ʹ�@�Q�@�ی@�z@�.I@���@�ی@ʨ�@ʆY@�h�@ɶF@�C@��]@Ȗ�@Ǯ@�:�@ƶ�@��;@��N@��6@��z@�@�2�@ÖS@��@A@��Z@��C@�A�@��@��@�@�P�@��@���@���@��)@���@��[@�j@�o@�R�@��h@�b�@�/@���@�~�@��0@�\�@��8@���@�z�@��@��W@���@��[@��@�Ɇ@��@���@��"@�0�@���@�~(@���@�f�@�>�@�V@��}@�YK@��@��#@��z@�@���@��:@���@��@�m�@��@��]@���@��;@��"@��@��@���@�?�@��-@��f@�8�@��@��@�z@�C�@�!@�b@��#@���@�~�@�X�@�Y@�Ɇ@�z�@�@���@�b�@�@��?@��+@�_@�(�@���@�@�͟@�R�@���@���@�Vm@�,�@�@���@�-@��@��q@�x@�S�@��@��9@��I@�V�@��'@�IR@�;@��6@�Z�@�#:@��D@��@��H@�|�@�>�@�'�@��f@�q@�>B@�~@��@�a@��K@���@�}V@��@��&@�� @��d@��*@�]�@��8@��?@��o@�Ft@�(�@�v`@� \@���@���@��@�|�@�0U@��o@��@@�_p@�%F@��R@���@�}V@�$@��@��@�n/@��"@��9@�Xy@��@��.@��>@���@�^�@�#�@���@��@��@���@�R�@�%�@��@��@���@�^�@�@��X@��I@�c�@���@���@�o�@�F@�!-@���@���@�Z@�8�@���@���@�o�@�C�@��@��@���@���@�u�@�_@�@�@��@���@��V@�F�@��@��I@���@�R�@��@���@��n@�k�@�:�@��@���@�0U@�@���@�ϫ@���@���@�v`@�F�@�'�@��@���@�n�@��>@���@��;@���@�hs@���@��.@�V�@���@��F@���@�{J@��@��F@���@���@�]d@�PH@�Q�@�!@4�@~��@~��@~ȴ@~=q@}��@}�'@}=�@|ی@|I�@{�@{� @{��@{g�@z�R@z$�@yԕ@y�M@y+@x��@xw�@w�@wS�@wC�@w@O@v��@u�Z@uj@tl"@s@O@r��@r��@q�@q�h@q=�@p��@p�j@pC-@o�W@oRT@n��@nYK@n!�@m`B@m;@l�v@lѷ@l�j@lS�@k�@@j��@j
�@i�C@i`B@h��@hy>@h  @g�4@g9�@g�@f�}@fL0@f
�@e��@eX@eG�@e7L@e	l@d��@dc�@d�@c�{@c8@b�@b=q@a��@a0�@`bN@_~�@^��@^ں@^�,@^�@^��@]�@]�@]hs@]%F@\Ɇ@\]d@[خ@[U�@[Y@Z�R@ZR�@Y�T@YVm@Y@@X��@Xr�@W��@W_p@V��@V}V@U�z@Tی@T�@T2�@S�q@R�"@R��@R\�@R@�@Q�>@Q�~@Q@P��@P��@P�@P��@P�.@Px@O�{@O"�@N��@M��@M�S@Ma�@Le�@L$@K�@K�@K�{@K.I@J�2@J�@Je@I8�@H�@H?�@G{J@F��@F1�@E�>@Em]@E2a@D��@D�D@D2�@Cݘ@C� @C�a@C�q@CJ#@B��@Bd�@A�)@Ac@A^�@A/@A�@@�`@@��@?ƨ@?��@?S�@>��@>H�@=��@=�-@=[W@<Ĝ@<w�@<S�@<,=@;��@;˒@;�*@;s@:҉@:J�@:�@9�)@9��@9�S@9}�@9B�@8�P@8��@7��@7�4@7t�@76z@6��@6#:@5�C@5o @5G�@4�5@4��@4��@49X@47@3�@3��@3��@3�*@3A�@2�m@2��@2_@1�9@1�-@1��@1�M@1f�@15�@0�?@0��@0��@0 �@/�a@/g�@/;d@/
=@.�@.s�@.W�@.8�@-�.@-�#@-�t@-�S@-p�@-@,�v@,�p@,��@,��@,�D@,Xy@+��@+��@+dZ@+=@+'�@*�M@*B[@*)�@)��@)��@)��@)��@)e,@)q@(�I@(u�@(A�@(7@'�@'�
@'�}@'��@'F�@'4�@'�@&�B@&��@&	@%�o@%��@%L�@%@%�@$��@$ی@$l"@$�@#ƨ@#S�@"�2@"��@"L0@"�@!�o@!��@![W@ �O@ Z@ K^@ *�@ �@خ@�@@o�@RT@�@��@��@}V@_�@V@H�@$�@�Z@��@��@�~@p�@X@!�@��@�/@��@S�@'R@�@��@iD@O@6z@�@��@ں@��@ff@5?@{@�@�t@c�@2a@�@;@��@oi@$@�Q@�*@��@x@O@&@�L@��@��@�@<6@:�@%@ی@��@w�@6@�@��@�6@�P@b�@6z@Y@@�2@�b@n�@?@�@�@��@�X@�"@`B@Dg@+�@�@��@��@�@�@j@Q�@2�@�r@�;@�@iD@4�@�@��@��@��@�+@3�@@�j@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B�LB�zB��B�.B	�B	 �B	�B		7B	
=B	6B	B	�B	bB	�B	B	SB	kB	�B	+B
�B
0�B
*�B
.�B
6�B
7�B
/�B
/5B
3hB
9�B
�B

�B
G�B
L0B
M6B
OBB
LdB
D�B
=�B
8�B
7�B
7�B
7�B
9>B
<6B
:�B
:^B
O�B
u�B
�B
�QB
�2B
�SB
ƨB�BEB	�B�B�B;�B>]B&�BTB]�BcTBf�B|PB��B��B_�BVBB�B:*B5B%B�B�B
��B
��B
��B
�:B
��B
~�B
iDB
VB
M�B
EmB
;JB
aB	��B	��B	�oB	�B	xB	e�B	WsB	F%B	# B	�B	�B��B�vB�B�B�B�0B��B�B��B	�B	B	$B	�B	!B	(sB	B[B	jeB	��B	�|B	�!B	��B	�SB	�.B	ٴB	�kB	�B	�=B	�B	�;B	�;B	��B	��B	ּB	�B	�gB	��B	�>B	�"B	��B	�+B	�B	�lB	�B	��B	�bB	�=B	�B	��B
'B
~B
�B
	�B
�B
�B
%B
B
 B
uB
�B
�B
�B
�B
�B
�B
FB
�B
�B
�B
�B
aB
}B
�B
�B
�B
�B
�B	��B	�BB	�B	�wB	�,B	��B	�XB	�oB	�B	�B	�B	�!B	�B	�jB	��B	�eB	ٚB	�kB	�xB	�CB	��B	�?B	ؓB	ܒB	��B	�DB	��B	��B	��B
�B
MB
�B
SB
�B
_B
	B

�B
�B
�B
KB
"�B
-B
,B
+�B
)B
#�B
#B
�B
�B
�B
�B
�B	�2B	�MB	��B	�B	�B	��B	�B	��B	�B	�B	��B	��B	��B	�-B	�BB	�\B	��B	�B	�B	��B	��B	�HB	��B	��B	�B	�LB	��B	��B	�
B	�$B	�>B	�$B	�B	�B	�_B	�0B	�kB	��B	��B	�]B	�=B	�B	��B	�B	�]B	�qB	�B	�B	�kB	�B	�qB	�)B	�B	�WB	�B	�"B	�"B	�B	�6B	�kB	�B	�6B	�0B	��B	��B	�DB	��B	�B	�B	��B	�QB	�B	��B	�B	�B	�B	�B	��B	��B	�XB	�B	�yB	��B	�*B	�yB	��B	��B	�B	�B	��B	��B	�B	�KB	�KB	�eB	�B	�B	�_B	��B	�B	��B	�B	�B	�KB	��B	�B	�_B	�B	��B	�KB	�B	�B	�eB	��B	�B	�B	�B	�6B	��B	�kB	��B	�B	�B	�6B	�QB	�kB	�6B	��B	�KB	�B	�B	�
B	�B	��B	�>B	��B	��B	��B	�zB	�nB	�B	��B	�4B	�B	�B	��B	�vB	��B	�B	�VB	�OB	߾B	� B	�TB	�B	��B	�4B	�B	� B	�B	�B	��B	�OB	��B	�B	�9B	��B	��B	�B	��B	��B	��B	�B	��B	�fB	��B	�B	��B	��B	��B	�rB	�*B	�^B	�DB	�*B	��B	��B	��B	��B	�B	�B	�<B	��B	�BB	��B
;B
�B
�B
�B
�B
�B
uB
uB
uB
�B
�B
[B
'B
B
B
'B
�B
[B
uB
�B
{B
�B
gB
9B
�B
�B
YB
?B
?B
tB
_B
EB
�B
�B
�B
�B
�B
_B
�B
�B
�B
B
1B
KB
fB
�B
�B
�B
	B
	B
�B
	7B
	�B
	�B
	�B
	�B

rB
)B
B
)B
�B
B
�B
�B
JB
�B
6B
6B
6B
6B
B
B
�B
�B
\B
BB
\B
�B
�B
}B
�B
B
�B
NB
NB
�B
TB
TB
�B
�B
[B
�B
�B
B
FB
�B
�B
�B
�B
B
MB
�B
�B
�B
�B
B
B
mB

B
$B
YB
?B
$B
$B

B
?B
EB
�B
�B
�B
+B
EB
EB
�B
�B
�B
�B
B
�B
�B
�B
B
QB
=B
�B
B
~B
�B
�B
jB
�B
!B
pB
pB
�B
 BB
 �B
!-B
!|B
!�B
"4B
"NB
"�B
#:B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%,B
%�B
%�B
&2B
&�B
&�B
'B
&�B
%�B
#�B
"NB
#nB
#TB
$B
$�B
$@B
%FB
%`B
%FB
%zB
&�B
'�B
'�B
(XB
(�B
)B
)B
)DB
)yB
*0B
*�B
*�B
*�B
*�B
+6B
+kB
+QB
+�B
+�B
+�B
+QB
*�B
)_B
'�B
&�B
'mB
'�B
'�B
(sB
(�B
)B
)_B
)yB
)�B
)�B
)�B
)�B
)�B
)�B
*�B
+6B
+�B
+�B
+�B
,B
,�B
-�B
-�B
./B
./B
.�B
.�B
.�B
/5B
/OB
/�B
/�B
0B
0;B
0�B
0�B
0�B
0�B
1B
1vB
1�B
2B
2GB
2B
2GB
2�B
2�B
3MB
4B
4�B
5ZB
5tB
5tB
5�B
5�B
6�B
6�B
7LB
7fB
8B
8�B
9	B
9�B
9�B
:*B
:�B
:�B
;0B
;0B
;�B
;�B
<jB
<�B
<�B
<�B
=�B
>wB
>�B
>�B
?�B
@OB
@�B
A B
A;B
A�B
A�B
B[B
BuB
BuB
B�B
B�B
BuB
B�B
C�B
DB
D�B
E�B
E�B
E�B
F�B
GB
H1B
H�B
H�B
IRB
IlB
I�B
I�B
IB
H�B
I�B
J�B
KB
KB
K^B
KxB
K�B
K�B
K�B
L0B
LJB
LJB
LJB
LdB
L�B
M6B
MjB
M�B
NVB
NVB
N�B
N�B
N�B
OB
O�B
O�B
P.B
P�B
P�B
QB
QNB
Q�B
Q�B
R B
R:B
RoB
R�B
R�B
R�B
R�B
S�B
S�B
TB
TFB
TaB
T{B
T�B
T�B
T�B
UB
U�B
VB
VB
VSB
V�B
W?B
W�B
W�B
W�B
X+B
XEB
XyB
X�B
X�B
X�B
YB
YB
X�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
[	B
[	B
[WB
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]IB
]�B
]�B
]�B
]�B
]�B
^B
^B
^5B
^OB
^�B
^�B
^�B
_;B
_!B
_B
_;B
_�B
_�B
`'B
`BB
`BB
`vB
abB
aB
abB
aHB
a|B
a�B
a�B
a�B
b�B
b�B
b�B
cB
c B
c:B
c:B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
eB
ezB
e�B
e�B
e�B
e�B
ffB
f�B
f�B
g8B
g�B
g�B
h$B
hXB
hsB
hsB
h�B
i�B
i�B
i�B
i�B
j0B
jKB
jB
j�B
j�B
kB
k6B
kkB
k�B
k�B
k�B
k�B
lB
l"B
lWB
l�B
l�B
l�B
l�B
l�B
m)B
mCB
mwB
m�B
m�B
m�B
n�B
n�B
o B
o B
o5B
o5B
oOB
o�B
o�B
pB
p!B
p;B
p�B
p�B
qB
qB
q'B
qvB
q�B
q�B
r-B
raB
r|B
r|B
r�B
r�B
s3B
tB
tB
tnB
t�B
t�B
t�B
t�B
u%B
uZB
u�B
u�B
u�B
u�B
vFB
vzB
v�B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
xB
xB
xB
xB
xRB
xlB
x�B
x�B
x�B
x�B
y$B
y>B
y>B
yXB
yrB
y�B
y�B
y�B
zDB
z^B
z�B
z�B
z�B
z�B
z�B
{dB
{�B
{�B
{�B
{�B
|P1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B�LB�zB��B�.B	�B	 �B	�B		7B	
=B	6B	B	�B	bB	�B	B	SB	kB	�B	+B
�B
0�B
*�B
.�B
6�B
7�B
/�B
/5B
3hB
9�B
�B

�B
G�B
L0B
M6B
OBB
LdB
D�B
=�B
8�B
7�B
7�B
7�B
9>B
<6B
:�B
:^B
O�B
u�B
�B
�QB
�2B
�SB
ƨB�BEB	�B�B�B;�B>]B&�BTB]�BcTBf�B|PB��B��B_�BVBB�B:*B5B%B�B�B
��B
��B
��B
�:B
��B
~�B
iDB
VB
M�B
EmB
;JB
aB	��B	��B	�oB	�B	xB	e�B	WsB	F%B	# B	�B	�B��B�vB�B�B�B�0B��B�B��B	�B	B	$B	�B	!B	(sB	B[B	jeB	��B	�|B	�!B	��B	�SB	�.B	ٴB	�kB	�B	�=B	�B	�;B	�;B	��B	��B	ּB	�B	�gB	��B	�>B	�"B	��B	�+B	�B	�lB	�B	��B	�bB	�=B	�B	��B
'B
~B
�B
	�B
�B
�B
%B
B
 B
uB
�B
�B
�B
�B
�B
�B
FB
�B
�B
�B
�B
aB
}B
�B
�B
�B
�B
�B	��B	�BB	�B	�wB	�,B	��B	�XB	�oB	�B	�B	�B	�!B	�B	�jB	��B	�eB	ٚB	�kB	�xB	�CB	��B	�?B	ؓB	ܒB	��B	�DB	��B	��B	��B
�B
MB
�B
SB
�B
_B
	B

�B
�B
�B
KB
"�B
-B
,B
+�B
)B
#�B
#B
�B
�B
�B
�B
�B	�2B	�MB	��B	�B	�B	��B	�B	��B	�B	�B	��B	��B	��B	�-B	�BB	�\B	��B	�B	�B	��B	��B	�HB	��B	��B	�B	�LB	��B	��B	�
B	�$B	�>B	�$B	�B	�B	�_B	�0B	�kB	��B	��B	�]B	�=B	�B	��B	�B	�]B	�qB	�B	�B	�kB	�B	�qB	�)B	�B	�WB	�B	�"B	�"B	�B	�6B	�kB	�B	�6B	�0B	��B	��B	�DB	��B	�B	�B	��B	�QB	�B	��B	�B	�B	�B	�B	��B	��B	�XB	�B	�yB	��B	�*B	�yB	��B	��B	�B	�B	��B	��B	�B	�KB	�KB	�eB	�B	�B	�_B	��B	�B	��B	�B	�B	�KB	��B	�B	�_B	�B	��B	�KB	�B	�B	�eB	��B	�B	�B	�B	�6B	��B	�kB	��B	�B	�B	�6B	�QB	�kB	�6B	��B	�KB	�B	�B	�
B	�B	��B	�>B	��B	��B	��B	�zB	�nB	�B	��B	�4B	�B	�B	��B	�vB	��B	�B	�VB	�OB	߾B	� B	�TB	�B	��B	�4B	�B	� B	�B	�B	��B	�OB	��B	�B	�9B	��B	��B	�B	��B	��B	��B	�B	��B	�fB	��B	�B	��B	��B	��B	�rB	�*B	�^B	�DB	�*B	��B	��B	��B	��B	�B	�B	�<B	��B	�BB	��B
;B
�B
�B
�B
�B
�B
uB
uB
uB
�B
�B
[B
'B
B
B
'B
�B
[B
uB
�B
{B
�B
gB
9B
�B
�B
YB
?B
?B
tB
_B
EB
�B
�B
�B
�B
�B
_B
�B
�B
�B
B
1B
KB
fB
�B
�B
�B
	B
	B
�B
	7B
	�B
	�B
	�B
	�B

rB
)B
B
)B
�B
B
�B
�B
JB
�B
6B
6B
6B
6B
B
B
�B
�B
\B
BB
\B
�B
�B
}B
�B
B
�B
NB
NB
�B
TB
TB
�B
�B
[B
�B
�B
B
FB
�B
�B
�B
�B
B
MB
�B
�B
�B
�B
B
B
mB

B
$B
YB
?B
$B
$B

B
?B
EB
�B
�B
�B
+B
EB
EB
�B
�B
�B
�B
B
�B
�B
�B
B
QB
=B
�B
B
~B
�B
�B
jB
�B
!B
pB
pB
�B
 BB
 �B
!-B
!|B
!�B
"4B
"NB
"�B
#:B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%,B
%�B
%�B
&2B
&�B
&�B
'B
&�B
%�B
#�B
"NB
#nB
#TB
$B
$�B
$@B
%FB
%`B
%FB
%zB
&�B
'�B
'�B
(XB
(�B
)B
)B
)DB
)yB
*0B
*�B
*�B
*�B
*�B
+6B
+kB
+QB
+�B
+�B
+�B
+QB
*�B
)_B
'�B
&�B
'mB
'�B
'�B
(sB
(�B
)B
)_B
)yB
)�B
)�B
)�B
)�B
)�B
)�B
*�B
+6B
+�B
+�B
+�B
,B
,�B
-�B
-�B
./B
./B
.�B
.�B
.�B
/5B
/OB
/�B
/�B
0B
0;B
0�B
0�B
0�B
0�B
1B
1vB
1�B
2B
2GB
2B
2GB
2�B
2�B
3MB
4B
4�B
5ZB
5tB
5tB
5�B
5�B
6�B
6�B
7LB
7fB
8B
8�B
9	B
9�B
9�B
:*B
:�B
:�B
;0B
;0B
;�B
;�B
<jB
<�B
<�B
<�B
=�B
>wB
>�B
>�B
?�B
@OB
@�B
A B
A;B
A�B
A�B
B[B
BuB
BuB
B�B
B�B
BuB
B�B
C�B
DB
D�B
E�B
E�B
E�B
F�B
GB
H1B
H�B
H�B
IRB
IlB
I�B
I�B
IB
H�B
I�B
J�B
KB
KB
K^B
KxB
K�B
K�B
K�B
L0B
LJB
LJB
LJB
LdB
L�B
M6B
MjB
M�B
NVB
NVB
N�B
N�B
N�B
OB
O�B
O�B
P.B
P�B
P�B
QB
QNB
Q�B
Q�B
R B
R:B
RoB
R�B
R�B
R�B
R�B
S�B
S�B
TB
TFB
TaB
T{B
T�B
T�B
T�B
UB
U�B
VB
VB
VSB
V�B
W?B
W�B
W�B
W�B
X+B
XEB
XyB
X�B
X�B
X�B
YB
YB
X�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
[	B
[	B
[WB
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]IB
]�B
]�B
]�B
]�B
]�B
^B
^B
^5B
^OB
^�B
^�B
^�B
_;B
_!B
_B
_;B
_�B
_�B
`'B
`BB
`BB
`vB
abB
aB
abB
aHB
a|B
a�B
a�B
a�B
b�B
b�B
b�B
cB
c B
c:B
c:B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
eB
ezB
e�B
e�B
e�B
e�B
ffB
f�B
f�B
g8B
g�B
g�B
h$B
hXB
hsB
hsB
h�B
i�B
i�B
i�B
i�B
j0B
jKB
jB
j�B
j�B
kB
k6B
kkB
k�B
k�B
k�B
k�B
lB
l"B
lWB
l�B
l�B
l�B
l�B
l�B
m)B
mCB
mwB
m�B
m�B
m�B
n�B
n�B
o B
o B
o5B
o5B
oOB
o�B
o�B
pB
p!B
p;B
p�B
p�B
qB
qB
q'B
qvB
q�B
q�B
r-B
raB
r|B
r|B
r�B
r�B
s3B
tB
tB
tnB
t�B
t�B
t�B
t�B
u%B
uZB
u�B
u�B
u�B
u�B
vFB
vzB
v�B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
xB
xB
xB
xB
xRB
xlB
x�B
x�B
x�B
x�B
y$B
y>B
y>B
yXB
yrB
y�B
y�B
y�B
zDB
z^B
z�B
z�B
z�B
z�B
z�B
{dB
{�B
{�B
{�B
{�B
|P1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105000  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175543  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175543  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175543                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025551  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025551  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                