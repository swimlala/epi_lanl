CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:25:00Z creation;2022-06-04T17:25:00Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604172500  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��n�/�c1   @��oA��@,�~��"��c���l�D1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�  @�33A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB933B@  BG��BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C�C�C  C�fC�fC  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<33C=�fC?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D���D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @/\)@�G�@�z�A=qA ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0�\B9\)B@(�BGBP(�BX(�B_Bh(�Bp(�Bx(�B�{B�{B�{B�{B��B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B��B�{B�{B�{B�{B�{B�{B�{B�G�B��HB�{B�{B�{B�{B�{B�{B��HC 
=C
=C
=C
=C
=C

=C#�C#�C
=C�C�C
=C�C
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
=C2
=C4
=C6
=C8
=C:
=C<=pC=�C?�CB
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
=CZ
=C\
=C^
=C`
=Cb
=Cc�Cf
=Ch
=Cj#�Cl
=Cn
=Cp
=Cr
=Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�)D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�{D�D{D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��{D��{D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD��D�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��{D�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��vA��A��A��QA��A���A�� A��A��A���A���A��A���A��iA��vA��A���A���A���A��JA��JA���A��2A��rA���A��xA��A׮�A�~]A��AӁ�A�W�A�{A���A���A�A�iA��UA�P�A��HA�ѷAȒ�A�poAǞ�Aƫ6A���A�h�A� \A��fA��A���A���A��uA�+�A��A���A�ںA�	A�ӏA��aA�GEA��_A��A��A��A�A�T,A���A�ߤA�S[A�cTA�cA��A�^5A��A��"A�,A� �A�$�A���A�g�A��XA���A��0A�:^A��A��A���A���A�
	A�qvA�)�A��A���A���A��A}��A{@Ay/�Axp;Aw��At�\Aq��Aly>AeɆAbqA_�A]ƨAZ�AVU�AP>�AL,�AJ|�AHS�AD��AB�;A@ZA>ݘA=�VA<�$A<n�A<;�A<�A:dZA6�A4�nA4#�A3.IA1�MA/"hA-�+A-�A-ںA.&�A-u%A+e�A*�SA)O�A'+A'��A'�7A'2aA&+�A$�MA#�>A#��A#l"A#>BA#1�A"��A"1'A!خA!h
A �FA (�A��A��A�fAc�AbA�MA�A�AS�A��A�HA�9A�~A�A��A�4ASAJ�A�A�AoiA?AA A&�A�$A��A	Ah
A�A�9AU�A�"A��A=qA�A�jA�"A�AQ�A��A[WA+A��A҉A��Av`A2�A�]A��Ae�A��A�'A�YA&�A�EA�AMA
��A
r�A
�A	�DA	�A|�A��AMA&�AϫA}�A[�A7A�uA��A�MA�1Av�A�A�A��A7A oiA Dg@��:@��VA �@��@��@��@�c@��[@�@@��f@���@��'@�H�@��@��8@��@��@�6z@�@��r@�2a@�	@�	@��@��@�}�@�.I@�!@��@��@��@��p@� �@�q@�Q�@��@�a|@�6@��Q@�=�@�z@��@�<6@���@�kQ@��@��@�L0@��@��.@� �@߷@��@޼j@�c @��@�=�@�V�@��@��@�-�@٥�@�RT@�o�@�&�@�Z@�|�@��@֗�@֗�@�;�@�ϫ@�7L@���@��'@�u�@�Xy@�2�@���@ӊ	@�@Ҩ�@�w�@�Q�@�6�@�_@��N@Ѥ@@��H@�^5@���@ϛ=@�f�@ΝI@��@�˒@�+@�;d@̴9@��;@ˊ	@ʼj@�@�/@�2a@�7L@�͟@ǒ:@ư!@�W�@�J@ū�@�_p@���@�]d@Ê�@��@���@�~�@�Xy@�(�@�Z@@�A�@�"h@��@�?}@�q@�%�@���@���@�@���@��@���@��1@�/�@���@�%F@���@��@��	@�A @��@�Z@��@�a�@�ߤ@���@�V�@�M@���@�C�@�#�@��@�S@��e@��F@��@��n@�u�@�'�@�֡@���@�4n@��@��T@��@�Mj@���@�&�@��@��	@�H�@���@��/@���@�q�@�'R@���@��H@�c@�S�@�$t@��@�ȴ@�PH@�u@���@�k�@�+�@��\@��@���@���@�e@��z@���@���@��0@���@�X@�"�@�ں@��Y@�ԕ@�&@���@�c @�7@�J@��r@��@�@�c@�F�@���@��@��]@��<@�m�@�7�@�{@��@��@���@�8�@�ی@�h
@�;�@�#:@���@��@���@�/�@�;@���@�v�@�b@��@��@�K^@�@���@��^@���@��P@�s@�9�@��@���@��;@�L�@�!�@���@���@���@�`�@�B[@��Q@�=�@�ی@�e�@�($@��0@�dZ@�33@��K@��b@�a|@�(�@��o@�X�@� i@��@�Ov@��g@���@�b�@�F@�&@�	l@�ی@��D@�A�@��H@��@��4@�Ft@�e@���@��{@�p�@���@���@���@���@�>B@�1@�
=@��@�}V@�V�@�H@�j@���@��@�l�@�C-@��;@��6@�s@���@���@�oi@�h�@��@���@���@���@�RT@��@��s@��!@��u@�c @��@��g@�c@�Q�@�F�@�8�@�2a@�@��@���@�xl@�Ft@�e@�  @���@�B�@��@��]@��'@���@�K^@��T@�c@�(�@���@�y>@�@�@�@��@'�@~��@~{�@~4@}�H@}�@|�D@|A�@{�@{��@{>�@z��@z^5@y��@y��@y@x�|@x��@x%�@w��@wZ�@v��@v��@vR�@u�9@u�@t`�@s�Q@so�@s@O@sS@rȴ@r�1@rL0@r1�@q�9@qQ�@p�@p|�@p�@o��@o)_@m�@l��@lm�@k��@k�V@k{J@kZ�@k,�@j{�@j�@i��@iN<@i�@h��@h�$@h[�@h,=@g�m@g_p@f�]@fn�@f	@e��@eF@dĜ@d�O@dg8@c�]@c�;@c�a@cdZ@b��@bJ@a��@a��@a��@aB�@a#�@`�O@`Z@_�K@_4�@^�"@^�@^��@^��@^d�@]�@](�@\��@\?�@\"h@\I�@\�@\-�@[�[@[F�@Z��@ZH�@Z{@Y�)@Yu�@YF@X�f@X�@X_@X?�@W�@Wo@V�@Vl�@V�@U�@U[W@UV@T�E@T��@T,=@S��@S(@R�R@Ri�@R$�@Q�D@Q��@Qe,@Q�@Q	l@P�@P��@Pz�@Oƨ@O>�@N�y@Nv�@NOv@Mϫ@MrG@MB�@M�@LĜ@L�u@L*�@K�@K��@K_p@K/�@K�@J��@J�b@J�+@J_�@J1�@I��@I��@Im]@IL�@I�@H��@Hj@HH@G��@G�$@GP�@G)_@G i@F�\@F0U@E�9@Ea�@E%F@D��@D��@DS�@C�A@C��@CH�@C�@C�@Bں@B�F@Bh
@BH�@B5?@A��@A�-@Azx@A�@@��@@$@?��@?Mj@>�y@>�@>�1@=�@=�t@=��@=��@=Vm@<z�@<4n@;��@;~�@:ߤ@:�@:B[@9�T@9}�@97L@9&�@8ѷ@8]d@8�@7�W@7� @7��@7@6��@6kQ@6:*@5��@5�N@5��@5|@5j@5[W@5Q�@5�@4�@4��@3�@3�@3��@3s@3l�@3&@2�L@2	@1�X@1 \@0�9@0��@0�@0C-@/�@/s@/�@.ff@.@-�@-�d@-�@-�=@-�~@-p�@-%F@,��@,S�@+� @+l�@+,�@*�@*�h@*YK@*3�@)��@(�P@(��@(Ĝ@(�$@(�z@(��@(��@(z�@(h�@(Ft@(x@'�K@'��@'Mj@&��@&_�@%��@%�7@%}�@%o @%B�@$�@$��@$w�@#�]@#��@#��@#x@#C�@"�s@"�\@"E�@" �@!��@!��@!��@!��@!c�@!#�@ �@ �j@ y>@ I�@ b@��@��@H�@�"@�@�1@d�@;�@+k@�d@��@w2@=�@�K@�Y@%�@�@��@خ@��@,�@�@��@�s@�+@�.@�@��@S&@��@�z@��@m�@N�@�@�W@�}@x@F�@)_@(@�"@�@�s@��@c @H�@E�@;�@#:@J@�#@�t@�n@��@��@Y�@��@7�@�
@�@��@iD@�@c @B[@($@��@ϫ@��@|@[W@?}@@�j@z�@Z@>B@"h@��@ݘ@��@Z�@,�@�"@�@�,@�!@��@Q@)�@	@�j@�t@rG@`B@B�@�@�@֡@��@�@`�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��vA��A��A��QA��A���A�� A��A��A���A���A��A���A��iA��vA��A���A���A���A��JA��JA���A��2A��rA���A��xA��A׮�A�~]A��AӁ�A�W�A�{A���A���A�A�iA��UA�P�A��HA�ѷAȒ�A�poAǞ�Aƫ6A���A�h�A� \A��fA��A���A���A��uA�+�A��A���A�ںA�	A�ӏA��aA�GEA��_A��A��A��A�A�T,A���A�ߤA�S[A�cTA�cA��A�^5A��A��"A�,A� �A�$�A���A�g�A��XA���A��0A�:^A��A��A���A���A�
	A�qvA�)�A��A���A���A��A}��A{@Ay/�Axp;Aw��At�\Aq��Aly>AeɆAbqA_�A]ƨAZ�AVU�AP>�AL,�AJ|�AHS�AD��AB�;A@ZA>ݘA=�VA<�$A<n�A<;�A<�A:dZA6�A4�nA4#�A3.IA1�MA/"hA-�+A-�A-ںA.&�A-u%A+e�A*�SA)O�A'+A'��A'�7A'2aA&+�A$�MA#�>A#��A#l"A#>BA#1�A"��A"1'A!خA!h
A �FA (�A��A��A�fAc�AbA�MA�A�AS�A��A�HA�9A�~A�A��A�4ASAJ�A�A�AoiA?AA A&�A�$A��A	Ah
A�A�9AU�A�"A��A=qA�A�jA�"A�AQ�A��A[WA+A��A҉A��Av`A2�A�]A��Ae�A��A�'A�YA&�A�EA�AMA
��A
r�A
�A	�DA	�A|�A��AMA&�AϫA}�A[�A7A�uA��A�MA�1Av�A�A�A��A7A oiA Dg@��:@��VA �@��@��@��@�c@��[@�@@��f@���@��'@�H�@��@��8@��@��@�6z@�@��r@�2a@�	@�	@��@��@�}�@�.I@�!@��@��@��@��p@� �@�q@�Q�@��@�a|@�6@��Q@�=�@�z@��@�<6@���@�kQ@��@��@�L0@��@��.@� �@߷@��@޼j@�c @��@�=�@�V�@��@��@�-�@٥�@�RT@�o�@�&�@�Z@�|�@��@֗�@֗�@�;�@�ϫ@�7L@���@��'@�u�@�Xy@�2�@���@ӊ	@�@Ҩ�@�w�@�Q�@�6�@�_@��N@Ѥ@@��H@�^5@���@ϛ=@�f�@ΝI@��@�˒@�+@�;d@̴9@��;@ˊ	@ʼj@�@�/@�2a@�7L@�͟@ǒ:@ư!@�W�@�J@ū�@�_p@���@�]d@Ê�@��@���@�~�@�Xy@�(�@�Z@@�A�@�"h@��@�?}@�q@�%�@���@���@�@���@��@���@��1@�/�@���@�%F@���@��@��	@�A @��@�Z@��@�a�@�ߤ@���@�V�@�M@���@�C�@�#�@��@�S@��e@��F@��@��n@�u�@�'�@�֡@���@�4n@��@��T@��@�Mj@���@�&�@��@��	@�H�@���@��/@���@�q�@�'R@���@��H@�c@�S�@�$t@��@�ȴ@�PH@�u@���@�k�@�+�@��\@��@���@���@�e@��z@���@���@��0@���@�X@�"�@�ں@��Y@�ԕ@�&@���@�c @�7@�J@��r@��@�@�c@�F�@���@��@��]@��<@�m�@�7�@�{@��@��@���@�8�@�ی@�h
@�;�@�#:@���@��@���@�/�@�;@���@�v�@�b@��@��@�K^@�@���@��^@���@��P@�s@�9�@��@���@��;@�L�@�!�@���@���@���@�`�@�B[@��Q@�=�@�ی@�e�@�($@��0@�dZ@�33@��K@��b@�a|@�(�@��o@�X�@� i@��@�Ov@��g@���@�b�@�F@�&@�	l@�ی@��D@�A�@��H@��@��4@�Ft@�e@���@��{@�p�@���@���@���@���@�>B@�1@�
=@��@�}V@�V�@�H@�j@���@��@�l�@�C-@��;@��6@�s@���@���@�oi@�h�@��@���@���@���@�RT@��@��s@��!@��u@�c @��@��g@�c@�Q�@�F�@�8�@�2a@�@��@���@�xl@�Ft@�e@�  @���@�B�@��@��]@��'@���@�K^@��T@�c@�(�@���@�y>@�@�@�@��@'�@~��@~{�@~4@}�H@}�@|�D@|A�@{�@{��@{>�@z��@z^5@y��@y��@y@x�|@x��@x%�@w��@wZ�@v��@v��@vR�@u�9@u�@t`�@s�Q@so�@s@O@sS@rȴ@r�1@rL0@r1�@q�9@qQ�@p�@p|�@p�@o��@o)_@m�@l��@lm�@k��@k�V@k{J@kZ�@k,�@j{�@j�@i��@iN<@i�@h��@h�$@h[�@h,=@g�m@g_p@f�]@fn�@f	@e��@eF@dĜ@d�O@dg8@c�]@c�;@c�a@cdZ@b��@bJ@a��@a��@a��@aB�@a#�@`�O@`Z@_�K@_4�@^�"@^�@^��@^��@^d�@]�@](�@\��@\?�@\"h@\I�@\�@\-�@[�[@[F�@Z��@ZH�@Z{@Y�)@Yu�@YF@X�f@X�@X_@X?�@W�@Wo@V�@Vl�@V�@U�@U[W@UV@T�E@T��@T,=@S��@S(@R�R@Ri�@R$�@Q�D@Q��@Qe,@Q�@Q	l@P�@P��@Pz�@Oƨ@O>�@N�y@Nv�@NOv@Mϫ@MrG@MB�@M�@LĜ@L�u@L*�@K�@K��@K_p@K/�@K�@J��@J�b@J�+@J_�@J1�@I��@I��@Im]@IL�@I�@H��@Hj@HH@G��@G�$@GP�@G)_@G i@F�\@F0U@E�9@Ea�@E%F@D��@D��@DS�@C�A@C��@CH�@C�@C�@Bں@B�F@Bh
@BH�@B5?@A��@A�-@Azx@A�@@��@@$@?��@?Mj@>�y@>�@>�1@=�@=�t@=��@=��@=Vm@<z�@<4n@;��@;~�@:ߤ@:�@:B[@9�T@9}�@97L@9&�@8ѷ@8]d@8�@7�W@7� @7��@7@6��@6kQ@6:*@5��@5�N@5��@5|@5j@5[W@5Q�@5�@4�@4��@3�@3�@3��@3s@3l�@3&@2�L@2	@1�X@1 \@0�9@0��@0�@0C-@/�@/s@/�@.ff@.@-�@-�d@-�@-�=@-�~@-p�@-%F@,��@,S�@+� @+l�@+,�@*�@*�h@*YK@*3�@)��@(�P@(��@(Ĝ@(�$@(�z@(��@(��@(z�@(h�@(Ft@(x@'�K@'��@'Mj@&��@&_�@%��@%�7@%}�@%o @%B�@$�@$��@$w�@#�]@#��@#��@#x@#C�@"�s@"�\@"E�@" �@!��@!��@!��@!��@!c�@!#�@ �@ �j@ y>@ I�@ b@��@��@H�@�"@�@�1@d�@;�@+k@�d@��@w2@=�@�K@�Y@%�@�@��@خ@��@,�@�@��@�s@�+@�.@�@��@S&@��@�z@��@m�@N�@�@�W@�}@x@F�@)_@(@�"@�@�s@��@c @H�@E�@;�@#:@J@�#@�t@�n@��@��@Y�@��@7�@�
@�@��@iD@�@c @B[@($@��@ϫ@��@|@[W@?}@@�j@z�@Z@>B@"h@��@ݘ@��@Z�@,�@�"@�@�,@�!@��@Q@)�@	@�j@�t@rG@`B@B�@�@�@֡@��@�@`�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	oiB	o5B	o�B	oOB	o B	o B	oB	o B	o B	o B	n�B	n�B	n�B	n}B	nIB	n�B	n�B	n�B	n�B	o5B	o5B	o5B	o B	o5B	oiB	o�B	oB	s�B	��B	��B	��B	��B	�B	��B	�-B	�B	�]B	�B	�B	��B	��B	��B	��B
�B
<jB
`�B
��B
�]B_B1'BjKBB�FB�BB�rB��B�*B��B׍B�[B�[B�YB�$B�B��B�BB1B�B@B�B�B
�B�BɆB�RB��B��B��B��B�BuZBbNBSBC�B2|B$tB�B�BaB
�QB
�B
��B
��B
:B
;B
DB	��B	�zB	�9B	�B	�B	��B	��B	�B	h>B	X�B	H�B	>BB	2�B	�B	�B�B�ZB�B	($B	2aB	1'B	,"B	-B	0�B	2aB	3�B	2�B	3hB	7fB	5�B	3hB	.�B	)DB	-�B	/5B	/iB	88B	iyB	tnB	xB	r�B	u�B	{�B	��B	�B	��B	�mB	�5B	�B	��B	��B	��B	ƎB	یB	�)B	��B	�B	�B	�B	�KB	�B	��B	� B	�B	��B	��B
-B
%B
B

=B
�B
PB
B
�B
�B
dB
�B
;B
 �B
B
 'B
%,B
&�B
)*B
)�B
)yB
%�B
'B
,�B
,qB
/�B
1B
1[B
4B
6�B
6zB
4�B
8�B
<�B
="B
>BB
?B
>�B
?HB
@�B
@�B
AUB
?�B
>�B
=�B
>BB
AoB
@�B
@�B
?�B
>wB
=VB
<�B
;�B
;JB
:DB
7�B
3MB
0�B
3B
1�B
1vB
1�B
0�B
-�B
)�B
%B
!bB
�B
~B
B
B
$B
�B
�B
(B
�B
�B
�B
�B
_B
KB
�B
B
�B
�B
\B
<B
�B
�B
�B
B
JB
dB
)B
	�B
	B
�B
�B
�B
�B
�B
?B
B
�B
mB
�B
?B
�B
mB
MB
aB
MB
�B
�B
B
_B
�B
B
�B
�B
�B
YB
tB
SB
3B
�B
�B
[B
�B
 �B
�B	��B	��B	��B	��B	��B	��B	��B	�}B	�"B	��B	�>B	�8B	�rB	��B	��B	�B	�B	�*B	�B	�xB	��B	��B	��B	��B	�B	��B	�}B	�cB	��B	��B
 B
;B	��B	��B	��B	�<B	��B	�B	�B	�8B	��B	�B	��B	��B	��B	��B	�B	�*B	�(B	�<B	�dB	�0B	��B	��B	�qB	�VB	�wB	��B	�B	��B	�HB	��B	��B
 �B
�B
�B
	�B
	�B
�B
B
B
B
�B
B
B
?B
�B
�B
�B
�B
�B
	�B
	B
�B
	lB
	B
�B
fB
�B
�B
�B
B
B
B
�B
B
1B
1B
1B
	B
�B

#B

�B

�B
DB
DB
^B
)B
)B
�B
�B
�B
�B
0B
�B
JB
B
�B
\B
bB
4B
4B
B
�B
�B
bB
HB
B
HB
�B
B
 B
�B
�B
B
�B
hB
�B
�B
hB
�B
�B
@B
�B
uB
uB
�B
�B
,B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
aB
{B
�B
{B
{B
{B
aB
aB
aB
,B
�B
�B
B
B
mB
�B
�B

B
$B
�B
?B

B
�B
EB
�B
�B
�B
�B
�B
#B
WB
qB
qB
)B
)B
�B
�B
=B
�B
�B
�B
�B
qB
�B
�B
)B
xB
�B
B
xB
�B
�B
/B
dB
B
5B
B
VB
VB
 B
 \B
 �B
 �B
 �B
!B
!�B
!�B
"B
"hB
 �B
!�B
"4B
!�B
"�B
#�B
#�B
#�B
$B
$ZB
$�B
$�B
$�B
"4B
!�B
"NB
# B
$&B
%�B
)B
+�B
+�B
+B
*�B
*�B
,"B
,�B
,=B
,�B
-�B
-�B
-�B
-�B
-�B
./B
.�B
/5B
/5B
/iB
0B
0oB
0�B
1[B
1�B
1vB
1vB
1[B
1[B
1�B
1�B
1�B
1�B
2-B
2GB
2�B
3�B
3�B
4B
3�B
3�B
49B
4�B
4�B
4�B
5B
5ZB
5ZB
5�B
6zB
6�B
6�B
7LB
7�B
7�B
8lB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
:B
:*B
:*B
:*B
:xB
:�B
:�B
:�B
;JB
;�B
;�B
<B
<6B
<jB
<jB
<jB
<�B
<�B
<�B
=VB
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?}B
?cB
?�B
?�B
?�B
@4B
@ B
@ B
@ B
@4B
@OB
@iB
@�B
@iB
@�B
@�B
A;B
AUB
A�B
A�B
BuB
B�B
B�B
B�B
B�B
B�B
C-B
C{B
C�B
C�B
C�B
C�B
DB
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
F?B
EmB
EmB
E�B
F?B
F�B
HB
HB
G�B
G�B
HfB
H�B
I7B
I7B
IRB
I�B
I�B
I�B
I�B
IlB
I�B
J=B
KB
K�B
K�B
K�B
K^B
K�B
K�B
LJB
L~B
L�B
L�B
MB
MB
M�B
NVB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
PB
P.B
PB
P}B
P}B
P�B
P�B
Q B
QB
Q4B
QhB
Q�B
Q�B
Q�B
R B
RTB
RTB
RTB
RTB
R�B
R�B
SB
S&B
S@B
S[B
S�B
S�B
S�B
T,B
TFB
T�B
T�B
T�B
U2B
U�B
U�B
VB
V9B
VSB
VmB
V�B
W$B
WYB
W�B
W�B
W�B
W�B
X+B
X_B
X_B
X_B
X�B
X�B
X�B
YB
YKB
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
[	B
[#B
Z�B
[=B
[�B
[�B
\)B
\)B
\�B
\�B
]B
]/B
]�B
]�B
]�B
^B
^5B
^�B
^�B
^�B
^�B
_VB
_�B
_�B
_�B
`B
`'B
`BB
`vB
`vB
`vB
`\B
`�B
`�B
`�B
a|B
a�B
a�B
a�B
a�B
a�B
b4B
b�B
b�B
c:B
c�B
c�B
c�B
c�B
dZB
dtB
d�B
eFB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
f�B
f�B
gB
gB
g8B
gmB
g�B
g�B
g8B
g�B
g�B
g�B
g�B
h
B
g�B
g�B
g�B
g�B
g�B
h
B
g�B
g�B
g�B
g�B
h>B
hsB
h�B
h�B
iB
h�B
iDB
jKB
jKB
jeB
kB
kB
kB
k�B
k�B
l=B
lqB
l�B
mB
l�B
l�B
l�B
mCB
mCB
mwB
m�B
m�B
m�B
m�B
nB
n/B
ncB
n�B
n�B
n�B
o B
o B
oB
oB
o�B
o�B
o�B
o�B
o�B
p;B
p�B
p�B
p�B
p�B
qB
rB
raB
raB
r�B
r�B
r�B
r�B
r�B
s3B
s�B
tB
tB
t9B
tnB
t�B
t�B
t�B
u?B
u?B
uZB
uZB
utB
utB
utB
u�B
u�B
u�B
u�B
u�B
vB
vB
vzB
v�B
v�B
v�B
v�B
v�B
wfB
w�B
x8B
xB
x8B
xRB
y	B
y>B
yrB
yrB
y�B
y�B
zB
zB
zDB
z^B
z�B
z�B
{0B
{dB
{B
{B
{�B
{�B
|B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}<B
}�B
}�B
}�B
}�B
}�B
~(B
~BB
~BB
~wB
~wB
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	oiB	o5B	o�B	oOB	o B	o B	oB	o B	o B	o B	n�B	n�B	n�B	n}B	nIB	n�B	n�B	n�B	n�B	o5B	o5B	o5B	o B	o5B	oiB	o�B	oB	s�B	��B	��B	��B	��B	�B	��B	�-B	�B	�]B	�B	�B	��B	��B	��B	��B
�B
<jB
`�B
��B
�]B_B1'BjKBB�FB�BB�rB��B�*B��B׍B�[B�[B�YB�$B�B��B�BB1B�B@B�B�B
�B�BɆB�RB��B��B��B��B�BuZBbNBSBC�B2|B$tB�B�BaB
�QB
�B
��B
��B
:B
;B
DB	��B	�zB	�9B	�B	�B	��B	��B	�B	h>B	X�B	H�B	>BB	2�B	�B	�B�B�ZB�B	($B	2aB	1'B	,"B	-B	0�B	2aB	3�B	2�B	3hB	7fB	5�B	3hB	.�B	)DB	-�B	/5B	/iB	88B	iyB	tnB	xB	r�B	u�B	{�B	��B	�B	��B	�mB	�5B	�B	��B	��B	��B	ƎB	یB	�)B	��B	�B	�B	�B	�KB	�B	��B	� B	�B	��B	��B
-B
%B
B

=B
�B
PB
B
�B
�B
dB
�B
;B
 �B
B
 'B
%,B
&�B
)*B
)�B
)yB
%�B
'B
,�B
,qB
/�B
1B
1[B
4B
6�B
6zB
4�B
8�B
<�B
="B
>BB
?B
>�B
?HB
@�B
@�B
AUB
?�B
>�B
=�B
>BB
AoB
@�B
@�B
?�B
>wB
=VB
<�B
;�B
;JB
:DB
7�B
3MB
0�B
3B
1�B
1vB
1�B
0�B
-�B
)�B
%B
!bB
�B
~B
B
B
$B
�B
�B
(B
�B
�B
�B
�B
_B
KB
�B
B
�B
�B
\B
<B
�B
�B
�B
B
JB
dB
)B
	�B
	B
�B
�B
�B
�B
�B
?B
B
�B
mB
�B
?B
�B
mB
MB
aB
MB
�B
�B
B
_B
�B
B
�B
�B
�B
YB
tB
SB
3B
�B
�B
[B
�B
 �B
�B	��B	��B	��B	��B	��B	��B	��B	�}B	�"B	��B	�>B	�8B	�rB	��B	��B	�B	�B	�*B	�B	�xB	��B	��B	��B	��B	�B	��B	�}B	�cB	��B	��B
 B
;B	��B	��B	��B	�<B	��B	�B	�B	�8B	��B	�B	��B	��B	��B	��B	�B	�*B	�(B	�<B	�dB	�0B	��B	��B	�qB	�VB	�wB	��B	�B	��B	�HB	��B	��B
 �B
�B
�B
	�B
	�B
�B
B
B
B
�B
B
B
?B
�B
�B
�B
�B
�B
	�B
	B
�B
	lB
	B
�B
fB
�B
�B
�B
B
B
B
�B
B
1B
1B
1B
	B
�B

#B

�B

�B
DB
DB
^B
)B
)B
�B
�B
�B
�B
0B
�B
JB
B
�B
\B
bB
4B
4B
B
�B
�B
bB
HB
B
HB
�B
B
 B
�B
�B
B
�B
hB
�B
�B
hB
�B
�B
@B
�B
uB
uB
�B
�B
,B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
aB
{B
�B
{B
{B
{B
aB
aB
aB
,B
�B
�B
B
B
mB
�B
�B

B
$B
�B
?B

B
�B
EB
�B
�B
�B
�B
�B
#B
WB
qB
qB
)B
)B
�B
�B
=B
�B
�B
�B
�B
qB
�B
�B
)B
xB
�B
B
xB
�B
�B
/B
dB
B
5B
B
VB
VB
 B
 \B
 �B
 �B
 �B
!B
!�B
!�B
"B
"hB
 �B
!�B
"4B
!�B
"�B
#�B
#�B
#�B
$B
$ZB
$�B
$�B
$�B
"4B
!�B
"NB
# B
$&B
%�B
)B
+�B
+�B
+B
*�B
*�B
,"B
,�B
,=B
,�B
-�B
-�B
-�B
-�B
-�B
./B
.�B
/5B
/5B
/iB
0B
0oB
0�B
1[B
1�B
1vB
1vB
1[B
1[B
1�B
1�B
1�B
1�B
2-B
2GB
2�B
3�B
3�B
4B
3�B
3�B
49B
4�B
4�B
4�B
5B
5ZB
5ZB
5�B
6zB
6�B
6�B
7LB
7�B
7�B
8lB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
:B
:*B
:*B
:*B
:xB
:�B
:�B
:�B
;JB
;�B
;�B
<B
<6B
<jB
<jB
<jB
<�B
<�B
<�B
=VB
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?}B
?cB
?�B
?�B
?�B
@4B
@ B
@ B
@ B
@4B
@OB
@iB
@�B
@iB
@�B
@�B
A;B
AUB
A�B
A�B
BuB
B�B
B�B
B�B
B�B
B�B
C-B
C{B
C�B
C�B
C�B
C�B
DB
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
F?B
EmB
EmB
E�B
F?B
F�B
HB
HB
G�B
G�B
HfB
H�B
I7B
I7B
IRB
I�B
I�B
I�B
I�B
IlB
I�B
J=B
KB
K�B
K�B
K�B
K^B
K�B
K�B
LJB
L~B
L�B
L�B
MB
MB
M�B
NVB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
PB
P.B
PB
P}B
P}B
P�B
P�B
Q B
QB
Q4B
QhB
Q�B
Q�B
Q�B
R B
RTB
RTB
RTB
RTB
R�B
R�B
SB
S&B
S@B
S[B
S�B
S�B
S�B
T,B
TFB
T�B
T�B
T�B
U2B
U�B
U�B
VB
V9B
VSB
VmB
V�B
W$B
WYB
W�B
W�B
W�B
W�B
X+B
X_B
X_B
X_B
X�B
X�B
X�B
YB
YKB
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
[	B
[#B
Z�B
[=B
[�B
[�B
\)B
\)B
\�B
\�B
]B
]/B
]�B
]�B
]�B
^B
^5B
^�B
^�B
^�B
^�B
_VB
_�B
_�B
_�B
`B
`'B
`BB
`vB
`vB
`vB
`\B
`�B
`�B
`�B
a|B
a�B
a�B
a�B
a�B
a�B
b4B
b�B
b�B
c:B
c�B
c�B
c�B
c�B
dZB
dtB
d�B
eFB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
f�B
f�B
gB
gB
g8B
gmB
g�B
g�B
g8B
g�B
g�B
g�B
g�B
h
B
g�B
g�B
g�B
g�B
g�B
h
B
g�B
g�B
g�B
g�B
h>B
hsB
h�B
h�B
iB
h�B
iDB
jKB
jKB
jeB
kB
kB
kB
k�B
k�B
l=B
lqB
l�B
mB
l�B
l�B
l�B
mCB
mCB
mwB
m�B
m�B
m�B
m�B
nB
n/B
ncB
n�B
n�B
n�B
o B
o B
oB
oB
o�B
o�B
o�B
o�B
o�B
p;B
p�B
p�B
p�B
p�B
qB
rB
raB
raB
r�B
r�B
r�B
r�B
r�B
s3B
s�B
tB
tB
t9B
tnB
t�B
t�B
t�B
u?B
u?B
uZB
uZB
utB
utB
utB
u�B
u�B
u�B
u�B
u�B
vB
vB
vzB
v�B
v�B
v�B
v�B
v�B
wfB
w�B
x8B
xB
x8B
xRB
y	B
y>B
yrB
yrB
y�B
y�B
zB
zB
zDB
z^B
z�B
z�B
{0B
{dB
{B
{B
{�B
{�B
|B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}<B
}�B
}�B
}�B
}�B
}�B
~(B
~BB
~BB
~wB
~wB
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104847  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172500  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172500  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172500                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022508  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022508  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                