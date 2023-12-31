CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-03-21T17:02:42Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ˴   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ϩ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ߨ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �T        �TArgo profile    3.1 1.2 19500101000000  20180321170242  20181025093512  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4741                            2C  D   NAVIS_A                         0185                            052512                          863 @�U�v�1   @�UO�	`@;A$�/�c���`A�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�3D�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
=C
=C
=C
=C
=C

=C
=C
=C
=C
=C
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
=C2
=C4
=C6
=C8
=C:
=C<
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
=CZ
=C\
=C^
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
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D^�)D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�{D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�D{D��HD��HD�{D�*�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��\A��DA��\A���A���A���A���A��uA��uA��hA��PA��A��A��A��A�z�A�t�A�jA�hsA�jA�ffA�dZA�bNA�dZA�dZA�dZA�`BA�`BA�^5A�\)A�^5A�^5A�`BA�`BA�bNA�dZA�dZA�dZA�dZA�bNA�\)A�S�A�=qA�{A���A���A��PA�n�A�=qA��hA��
A��-A�v�A�l�A�E�A�"�A�VA��A��HA���A��
A�?}A���A��
A�hsA�JA�XA�/A�z�A��+A�z�A�^5A���A��A��A�ĜA��A�A��#A�G�A��jA��FA�z�A�oA�A��A��PA��A�ffA��A�G�A���A��A�K�A��RA���A�\)A�^5A}�A|n�A{Ax�Au�-Ar�yAp�An9XAjE�Ah�Ag�;Af�!Ae��AdbAb{AaS�A`�DA_oA]l�A\�DA[t�AY�
AX��AW�AW?}AV�AU|�AT�ATffAR�/AQ��AQ`BAP�`APjAP(�AO�AO�PAO+ANZAM|�AL�+AKXAJA�AI�FAIG�AI�AHbAGhsAF-AD�+AC�AB��AA��AAG�A@z�A@bA?��A?G�A>��A>Q�A=p�A<�HA<�yA<E�A;t�A:��A:��A:{A9&�A8v�A7�hA6�HA6 �A5�#A5�A4�A3�PA3A2z�A2$�A1��A1hsA1%A0��A0�A0~�A0VA0{A/hsA/;dA.�A.��A.~�A-�FA,E�A+�hA+oA*5?A*{A)�TA)p�A(VA'�A&1A$-A#�A#`BA"�HA"ZA!�A �RA�A�DA��A�RA��AQ�AVAE�A�
AS�A�9A�AK�A��AM�A�
AK�AbNA�AA33A�9AA�A�A5?Ap�A
Q�A	��A��AjAO�AȴAVA5?A��A7LA�/A��AVA�^A+A ��@�33@��@�&�@�Q�@���@���@���@�v�@��@�1'@�Z@�j@��
@�{@�dZ@�p�@��@⟾@���@��y@���@��/@۾w@���@�?}@�1@�V@Ձ@�r�@ӝ�@��y@�@мj@��;@Ͼw@��@́@̴9@˕�@��@ǝ�@�@�5?@�r�@��;@å�@�;d@�$�@��@���@�`B@�Ĝ@�
=@��@��/@��u@��P@��@�O�@��/@�I�@�1@�dZ@�n�@���@��`@�j@�b@��@��@�J@�x�@���@�A�@�dZ@��H@�ȴ@��R@���@�E�@�{@��T@�x�@��`@��9@�1@��w@��@�@��^@�b@�ȴ@�@�V@�j@�I�@�9X@��@�+@���@���@���@�^5@��@�@��h@�7L@�Ĝ@�r�@�|�@��!@�E�@�@��h@��j@�r�@��@�dZ@��@��@�@��y@���@�-@��@���@��h@���@�t�@��w@���@�l�@�K�@�+@��@���@��H@���@�ff@��@��@���@�?}@��@���@��@�x�@���@��-@�I�@�@�~�@�n�@�V@�n�@�{@���@���@��h@��/@� �@��F@��F@��@���@�|�@�t�@�
=@��!@�5?@���@�7L@�r�@�1@�b@��@��@���@�33@�{@���@��h@��@��@�j@��@��
@���@��\@�x�@��7@��7@��7@�x�@���@��D@�r�@���@�t�@�o@�ȴ@���@��\@�v�@�=q@�@���@��@���@���@�Z@�1'@� �@�;@l�@K�@\)@|�@l�@;d@
=@~�@~�R@~��@}p�@|��@|Z@{��@{�F@{�F@|�@|j@|Z@|Z@|(�@|1@{ƨ@{dZ@{@y��@x��@w\)@v@u��@u�h@u�T@u�h@up�@up�@u/@t��@tj@tZ@s�
@s�
@s�m@s�
@s�F@sdZ@sC�@r�H@q��@q�#@q��@q�7@p��@p�9@p�9@p�9@p�9@p�u@pr�@pbN@o��@o�P@p  @pbN@pA�@p  @n��@n�+@n��@o+@n�y@nȴ@n��@n�+@nff@n5?@n5?@n{@m�h@m�@l�/@lz�@lj@lI�@l(�@l1@k��@k@jM�@j�@i��@i��@ihs@i&�@i�@i%@i�@h�`@h�`@h�9@hA�@g�;@gK�@f��@e�h@e?}@e/@d�/@d�@d�j@d��@d�/@d�D@d1@c�F@cdZ@cC�@c33@c"�@co@b�H@b��@b~�@bn�@bM�@a��@a��@a�7@a&�@`��@`�9@`�@`A�@` �@_�@_��@_��@_\)@^�y@^ff@^E�@]�@]�h@]/@]?}@]?}@]/@\��@\Z@\I�@\1@[�F@[��@[�@[t�@[t�@[S�@["�@Z�@Y��@Yx�@Yhs@YX@YG�@YG�@Y�@X�`@X�9@X�u@XbN@XA�@X �@W��@V�R@V{@U`B@U/@T�/@T��@Tj@T(�@T1@S��@S�
@SS�@So@R��@R~�@Qx�@QX@Q�@P��@P�`@P�u@Pr�@PQ�@O��@N�y@N�@N�@Nff@M@MO�@L�@L�D@Lj@Kƨ@Kt�@KC�@KC�@K33@K"�@J��@J~�@J^5@J�@Ihs@G��@G�@F��@FE�@F@E�@EO�@D��@D��@D�j@D��@DZ@DI�@D1@C��@CdZ@C"�@Co@C@C@B�@B��@B��@B~�@Bn�@B^5@A�@Ahs@A%@@�`@@Ĝ@@�u@@A�@?��@?
=@>�@>�R@>V@=@=�@<��@<��@<z�@<z�@<I�@<�@;��@;�F@;C�@:��@:^5@:=q@:�@:J@9��@9�@9��@9�^@9x�@8��@8bN@8Q�@8A�@8 �@8  @7��@7�P@6�R@6$�@6@5�T@5p�@5V@4�/@4�@4z�@4j@4j@4j@41@3�@3"�@2�H@2��@2n�@2n�@2=q@1��@1x�@1hs@1&�@0��@0Q�@0  @/�w@/\)@.��@.�@.�+@.{@-�-@-��@-�@-/@,�D@,9X@,(�@,�@,�@,1@,1@,1@+�
@+t�@+C�@+"�@*�@*��@*=q@)��@)�^@)��@)��@)x�@)&�@)�@(��@(�9@(�u@(bN@(b@(  @'��@'��@'�P@'|�@'K�@&��@&�R@&��@&�+@&$�@%�T@%��@%�@%O�@%�@$�@$�j@$��@$j@$(�@#��@#�m@#ƨ@#�F@#��@#�@#C�@"�@"��@"��@"�\@"M�@!��@!�7@!hs@!G�@!&�@ �`@ ��@ �u@ bN@ A�@  �@�@|�@K�@
=@�@�R@��@��@��@V@@@�h@`B@?}@�@V@V@��@�/@z�@9X@�@1@��@�m@��@S�@o@�H@��@��@~�@n�@M�@=q@��@7L@��@�@bN@A�@A�@ �@�@�@�@�@|�@K�@�@ȴ@��@��@��@v�@E�@��@�h@`B@�/@z�@(�@��@�
@��@t�@dZ@dZ@33@�@��@~�@n�@^5@-@-@�@��@�#@��@�^@�7@G�@G�@&�@%@��@�9@�@r�@bN@1'@ �@�@�;@�w@��@l�@+@��@�@ȴ@��@��@v�@V@5?@$�@{@@�T@�-@�@�@`B@?}@��@��@��@�j@�D@I�@�@��@�
@ƨ@��@�@t�@33@o@
�H@
��@
�\@
~�@
^5@
=q@
-@
J@	�@	��@	�7@	x�@	7L@	�@	%@��@�`@�9@�u@�@bN@1'@  @��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��\A��DA��\A���A���A���A���A��uA��uA��hA��PA��A��A��A��A�z�A�t�A�jA�hsA�jA�ffA�dZA�bNA�dZA�dZA�dZA�`BA�`BA�^5A�\)A�^5A�^5A�`BA�`BA�bNA�dZA�dZA�dZA�dZA�bNA�\)A�S�A�=qA�{A���A���A��PA�n�A�=qA��hA��
A��-A�v�A�l�A�E�A�"�A�VA��A��HA���A��
A�?}A���A��
A�hsA�JA�XA�/A�z�A��+A�z�A�^5A���A��A��A�ĜA��A�A��#A�G�A��jA��FA�z�A�oA�A��A��PA��A�ffA��A�G�A���A��A�K�A��RA���A�\)A�^5A}�A|n�A{Ax�Au�-Ar�yAp�An9XAjE�Ah�Ag�;Af�!Ae��AdbAb{AaS�A`�DA_oA]l�A\�DA[t�AY�
AX��AW�AW?}AV�AU|�AT�ATffAR�/AQ��AQ`BAP�`APjAP(�AO�AO�PAO+ANZAM|�AL�+AKXAJA�AI�FAIG�AI�AHbAGhsAF-AD�+AC�AB��AA��AAG�A@z�A@bA?��A?G�A>��A>Q�A=p�A<�HA<�yA<E�A;t�A:��A:��A:{A9&�A8v�A7�hA6�HA6 �A5�#A5�A4�A3�PA3A2z�A2$�A1��A1hsA1%A0��A0�A0~�A0VA0{A/hsA/;dA.�A.��A.~�A-�FA,E�A+�hA+oA*5?A*{A)�TA)p�A(VA'�A&1A$-A#�A#`BA"�HA"ZA!�A �RA�A�DA��A�RA��AQ�AVAE�A�
AS�A�9A�AK�A��AM�A�
AK�AbNA�AA33A�9AA�A�A5?Ap�A
Q�A	��A��AjAO�AȴAVA5?A��A7LA�/A��AVA�^A+A ��@�33@��@�&�@�Q�@���@���@���@�v�@��@�1'@�Z@�j@��
@�{@�dZ@�p�@��@⟾@���@��y@���@��/@۾w@���@�?}@�1@�V@Ձ@�r�@ӝ�@��y@�@мj@��;@Ͼw@��@́@̴9@˕�@��@ǝ�@�@�5?@�r�@��;@å�@�;d@�$�@��@���@�`B@�Ĝ@�
=@��@��/@��u@��P@��@�O�@��/@�I�@�1@�dZ@�n�@���@��`@�j@�b@��@��@�J@�x�@���@�A�@�dZ@��H@�ȴ@��R@���@�E�@�{@��T@�x�@��`@��9@�1@��w@��@�@��^@�b@�ȴ@�@�V@�j@�I�@�9X@��@�+@���@���@���@�^5@��@�@��h@�7L@�Ĝ@�r�@�|�@��!@�E�@�@��h@��j@�r�@��@�dZ@��@��@�@��y@���@�-@��@���@��h@���@�t�@��w@���@�l�@�K�@�+@��@���@��H@���@�ff@��@��@���@�?}@��@���@��@�x�@���@��-@�I�@�@�~�@�n�@�V@�n�@�{@���@���@��h@��/@� �@��F@��F@��@���@�|�@�t�@�
=@��!@�5?@���@�7L@�r�@�1@�b@��@��@���@�33@�{@���@��h@��@��@�j@��@��
@���@��\@�x�@��7@��7@��7@�x�@���@��D@�r�@���@�t�@�o@�ȴ@���@��\@�v�@�=q@�@���@��@���@���@�Z@�1'@� �@�;@l�@K�@\)@|�@l�@;d@
=@~�@~�R@~��@}p�@|��@|Z@{��@{�F@{�F@|�@|j@|Z@|Z@|(�@|1@{ƨ@{dZ@{@y��@x��@w\)@v@u��@u�h@u�T@u�h@up�@up�@u/@t��@tj@tZ@s�
@s�
@s�m@s�
@s�F@sdZ@sC�@r�H@q��@q�#@q��@q�7@p��@p�9@p�9@p�9@p�9@p�u@pr�@pbN@o��@o�P@p  @pbN@pA�@p  @n��@n�+@n��@o+@n�y@nȴ@n��@n�+@nff@n5?@n5?@n{@m�h@m�@l�/@lz�@lj@lI�@l(�@l1@k��@k@jM�@j�@i��@i��@ihs@i&�@i�@i%@i�@h�`@h�`@h�9@hA�@g�;@gK�@f��@e�h@e?}@e/@d�/@d�@d�j@d��@d�/@d�D@d1@c�F@cdZ@cC�@c33@c"�@co@b�H@b��@b~�@bn�@bM�@a��@a��@a�7@a&�@`��@`�9@`�@`A�@` �@_�@_��@_��@_\)@^�y@^ff@^E�@]�@]�h@]/@]?}@]?}@]/@\��@\Z@\I�@\1@[�F@[��@[�@[t�@[t�@[S�@["�@Z�@Y��@Yx�@Yhs@YX@YG�@YG�@Y�@X�`@X�9@X�u@XbN@XA�@X �@W��@V�R@V{@U`B@U/@T�/@T��@Tj@T(�@T1@S��@S�
@SS�@So@R��@R~�@Qx�@QX@Q�@P��@P�`@P�u@Pr�@PQ�@O��@N�y@N�@N�@Nff@M@MO�@L�@L�D@Lj@Kƨ@Kt�@KC�@KC�@K33@K"�@J��@J~�@J^5@J�@Ihs@G��@G�@F��@FE�@F@E�@EO�@D��@D��@D�j@D��@DZ@DI�@D1@C��@CdZ@C"�@Co@C@C@B�@B��@B��@B~�@Bn�@B^5@A�@Ahs@A%@@�`@@Ĝ@@�u@@A�@?��@?
=@>�@>�R@>V@=@=�@<��@<��@<z�@<z�@<I�@<�@;��@;�F@;C�@:��@:^5@:=q@:�@:J@9��@9�@9��@9�^@9x�@8��@8bN@8Q�@8A�@8 �@8  @7��@7�P@6�R@6$�@6@5�T@5p�@5V@4�/@4�@4z�@4j@4j@4j@41@3�@3"�@2�H@2��@2n�@2n�@2=q@1��@1x�@1hs@1&�@0��@0Q�@0  @/�w@/\)@.��@.�@.�+@.{@-�-@-��@-�@-/@,�D@,9X@,(�@,�@,�@,1@,1@,1@+�
@+t�@+C�@+"�@*�@*��@*=q@)��@)�^@)��@)��@)x�@)&�@)�@(��@(�9@(�u@(bN@(b@(  @'��@'��@'�P@'|�@'K�@&��@&�R@&��@&�+@&$�@%�T@%��@%�@%O�@%�@$�@$�j@$��@$j@$(�@#��@#�m@#ƨ@#�F@#��@#�@#C�@"�@"��@"��@"�\@"M�@!��@!�7@!hs@!G�@!&�@ �`@ ��@ �u@ bN@ A�@  �@�@|�@K�@
=@�@�R@��@��@��@V@@@�h@`B@?}@�@V@V@��@�/@z�@9X@�@1@��@�m@��@S�@o@�H@��@��@~�@n�@M�@=q@��@7L@��@�@bN@A�@A�@ �@�@�@�@�@|�@K�@�@ȴ@��@��@��@v�@E�@��@�h@`B@�/@z�@(�@��@�
@��@t�@dZ@dZ@33@�@��@~�@n�@^5@-@-@�@��@�#@��@�^@�7@G�@G�@&�@%@��@�9@�@r�@bN@1'@ �@�@�;@�w@��@l�@+@��@�@ȴ@��@��@v�@V@5?@$�@{@@�T@�-@�@�@`B@?}@��@��@��@�j@�D@I�@�@��@�
@ƨ@��@�@t�@33@o@
�H@
��@
�\@
~�@
^5@
=q@
-@
J@	�@	��@	�7@	x�@	7L@	�@	%@��@�`@�9@�u@�@bN@1'@  @��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�DB�DB�DB�DB�DB�DB�DB�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�\B�VB�JB�7B�1B�%B�B}�Bn�BcTBJ�B+B$�B�B\B��B�BǮB�3B��B��B�VBv�BcTBXBVBR�BI�B@�B8RB6FB-B'�B'�B/B,B!�B�BDB
�B
�ZB
�B
��B
�RB
��B
�PB
x�B
jB
]/B
K�B
5?B
"�B
hB
B	�B	�ZB	�5B	��B	��B	ŢB	�jB	�RB	�3B	�B	��B	��B	��B	�bB	�1B	�B	}�B	u�B	u�B	o�B	m�B	ffB	aHB	^5B	\)B	YB	XB	VB	S�B	Q�B	M�B	I�B	D�B	>wB	8RB	5?B	2-B	1'B	+B	%�B	�B	�B	oB	\B		7B	%B	B	B	B	B��B��B��B��B�B�B�B�B�yB�`B�BB�/B�B�B��B��B��B��BĜB��B�}B�qB�jB�XB�LB�LB�?B�9B�3B�-B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�bB�7B�1B�+B�%B�%B�B�B|�Bw�Bt�Bp�Bl�BiyBe`BbNBaHB_;B]/B[#BYBW
BVBS�BP�BM�BI�BG�BE�BC�BA�B?}B>wB<jB:^B8RB7LB5?B49B33B2-B1'B0!B/B-B)�B'�B&�B%�B#�B!�B!�B �B�B�B�B�B�B�B�BuBhBbB\BbBhBhBbBbBPBVB\BuB�B�B{B�B�B�B�B�B�B�B!�B!�B!�B#�B$�B$�B)�B,B,B+B.B.B.B-B.B/B33B5?B5?B9XB<jB>wB>wBB�BF�BM�BM�BM�BN�BR�BS�BVBW
BW
BW
BXBZB]/B_;BaHBbNBe`BgmBgmBhsBjBl�Bm�Bo�Bs�Bu�Bu�Bv�Bw�By�B{�Bz�B}�B� B�B�B�B�B�%B�7B�DB�JB�JB�JB�PB�VB�VB�\B�bB�oB�oB��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�?B�?B�LB�wBŢB��B��B��B��B�
B�B�B�#B�)B�5B�NB�mB�B�B�B�B�B�B��B��B��B��B	B	B	%B	
=B	VB	uB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	(�B	-B	1'B	49B	8RB	<jB	?}B	@�B	A�B	C�B	F�B	I�B	K�B	N�B	O�B	P�B	O�B	O�B	O�B	P�B	Q�B	O�B	P�B	R�B	R�B	R�B	R�B	R�B	T�B	T�B	VB	XB	ZB	[#B	\)B	\)B	]/B	^5B	`BB	cTB	k�B	l�B	l�B	m�B	n�B	o�B	p�B	t�B	w�B	{�B	~�B	�B	�B	�B	�B	�B	�1B	�DB	�PB	�PB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�9B	�9B	�9B	�?B	�LB	�LB	�RB	�XB	�jB	�}B	��B	��B	��B	B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�)B	�;B	�BB	�HB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B

=B
DB
DB

=B

=B
JB
JB
JB
PB
PB
VB
VB
VB
VB
VB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
uB
uB
uB
uB
uB
{B
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
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
!�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
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
/B
0!B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
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
;dB
;dB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
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
I�B
I�B
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
Q�B
Q�B
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
XB
XB
XB
XB
XB
XB
XB
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
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
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
bNB
bNB
bNB
cTB
cTB
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
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
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
o�B
o�B
o�B
o�B
p�B
p�B
p�B
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
r�B
s�B
r�B
s�B
s�B
s�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�PB�*B�B�7B�7B�ZB�eB�^B�qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�4B�xB�gB��B��B�nB��B�LB��B�9B�jB��B��B��B�tB��B�?B�B�nBp�Bh7BRB,�B&kB �B�B��B��B�$B�	B�WB��B��B{yBfBX}BV�BUjBL6BD�B9QB8B.mB(HB(�B1�B.uB#'BB^B
�wB
��B
ڃB
�DB
��B
��B
�B
|$B
m�B
a�B
R�B
;cB
)3B
�B
�B	��B	��B	��B	�gB	�sB	�,B	�<B	�1B	��B	��B	��B	��B	��B	��B	�$B	�B	�JB	w�B	xB	p�B	rB	h�B	cB	_�B	]xB	Y�B	X�B	V�B	T�B	S�B	O~B	K�B	G-B	@�B	9zB	6$B	2�B	33B	,ZB	(�B	#�B	�B	�B	B	
eB	�B	�B	�B	OB	�B	 6B��B�B��B�MB�~B�B�_B�B�|B��B�8BڬB׳BҎB�WB��B��B��B¹B�/B��B��B�'B�pB��B��B��B��B��B��B�UB�+B�DB��B�;B�@B��B��B��B�B��B�1B��B�
B�ZB��B��B�dB�`B��B��B�6B�ByYBw�Bs�Bp8Bm(BgtBc�Bb�BaB_�B\�BZ�BXrBWiBU�BSqBQ�BL[BI�BGBD�BD�BA�B@�B?�B<<B:B8�B84B5�B4SB2�B1�B2B0&B0ZB-pB)�B(KB'B%�B#B"�B!�B!MBtB�BB-BcB�B�BpB�B}B�BlB�B�B�B�B�B�B�BjB%B�B�B�B�BtB�B9B�B!�B"�B#�B$�B&IB'�B+�B,�B-$B-UB.�B.aB.�B.�B/�B2[B4�B6,B7�B;zB=VB>�B?�BC�BH�BN�BN�BN9BO�BTLBU#BWBW�BW�BW�BYB[@B]�B`"BbBcvBfBg�Bg�Bh�Bj�Bl�Bm�BpBtFBu�BvlBwBx�Bz�B|?B|�BiB��B�#B��B�5B�2B��B��B�|B��B�pB��B��B��B��B��B��B��B��B��B�B��B�:B��B�'B�DB��B�QB��B�B�%B�aB��B��B��B�LB�BĂB�dB��B�B�#B�/B�B�<B�@B�TBޣB��B�vB�B�:B�B�B�[B�1B�}B��B��B��B	�B	4B	@B	
B	�B	%B	B	�B	�B	�B	=B	�B	�B	�B	 �B	"�B	$B	)�B	-�B	1�B	5%B	9�B	=B	?hB	@vB	A�B	DB	GlB	KeB	LBB	O:B	P�B	Q�B	PB	PfB	PEB	QJB	S�B	Q�B	P�B	R�B	R�B	SB	S�B	S�B	U0B	U�B	V�B	X�B	Z�B	[aB	\@B	\NB	]�B	^�B	`�B	dRB	k�B	l�B	l�B	m�B	n�B	o�B	p�B	t�B	w�B	{�B	~�B	�B	�#B	�*B	�)B	�2B	��B	��B	��B	��B	�zB	�aB	�/B	�SB	��B	��B	��B	��B	��B	��B	��B	�qB	��B	��B	��B	�B	��B	��B	�B	��B	��B	�B	�)B	�-B	�B	�RB	�B	�B	�.B	�BB	�eB	�GB	�pB	��B	�\B	�gB	�eB	��B	��B	�yB	�~B	�{B	��B	 B	ßB	��B	��B	�`B	�B	��B	�	B	��B	��B	κB	�}B	�B	�B	�!B	� B	�&B	�-B	�B	�4B	܁B	߅B	�nB	�B	�NB	�ZB	�`B	�gB	�B	��B	��B	�~B	�B	�vB	�B	�B	�lB	�uB	�^B	�B	�|B	�B	��B	��B	��B	�B	�<B	��B	�B	��B	�B	�B	�B	�B	��B	�,B	�	B	�B	��B	��B	��B	��B	�
B	�B	��B	��B	�	B	�PB	�B	�B
 IB
HB
%B
4B
LB
(B
AB
4B
DB
UB
}B
�B
EB
jB
	�B
	yB

+B
@B
NB

eB

�B
UB
xB
�B
bB
VB
_B
SB
jB
�B
B
�B
yB
sB
rB
rB
dB
�B
�B
�B
�B
�B
�B
�B
�B
.B
B
B
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
�B
dB
�B
�B
�B
�B
�B
�B
�B
B
SB
�B
�B
B
/B
B
�B
�B
�B
2B
�B
�B
�B
�B
�B
 B
 �B
�B
�B
 (B
�B
 B
 B
�B
 �B
!�B
#1B
$B
$�B
$�B
$�B
&	B
%�B
&B
'0B
(B
(B
*B
*B
)�B
*B
+B
+B
+B
,B
,B
,QB
,TB
-IB
-B
-!B
.0B
.KB
/�B
/yB
/7B
02B
1cB
1�B
1�B
2^B
3MB
3CB
30B
3MB
3NB
3CB
4aB
4~B
4�B
4rB
5QB
5PB
5EB
5EB
5DB
5QB
5FB
5lB
6�B
7�B
7PB
7OB
7^B
7]B
8nB
8B
8�B
9�B
:rB
:tB
:�B
:�B
;�B
;�B
;�B
;nB
;_B
;bB
;�B
;�B
<�B
=�B
=�B
=�B
>qB
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
FB
F�B
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
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
LB
K�B
K�B
K�B
K�B
L�B
MB
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
O
B
O�B
P B
O�B
O�B
P�B
P�B
P�B
Q�B
RB
RB
RB
Q�B
Q�B
R�B
SB
R�B
SB
S%B
TB
S�B
TB
T B
T*B
UIB
UB
UB
UB
V)B
V-B
V
B
V!B
VB
VB
W'B
WRB
W(B
X9B
X/B
X$B
XB
XB
XB
X8B
YMB
YCB
Y5B
Z9B
Z1B
Z+B
Z#B
ZB
Z%B
Z1B
Z[B
[KB
[8B
[+B
[-B
[,B
[ZB
\TB
\NB
\FB
]5B
]KB
]<B
]5B
]>B
]6B
]�B
^}B
^oB
_mB
_LB
_IB
_7B
_MB
`_B
`=B
`?B
`hB
`bB
``B
`]B
ayB
aTB
aPB
aDB
a\B
adB
a�B
bVB
blB
b�B
c�B
c�B
drB
djB
dsB
e|B
efB
e\B
e}B
e�B
e�B
fyB
fmB
fmB
f�B
gfB
gvB
g�B
gB
gvB
gtB
g�B
g�B
hoB
h�B
h�B
h�B
h�B
h�B
i~B
i~B
i�B
i�B
i�B
iB
i�B
i�B
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
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
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
o�B
o�B
o�B
o�B
p�B
p�B
p�B
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
r�B
s�B
r�B
s�B
s�B
s�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.04 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810231352282018102313522820181023135228  AO  ARCAADJP                                                                    20180321170242    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180321170242  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180321170242  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181023135228  QC  PRES            @�  D�)�G�O�                PM  ARSQCTM V1.1                                                                20181023135228  QC  PSAL            @�  D�)�G�O�                PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181025093512  IP                  G�O�G�O�G�O�                