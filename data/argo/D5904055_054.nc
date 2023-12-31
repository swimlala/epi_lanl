CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2016-04-19T15:14:50Z creation; 2016-04-19T15:14:50Z updated; 2016-09-02T17:52:14Z converted from 3.0   
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7$   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7d   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8    	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8(   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8H   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8h   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           8l   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8t   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8x   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �  �Argo profile    3.1 1.2 19500101000000  20160419151450  20181103100338  5904055 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               6A   AO  5004_0303_054                   2C  D   NAVIS_A                         0303                            082713                          863 @�i[k���1   @�i\�@6�z�G��d�333331   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      6A   A   A   @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{y�D|  D|� D}  D}� D}��D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@(�@�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��B (�B(�BB(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
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
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{|)D|�D|��D}�D}��D}�)D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�A�A�n�A�Q�A�^A�S�A�G�A�9XA�&�A��A�JA�A�  A��A���A�Q�AمA�v�AլA�ȴA�O�A���Aѡ�A�
=Aκ^A;wA�/A˥�A�(�A�ffA��
A�bNA�;dA���A�l�A�z�A�v�A�S�A���A�A�/A�bNA���A�%A��A���A�=qA��A��A�ƨA��9A�A��RA��9A�C�A�hsA�"�A��TA�M�A��PA���A��A��PA��A��yA���A�K�A��uA��mA�v�A��hA�ƨA�5?A�p�A�ƨA�oA��A��mA�ƨA��A��A���A���A��A���A�z�A�I�A���A�r�A��PA�z�A�=qA��A�%A���A�jA��A��!A��A�33A��wA���A��A���A��!A�bNA�bA��HA�%A���A��`A�hsA~�uA|A�Az��Ax�RAw"�Av�DAu�7AtVAs��Ar��Aq�Ap~�An-Al�9Ak�Aj�9Aj��AjVAj{Ai�TAi��Ah�9Ag��Af�DAdȴAbI�Aa?}A`Q�A_33A^��A]�#A[�AW��AU��AR��AQ|�APjAO�wAN1'AK��AI��AH^5AF�9AF{AC�ACXAB5?AAVA>��A<�RA;`BA;VA:�A:=qA:5?A9��A7��A6M�A5�A4bNA3hsA333A1��A0��A/?}A.��A,�jA+��A+��A+�7A+C�A*I�A($�A&��A&I�A$��A#�FA"�9A!�A $�A33A��A�A�\AG�A�AhsA�+A�A(�A�;AK�A��A�A�-A�A�
AhsAĜAI�A�#AS�A�A
=A
$�A�HA�A`BA��A��A  A�PA;dA-A�7AS�A �!@�@�j@�l�@��@���@�"�@�V@� �@�~�@���@�F@���@���@�bN@�w@��@�$�@�D@�K�@���@�!@�V@��@��T@�O�@�/@�(�@�@㝲@��@��#@��u@�ƨ@ޗ�@��#@��`@��@�G�@�"�@֧�@�"�@��H@��@Դ9@Ӯ@���@�l�@��@���@�9X@�33@ʗ�@ȓu@�E�@���@��@�(�@���@�\)@�@���@�b@�S�@���@��^@�/@�Ĝ@��w@���@�V@��T@�x�@��/@���@���@�@�ff@�5?@�x�@��j@���@���@�V@��T@���@�&�@��@���@��@��D@��D@� �@���@�K�@�o@��R@�^5@��@��@���@���@��7@�%@��@�1@��w@�dZ@��@�$�@���@�7L@���@�r�@�bN@�Q�@���@�"�@��R@�`B@���@�z�@�r�@�j@� �@��@��!@���@�/@��u@�b@�l�@��!@�V@��7@���@�j@�1@��P@�|�@�\)@�
=@���@��@�E�@��7@���@��@�b@���@�"�@�E�@��@��h@�?}@���@�j@��m@�S�@��H@��!@��!@�33@�|�@�o@��@�
=@���@�hs@���@��P@���@��m@�I�@��u@�=q@�@�\)@��@��@��P@�ƨ@�ƨ@�33@���@�@��F@��\@�=q@��@���@�p�@�p�@��7@���@���@���@��@�v�@�ȴ@���@�Z@��@�j@�r�@���@�V@��D@�1'@�l�@��@�
=@�
=@�ȴ@�ff@���@�@��T@���@�p�@�X@�?}@�V@���@���@� �@��@� �@��@�1@��;@���@�C�@��y@��\@�~�@�J@��-@��7@��@�G�@���@���@��F@�
=@�=q@���@��@�J@�7L@��`@�p�@�J@���@�X@�Ĝ@�b@~�R@}p�@|�@|j@{�
@z��@z��@z��@z��@y7L@xA�@xA�@x �@xb@x  @w�;@w�;@w\)@v�y@v�R@w�@wl�@w�P@xb@w�;@w�@w��@w�w@w�w@w|�@v�R@u�h@u`B@v��@xbN@w��@w
=@v@t��@r^5@q�@r-@r=q@r-@r-@r�@q�@q��@q�7@q�@p��@q%@p��@pr�@pr�@pr�@p�@pQ�@o�@n��@n�+@n$�@m�T@m@mO�@l��@l�@l��@l�@kƨ@k�@kdZ@j�@j=q@h��@g�;@g�@g|�@f�@f�R@f��@f@f@fE�@fV@fv�@fff@e?}@d(�@dZ@d�j@c�m@c33@b�H@bM�@bM�@b-@a�#@`1'@_+@^$�@]��@^E�@_��@` �@`  @` �@`�9@_;d@\��@\�D@\I�@\z�@\9X@[dZ@["�@Z��@Z�@Y��@Y&�@Yhs@Yx�@Y�@X��@X�9@X�u@X  @W�w@WK�@V��@V�+@V$�@V@U�-@U��@U��@U��@U�-@U��@UO�@T��@T��@Tz�@Tj@TI�@T�@S��@Sƨ@St�@SS�@SC�@S33@S"�@So@So@R�@R�H@R�H@R��@R�@Q�@Q�@Q��@Q7L@Pr�@O�;@O��@P  @P �@P �@OK�@Nv�@NV@N5?@M�T@M@M�-@M��@M�h@Mp�@MO�@L�/@L�j@Lj@L9X@L9X@L�@L1@K��@Kƨ@K�@K33@Ko@J�@J��@I��@I�@H�`@H��@Hb@G�P@GK�@G+@GK�@G
=@F�@Fff@E@E�-@E?}@C�
@CS�@C33@C33@C33@C33@B�H@B=q@BJ@A��@A��@A�^@A�^@A�7@A&�@@��@@��@?��@>�y@>��@>v�@>v�@>V@>@=�-@=V@<�/@<�j@<�j@<�j@<Z@<Z@<1@;�F@;��@;t�@;"�@:��@9�#@9G�@9�@8��@8A�@81'@8b@7�;@7|�@6�y@6V@6@5@5?}@4�j@49X@49X@3�
@2��@2M�@1�@1G�@0�@0b@/�;@/��@/�@.�@.v�@-�T@-�-@-�@-?}@-?}@-O�@-�@,�@,�D@,Z@,I�@,�D@,��@,z�@,9X@+��@+t�@+S�@+S�@+33@+"�@+o@*��@*��@*M�@)�@)�7@)X@(�9@'�;@'��@'+@'
=@&��@&�y@&�y@&�@&ȴ@&v�@&E�@&$�@&{@%�@%@%��@%�@%/@$��@$��@$j@#ƨ@#�F@#��@#t�@"�!@"-@"J@!��@!�#@!�^@!�7@!x�@!x�@!x�@!G�@ Ĝ@ �u@ bN@ 1'@l�@E�@@��@�@`B@?}@/@V@�/@z�@��@t�@dZ@S�@C�@@�H@�H@�H@�H@�H@��@M�@=q@-@-@��@&�@��@Ĝ@��@�u@�u@�u@�@r�@bN@A�@b@  @�w@�@|�@+@�@��@��@�y@�@�R@��@��@ff@$�@@@�@��@�h@O�@?}@/@�@j@(�@�m@��@t�@C�@@��@��@~�@n�@^5@-@��@�^@hs@�@�`@r�@A�@ �@b@�@��@|�@\)@+@�@��@ff@5?@@��@��@��@@�@?}@��@��@�@��@�F@�F@��@�@33@@
��@
n�@
J@	�7@	�@	%@��@Ĝ@�9@�@r�@Q�@ �@��@\)@;d@�y@ȴ@�R@V@@p�@��@�j@�j@�j@��@j@I�@�@�
@��@t�@"�@�@�!@��@~�@^5@-@J@J@��@��@�7@�7@X@&�@&�@%@ ��@ ��@ ��@ ��@ �`@ ��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�+A�A�A�n�A�Q�A�^A�S�A�G�A�9XA�&�A��A�JA�A�  A��A���A�Q�AمA�v�AլA�ȴA�O�A���Aѡ�A�
=Aκ^A;wA�/A˥�A�(�A�ffA��
A�bNA�;dA���A�l�A�z�A�v�A�S�A���A�A�/A�bNA���A�%A��A���A�=qA��A��A�ƨA��9A�A��RA��9A�C�A�hsA�"�A��TA�M�A��PA���A��A��PA��A��yA���A�K�A��uA��mA�v�A��hA�ƨA�5?A�p�A�ƨA�oA��A��mA�ƨA��A��A���A���A��A���A�z�A�I�A���A�r�A��PA�z�A�=qA��A�%A���A�jA��A��!A��A�33A��wA���A��A���A��!A�bNA�bA��HA�%A���A��`A�hsA~�uA|A�Az��Ax�RAw"�Av�DAu�7AtVAs��Ar��Aq�Ap~�An-Al�9Ak�Aj�9Aj��AjVAj{Ai�TAi��Ah�9Ag��Af�DAdȴAbI�Aa?}A`Q�A_33A^��A]�#A[�AW��AU��AR��AQ|�APjAO�wAN1'AK��AI��AH^5AF�9AF{AC�ACXAB5?AAVA>��A<�RA;`BA;VA:�A:=qA:5?A9��A7��A6M�A5�A4bNA3hsA333A1��A0��A/?}A.��A,�jA+��A+��A+�7A+C�A*I�A($�A&��A&I�A$��A#�FA"�9A!�A $�A33A��A�A�\AG�A�AhsA�+A�A(�A�;AK�A��A�A�-A�A�
AhsAĜAI�A�#AS�A�A
=A
$�A�HA�A`BA��A��A  A�PA;dA-A�7AS�A �!@�@�j@�l�@��@���@�"�@�V@� �@�~�@���@�F@���@���@�bN@�w@��@�$�@�D@�K�@���@�!@�V@��@��T@�O�@�/@�(�@�@㝲@��@��#@��u@�ƨ@ޗ�@��#@��`@��@�G�@�"�@֧�@�"�@��H@��@Դ9@Ӯ@���@�l�@��@���@�9X@�33@ʗ�@ȓu@�E�@���@��@�(�@���@�\)@�@���@�b@�S�@���@��^@�/@�Ĝ@��w@���@�V@��T@�x�@��/@���@���@�@�ff@�5?@�x�@��j@���@���@�V@��T@���@�&�@��@���@��@��D@��D@� �@���@�K�@�o@��R@�^5@��@��@���@���@��7@�%@��@�1@��w@�dZ@��@�$�@���@�7L@���@�r�@�bN@�Q�@���@�"�@��R@�`B@���@�z�@�r�@�j@� �@��@��!@���@�/@��u@�b@�l�@��!@�V@��7@���@�j@�1@��P@�|�@�\)@�
=@���@��@�E�@��7@���@��@�b@���@�"�@�E�@��@��h@�?}@���@�j@��m@�S�@��H@��!@��!@�33@�|�@�o@��@�
=@���@�hs@���@��P@���@��m@�I�@��u@�=q@�@�\)@��@��@��P@�ƨ@�ƨ@�33@���@�@��F@��\@�=q@��@���@�p�@�p�@��7@���@���@���@��@�v�@�ȴ@���@�Z@��@�j@�r�@���@�V@��D@�1'@�l�@��@�
=@�
=@�ȴ@�ff@���@�@��T@���@�p�@�X@�?}@�V@���@���@� �@��@� �@��@�1@��;@���@�C�@��y@��\@�~�@�J@��-@��7@��@�G�@���@���@��F@�
=@�=q@���@��@�J@�7L@��`@�p�@�J@���@�X@�Ĝ@�b@~�R@}p�@|�@|j@{�
@z��@z��@z��@z��@y7L@xA�@xA�@x �@xb@x  @w�;@w�;@w\)@v�y@v�R@w�@wl�@w�P@xb@w�;@w�@w��@w�w@w�w@w|�@v�R@u�h@u`B@v��@xbN@w��@w
=@v@t��@r^5@q�@r-@r=q@r-@r-@r�@q�@q��@q�7@q�@p��@q%@p��@pr�@pr�@pr�@p�@pQ�@o�@n��@n�+@n$�@m�T@m@mO�@l��@l�@l��@l�@kƨ@k�@kdZ@j�@j=q@h��@g�;@g�@g|�@f�@f�R@f��@f@f@fE�@fV@fv�@fff@e?}@d(�@dZ@d�j@c�m@c33@b�H@bM�@bM�@b-@a�#@`1'@_+@^$�@]��@^E�@_��@` �@`  @` �@`�9@_;d@\��@\�D@\I�@\z�@\9X@[dZ@["�@Z��@Z�@Y��@Y&�@Yhs@Yx�@Y�@X��@X�9@X�u@X  @W�w@WK�@V��@V�+@V$�@V@U�-@U��@U��@U��@U�-@U��@UO�@T��@T��@Tz�@Tj@TI�@T�@S��@Sƨ@St�@SS�@SC�@S33@S"�@So@So@R�@R�H@R�H@R��@R�@Q�@Q�@Q��@Q7L@Pr�@O�;@O��@P  @P �@P �@OK�@Nv�@NV@N5?@M�T@M@M�-@M��@M�h@Mp�@MO�@L�/@L�j@Lj@L9X@L9X@L�@L1@K��@Kƨ@K�@K33@Ko@J�@J��@I��@I�@H�`@H��@Hb@G�P@GK�@G+@GK�@G
=@F�@Fff@E@E�-@E?}@C�
@CS�@C33@C33@C33@C33@B�H@B=q@BJ@A��@A��@A�^@A�^@A�7@A&�@@��@@��@?��@>�y@>��@>v�@>v�@>V@>@=�-@=V@<�/@<�j@<�j@<�j@<Z@<Z@<1@;�F@;��@;t�@;"�@:��@9�#@9G�@9�@8��@8A�@81'@8b@7�;@7|�@6�y@6V@6@5@5?}@4�j@49X@49X@3�
@2��@2M�@1�@1G�@0�@0b@/�;@/��@/�@.�@.v�@-�T@-�-@-�@-?}@-?}@-O�@-�@,�@,�D@,Z@,I�@,�D@,��@,z�@,9X@+��@+t�@+S�@+S�@+33@+"�@+o@*��@*��@*M�@)�@)�7@)X@(�9@'�;@'��@'+@'
=@&��@&�y@&�y@&�@&ȴ@&v�@&E�@&$�@&{@%�@%@%��@%�@%/@$��@$��@$j@#ƨ@#�F@#��@#t�@"�!@"-@"J@!��@!�#@!�^@!�7@!x�@!x�@!x�@!G�@ Ĝ@ �u@ bN@ 1'@l�@E�@@��@�@`B@?}@/@V@�/@z�@��@t�@dZ@S�@C�@@�H@�H@�H@�H@�H@��@M�@=q@-@-@��@&�@��@Ĝ@��@�u@�u@�u@�@r�@bN@A�@b@  @�w@�@|�@+@�@��@��@�y@�@�R@��@��@ff@$�@@@�@��@�h@O�@?}@/@�@j@(�@�m@��@t�@C�@@��@��@~�@n�@^5@-@��@�^@hs@�@�`@r�@A�@ �@b@�@��@|�@\)@+@�@��@ff@5?@@��@��@��@@�@?}@��@��@�@��@�F@�F@��@�@33@@
��@
n�@
J@	�7@	�@	%@��@Ĝ@�9@�@r�@Q�@ �@��@\)@;d@�y@ȴ@�R@V@@p�@��@�j@�j@�j@��@j@I�@�@�
@��@t�@"�@�@�!@��@~�@^5@-@J@J@��@��@�7@�7@X@&�@&�@%@ ��@ ��@ ��@ ��@ �`@ ��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�fB
�`B
�`B
�`B
�`B
�ZB
�`B
�fB
�`B
�NB
�/B
��B#�BQ�BYB{�B��B�#B�mB��BPB0!B>wBJ�B[#BjBt�Bu�Bw�B}�B�VB��B�B�sB�B�;B�NB�ZB�mB��B�B��B��B�3BĜB�B�ZB�BuB1'B&�B.B1'B49B2-B1'B)�B&�B7LB;dB=qB;dB49B-B&�B�BhBDBB  BB��B�B�'B�oBv�Bn�BXB9XB2-B5?B0!B-BuB��B��B��B��B��B�{B�VBp�BH�B�B�BbB%B1BhBuBVB+B
�B
��B
�dB
�B
��B
�VB
z�B
n�B
_;B
S�B
N�B
F�B
>wB
8RB
5?B
7LB
$�B
JB
B
B
B
{B
hB
PB
DB
+B
  B	��B	�B	�;B	��B	ǮB	ĜB	�jB	�FB	�B	��B	q�B	]/B	I�B	@�B	:^B	6FB	.B	�B	�B	bB	
=B	%B��B��B��B�B�B�fB�TB�NB�BB�BB�BB�BB�#B�
B��B��B��B��BÖB�jB�RB�3B�!B�!B�-B�-B�'B�B��B��B��B��B��B��B��B�\B�JB�=B�+B�B}�By�Bw�Bt�Bq�Bp�Bo�Bn�Bl�BiyBgmBe`Be`BdZBdZBcTBbNBaHB_;B]/B[#BZBYBXBXBW
BXBYBYBXBXBVBVBW
BYBYBXBT�BS�BQ�BO�BN�BN�BO�BO�BN�BO�BP�BR�BP�BT�BXBYBZBYBYBYB]/BaHBbNBbNBbNBcTBcTBaHB`BB`BBaHB_;B^5B[#B[#B`BBgmBjBl�Bm�Bm�Bl�Bm�Bm�Bp�Bu�Bx�Bw�Bz�B{�Bz�Bz�Bw�Br�Br�Bq�Bp�Bp�Bo�Bn�Bl�Bl�Bl�Bk�Bl�Bl�Bo�Bp�Br�Bs�Bu�Bx�Bz�B{�B|�B|�B~�B� B�B�+B�=B�\B�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�9B�9B�FB�RB�XB�XB�XB�^B�qB�wBĜBɺB��B��B��B��B��B�#B�BB�HB�ZB�sB�B��B��B��B��B	B	B	B	B	B	1B		7B	DB	VB	bB	�B	�B	�B	!�B	#�B	'�B	'�B	(�B	)�B	)�B	,B	.B	/B	1'B	33B	49B	8RB	<jB	<jB	=qB	C�B	D�B	?}B	=qB	?}B	C�B	D�B	F�B	I�B	T�B	`BB	hsB	hsB	l�B	q�B	t�B	t�B	u�B	w�B	t�B	m�B	jB	iyB	iyB	iyB	gmB	hsB	jB	k�B	l�B	m�B	q�B	v�B	y�B	~�B	�B	�+B	�1B	�=B	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�9B	�RB	�RB	�RB	�RB	�^B	�dB	�jB	�wB	�wB	��B	��B	��B	��B	ÖB	ƨB	��B	ȴB	ǮB	ƨB	ǮB	ȴB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	ɺB	ǮB	ŢB	ŢB	ĜB	ŢB	ĜB	ĜB	ĜB	ĜB	ĜB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ÖB	ÖB	ŢB	��B	��B	��B	��B	�
B	�B	�B	�)B	�;B	�BB	�NB	�TB	�TB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
B
B
%B
+B
+B
+B
%B
%B
%B
%B
%B
%B
+B
+B
1B
	7B

=B
JB
JB
DB
DB
PB
PB
PB
PB
PB
VB
\B
bB
\B
VB
VB
VB
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
$�B
%�B
%�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
)�B
)�B
+B
-B
.B
.B
.B
.B
-B
-B
-B
-B
-B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
0!B
0!B
0!B
1'B
1'B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
49B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
:^B
<jB
=qB
<jB
=qB
>wB
>wB
>wB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
A�B
A�B
A�B
A�B
@�B
@�B
@�B
@�B
@�B
?}B
@�B
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
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
I�B
J�B
J�B
K�B
K�B
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
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
P�B
Q�B
Q�B
Q�B
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
S�B
S�B
S�B
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
YB
YB
ZB
YB
YB
YB
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
]/B
^5B
^5B
^5B
^5B
^5B
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
cTB
cTB
cTB
cTB
cTB
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
jB
jB
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
m�B
n�B
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
p�B
q�B
q�B
q�B
q�B
q�B
q�B
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
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
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
�BB.B\)BffB�B�B�yB��B%B�B:^BH�BVBe`Bs�B|�B~�B�B�7B��B�3B��B��B�HB�yB�B�B��B�#B�^B��B�-B�dB��B�/B�B��B�B<jB0!B7LB;dBC�B>wB;dB49B5?B@�BD�BF�BF�B?}B7LB33B'�B�B�B\BDBPBJBB��B��B�Bz�Be`BB�B>wBA�B:^B:^B'�B��B��B��B��B��B��B��B~�B\)B%�B�B�BbB\B�B�B�B{BB
�;B
ƨB
�LB
�B
��B
�%B
z�B
jB
]/B
YB
Q�B
H�B
B�B
?}B
C�B
33B
�B
\B
JB
JB
�B
�B
�B
uB
hB

=B
B	��B	�B	�B	��B	��B	ŢB	�}B	�RB	��B	~�B	k�B	T�B	J�B	C�B	A�B	<jB	+B	 �B	�B	oB	hB	%B	B	  B��B��B�B�B�B�yB�mB�yB�B�`B�BB�/B�)B�B�B��BǮB��B�}B�XB�RB�^B�^B�dB�^B�3B�B�B��B��B��B��B��B��B�uB�uB�VB�1B�B�B� B{�Bx�Bx�Bw�Bw�Bt�Bq�Bn�Bm�Bm�Bm�Bk�Bk�BjBjBgmBffBdZBaHBcTBaHB`BB`BBaHBcTBaHB`BB_;B`BBaHBbNBbNBbNB]/B\)B\)BYBYBXBZBXBW
BXBZB[#BZB^5B`BBaHBbNBaHBaHBaHBe`BjBjBiyBjBl�Bl�BiyBiyBhsBjBiyBgmBe`BcTBgmBo�Bs�Bu�Bv�Bw�Bv�Bv�Bv�Bx�B~�B�B�B�B�B�B�B�B}�B{�Bz�By�Bx�Bw�Bw�Bt�Bt�Bu�Bt�Bt�Bt�Bw�Bx�Bz�B|�B~�B�B�B�B�%B�%B�1B�1B�JB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�3B�9B�XB�jB�jB�jB�wB��B��B��B��BÖBŢBȴB��B��B��B��B�B�#B�5B�ZB�sB�yB�B�B��B��B	  B	B	+B		7B	DB		7B	DB	PB	\B	hB	uB	�B	�B	�B	"�B	&�B	)�B	-B	0!B	0!B	1'B	2-B	2-B	49B	6FB	7LB	9XB	:^B	;dB	?}B	D�B	C�B	D�B	K�B	M�B	H�B	E�B	F�B	J�B	K�B	M�B	N�B	[#B	gmB	p�B	o�B	s�B	x�B	{�B	|�B	}�B	� B	� B	v�B	r�B	q�B	q�B	q�B	n�B	o�B	q�B	r�B	s�B	t�B	x�B	}�B	� B	�B	�DB	�\B	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�3B	�FB	�LB	�RB	�XB	�^B	�^B	�^B	�dB	�}B	�}B	��B	��B	B	ÖB	ĜB	ƨB	ƨB	ȴB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�)B	�5B	�BB	�BB	�TB	�mB	�sB	�B	�B	�yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
1B
1B
	7B

=B

=B
DB
JB
DB
PB
VB
PB
PB
VB
\B
bB
\B
VB
VB
VB
VB
PB
VB
VB
VB
\B
bB
oB
{B
{B
oB
oB
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
#�B
#�B
�B
!�B
 �B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
)�B
,B
,B
-B
.B
.B
-B
-B
.B
.B
.B
.B
/B
.B
.B
.B
.B
/B
0!B
0!B
0!B
1'B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
1'B
1'B
1'B
1'B
1'B
2-B
1'B
1'B
2-B
2-B
2-B
1'B
2-B
33B
33B
2-B
1'B
2-B
49B
5?B
6FB
6FB
6FB
5?B
5?B
5?B
49B
49B
33B
49B
49B
49B
49B
5?B
5?B
49B
5?B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
7LB
9XB
8RB
8RB
8RB
8RB
9XB
8RB
8RB
8RB
7LB
7LB
8RB
9XB
9XB
;dB
;dB
<jB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
D�B
E�B
C�B
E�B
F�B
F�B
F�B
E�B
E�B
F�B
F�B
E�B
F�B
F�B
F�B
G�B
G�B
I�B
I�B
I�B
I�B
H�B
G�B
H�B
H�B
H�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
R�B
R�B
S�B
S�B
R�B
S�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
VB
VB
W
B
XB
XB
XB
XB
XB
XB
YB
XB
YB
YB
YB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
YB
YB
YB
ZB
ZB
[#B
[#B
ZB
[#B
[#B
[#B
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
^5B
^5B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
_;B
aHB
`BB
aHB
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
dZB
e`B
e`B
ffB
ffB
e`B
ffB
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
gmB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
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
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
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
o�B
o�B
p�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
t�B
t�B
t�B
t�B
u�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
v�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
{�B
z�B
{�B
{�B
|�B
|�B
{�B
|�B
|�B
|�B
{�B
|�B
}�B
|�B
}�B
|�B
|�B
|�B
|�B
}�B
|�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =-0.04 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 1.0002 (+/-0), vertically averaged dS = 0.007 (+/-0.001)                                                                                                                         Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      201811021508342018110215083420181102150834  AO  ARCAADJP                                                                    20160419151450    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160419151450  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160419151450  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20160929165020  QC  PRES            @��D���G�O�                PM  ARSQCTM V1.1                                                                20160929165020  QC  PSAL            @��D���G�O�                PM  ARSQOWGUV1.0                                                                20181103100338  IP                  G�O�G�O�G�O�                