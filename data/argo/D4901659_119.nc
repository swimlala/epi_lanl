CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-11-01T15:05:54Z creation      
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ެ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �|   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �        �Argo profile    3.1 1.2 19500101000000  20171101150554  20230721230913  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               wA   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�2Z�0D1   @�2F5�@<WKƧ��c�V�u1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      wA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP��BXffB^��Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP��BX�\B^��BgBp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�C 
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
=C.#�C0
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
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh|)Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD�ǮD��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A��
A��
A��A��
A��
A��
A��A���A��A��A���A���A���A���A���A���A���A���A���A�AǴ9AƮA�7LA�1A�~�A��DA��uA�t�A�x�A���A�\)A�jA��jA��A��jA�p�A�VA� �A���A�I�A��A�
=A��HA��A��DA���A��PA�jA�=qA��A�|�A�7LA���A���A�p�A�bA�l�A�bA��DA�v�A�{A�l�A��RA��+A�VA��^A�bNA���A�9XA���A��A�PA~��A}�A|�/A{��Ay�7Axn�Aw/At�uAs7LAr�!Ap��ApjAo�AnbNAm��Am7LAl �Ak;dAj��AjAh��Ah�Ag`BAf�9Ae�FAd�AcO�AcoAbbNAal�A` �A_VA]�A\�AZ�jAZr�AZ1AX�!AW�;AW/AV��AT�yAS�TARȴARI�AQ��AP��AO?}AM�AL��AK�hAJ��AJ��AIx�AH(�AGXAG�AGAF��AF(�AE�hAE&�AD�RADQ�AC�^ABr�AA�-AA;dA@�A@��A@=qA@$�A?��A?`BA>^5A=��A=|�A=VA<r�A;A;p�A;&�A:�9A:bNA:1A9��A9G�A8bA7\)A6��A6�+A5�A533A49XA3C�A2-A1�;A1G�A/ƨA.~�A-�mA-33A,M�A*��A(��A'�^A'O�A&jA%oA$�A#33A"�A"�DA"A�A"  A!A!O�A =qAdZA�/A�A  A|�A%A��AffA�hAz�A�PAoA��AZAE�AA�
A�AbA�A"�A��AjA�
A|�A+AM�A%A�+A�;AoA�A��A{A
�yA
v�A	��A�`A�^A��A�TAt�A�A�A7LA�A �A 5?@�S�@�J@��@�9X@���@��+@���@�j@�n�@�@�C�@��@���@��@��@�@�V@���@��@��#@�E�@߮@���@�J@���@��@��@�r�@׍P@���@�M�@�@Ԭ@�o@�o@�@�@�`B@��/@�(�@θR@�=q@�-@��#@�hs@�&�@��;@�^5@ɺ^@���@�S�@ź^@Ĵ9@�1'@���@�5?@�G�@�S�@��H@���@��@���@�l�@��y@��\@��@�%@��@���@�1@�(�@�j@�b@��w@�dZ@���@���@��T@��D@��@��@�`B@��@��y@��+@�=q@��-@�%@��/@�(�@�\)@�M�@�Ĝ@��F@�K�@�
=@���@���@��!@�~�@�J@�V@�1@��@���@�b@��@�A�@�bN@�Q�@��m@�K�@�o@�
=@�~�@�$�@��h@���@�b@���@�$�@�x�@�%@�V@��@�Q�@�9X@���@��@��+@�$�@�`B@��@��;@��P@���@�n�@�E�@�-@��@�O�@�&�@���@��D@�j@���@�ƨ@��P@�K�@��@���@�^5@�M�@�@���@���@��@��@���@�G�@���@���@���@�Z@��m@��@��@�V@���@��/@�j@�Q�@���@��@�O�@�{@��\@�~�@�?}@��j@�1'@���@�^5@���@���@��!@��+@��+@��\@��+@�M�@��^@�X@�%@�Ĝ@��@��7@�J@�b@��@��m@��h@�@�v�@��@���@���@��R@�z�@��`@���@���@��9@�Z@�1@�  @���@�n�@�E�@�{@���@���@��T@�J@��#@���@���@��7@�?}@��@��@�A�@�(�@�P@�w@��@�@~�@~v�@~{@}��@}�-@|��@{33@{@z��@z�@y�7@x��@xr�@xb@v{@tI�@t�@uO�@tZ@t�@s��@s��@sdZ@q�#@q&�@q��@q�@q�@q�@q�#@q��@q�^@q�7@qhs@q&�@p��@p��@p��@p��@p�u@pr�@p �@ol�@n�@n��@n��@nȴ@n�+@m�@m�@m�@l�D@l1@k��@kC�@j��@jJ@iX@h�`@h��@h1'@hb@g�;@g�w@g�@g��@g|�@gl�@g\)@f��@fȴ@f�R@fff@f5?@e�@e@e��@e��@e�h@eV@d��@dj@c��@cdZ@b�@bM�@a�#@a��@ax�@aX@a�@`�u@`1'@`  @_��@_l�@_\)@_
=@^�@^�+@^E�@^$�@^{@]��@]�-@]O�@\�/@\��@\I�@\I�@\9X@\1@[ƨ@[t�@["�@Z�@Z�\@Z�@Y�^@YX@X��@X��@X�@XA�@X  @W�w@WK�@U�@Tz�@T�@T�j@T��@UO�@UV@T�@T��@Sƨ@So@Qx�@Q�^@R�!@Rn�@RJ@Q��@Q��@Q7L@P��@P��@P  @O�@Nv�@N$�@M�@M��@M��@L��@Lj@K�m@K��@KS�@Ko@J�H@J�!@J�@J�H@J�H@J�H@J��@J��@JM�@I��@G�@Gl�@G��@F��@E�h@E�@D�j@CdZ@C"�@C33@CS�@C33@B��@A�@A��@AX@A7L@A7L@A&�@A�@@��@@��@@�`@@��@@�`@@��@@��@@Ĝ@@��@@Q�@@1'@@  @?�;@?l�@>�R@>v�@=�@=�h@=`B@=�@<�@<z�@<I�@<I�@<I�@<I�@<I�@<9X@<(�@<(�@;��@<1@;�
@;dZ@;dZ@;dZ@:n�@:J@9�^@9��@9x�@9%@9�@8  @7|�@7\)@6��@6�y@6�@7�P@8A�@8�9@8��@8  @8  @7�P@6��@6�@6��@6V@6$�@5�T@5�-@5��@5p�@5V@4��@4��@4j@3�
@3��@3S�@333@2�@2��@2��@2~�@1��@1&�@0��@0Q�@/�;@.�y@.v�@.5?@.@-@-p�@,��@,��@,��@,j@+�F@+"�@+@*�H@*��@*�\@)��@)hs@)G�@)%@(A�@(  @'�;@'�w@'l�@'+@&�@&ff@%�@%@%�@%?}@%V@$��@#�m@#S�@#o@"��@"�\@"M�@"J@!�7@!7L@!�@!%@ ��@ r�@ bN@ bN@ A�@�@��@�P@l�@;d@+@+@�@ȴ@�+@�+@�+@�+@v�@v�@v�@ff@ff@E�@$�@{@{@@�@�T@��@@@��@�h@�@��@�@9X@�m@�F@��@�@dZ@33@33@C�@C�@��@��@�\@�\@n�@^5@^5@M�@M�@�@�@�#@��@��@�^@��@�7@hs@hs@G�@�@��@�9@�u@bN@Q�@b@  @  @�w@�@��@�P@K�@��@��@�+@ff@V@V@V@V@E�@E�@E�@5?@$�@$�@{@@��@�-@��@�h@`B@/@�@V@��@Z@S�@"�@o@o@��@��@��@~�@^5@M�@-@�@J@��@�@�#@��@hs@X@7L@&�@�@�`@��@r�@A�@  @�@��@�P@V@�T@@��@��@�h@�h@�h@�h@�h@`B@?}@/@?}@/@��@��@z�@(�@(�@�@�@�@ƨ@�@S�@@
�H@
��@
��@
�!@
~�@
=q@	�@	��@	��@	�7@	X@	G�@	&�@�9@�u@r�@Q�@  @��@�@|�@\)@K�@+@�@�R@v�@ff@5?@�T@@`B@�@�@�/@9X@1@1@��@�m@�@S�@33@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A���A��
A��
A��A��
A��
A��
A��A���A��A��A���A���A���A���A���A���A���A���A���A�AǴ9AƮA�7LA�1A�~�A��DA��uA�t�A�x�A���A�\)A�jA��jA��A��jA�p�A�VA� �A���A�I�A��A�
=A��HA��A��DA���A��PA�jA�=qA��A�|�A�7LA���A���A�p�A�bA�l�A�bA��DA�v�A�{A�l�A��RA��+A�VA��^A�bNA���A�9XA���A��A�PA~��A}�A|�/A{��Ay�7Axn�Aw/At�uAs7LAr�!Ap��ApjAo�AnbNAm��Am7LAl �Ak;dAj��AjAh��Ah�Ag`BAf�9Ae�FAd�AcO�AcoAbbNAal�A` �A_VA]�A\�AZ�jAZr�AZ1AX�!AW�;AW/AV��AT�yAS�TARȴARI�AQ��AP��AO?}AM�AL��AK�hAJ��AJ��AIx�AH(�AGXAG�AGAF��AF(�AE�hAE&�AD�RADQ�AC�^ABr�AA�-AA;dA@�A@��A@=qA@$�A?��A?`BA>^5A=��A=|�A=VA<r�A;A;p�A;&�A:�9A:bNA:1A9��A9G�A8bA7\)A6��A6�+A5�A533A49XA3C�A2-A1�;A1G�A/ƨA.~�A-�mA-33A,M�A*��A(��A'�^A'O�A&jA%oA$�A#33A"�A"�DA"A�A"  A!A!O�A =qAdZA�/A�A  A|�A%A��AffA�hAz�A�PAoA��AZAE�AA�
A�AbA�A"�A��AjA�
A|�A+AM�A%A�+A�;AoA�A��A{A
�yA
v�A	��A�`A�^A��A�TAt�A�A�A7LA�A �A 5?@�S�@�J@��@�9X@���@��+@���@�j@�n�@�@�C�@��@���@��@��@�@�V@���@��@��#@�E�@߮@���@�J@���@��@��@�r�@׍P@���@�M�@�@Ԭ@�o@�o@�@�@�`B@��/@�(�@θR@�=q@�-@��#@�hs@�&�@��;@�^5@ɺ^@���@�S�@ź^@Ĵ9@�1'@���@�5?@�G�@�S�@��H@���@��@���@�l�@��y@��\@��@�%@��@���@�1@�(�@�j@�b@��w@�dZ@���@���@��T@��D@��@��@�`B@��@��y@��+@�=q@��-@�%@��/@�(�@�\)@�M�@�Ĝ@��F@�K�@�
=@���@���@��!@�~�@�J@�V@�1@��@���@�b@��@�A�@�bN@�Q�@��m@�K�@�o@�
=@�~�@�$�@��h@���@�b@���@�$�@�x�@�%@�V@��@�Q�@�9X@���@��@��+@�$�@�`B@��@��;@��P@���@�n�@�E�@�-@��@�O�@�&�@���@��D@�j@���@�ƨ@��P@�K�@��@���@�^5@�M�@�@���@���@��@��@���@�G�@���@���@���@�Z@��m@��@��@�V@���@��/@�j@�Q�@���@��@�O�@�{@��\@�~�@�?}@��j@�1'@���@�^5@���@���@��!@��+@��+@��\@��+@�M�@��^@�X@�%@�Ĝ@��@��7@�J@�b@��@��m@��h@�@�v�@��@���@���@��R@�z�@��`@���@���@��9@�Z@�1@�  @���@�n�@�E�@�{@���@���@��T@�J@��#@���@���@��7@�?}@��@��@�A�@�(�@�P@�w@��@�@~�@~v�@~{@}��@}�-@|��@{33@{@z��@z�@y�7@x��@xr�@xb@v{@tI�@t�@uO�@tZ@t�@s��@s��@sdZ@q�#@q&�@q��@q�@q�@q�@q�#@q��@q�^@q�7@qhs@q&�@p��@p��@p��@p��@p�u@pr�@p �@ol�@n�@n��@n��@nȴ@n�+@m�@m�@m�@l�D@l1@k��@kC�@j��@jJ@iX@h�`@h��@h1'@hb@g�;@g�w@g�@g��@g|�@gl�@g\)@f��@fȴ@f�R@fff@f5?@e�@e@e��@e��@e�h@eV@d��@dj@c��@cdZ@b�@bM�@a�#@a��@ax�@aX@a�@`�u@`1'@`  @_��@_l�@_\)@_
=@^�@^�+@^E�@^$�@^{@]��@]�-@]O�@\�/@\��@\I�@\I�@\9X@\1@[ƨ@[t�@["�@Z�@Z�\@Z�@Y�^@YX@X��@X��@X�@XA�@X  @W�w@WK�@U�@Tz�@T�@T�j@T��@UO�@UV@T�@T��@Sƨ@So@Qx�@Q�^@R�!@Rn�@RJ@Q��@Q��@Q7L@P��@P��@P  @O�@Nv�@N$�@M�@M��@M��@L��@Lj@K�m@K��@KS�@Ko@J�H@J�!@J�@J�H@J�H@J�H@J��@J��@JM�@I��@G�@Gl�@G��@F��@E�h@E�@D�j@CdZ@C"�@C33@CS�@C33@B��@A�@A��@AX@A7L@A7L@A&�@A�@@��@@��@@�`@@��@@�`@@��@@��@@Ĝ@@��@@Q�@@1'@@  @?�;@?l�@>�R@>v�@=�@=�h@=`B@=�@<�@<z�@<I�@<I�@<I�@<I�@<I�@<9X@<(�@<(�@;��@<1@;�
@;dZ@;dZ@;dZ@:n�@:J@9�^@9��@9x�@9%@9�@8  @7|�@7\)@6��@6�y@6�@7�P@8A�@8�9@8��@8  @8  @7�P@6��@6�@6��@6V@6$�@5�T@5�-@5��@5p�@5V@4��@4��@4j@3�
@3��@3S�@333@2�@2��@2��@2~�@1��@1&�@0��@0Q�@/�;@.�y@.v�@.5?@.@-@-p�@,��@,��@,��@,j@+�F@+"�@+@*�H@*��@*�\@)��@)hs@)G�@)%@(A�@(  @'�;@'�w@'l�@'+@&�@&ff@%�@%@%�@%?}@%V@$��@#�m@#S�@#o@"��@"�\@"M�@"J@!�7@!7L@!�@!%@ ��@ r�@ bN@ bN@ A�@�@��@�P@l�@;d@+@+@�@ȴ@�+@�+@�+@�+@v�@v�@v�@ff@ff@E�@$�@{@{@@�@�T@��@@@��@�h@�@��@�@9X@�m@�F@��@�@dZ@33@33@C�@C�@��@��@�\@�\@n�@^5@^5@M�@M�@�@�@�#@��@��@�^@��@�7@hs@hs@G�@�@��@�9@�u@bN@Q�@b@  @  @�w@�@��@�P@K�@��@��@�+@ff@V@V@V@V@E�@E�@E�@5?@$�@$�@{@@��@�-@��@�h@`B@/@�@V@��@Z@S�@"�@o@o@��@��@��@~�@^5@M�@-@�@J@��@�@�#@��@hs@X@7L@&�@�@�`@��@r�@A�@  @�@��@�P@V@�T@@��@��@�h@�h@�h@�h@�h@`B@?}@/@?}@/@��@��@z�@(�@(�@�@�@�@ƨ@�@S�@@
�H@
��@
��@
�!@
~�@
=q@	�@	��@	��@	�7@	X@	G�@	&�@�9@�u@r�@Q�@  @��@�@|�@\)@K�@+@�@�R@v�@ff@5?@�T@@`B@�@�@�/@9X@1@1@��@�m@�@S�@33@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�NB��B~�Bw�BiyBYBH�BC�B?}B6FB.B#�B�BVB  B��B�fB�/B�B��B��B�-B�B��B��B�hB�B�Bv�Bp�Bl�BbNB^5B[#BW
BM�B>wB+B�BDBB
��B
�`B
��B
��B
ƨB
�jB
�FB
�!B
�B
��B
��B
��B
�JB
�B
u�B
m�B
cTB
R�B
I�B
E�B
:^B
6FB
0!B
'�B
$�B
�B
�B
oB
VB

=B
B	��B	��B	��B	�B	�yB	�BB	�/B	�B	��B	ǮB	�}B	�LB	�B	��B	��B	��B	��B	�uB	�PB	�=B	� B	w�B	p�B	q�B	m�B	gmB	\)B	P�B	M�B	K�B	J�B	H�B	C�B	>wB	<jB	:^B	9XB	8RB	5?B	33B	1'B	0!B	.B	)�B	%�B	"�B	 �B	�B	�B	�B	�B	�B	�B	oB	\B	PB	DB		7B	+B	B	B	B	B��B��B��B��B�B�B�B�B�yB�ZB�HB�/B�#B�B��B��B��BǮBB�jB�FB�-B�!B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�bB�VB�PB�DB�=B�+B�B�B� B~�B}�B|�B{�Bz�By�Bv�Bt�Br�Bn�Bk�BiyBhsBgmBdZBbNBaHB_;B^5B]/B\)BZBXBW
BT�BQ�BO�BM�BK�BH�BF�BD�BD�BC�BA�B>wB=qB:^B8RB8RB8RB6FB1'B.B,B,B)�B.B-B)�B(�B(�B(�B)�B,B(�B)�B+B)�B(�B&�B"�B �B�B"�B!�B!�B �B�B �B"�B#�B$�B$�B#�B"�B#�B$�B$�B#�B$�B$�B$�B$�B#�B!�B"�B"�B#�B#�B$�B%�B%�B'�B'�B(�B)�B-B.B/B/B0!B33B49B6FB=qB@�BE�BF�BF�BF�BH�BH�BG�BF�BG�BG�BC�B@�BA�BA�BB�BC�BD�BD�BF�BH�BL�BQ�BT�BW
BXBZB^5BcTBgmBjBk�Bl�Bp�Bo�Bt�Bw�B{�B}�B~�B�B�B�B�B�+B�+B�1B�DB�VB�oB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�9B�?B�FB�FB�LB�jB�qB�wB��B��BĜBŢBǮBɺB��B��B��B��B��B�B�/B�/B�5B�BB�fB�B�B�B�B�B�B�B�B��B��B��B	  B	B	+B	
=B	bB	�B	�B	�B	�B	uB	oB	�B	�B	�B	"�B	#�B	#�B	#�B	#�B	"�B	"�B	"�B	#�B	'�B	,B	1'B	5?B	B�B	D�B	D�B	>wB	8RB	>wB	A�B	B�B	E�B	K�B	XB	]/B	_;B	aHB	bNB	e`B	ffB	hsB	iyB	hsB	k�B	o�B	n�B	q�B	w�B	z�B	~�B	�B	�B	�B	�+B	�1B	�PB	�\B	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�-B	�3B	�3B	�9B	�^B	�qB	�wB	��B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�#B	�B	�B	�B	�)B	�)B	�/B	�;B	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
%B
%B
%B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B
+B
1B
JB
PB
JB
PB
PB
PB
VB
VB
VB
VB
VB
PB
PB
PB
PB
JB
DB

=B
DB
DB
DB

=B
DB
VB
bB
bB
hB
hB
hB
hB
hB
\B
bB
uB
uB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
%�B
%�B
&�B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
%�B
&�B
%�B
&�B
&�B
'�B
)�B
+B
.B
0!B
1'B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
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
O�B
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
Q�B
Q�B
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
R�B
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
XB
XB
XB
YB
YB
YB
YB
YB
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
[#B
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
_;B
_;B
_;B
_;B
_;B
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
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
k�B
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
o�B
o�B
o�B
o�B
o�B
o�B
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
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�B�B��B��B��B�UBpYB`�BL�BE�BB	B:B50B'�B(�B1B�B��B��B�B�^B�RB̨B��B�BB��B��B�>B�VB��By�BrGBr�Bd@B_B]BZ"BS�BDB/B!�B�B�B
��B
�B
�QB
�EB
��B
�AB
�,B
��B
�mB
��B
�PB
�B
�7B
�0B
xmB
p�B
iyB
VJB
K3B
J?B
;rB
8�B
2�B
)B
&�B
"ZB
�B
�B
�B
�B
@B	��B	��B	�B	��B	�IB	��B	��B	ڈB	�
B	�wB	�iB	��B	�zB	��B	�B	�gB	��B	�OB	�B	��B	��B	z�B	rB	svB	o�B	k�B	`�B	S6B	P�B	MPB	K�B	K�B	GB	@�B	=B	:�B	9�B	:AB	6�B	4_B	2SB	1OB	/�B	-�B	'�B	$B	!�B	 �B	�B	�B	�B	�B	�B	B	PB	�B	"B	XB	"B	�B	oB	B	B��B��B�TB��B��B��B�:B�JB�B�gB�B��B�oB�hB��B�B�eBɵB��B�B�fB�B�TB�/B�NB��B�tB��B�nB�UB�JB��B�XB��B��B�B�.B��B��B��B�aB��B�9B��B�QB�OB�B~+B}�B|uB|�B|�Bw�BveBv�BrhBm!BjkBi`Bi�Bg�Bc�BcBa\B_AB]lB]�B]BYEBY`BV�BUBROBPBL�BL(BH.BEgBE�BE�BF�B?�B?4B;�B9DB8�B:uB;�B4YB0�B-]B.�B*�B/�B.<B*�B)�B)�B);B+2B-�B- B,�B,B*�B+�B)KB#�B!\B �B#�B"[B".B"�B!�B �B"�B%&B%�B%�B$�B$�B${B$�B%TB$tB%MB&�B'"B%�B%B$8B%HB$aB$�B%�B&B'�B(�B(�B)�B*)B+�B-�B.�B/�B0 B1�B4B5�B5�B=1B@ BF4BG,BG3BGLBI!BJBI�BH=BH�BI�BE�BBMBBBA�BC^BD�BD�BE�BG�BJRBN�BSwBU�BWgBXrBZB^IBc�BhBk�BmBn7Bs�BpiBt�Bw�B{�B~B�B��B�YB�!B��B��B��B�B�$B��B�+B�AB�B��B�B�&B��B�EB��B��B�tB� B�B��B��B�QB��B�pB�aB��B�#B��B��B�B��B�"B��B��B�B�RB�(B�"B��B�IB�3B�BݠB��BބBߐB�B��B�ZB�dB�<B�8B�B��B��B��B�rB��B��B	�B	�B		B	�B	�B	nB	[B	aB	�B	�B	�B	RB	 -B	#B	#�B	#�B	#�B	$5B	#�B	#kB	#PB	$1B	'�B	+B	0:B	2B	B�B	EB	HoB	BwB	91B	?9B	A�B	B<B	DSB	IB	W]B	]B	_<B	a�B	b�B	e�B	fwB	iB	kOB	h�B	k�B	p?B	n�B	qLB	w�B	{)B	=B	�B	�?B	��B	�nB	�B	��B	��B	��B	�=B	��B	�wB	�B	��B	��B	��B	��B	�<B	��B	��B	�
B	�%B	�/B	�`B	��B	�)B	�<B	�B	��B	��B	��B	�IB	�2B	�XB	�[B	�)B	��B	��B	�"B	�mB	�pB	��B	B	ÜB	ĺB	įB	��B	��B	ȱB	ʻB	ʾB	��B	��B	�B	�mB	�VB	��B	��B	�B	�6B	�rB	�[B	�TB	�{B	�yB	�vB	�aB	�xB	ګB	ڒB	�lB	�JB	�nB	�JB	�bB	�WB	�TB	�RB	�bB	�WB	�\B	�B	�pB	�eB	�B	�B	�B	�B	�B	�|B	�B	��B	�B	��B	��B	��B	��B	�	B	��B	��B	�B	��B	��B	�B	��B	��B	�
B	��B	��B	�B	��B	�B	�B	��B	��B	�B	��B	�5B	�JB	�,B	�,B	��B
 
B
 "B
 +B
 >B
?B
,B
TB
^B
aB
^B
hB
UB
?B
WB
SB
^B
�B
	�B
�B
�B
B
�B
�B
	gB
	OB
	YB

B
	�B

qB
�B
lB
�B
�B
�B
EB
�B
�B
XB
�B
B
�B
�B
tB
kB
vB
�B
�B
�B

zB
rB
qB
gB

ZB
B
\B
^B
^B
uB
�B
�B
�B
�B
�B
 B
aB
4B
�B
�B
B
�B
nB
jB
�B
�B
B
�B
�B
�B
�B
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
 B
 ?B
�B
 B
!	B
 �B
 �B
 �B
!B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
&B
&4B
&�B
%�B
&�B
&,B
&B
%�B
%B
%/B
%�B
'�B
&:B
&�B
'B
'�B
)�B
*�B
-�B
/�B
1-B
2�B
3/B
3xB
4�B
4IB
4_B
5dB
5ZB
5dB
6_B
6LB
6dB
6�B
7QB
7�B
7mB
8�B
8{B
8yB
8aB
8zB
9gB
9aB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
;�B
;�B
;�B
;�B
<�B
<�B
=jB
=�B
=�B
>�B
?�B
?�B
@�B
@�B
AB
A�B
B�B
B�B
CB
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
HQB
J"B
J�B
J�B
J�B
K�B
K�B
LB
MB
M�B
M�B
M�B
NB
N�B
N�B
N�B
OB
N�B
PB
O�B
O�B
P�B
P�B
P�B
QB
QB
Q�B
Q�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
SB
R�B
S9B
S"B
T	B
T=B
U3B
UB
U	B
UB
VB
VB
VB
U�B
VB
VRB
W*B
WB
WB
WB
WB
WB
WB
WB
W'B
X2B
XB
XB
X	B
XB
XB
X&B
Y.B
YB
Y.B
Y9B
YDB
Z0B
Z1B
Z>B
Z%B
ZLB
[.B
[B
[NB
[,B
[-B
[-B
[XB
\fB
\uB
]:B
]EB
]4B
]'B
])B
])B
]4B
]'B
])B
]7B
]9B
])B
]7B
^@B
^]B
^NB
^?B
^AB
^YB
^WB
_FB
_FB
_sB
_�B
`B
akB
aNB
bKB
b~B
bXB
bfB
ciB
cmB
c`B
ckB
c]B
c_B
ddB
dcB
dfB
d�B
d�B
ehB
ewB
elB
ejB
e�B
f�B
f�B
f�B
g�B
gyB
g�B
g�B
hgB
i�B
j�B
j�B
j{B
j�B
jxB
j|B
jxB
j{B
j�B
j�B
j�B
jkB
j�B
j�B
j�B
k�B
k�B
l�B
l�B
l�B
l�B
k�B
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
o�B
o�B
o�B
o�B
o�B
o�B
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
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
v+B
v�B
v�B
v�B
w�B
xB
w�B
w�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<g!/<�v�<#�
<- <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<1]E<1�q<#�
<'?�<#�
<#�
<#�
<9s<C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.04 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810310930082018103109300820181031093008  AO  ARCAADJP                                                                    20171101150554    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171101150554  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171101150554  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181031093008  QC  PRES            @�  D�� G�O�                PM  ARSQCTM V1.1                                                                20181031093008  QC  PSAL            @�  D�� G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230913  IP                  G�O�G�O�G�O�                