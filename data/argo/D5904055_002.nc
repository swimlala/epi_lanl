CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2016-04-19T15:14:15Z creation; 2016-04-19T15:14:15Z updated; 2016-09-02T17:52:03Z converted from 3.0   
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
resolution        =���   axis      Z        L  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  o8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ڀ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ݀   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,  �,Argo profile    3.1 1.2 19500101000000  20160419151415  20181103100327  5904055 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  5004_0303_002                   2C  D   NAVIS_A                         0303                            082713                          863 @��[7���1   @��[�	�@5���+�e$1&�x�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      A   A   A   @�ff@�  @���A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:�fD;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]  D]� D^  D^�fD_fD_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv��Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��fD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@�G�@�{A ��A@��A`��A�Q�A�Q�A��A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
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
=C$#�C&
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
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��De�)Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dv�)Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�{D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD��D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD�ǮD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��\A��DA�v�A�M�A�G�A�33A��A�C�A��A�S�A� �A�A��;A���A���A�dZA�K�A�JA��A�Q�A��A��A�^5A�9XA�33A�-A� �A���A�&�A�v�A�~�A�\)A�M�A�{A�p�A�M�A���A���A�%A��uA���A�dZA���A��DA�^5A��-A���A�\)A�9XA�VA�A���A�~�A�7LA��A��+A�x�A�VA���A��7A��A��RA��A�A�|�A��A�dZA���A���A�-A�ȴA��jA�A�jA��HA�ĜA�hsA���A��jA��!A� �A�;dA��A�p�A�9XA�7LA��^A�r�A�1A�33A�=qA��FA���A���A�n�A�A�A��A���A���A���A��A�=qA���A�oA��wAS�A~�RA~A{��AzĜAzM�Az�Ayp�Ax1'AvZAu+AtE�Ar��Aq�-Aqt�Ap�yAop�Am�mAmoAj{Ag�Af�+Ac��A_��A_|�A^��A]�A\{A[t�AYt�AW�
AV  AS�#AR(�AP��AN�AN�AMl�AKdZAI�AH�AF~�AE��ADv�AA�;AA;dA>ZA<Q�A8n�A7p�A7
=A5G�A3t�A2v�A1�#A1/A0ffA.�uA-�hA,ZA+�TA+��A+`BA*ȴA)��A(��A'7LA&bNA$M�A#��A#`BA#+A"�HA!�hA ZA�
A�hAȴA�A��A/A�RA��AVA��A+AĜA�wA7LAffAx�A�`Ar�Ap�A�A�9AA�AJA�A�FA�-A��A
��A�jA�AQ�A�mA��AZA�
AƨA�AƨA7LA �RA n�A 5?@�
=@�`B@�|�@�?}@���@�@�n�@���@�@���@�9X@���@�ƨ@�+@�~�@�O�@�  @�l�@��@���@�h@�b@��@�V@���@�-@�`B@��u@��@ߥ�@�"�@އ+@��#@�hs@���@� �@�o@ٲ-@�1'@֏\@�O�@ԓu@��;@ӶF@ӕ�@�|�@�+@�v�@��@с@��@ЋD@�Q�@��@Ώ\@��/@˾w@�M�@�O�@�Q�@Ə\@�E�@��#@�x�@��@��;@§�@��T@��-@���@�G�@�7L@�(�@��@�o@�$�@��T@��@���@���@���@�Z@��;@��
@���@��P@�l�@�;d@��@��-@�X@�V@���@�j@�9X@�  @��
@���@�l�@�+@���@��R@��!@��!@��!@��!@���@���@�n�@�5?@���@���@�x�@�`B@�X@��@�z�@�(�@��;@��P@�K�@��@��H@���@��@�I�@���@��@�-@��-@���@��@��@�1@���@�
=@�ff@�X@��@�b@��w@�l�@�
=@���@�ff@��@��-@�hs@��@���@��`@���@��@��@�{@��^@��7@�hs@�7L@��@���@�l�@��@���@�^5@�5?@�$�@��7@��9@� �@�S�@�@�ff@�E�@�$�@�$�@�@��#@��-@��7@�7L@���@��j@��j@��9@��9@��@��u@�r�@�bN@�bN@�b@��;@��
@��w@��w@���@�|�@�dZ@�S�@�+@�@���@��@��R@���@���@���@�5?@��T@�O�@�/@��@�r�@�1'@� �@�(�@� �@�1'@���@��m@��
@���@���@�ƨ@��w@���@��@��@���@���@��P@�|�@�l�@�\)@�S�@�;d@�+@��@�o@�@��@��y@��H@��H@��@��@���@�ȴ@��R@��!@��\@�@���@��h@��h@�x�@��@��@�j@�A�@��
@�|�@�t�@�l�@�l�@�dZ@�"�@��H@���@���@�V@��@���@��h@�x�@�X@�O�@�7L@���@�bN@�1'@���@�|�@�;d@��y@�ff@�5?@��@��@�`B@�G�@�&�@���@�I�@�C�@�
=@��R@���@�hs@�/@�%@��/@��9@���@�I�@��m@���@�
=@�ȴ@���@�v�@�-@���@��@���@�j@�Q�@� �@�w@l�@
=@~$�@}?}@|��@|�D@|Z@{�
@{C�@z��@y��@x�u@xA�@x �@x  @w��@wl�@wK�@w�@v�y@v�R@v{@up�@s�
@rM�@q�@q&�@p�@p1'@o\)@n��@nE�@m��@l�@ko@j��@jn�@i��@iG�@i%@h��@g�@g�w@gK�@fȴ@fV@f@e@e��@eO�@d�/@cS�@b�H@b��@b��@bn�@bJ@ax�@`�@_�;@_K�@_
=@^��@^5?@]p�@\�@\�/@\�j@\j@\9X@[ƨ@[33@Zn�@Y��@Y��@X��@Xb@W�;@W�w@W�P@WK�@V�@VE�@U�T@U�h@UO�@U?}@Tz�@So@Rn�@RJ@Q��@Q�^@Q��@QG�@P��@P�9@Pr�@P  @O�w@O|�@Ol�@OK�@N�R@M�@M@M`B@L��@Lz�@L(�@Kƨ@K��@K�@K�@Kt�@KdZ@KC�@J�H@J��@J~�@J~�@JM�@J=q@JJ@I��@I��@I�7@I&�@HĜ@H�u@HQ�@Hb@G�P@F��@F�@Fȴ@FV@F{@E�T@E�h@EV@D�@D9X@Co@B��@Bn�@B^5@B=q@B�@BJ@A7L@@ �@?�@>�@>$�@=?}@<�@<z�@;��@;t�@;S�@;33@;o@;@;@;@;@;@;@;@;@:�@:n�@:�@9�@9��@9�^@9��@9x�@9hs@9X@9X@9X@9X@9X@9G�@9G�@9&�@8��@8�9@8�9@8bN@7�w@7��@7��@7�w@7��@7��@7��@7�P@6�@6��@6v�@6V@6{@5�@5�T@5�T@5�T@5�T@5��@5`B@4z�@4(�@3�m@3��@3��@3�@3�@3C�@3"�@2�@2��@2�!@2n�@2-@1�@1��@1G�@0�@0�@0�@0bN@01'@0 �@0 �@0b@0  @/�@/�w@/+@/+@/
=@.�@.ȴ@.��@.ff@.$�@-�h@-?}@-?}@-?}@-/@,�j@+��@+�m@+�
@+"�@+o@*�@*�@*��@*��@*��@*��@*�\@*^5@*J@)�#@)��@)X@)�@)%@(�`@(��@(�9@(�9@(��@(��@(��@(�u@(�@(bN@(bN@(b@'|�@';d@&�y@&v�@&5?@&{@%��@%`B@%/@%V@%�@%V@$�@$��@$�j@$�D@$�@$1@#�
@#ƨ@#dZ@"�@"��@"~�@"J@!�@ bN@  �@   @�;@�w@�@�P@l�@l�@l�@l�@l�@l�@l�@l�@l�@;d@�@�+@@/@/@�@V@��@��@�/@�/@��@�@z�@Z@I�@�@ƨ@dZ@33@@�@��@~�@M�@=q@�@��@��@��@G�@%@�`@�9@�u@�@bN@1'@�;@��@��@|�@�@�y@�y@�@��@�+@ff@E�@$�@{@��@�@V@�@�j@�D@9X@��@�
@ƨ@��@��@��@S�@@��@^5@-@�@��@x�@&�@%@��@Ĝ@Ĝ@Ĝ@Ĝ@�9@�@A�@1'@�@�@�@�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�|�A��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��\A��DA�v�A�M�A�G�A�33A��A�C�A��A�S�A� �A�A��;A���A���A�dZA�K�A�JA��A�Q�A��A��A�^5A�9XA�33A�-A� �A���A�&�A�v�A�~�A�\)A�M�A�{A�p�A�M�A���A���A�%A��uA���A�dZA���A��DA�^5A��-A���A�\)A�9XA�VA�A���A�~�A�7LA��A��+A�x�A�VA���A��7A��A��RA��A�A�|�A��A�dZA���A���A�-A�ȴA��jA�A�jA��HA�ĜA�hsA���A��jA��!A� �A�;dA��A�p�A�9XA�7LA��^A�r�A�1A�33A�=qA��FA���A���A�n�A�A�A��A���A���A���A��A�=qA���A�oA��wAS�A~�RA~A{��AzĜAzM�Az�Ayp�Ax1'AvZAu+AtE�Ar��Aq�-Aqt�Ap�yAop�Am�mAmoAj{Ag�Af�+Ac��A_��A_|�A^��A]�A\{A[t�AYt�AW�
AV  AS�#AR(�AP��AN�AN�AMl�AKdZAI�AH�AF~�AE��ADv�AA�;AA;dA>ZA<Q�A8n�A7p�A7
=A5G�A3t�A2v�A1�#A1/A0ffA.�uA-�hA,ZA+�TA+��A+`BA*ȴA)��A(��A'7LA&bNA$M�A#��A#`BA#+A"�HA!�hA ZA�
A�hAȴA�A��A/A�RA��AVA��A+AĜA�wA7LAffAx�A�`Ar�Ap�A�A�9AA�AJA�A�FA�-A��A
��A�jA�AQ�A�mA��AZA�
AƨA�AƨA7LA �RA n�A 5?@�
=@�`B@�|�@�?}@���@�@�n�@���@�@���@�9X@���@�ƨ@�+@�~�@�O�@�  @�l�@��@���@�h@�b@��@�V@���@�-@�`B@��u@��@ߥ�@�"�@އ+@��#@�hs@���@� �@�o@ٲ-@�1'@֏\@�O�@ԓu@��;@ӶF@ӕ�@�|�@�+@�v�@��@с@��@ЋD@�Q�@��@Ώ\@��/@˾w@�M�@�O�@�Q�@Ə\@�E�@��#@�x�@��@��;@§�@��T@��-@���@�G�@�7L@�(�@��@�o@�$�@��T@��@���@���@���@�Z@��;@��
@���@��P@�l�@�;d@��@��-@�X@�V@���@�j@�9X@�  @��
@���@�l�@�+@���@��R@��!@��!@��!@��!@���@���@�n�@�5?@���@���@�x�@�`B@�X@��@�z�@�(�@��;@��P@�K�@��@��H@���@��@�I�@���@��@�-@��-@���@��@��@�1@���@�
=@�ff@�X@��@�b@��w@�l�@�
=@���@�ff@��@��-@�hs@��@���@��`@���@��@��@�{@��^@��7@�hs@�7L@��@���@�l�@��@���@�^5@�5?@�$�@��7@��9@� �@�S�@�@�ff@�E�@�$�@�$�@�@��#@��-@��7@�7L@���@��j@��j@��9@��9@��@��u@�r�@�bN@�bN@�b@��;@��
@��w@��w@���@�|�@�dZ@�S�@�+@�@���@��@��R@���@���@���@�5?@��T@�O�@�/@��@�r�@�1'@� �@�(�@� �@�1'@���@��m@��
@���@���@�ƨ@��w@���@��@��@���@���@��P@�|�@�l�@�\)@�S�@�;d@�+@��@�o@�@��@��y@��H@��H@��@��@���@�ȴ@��R@��!@��\@�@���@��h@��h@�x�@��@��@�j@�A�@��
@�|�@�t�@�l�@�l�@�dZ@�"�@��H@���@���@�V@��@���@��h@�x�@�X@�O�@�7L@���@�bN@�1'@���@�|�@�;d@��y@�ff@�5?@��@��@�`B@�G�@�&�@���@�I�@�C�@�
=@��R@���@�hs@�/@�%@��/@��9@���@�I�@��m@���@�
=@�ȴ@���@�v�@�-@���@��@���@�j@�Q�@� �@�w@l�@
=@~$�@}?}@|��@|�D@|Z@{�
@{C�@z��@y��@x�u@xA�@x �@x  @w��@wl�@wK�@w�@v�y@v�R@v{@up�@s�
@rM�@q�@q&�@p�@p1'@o\)@n��@nE�@m��@l�@ko@j��@jn�@i��@iG�@i%@h��@g�@g�w@gK�@fȴ@fV@f@e@e��@eO�@d�/@cS�@b�H@b��@b��@bn�@bJ@ax�@`�@_�;@_K�@_
=@^��@^5?@]p�@\�@\�/@\�j@\j@\9X@[ƨ@[33@Zn�@Y��@Y��@X��@Xb@W�;@W�w@W�P@WK�@V�@VE�@U�T@U�h@UO�@U?}@Tz�@So@Rn�@RJ@Q��@Q�^@Q��@QG�@P��@P�9@Pr�@P  @O�w@O|�@Ol�@OK�@N�R@M�@M@M`B@L��@Lz�@L(�@Kƨ@K��@K�@K�@Kt�@KdZ@KC�@J�H@J��@J~�@J~�@JM�@J=q@JJ@I��@I��@I�7@I&�@HĜ@H�u@HQ�@Hb@G�P@F��@F�@Fȴ@FV@F{@E�T@E�h@EV@D�@D9X@Co@B��@Bn�@B^5@B=q@B�@BJ@A7L@@ �@?�@>�@>$�@=?}@<�@<z�@;��@;t�@;S�@;33@;o@;@;@;@;@;@;@;@;@:�@:n�@:�@9�@9��@9�^@9��@9x�@9hs@9X@9X@9X@9X@9X@9G�@9G�@9&�@8��@8�9@8�9@8bN@7�w@7��@7��@7�w@7��@7��@7��@7�P@6�@6��@6v�@6V@6{@5�@5�T@5�T@5�T@5�T@5��@5`B@4z�@4(�@3�m@3��@3��@3�@3�@3C�@3"�@2�@2��@2�!@2n�@2-@1�@1��@1G�@0�@0�@0�@0bN@01'@0 �@0 �@0b@0  @/�@/�w@/+@/+@/
=@.�@.ȴ@.��@.ff@.$�@-�h@-?}@-?}@-?}@-/@,�j@+��@+�m@+�
@+"�@+o@*�@*�@*��@*��@*��@*��@*�\@*^5@*J@)�#@)��@)X@)�@)%@(�`@(��@(�9@(�9@(��@(��@(��@(�u@(�@(bN@(bN@(b@'|�@';d@&�y@&v�@&5?@&{@%��@%`B@%/@%V@%�@%V@$�@$��@$�j@$�D@$�@$1@#�
@#ƨ@#dZ@"�@"��@"~�@"J@!�@ bN@  �@   @�;@�w@�@�P@l�@l�@l�@l�@l�@l�@l�@l�@l�@;d@�@�+@@/@/@�@V@��@��@�/@�/@��@�@z�@Z@I�@�@ƨ@dZ@33@@�@��@~�@M�@=q@�@��@��@��@G�@%@�`@�9@�u@�@bN@1'@�;@��@��@|�@�@�y@�y@�@��@�+@ff@E�@$�@{@��@�@V@�@�j@�D@9X@��@�
@ƨ@��@��@��@S�@@��@^5@-@�@��@x�@&�@%@��@Ĝ@Ĝ@Ĝ@Ĝ@�9@�@A�@1'@�@�@�@�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBJBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDB
=B
=B
=B	7B	7B1B1B1B1BDBJBJBJBJB
=B	7B%B  B�B��BǮB��B�FB�9B�FB�?B�3B�B�oB�PB�B�=B�VB�1Bv�B^5BQ�Bl�B|�Bu�BZB7LB#�B�B{BDB��B��B�mB�B�B�B�yB�BB�
B��BŢB�B�B�LB�'B��B�DBu�BhsBXBD�B)�B��B��BƨBŢBŢB��B�?B�{B�By�Bs�BdZBL�B2-B%�B#�B�B
=B
��B
�B
�B
��B
��B
�XB
�B
��B
��B
��B
��B
��B
�oB
�\B
�%B
{�B
r�B
iyB
ZB
N�B
I�B
D�B
7LB
2-B
/B
-B
(�B
 �B
�B
bB

=B
B	��B	��B	��B	�B	�fB	�;B	��B	ŢB	�jB	�B	��B	��B	��B	�\B	�7B	�B	z�B	r�B	jB	aHB	ZB	T�B	M�B	H�B	D�B	=qB	6FB	,B	"�B	�B	�B	PB	+B��B�B�;B�#B�B��B��B��B��BȴBƨBÖB��B�wB�jB�dB�^B�RB�9B�'B�B��B��B��B��B��B��B��B��B��B��B�oB�VB�JB�=B�=B�7B�1B�%B�B� B|�By�Bu�Bt�Bs�Bq�Bp�Bo�Bn�Bn�Bn�Bm�Bm�Bl�BjBjBo�Bo�Bo�Bm�BiyBiyBl�Bn�Bq�Bp�Bo�Bo�Bn�Bm�Bm�Bm�Bs�Bt�Bu�Bv�Bw�Bw�Bv�Bv�Bu�Bw�Bx�Bx�Bx�Bz�B{�B|�B|�B{�B|�B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�1B�DB�VB�\B�bB�hB�oB�oB�oB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B�!B�'B�-B�3B�3B�?B�FB�FB�LB�LB�LB�RB�LB�RB�jBÖBŢBȴBȴBȴB��B��B��B��B��B��B��B��B�B�)B�;B�BB�HB�`B�mB�sB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	+B	1B	
=B	DB	JB	JB	\B	�B	�B	$�B	&�B	'�B	.B	0!B	2-B	7LB	7LB	7LB	7LB	9XB	<jB	?}B	A�B	C�B	I�B	J�B	K�B	N�B	P�B	R�B	T�B	W
B	XB	ZB	_;B	e`B	l�B	m�B	n�B	n�B	n�B	p�B	t�B	w�B	z�B	{�B	� B	�B	�B	�7B	�VB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�-B	�3B	�3B	�9B	�?B	�FB	�FB	�FB	�LB	�LB	�LB	�LB	�XB	�^B	�qB	�qB	�qB	��B	��B	��B	��B	��B	��B	��B	B	B	B	B	B	ÖB	ĜB	ĜB	ŢB	ĜB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�5B	�;B	�HB	�NB	�NB	�ZB	�fB	�mB	�mB	�sB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
+B
1B
	7B

=B
DB
DB
DB
JB
JB
JB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
+B
,B
-B
.B
.B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
49B
5?B
5?B
5?B
5?B
5?B
5?B
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
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
?}B
@�B
@�B
@�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
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
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
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
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
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
P�B
P�B
P�B
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
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
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
YB
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
[#B
ZB
ZB
ZB
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
dZB
e`B
e`B
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
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iy1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B<B*B=B!BZB^B1B9B9BMB@B;B9BDB;BQBVB�B
SB
�B
�B	ZB	�B	�B
�B	�B
�BB�B�BuBBB	�BBuB��B��B��B��B��B�KB�_B��B�kB��B�cB�B��B��B�gB�@B{�Ba%BPnBlhBBy�B`JB9�B%*B�B�B:B��B�2B�FB�7B�YB�FB��B�B�lB��B�fB��B��B��B�NB��B��Bx�BkeBZ�BH�B3�BB�B��BőB��BB�!B��B��Bz�Bw�Bj}BTtB4�B&"B$�B yB:BB
�B
܆B
��B
��B
�B
��B
��B
��B
�wB
�B
�eB
�BB
�YB
��B
~�B
v*B
p�B
`B
P�B
K�B
J�B
9�B
3xB
/�B
/B
,QB
%�B
�B
�B
�B
DB	��B	�lB	�B	�@B	�B	�B	�zB	�bB	�{B	�jB	�}B	�8B	��B	��B	��B	��B	~�B	wB	o�B	eqB	]�B	Y:B	O<B	JB	H�B	@^B	:B	/CB	$�B	:B	B	�B	�B�hB��B�B�1B�NB�HB�NB΀B̞B��B�hB�)BÆB��B�B�B��B��B�LB��B�MB�/B�|B�B�NB��B�B��B��B�IB��B� B��B�OB�fB��B��B��B��B��B��B~hB��Bx"Bv2Bt�Bt6Bt�Br�Bo�BoBn�Bn'Bm�Bl�Bm@BohBp7BpBp�Bq�BlZBj�Bl�Bp�Bt	Bq�Bp�BpHBo(Bo=Bo�BpBv�Bx^Bz4BxRBxtBx�Bw�Bw�BzBz�By�BzBz�B}(B|�B}�B}�B~=B�B�B�B��B�LB��B�tB��B��B�	B�9B�RB��B��B��B��B��B��B��B�ZB��B�tB��B��B��B��B�iB�0B�B�B�=B��B�B�`B��B�-B��B� B�,B�-B�{B��B��B��B��B��B�=B��B�jB��B�iB��B��B�(B��B��BƻB��B��B�B�)B͌B��B��B�BB�"B�TB��B��B��BߺB�uB�,B�B��B�B��B��B�B��B�B�B�B�B�B��B��B�B�;B�;B�4B�cB�B��B�lB�B	�B	�B	�B	�B	
�B	�B	�B	\B	�B	mB	sB	&B	'�B	)�B	/B	0"B	2VB	8B	8bB	8|B	9B	:�B	=uB	@B	BB	D9B	JsB	KB	L�B	OCB	QXB	S\B	U+B	W1B	XyB	Z�B	`�B	f�B	l�B	m�B	n�B	n�B	o;B	q�B	u0B	x-B	{6B	|iB	�-B	�$B	��B	�B	��B	�B	��B	�5B	��B	��B	��B	��B	�B	�
B	�B	�LB	��B	�!B	�B	�B	�
B	�B	�-B	�:B	�(B	�B	��B	�fB	�2B	�FB	�+B	�QB	�lB	�TB	�KB	�|B	��B	�QB	�|B	�yB	�bB	�IB	�UB	�	B	��B	�WB	��B	��B	��B	��B	��B	�xB	��B	�rB	��B	��B	§B	B	B	B	B	ùB	ĉB	ĞB	ŹB	ĨB	īB	źB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�3B	��B	ԜB	�B	�B	�DB	�B	��B	�@B	�^B	��B	מB	�B	�B	�B	�B	�yB	ڍB	�CB	݆B	޺B	��B	��B	�^B	�vB	�B	�eB	�B	�*B	�B	��B	��B	�5B	��B	��B	�@B	��B	��B	�#B	��B	��B	��B	�JB	�B	��B	�B	�0B	��B	�YB	�'B	�B	�"B	�#B	�B	�SB	�vB
bB
�B
RB
B
eB
qB
�B
�B
}B
jB
DB
kB
~B
]B
fB
�B
�B
	�B

BB
bB
�B
�B
�B
�B
=B
�B
hB
kB
}B
�B
tB
{B
~B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
�B
�B
�B
UB
�B
�B
B
�B
�B
�B
$B
�B
�B
B
�B
�B
�B
�B
�B
 
B
�B
!
B
 �B
 �B
 �B
! B
"(B
"eB
#6B
$4B
#�B
$B
$B
$QB
%*B
%�B
%�B
&B
%�B
&*B
&<B
'aB
'4B
(#B
(�B
)OB
)B
)B
*B
*$B
*BB
*ZB
+AB
+6B
+$B
,B
+�B
,�B
-yB
.QB
.=B
/!B
/!B
/\B
/hB
/1B
/EB
/jB
0NB
0KB
0+B
01B
0uB
1�B
1@B
1_B
1yB
2WB
2\B
3lB
3MB
39B
3-B
39B
4=B
4HB
4qB
4^B
4GB
59B
4UB
5CB
5\B
5gB
5=B
5hB
5�B
6�B
6cB
6nB
7tB
7�B
7�B
8hB
8[B
8�B
8yB
9wB
9�B
9�B
9�B
:�B
;*B
;�B
;�B
<qB
<yB
<yB
<xB
<�B
=(B
=�B
?B
>�B
@!B
@�B
@�B
AB
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
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
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FB
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
HCB
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K!B
KCB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
M7B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
N3B
NB
N�B
N�B
N�B
O+B
OXB
O�B
O�B
PZB
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q B
QB
QB
QB
RB
RB
RB
Q�B
Q�B
Q�B
S B
R�B
R�B
R�B
R�B
R�B
R�B
SB
R�B
S&B
SPB
T!B
T-B
TCB
U%B
UB
U&B
UEB
UB
VB
U�B
VB
VB
VB
VB
V#B
VLB
V
B
V$B
WB
WNB
WSB
X&B
XIB
X[B
X�B
Y�B
ZHB
Z0B
Z/B
Z,B
Z#B
Z.B
Z-B
[B
[B
[B
[B
[B
[!B
[B
ZB
Z>B
ZfB
[hB
[�B
\�B
\&B
]7B
]6B
]5B
](B
]BB
]-B
]7B
]?B
]FB
];B
]2B
]HB
]_B
^kB
^OB
^OB
_AB
_iB
_MB
_XB
_@B
_KB
_PB
_WB
_[B
`wB
`lB
`UB
``B
`SB
aNB
a\B
akB
a~B
aQB
aEB
a�B
a�B
blB
bHB
bVB
bpB
bdB
bbB
clB
ciB
caB
c�B
c�B
c�B
dpB
d|B
d~B
d�B
d�B
dnB
ekB
epB
e]B
elB
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
gtB
ghB
ghB
ghB
gwB
g�B
h�B
h|B
h�B
h�B
hjB
hsB
huB
hpB
is1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<1U<F8M<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*]<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'i�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.04 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201609291649592016092916495920160929164959  AO  ARCAADJP                                                                    20160419151415    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160419151415  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160419151415  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20160929164959  QC  PRES            @�ffD��G�O�                PM  ARSQCTM V1.1                                                                20160929164959  QC  PSAL            @�ffD��G�O�                PM  ARSQOWGUV1.0                                                                20181103100327  IP                  G�O�G�O�G�O�                