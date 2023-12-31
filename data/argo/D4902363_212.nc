CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-21T21:35:42Z creation;2018-02-21T21:35:57Z conversion to V3.1;2019-12-19T07:48:48Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180221213542  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_212                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�N2�� 1   @�N3�[�@:}B�����deKƧ�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @5@�z�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
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
=CQ�CT
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
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�~D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��{D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�>D�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�D{D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A� �A�5?A�&�A�+A�1'A�C�A�E�A�C�A�=qA�$�A���A��TA���A�Q�A�A��A���A��!A���A�~�A�ZA��A���A��!A�O�A�9XA��A�bA���A�%A��mA�ȴA��7A��7A�-A���A��A��TA�dZA�t�A� �A�C�A�;dA�%A��A�7LA��#A��A� �A��A���A���A���A��#A�9XA�p�A�\)A��A��
A�A�A���A�=qA�=qA��#A��+A���A�A�ƨA�?}A�Q�A���A�v�A�?}A��A��DA�C�A���A��TA��A��+A���A��9A�ZA���A��A���A|��Az{Ay�TAy
=Aul�Ar��Aq�mAqS�Ao�FAnJAl�RAkXAi��Af�RAf=qAe�-Ae�PAe��Ae��Ae?}Ad9XAb��Ab�!Ab��Ab��Ab��AaC�A`��A_VAZ~�AZ-AY��AX5?AW7LAU�PAUC�AU�AT��AT�9ASƨAS�AR��ARJAP��AP��AP~�AO`BAN��ANI�AN-AM�ALM�ALJAK�PAI�;AH��AG�mAF�yAE�AE�ADJAC?}ABE�A@~�A>�9A=�A<�`A:��A9��A8JA5�
A5�A4�jA3hsA1�;A1�A1oA1VA1VA1VA1A0�yA0��A0jA0JA/7LA-��A,-A+p�A+33A*�\A)��A)%A(�+A(=qA'��A'7LA&A�A%O�A%�A%%A$��A#�A"ĜA"1A r�A��AjA�A�A��A��AĜA5?A�A�A�`A��AffAI�A  AȴAE�A��A��A�RA�A1'AdZAr�A{A
��A	��A��AQ�A��A
=A�
A�9A%AdZA J@��7@��/@�I�@��@���@���@�33@��@���@�\)@�{@��/@�Ĝ@��@��`@蛦@�j@�bN@睲@�@�u@�@�t�@�|�@�7L@�z�@���@��#@��@؃@ם�@��y@��@�v�@�hs@�I�@�|�@�ȴ@�&�@�@Ͳ-@�V@���@�I�@�ȴ@�x�@���@�^5@ă@Ý�@�\)@��@�`B@�Z@� �@��@���@�t�@�"�@��+@�@�@�X@��`@��P@�E�@��h@�V@���@��u@�|�@�o@��R@��T@��@�/@���@�dZ@��h@��/@��j@��j@��9@��@���@��@��@�Z@� �@���@�-@���@��@��@��\@�=q@�J@���@�O�@���@�r�@�
=@���@���@��m@��P@�"�@��@���@�~�@�V@�$�@�@��^@��@�O�@���@�r�@�I�@��;@���@�M�@�{@���@�(�@���@���@�M�@���@���@��^@���@�x�@�`B@�V@���@��j@�9X@�;d@�~�@�@��-@�G�@�/@�7L@�7L@�7L@�/@�j@��w@���@�|�@�\)@�+@�ȴ@�ff@�@�?}@�Z@��m@���@�l�@��@��R@�=q@���@�?}@�/@��/@�z�@�1@���@��w@���@�t�@�\)@�C�@�+@���@���@��!@��!@��\@�M�@�@�-@��@��@�@��7@�X@�/@��`@�I�@��w@��@���@�l�@�C�@��@�v�@���@���@���@��R@��+@�ff@�V@�E�@�5?@�@��-@���@��/@���@���@��u@�z�@�I�@��@���@�\)@�;d@�+@�o@��y@��@�x�@�&�@��/@�1'@�  @�  @l�@~ȴ@~v�@~V@}��@}O�@}/@|�/@|��@|I�@|�@{�m@{��@{@z=q@y�@y7L@x�9@w��@vȴ@v�+@v5?@u�h@t��@t9X@sƨ@sS�@r��@r=q@r=q@rJ@q�7@qhs@qx�@qX@q%@pĜ@p�9@p�u@pr�@pr�@pb@o��@o�@o|�@o;d@n��@n��@n5?@n$�@m�@m��@m@m�-@m`B@l��@lj@lI�@l�@l1@l�D@lI�@k�@j�\@jn�@jM�@j�@i�#@ix�@i7L@i&�@hr�@h1'@hb@g�w@fv�@fE�@f�@f��@fE�@f{@e@ep�@e`B@e?}@eV@d��@d�/@d�/@d�@dZ@d�@c�F@c"�@b��@b�\@bJ@a�#@a��@a&�@a%@`Ĝ@`�9@`�u@`  @_�P@_\)@^��@^��@^v�@]�T@\��@[�m@[S�@Z��@Z-@Y7L@X�@XbN@XA�@X  @W�;@W��@W�@W��@W|�@W�@V��@V�@Vff@U@Up�@UV@T��@T��@Tz�@T(�@S�F@S�@SC�@R�@R��@Q��@Q&�@P�9@Pr�@O�;@O+@N�y@N�@Nȴ@N�+@Nv�@Nv�@Nv�@N�+@N�+@N�+@N�+@Nv�@Nv�@Nff@Nff@NE�@M��@M`B@M?}@M�@L��@L��@L1@JJ@I��@Ix�@Ihs@IX@IG�@IG�@I7L@I%@HĜ@H�@HQ�@G�;@GK�@F�y@Fȴ@Fv�@F$�@E��@E/@D�@D�D@D1@C"�@B�@B��@BM�@B�@BJ@A�^@A�7@Ax�@A�@@��@@bN@@Q�@?�@?��@?;d@>ȴ@>v�@>$�@=/@<�j@<�@<z�@<�@;�F@;33@:��@:~�@:^5@:=q@:-@9��@9hs@9&�@8�`@8�@8A�@8 �@8  @7�;@7�@7�P@7;d@7;d@6��@6�y@6ȴ@6��@6��@6�+@6ff@5�-@5/@5�@4��@4��@4�j@4Z@4I�@49X@4(�@4�@3��@3ƨ@3��@3C�@2�H@2~�@2M�@2-@2�@1�7@0��@0��@0bN@/�;@/�;@/�;@/��@/��@/�P@/\)@/+@.�@.E�@.{@.{@.@-�T@-�-@-`B@-?}@,��@,��@,��@,��@,Z@+ƨ@+C�@*��@*��@*��@*~�@*^5@*=q@*J@)��@)�7@)x�@)X@)G�@(��@(��@(r�@(A�@(  @'�;@'�P@';d@'
=@&�R@&v�@&$�@%�@%�h@%?}@%�@$�j@$j@$9X@#��@#�m@#�@"�@"��@"�!@"n�@!�@!�7@!hs@!hs@!X@ �`@ Ĝ@ Ĝ@ ��@ A�@�@�;@��@�@�P@|�@K�@
=@��@v�@E�@5?@�T@��@@@�@�@V@�/@��@z�@(�@ƨ@��@t�@"�@��@�!@��@n�@M�@�@�@J@��@��@�^@�^@��@hs@G�@7L@&�@%@��@�`@�`@Ĝ@r�@1'@b@��@+@�@�+@ff@ff@V@V@5?@{@@�@�T@�h@?}@��@�/@��@�@�@�@�@��@�D@j@I�@1@t�@C�@33@"�@"�@o@o@@�H@��@�\@�@��@7L@&�@�`@�@r�@r�@Q�@A�@ �@�@��@l�@�@�@��@ff@{@@��@`B@V@�@�j@�D@z�@j@Z@�@��@ƨ@�@33@
��@
��@
�\@
~�@
n�@
^5@
=q@
�@
J@
J@	�#@	��@	x�@	G�@	&�@	%@��@�`@��@Ĝ@��@�u@�u@�@Q�@b@  @�;@��@�w@�@�@��@��@�P@\)@;d@��@�@��@v�@V@E�@E�@V@V@E�@{@��@�-@�h@`B@?}@�@�@�@��@��@�D@I�@(�@�@�m@ƨ@�@t�@dZ@S�@33@o@@@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A� �A�5?A�&�A�+A�1'A�C�A�E�A�C�A�=qA�$�A���A��TA���A�Q�A�A��A���A��!A���A�~�A�ZA��A���A��!A�O�A�9XA��A�bA���A�%A��mA�ȴA��7A��7A�-A���A��A��TA�dZA�t�A� �A�C�A�;dA�%A��A�7LA��#A��A� �A��A���A���A���A��#A�9XA�p�A�\)A��A��
A�A�A���A�=qA�=qA��#A��+A���A�A�ƨA�?}A�Q�A���A�v�A�?}A��A��DA�C�A���A��TA��A��+A���A��9A�ZA���A��A���A|��Az{Ay�TAy
=Aul�Ar��Aq�mAqS�Ao�FAnJAl�RAkXAi��Af�RAf=qAe�-Ae�PAe��Ae��Ae?}Ad9XAb��Ab�!Ab��Ab��Ab��AaC�A`��A_VAZ~�AZ-AY��AX5?AW7LAU�PAUC�AU�AT��AT�9ASƨAS�AR��ARJAP��AP��AP~�AO`BAN��ANI�AN-AM�ALM�ALJAK�PAI�;AH��AG�mAF�yAE�AE�ADJAC?}ABE�A@~�A>�9A=�A<�`A:��A9��A8JA5�
A5�A4�jA3hsA1�;A1�A1oA1VA1VA1VA1A0�yA0��A0jA0JA/7LA-��A,-A+p�A+33A*�\A)��A)%A(�+A(=qA'��A'7LA&A�A%O�A%�A%%A$��A#�A"ĜA"1A r�A��AjA�A�A��A��AĜA5?A�A�A�`A��AffAI�A  AȴAE�A��A��A�RA�A1'AdZAr�A{A
��A	��A��AQ�A��A
=A�
A�9A%AdZA J@��7@��/@�I�@��@���@���@�33@��@���@�\)@�{@��/@�Ĝ@��@��`@蛦@�j@�bN@睲@�@�u@�@�t�@�|�@�7L@�z�@���@��#@��@؃@ם�@��y@��@�v�@�hs@�I�@�|�@�ȴ@�&�@�@Ͳ-@�V@���@�I�@�ȴ@�x�@���@�^5@ă@Ý�@�\)@��@�`B@�Z@� �@��@���@�t�@�"�@��+@�@�@�X@��`@��P@�E�@��h@�V@���@��u@�|�@�o@��R@��T@��@�/@���@�dZ@��h@��/@��j@��j@��9@��@���@��@��@�Z@� �@���@�-@���@��@��@��\@�=q@�J@���@�O�@���@�r�@�
=@���@���@��m@��P@�"�@��@���@�~�@�V@�$�@�@��^@��@�O�@���@�r�@�I�@��;@���@�M�@�{@���@�(�@���@���@�M�@���@���@��^@���@�x�@�`B@�V@���@��j@�9X@�;d@�~�@�@��-@�G�@�/@�7L@�7L@�7L@�/@�j@��w@���@�|�@�\)@�+@�ȴ@�ff@�@�?}@�Z@��m@���@�l�@��@��R@�=q@���@�?}@�/@��/@�z�@�1@���@��w@���@�t�@�\)@�C�@�+@���@���@��!@��!@��\@�M�@�@�-@��@��@�@��7@�X@�/@��`@�I�@��w@��@���@�l�@�C�@��@�v�@���@���@���@��R@��+@�ff@�V@�E�@�5?@�@��-@���@��/@���@���@��u@�z�@�I�@��@���@�\)@�;d@�+@�o@��y@��@�x�@�&�@��/@�1'@�  @�  @l�@~ȴ@~v�@~V@}��@}O�@}/@|�/@|��@|I�@|�@{�m@{��@{@z=q@y�@y7L@x�9@w��@vȴ@v�+@v5?@u�h@t��@t9X@sƨ@sS�@r��@r=q@r=q@rJ@q�7@qhs@qx�@qX@q%@pĜ@p�9@p�u@pr�@pr�@pb@o��@o�@o|�@o;d@n��@n��@n5?@n$�@m�@m��@m@m�-@m`B@l��@lj@lI�@l�@l1@l�D@lI�@k�@j�\@jn�@jM�@j�@i�#@ix�@i7L@i&�@hr�@h1'@hb@g�w@fv�@fE�@f�@f��@fE�@f{@e@ep�@e`B@e?}@eV@d��@d�/@d�/@d�@dZ@d�@c�F@c"�@b��@b�\@bJ@a�#@a��@a&�@a%@`Ĝ@`�9@`�u@`  @_�P@_\)@^��@^��@^v�@]�T@\��@[�m@[S�@Z��@Z-@Y7L@X�@XbN@XA�@X  @W�;@W��@W�@W��@W|�@W�@V��@V�@Vff@U@Up�@UV@T��@T��@Tz�@T(�@S�F@S�@SC�@R�@R��@Q��@Q&�@P�9@Pr�@O�;@O+@N�y@N�@Nȴ@N�+@Nv�@Nv�@Nv�@N�+@N�+@N�+@N�+@Nv�@Nv�@Nff@Nff@NE�@M��@M`B@M?}@M�@L��@L��@L1@JJ@I��@Ix�@Ihs@IX@IG�@IG�@I7L@I%@HĜ@H�@HQ�@G�;@GK�@F�y@Fȴ@Fv�@F$�@E��@E/@D�@D�D@D1@C"�@B�@B��@BM�@B�@BJ@A�^@A�7@Ax�@A�@@��@@bN@@Q�@?�@?��@?;d@>ȴ@>v�@>$�@=/@<�j@<�@<z�@<�@;�F@;33@:��@:~�@:^5@:=q@:-@9��@9hs@9&�@8�`@8�@8A�@8 �@8  @7�;@7�@7�P@7;d@7;d@6��@6�y@6ȴ@6��@6��@6�+@6ff@5�-@5/@5�@4��@4��@4�j@4Z@4I�@49X@4(�@4�@3��@3ƨ@3��@3C�@2�H@2~�@2M�@2-@2�@1�7@0��@0��@0bN@/�;@/�;@/�;@/��@/��@/�P@/\)@/+@.�@.E�@.{@.{@.@-�T@-�-@-`B@-?}@,��@,��@,��@,��@,Z@+ƨ@+C�@*��@*��@*��@*~�@*^5@*=q@*J@)��@)�7@)x�@)X@)G�@(��@(��@(r�@(A�@(  @'�;@'�P@';d@'
=@&�R@&v�@&$�@%�@%�h@%?}@%�@$�j@$j@$9X@#��@#�m@#�@"�@"��@"�!@"n�@!�@!�7@!hs@!hs@!X@ �`@ Ĝ@ Ĝ@ ��@ A�@�@�;@��@�@�P@|�@K�@
=@��@v�@E�@5?@�T@��@@@�@�@V@�/@��@z�@(�@ƨ@��@t�@"�@��@�!@��@n�@M�@�@�@J@��@��@�^@�^@��@hs@G�@7L@&�@%@��@�`@�`@Ĝ@r�@1'@b@��@+@�@�+@ff@ff@V@V@5?@{@@�@�T@�h@?}@��@�/@��@�@�@�@�@��@�D@j@I�@1@t�@C�@33@"�@"�@o@o@@�H@��@�\@�@��@7L@&�@�`@�@r�@r�@Q�@A�@ �@�@��@l�@�@�@��@ff@{@@��@`B@V@�@�j@�D@z�@j@Z@�@��@ƨ@�@33@
��@
��@
�\@
~�@
n�@
^5@
=q@
�@
J@
J@	�#@	��@	x�@	G�@	&�@	%@��@�`@��@Ĝ@��@�u@�u@�@Q�@b@  @�;@��@�w@�@�@��@��@�P@\)@;d@��@�@��@v�@V@E�@E�@V@V@E�@{@��@�-@�h@`B@?}@�@�@�@��@��@�D@I�@(�@�@�m@ƨ@�@t�@dZ@S�@33@o@@@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BDBJBPBDBDBPBVBPBJBDB1BB%B  B��B��B�B��BB%BBB  B��BB��BBBBBB��B��B�B�B�`B��BĜB�?B�XB��B�9BŢB��BŢB�}B�3B�B��B��B��B�=Bq�BaHBXBVB@�B"�BB��B�B��B{�Bw�Bv�BYBG�B,B:^B/BhB	7B
�B
��B
�B
ƨB
ǮB
�qB
��B
��B
��B
�\B
��B
�VB
u�B
l�B
\)B
&�B
�B
=qB
(�B	��B	��B

=B
DB	��B	�fB	�ZB	�B	ȴB	�B	��B	��B	��B	��B	��B	ǮB	�jB	�B	�qB	��B	�}B	�?B	��B	��B	w�B	<jB	y�B	s�B	aHB	q�B	_;B	s�B	t�B	n�B	iyB	\)B	P�B	W
B	R�B	F�B	S�B	VB	F�B	J�B	K�B	J�B	>wB	8RB	>wB	49B	�B	�B	 �B	�B	hB	�B	\B	JB��B�B�mB�B�ZB��B��BŢB�-BȴB��B�dB�?B��B��B��B��B��B��BȴBŢB�}B�XB�B��B��B��B�B��B��B��B��B��B��B��B�bB�PB��B��B�hB�Bz�B{�Bk�Be`By�Bo�BZBXBdZB^5BdZBcTBe`B\)BO�BL�BaHB[#BK�BS�BXBP�BF�B=qB9XB>wB=qB@�B0!B6FB33B=qB8RB/B!�B!�B{BhB�B�B)�B+B'�B$�B"�B�B#�B#�B �B#�B�B
=B1B�B&�B&�B$�B�BuB�B�BuB��BB�BhB�B�B�B�B�B!�B�B{B{B{B�BVBVB�B�B"�B�BoBuBhB�B�B!�B(�B%�B�B%�B2-B33B2-B2-B1'B.B/B1'B/B,B%�B%�B1'B2-B6FB49B.B7LB7LB49B7LB7LB49B,B+B=qBD�BG�BF�BF�BF�BE�BC�B@�B>wB9XB2-B7LB;dBJ�BK�BO�BP�BO�BN�BM�BM�BE�BI�BQ�BYB`BBaHBe`BgmBgmBiyBiyBjBiyBjBjBiyBiyBm�BjBgmBo�Bs�Bp�BgmBp�B�B�B�B�7B�=B�=B�=B�=B�7B�DB�7B�%B�%B�PB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B��B�B�B�B�'B�XB�wB��B�wB��B��BBǮB��B��B��B��B��B�B�B�B�B�#B�#B�#B�)B�NB�mB�mB�sB�yB�B�B��B��B��B��B��B��B��B��B	%B	+B	+B	1B	JB	VB	�B	�B	�B	!�B	!�B	"�B	'�B	(�B	(�B	&�B	&�B	)�B	$�B	+B	0!B	0!B	0!B	0!B	/B	9XB	;dB	>wB	?}B	>wB	<jB	8RB	8RB	@�B	@�B	>wB	E�B	H�B	F�B	H�B	L�B	N�B	L�B	N�B	R�B	S�B	S�B	VB	XB	YB	XB	W
B	XB	^5B	]/B	`BB	`BB	bNB	hsB	iyB	iyB	jB	p�B	s�B	u�B	x�B	|�B	�B	�7B	�=B	�hB	�{B	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�9B	�FB	�?B	�3B	�9B	�^B	�^B	�^B	�^B	�dB	�jB	�qB	�dB	�wB	�}B	�wB	�dB	��B	ƨB	ŢB	ĜB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�#B	�#B	�#B	�)B	�B	�B	�B	�5B	�HB	�HB	�NB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B	��B
1B
DB
JB
JB
JB
JB
JB
DB
DB
DB
JB
DB
DB
JB
VB
PB
PB
PB
JB
VB
VB
PB
PB
oB
oB
hB
uB
uB
uB
uB
{B
uB
uB
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
%�B
&�B
&�B
&�B
&�B
%�B
'�B
'�B
(�B
(�B
'�B
(�B
(�B
'�B
%�B
'�B
)�B
+B
,B
+B
+B
-B
-B
-B
-B
-B
-B
,B
,B
,B
-B
.B
.B
.B
,B
,B
/B
/B
.B
1'B
1'B
0!B
0!B
0!B
0!B
/B
/B
/B
2-B
33B
33B
2-B
2-B
1'B
33B
33B
33B
49B
33B
2-B
1'B
2-B
49B
7LB
7LB
7LB
7LB
8RB
7LB
7LB
9XB
9XB
9XB
9XB
8RB
9XB
9XB
:^B
9XB
:^B
9XB
9XB
:^B
:^B
;dB
;dB
<jB
;dB
<jB
=qB
=qB
=qB
?}B
?}B
@�B
>wB
>wB
A�B
B�B
A�B
@�B
B�B
D�B
E�B
E�B
D�B
E�B
F�B
E�B
E�B
E�B
H�B
H�B
H�B
H�B
H�B
G�B
G�B
G�B
H�B
I�B
J�B
I�B
K�B
K�B
K�B
J�B
I�B
K�B
K�B
K�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
P�B
P�B
P�B
R�B
S�B
T�B
VB
VB
VB
VB
VB
VB
VB
T�B
T�B
T�B
VB
XB
XB
XB
XB
XB
XB
XB
XB
W
B
W
B
VB
VB
XB
YB
ZB
ZB
ZB
ZB
YB
YB
YB
XB
W
B
YB
YB
\)B
[#B
[#B
]/B
]/B
]/B
]/B
]/B
\)B
\)B
[#B
[#B
\)B
\)B
]/B
\)B
^5B
]/B
]/B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
_;B
`BB
`BB
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
cTB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
ffB
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
gmB
hsB
gmB
hsB
hsB
hsB
iyB
jB
k�B
jB
jB
jB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
m�B
m�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
o�B
o�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BDB0B6B^BDB6B<BPBdB^B�B�BYB �B��B�$B�B�}BmBYBgBoB �B��BUB��BABGB-B;BB�<B�B�3B��B�LBևBƨB�2B��B��B��B�mB��B�?B�B��B��B��B��B�=B�^Bs�Bc�BZ7BW�BCB'8BYB��B�CB��B��By�BxB\)BJ	B/ B;B0oB�BDB
�wB
�B
��B
��B
ȚB
��B
�$B
�
B
��B
��B
��B
�\B
w�B
m�B
^B
,B
�B
=�B
*�B
 4B	��B
)B
dB	��B	�B	��B	�B	��B	��B	�B	�vB	�&B	��B	�B	�fB	��B	�}B	�qB	�oB	�}B	�B	�jB	�_B	z�B	A;B	z*B	t�B	cTB	r�B	`�B	s�B	t�B	o B	jB	]IB	Q�B	W�B	S�B	G�B	T,B	VmB	HB	K�B	LdB	K)B	?�B	9>B	?B	5?B	!�B	!B	!�B	�B	B	QB	�B	�B��B��B�B��B��BЗB�oB��B��BɠB�~B�<B��B�UB��B��B��B��B�B�B��B�B�*B�IB��B��B��B�wB��B��B��B�vB�NB�jB��B��B�pB��B��B�B�uB|PB}"Bm�Bg8Bz^Bp�B\�BY�BezB_�BeBdBe�B]IBQ�BNVBa�B[�BMjBT�BX�BQ�BG�B?B;0B?�B>�BAUB2GB7�B4nB=�B9>B0oB#�B#nB�B�BIB�B*eB+�B(�B%�B#�B1B$�B$�B!�B$�B�B�B	�B 'B'B'B%,B�B�BSB$B,B�BB�B�BoBeBIB5B]B)B!�B/B2B2BB?B�B�BeB BB# BOB�B{B�B�B�B"NB)DB&fB �B&�B2GB3hB2|B2GB1vB.�B/iB1vB/�B,�B&�B&�B1�B2�B6zB4�B/ B7�B7�B4�B7�B7�B4�B-)B,WB=�BD�BG�BF�BF�BF�BE�BC�B@�B>�B9�B33B88B<PBKBL0BP.BQ BP.BOBBNVBNVBF�BJ�BR�BY�B`vBa�Be�Bg�Bg�Bi�Bi�Bj�Bi�Bj�Bj�Bi�Bi�Bm�Bj�Bh>BpBtBq'Bh�BqvB�gB�uB�mB�lB�XB�rB�XB�rB��B�xB�lB��B��B��B��B��B��B��B��B��B��B��B�5B�TB�"B�)B�=B�6B�KB�WB��B��B��B��B��B��B��B��B�B�B��B��B�B�B�BB�,B�+B�EB�EB�7B�#B�=B�WB�]B�NB�B�B�B�B��B��B��B�B��B�B�B�0B�^B�]B	%B	+B	EB	fB	�B	�B	xB	�B	�B	!�B	!�B	"�B	'�B	)B	)B	'B	'B	*B	%`B	+6B	0!B	0;B	0UB	0UB	/�B	9XB	;B	>�B	?�B	>�B	<�B	9	B	8�B	@�B	@�B	>�B	E�B	H�B	F�B	H�B	L�B	N�B	MB	OB	SB	S�B	T,B	VB	X+B	YB	XEB	WYB	X_B	^jB	]~B	`vB	`�B	b�B	hsB	i�B	i�B	j�B	p�B	s�B	u�B	x�B	}"B	�B	�7B	�rB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�*B	�B	�!B	�AB	�AB	�9B	�+B	�ZB	��B	��B	�xB	�xB	�xB	�xB	��B	��B	��B	��B	�wB	��B	��B	��B	��B	ƎB	ŢB	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�2B	�B	�+B	�B	�1B	�?B	�KB	�#B	�WB	�=B	�]B	�eB	�eB	�B	�jB	�|B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	�B	�	B	�B	��B
B
B
B
B
�B
B
B
B
B
B
B
B
B
'B
 B
3B
?B
?B
B
MB
oB	��B
fB
DB
JB
JB
JB
JB
dB
^B
^B
^B
dB
^B
^B
dB
VB
�B
jB
�B
dB
pB
�B
jB
�B
�B
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
mB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
%�B
&�B
'B
'B
'B
%�B
'�B
(
B
(�B
(�B
(
B
(�B
)B
(
B
&2B
($B
)�B
+B
,B
+B
+B
,�B
-B
,�B
-)B
-)B
-B
,"B
,B
,=B
-)B
./B
./B
./B
,=B
,=B
/B
/B
.IB
1'B
1'B
0;B
0;B
0;B
0;B
/5B
/OB
/OB
2GB
3B
33B
2GB
2GB
1AB
3MB
33B
3MB
49B
3MB
2GB
1[B
2aB
4TB
7LB
7fB
7fB
7fB
8lB
7LB
7fB
9XB
9rB
9rB
9rB
8�B
9rB
9rB
:xB
9rB
:xB
9rB
9XB
:xB
:xB
;B
;dB
<�B
;�B
<�B
=�B
=�B
=�B
?�B
?�B
@�B
>�B
>�B
A�B
B�B
A�B
@�B
B�B
D�B
E�B
E�B
D�B
E�B
F�B
E�B
E�B
E�B
H�B
H�B
H�B
H�B
H�B
G�B
G�B
G�B
H�B
I�B
J�B
I�B
K�B
K�B
K�B
J�B
I�B
K�B
K�B
K�B
J�B
J�B
K�B
L�B
L�B
L�B
MB
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q B
P�B
Q�B
Q B
P�B
Q�B
Q�B
Q�B
RB
Q�B
Q�B
Q�B
RB
Q B
Q B
Q B
Q B
Q B
SB
TB
T�B
U�B
VB
VB
VB
VB
VB
VB
T�B
UB
T�B
VB
XB
XB
XB
XB
XB
XB
W�B
XB
W$B
W$B
VB
V9B
X+B
YB
ZB
ZB
ZB
ZB
YB
Y1B
Y1B
X+B
W?B
YB
Y1B
\B
[=B
[=B
]/B
]B
]IB
]/B
]IB
\)B
\CB
[WB
[=B
\)B
\CB
]IB
\CB
^5B
]IB
]IB
^5B
_VB
_VB
_VB
`BB
`'B
`\B
`BB
`\B
`BB
_VB
`\B
`vB
bNB
c:B
cTB
c:B
cTB
cnB
cnB
dZB
dZB
dtB
cnB
dtB
dtB
ezB
ezB
ffB
ffB
fLB
ffB
f�B
ffB
gRB
ffB
f�B
f�B
gmB
gmB
hXB
hsB
hsB
hXB
hXB
hsB
hsB
g�B
hsB
g�B
h�B
h�B
h�B
i�B
jB
k�B
jB
jB
jB
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
m�B
m�B
l�B
l�B
l�B
mwB
m�B
m�B
m�B
o�B
o�B
n}B
o�B
o�B
o�B
p�B
p�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.04(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802270045572018022700455720180227004557201806221238042018062212380420180622123804201804050434572018040504345720180405043457  JA  ARFMdecpA19c                                                                20180222063531  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180221213542  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180221213546  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180221213547  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180221213548  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180221213548  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180221213551  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180221213551  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180221213553  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180221213557                      G�O�G�O�G�O�                JA  ARUP                                                                        20180221220613                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180222153530  CV  JULD            G�O�G�O�F�q�                JM  ARCAJMQC2.0                                                                 20180226154557  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180226154557  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193457  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033804  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                