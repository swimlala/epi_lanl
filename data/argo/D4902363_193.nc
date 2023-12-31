CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-12-27T00:35:19Z creation;2017-12-27T00:35:22Z conversion to V3.1;2019-12-19T07:53:16Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171227003519  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_193                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�?�� 1   @�?�o� @;t���dV�4֡b1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۃ3D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@��HAp�A!p�AAp�Aap�A��RA��RA��RA��RA��RAиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C 
C
C
C
C
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
C0
C2
C4
C6
C8
C:
C<
C>
C@
CB
CD
CF0�CH
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
Cj
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C�RC��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�\D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,\D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI�)DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\�)D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D�D���D��D�B�DÂ�D���D��D�B�DĂ�D���D��D�B�Dł�D���D��D�B�DƂ�D���D��D�B�Dǂ�D���D��D�B�DȂ�D���D��D�B�Dɂ�D���D��D�B�Dʂ�D���D��D�B�D˂�D���D��D�B�D̂�D���D��D�B�D͂�D���D��D�B�D΂�D���D��D�B�Dς�D���D��D�B�DЂ�D���D��D�B�Dт�D���D��D�B�D҂�D���D��D�B�Dӂ�D���D��D�B�DԂ�D���D��D�B�DՂ�D���D��D�B�Dւ�D���D��D�?�D��D���D��D�B�D؂�D���D��D�B�Dق�D���D�D�FDڂ�D���D��D�B�DۆD���D��D�B�D܂�D���D��D�B�D݂�D���D��D�B�Dނ�D���D��D�B�D߂�D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D��{D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ZA�ZA�\)A�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�`BA�`BA�`BA�`BA�`BA�bNA�`BA�bNA�^5A�\)A�ZA�S�A�O�A�M�A�M�A�M�A�M�A�M�A�M�A�I�A�C�A�A�A�?}A�A�A�?}A�=qA�?}A�A�A�?}A�33A�(�A� �A�1A���A���A��wA��A��PA�K�A�oA��
A�|�A��-A���A�7LA���A��!A�Q�A��A��A���A��/A�C�A� �A��DA�p�A��A��A��A�A�A�bA��RA�K�A�|�A��A���A�|�A���A��A�^5AdZA~ȴA}�TA}�A|1Ay�Ay?}Ax �Av��AvA�Au\)At~�AsƨArbNAo�hAn��Am/Ak��AjJAf��Ae+Ad~�Ac�TAcC�AcoAbȴAb�+Aa�Aa��Aa�^AaXA`�+A`^5A_�FA_7LA^��A^��A^��A^bNA^=qA^  A]��A]
=A[��AZ��AZ�\AZ5?AY��AX=qAW��AV�yAUl�AR��AR�AR��ARVARA�AQ��AQ��AQp�AP��AO��AO�AO
=AM��AKAJ��AJĜAJ�AI��AH5?AG�PAFn�AD�`AD1AC�7AC&�AB��AB �A@v�A?O�A>z�A=�^A<~�A;�FA:��A:1'A933A7/A6�RA6�DA6=qA4�A3�mA3oA1��A0bA/&�A.�A-�A-S�A-�A,��A-A,�uA+�A+��A+G�A*�HA*5?A(��A'�^A&��A%�A$��A$=qA#�A!x�A  �A%A1An�A�#A�^At�A�yA��A��A^5A  AƨA�A^5A$�A�A�A�A��AĜA��A��A�A�!A1'A��A7LAoA�!A��A��Ar�A�PA��AbNA-A�#Al�A
��A
jA	�-AjAt�AG�A7LA"�A�yA^5A�wA�`AhsA�\A�PA �jA �A 9X@�ƨ@�l�@���@��u@��@��7@��@�Ĝ@�@�1@��@�E�@�&�@�w@�w@�~�@�-@���@�1@���@�$�@���@�hs@�V@߾w@�@��@�+@ڇ+@�-@ٺ^@أ�@�b@��
@ץ�@�V@�1@�
=@���@��@�`B@Гu@�9X@�1'@��H@�-@�hs@�Z@˅@�E�@��`@���@Ł@�r�@�33@�~�@�-@�p�@�
=@�Q�@��;@�K�@�S�@�K�@��y@�{@��@��`@�r�@��m@�;d@��y@�M�@�@�@�@�X@��9@�dZ@��^@��j@��;@�33@���@��T@�r�@�{@�@�p�@��@���@��@�E�@�{@��@���@�X@�V@��@�Ĝ@�9X@�l�@��+@�hs@��@��j@�b@��;@�K�@��R@��@��j@�j@�I�@�(�@�t�@�
=@��@�ȴ@���@�v�@�^5@�$�@��-@��9@�j@�Q�@� �@��
@�t�@�S�@��@�=q@��/@� �@��;@���@���@�@��j@�ƨ@��@��y@�-@��T@��@�?}@��@�Ĝ@��;@�l�@�C�@���@���@�ff@�M�@�$�@�J@���@�`B@��/@��j@���@�j@�9X@�1@��m@���@�t�@�+@��@�^5@�@�`B@��@��D@�I�@��w@��@���@��@��@�v�@�-@�@���@�p�@��@���@�Ĝ@��D@�j@�9X@��@|�@�@~�y@~�+@~5?@}�T@}��@}�-@}?}@|�@|�@{ƨ@{C�@z�\@z�\@z~�@z�@yhs@xb@w�@v�@v�+@vE�@v{@u��@u�h@u?}@t�@t��@t(�@s��@r�@qX@q%@q�^@o�;@o
=@o�P@o�;@o��@o;d@o|�@o��@p1'@q�@q��@q��@pĜ@pA�@n��@l�D@l�/@m`B@m�h@m�@m�@l�@l�j@l��@l�D@lj@l�@k��@k��@k��@k�F@k�@k�@k��@k��@k��@k��@k��@k��@k��@k��@k��@k�@kdZ@k"�@j��@j~�@j=q@jJ@hĜ@h�@h  @g�w@g��@fȴ@fE�@ep�@e/@eV@d�@d�@d�@d�@c��@c��@cS�@cC�@b��@bn�@b-@bJ@a�@a�#@a�7@a7L@`�`@`��@` �@_�;@_�w@^��@^��@^�+@^E�@]�@\Z@[��@[�
@[t�@[C�@["�@Z�@Z~�@Y�@Y��@Y�^@Y��@Y�7@YG�@Y&�@Y%@X��@X�`@X�@X �@W��@W;d@W;d@W+@V�R@U�@U�@UO�@UO�@UO�@T�@T�@TI�@Sƨ@SC�@Rn�@Q�@Qhs@P�`@Pb@O�@O
=@N��@N$�@M�@L��@L�@K��@K�@J��@J~�@I�#@I&�@H�`@HĜ@H�u@HbN@HQ�@HQ�@HQ�@HQ�@HQ�@HA�@HA�@H �@Hb@H  @G|�@Gl�@G\)@G\)@G�@F�+@FV@F{@E�T@E@E�-@E��@E�h@EV@D�@D��@D�@Dj@DI�@D1@C�m@Ct�@B�H@BM�@A��@@��@?��@?\)@>V@=��@=�-@=p�@<��@<�@;�
@;��@;S�@:��@:�!@:n�@:-@9��@9�#@9�#@9�^@9G�@9%@8�@7�;@7K�@7+@7\)@7;d@6�@6��@6��@6E�@6$�@6$�@6{@5�@5@5��@5��@5�h@5�h@5�@5?}@5�@5�@4��@4z�@4j@4j@4j@41@3��@3��@3�m@3ƨ@3�@333@3@2�@2�\@2M�@2-@1�@1��@1�#@1hs@1G�@17L@0��@0�u@0b@/|�@.V@-p�@-?}@-V@,�D@+��@+�
@+ƨ@+ƨ@+��@+dZ@+C�@+"�@*�H@*��@*M�@)��@)X@)&�@(Ĝ@(Q�@'�w@'l�@'K�@';d@'+@'+@&ȴ@&�+@&5?@&$�@%�@%�T@%�T@%�h@%O�@%/@$��@$�@$�@$��@$j@$�@$1@#��@#�m@#�m@#�F@#o@"�!@"~�@"~�@"n�@"-@"J@!�@!��@!��@!��@!�^@!��@!hs@!�@ �`@ �9@ 1'@�;@|�@�@�@��@$�@�T@�-@�h@�@`B@?}@V@�@(�@��@��@t�@dZ@C�@33@33@"�@"�@"�@o@o@@@�H@�!@~�@�@�#@��@X@�@�`@Ĝ@��@r�@1'@ �@�@�w@l�@
=@�@v�@@�T@��@��@��@�T@�T@��@/@�@�j@��@z�@I�@�@��@�m@�
@ƨ@dZ@"�@"�@"�@o@��@^5@M�@=q@�@��@�@�@�#@�#@��@hs@G�@&�@�@%@��@Ĝ@�9@��@�@r�@Q�@  @�@�@�;@l�@;d@�@�y@�@�R@��@v�@E�@{@�T@��@p�@`B@V@�@��@��@��@�j@z�@j@Z@I�@9X@�@�@�@�@�@�@�m@�@t�@S�@33@o@
�H@
��@
M�@
J@	��@	��@	x�@	&�@�`@��@A�@A�@ �@b@  @�@��@��@�@��@E�@5?@{@@�T@�T@��@�-@�h@p�@�@��@��@��@��@��@��@j@1@�F@��@�@S�@C�@S�@33@33@"�@o@��@�@�@�^@�7@X@ ��@ �9@ �9@ ��@ �u@ bN@ Q�@  �@ b@ b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ZA�ZA�\)A�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�`BA�`BA�`BA�`BA�`BA�bNA�`BA�bNA�^5A�\)A�ZA�S�A�O�A�M�A�M�A�M�A�M�A�M�A�M�A�I�A�C�A�A�A�?}A�A�A�?}A�=qA�?}A�A�A�?}A�33A�(�A� �A�1A���A���A��wA��A��PA�K�A�oA��
A�|�A��-A���A�7LA���A��!A�Q�A��A��A���A��/A�C�A� �A��DA�p�A��A��A��A�A�A�bA��RA�K�A�|�A��A���A�|�A���A��A�^5AdZA~ȴA}�TA}�A|1Ay�Ay?}Ax �Av��AvA�Au\)At~�AsƨArbNAo�hAn��Am/Ak��AjJAf��Ae+Ad~�Ac�TAcC�AcoAbȴAb�+Aa�Aa��Aa�^AaXA`�+A`^5A_�FA_7LA^��A^��A^��A^bNA^=qA^  A]��A]
=A[��AZ��AZ�\AZ5?AY��AX=qAW��AV�yAUl�AR��AR�AR��ARVARA�AQ��AQ��AQp�AP��AO��AO�AO
=AM��AKAJ��AJĜAJ�AI��AH5?AG�PAFn�AD�`AD1AC�7AC&�AB��AB �A@v�A?O�A>z�A=�^A<~�A;�FA:��A:1'A933A7/A6�RA6�DA6=qA4�A3�mA3oA1��A0bA/&�A.�A-�A-S�A-�A,��A-A,�uA+�A+��A+G�A*�HA*5?A(��A'�^A&��A%�A$��A$=qA#�A!x�A  �A%A1An�A�#A�^At�A�yA��A��A^5A  AƨA�A^5A$�A�A�A�A��AĜA��A��A�A�!A1'A��A7LAoA�!A��A��Ar�A�PA��AbNA-A�#Al�A
��A
jA	�-AjAt�AG�A7LA"�A�yA^5A�wA�`AhsA�\A�PA �jA �A 9X@�ƨ@�l�@���@��u@��@��7@��@�Ĝ@�@�1@��@�E�@�&�@�w@�w@�~�@�-@���@�1@���@�$�@���@�hs@�V@߾w@�@��@�+@ڇ+@�-@ٺ^@أ�@�b@��
@ץ�@�V@�1@�
=@���@��@�`B@Гu@�9X@�1'@��H@�-@�hs@�Z@˅@�E�@��`@���@Ł@�r�@�33@�~�@�-@�p�@�
=@�Q�@��;@�K�@�S�@�K�@��y@�{@��@��`@�r�@��m@�;d@��y@�M�@�@�@�@�X@��9@�dZ@��^@��j@��;@�33@���@��T@�r�@�{@�@�p�@��@���@��@�E�@�{@��@���@�X@�V@��@�Ĝ@�9X@�l�@��+@�hs@��@��j@�b@��;@�K�@��R@��@��j@�j@�I�@�(�@�t�@�
=@��@�ȴ@���@�v�@�^5@�$�@��-@��9@�j@�Q�@� �@��
@�t�@�S�@��@�=q@��/@� �@��;@���@���@�@��j@�ƨ@��@��y@�-@��T@��@�?}@��@�Ĝ@��;@�l�@�C�@���@���@�ff@�M�@�$�@�J@���@�`B@��/@��j@���@�j@�9X@�1@��m@���@�t�@�+@��@�^5@�@�`B@��@��D@�I�@��w@��@���@��@��@�v�@�-@�@���@�p�@��@���@�Ĝ@��D@�j@�9X@��@|�@�@~�y@~�+@~5?@}�T@}��@}�-@}?}@|�@|�@{ƨ@{C�@z�\@z�\@z~�@z�@yhs@xb@w�@v�@v�+@vE�@v{@u��@u�h@u?}@t�@t��@t(�@s��@r�@qX@q%@q�^@o�;@o
=@o�P@o�;@o��@o;d@o|�@o��@p1'@q�@q��@q��@pĜ@pA�@n��@l�D@l�/@m`B@m�h@m�@m�@l�@l�j@l��@l�D@lj@l�@k��@k��@k��@k�F@k�@k�@k��@k��@k��@k��@k��@k��@k��@k��@k��@k�@kdZ@k"�@j��@j~�@j=q@jJ@hĜ@h�@h  @g�w@g��@fȴ@fE�@ep�@e/@eV@d�@d�@d�@d�@c��@c��@cS�@cC�@b��@bn�@b-@bJ@a�@a�#@a�7@a7L@`�`@`��@` �@_�;@_�w@^��@^��@^�+@^E�@]�@\Z@[��@[�
@[t�@[C�@["�@Z�@Z~�@Y�@Y��@Y�^@Y��@Y�7@YG�@Y&�@Y%@X��@X�`@X�@X �@W��@W;d@W;d@W+@V�R@U�@U�@UO�@UO�@UO�@T�@T�@TI�@Sƨ@SC�@Rn�@Q�@Qhs@P�`@Pb@O�@O
=@N��@N$�@M�@L��@L�@K��@K�@J��@J~�@I�#@I&�@H�`@HĜ@H�u@HbN@HQ�@HQ�@HQ�@HQ�@HQ�@HA�@HA�@H �@Hb@H  @G|�@Gl�@G\)@G\)@G�@F�+@FV@F{@E�T@E@E�-@E��@E�h@EV@D�@D��@D�@Dj@DI�@D1@C�m@Ct�@B�H@BM�@A��@@��@?��@?\)@>V@=��@=�-@=p�@<��@<�@;�
@;��@;S�@:��@:�!@:n�@:-@9��@9�#@9�#@9�^@9G�@9%@8�@7�;@7K�@7+@7\)@7;d@6�@6��@6��@6E�@6$�@6$�@6{@5�@5@5��@5��@5�h@5�h@5�@5?}@5�@5�@4��@4z�@4j@4j@4j@41@3��@3��@3�m@3ƨ@3�@333@3@2�@2�\@2M�@2-@1�@1��@1�#@1hs@1G�@17L@0��@0�u@0b@/|�@.V@-p�@-?}@-V@,�D@+��@+�
@+ƨ@+ƨ@+��@+dZ@+C�@+"�@*�H@*��@*M�@)��@)X@)&�@(Ĝ@(Q�@'�w@'l�@'K�@';d@'+@'+@&ȴ@&�+@&5?@&$�@%�@%�T@%�T@%�h@%O�@%/@$��@$�@$�@$��@$j@$�@$1@#��@#�m@#�m@#�F@#o@"�!@"~�@"~�@"n�@"-@"J@!�@!��@!��@!��@!�^@!��@!hs@!�@ �`@ �9@ 1'@�;@|�@�@�@��@$�@�T@�-@�h@�@`B@?}@V@�@(�@��@��@t�@dZ@C�@33@33@"�@"�@"�@o@o@@@�H@�!@~�@�@�#@��@X@�@�`@Ĝ@��@r�@1'@ �@�@�w@l�@
=@�@v�@@�T@��@��@��@�T@�T@��@/@�@�j@��@z�@I�@�@��@�m@�
@ƨ@dZ@"�@"�@"�@o@��@^5@M�@=q@�@��@�@�@�#@�#@��@hs@G�@&�@�@%@��@Ĝ@�9@��@�@r�@Q�@  @�@�@�;@l�@;d@�@�y@�@�R@��@v�@E�@{@�T@��@p�@`B@V@�@��@��@��@�j@z�@j@Z@I�@9X@�@�@�@�@�@�@�m@�@t�@S�@33@o@
�H@
��@
M�@
J@	��@	��@	x�@	&�@�`@��@A�@A�@ �@b@  @�@��@��@�@��@E�@5?@{@@�T@�T@��@�-@�h@p�@�@��@��@��@��@��@��@j@1@�F@��@�@S�@C�@S�@33@33@"�@o@��@�@�@�^@�7@X@ ��@ �9@ �9@ ��@ �u@ bN@ Q�@  �@ b@ b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BbBhBhBbBbBbBbBbBhBhBbBbBhBbBbBhBbBbBbBbBbBbBbBbBbBhBhBbBbBbBhBhBhBhBhBhBhBhBhBbBhBhBhBhBbBhBbBPB+B��B�/B�LBo�B/BŢBz�BXBE�B+B+B+B�B�BPB  B
��B
��B
�B
��B
��B
��B
ƨB
�XB
�'B
�B
�-B
��B
��B
�oB
�\B
�1B
�1B
� B
{�B
p�B
\)B
_;B
W
B
J�B
K�B
D�B
;dB
6FB
)�B
�B
�B
	7B
  B	��B	�BB	�TB	�`B	�TB	�BB	�HB	�;B	�5B	�B	�B	�B	��B	��B	��B	��B	ǮB	ȴB	ȴB	ŢB	ĜB	B	��B	�dB	�FB	�B	��B	�B	��B	��B	��B	�oB	�JB	�B	r�B	~�B	� B	}�B	|�B	z�B	v�B	u�B	o�B	k�B	jB	e`B	]/B	O�B	O�B	R�B	M�B	G�B	>wB	:^B	2-B	)�B	&�B	%�B	$�B	�B	�B	DB	+B	B	B��B��B��B�B�B�5B�sB�yB�ZB�#B��B��BɺB��BÖB��BÖBƨBƨBŢBŢB��B�qB�}B�qB�^B�?B�B��B��B��B��B��B�PB�B�B�B�Bz�B~�B�B�B~�B�B�B~�B{�Bz�Bu�Bq�Br�Bq�Bk�BffB]/B^5BXBP�BVB[#BXBXBXB\)B[#B_;B`BB]/BXBW
B]/B\)BYBW
BT�BP�BM�BH�BL�BT�BVBS�BP�BL�BK�BG�BC�BD�BF�BD�BL�BH�BF�BD�B6FB33B9XB:^B<jB9XB/B2-B49B6FB2-B,B%�B1'B8RB7LB1'B1'B5?B5?B49B2-B-B'�B(�B0!B1'B33B2-B2-B5?B6FB49B.B(�B33B8RB6FB6FB7LB8RB9XB33B5?B5?B5?B5?B33B33B0!B5?B9XB9XB<jB>wB:^B5?B49BD�BF�BL�BN�BL�BJ�BK�BP�BP�BP�BQ�BS�BT�BT�BW
BT�BP�BM�BJ�BK�BQ�BR�BVBYBXBS�BQ�BcTBcTBaHB`BBcTBhsBl�Bl�Bl�Bk�Bn�Bo�Bo�Bn�Bp�Bq�Bs�B{�B|�B{�B� B~�B� B�B�B�PB�bB�bB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B�'B�FB�FB�9B�-B�?B�qBŢBŢBǮB��B��B��B��B��B��B�/B�NB�NB�ZB�mB�B�B�B�B�B�B��B��B��B��B��B	B	B	B	%B	1B	
=B	\B	{B	�B	�B	�B	�B	�B	!�B	#�B	$�B	$�B	'�B	'�B	+B	+B	,B	.B	1'B	1'B	2-B	33B	49B	5?B	7LB	:^B	;dB	=qB	?}B	A�B	A�B	A�B	C�B	D�B	I�B	K�B	N�B	S�B	T�B	T�B	XB	ZB	^5B	bNB	cTB	e`B	gmB	gmB	iyB	l�B	n�B	p�B	r�B	s�B	t�B	s�B	u�B	y�B	u�B	u�B	{�B	~�B	� B	�B	�B	�B	�7B	�VB	�hB	�oB	�hB	�oB	�bB	�DB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�'B	�-B	�-B	�3B	�9B	�LB	�^B	�jB	�wB	�}B	�wB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�#B	�/B	�5B	�/B	�)B	�/B	�HB	�NB	�NB	�ZB	�`B	�fB	�`B	�fB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
1B
%B
1B
+B
1B
DB
JB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
VB
\B
\B
\B
\B
VB
\B
bB
hB
hB
oB
oB
oB
oB
uB
{B
uB
uB
{B
{B
{B
uB
uB
{B
{B
uB
uB
�B
{B
�B
�B
�B
�B
�B
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
"�B
$�B
%�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
+B
-B
.B
.B
.B
-B
/B
/B
/B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
1'B
2-B
33B
2-B
2-B
2-B
2-B
2-B
49B
7LB
8RB
7LB
8RB
9XB
:^B
:^B
9XB
:^B
:^B
:^B
:^B
:^B
9XB
9XB
:^B
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
<jB
<jB
=qB
?}B
?}B
@�B
@�B
@�B
@�B
B�B
B�B
B�B
C�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
C�B
C�B
D�B
F�B
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
G�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
L�B
L�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
T�B
VB
VB
VB
VB
T�B
S�B
S�B
T�B
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
VB
W
B
XB
XB
XB
W
B
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
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
[#B
\)B
]/B
]/B
]/B
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
`BB
`BB
aHB
aHB
aHB
`BB
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
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
dZB
dZB
cTB
dZB
e`B
e`B
ffB
ffB
ffB
gmB
ffB
ffB
ffB
e`B
ffB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
m�B
m�B
m�B
l�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BbBhBhBbBbBHBbBbBhBhBbBbBhBHBbBNBbBHBHBbBbBbBHBHBbBhBhBbBbBHBhBhBNBhBhBhBhBhB�B}B�B�B�B�B�B�B�B�B�B�B�\B��Bv�B7�BҽB�uB]IBKB0!B-�B,�B!�BCB�BB
��B
ּB
یB
֡B
οB
�\B
�zB
��B
��B
��B
��B
�QB
�|B
��B
��B
��B
�B
� B
|�B
r|B
^�B
_�B
X_B
LdB
LdB
E�B
<�B
7�B
,B
�B
�B
DB
AB	�*B	�B	��B	�B	�B	��B	�B	ߤB	ބB	��B	�QB	�KB	՛B	οB	�BB	�~B	�KB	��B	��B	��B	��B	��B	��B	�B	�LB	��B	��B	�QB	�fB	��B	�
B	�uB	�jB	��B	u%B	.B	�4B	~BB	}"B	{JB	wLB	v+B	p�B	lqB	kB	fLB	^�B	Q�B	P�B	S[B	N�B	H�B	@4B	;B	3�B	+�B	'�B	&�B	%zB	 vB	�B	PB	�B	9B	-B�jB��B��B��B�B�BB��B��B�,B��B�SB�2BˬBB��B��B�MB��B��B��B��B�'B�BB� B��B��B�`B��B�ZB�B�QB�dB��B��B�_B��B��B��B|�B�B�MB�uB�B�AB�UB}B|jB{dBv�Br|BsBr-Bl=BgmB^�B_�BY�BS&BW
B[�BX�BX�BX�B\xB[�B_pB`\B]�BYKBXB]~B\�BY�BW�BU�BQ�BOBJ=BM�BUBV9BT,BQhBM�BL�BIBE�BE�BG�BE�BMBIlBG_BEmB8�B5%B:^B;dB<�B:B0�B3MB5B6�B3B-]B(XB1�B8�B7�B2aB1�B5�B5�B4�B2�B.B)DB*0B0�B1�B3�B2�B2�B5�B6�B4�B/B*B3�B8�B6�B6�B7�B8�B9�B4B5�B5�B5�B5�B4B4TB1�B6FB:B:*B<�B>�B;JB6�B5�BD�BGBL�BOBM6BKxBLdBQBQNBQhBRoBT,BUgBUMBW
BUBQhBN�BK�BL�BR�BS�BV�BYBX�BUBSuBc�Bc�Ba�BaBc�Bh�Bl�Bl�Bl�Bk�Bn�Bo�Bo�BoBq[BraBtnB|6B}VB|PB�OB}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�+B��B�B��B�$B�DB�6B�yB��B��B��B��B��B��B��B�+B��B��B�%B�1B�B�.B�B�B�gB՛B�dB�hB�B�B�B�B�B�B��B��B��B�B��B�B�B�B	 B	AB	SB	YB	fB	
�B	�B	�B	�B	�B	�B	B	B	!�B	#�B	$�B	%FB	($B	($B	+B	+6B	,=B	.IB	1'B	1AB	2aB	3hB	4nB	5�B	7fB	:xB	;B	=�B	?�B	A�B	A�B	A�B	C�B	D�B	I�B	K�B	N�B	S�B	T�B	UB	XEB	Z�B	^�B	bhB	c�B	e`B	gmB	g�B	i�B	l�B	n�B	p�B	r�B	s�B	uB	tB	u�B	y�B	v`B	vB	{�B	~�B	�B	� B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�TB	�gB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�!B	�!B	�B	�'B	�'B	�B	�-B	�GB	�3B	�TB	�fB	�xB	��B	�wB	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	� B	�B	�B	��B	�B	�B	�2B	�B	�$B	�B	�1B	�1B	�1B	�KB	�7B	�B	�QB	�#B	�CB	�WB	�/B	�OB	�IB	�]B	ݘB	�HB	�hB	�hB	�tB	�`B	�B	�zB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�	B	��B	�*B	�"B	�<B	�.B
 B
oB
MB
9B
YB
1B
tB
1B
_B
fB
DB
dB
dB
dB
6B
PB
6B
PB
6B
PB
6B
jB
PB
PB
pB
\B
\B
vB
\B
pB
\B
bB
hB
hB
TB
�B
�B
�B
�B
�B
�B
uB
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
$�B
%�B
%B
%�B
%�B
%�B
&�B
'�B
(
B
'�B
)B
)�B
)�B
)�B
)�B
*B
*B
+B
+�B
+B
-)B
-�B
.B
-�B
-)B
/B
/ B
/B
./B
/5B
/B
0!B
0!B
0UB
1AB
1'B
1'B
2-B
2B
1AB
2GB
3MB
2GB
2GB
2GB
2aB
2�B
4nB
7fB
8RB
7fB
8�B
9rB
:^B
:DB
9XB
:xB
:xB
:xB
:^B
:xB
9rB
9rB
:^B
;dB
;B
;B
;B
<jB
=qB
=qB
=VB
=qB
<�B
<�B
=qB
?cB
?�B
@iB
@�B
@�B
@�B
B�B
B�B
BuB
C{B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
C�B
C�B
D�B
F�B
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
G�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
L�B
MB
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
Q B
Q B
RB
RB
Q�B
Q�B
Q�B
R�B
RB
RB
RB
R B
SB
S&B
TB
T�B
VB
U�B
VB
VB
T�B
TB
T,B
T�B
VB
VB
VB
VB
VB
V�B
V�B
W
B
W
B
VB
W
B
XB
XB
X+B
W?B
X+B
X�B
YB
Y1B
Y1B
YB
ZB
ZB
ZB
ZB
ZB
[=B
ZB
[	B
[#B
[#B
[#B
[	B
[#B
[#B
[=B
[#B
[#B
\)B
\B
\)B
[WB
\CB
]/B
]IB
]IB
^OB
^5B
^OB
^OB
^OB
^OB
_VB
_;B
_;B
_VB
`BB
`\B
aHB
aHB
a-B
`BB
aHB
a-B
aHB
aHB
aHB
b4B
b4B
b4B
bNB
bNB
abB
abB
bNB
bhB
bhB
bNB
bhB
bhB
bNB
bhB
bNB
d@B
dtB
cTB
dtB
e`B
ezB
ffB
f�B
ffB
gmB
ffB
f�B
ffB
ezB
f�B
gmB
hsB
iyB
i_B
i�B
i_B
i_B
i�B
iyB
i�B
i�B
jB
kkB
kkB
kkB
kkB
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
n}B
m�B
m�B
m�B
m�B
m�B
l�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Q�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.09(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712310035412017123100354120171231003541201806221235272018062212352720180622123527201804050431462018040504314620180405043146  JA  ARFMdecpA19c                                                                20171227093517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171227003519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171227003521  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171227003521  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171227003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171227003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171227003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171227003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171227003522  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171227003522                      G�O�G�O�G�O�                JA  ARUP                                                                        20171227005543                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171227153219  CV  JULD            G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20171227153219  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20171227153219  CV  LONGITUDE       G�O�G�O��"��                JM  ARCAJMQC2.0                                                                 20171230153541  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171230153541  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193146  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033527  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                