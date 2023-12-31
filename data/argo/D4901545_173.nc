CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-01-30T18:04:39Z creation      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
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
resolution        :�o     �  �|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ˔   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ψ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ߄   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0        �0Argo profile    3.1 1.2 19500101000000  20180130180439  20181025093512  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4741                            2C  D   NAVIS_A                         0185                            052512                          863 @�H�wF��1   @�H�'Ҍ&@;BM����czE����1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�� D��fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��
@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B �B�B�B�B!zB(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
�D ��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D	
�D	��D

�D
��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D�{D
�D��D
�D��D
�D��D 
�D ��D!
�D!��D"
�D"��D#
�D#��D$
�D$��D%
�D%��D&
�D&��D'
�D'��D(
�D(��D)
�D)��D*
�D*��D+
�D+��D,
�D,��D-
�D-��D.
�D.��D/
�D/��D0
�D0��D1
�D1��D2
�D2��D3
�D3��D4
�D4��D5
�D5��D6
�D6��D7
�D7��D8
�D8��D9
�D9��D:
�D:��D;
�D;��D<
�D<��D=
�D=��D>
�D>��D?
�D?��D@
�D@��DA
�DA��DB
�DB��DC
�DC��DD
�DD��DE
�DE��DF
�DF��DG
�DG��DH
�DH��DI
�DI��DJ
�DJ��DK
�DK��DL
�DL��DM
�DM��DN
�DN��DO
�DO��DP
�DP��DQ
�DQ��DR
�DR��DS
�DS��DT
�DT��DU
�DU��DV
�DV��DW
�DW��DX
�DX��DY
�DY��DZ
�DZ��D[
�D[��D\
�D\��D]
�D]��D^
�D^��D_
�D_��D`
�D`��Da
�Da��Db
�Db��Dc
�Dc��Dd
�Dd��De
�De��Df
�Df��Dg
�Dg��Dh
�Dh��Di
�Di��Dj
�Dj��Dk
�Dk��Dl
�Dl��Dm
�Dm��Dn
�Dn��Do
�Do��Dp
�Dp��Dq
�Dq��Dr
�Dr��Ds
�Ds��Dt
�Dt��Du
�Du��Dv
�Dv��Dw
�Dw��Dx
�Dx��Dy
�Dy��Dz
�Dz��D{
�D{��D|
�D|��D}
�D}��D~
�D~��D
�D��D�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD���D��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�B>D��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqDqD��qD�qD�EqDÅqD��qD�qD�EqDąqD��qD�qD�EqDŅqD��qD�qD�EqDƅqD��qD�qD�EqDǅqD��qD�qD�EqDȅqD��qD�qD�EqDɅqD��qD�qD�EqDʅqD��qD�qD�EqD˅qD��qD�qD�EqD̅qD��qD�qD�EqDͅqD��qD�qD�EqD΅qD��qD�qD�EqDυqD��qD�qD�EqDЅqD��qD�qD�EqDхqD��qD�qD�EqD҅qD��qD�qD�EqDӅqD��qD�qD�EqDԅqD��qD�qD�EqDՅqD��qD�qD�EqDօqD��qD�qD�EqDׅqD��qD�qD�EqD؅qD��qD�qD�EqDمqD��qD�qD�EqDڅqD��qD�qD�EqDۅqD��qD�qD�EqD܅qD��qD�qD�EqD݅qD��qD�qD�EqDޅqD��qD�qD�EqD߅qD��qD�qD�EqD��qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD��qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�K�D��qD���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�M�A�K�A�K�A�K�A�E�A�/A��A��/A��A�Q�A�oA�A��HA�ȴA��jA���A��hA�v�A�A�A��A��/A�~�A�XA��A��TA�r�A�1'A�C�A�dZA��PA�7LA�=qA�=qA�VA�A�\)A�1'A��RA��hA��A�hsA� �A��A���A��9A���A�x�A�G�A��#A���A���A���A��hA��\A��A��A�r�A�ZA��jA���A��A�  A���A��A�1A���A��A�I�A��-A�+A��/A�r�A�1A�ZA�\)A�
=A�A�9XA��A�XA���A���A�A�A���A���A�dZA�E�A�+A��HA}��Az�/Ax�yAv��Au��As�wAq�PAp��ApE�Ao��An��An �Amt�Al1'Aj�!Aj1'AjJAi`BAhVAg�Af  AdAcx�Ac�AbffA`��A_7LA^E�A^A]A]%A[?}AY�#AX��AV��AU��AUC�AT��ATM�AR~�AQx�APz�AO�PAO�AOAN�uAL�RAKG�AJ�!AI�mAIXAI�AH��AG�^AF�9AF=qAE��AEC�AE/ADr�AC�hAA�TA?�^A>�\A=��A=&�A<��A;��A;&�A:��A9�#A9&�A81'A7C�A7A6-A4-A2�+A1&�A0ĜA0JA.��A-`BA,�HA,5?A+�;A+��A+;dA*��A)��A(�A(JA&ȴA&-A%/A$n�A#��A#�A!��A ��A =qA�TAK�A�AVA��A9XA�A��A"�A�AAr�A��AffA�A�FA��A�A
=A�uA9XAJA�A�AAx�A�yAA�AC�A�\A��Ax�A33A�A�^A�A
��A
ȴA
�A	O�AJAS�AAn�A�A5?AdZA��A1A��AO�AoA �yA �RA =q@���@�J@�X@�j@��y@�;d@�=q@���@�-@@���@웦@���@�ff@�O�@�  @��@�I�@⟾@��/@߾w@�t�@޸R@݁@�%@�z�@۶F@�"�@��y@�V@� �@���@��@ԋD@�V@ѡ�@���@Ͼw@��H@ΰ!@ͺ^@̣�@�t�@ʏ\@�E�@�O�@ǍP@őh@öF@��H@�v�@�@�/@��D@�(�@��m@�33@�@���@�p�@��u@��;@�S�@�+@��@��D@�1'@���@�;d@�"�@��@�v�@�p�@��@�|�@�=q@��@��T@�@��@�7L@���@��D@���@�p�@���@�r�@�(�@���@��F@�|�@�V@���@�x�@��@�&�@��@���@�"�@��@��7@�V@��D@�dZ@���@���@���@�X@�b@��^@�(�@��
@��@��H@�@��9@�(�@��F@�ff@��#@��h@��@���@�r�@�Z@�(�@��@���@�v�@�5?@�hs@�hs@�O�@���@��D@�b@��w@�\)@�
=@���@�-@�-@�{@��#@�x�@�&�@��9@���@��w@�\)@��@�n�@�E�@�=q@�-@���@�`B@��@��`@���@��j@��D@�I�@�1'@� �@�  @��@�l�@�dZ@�|�@�|�@�33@��@��y@���@��R@��R@���@�v�@�=q@�{@��@���@���@���@�@��^@��^@��-@���@��7@�?}@��@�%@�V@�I�@�1@���@��
@��@��P@�t�@�C�@��@���@�ȴ@�n�@�=q@��@���@�G�@�/@�V@��`@��@��u@�Q�@��D@���@�?}@�Z@|�@�w@��@�7L@�b@~$�@}��@}�@}V@|�j@|��@|�@|j@|I�@{dZ@zn�@y�#@yhs@yhs@y7L@y&�@y7L@yx�@y�^@yhs@y�@xĜ@xĜ@xb@wK�@w
=@v�y@v��@v�@v��@v��@w�@xr�@xQ�@xQ�@y�#@z�@y��@x��@w�@x�u@wK�@w+@y�@y�@y7L@x��@y&�@y��@z�@y��@yG�@xr�@xb@w�@vȴ@v�R@wK�@x  @w|�@u�@t��@up�@u�T@up�@uO�@u`B@t�D@sdZ@o�@l9X@kƨ@l(�@k�F@kC�@j~�@j=q@jM�@j�\@jJ@i�^@jM�@j��@j�!@j��@j�@hbN@g�@g��@hQ�@g+@g
=@e�-@d�/@d�j@d�D@d�j@d�j@d��@dz�@d�@d1@e�@fV@d��@ct�@c�
@c�
@b~�@b�\@cS�@b�@b�!@b�H@b^5@a��@a�7@a7L@`��@`��@`�u@`Q�@`A�@`A�@`A�@_��@_�P@^�@^�+@^�+@^��@]�T@]��@]O�@]/@]/@\��@\�@[��@Z�@Z��@Z��@Z��@Z��@Z�!@Z�@Y��@Yhs@X��@X�@XbN@XA�@W�;@W�P@W;d@W+@V�@V�+@V{@U�@U�@V@V@U��@U`B@T�@T�j@T�@T�D@Tz�@Tj@TZ@T(�@Sƨ@S33@S33@S@R��@R�\@Rn�@R=q@R�@Q��@QG�@Q7L@Q�@P��@P�`@P��@P�9@O�;@OK�@N�y@Nv�@N@M@M�h@Mp�@M/@L�@Lj@K�
@Kƨ@K�F@K�F@K��@KdZ@KC�@K@Jn�@I�@I&�@H��@HQ�@H �@H �@G�P@G�w@G�w@G;d@G
=@F��@F�+@FE�@F@E�h@EO�@E`B@Ep�@EO�@EV@D��@D��@D��@D��@D�D@Dz�@D�@C�@Co@B��@B~�@B-@B�@A�^@A7L@A�@@A�@?�@?�@?�@?�w@?�@?��@?��@?��@?K�@=�T@=V@<��@<��@<I�@<(�@;��@;C�@;33@;"�@:��@:��@:��@:=q@9�#@9G�@8�u@8A�@81'@8 �@7�;@7K�@6ȴ@6V@5�T@5�h@4�@4��@4z�@3ƨ@3�@333@2��@2^5@1��@1��@1�7@1�7@1x�@0��@0��@0��@0r�@0bN@0Q�@0 �@0A�@0Q�@0Q�@0 �@/�@/;d@.�R@.��@.v�@.ff@.E�@.@-`B@-V@,�j@,��@,Z@,9X@+�m@+t�@+"�@*��@*��@*^5@*=q@*�@)��@)�@)��@)G�@(Ĝ@(�u@(�@(r�@(A�@( �@(b@'��@'�P@'\)@';d@&�y@&@%p�@%O�@%O�@%�@%V@$�/@$�@$��@$z�@$I�@$1@#�m@#��@#�
@#��@"��@!�#@!x�@!G�@!&�@!7L@!&�@!�@ �9@ �@ b@�@�w@l�@;d@��@�R@��@�+@$�@�T@�-@��@�@p�@O�@�@�/@j@I�@�@�
@�m@�m@�F@��@t�@dZ@C�@@~�@��@-@�@��@�#@��@hs@&�@��@�9@r�@Q�@1'@b@�@�;@��@�w@�@l�@K�@+@ȴ@�R@��@V@5?@@��@@p�@/@V@�@�/@��@�j@��@�D@z�@I�@�@1@��@�
@��@��@dZ@"�@@�@��@�\@^5@M�@-@��@�#@�#@��@��@G�@&�@��@��@�`@Ĝ@��@�@A�@b@  @�@�;@�;@��@l�@+@
=@�y@ȴ@��@ff@{@@�@�@��@�-@�-@��@p�@O�@/@V@��@��@�/@�j@�D@z�@Z@I�@9X@9X@(�@��@�
@ƨ@�F@��@dZ@C�@33@o@
�H@
��@
�!@
~�@
n�@
^5@
-@	�@	��@	x�@	7L@	%@��@�9@�u@�@�@Q�@b@�@��@��@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�K�A�K�A�K�A�E�A�/A��A��/A��A�Q�A�oA�A��HA�ȴA��jA���A��hA�v�A�A�A��A��/A�~�A�XA��A��TA�r�A�1'A�C�A�dZA��PA�7LA�=qA�=qA�VA�A�\)A�1'A��RA��hA��A�hsA� �A��A���A��9A���A�x�A�G�A��#A���A���A���A��hA��\A��A��A�r�A�ZA��jA���A��A�  A���A��A�1A���A��A�I�A��-A�+A��/A�r�A�1A�ZA�\)A�
=A�A�9XA��A�XA���A���A�A�A���A���A�dZA�E�A�+A��HA}��Az�/Ax�yAv��Au��As�wAq�PAp��ApE�Ao��An��An �Amt�Al1'Aj�!Aj1'AjJAi`BAhVAg�Af  AdAcx�Ac�AbffA`��A_7LA^E�A^A]A]%A[?}AY�#AX��AV��AU��AUC�AT��ATM�AR~�AQx�APz�AO�PAO�AOAN�uAL�RAKG�AJ�!AI�mAIXAI�AH��AG�^AF�9AF=qAE��AEC�AE/ADr�AC�hAA�TA?�^A>�\A=��A=&�A<��A;��A;&�A:��A9�#A9&�A81'A7C�A7A6-A4-A2�+A1&�A0ĜA0JA.��A-`BA,�HA,5?A+�;A+��A+;dA*��A)��A(�A(JA&ȴA&-A%/A$n�A#��A#�A!��A ��A =qA�TAK�A�AVA��A9XA�A��A"�A�AAr�A��AffA�A�FA��A�A
=A�uA9XAJA�A�AAx�A�yAA�AC�A�\A��Ax�A33A�A�^A�A
��A
ȴA
�A	O�AJAS�AAn�A�A5?AdZA��A1A��AO�AoA �yA �RA =q@���@�J@�X@�j@��y@�;d@�=q@���@�-@@���@웦@���@�ff@�O�@�  @��@�I�@⟾@��/@߾w@�t�@޸R@݁@�%@�z�@۶F@�"�@��y@�V@� �@���@��@ԋD@�V@ѡ�@���@Ͼw@��H@ΰ!@ͺ^@̣�@�t�@ʏ\@�E�@�O�@ǍP@őh@öF@��H@�v�@�@�/@��D@�(�@��m@�33@�@���@�p�@��u@��;@�S�@�+@��@��D@�1'@���@�;d@�"�@��@�v�@�p�@��@�|�@�=q@��@��T@�@��@�7L@���@��D@���@�p�@���@�r�@�(�@���@��F@�|�@�V@���@�x�@��@�&�@��@���@�"�@��@��7@�V@��D@�dZ@���@���@���@�X@�b@��^@�(�@��
@��@��H@�@��9@�(�@��F@�ff@��#@��h@��@���@�r�@�Z@�(�@��@���@�v�@�5?@�hs@�hs@�O�@���@��D@�b@��w@�\)@�
=@���@�-@�-@�{@��#@�x�@�&�@��9@���@��w@�\)@��@�n�@�E�@�=q@�-@���@�`B@��@��`@���@��j@��D@�I�@�1'@� �@�  @��@�l�@�dZ@�|�@�|�@�33@��@��y@���@��R@��R@���@�v�@�=q@�{@��@���@���@���@�@��^@��^@��-@���@��7@�?}@��@�%@�V@�I�@�1@���@��
@��@��P@�t�@�C�@��@���@�ȴ@�n�@�=q@��@���@�G�@�/@�V@��`@��@��u@�Q�@��D@���@�?}@�Z@|�@�w@��@�7L@�b@~$�@}��@}�@}V@|�j@|��@|�@|j@|I�@{dZ@zn�@y�#@yhs@yhs@y7L@y&�@y7L@yx�@y�^@yhs@y�@xĜ@xĜ@xb@wK�@w
=@v�y@v��@v�@v��@v��@w�@xr�@xQ�@xQ�@y�#@z�@y��@x��@w�@x�u@wK�@w+@y�@y�@y7L@x��@y&�@y��@z�@y��@yG�@xr�@xb@w�@vȴ@v�R@wK�@x  @w|�@u�@t��@up�@u�T@up�@uO�@u`B@t�D@sdZ@o�@l9X@kƨ@l(�@k�F@kC�@j~�@j=q@jM�@j�\@jJ@i�^@jM�@j��@j�!@j��@j�@hbN@g�@g��@hQ�@g+@g
=@e�-@d�/@d�j@d�D@d�j@d�j@d��@dz�@d�@d1@e�@fV@d��@ct�@c�
@c�
@b~�@b�\@cS�@b�@b�!@b�H@b^5@a��@a�7@a7L@`��@`��@`�u@`Q�@`A�@`A�@`A�@_��@_�P@^�@^�+@^�+@^��@]�T@]��@]O�@]/@]/@\��@\�@[��@Z�@Z��@Z��@Z��@Z��@Z�!@Z�@Y��@Yhs@X��@X�@XbN@XA�@W�;@W�P@W;d@W+@V�@V�+@V{@U�@U�@V@V@U��@U`B@T�@T�j@T�@T�D@Tz�@Tj@TZ@T(�@Sƨ@S33@S33@S@R��@R�\@Rn�@R=q@R�@Q��@QG�@Q7L@Q�@P��@P�`@P��@P�9@O�;@OK�@N�y@Nv�@N@M@M�h@Mp�@M/@L�@Lj@K�
@Kƨ@K�F@K�F@K��@KdZ@KC�@K@Jn�@I�@I&�@H��@HQ�@H �@H �@G�P@G�w@G�w@G;d@G
=@F��@F�+@FE�@F@E�h@EO�@E`B@Ep�@EO�@EV@D��@D��@D��@D��@D�D@Dz�@D�@C�@Co@B��@B~�@B-@B�@A�^@A7L@A�@@A�@?�@?�@?�@?�w@?�@?��@?��@?��@?K�@=�T@=V@<��@<��@<I�@<(�@;��@;C�@;33@;"�@:��@:��@:��@:=q@9�#@9G�@8�u@8A�@81'@8 �@7�;@7K�@6ȴ@6V@5�T@5�h@4�@4��@4z�@3ƨ@3�@333@2��@2^5@1��@1��@1�7@1�7@1x�@0��@0��@0��@0r�@0bN@0Q�@0 �@0A�@0Q�@0Q�@0 �@/�@/;d@.�R@.��@.v�@.ff@.E�@.@-`B@-V@,�j@,��@,Z@,9X@+�m@+t�@+"�@*��@*��@*^5@*=q@*�@)��@)�@)��@)G�@(Ĝ@(�u@(�@(r�@(A�@( �@(b@'��@'�P@'\)@';d@&�y@&@%p�@%O�@%O�@%�@%V@$�/@$�@$��@$z�@$I�@$1@#�m@#��@#�
@#��@"��@!�#@!x�@!G�@!&�@!7L@!&�@!�@ �9@ �@ b@�@�w@l�@;d@��@�R@��@�+@$�@�T@�-@��@�@p�@O�@�@�/@j@I�@�@�
@�m@�m@�F@��@t�@dZ@C�@@~�@��@-@�@��@�#@��@hs@&�@��@�9@r�@Q�@1'@b@�@�;@��@�w@�@l�@K�@+@ȴ@�R@��@V@5?@@��@@p�@/@V@�@�/@��@�j@��@�D@z�@I�@�@1@��@�
@��@��@dZ@"�@@�@��@�\@^5@M�@-@��@�#@�#@��@��@G�@&�@��@��@�`@Ĝ@��@�@A�@b@  @�@�;@�;@��@l�@+@
=@�y@ȴ@��@ff@{@@�@�@��@�-@�-@��@p�@O�@/@V@��@��@�/@�j@�D@z�@Z@I�@9X@9X@(�@��@�
@ƨ@�F@��@dZ@C�@33@o@
�H@
��@
�!@
~�@
n�@
^5@
-@	�@	��@	x�@	7L@	%@��@�9@�u@�@�@Q�@b@�@��@��@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBhBbBhBoBuBuBoBoB�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BPB��B��B�B�fB�
BɺB��B��B��B�
B�`B�B�B�yB�yB�sB�sB�mB�mB�fB�`B�TB�HB�HB�#B��B��B��B��B��B��B��B��B��B�RB��Bz�Bn�BP�B�B��B�B��B�qB�FB��B�BYBC�B6FB%�BoBB
��B
��B
�B
�HB
�B
��B
�FB
��B
��B
��B
��B
�VB
q�B
[#B
M�B
=qB
6FB
+B
�B
�B
�B
uB
PB
1B
B	��B	��B	��B	��B	�B	�B	�ZB	�
B	��B	ȴB	ƨB	ŢB	�qB	�3B	�!B	�'B	�!B	�B	��B	��B	��B	�=B	�B	~�B	z�B	u�B	k�B	dZB	_;B	YB	VB	T�B	P�B	F�B	@�B	?}B	<jB	9XB	7LB	5?B	0!B	+B	(�B	%�B	#�B	"�B	�B	�B	hB	1B	B��B��B��B��B�B�B�B�mB�TB�;B�)B��BɺBĜBB�}B�dB�LB�-B�!B�B�B��B��B��B��B��B��B��B��B��B�oB�\B�JB�7B�%B�B�B�B~�B|�Bw�Bt�Bs�Bq�Bo�Bl�BiyBffBcTBbNBcTBbNB`BB\)B[#B[#BZBZBZBZBZBYBXBVBT�BS�BR�BQ�BP�BO�BL�BK�BJ�BI�BG�BE�BA�B>wB=qB:^B5?B1'B/B-B,B,B,B+B,B,B+B'�B&�B%�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�BoBhBhBoBoBhBhBhBbBbBbBbB\BVBPBJBDBVBPBJBJBJBPBPBPBbB�B�B�B �B$�B(�B.B1'B2-B33B49B5?B5?B5?B5?B6FB6FB6FB9XB<jBB�BC�BK�BL�BL�BM�BN�BO�BP�BR�BW
BYBZB]/B^5B^5B`BBdZBe`Be`BdZBdZBdZBffBl�Bq�Br�Bv�Bv�Bu�Bt�Bt�Bu�Bv�Bu�Bu�Bz�B{�B{�B|�B{�B}�B� B�B�B�B�B�7B�VB�VB�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�FB�^B�dB�dB�qB�}B��BÖBĜBǮBɺB��B��B��B��B��B��B��B�B�
B�B�#B�5B�;B�;B�BB�`B�yB�B�B�B�B�B��B��B��B��B��B	B	B	B	1B	JB	VB	bB	oB	�B	�B	�B	�B	!�B	$�B	(�B	+B	,B	,B	-B	.B	/B	/B	0!B	1'B	1'B	2-B	1'B	33B	:^B	<jB	<jB	=qB	?}B	@�B	A�B	D�B	G�B	J�B	J�B	N�B	P�B	T�B	VB	W
B	XB	YB	[#B	^5B	_;B	`BB	ffB	k�B	p�B	t�B	v�B	z�B	�B	�B	�B	� B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�+B	�=B	�JB	�JB	�PB	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�LB	�^B	�^B	�RB	�XB	�qB	�qB	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�B	�/B	�5B	�5B	�)B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�B	�B	�B	�)B	�5B	�)B	�5B	�5B	�5B	�;B	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
+B
+B
1B
	7B

=B

=B

=B

=B

=B

=B
DB
JB
PB
PB
VB
VB
VB
VB
VB
\B
bB
bB
hB
hB
oB
oB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
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
%�B
%�B
$�B
$�B
$�B
$�B
%�B
'�B
'�B
(�B
(�B
'�B
'�B
(�B
+B
+B
+B
+B
+B
+B
+B
,B
-B
-B
.B
/B
.B
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
.B
-B
-B
-B
.B
/B
/B
0!B
1'B
1'B
2-B
33B
33B
49B
5?B
5?B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
6FB
7LB
8RB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
@�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
O�B
O�B
O�B
P�B
R�B
R�B
S�B
S�B
S�B
S�B
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
T�B
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
YB
YB
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
bNB
bNB
bNB
bNB
bNB
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
ffB
ffB
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
jB
iyB
jB
jB
jB
jB
jB
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
m�B
n�B
n�B
n�B
n�B
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
o�B
o�B
o�B
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
r�B
s�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B_BRBZBsB�B�B�B�B�B|B�BB�B�B�B BB�BVB�BB0BVBRB �B��B��B�MBڸB�"BʘB��BѾB�hB�HB�VB�B�1B�B�B��B�hB�B��B��B�
B�RB�B�IB�B��B�B��B�*B��B�B�1B�/B�B��B|�BsBY�B,B�,B��B��B��B�8B�B��B_
BF�B:VB+LB�BWB
�tB
�CB
�B
�4B
�QB
��B
��B
��B
�$B
�B
��B
�-B
w�B
_�B
S.B
?�B
:�B
0B
 pB
�B
)B
�B
�B
	�B
BB
�B	�B	�)B	��B	�\B	��B	�4B	�B	�,B	ɻB	șB	�bB	�
B	��B	��B	��B	�QB	��B	��B	��B	�OB	��B	��B	�B	|�B	z�B	n\B	f�B	a�B	Z3B	VJB	VaB	U�B	JiB	BB	A�B	=�B	9�B	8TB	7 B	2B	+�B	*1B	&nB	#�B	$=B	!kB	�B	�B	
�B	�B	 GB��B��B�B�B�)B�B�B�B��B�BهB�RB�sB�eB�B�8B�|B�GB��B��B��B��B�TB�3B�~B��B��B�	B��B�1B��B��B�B��B��B��B�oB��B�qB��Bx�Bu^Bt�Br�Br1Bo)Bj�BjeBdTBcaBc�Bd#Bb�B^�B\AB\BZ�BZ^BZBZ�BZ�BZ�BY�BX�BV�BU�BS�BRnBRBQ�BNBLBKBK1BIiBHtBCB?/B>�B=oB7GB2�B0�B.DB,�B,�B,{B+PB,nB-(B-B(�B'�B&�B%�B&�B"�B"�B!�B�B�BB~B4B�BB�B�B�B�B�B�BMB�B�BBUBB�B!B0BBkBDB*B;B2B�BdB�B�B�B
B�BBB#5B'�B+wB/B1�B2�B4NB4�B5�B5�B64B6�B6�B6�B7\B:BB=BB�BF�BL=BM?BM�BNDBN�BPBQrBT BW�BZJB[iB]rB^)B^DB`uBd�Be�Be�Bf�Bf
Bd�BgBl�BrBr�BwBxBBvhBu#Bu!Bu�Bw�Bw�Bu�B|wB|mB|�B}�B}eBB��B�gB�{B��B�aB�WB��B��B��B��B�GB�;B�2B�iB�aB�
B�MB�iB�
B��B�"B��B�-B��B�uB�B�GB�iB��B��B��B��B��B��B�B�'BˮB��B� B�9B�-B�bB��B�:B�wBٲB۟B�TB�,B�BB�B��B��B�B�B�B��B��B��B��B��B�MB�=B	 �B	�B	B	xB	WB	~B	�B	fB	lB	�B	�B	�B	!�B	$�B	(�B	*�B	+�B	+�B	-B	-�B	/B	/B	07B	1mB	1DB	2.B	1	B	4B	:�B	<hB	<B	=�B	?�B	@�B	A�B	D�B	G�B	J�B	K#B	OB	Q-B	UPB	VeB	WB	X&B	Y7B	[[B	^DB	_vB	_�B	e�B	j�B	q�B	uuB	vrB	yB	��B	��B	�mB	�NB	�B	�CB	�.B	�B	��B	�B	�B	��B	��B	�iB	�lB	�3B	�PB	�CB	�5B	�$B	�)B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�4B	��B	�!B	�B	�B	�B	��B	�#B	��B	��B	�HB	�ZB	��B	̽B	�9B	�B	�VB	�aB	уB	� B	�BB	�vB	�'B	�1B	�B	��B	�pB	�tB	�mB	٬B	� B	ՖB	׮B	�_B	�B	�B	��B	�B	��B	�B	� B	ѕB	�;B	�=B	�eB	�B	��B	ҰB	�?B	�B	ՆB	��B	��B	�B	�uB	�WB	�XB	�B	ۼB	��B	�5B	�!B	޿B	�:B	�DB	�B	�6B	�-B	�B	�B	�MB	�}B	�B	��B	�AB	�B	�wB	�vB	�]B	��B	��B	�B	�rB	� B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	�<B	��B	��B	��B	�BB	�B	�B	��B	��B	��B	�B	��B	�QB	�B	��B	��B	��B
 B
 TB
QB
B
MB
MB
B
B
NB
=B
<B
%B
DB
LB
	iB

@B

'B

B

*B

GB

nB
sB
PB
DB
MB
KB
GB
IB
bB
�B
�B
LB
mB
zB
pB
mB
yB
uB
�B
�B
sB
zB
xB
wB
yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
! B
"B
!BB
!B
 �B
 �B
 �B
"#B
"�B
#�B
%+B
%�B
&B
%�B
$�B
$�B
%$B
$�B
%�B
'�B
'�B
)B
(�B
'�B
'�B
)	B
+ B
*�B
+?B
+[B
+?B
+!B
+B
,#B
-B
-6B
.KB
/B
.�B
/9B
/B
/B
/!B
/B
/B
0B
0B
0?B
0�B
.�B
-B
-:B
-,B
.B
/[B
/@B
0B
1 B
1aB
2B
3 B
3^B
4bB
5�B
5�B
4VB
4-B
4.B
5RB
5�B
5zB
6{B
6wB
7nB
6�B
7HB
8wB
9�B
:vB
:B
:�B
;�B
;�B
<�B
<iB
<WB
<`B
<�B
=�B
=XB
>�B
>lB
?rB
?�B
?NB
@cB
AsB
A�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
FB
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IB
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
M?B
MB
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
QMB
PeB
PB
O�B
P�B
R�B
R�B
S�B
T"B
S�B
T/B
R�B
R�B
SB
R�B
S	B
S	B
R�B
S�B
T%B
TB
TB
T�B
VB
V�B
WB
WB
X)B
XEB
XB
Y&B
Y-B
Y�B
[B
[1B
[%B
[#B
[B
[&B
[?B
[pB
[gB
Z�B
\$B
]0B
]0B
]&B
]bB
^PB
^UB
_@B
_VB
_?B
_?B
`CB
`AB
`;B
`<B
`8B
`<B
``B
aIB
aNB
a{B
a?B
bPB
beB
bPB
b[B
b^B
cLB
czB
cqB
c[B
d\B
dQB
dRB
dSB
d]B
dPB
dPB
dkB
epB
eUB
eYB
efB
epB
eXB
etB
eB
fmB
f]B
fxB
ftB
fvB
f\B
grB
gB
goB
gZB
gfB
g�B
g�B
hxB
hB
h]B
hhB
hvB
htB
iB
i�B
i�B
ipB
imB
ioB
jkB
i�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k{B
l�B
lzB
l�B
l�B
lxB
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
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
o�B
o�B
o�B
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
r�B
s�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<$z/<#�
<#�
<#�
<#�
<#�
<#�
<#�
<;��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.17 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810231352222018102313522220181023135222  AO  ARCAADJP                                                                    20180130180439    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180130180439  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180130180439  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181023135222  QC  PRES            @�ffD��3G�O�                PM  ARSQCTM V1.1                                                                20181023135222  QC  PSAL            @�ffD��3G�O�                PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181025093512  IP                  G�O�G�O�G�O�                