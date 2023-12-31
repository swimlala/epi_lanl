CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-12-23T21:43:56Z creation;2022-12-23T21:43:58Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20221223214356  20221223215841  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               uA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��Y�S�1   @���ޠ@;��"��`�d�-1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A~ffA�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.�C0  C1�fC4  C6  C8  C:�C<  C>  C@�CB  CD  CF�CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��C�  C��3C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C��3C�  C�  C��C��C�  C��3C�  C��C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D	� D
  D
y�D
��D� D  D� D  D� D  D�fDfD� D  D� D��Dy�D  D� D  D� D  D�fD  D� DfD� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D$��D%� D&  D&y�D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D1��D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DV��DWy�DX  DX� DY  DY� DY��DZy�D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Ds��Dt� Du  Duy�Dv  Dv� DwfDw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�<�D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D��3D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�C3D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�<�D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D��3D�  D�<�D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�<�D�|�D�� D�3D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D���D�@ Dǀ D�� D�  D�@ D�|�D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ D˃3D�� D�  D�@ D̃3D�� D�  D�@ D̓3D�� D�  D�@ D΀ D�� D�  D�<�Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D��3D�3D�@ DҀ D�� D�  D�@ DӃ3D�� D�  D�@ DԀ D��3D�  D�@ DՀ D��3D�  D�@ Dր D�� D���D�<�D׀ D��3D�  D�@ D؀ D�� D�  D�C3Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D���D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��3D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�<�D�|�D�� D�3D�C3D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@�@�A�HA>�HA^�HA}G�A���A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBh�Bp�Bw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B���Bè�BǨ�B˨�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�zC�C�C�C�C�C�C�C�C�C!�C#�C%�zC'�C)�C+�C.�C/�C1�zC3�C5�C7�C:�C;�C=�C@�CA�CC�CF�CG�CI�CK�CN�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Cf�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��=C��
C��
C��
C��
C��=C��C��
C��=C��
C��C��
C��
C��
C��
C��C��
C��
C��
C��=C��
C��
C��=C��
C��
C��C��C��
C��=C��
C��C��C��C��
C��
C��=C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��=C��
C��C��C��
C��=C��=C��
C��
C��=C��
C��
C��
C��
C��
C��=C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��C��C��
C��
C��
C��
C��
C��
C��
C��
C��C��C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�D	{�D	��D
uD
�D{�D��D{�D��D{�D��D��D�D{�D��D{�D�DuD��D{�D��D{�D��D��D��D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"uD"��D#{�D#��D${�D$�D%{�D%��D&uD&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D/�D/{�D/��D0{�D0��D1{�D1�D2uD2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?�D@uD@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DK�DK��DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DU�DU{�DU��DV{�DV�DWuDW��DX{�DX��DY{�DY�DZuDZ��D[{�D[��D\uD\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��DsuDs�Dt{�Dt��DuuDu��Dv{�Dw�Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�:�D�}�D���D���D�:�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D���D���D���D�@�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�@�D�}�D���D���D�:�D�}�D���D���D�@�D�}�D���D���D�=�D�}�D���D���D�:�D�}�D���D���D�@�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�@�D�}�D���D���D�:�D�z�D���D� �D�=�D�}�D���D���D�:�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�z�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�z�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�Dˀ�D˽�D���D�=�D̀�D̽�D���D�=�D̀�Dͽ�D���D�=�D�}�Dν�D���D�:�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�D���D� �D�=�D�}�Dҽ�D���D�=�DӀ�Dӽ�D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�Dֽ�D���D�:�D�}�D���D���D�=�D�}�Dؽ�D���D�@�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�:�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D���D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D纐D���D�:�D�z�D��D� �D�@�D��D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�z�D��D���D�:�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D��]1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��A���A���A��A��A��1A��@A��:A��hA���A��FA��A���A���A��RA��XA��*A��6A��A��A��!A��OA���A��!A���A��A���A���A���A��A���A���A��^A���A��A���A��A��XA��dA�ɺA��gA���A���A���A��?A��A���A���A��aA��gA��BA��A��0A��}A�?�A�	�A���A�\A�]�A� �A�S&A�o�A��A���A���A���A��A��A���A�HA���A��A���A��4A��A�AUA�N<A���A�PA�"hA��,A��_A��A���A�NpA���A�ZQA�MA�s�A���A�J�A��HA�#�A��A�(�A�u�A���A��A��PA���A�ƨA�O�A��;A�~(A�X�A�A�j�A��qA�D�A�#�A(A}�]A}�AzԕAyJ�Ay#:Ax��Ax|�AxF�Aw�ZAw\�Av>BAt��Ar6Ap�zAoAm�Al_pAkH�Aj�DAi>BAfc�Ac9�AaE9A`J#A`4A_+�A]FtA\33AZ�zAVrGAT�qAS��AS33AR-wAQ��AQ�}AQ�AP�ZAP�AN�XAM�`AM��AM~�AMDgALy>AK�9AK�AJ�fAJB�AI��AI��AIe,AI;AHqvAG��AFo�AD�AB�AB��AA�PA>�HA=T�A<jA;5?A:�bA:�A9�A9~(A9B[A8�XA8DgA8#�A7z�A4خA4�A3��A1'�A/hsA.��A.,�A-�A-$tA,ffA+��A+0UA*�vA*n�A*Y�A*@OA)�A(�hA'�{A&�qA$B�A#A!��A�<Aw�A�Ay�A"�AzA/�A�A�A��A7�A�#A��A�DA`BA�sAy�A-A�AzA�Al"A
�A��A$�A�3A�PA~(Am�A?}AS&A��A��Ad�A,=A'�A($A�@���@�x@��@�L0@�O�@��@��@�Z@�Ft@�#:@��@�>�@�U2@�|@���@@�Dg@@� @��@���@��9@��@�l�@薼@�x@��@�@��m@���@��o@ߠ�@ݶF@ܾ�@��]@�j�@��@��s@ڱ�@��@��@�ϫ@�f�@���@�o @�%�@�0�@Ϗ�@λ�@��@˲�@˞�@˘�@ˁ@�hs@�h
@ȏ\@Ǧ�@�l"@Ŝ�@�m]@��@ĺ�@�S�@Â�@¾�@�q@�h
@�&�@���@���@�}�@�Dg@���@��H@��@���@��@�V@���@�q�@�c @�'R@��+@��;@��^@���@�($@��@��@���@��b@�]d@�
�@��[@�;�@��Z@��@���@���@�a@�%@�Xy@���@�tT@�A�@���@��@��@��@��@��@���@�͟@��@�{@���@�8@��@��1@� �@���@�>�@���@���@�~(@�K^@���@�Z�@�҉@��@��^@��~@��{@�~�@�x�@�s�@�f�@�`B@�O@�2a@�"�@���@���@���@�1@��:@�%F@�l�@���@�E9@��)@�i�@�1'@���@���@�5�@���@��r@�!�@��C@�)_@�l�@��@�O�@��F@�Q�@���@�p�@�7L@� \@���@��@��B@��6@�L0@�Mj@���@�}V@�p;@�l"@�K^@��@��P@�=@�V@���@��F@�e@��g@��C@�hs@�ی@�p;@�m�@�PH@��@��@��)@��>@���@���@�J�@�:�@�9�@�33@�/�@�C@�S@��@�C-@�ϫ@���@��=@�a�@�	l@��I@�4@���@�Vm@�=�@�'�@�q@�@�ѷ@��@�{�@�n�@�h�@�d�@�+k@خ@RT@~��@}�N@}:�@}-w@}@|1@{"�@{.I@z�c@z��@z��@zR�@x��@wx@wJ#@w�@v��@u�'@uL�@u=�@t��@t�@s�$@sW?@r�]@r�L@rL0@q[W@p�v@pQ�@o�k@oW?@oE9@oC@n��@n��@m��@m�@m�@l�O@lbN@lm�@lQ�@l4n@k_p@kP�@kU�@k4�@kS@i��@im]@iV@h��@h<�@h'R@hFt@h_@h`�@h	�@g��@g��@g@O@g'�@g"�@g�@f��@e(�@d�$@d~(@dN�@d�@c��@c�}@c�{@c@b�@bȴ@b�@b�@bJ�@a��@aVm@aA @`��@`��@`��@`h�@_��@_l�@_=@_�@^�@^)�@]�=@]a�@]L�@]B�@]7L@\�)@\1'@[��@[&@ZYK@Z#:@Z�@Y��@Y��@Y�H@Y��@YQ�@X�/@X��@X6@W�]@W�w@WK�@W�@V�2@V�L@VM�@V�@U�t@Uo @U!�@U+@U;@Tѷ@T��@T��@Ty>@S��@S�$@S�{@Sy�@Sv`@St�@Se�@S9�@S�@R�b@R1�@Q�Z@Q��@Q��@Q@P�?@Pz�@PV�@P>B@O�@@N�'@Ns�@Na|@NTa@N?@N#:@N�@N
�@M�C@MX@L�E@LV�@K�}@K��@Kj�@KZ�@K\)@K@O@K@JH�@I2a@I�@I@H��@G��@G�@G��@G�P@G��@G��@G��@GRT@Fȴ@F��@Fxl@F;�@E�T@E��@E�~@Ec@Ee,@E%F@D��@D/�@DG@C�A@C�w@Cn/@CMj@C;d@C!-@B�@B�\@Bxl@BGE@A�@A�@ArG@A8�@A�@@�4@@�Y@@N�@?�@?�k@?y�@?v`@?_p@?J#@?"�@>�8@>�<@>}V@=��@<�@<�4@<j@<H@< �@<x@;�@;��@;�a@;�*@;RT@;A�@;!-@;�@:�M@:��@:�@:��@:��@:�"@:�"@; i@;@:�"@:��@;@;@;S@;�@;�@:�y@:�!@:��@:��@:��@:8�@9�@9�'@9rG@9/@8�)@8"h@6҉@6.�@5��@5�^@5��@5�n@5[W@4��@4��@4�@3��@3&@2�@2�!@2n�@2-@1o @0�E@0��@02�@/�;@/�Q@/� @/g�@.ߤ@.�<@.�L@.v�@.=q@.O@-�.@-�z@-��@-[W@-�@,��@,�@,|�@,(�@+�r@+ƨ@+�f@*�s@*�}@*�}@*�b@*�@*�+@*R�@*�@)�o@)�-@)c�@)&�@)�@(�	@(�K@(�v@(֡@(�j@([�@'��@'�@&��@%��@%IR@%*0@%�@$�@$�@$��@$�)@$�?@$��@$�@$�j@$��@$�Y@#�q@"�@"�@"��@"Z�@"e@!�9@!��@!O�@ �f@ �$@ ��@ �D@ m�@ c�@ 2�@�Q@��@b�@F�@>�@�@�B@v�@)�@�#@��@��@��@Vm@��@q@�
@>�@��@ں@�s@�B@Q@��@��@Q�@&�@�@�j@�z@/�@�[@v`@S�@O@Mj@@O@��@W�@;�@5?@J@�@�3@��@|@rG@c�@A @�@��@�4@�D@tT@g8@`�@V�@C-@/�@/�@*�@x@��@��@��@��@��@s@W?@K�@K�@Mj@K�@H�@�@�!@~�@c @R�@5?@�@�@	@�)@S&@!�@%@��@�[@�p@��@�_@��@m�@H@>B@4n@�@��@�g@ƨ@�k@s@W?@;d@�@
=@�@�X@�!@��@{�@z@c @	@�d@�M@�|@`�@c�@`�@[�@Z@[�@`�@c�@e�@j@j@Xy@N�@I�@9X@,=@1@�;@��@�@
�F@
�+@
s�@	�o@	��@	��@	�N@	��@	�-@	�-@	�C@	��@	�@	��@	�@	rG@	c�@	O�@	5�@�@S�@�q@o�@S�@K�@'�@��@��@}V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��A���A���A��A��A��1A��@A��:A��hA���A��FA��A���A���A��RA��XA��*A��6A��A��A��!A��OA���A��!A���A��A���A���A���A��A���A���A��^A���A��A���A��A��XA��dA�ɺA��gA���A���A���A��?A��A���A���A��aA��gA��BA��A��0A��}A�?�A�	�A���A�\A�]�A� �A�S&A�o�A��A���A���A���A��A��A���A�HA���A��A���A��4A��A�AUA�N<A���A�PA�"hA��,A��_A��A���A�NpA���A�ZQA�MA�s�A���A�J�A��HA�#�A��A�(�A�u�A���A��A��PA���A�ƨA�O�A��;A�~(A�X�A�A�j�A��qA�D�A�#�A(A}�]A}�AzԕAyJ�Ay#:Ax��Ax|�AxF�Aw�ZAw\�Av>BAt��Ar6Ap�zAoAm�Al_pAkH�Aj�DAi>BAfc�Ac9�AaE9A`J#A`4A_+�A]FtA\33AZ�zAVrGAT�qAS��AS33AR-wAQ��AQ�}AQ�AP�ZAP�AN�XAM�`AM��AM~�AMDgALy>AK�9AK�AJ�fAJB�AI��AI��AIe,AI;AHqvAG��AFo�AD�AB�AB��AA�PA>�HA=T�A<jA;5?A:�bA:�A9�A9~(A9B[A8�XA8DgA8#�A7z�A4خA4�A3��A1'�A/hsA.��A.,�A-�A-$tA,ffA+��A+0UA*�vA*n�A*Y�A*@OA)�A(�hA'�{A&�qA$B�A#A!��A�<Aw�A�Ay�A"�AzA/�A�A�A��A7�A�#A��A�DA`BA�sAy�A-A�AzA�Al"A
�A��A$�A�3A�PA~(Am�A?}AS&A��A��Ad�A,=A'�A($A�@���@�x@��@�L0@�O�@��@��@�Z@�Ft@�#:@��@�>�@�U2@�|@���@@�Dg@@� @��@���@��9@��@�l�@薼@�x@��@�@��m@���@��o@ߠ�@ݶF@ܾ�@��]@�j�@��@��s@ڱ�@��@��@�ϫ@�f�@���@�o @�%�@�0�@Ϗ�@λ�@��@˲�@˞�@˘�@ˁ@�hs@�h
@ȏ\@Ǧ�@�l"@Ŝ�@�m]@��@ĺ�@�S�@Â�@¾�@�q@�h
@�&�@���@���@�}�@�Dg@���@��H@��@���@��@�V@���@�q�@�c @�'R@��+@��;@��^@���@�($@��@��@���@��b@�]d@�
�@��[@�;�@��Z@��@���@���@�a@�%@�Xy@���@�tT@�A�@���@��@��@��@��@��@���@�͟@��@�{@���@�8@��@��1@� �@���@�>�@���@���@�~(@�K^@���@�Z�@�҉@��@��^@��~@��{@�~�@�x�@�s�@�f�@�`B@�O@�2a@�"�@���@���@���@�1@��:@�%F@�l�@���@�E9@��)@�i�@�1'@���@���@�5�@���@��r@�!�@��C@�)_@�l�@��@�O�@��F@�Q�@���@�p�@�7L@� \@���@��@��B@��6@�L0@�Mj@���@�}V@�p;@�l"@�K^@��@��P@�=@�V@���@��F@�e@��g@��C@�hs@�ی@�p;@�m�@�PH@��@��@��)@��>@���@���@�J�@�:�@�9�@�33@�/�@�C@�S@��@�C-@�ϫ@���@��=@�a�@�	l@��I@�4@���@�Vm@�=�@�'�@�q@�@�ѷ@��@�{�@�n�@�h�@�d�@�+k@خ@RT@~��@}�N@}:�@}-w@}@|1@{"�@{.I@z�c@z��@z��@zR�@x��@wx@wJ#@w�@v��@u�'@uL�@u=�@t��@t�@s�$@sW?@r�]@r�L@rL0@q[W@p�v@pQ�@o�k@oW?@oE9@oC@n��@n��@m��@m�@m�@l�O@lbN@lm�@lQ�@l4n@k_p@kP�@kU�@k4�@kS@i��@im]@iV@h��@h<�@h'R@hFt@h_@h`�@h	�@g��@g��@g@O@g'�@g"�@g�@f��@e(�@d�$@d~(@dN�@d�@c��@c�}@c�{@c@b�@bȴ@b�@b�@bJ�@a��@aVm@aA @`��@`��@`��@`h�@_��@_l�@_=@_�@^�@^)�@]�=@]a�@]L�@]B�@]7L@\�)@\1'@[��@[&@ZYK@Z#:@Z�@Y��@Y��@Y�H@Y��@YQ�@X�/@X��@X6@W�]@W�w@WK�@W�@V�2@V�L@VM�@V�@U�t@Uo @U!�@U+@U;@Tѷ@T��@T��@Ty>@S��@S�$@S�{@Sy�@Sv`@St�@Se�@S9�@S�@R�b@R1�@Q�Z@Q��@Q��@Q@P�?@Pz�@PV�@P>B@O�@@N�'@Ns�@Na|@NTa@N?@N#:@N�@N
�@M�C@MX@L�E@LV�@K�}@K��@Kj�@KZ�@K\)@K@O@K@JH�@I2a@I�@I@H��@G��@G�@G��@G�P@G��@G��@G��@GRT@Fȴ@F��@Fxl@F;�@E�T@E��@E�~@Ec@Ee,@E%F@D��@D/�@DG@C�A@C�w@Cn/@CMj@C;d@C!-@B�@B�\@Bxl@BGE@A�@A�@ArG@A8�@A�@@�4@@�Y@@N�@?�@?�k@?y�@?v`@?_p@?J#@?"�@>�8@>�<@>}V@=��@<�@<�4@<j@<H@< �@<x@;�@;��@;�a@;�*@;RT@;A�@;!-@;�@:�M@:��@:�@:��@:��@:�"@:�"@; i@;@:�"@:��@;@;@;S@;�@;�@:�y@:�!@:��@:��@:��@:8�@9�@9�'@9rG@9/@8�)@8"h@6҉@6.�@5��@5�^@5��@5�n@5[W@4��@4��@4�@3��@3&@2�@2�!@2n�@2-@1o @0�E@0��@02�@/�;@/�Q@/� @/g�@.ߤ@.�<@.�L@.v�@.=q@.O@-�.@-�z@-��@-[W@-�@,��@,�@,|�@,(�@+�r@+ƨ@+�f@*�s@*�}@*�}@*�b@*�@*�+@*R�@*�@)�o@)�-@)c�@)&�@)�@(�	@(�K@(�v@(֡@(�j@([�@'��@'�@&��@%��@%IR@%*0@%�@$�@$�@$��@$�)@$�?@$��@$�@$�j@$��@$�Y@#�q@"�@"�@"��@"Z�@"e@!�9@!��@!O�@ �f@ �$@ ��@ �D@ m�@ c�@ 2�@�Q@��@b�@F�@>�@�@�B@v�@)�@�#@��@��@��@Vm@��@q@�
@>�@��@ں@�s@�B@Q@��@��@Q�@&�@�@�j@�z@/�@�[@v`@S�@O@Mj@@O@��@W�@;�@5?@J@�@�3@��@|@rG@c�@A @�@��@�4@�D@tT@g8@`�@V�@C-@/�@/�@*�@x@��@��@��@��@��@s@W?@K�@K�@Mj@K�@H�@�@�!@~�@c @R�@5?@�@�@	@�)@S&@!�@%@��@�[@�p@��@�_@��@m�@H@>B@4n@�@��@�g@ƨ@�k@s@W?@;d@�@
=@�@�X@�!@��@{�@z@c @	@�d@�M@�|@`�@c�@`�@[�@Z@[�@`�@c�@e�@j@j@Xy@N�@I�@9X@,=@1@�;@��@�@
�F@
�+@
s�@	�o@	��@	��@	�N@	��@	�-@	�-@	�C@	��@	�@	��@	�@	rG@	c�@	O�@	5�@�@S�@�q@o�@S�@K�@'�@��@��@}V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Be,BezBeBe�Be�Be`Be�Be`Be�Be�Be�Be`BeFBeFBd�BdtBdZBd�Bd�Bd�Bd�Be�BeFBe,Be`Be�Be�Be�Be�Be�Be�Be�Bf2BfBfLBfBfLBffBffBffBffBe�BfBe�BeBe�Be�Be�Be,BezBe�Bd�BdtBc�Bc�B[#BUMBOBB1AB�B��B��BwLBS@B:�B./B1BB"B^B 4B��B�B��B�pB�B�PB�AB��B�IB�uB��Bo Be`BUMB?�B2GB$tB{B�qB�pB�+B�uB�7B��B�}B�`B�7B�B�B�-B��B}qBvzBp�Bm�Bj�Bb�BW$BHB:xB%�B�B_BBB B
�cB
��B
�B
��B
�tB
� B
�B
�$B
��B
��B
�B
�GB
��B
�ZB
��B
�<B
y>B
p!B
g�B
fLB
bB
X�B
PHB
I7B
88B
*KB
$�B
"�B
xB
B
�B
mB
�B
hB

#B
mB
�B
�B
�B	�HB	��B	�XB	�lB	��B	�B	�B	�)B	�B	�B	��B	�/B	ҽB	��B	�7B	��B	�VB	�?B	�'B	��B	�B	��B	��B	��B	��B	��B	��B	�WB	�EB	��B	�(B	�pB	��B	}"B	z^B	xB	u�B	s�B	p�B	l�B	kQB	jeB	g�B	g�B	f�B	e,B	b�B	ZQB	X+B	L�B	GEB	A�B	=<B	5�B	1�B	/�B	.�B	,B	(�B	(XB	'B	'B	#B	!bB	�B	YB	B	�B	 B	B	�B	�B	�B	�B	�B	�B��B�}B��B�wB��B�<B��B��B�rB��B�8B�LB�tB�LB��B�vB�B�iB��B�B��B��B��B��B�B�B�B�B�B�B�6B��B��B�B��B�B�KB�0B��B�B�KB�B��B�KB�eB�B�B�B�B�wB�B�CB��B��B��B��B�'B�GB��B�nB�B�LB�lB��B�JB�dB�0B�0B��B�B�}B	 �B	-B	B	gB	B	mB	%B	EB		B		7B		7B		�B	
=B	
�B	
�B	
�B	^B	B	vB	�B	,B	�B	�B	IB	dB	5B	�B	�B	jB	!-B	!�B	"�B	#�B	#TB	#nB	)DB	+�B	0UB	2�B	49B	4�B	4�B	5B	5�B	6�B	9$B	=�B	>(B	@4B	@�B	A�B	GzB	L0B	NpB	Q B	TaB	T{B	T�B	V�B	X�B	ZQB	[#B	\B	^OB	d�B	j�B	l�B	mB	mCB	nB	pUB	r�B	uZB	x�B	{B	|B	|PB	|jB	|�B	|�B	}B	|�B	}<B	}�B	}�B	}B	�B	�B	��B	��B	�zB	��B	�bB	��B	��B	��B	��B	�B	�#B	�~B	�VB	�|B	�B	�8B	��B	�;B	�|B	��B	��B	��B	ðB	��B	ǔB	�1B	�B	ɺB	�	B	ʌB	�~B	ҽB	ԯB	�gB	��B	�B	�
B	�B	��B	�B	�ZB	�2B	�eB	�B	��B	�9B	�FB	��B	�]B	�BB	�cB
�B
B
AB
[B
B
MB
�B
�B
	B
	�B

	B
xB
�B
vB
�B
�B
_B
yB
�B
)B
�B
!B
#TB
$@B
$�B
%`B
%�B
&fB
'mB
(�B
)�B
)�B
)�B
)�B
+�B
,�B
.}B
0�B
4B
5�B
5�B
5�B
6`B
6�B
6�B
8�B
<B
>B
@B
C{B
E�B
HB
IB
J�B
M�B
N�B
N�B
P�B
Q�B
QB
QB
R:B
S[B
T�B
X�B
Z�B
]�B
`B
`�B
a�B
b�B
b�B
c:B
c�B
gB
e�B
e�B
ffB
gRB
h�B
k6B
n�B
n�B
n�B
oOB
o�B
r-B
raB
shB
t�B
u�B
v�B
y�B
z�B
{B
}<B
~wB
~�B
�B
�B
�B
�B
��B
��B
�B
�B
�tB
��B
�EB
��B
��B
�#B
��B
��B
��B
�JB
�B
�BB
�}B
� B
��B
��B
��B
�uB
��B
��B
��B
�B
��B
�KB
��B
�B
�~B
��B
��B
��B
��B
�:B
��B
��B
�8B
�RB
�RB
�mB
��B
�XB
��B
�6B
��B
�)B
�wB
�IB
� B
��B
��B
��B
�AB
��B
�|B
�hB
�B
�9B
��B
�%B
�%B
�?B
�tB
��B
��B
�	B
�>B
�XB
�XB
�rB
�B
�xB
��B
��B
�<B
��B
�B
�B
��B
��B
��B
��B
�AB
āB
�B
�SB
ňB
żB
��B
�%B
�B
��B
ǔB
�B
�rB
˒B
��B
�B
�0B
�0B
�dB
̘B
οB
� B
� B
� B
҉B
��B
�FB
ԕB
ԯB
��B
ԯB
ԯB
��B
�SB
ևB
��B
�?B
�B
خB
��B
��B
�1B
��B
��B
��B
�)B
�CB
ܒB
�/B
�~B
�~B
��B
�jB
�B
�B
�VB
��B
�\B
��B
�HB
��B
�hB
�B
�B
��B
�tB
�B
�B
�B
��B
�,B
�`B
��B
�2B
�B
�B
�_B
��B
��B
�eB
�B
�B
��B
��B
�B
�B
��B
�"B
�=B
�B
�qB
�B
�qB
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
�]B
�CB
�CB
�]B
�/B
��B
� B
�OB
�B
�;B
�'B
��B
�B
�B
�ZB
�ZB
�ZB
��B
��B
��B
��B
��B
�>B
�rB
��B
�*B
�xB
��B
��B
��B
��B
�(B
�B
�B
��B
��B
��B
��B 4B �B �B �B;BoB�BAB�B�B�B{B�B�B3BmB�BmB�B�B�BB?B�B�BEB�B�B�B�B�B�BB�B	�B
=B
�B�B�B6BPB�B�B�B�B�B�B�B�B�BB�B�B�B�B4B�B�BTB�B&BuB�B�B�B�B,B�B�BMBgBgB�BB�B�BYB�B�B�B�B�B�B�B�B	B=B=B#BBxB�B/B�B�BBB�BpB�B B�B B�B �B!HB!|B!|B!�B!�B"NB"hB"�B"�B"�B"�B# B#�B#�B#�B#�B$B$&B$&B$ZB$ZB$tB$tB$�B$�B%B%FB%FB%`B%`B%�B%�B%�B%�B%�B%�B%�B&�B&�B&�B&�B'B'RB'mB'RB'mB(sB(�B(�B(�B)B(�B)DB)_B)_B)�B)�B)�B)�B*B*KB*eB*B*�B*�B*�B+B+6B+B+QB+kB+�B+�B+�B+�B+�B,=B,qB,�B-wB.B.B./B.B.B-�B-�B-�B-�B-�B-�B-�B-�B-�B-�B.B.IB.B.�B/iB0B/�B0B1B1B1B1'B1'B1'B1AB1'B1'B1AB1AB1AB1AB1AB1[B1[B1�B2GB3�B49B4�B4nB4�B5�B5ZB64444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  Be,BezBeBe�Be�Be`Be�Be`Be�Be�Be�Be`BeFBeFBd�BdtBdZBd�Bd�Bd�Bd�Be�BeFBe,Be`Be�Be�Be�Be�Be�Be�Be�Bf2BfBfLBfBfLBffBffBffBffBe�BfBe�BeBe�Be�Be�Be,BezBe�Bd�BdtBc�Bc�B[#BUMBOBB1AB�B��B��BwLBS@B:�B./B1BB"B^B 4B��B�B��B�pB�B�PB�AB��B�IB�uB��Bo Be`BUMB?�B2GB$tB{B�qB�pB�+B�uB�7B��B�}B�`B�7B�B�B�-B��B}qBvzBp�Bm�Bj�Bb�BW$BHB:xB%�B�B_BBB B
�cB
��B
�B
��B
�tB
� B
�B
�$B
��B
��B
�B
�GB
��B
�ZB
��B
�<B
y>B
p!B
g�B
fLB
bB
X�B
PHB
I7B
88B
*KB
$�B
"�B
xB
B
�B
mB
�B
hB

#B
mB
�B
�B
�B	�HB	��B	�XB	�lB	��B	�B	�B	�)B	�B	�B	��B	�/B	ҽB	��B	�7B	��B	�VB	�?B	�'B	��B	�B	��B	��B	��B	��B	��B	��B	�WB	�EB	��B	�(B	�pB	��B	}"B	z^B	xB	u�B	s�B	p�B	l�B	kQB	jeB	g�B	g�B	f�B	e,B	b�B	ZQB	X+B	L�B	GEB	A�B	=<B	5�B	1�B	/�B	.�B	,B	(�B	(XB	'B	'B	#B	!bB	�B	YB	B	�B	 B	B	�B	�B	�B	�B	�B	�B��B�}B��B�wB��B�<B��B��B�rB��B�8B�LB�tB�LB��B�vB�B�iB��B�B��B��B��B��B�B�B�B�B�B�B�6B��B��B�B��B�B�KB�0B��B�B�KB�B��B�KB�eB�B�B�B�B�wB�B�CB��B��B��B��B�'B�GB��B�nB�B�LB�lB��B�JB�dB�0B�0B��B�B�}B	 �B	-B	B	gB	B	mB	%B	EB		B		7B		7B		�B	
=B	
�B	
�B	
�B	^B	B	vB	�B	,B	�B	�B	IB	dB	5B	�B	�B	jB	!-B	!�B	"�B	#�B	#TB	#nB	)DB	+�B	0UB	2�B	49B	4�B	4�B	5B	5�B	6�B	9$B	=�B	>(B	@4B	@�B	A�B	GzB	L0B	NpB	Q B	TaB	T{B	T�B	V�B	X�B	ZQB	[#B	\B	^OB	d�B	j�B	l�B	mB	mCB	nB	pUB	r�B	uZB	x�B	{B	|B	|PB	|jB	|�B	|�B	}B	|�B	}<B	}�B	}�B	}B	�B	�B	��B	��B	�zB	��B	�bB	��B	��B	��B	��B	�B	�#B	�~B	�VB	�|B	�B	�8B	��B	�;B	�|B	��B	��B	��B	ðB	��B	ǔB	�1B	�B	ɺB	�	B	ʌB	�~B	ҽB	ԯB	�gB	��B	�B	�
B	�B	��B	�B	�ZB	�2B	�eB	�B	��B	�9B	�FB	��B	�]B	�BB	�cB
�B
B
AB
[B
B
MB
�B
�B
	B
	�B

	B
xB
�B
vB
�B
�B
_B
yB
�B
)B
�B
!B
#TB
$@B
$�B
%`B
%�B
&fB
'mB
(�B
)�B
)�B
)�B
)�B
+�B
,�B
.}B
0�B
4B
5�B
5�B
5�B
6`B
6�B
6�B
8�B
<B
>B
@B
C{B
E�B
HB
IB
J�B
M�B
N�B
N�B
P�B
Q�B
QB
QB
R:B
S[B
T�B
X�B
Z�B
]�B
`B
`�B
a�B
b�B
b�B
c:B
c�B
gB
e�B
e�B
ffB
gRB
h�B
k6B
n�B
n�B
n�B
oOB
o�B
r-B
raB
shB
t�B
u�B
v�B
y�B
z�B
{B
}<B
~wB
~�B
�B
�B
�B
�B
��B
��B
�B
�B
�tB
��B
�EB
��B
��B
�#B
��B
��B
��B
�JB
�B
�BB
�}B
� B
��B
��B
��B
�uB
��B
��B
��B
�B
��B
�KB
��B
�B
�~B
��B
��B
��B
��B
�:B
��B
��B
�8B
�RB
�RB
�mB
��B
�XB
��B
�6B
��B
�)B
�wB
�IB
� B
��B
��B
��B
�AB
��B
�|B
�hB
�B
�9B
��B
�%B
�%B
�?B
�tB
��B
��B
�	B
�>B
�XB
�XB
�rB
�B
�xB
��B
��B
�<B
��B
�B
�B
��B
��B
��B
��B
�AB
āB
�B
�SB
ňB
żB
��B
�%B
�B
��B
ǔB
�B
�rB
˒B
��B
�B
�0B
�0B
�dB
̘B
οB
� B
� B
� B
҉B
��B
�FB
ԕB
ԯB
��B
ԯB
ԯB
��B
�SB
ևB
��B
�?B
�B
خB
��B
��B
�1B
��B
��B
��B
�)B
�CB
ܒB
�/B
�~B
�~B
��B
�jB
�B
�B
�VB
��B
�\B
��B
�HB
��B
�hB
�B
�B
��B
�tB
�B
�B
�B
��B
�,B
�`B
��B
�2B
�B
�B
�_B
��B
��B
�eB
�B
�B
��B
��B
�B
�B
��B
�"B
�=B
�B
�qB
�B
�qB
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
�]B
�CB
�CB
�]B
�/B
��B
� B
�OB
�B
�;B
�'B
��B
�B
�B
�ZB
�ZB
�ZB
��B
��B
��B
��B
��B
�>B
�rB
��B
�*B
�xB
��B
��B
��B
��B
�(B
�B
�B
��B
��B
��B
��B 4B �B �B �B;BoB�BAB�B�B�B{B�B�B3BmB�BmB�B�B�BB?B�B�BEB�B�B�B�B�B�BB�B	�B
=B
�B�B�B6BPB�B�B�B�B�B�B�B�B�BB�B�B�B�B4B�B�BTB�B&BuB�B�B�B�B,B�B�BMBgBgB�BB�B�BYB�B�B�B�B�B�B�B�B	B=B=B#BBxB�B/B�B�BBB�BpB�B B�B B�B �B!HB!|B!|B!�B!�B"NB"hB"�B"�B"�B"�B# B#�B#�B#�B#�B$B$&B$&B$ZB$ZB$tB$tB$�B$�B%B%FB%FB%`B%`B%�B%�B%�B%�B%�B%�B%�B&�B&�B&�B&�B'B'RB'mB'RB'mB(sB(�B(�B(�B)B(�B)DB)_B)_B)�B)�B)�B)�B*B*KB*eB*B*�B*�B*�B+B+6B+B+QB+kB+�B+�B+�B+�B+�B,=B,qB,�B-wB.B.B./B.B.B-�B-�B-�B-�B-�B-�B-�B-�B-�B-�B.B.IB.B.�B/iB0B/�B0B1B1B1B1'B1'B1'B1AB1'B1'B1AB1AB1AB1AB1AB1[B1[B1�B2GB3�B49B4�B4nB4�B5�B5ZB64444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221223214347  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20221223214356  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221223214358  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221223214358                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221223214358  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221223214358  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20221223215841                      G�O�G�O�G�O�                