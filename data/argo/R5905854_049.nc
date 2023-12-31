CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:53:19Z creation;2022-06-04T17:53:19Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604175319  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               1A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��N ��1   @����t@/���R�c'��v�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�33@�33A   A   A@  A`  A~ffA�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bz��B}��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B���B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�fC
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*�C,�C.  C033C1��C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCI�fCL  CN  CP  CR  CS�fCV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� DzfDz�fD{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D��3D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߃3D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�&f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @.�R@���@���@�A�HA>�HA^�HA}G�A�p�A�p�A�p�A���A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBz�B}Q�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\B���B���B��)B��)B��)B�\B�\BϨ�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�zC	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C*�C,�C-�C0!GC1��C3�zC5�C7�C9�C;�C=�C?�CA�CC�CE�CG�zCI�zCK�CM�CO�CQ�CS�zCU�CW�CY�C[�zC]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C��C��C��
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
C��
C��
C��
C��=C��=C��
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
C��
C��C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dz�Dz��Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�:�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�z�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D���D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D߀�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�$)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�c�A�h�A�m)A�hsA�iDA�`BA�_pA�dZA�k�A�poA�v�A�q�A�f�A�e�A�ffA�]/A�]�A�Y�A�S&A�N<A�GEA�JA��A��iA��cA���A��8A���A���Aʱ�A�x�A��KA�=�A�o�A��A�,A���A��A��1A���A�Y�A��A���A�/�A�"�A�S�A��A��aA�3hA���A�M�A��A�D3A��A�2�A�#nA��A�7A��UA��:A� 4A�}VA��\A��AA���A�ٴA�E�A��HA��|A�B�A���A��+A�~�A�fA�-wA�k�A�WsA�A��A���A��A�_A�,A� �A�8RA�v+A�jKA���A}�EAt{JAn$�Al9�Ai�HAfR�Ac��AaGEA]�AY�AU��AR�AO�OAM�hAK<6AJ5?AH��AD��AC�AAB�)ABQ�AA��A@�A>8A=-�A<�9A<uA;	lA8�A7l�A81'A7�"A7�A4�A2�,A2VA1*0A07LA/��A.B[A-I�A,Y�A+1A*ƨA*=qA)PHA(g8A&��A%=A$($A!:�A�AB[AC�A�EA��AAo A�rA($A��A�1A�uA��A��A�&A+�A�IAFtAz�A+A҉A�A��A��A��A3�A�jA�6AjA+A��A�A4nA*0AA�AcA�MA5?A33A��A@A}�A<6A�A�vAu�A4nA	lA
l�A	�MA	��A	Y�A�jAW�A+�A�A��A�AqvAP�A iA��AcAg�A=A�/A�xA2aA��AA�zA�MAK^A�]A��AZ�AϫA>�A
�A �cA ��A C�@���@�n/@��?@�{@��/@���@�y>@��@�B�@��R@��.@���@��'@���@�S@��W@�IR@�_@���@�8@���@�O@�O@��@��W@��@��@��@�j�@�\)@�4�@�Ɇ@�Q@��@�"�@�r@�ƨ@濱@�p;@�{@�Z�@�R@�I@�+k@��@�m]@��@�)�@�J�@�z@���@�\�@ެ@��@��v@�@ۣn@�X�@�e�@��9@�Z�@��f@�l"@��K@�U�@ցo@��@�%@�ߤ@Է�@�tT@�5?@�x@��`@�~�@�i�@��@�|@�@л�@�S�@��#@�?}@Κ�@���@���@��3@˟V@˓�@�iD@�(@���@ʎ�@�@ɱ[@�b�@ȸR@�s�@��@ǿH@�p�@� i@Ƶ�@�!@��g@Ū�@Ľ<@���@�@�e@��T@���@���@�;�@��@�1@��@��A@��@��^@�@�{�@�K^@�ݘ@�L�@��@��@��f@��U@�Z@��@� �@�˒@�Z�@���@��f@�G�@��[@�|�@�[�@�N�@�7@��]@��9@�e,@��9@�:�@�  @��+@���@��}@�}�@�6z@��@��@��t@�g�@�_p@��@��R@�C-@��[@���@�8@���@��]@���@��!@��u@�Xy@��@��L@��@�dZ@��@��O@��@�M@��*@�P�@��@���@�]d@��@��j@���@���@�7@��6@��"@��`@��T@�qv@�[W@�/�@��@��F@�l"@��@��P@�a@�H�@�C@��@��@��I@�,=@��@��t@��@���@���@��-@���@�x�@�N<@���@�H@��C@�.I@��@�ȴ@���@�c�@���@�1�@��@�^5@�0U@���@��k@�@O@��,@�|�@��@��@��=@�s�@�S&@�<6@��@���@�kQ@�N�@�:�@���@���@�hs@�&@�@��@���@���@���@�C�@��@�ƨ@���@���@�e�@�,�@��\@���@�C�@��d@���@�F�@��/@��@�q�@�1�@�  @��-@�/�@���@�<�@���@��
@�dZ@�2a@��@�ں@��4@��@���@�h�@�)�@���@�\�@�@@��y@���@�]d@�+k@�v`@���@��@�tT@�y>@�l"@�_�@�  @��Z@�@���@��w@��P@�x�@�S&@�C�@�;d@�)_@�@��@���@�tT@�D�@�~@��#@��}@���@�g�@�RT@�Mj@�N<@�Q�@�5�@���@��@���@�u%@��@��@��Q@���@��@���@��S@�|�@�W?@��@���@���@�\�@�	@�ϫ@��'@�^�@��@� i@��5@���@�V@�N�@�?@� �@��3@���@�hs@�@@��@�bN@�&�@��@�@J#@~��@~d�@}��@}@|�_@|�Y@|N�@|	�@{{J@{/�@z��@y��@yϫ@y\�@xbN@w�@w�w@w��@v��@uzx@tV�@s��@s�a@s�V@se�@s$t@r�y@r��@rTa@qԕ@q+�@pm�@p~@pG@o�Q@o�0@o��@oy�@o)_@n�]@n��@m�@m�@lx@k�&@k��@kƨ@k�@j~�@j=q@j4@j&�@jH�@i��@i�^@i��@iO�@h-�@gC@f?@e�@ef�@d�v@d/�@c��@c��@c33@bR�@a�>@a�@a^�@a%F@`Ɇ@`<�@_��@_C�@_
=@^��@^�6@^u%@^B[@^	@]�n@\�)@\!@[�]@[�0@[t�@Z��@Z�@Z�R@Zv�@Y�j@Y�@X��@W��@WX�@W�@V��@V��@V}V@Vd�@VOv@V-@U��@U�7@U%F@T��@T!@S�@S;d@R�B@R� @R-@Q[W@P�v@P�@P��@PS�@P �@O�k@OS�@O�@Nں@N��@N��@N�F@N��@N{�@Nq�@Np;@Nc @NJ@M�#@M�@M��@M��@M�X@M|@L�U@K��@K"�@JQ@I��@I?}@I�@H��@HD�@Hx@G~�@GX�@G�@F��@Fq�@FW�@F?@F8�@E��@Ex�@E8�@D�`@D��@D��@C��@C�a@C~�@C,�@B�y@Bl�@B@�@B@A�@As�@A�@@|�@@:�@?�@@?�	@?v`@>��@>�1@>J�@>�@=�@=��@=�n@=�h@=rG@=7L@<�|@<�u@<j@<%�@;o@:��@:� @:Ta@9�)@9��@9��@9T�@8��@7��@7� @7��@7,�@6͟@6��@6��@6�b@6L0@6u@5��@5c@5c�@5\�@5L�@5�@4�$@4�@4h�@4�@3��@3�V@3y�@3j�@3j�@3O@3�@2��@2e@1��@1L�@1+�@0��@0Q�@/� @/K�@.��@.��@.�L@.�@.=q@-�@-�z@-j@,�`@,7@+��@+�}@+� @+�}@+��@+��@+j�@+,�@*�]@*d�@)�S@)q@(Ɇ@(�4@(��@(��@(�@(M@'�@'�@'�$@'\)@'F�@&�s@&�@&i�@&.�@%��@%�X@%s�@%k�@%k�@%Q�@%=�@%�@$�@$j@$C-@$'R@$@$@#�w@#�{@#A�@"��@"��@"i�@"{@!�@!J�@!�@ �@ �9@ ��@ w�@ Q�@ 2�@��@�{@W?@@O@"�@ں@s�@�@�@��@�@hs@F@(�@��@��@��@��@�@Z@/�@�@��@�f@y�@P�@�@�@�R@��@H�@)�@�@�3@��@}�@S&@q@�@�.@c�@G@ƨ@�k@y�@4�@S@�8@�M@��@��@E�@��@�@j@\�@:�@ی@g8@4n@��@x@_p@J#@)_@�,@xl@)�@��@�@k�@�@�f@��@��@[�@:�@ �@�@�
@��@n/@>�@�@��@��@��@��@i�@C�@)�@{@��@�>@�#@�^@�X@}�@S&@7L@!�@�@@�@��@�@�@��@�I@��@�@PH111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�c�A�h�A�m)A�hsA�iDA�`BA�_pA�dZA�k�A�poA�v�A�q�A�f�A�e�A�ffA�]/A�]�A�Y�A�S&A�N<A�GEA�JA��A��iA��cA���A��8A���A���Aʱ�A�x�A��KA�=�A�o�A��A�,A���A��A��1A���A�Y�A��A���A�/�A�"�A�S�A��A��aA�3hA���A�M�A��A�D3A��A�2�A�#nA��A�7A��UA��:A� 4A�}VA��\A��AA���A�ٴA�E�A��HA��|A�B�A���A��+A�~�A�fA�-wA�k�A�WsA�A��A���A��A�_A�,A� �A�8RA�v+A�jKA���A}�EAt{JAn$�Al9�Ai�HAfR�Ac��AaGEA]�AY�AU��AR�AO�OAM�hAK<6AJ5?AH��AD��AC�AAB�)ABQ�AA��A@�A>8A=-�A<�9A<uA;	lA8�A7l�A81'A7�"A7�A4�A2�,A2VA1*0A07LA/��A.B[A-I�A,Y�A+1A*ƨA*=qA)PHA(g8A&��A%=A$($A!:�A�AB[AC�A�EA��AAo A�rA($A��A�1A�uA��A��A�&A+�A�IAFtAz�A+A҉A�A��A��A��A3�A�jA�6AjA+A��A�A4nA*0AA�AcA�MA5?A33A��A@A}�A<6A�A�vAu�A4nA	lA
l�A	�MA	��A	Y�A�jAW�A+�A�A��A�AqvAP�A iA��AcAg�A=A�/A�xA2aA��AA�zA�MAK^A�]A��AZ�AϫA>�A
�A �cA ��A C�@���@�n/@��?@�{@��/@���@�y>@��@�B�@��R@��.@���@��'@���@�S@��W@�IR@�_@���@�8@���@�O@�O@��@��W@��@��@��@�j�@�\)@�4�@�Ɇ@�Q@��@�"�@�r@�ƨ@濱@�p;@�{@�Z�@�R@�I@�+k@��@�m]@��@�)�@�J�@�z@���@�\�@ެ@��@��v@�@ۣn@�X�@�e�@��9@�Z�@��f@�l"@��K@�U�@ցo@��@�%@�ߤ@Է�@�tT@�5?@�x@��`@�~�@�i�@��@�|@�@л�@�S�@��#@�?}@Κ�@���@���@��3@˟V@˓�@�iD@�(@���@ʎ�@�@ɱ[@�b�@ȸR@�s�@��@ǿH@�p�@� i@Ƶ�@�!@��g@Ū�@Ľ<@���@�@�e@��T@���@���@�;�@��@�1@��@��A@��@��^@�@�{�@�K^@�ݘ@�L�@��@��@��f@��U@�Z@��@� �@�˒@�Z�@���@��f@�G�@��[@�|�@�[�@�N�@�7@��]@��9@�e,@��9@�:�@�  @��+@���@��}@�}�@�6z@��@��@��t@�g�@�_p@��@��R@�C-@��[@���@�8@���@��]@���@��!@��u@�Xy@��@��L@��@�dZ@��@��O@��@�M@��*@�P�@��@���@�]d@��@��j@���@���@�7@��6@��"@��`@��T@�qv@�[W@�/�@��@��F@�l"@��@��P@�a@�H�@�C@��@��@��I@�,=@��@��t@��@���@���@��-@���@�x�@�N<@���@�H@��C@�.I@��@�ȴ@���@�c�@���@�1�@��@�^5@�0U@���@��k@�@O@��,@�|�@��@��@��=@�s�@�S&@�<6@��@���@�kQ@�N�@�:�@���@���@�hs@�&@�@��@���@���@���@�C�@��@�ƨ@���@���@�e�@�,�@��\@���@�C�@��d@���@�F�@��/@��@�q�@�1�@�  @��-@�/�@���@�<�@���@��
@�dZ@�2a@��@�ں@��4@��@���@�h�@�)�@���@�\�@�@@��y@���@�]d@�+k@�v`@���@��@�tT@�y>@�l"@�_�@�  @��Z@�@���@��w@��P@�x�@�S&@�C�@�;d@�)_@�@��@���@�tT@�D�@�~@��#@��}@���@�g�@�RT@�Mj@�N<@�Q�@�5�@���@��@���@�u%@��@��@��Q@���@��@���@��S@�|�@�W?@��@���@���@�\�@�	@�ϫ@��'@�^�@��@� i@��5@���@�V@�N�@�?@� �@��3@���@�hs@�@@��@�bN@�&�@��@�@J#@~��@~d�@}��@}@|�_@|�Y@|N�@|	�@{{J@{/�@z��@y��@yϫ@y\�@xbN@w�@w�w@w��@v��@uzx@tV�@s��@s�a@s�V@se�@s$t@r�y@r��@rTa@qԕ@q+�@pm�@p~@pG@o�Q@o�0@o��@oy�@o)_@n�]@n��@m�@m�@lx@k�&@k��@kƨ@k�@j~�@j=q@j4@j&�@jH�@i��@i�^@i��@iO�@h-�@gC@f?@e�@ef�@d�v@d/�@c��@c��@c33@bR�@a�>@a�@a^�@a%F@`Ɇ@`<�@_��@_C�@_
=@^��@^�6@^u%@^B[@^	@]�n@\�)@\!@[�]@[�0@[t�@Z��@Z�@Z�R@Zv�@Y�j@Y�@X��@W��@WX�@W�@V��@V��@V}V@Vd�@VOv@V-@U��@U�7@U%F@T��@T!@S�@S;d@R�B@R� @R-@Q[W@P�v@P�@P��@PS�@P �@O�k@OS�@O�@Nں@N��@N��@N�F@N��@N{�@Nq�@Np;@Nc @NJ@M�#@M�@M��@M��@M�X@M|@L�U@K��@K"�@JQ@I��@I?}@I�@H��@HD�@Hx@G~�@GX�@G�@F��@Fq�@FW�@F?@F8�@E��@Ex�@E8�@D�`@D��@D��@C��@C�a@C~�@C,�@B�y@Bl�@B@�@B@A�@As�@A�@@|�@@:�@?�@@?�	@?v`@>��@>�1@>J�@>�@=�@=��@=�n@=�h@=rG@=7L@<�|@<�u@<j@<%�@;o@:��@:� @:Ta@9�)@9��@9��@9T�@8��@7��@7� @7��@7,�@6͟@6��@6��@6�b@6L0@6u@5��@5c@5c�@5\�@5L�@5�@4�$@4�@4h�@4�@3��@3�V@3y�@3j�@3j�@3O@3�@2��@2e@1��@1L�@1+�@0��@0Q�@/� @/K�@.��@.��@.�L@.�@.=q@-�@-�z@-j@,�`@,7@+��@+�}@+� @+�}@+��@+��@+j�@+,�@*�]@*d�@)�S@)q@(Ɇ@(�4@(��@(��@(�@(M@'�@'�@'�$@'\)@'F�@&�s@&�@&i�@&.�@%��@%�X@%s�@%k�@%k�@%Q�@%=�@%�@$�@$j@$C-@$'R@$@$@#�w@#�{@#A�@"��@"��@"i�@"{@!�@!J�@!�@ �@ �9@ ��@ w�@ Q�@ 2�@��@�{@W?@@O@"�@ں@s�@�@�@��@�@hs@F@(�@��@��@��@��@�@Z@/�@�@��@�f@y�@P�@�@�@�R@��@H�@)�@�@�3@��@}�@S&@q@�@�.@c�@G@ƨ@�k@y�@4�@S@�8@�M@��@��@E�@��@�@j@\�@:�@ی@g8@4n@��@x@_p@J#@)_@�,@xl@)�@��@�@k�@�@�f@��@��@[�@:�@ �@�@�
@��@n/@>�@�@��@��@��@��@i�@C�@)�@{@��@�>@�#@�^@�X@}�@S&@7L@!�@�@@�@��@�@�@��@�I@��@�@PH111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B�B�B��B��B��B��B�aB�GB�aB�B�B�B�B�B�[B��B��B�B�;B��B��B�qB�qB�=B�B�WB�=B�B��B��BؓB��B�_B��B��B	
#B	?B	a�B	f2B	q�B	��B	�*B	ݘB
�KB
ȚB
�B
�B
�B
��B
�B
��B{Be�B�'B�9B��B��B�9B�Bg�BXB@�B[#Bs�BRoB�B B@�Bt�B�$B�zB��Bd�BB'B�B
�"B
��B
�MB
�tB
�$B
��B
}�B
d�B
N"B
,=B
B
+B	�B	�B	u�B	lWB	c B	Q�B	E�B	=�B	-�B	�B		�B��B�DB�fB�B�XB��B��B֡B��B�TBѷBچB��B��B�_BںB�B�-B��B	kB	,qB	)�B	~B	
B	B	(>B	+QB	)�B	+kB	7fB	DB	N<B	RoB	X�B	X�B	[�B	]dB	_pB	g�B	j�B	r�B	�xB	�B	�MB	�\B	��B	�rB	��B	��B	��B	q�B	nB	|�B	~]B	�{B	�hB	� B	��B	��B	�3B	��B	��B	��B	��B	�NB	�`B	�B	��B	�B	��B	� B	�rB	�B	��B	��B	�B	��B	�uB	�MB	ˬB	��B	�B	�NB	�oB	҉B	��B	׍B	��B	�EB	��B	�YB	��B	ևB	��B	��B	�B	�B	��B	�kB	�QB	��B	ٚB	�KB	�B	��B	چB	ݲB	�VB	�\B	߾B	�B	��B	�]B	�B	�/B	��B	�xB	�/B	��B	�B	�;B	��B	��B	ބB	߾B	�!B	��B	�qB	�CB	ݲB	��B	ݘB	�~B	��B	��B	�kB	��B	ۦB	ۦB	��B	�WB	��B	یB	ܒB	�]B	چB	�B	��B	�+B	׍B	��B	ؓB	ٴB	�#B	�B	�KB	�B	�IB	�IB	��B	�xB	��B	�B	�	B	�WB	��B	�~B	�5B	�OB	ޞB	�B	ߊB	�5B	ݘB	��B	޸B	��B	�;B	��B	ݲB	�~B	޸B	ޞB	�vB	�HB	��B	�4B	�B	�B	��B	�-B	�bB	��B	��B	�B	�nB	�nB	��B	��B	�B	�fB	�RB	�fB	�B	�B	�FB	�zB	�B	�B	�B	�B	�LB	�B	�B	�XB	�*B	�eB	�B	�B	�B	�B	�B	��B	�]B	�B	�5B	�B	��B	�B	�UB	�}B	�B	�B	�B	�B	��B	��B	��B	�CB	�/B	�IB	��B	�B	�B	�;B	�B	�;B	��B	�B	��B	��B	�MB	�B	�nB	�B	��B	�>B	��B	��B
GB
�B
gB
�B
�B
gB
MB
�B
�B
-B
�B
�B
�B
9B
mB
9B
�B
mB
9B
9B
�B
gB
9B
gB
�B
�B
�B
B
9B
B
B
�B
�B
3B
GB
�B
 �B	�cB	��B	��B	�6B	��B	�0B	�^B	�xB	�^B	�xB	��B	�*B	�*B	�B	��B	�VB	��B	�B	�B	�B	�dB	��B	�B	�jB	�6B	��B	�<B	��B	��B	��B	��B
  B
 �B
 OB
 �B
 �B	��B
 �B
 iB
�B
�B
�B
9B
�B
?B
YB
�B
EB
EB
�B
zB
�B
�B
�B
	�B
	�B

	B

�B
B
)B
�B
dB
�B
�B
B
B
6B
PB
<B
VB
VB
VB
�B
�B
�B
�B
�B
�B
B
�B
�B
B
�B
@B
&B
�B
&B
�B
�B
�B
FB
�B
�B
,B
�B
MB
�B
?B
�B
B
yB
+B
�B
�B
�B
�B
�B
B
CB
xB
xB
xB
]B
�B
5B
�B
OB
�B
VB
pB
VB
�B
~B
B
dB
jB
jB
�B
�B
B
!HB
"B
"NB
"B
"4B
"hB
"hB
"NB
"4B
"B
"NB
#B
#:B
$B
$@B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%zB
%�B
%�B
%�B
&fB
&�B
&�B
&�B
'B
&�B
'8B
'B
'RB
'mB
'�B
(>B
($B
(�B
(�B
)B
)B
)yB
)�B
)�B
)�B
)�B
)�B
)yB
)�B
)�B
*KB
*eB
*�B
+�B
,WB
,�B
,�B
,�B
-]B
-�B
-�B
.cB
.}B
.�B
./B
./B
./B
.}B
/5B
/iB
0B
0oB
0B
0�B
0�B
0UB
0;B
0;B
0oB
/ B
.IB
-�B
.cB
.}B
.�B
/5B
/OB
/iB
/OB
/B
/�B
0B
/�B
0B
0�B
1B
1�B
2-B
2�B
3B
3�B
3�B
3�B
2�B
2�B
2�B
2|B
2�B
2�B
3�B
49B
4�B
5�B
5�B
7LB
72B
7fB
7fB
6�B
5�B
5�B
5�B
6�B
8B
8RB
8�B
9�B
9�B
9�B
9rB
9rB
9�B
:DB
:^B
;dB
;�B
<B
<PB
=B
="B
=<B
=qB
=�B
>�B
?�B
?cB
>�B
?B
?�B
@ B
@4B
@iB
A B
A�B
BuB
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
E9B
F�B
GzB
HB
H1B
G�B
GzB
GEB
H�B
HKB
G�B
G�B
HKB
HfB
H�B
H�B
H�B
H�B
H�B
H�B
IB
IB
IB
IB
IB
I7B
IB
IB
IlB
I�B
J	B
J	B
I�B
I�B
JXB
J�B
J�B
J�B
KDB
KDB
KxB
K�B
LJB
L�B
L�B
M6B
MB
MjB
M�B
N"B
N"B
N"B
NB
N<B
N�B
N�B
OBB
O(B
OBB
O(B
N�B
OBB
OvB
PbB
PbB
P.B
P�B
QB
QNB
Q�B
R B
RTB
S@B
SB
R�B
S[B
SuB
S�B
S�B
TB
T,B
T,B
TaB
T{B
T�B
T�B
UB
T�B
T�B
V9B
V9B
VSB
VmB
VmB
V�B
W$B
W$B
W�B
X_B
X_B
X�B
X�B
Y1B
YB
X�B
YKB
YB
Y�B
ZB
Z7B
ZQB
ZQB
ZkB
Z�B
Z�B
[	B
[=B
[qB
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\xB
]/B
]dB
]�B
]�B
^B
^�B
^�B
_VB
_�B
_�B
_�B
_�B
`B
`'B
`BB
`�B
aB
a�B
bB
bNB
bNB
b4B
bNB
bNB
b�B
b�B
b�B
c B
d@B
d�B
d�B
e,B
e,B
eFB
e,B
e`B
e�B
e�B
e�B
fLB
f2B
f�B
f�B
gB
gRB
g�B
g�B
h$B
h
B
h
B
h$B
h>B
h>B
h�B
h�B
iB
iB
i*B
iB
iyB
i�B
i�B
jB
j0B
jKB
j0B
i�B
i�B
jKB
kB
k�B
k�B
lB
l=B
l=B
l�B
l�B
mB
mB
m)B
mCB
m]B
m�B
m�B
n/B
ncB
n}B
n�B
n�B
n�B
oB
o5B
oiB
oiB
o�B
o�B
o�B
o�B
pB
pB
p;B
pUB
poB
poB
p�B
p�B
p�B
p�B
q[B
q�B
q�B
q�B
rB
r-B
r�B
r�B
s3B
sMB
shB
s�B
s�B
tB
s�B
s�B
s�B
s�B
t�B
t�B
u%B
u%B
u?B
u%B
u�B
vB
v+B
vzB
v�B
v�B
v�B
v�B
w2B
wLB
wfB
w�B
w�B
xB
x8B
xRB
x�B
x�B
x�B
y	B
y$B
yXB
yXB
y�B
y�B
zB
zDB
z�B
z�B
{�B
{�B
|6B
|B
|B
{�B
{�B
{�B
|�B
}B
}B
}VB
}�B
}�B
}�B
}�B
}�B
~B
~(B
~BB
~(B
~(B
~(B
}�B
~B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B�B�B��B��B��B��B�aB�GB�aB�B�B�B�B�B�[B��B��B�B�;B��B��B�qB�qB�=B�B�WB�=B�B��B��BؓB��B�_B��B��B	
#B	?B	a�B	f2B	q�B	��B	�*B	ݘB
�KB
ȚB
�B
�B
�B
��B
�B
��B{Be�B�'B�9B��B��B�9B�Bg�BXB@�B[#Bs�BRoB�B B@�Bt�B�$B�zB��Bd�BB'B�B
�"B
��B
�MB
�tB
�$B
��B
}�B
d�B
N"B
,=B
B
+B	�B	�B	u�B	lWB	c B	Q�B	E�B	=�B	-�B	�B		�B��B�DB�fB�B�XB��B��B֡B��B�TBѷBچB��B��B�_BںB�B�-B��B	kB	,qB	)�B	~B	
B	B	(>B	+QB	)�B	+kB	7fB	DB	N<B	RoB	X�B	X�B	[�B	]dB	_pB	g�B	j�B	r�B	�xB	�B	�MB	�\B	��B	�rB	��B	��B	��B	q�B	nB	|�B	~]B	�{B	�hB	� B	��B	��B	�3B	��B	��B	��B	��B	�NB	�`B	�B	��B	�B	��B	� B	�rB	�B	��B	��B	�B	��B	�uB	�MB	ˬB	��B	�B	�NB	�oB	҉B	��B	׍B	��B	�EB	��B	�YB	��B	ևB	��B	��B	�B	�B	��B	�kB	�QB	��B	ٚB	�KB	�B	��B	چB	ݲB	�VB	�\B	߾B	�B	��B	�]B	�B	�/B	��B	�xB	�/B	��B	�B	�;B	��B	��B	ބB	߾B	�!B	��B	�qB	�CB	ݲB	��B	ݘB	�~B	��B	��B	�kB	��B	ۦB	ۦB	��B	�WB	��B	یB	ܒB	�]B	چB	�B	��B	�+B	׍B	��B	ؓB	ٴB	�#B	�B	�KB	�B	�IB	�IB	��B	�xB	��B	�B	�	B	�WB	��B	�~B	�5B	�OB	ޞB	�B	ߊB	�5B	ݘB	��B	޸B	��B	�;B	��B	ݲB	�~B	޸B	ޞB	�vB	�HB	��B	�4B	�B	�B	��B	�-B	�bB	��B	��B	�B	�nB	�nB	��B	��B	�B	�fB	�RB	�fB	�B	�B	�FB	�zB	�B	�B	�B	�B	�LB	�B	�B	�XB	�*B	�eB	�B	�B	�B	�B	�B	��B	�]B	�B	�5B	�B	��B	�B	�UB	�}B	�B	�B	�B	�B	��B	��B	��B	�CB	�/B	�IB	��B	�B	�B	�;B	�B	�;B	��B	�B	��B	��B	�MB	�B	�nB	�B	��B	�>B	��B	��B
GB
�B
gB
�B
�B
gB
MB
�B
�B
-B
�B
�B
�B
9B
mB
9B
�B
mB
9B
9B
�B
gB
9B
gB
�B
�B
�B
B
9B
B
B
�B
�B
3B
GB
�B
 �B	�cB	��B	��B	�6B	��B	�0B	�^B	�xB	�^B	�xB	��B	�*B	�*B	�B	��B	�VB	��B	�B	�B	�B	�dB	��B	�B	�jB	�6B	��B	�<B	��B	��B	��B	��B
  B
 �B
 OB
 �B
 �B	��B
 �B
 iB
�B
�B
�B
9B
�B
?B
YB
�B
EB
EB
�B
zB
�B
�B
�B
	�B
	�B

	B

�B
B
)B
�B
dB
�B
�B
B
B
6B
PB
<B
VB
VB
VB
�B
�B
�B
�B
�B
�B
B
�B
�B
B
�B
@B
&B
�B
&B
�B
�B
�B
FB
�B
�B
,B
�B
MB
�B
?B
�B
B
yB
+B
�B
�B
�B
�B
�B
B
CB
xB
xB
xB
]B
�B
5B
�B
OB
�B
VB
pB
VB
�B
~B
B
dB
jB
jB
�B
�B
B
!HB
"B
"NB
"B
"4B
"hB
"hB
"NB
"4B
"B
"NB
#B
#:B
$B
$@B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%zB
%�B
%�B
%�B
&fB
&�B
&�B
&�B
'B
&�B
'8B
'B
'RB
'mB
'�B
(>B
($B
(�B
(�B
)B
)B
)yB
)�B
)�B
)�B
)�B
)�B
)yB
)�B
)�B
*KB
*eB
*�B
+�B
,WB
,�B
,�B
,�B
-]B
-�B
-�B
.cB
.}B
.�B
./B
./B
./B
.}B
/5B
/iB
0B
0oB
0B
0�B
0�B
0UB
0;B
0;B
0oB
/ B
.IB
-�B
.cB
.}B
.�B
/5B
/OB
/iB
/OB
/B
/�B
0B
/�B
0B
0�B
1B
1�B
2-B
2�B
3B
3�B
3�B
3�B
2�B
2�B
2�B
2|B
2�B
2�B
3�B
49B
4�B
5�B
5�B
7LB
72B
7fB
7fB
6�B
5�B
5�B
5�B
6�B
8B
8RB
8�B
9�B
9�B
9�B
9rB
9rB
9�B
:DB
:^B
;dB
;�B
<B
<PB
=B
="B
=<B
=qB
=�B
>�B
?�B
?cB
>�B
?B
?�B
@ B
@4B
@iB
A B
A�B
BuB
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
E9B
F�B
GzB
HB
H1B
G�B
GzB
GEB
H�B
HKB
G�B
G�B
HKB
HfB
H�B
H�B
H�B
H�B
H�B
H�B
IB
IB
IB
IB
IB
I7B
IB
IB
IlB
I�B
J	B
J	B
I�B
I�B
JXB
J�B
J�B
J�B
KDB
KDB
KxB
K�B
LJB
L�B
L�B
M6B
MB
MjB
M�B
N"B
N"B
N"B
NB
N<B
N�B
N�B
OBB
O(B
OBB
O(B
N�B
OBB
OvB
PbB
PbB
P.B
P�B
QB
QNB
Q�B
R B
RTB
S@B
SB
R�B
S[B
SuB
S�B
S�B
TB
T,B
T,B
TaB
T{B
T�B
T�B
UB
T�B
T�B
V9B
V9B
VSB
VmB
VmB
V�B
W$B
W$B
W�B
X_B
X_B
X�B
X�B
Y1B
YB
X�B
YKB
YB
Y�B
ZB
Z7B
ZQB
ZQB
ZkB
Z�B
Z�B
[	B
[=B
[qB
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\xB
]/B
]dB
]�B
]�B
^B
^�B
^�B
_VB
_�B
_�B
_�B
_�B
`B
`'B
`BB
`�B
aB
a�B
bB
bNB
bNB
b4B
bNB
bNB
b�B
b�B
b�B
c B
d@B
d�B
d�B
e,B
e,B
eFB
e,B
e`B
e�B
e�B
e�B
fLB
f2B
f�B
f�B
gB
gRB
g�B
g�B
h$B
h
B
h
B
h$B
h>B
h>B
h�B
h�B
iB
iB
i*B
iB
iyB
i�B
i�B
jB
j0B
jKB
j0B
i�B
i�B
jKB
kB
k�B
k�B
lB
l=B
l=B
l�B
l�B
mB
mB
m)B
mCB
m]B
m�B
m�B
n/B
ncB
n}B
n�B
n�B
n�B
oB
o5B
oiB
oiB
o�B
o�B
o�B
o�B
pB
pB
p;B
pUB
poB
poB
p�B
p�B
p�B
p�B
q[B
q�B
q�B
q�B
rB
r-B
r�B
r�B
s3B
sMB
shB
s�B
s�B
tB
s�B
s�B
s�B
s�B
t�B
t�B
u%B
u%B
u?B
u%B
u�B
vB
v+B
vzB
v�B
v�B
v�B
v�B
w2B
wLB
wfB
w�B
w�B
xB
x8B
xRB
x�B
x�B
x�B
y	B
y$B
yXB
yXB
y�B
y�B
zB
zDB
z�B
z�B
{�B
{�B
|6B
|B
|B
{�B
{�B
{�B
|�B
}B
}B
}VB
}�B
}�B
}�B
}�B
}�B
~B
~(B
~BB
~(B
~(B
~(B
}�B
~B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104955  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175319  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175319  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175319                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025327  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025327  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                