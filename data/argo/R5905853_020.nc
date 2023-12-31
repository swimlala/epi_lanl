CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:26:10Z creation;2022-06-04T17:26:10Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604172610  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�օ�F�1   @�ֆ1~K@.��E���c}����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�ffB���B�  B�33B�  B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�C�fC	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(L�C)�fC+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @(Q�@{�@��\@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\B���B��)B�B�B���B��)B�\B��)B��)B�B�BӨ�Bר�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�zC	�zC�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C(:�C)�zC+�zC-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�zCG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C^�C_�Ca�Cc�Ce�zCg�Ci�Ck�Cm�Cp�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D*�D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN��DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�s�A�g�A�"A�˒A��dA��[A�A�m]A�	�A��A� �A���A���A���A��A��A���A���A���A���A���A��A��A���A��A��;A��5A��dA�ܒA�ںA�ٴA��A��gA��&A��HA��A᥯A�?HA��A�:*A��`A�A�v�AطA�>Aɩ_A�:A�b�A�A�$�A��^A��#A��7A�f2A���A��+A���A��A�-�A�"hA�+A�~A���A�gA��9A��XA���A��A��PA�xA��A�qA�B'A��(A��[A��!A��WA�kA���A��A���A��A�!A{2aAu�-AsxlAq/Am��AkخAh�A`�xA[��AXi�AV�PASȴAQ��AM=�AIVAF�kAD%�A?J�A>��A>A<��A9�A8�A7V�A6 �A4zxA3��A3DgA2ĜA2��A1�;A0یA/��A.y�A-��A-��A-�A,�4A+�PA)��A(��A'�KA'@A&K�A&{A%QA$��A%S�A&��A'��A%�A%�A$kQA%-A$�qA$(A#z�A"<�A"+A"�A!��A @�A�AkQA��A��A9XAS�A֡A�FA�A��A��A�wA\�A4A��Av`AA=�A͟AjA�3A=qA�AL0A��AC-A6A�]A��A($AYKAJ#A�A�.A2�AS�A��A�.A��AA�+AIRAP�AZ�AXyAL�A.�A  A
�VA
6A	�RA	QA	 �A��A�AW�A�AFtA�zA�A1A~(AOA��A��A ��A X�A u@�E9@���@���@��6@��M@��@�+k@���@�8@�,=@��F@���@�P�@�!-@��8@��@�p�@�X�@�?}@���@��o@�;�@���@��'@�?}@��j@���@��@���@�l�@��@�n@��@��@��@�	@�v�@�w@�Ĝ@�g8@�D�@�:�@�&�@��@�A�@��@�@�9X@�ϫ@��@�Y@�V@��.@�ѷ@�  @�	l@�e�@�	�@��}@ḻ@�S@�5�@�u�@�"h@߲-@��@ެ@�O@�l�@�/�@��@��H@���@ܰ�@�;�@�ݘ@ڣ@�u@��@؆Y@� \@�҉@֍�@��@�f�@�_�@��@��@ҕ�@Ѥ@@�@Ь�@�h�@�I�@�7�@�@��"@���@�zx@�z�@ˠ�@ʴ9@ʁo@ʇ�@ʋD@�r�@�M@�:*@��@��a@���@�xl@�E�@��@�X�@Ƶ@��@�=�@�q@��H@�Z�@��o@�~�@�X�@�P�@���@��>@���@�O�@���@�K^@�&�@��@�8@���@�r�@�oi@��
@�+@��@���@�+k@��@���@���@�w2@�x�@�[W@�\)@�X�@�K�@��@�l�@���@�P�@��@�6�@���@��z@��*@��k@�ѷ@�V@�B[@�-@��@��:@�dZ@�Y�@�j@��@�}V@�	@��@�H�@��@���@�b@��6@���@�Q�@��K@�s�@� �@���@� i@���@�%�@��@��@��h@�T�@��@�[�@�>B@��r@�l"@�}V@��'@�Y@���@���@�y>@�V�@��>@�E9@��@��<@�oi@�g8@�YK@�{@�@�rG@��@��@��u@�W�@�o @���@�<�@�	@�˒@���@�@O@��@�(@��@���@�V@���@�a@�u�@��@���@�B�@��K@���@�1'@�,=@�-�@�"h@��Z@���@��=@�K�@��p@�z�@�u�@�Ov@�=q@��@���@��{@�F�@�	l@��K@�ȴ@���@���@���@��Y@�7�@�ϫ@��7@�x@�e�@�H�@��@��@��8@���@��F@�ƨ@�_p@�	l@���@�($@�g�@�&�@�
=@���@���@�l�@��6@�c@�?}@��@��@��x@�@��t@��~@�j�@�9�@��'@�7�@���@��C@�G�@�0�@�@@��`@��@���@�M@�+k@���@�B�@��|@��@��@�|�@�Ov@�&�@�1@�j�@��@��b@�Q@�{@�u@��@���@�j@�U�@�@��[@�`�@�;�@���@���@���@�t�@�;d@�	l@��@��@��U@�c�@�($@���@���@��@@�l�@�o@���@�w�@�6@��+@��3@���@�\�@�&�@���@��Y@�3�@���@�U�@��@���@��@��o@��@���@�l"@�*�@�{@���@��j@��9@���@�hs@�\)@�<6@���@�2�@�4@��@�:@H�@~ں@~��@~YK@~0U@}�@}�@|�j@|�.@|g8@|C-@{�+@{dZ@z��@z_�@z�@y�o@y�9@y \@x�|@x�/@x�I@w�@w��@w+@w6z@v��@v�@vkQ@u�t@t%�@s$t@r�@r�@r��@r�x@r� @rZ�@q�T@q��@q�@p�@p�O@p��@pV�@p7�@p!@o��@o�V@o�@n��@nYK@m�3@mc@me,@m�@lc�@kF�@jff@je@i��@i<6@h�f@h*�@g�a@g�f@gS�@fq�@ek�@e;@d�U@dK^@d!@c�&@c8@b��@b�m@b�@a�t@a/@`��@`�@`��@`u�@`V�@`-�@_�A@_�:@_,�@^��@^1�@]�@]��@]�@\��@\1'@\�@[�@[K�@Z�@Z�+@Z\�@ZO@Z�@Y�@Ya�@X�@X_@X2�@X  @W�6@W�6@W�q@Wt�@V��@U�@U��@U��@Uk�@U?}@U�@T�[@T[�@Sخ@S��@Sn/@SW?@S�@R� @RH�@R1�@R@R4@R@Q��@P�?@P�o@PM@P,=@P*�@P�@O��@O�6@O��@O��@O�[@O��@Oo�@O@O@Nh
@M��@Mo @M`B@M+�@Lz�@L7@K�+@K�@K�@J�"@J��@J��@J�A@J_�@J�@I�d@I�n@Ia�@I/@I@Iq@H��@H�@H7@G�a@Gqv@G�@F��@F�y@F�@F{@E��@EDg@E0�@D��@C�$@CRT@C;d@C/�@C�@B�,@B=q@A��@A��@A!�@@�@@�@?�V@?6z@>ں@>��@>��@>c @>5?@=��@=�z@=��@<��@<@;ݘ@;��@:��@:s�@:Ov@:@9�@9G�@9@@8��@8��@8_@89X@8'R@8�@8@7��@7�
@7'�@6��@6Q@5�@5Q�@5�@4��@3�@3'�@2�@2��@2��@2�@2�,@2��@2+k@1��@1��@1��@1�z@1��@1-w@0tT@0/�@/�@/��@/��@/U�@.�"@.�R@.�r@.E�@-ԕ@-*0@,��@,��@,r�@,Xy@,�@+��@+/�@*�m@*=q@)�@)��@)�"@)S&@)@(�@(�U@(��@(y>@(N�@'�+@'��@'F�@'
=@&}V@&6�@&�@& �@%�@%S&@$�E@$��@#ݘ@#dZ@#e�@#_p@#J#@#>�@#�@"ߤ@"B[@"	@!�@!hs@ ��@ ��@ �I@ _@ �@�Q@�}@�@��@�V@O@&@��@��@s�@Z�@C�@	@��@8�@��@��@�?@�4@C-@@1@��@��@x@�@�B@��@��@�h@�6@�h@� @��@B[@��@��@��@�@f�@=�@�f@��@�e@V�@�r@��@��@]�@/�@�@�c@ں@�@��@��@��@��@�}@� @^5@:*@($@	@�>@��@w2@c�@IR@�@��@�D@*�@�m@�}@��@�$@;d@&@��@�"@��@�c@҉@��@��@��@Q@�D@��@�C@�~@}�@T�@@�5@�)@�@PH@�6@�F@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�s�A�g�A�"A�˒A��dA��[A�A�m]A�	�A��A� �A���A���A���A��A��A���A���A���A���A���A��A��A���A��A��;A��5A��dA�ܒA�ںA�ٴA��A��gA��&A��HA��A᥯A�?HA��A�:*A��`A�A�v�AطA�>Aɩ_A�:A�b�A�A�$�A��^A��#A��7A�f2A���A��+A���A��A�-�A�"hA�+A�~A���A�gA��9A��XA���A��A��PA�xA��A�qA�B'A��(A��[A��!A��WA�kA���A��A���A��A�!A{2aAu�-AsxlAq/Am��AkخAh�A`�xA[��AXi�AV�PASȴAQ��AM=�AIVAF�kAD%�A?J�A>��A>A<��A9�A8�A7V�A6 �A4zxA3��A3DgA2ĜA2��A1�;A0یA/��A.y�A-��A-��A-�A,�4A+�PA)��A(��A'�KA'@A&K�A&{A%QA$��A%S�A&��A'��A%�A%�A$kQA%-A$�qA$(A#z�A"<�A"+A"�A!��A @�A�AkQA��A��A9XAS�A֡A�FA�A��A��A�wA\�A4A��Av`AA=�A͟AjA�3A=qA�AL0A��AC-A6A�]A��A($AYKAJ#A�A�.A2�AS�A��A�.A��AA�+AIRAP�AZ�AXyAL�A.�A  A
�VA
6A	�RA	QA	 �A��A�AW�A�AFtA�zA�A1A~(AOA��A��A ��A X�A u@�E9@���@���@��6@��M@��@�+k@���@�8@�,=@��F@���@�P�@�!-@��8@��@�p�@�X�@�?}@���@��o@�;�@���@��'@�?}@��j@���@��@���@�l�@��@�n@��@��@��@�	@�v�@�w@�Ĝ@�g8@�D�@�:�@�&�@��@�A�@��@�@�9X@�ϫ@��@�Y@�V@��.@�ѷ@�  @�	l@�e�@�	�@��}@ḻ@�S@�5�@�u�@�"h@߲-@��@ެ@�O@�l�@�/�@��@��H@���@ܰ�@�;�@�ݘ@ڣ@�u@��@؆Y@� \@�҉@֍�@��@�f�@�_�@��@��@ҕ�@Ѥ@@�@Ь�@�h�@�I�@�7�@�@��"@���@�zx@�z�@ˠ�@ʴ9@ʁo@ʇ�@ʋD@�r�@�M@�:*@��@��a@���@�xl@�E�@��@�X�@Ƶ@��@�=�@�q@��H@�Z�@��o@�~�@�X�@�P�@���@��>@���@�O�@���@�K^@�&�@��@�8@���@�r�@�oi@��
@�+@��@���@�+k@��@���@���@�w2@�x�@�[W@�\)@�X�@�K�@��@�l�@���@�P�@��@�6�@���@��z@��*@��k@�ѷ@�V@�B[@�-@��@��:@�dZ@�Y�@�j@��@�}V@�	@��@�H�@��@���@�b@��6@���@�Q�@��K@�s�@� �@���@� i@���@�%�@��@��@��h@�T�@��@�[�@�>B@��r@�l"@�}V@��'@�Y@���@���@�y>@�V�@��>@�E9@��@��<@�oi@�g8@�YK@�{@�@�rG@��@��@��u@�W�@�o @���@�<�@�	@�˒@���@�@O@��@�(@��@���@�V@���@�a@�u�@��@���@�B�@��K@���@�1'@�,=@�-�@�"h@��Z@���@��=@�K�@��p@�z�@�u�@�Ov@�=q@��@���@��{@�F�@�	l@��K@�ȴ@���@���@���@��Y@�7�@�ϫ@��7@�x@�e�@�H�@��@��@��8@���@��F@�ƨ@�_p@�	l@���@�($@�g�@�&�@�
=@���@���@�l�@��6@�c@�?}@��@��@��x@�@��t@��~@�j�@�9�@��'@�7�@���@��C@�G�@�0�@�@@��`@��@���@�M@�+k@���@�B�@��|@��@��@�|�@�Ov@�&�@�1@�j�@��@��b@�Q@�{@�u@��@���@�j@�U�@�@��[@�`�@�;�@���@���@���@�t�@�;d@�	l@��@��@��U@�c�@�($@���@���@��@@�l�@�o@���@�w�@�6@��+@��3@���@�\�@�&�@���@��Y@�3�@���@�U�@��@���@��@��o@��@���@�l"@�*�@�{@���@��j@��9@���@�hs@�\)@�<6@���@�2�@�4@��@�:@H�@~ں@~��@~YK@~0U@}�@}�@|�j@|�.@|g8@|C-@{�+@{dZ@z��@z_�@z�@y�o@y�9@y \@x�|@x�/@x�I@w�@w��@w+@w6z@v��@v�@vkQ@u�t@t%�@s$t@r�@r�@r��@r�x@r� @rZ�@q�T@q��@q�@p�@p�O@p��@pV�@p7�@p!@o��@o�V@o�@n��@nYK@m�3@mc@me,@m�@lc�@kF�@jff@je@i��@i<6@h�f@h*�@g�a@g�f@gS�@fq�@ek�@e;@d�U@dK^@d!@c�&@c8@b��@b�m@b�@a�t@a/@`��@`�@`��@`u�@`V�@`-�@_�A@_�:@_,�@^��@^1�@]�@]��@]�@\��@\1'@\�@[�@[K�@Z�@Z�+@Z\�@ZO@Z�@Y�@Ya�@X�@X_@X2�@X  @W�6@W�6@W�q@Wt�@V��@U�@U��@U��@Uk�@U?}@U�@T�[@T[�@Sخ@S��@Sn/@SW?@S�@R� @RH�@R1�@R@R4@R@Q��@P�?@P�o@PM@P,=@P*�@P�@O��@O�6@O��@O��@O�[@O��@Oo�@O@O@Nh
@M��@Mo @M`B@M+�@Lz�@L7@K�+@K�@K�@J�"@J��@J��@J�A@J_�@J�@I�d@I�n@Ia�@I/@I@Iq@H��@H�@H7@G�a@Gqv@G�@F��@F�y@F�@F{@E��@EDg@E0�@D��@C�$@CRT@C;d@C/�@C�@B�,@B=q@A��@A��@A!�@@�@@�@?�V@?6z@>ں@>��@>��@>c @>5?@=��@=�z@=��@<��@<@;ݘ@;��@:��@:s�@:Ov@:@9�@9G�@9@@8��@8��@8_@89X@8'R@8�@8@7��@7�
@7'�@6��@6Q@5�@5Q�@5�@4��@3�@3'�@2�@2��@2��@2�@2�,@2��@2+k@1��@1��@1��@1�z@1��@1-w@0tT@0/�@/�@/��@/��@/U�@.�"@.�R@.�r@.E�@-ԕ@-*0@,��@,��@,r�@,Xy@,�@+��@+/�@*�m@*=q@)�@)��@)�"@)S&@)@(�@(�U@(��@(y>@(N�@'�+@'��@'F�@'
=@&}V@&6�@&�@& �@%�@%S&@$�E@$��@#ݘ@#dZ@#e�@#_p@#J#@#>�@#�@"ߤ@"B[@"	@!�@!hs@ ��@ ��@ �I@ _@ �@�Q@�}@�@��@�V@O@&@��@��@s�@Z�@C�@	@��@8�@��@��@�?@�4@C-@@1@��@��@x@�@�B@��@��@�h@�6@�h@� @��@B[@��@��@��@�@f�@=�@�f@��@�e@V�@�r@��@��@]�@/�@�@�c@ں@�@��@��@��@��@�}@� @^5@:*@($@	@�>@��@w2@c�@IR@�@��@�D@*�@�m@�}@��@�$@;d@&@��@�"@��@�c@҉@��@��@��@Q@�D@��@�C@�~@}�@T�@@�5@�)@�@PH@�6@�F@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	.B	�B	pB	pB	<B	�B	�B	jB	jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	PB	B	�B	�B	yB	_B	�B	�B��B�0B	�B	6B	ZB	~(B	�MB
�B
�B
�B
��B
҉BcB�sB�FB�B�DB~�Bs3B[=BN�B72B6�BLJBJ�B+6B
��B
�mB
��B�B
�IB
�B
ɺB
��B
�uB
�B
m�B
]�B
D�B
�B	�B	�HB	�)B	��B	�fB	zxB	a-B	:^B	�B	�B		�B�B�B�IBѷB�GB��B�OB�yB�RB�B�>B��B�B�B��B�B�!B�vB�B�9B��B��B�B�XB�B��B��B��B��B�$B�wB	[B	�B	�B	"�B	7fB	QNB	��B	�&B	��B	��B	�'B	�mB	յB	��B	��B	�=B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�cB	��B	�GB	��B	��B	�-B	��B	��B	�?B	�RB	�B	�(B	�<B	�B	�<B	��B	�iB	��B	ּB	�B	��B	��B	�XB	� B	��B	�<B	ĜB	ĜB	�XB	�<B	��B	˒B	�rB	��B	�jB	�<B	�B	�{B	��B	ؓB	�B	�B	�QB	چB	�B	�B	��B	�'B	�-B	�B	�vB	ߤB	��B	�B	ٚB	�_B	�YB	�gB	�[B	�vB	οB	�BB	�vB	˒B	�fB	��B	�	B	�dB	̳B	�B	�PB	�[B	՛B	�9B	ՁB	��B	�9B	خB	�B	�FB	�zB	�B	�B	�RB	��B	�sB	�yB	�_B	��B	�B	�:B	�B	�B	�:B	�,B	�>B	�sB	�DB	�RB	�B	�$B	��B	��B	��B	�B	�=B	�B	��B	��B	�B	�B	�6B	��B	�B	�WB	��B	�=B	��B	�B	��B	��B	�CB	�cB	�B	� B	�B	�B	�B	�B	�vB	�aB	�B	��B	�3B	�3B	�hB	��B	�B	��B	��B	�B	��B	�FB	��B	�ZB	��B	��B	�B	��B	��B	�B	�|B	�B	�B	�TB	��B	�ZB	�?B	��B	�[B	�B	�!B	�/B	��B	��B	�iB	�B	��B	��B	��B	�oB	��B	�UB	�;B	�B	��B	�|B	�MB	��B	�B	�B	��B	�B	�nB	�tB	��B	�?B	��B	�?B	�%B	�%B	��B	��B	��B	��B	�FB	�FB	��B	�^B	��B	�DB	��B	��B	��B	�rB	��B	��B	��B	��B	��B
 �B
 B
;B
 �B	�B	�<B	��B	�"B	�"B	�qB	�BB	�]B	��B	�B	�B	��B	��B
 �B
 OB
 OB
 �B
B
�B
 �B
B
aB
�B
�B
gB
aB
'B
[B
B
'B
;B
oB
�B
UB
�B
UB
�B
�B
�B
�B
B
�B
;B
�B
�B
B
fB
B
�B
�B
�B
�B
B
�B
�B
B
�B
�B
�B
�B
zB
�B
�B
YB
�B
�B
�B
mB
�B
�B
B
?B
�B
+B
�B
�B
	B
	�B

#B
�B
�B
�B
4B
4B
NB
NB
B
 B
B
�B
B
uB
�B
B
 B
�B
�B
�B
�B
�B
�B

B
�B
�B
�B
+B
�B
EB
B
B
�B
B
B
eB
�B
�B
7B
kB
�B
kB
	B
�B
�B
CB
CB
]B
/B
�B
�B
�B
�B
�B
B
�B
�B
�B
VB
!B
pB
�B
�B
�B
�B
 vB
!B
!HB
!|B
"NB
#B
#TB
#�B
$&B
%,B
&fB
&2B
&fB
&�B
&fB
'B
'�B
'�B
'�B
'�B
'�B
'8B
'�B
(sB
)yB
*�B
*�B
+B
+�B
+�B
+�B
,B
,WB
,�B
-B
-]B
.B
-�B
-�B
.B
./B
.B
.B
.cB
.�B
/5B
/OB
/OB
/B
/iB
/�B
0!B
0UB
0�B
0�B
0�B
0�B
1AB
1B
0UB
0B
1B
1B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
3hB
3hB
3�B
3�B
3�B
3�B
4B
3�B
3�B
3�B
2�B
1�B
1'B
0�B
1B
1�B
2-B
2�B
33B
3hB
3�B
4�B
4B
4�B
4�B
4�B
4�B
4�B
4nB
4�B
5�B
7LB
72B
8�B
8�B
8B
8RB
7�B
72B
7B
8�B
8�B
8�B
8�B
8�B
9	B
8�B
9�B
:DB
:�B
:�B
:�B
;0B
;JB
;B
<B
<B
<B
<�B
<�B
<�B
<�B
<�B
="B
=�B
>B
>BB
>�B
>�B
>�B
>�B
?cB
?�B
?�B
?HB
@B
@�B
A;B
A�B
A�B
A�B
A�B
B'B
AB
@�B
A;B
BAB
A�B
BAB
B[B
B�B
B�B
B�B
C�B
CGB
DMB
D3B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F%B
FYB
F�B
GB
G�B
H1B
HKB
H�B
H�B
H�B
I�B
I�B
I�B
JrB
I�B
J=B
J�B
J=B
J=B
J=B
JXB
J�B
JrB
JXB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K)B
J�B
J�B
J�B
K^B
K�B
KDB
K�B
LB
L~B
L~B
L�B
L~B
LJB
L�B
M�B
MPB
MjB
M�B
M�B
M�B
NB
NB
NB
NB
M�B
M�B
M�B
M�B
NVB
N�B
N�B
N�B
NpB
O\B
O(B
O(B
O(B
O�B
P�B
P.B
PHB
PbB
P}B
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
R�B
R�B
S�B
SuB
S�B
S�B
S�B
S�B
S�B
TB
TB
S�B
TFB
U�B
U�B
UMB
T�B
TaB
T�B
U�B
U�B
U�B
VSB
V�B
V�B
W$B
W$B
W�B
W�B
W�B
W�B
W�B
WsB
XB
W�B
X�B
Y1B
YKB
X�B
Z7B
ZB
Z7B
Y�B
ZB
Z�B
[WB
[WB
[�B
[�B
[=B
[�B
[�B
[qB
\B
[�B
]B
\�B
]�B
^5B
]�B
^B
^�B
_B
_�B
_VB
_�B
_;B
_B
_�B
`'B
`�B
aB
`�B
`�B
`�B
`'B
`�B
`�B
a-B
a|B
a�B
a�B
a�B
b�B
c:B
c:B
c�B
c�B
c�B
c�B
d&B
c�B
c�B
d&B
d�B
d�B
eB
e�B
f�B
fB
f2B
f�B
f�B
f�B
gB
gmB
gmB
g8B
g�B
g�B
h
B
hsB
h�B
h�B
hsB
iB
iyB
i�B
i�B
j0B
j�B
kkB
j�B
j�B
k�B
k6B
kkB
kB
k�B
kB
kB
k�B
l=B
k�B
l=B
lqB
l�B
mCB
lWB
l�B
l�B
mB
m�B
m�B
m�B
n�B
n�B
o B
oB
o�B
p!B
poB
p�B
p�B
pUB
pUB
qvB
q'B
q'B
qAB
q�B
q'B
q�B
rB
rGB
rB
r-B
raB
q�B
r�B
q�B
rGB
r|B
shB
shB
s�B
s�B
shB
s�B
t9B
s�B
tTB
u?B
u?B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
vFB
u�B
u�B
vB
vB
u�B
vFB
v�B
vFB
v�B
v�B
v�B
wB
wB
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
yrB
y$B
y�B
y	B
y$B
y$B
y$B
yXB
x�B
y�B
y�B
z^B
y�B
zDB
zxB
z*B
z�B
z�B
z�B
{JB
{0B
z�B
|PB
{�B
{B
{�B
|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	.B	�B	pB	pB	<B	�B	�B	jB	jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	PB	B	�B	�B	yB	_B	�B	�B��B�0B	�B	6B	ZB	~(B	�MB
�B
�B
�B
��B
҉BcB�sB�FB�B�DB~�Bs3B[=BN�B72B6�BLJBJ�B+6B
��B
�mB
��B�B
�IB
�B
ɺB
��B
�uB
�B
m�B
]�B
D�B
�B	�B	�HB	�)B	��B	�fB	zxB	a-B	:^B	�B	�B		�B�B�B�IBѷB�GB��B�OB�yB�RB�B�>B��B�B�B��B�B�!B�vB�B�9B��B��B�B�XB�B��B��B��B��B�$B�wB	[B	�B	�B	"�B	7fB	QNB	��B	�&B	��B	��B	�'B	�mB	յB	��B	��B	�=B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�cB	��B	�GB	��B	��B	�-B	��B	��B	�?B	�RB	�B	�(B	�<B	�B	�<B	��B	�iB	��B	ּB	�B	��B	��B	�XB	� B	��B	�<B	ĜB	ĜB	�XB	�<B	��B	˒B	�rB	��B	�jB	�<B	�B	�{B	��B	ؓB	�B	�B	�QB	چB	�B	�B	��B	�'B	�-B	�B	�vB	ߤB	��B	�B	ٚB	�_B	�YB	�gB	�[B	�vB	οB	�BB	�vB	˒B	�fB	��B	�	B	�dB	̳B	�B	�PB	�[B	՛B	�9B	ՁB	��B	�9B	خB	�B	�FB	�zB	�B	�B	�RB	��B	�sB	�yB	�_B	��B	�B	�:B	�B	�B	�:B	�,B	�>B	�sB	�DB	�RB	�B	�$B	��B	��B	��B	�B	�=B	�B	��B	��B	�B	�B	�6B	��B	�B	�WB	��B	�=B	��B	�B	��B	��B	�CB	�cB	�B	� B	�B	�B	�B	�B	�vB	�aB	�B	��B	�3B	�3B	�hB	��B	�B	��B	��B	�B	��B	�FB	��B	�ZB	��B	��B	�B	��B	��B	�B	�|B	�B	�B	�TB	��B	�ZB	�?B	��B	�[B	�B	�!B	�/B	��B	��B	�iB	�B	��B	��B	��B	�oB	��B	�UB	�;B	�B	��B	�|B	�MB	��B	�B	�B	��B	�B	�nB	�tB	��B	�?B	��B	�?B	�%B	�%B	��B	��B	��B	��B	�FB	�FB	��B	�^B	��B	�DB	��B	��B	��B	�rB	��B	��B	��B	��B	��B
 �B
 B
;B
 �B	�B	�<B	��B	�"B	�"B	�qB	�BB	�]B	��B	�B	�B	��B	��B
 �B
 OB
 OB
 �B
B
�B
 �B
B
aB
�B
�B
gB
aB
'B
[B
B
'B
;B
oB
�B
UB
�B
UB
�B
�B
�B
�B
B
�B
;B
�B
�B
B
fB
B
�B
�B
�B
�B
B
�B
�B
B
�B
�B
�B
�B
zB
�B
�B
YB
�B
�B
�B
mB
�B
�B
B
?B
�B
+B
�B
�B
	B
	�B

#B
�B
�B
�B
4B
4B
NB
NB
B
 B
B
�B
B
uB
�B
B
 B
�B
�B
�B
�B
�B
�B

B
�B
�B
�B
+B
�B
EB
B
B
�B
B
B
eB
�B
�B
7B
kB
�B
kB
	B
�B
�B
CB
CB
]B
/B
�B
�B
�B
�B
�B
B
�B
�B
�B
VB
!B
pB
�B
�B
�B
�B
 vB
!B
!HB
!|B
"NB
#B
#TB
#�B
$&B
%,B
&fB
&2B
&fB
&�B
&fB
'B
'�B
'�B
'�B
'�B
'�B
'8B
'�B
(sB
)yB
*�B
*�B
+B
+�B
+�B
+�B
,B
,WB
,�B
-B
-]B
.B
-�B
-�B
.B
./B
.B
.B
.cB
.�B
/5B
/OB
/OB
/B
/iB
/�B
0!B
0UB
0�B
0�B
0�B
0�B
1AB
1B
0UB
0B
1B
1B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
3hB
3hB
3�B
3�B
3�B
3�B
4B
3�B
3�B
3�B
2�B
1�B
1'B
0�B
1B
1�B
2-B
2�B
33B
3hB
3�B
4�B
4B
4�B
4�B
4�B
4�B
4�B
4nB
4�B
5�B
7LB
72B
8�B
8�B
8B
8RB
7�B
72B
7B
8�B
8�B
8�B
8�B
8�B
9	B
8�B
9�B
:DB
:�B
:�B
:�B
;0B
;JB
;B
<B
<B
<B
<�B
<�B
<�B
<�B
<�B
="B
=�B
>B
>BB
>�B
>�B
>�B
>�B
?cB
?�B
?�B
?HB
@B
@�B
A;B
A�B
A�B
A�B
A�B
B'B
AB
@�B
A;B
BAB
A�B
BAB
B[B
B�B
B�B
B�B
C�B
CGB
DMB
D3B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F%B
FYB
F�B
GB
G�B
H1B
HKB
H�B
H�B
H�B
I�B
I�B
I�B
JrB
I�B
J=B
J�B
J=B
J=B
J=B
JXB
J�B
JrB
JXB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K)B
J�B
J�B
J�B
K^B
K�B
KDB
K�B
LB
L~B
L~B
L�B
L~B
LJB
L�B
M�B
MPB
MjB
M�B
M�B
M�B
NB
NB
NB
NB
M�B
M�B
M�B
M�B
NVB
N�B
N�B
N�B
NpB
O\B
O(B
O(B
O(B
O�B
P�B
P.B
PHB
PbB
P}B
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
R�B
R�B
S�B
SuB
S�B
S�B
S�B
S�B
S�B
TB
TB
S�B
TFB
U�B
U�B
UMB
T�B
TaB
T�B
U�B
U�B
U�B
VSB
V�B
V�B
W$B
W$B
W�B
W�B
W�B
W�B
W�B
WsB
XB
W�B
X�B
Y1B
YKB
X�B
Z7B
ZB
Z7B
Y�B
ZB
Z�B
[WB
[WB
[�B
[�B
[=B
[�B
[�B
[qB
\B
[�B
]B
\�B
]�B
^5B
]�B
^B
^�B
_B
_�B
_VB
_�B
_;B
_B
_�B
`'B
`�B
aB
`�B
`�B
`�B
`'B
`�B
`�B
a-B
a|B
a�B
a�B
a�B
b�B
c:B
c:B
c�B
c�B
c�B
c�B
d&B
c�B
c�B
d&B
d�B
d�B
eB
e�B
f�B
fB
f2B
f�B
f�B
f�B
gB
gmB
gmB
g8B
g�B
g�B
h
B
hsB
h�B
h�B
hsB
iB
iyB
i�B
i�B
j0B
j�B
kkB
j�B
j�B
k�B
k6B
kkB
kB
k�B
kB
kB
k�B
l=B
k�B
l=B
lqB
l�B
mCB
lWB
l�B
l�B
mB
m�B
m�B
m�B
n�B
n�B
o B
oB
o�B
p!B
poB
p�B
p�B
pUB
pUB
qvB
q'B
q'B
qAB
q�B
q'B
q�B
rB
rGB
rB
r-B
raB
q�B
r�B
q�B
rGB
r|B
shB
shB
s�B
s�B
shB
s�B
t9B
s�B
tTB
u?B
u?B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
vFB
u�B
u�B
vB
vB
u�B
vFB
v�B
vFB
v�B
v�B
v�B
wB
wB
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
yrB
y$B
y�B
y	B
y$B
y$B
y$B
yXB
x�B
y�B
y�B
z^B
y�B
zDB
zxB
z*B
z�B
z�B
z�B
{JB
{0B
z�B
|PB
{�B
{B
{�B
|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104850  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172610  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172610  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172610                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022618  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022618  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                