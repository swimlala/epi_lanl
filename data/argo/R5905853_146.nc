CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-02-08T00:52:29Z creation;2023-02-08T00:52:30Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230208005229  20230208010706  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�5@��|1   @�5�M^p@.��R�c&E����1   GPS     A   B   B   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�33@���A   A!��A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bj��Bo��Bw��B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  C �C�C  C  C�fC	�fC�fC  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D��3D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�R@���@��\@�A z�A>�HA^�HA}G�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBj�BoQ�BwQ�B�RB���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B���B�u�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��B��B��)B��)B��)B��)B��)C �C�C�C�C�zC	�zC�zC�C�C�C�C�C�C�C�C�C�C!�C#�zC%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CZ�C[�C]�C_�Ca�Cc�Cf�Ch�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG��DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DS�DS��DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�D���D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AʝAʞAʝ�AʤAʦ�AʥAʤ�AʥFAʣ�Aʤ@AʥAʦAʧAʧ�Aʧ�AʨXAʩ_Aʩ�AʫAʬ�Aʮ�AʮIAʭCAʭ�Aʭ�AʮIAʯ�Aʰ!Aʱ�Aʯ�A�rA�($Aɚ�A�uAȲ-AȓAȆ�AȄ�A�p�A�`A�q�A�?�A��A�FA�8RA�� A��A�j�A��jA�8�A�Q�A���A��A�AA���A�$@A��iA�7�A��A���A�7LA���A�˒A�Z�A��cA�tA�EA�$tA�w2A��`A��fA��A��A��#A�~]A� �A��\A�Q�A��A|VAy��Av��Aq�Am�Ak�Aj
=Ag5�Af�uAe�Ad�AcU2A^�A]xA[�AY�AX��AV:�ASk�AR�AN�AJ�AF�HAD,=AAOvA@+A?�A;5?A9W?A8P�A7�)A84nA7oA5��A4l"A1��A/�.A/P�A..�A,�A+�fA*�A*�rA)�A(	A&��A&i�A&A#�mA#uA"�5A"�`A"�9A"p;A"4�A"  A!IRA!%FA �3A R�A�2A��AA.IA��A�Ag8A�jA��A��A7�A~(A��Ab�A�0A�]A�A�A�sAh
AVA�AL�A��ArGA!-A��A	AVA��A�AȴA�AaAu%A,�A�AJ�A��A�
Ae�A�A\�A�AZ�A	�A	OAXA�6Av�A��A=qAA�>A�A�?A��A��A*0A�
A�}AVmAGEA:*A!�A�	A��Ap�AzA �e@���@�u�@�.I@���@��'@��Z@���@�GE@�($@��@��=@�J@�`�@�=@�n�@�;d@�e�@�@��v@�I@�$�@���@��M@�M@�)_@�@�_p@�ی@�Q�@�H�@��@�/@��@�!�@��d@オ@���@�H@��3@ᧇ@�
=@��3@��@��,@�I�@��@ݮ@�o @�"�@��	@�>B@۫�@��)@�2�@���@��K@�8�@�$�@��@��8@ֹ$@�B[@�E9@԰!@�M�@Ӝ@�@O@�Y@Ү}@�*�@�Y�@�z�@Ͻ�@�7L@ι$@�2�@�s�@���@�PH@�~@�p�@ʯO@��@��d@ɡ�@�6z@Ƚ<@���@��"@��"@�ȴ@�oi@Ȅ�@Ȟ�@ȣ@�-�@�\)@���@ƅ�@�Z�@�%�@�k�@ô�@�hs@��@�D�@�  @�c@��B@�oi@��_@�YK@��@�
�@��
@�l�@��L@��@�L�@�Ov@�˒@���@�|�@�A @�҉@�$�@��h@��H@�c @�1@�w2@�Ft@��@��P@��k@��V@���@��4@���@�hs@��@��D@�'R@���@��@��@���@��@���@���@���@���@��@�rG@��v@�Q�@�C�@��[@��@�֡@��,@��	@��@�_�@�A�@�K^@���@���@�K^@��@�@O@��@��@���@�z@�h
@���@�+@��"@�oi@�$@�iD@�q�@�_@�-@��W@��+@��#@�~�@��@�h
@�D�@�H@�B[@�(�@�b@��N@�|@�֡@�s�@�A�@���@��p@�Z�@�!�@��Q@�&�@���@�6@���@�qv@�(@���@���@��@�w�@�6@��@���@��6@��C@�{J@�+@���@�^5@��@�o�@��@���@�GE@��@��H@�c@��@��P@���@�j@��@�خ@���@�zx@�o�@�j�@�?}@�C�@�-w@�@�ȴ@���@�>B@��T@�a�@��@��B@�w�@�C�@�@��@���@��'@��	@���@�s�@��@���@���@��S@�w2@��@��$@��o@�Ft@�-�@��@��@�u�@�9�@�S@��s@��@���@�kQ@�%�@��o@��k@�s�@�S&@��@���@�q@���@��@��@�Z�@�J�@�9�@���@��!@�tT@� �@��@��#@��*@�*0@�֡@�z@�i�@�[�@�Ft@���@�hs@��@��\@�}V@���@�~�@�H@��>@���@�hs@�v`@�o @�X�@�C�@��@�S@�ߤ@��z@��Y@�6�@��W@��a@���@���@�`B@�!-@�ȴ@�Z�@��@��@�@~GE@~@}�Z@}�z@}��@|��@|~@{�	@z�8@z�r@yԕ@x�K@w@O@vkQ@v?@u�T@t��@s��@s�@r�@qϫ@q@pPH@o��@n��@n0U@mhs@l�@l�@l*�@k�@k��@kj�@kS�@j�@j�r@jYK@j	@hѷ@hQ�@h	�@g��@g�a@g|�@gs@gv`@gx@g9�@f͟@f�@fd�@e�@e�n@e�@d��@dFt@c�@cy�@c_p@b�c@b͟@b�'@b�A@b	@a��@`�@`4n@_��@_�*@_S�@^�@^��@^J@]�9@]�@];@\��@\]d@[�w@[qv@[g�@[H�@Z�s@Z�L@Z�r@ZW�@Z
�@Y�z@Y��@YX@X�`@Xl"@X<�@W�@W�f@W!-@V�@V}V@U��@UL�@T�v@T	�@S��@S�0@S��@S)_@R�@R�+@R!�@Q�o@QG�@P��@P�@P�@P_@P2�@O��@O�;@O��@N�@Nxl@N8�@M�"@M�@L�@L`�@L1@K�;@KK�@J�L@J3�@I�j@Izx@I#�@H�@H��@HtT@H�@G��@G=@G�@F��@F��@F��@Fa|@F3�@F�@Eϫ@Ew2@E�@D�j@D�Y@DQ�@D7@Cg�@B�@B�@A��@A��@A[W@A \@@�/@@Ĝ@@Z@?�@@?�@>V@=�@=��@=%F@<(�@;��@;�P@;n/@;>�@;�@:�]@:s�@:3�@9��@9�^@9}�@9*0@8�[@8��@8`�@82�@7�;@76z@6��@6��@6�@6�@6z@6R�@6J@5w2@5*0@5#�@5V@4�f@4��@4m�@4K^@4�@3�w@3�f@3O@3�@3�@2�@2z@2Ov@2GE@1��@1��@1o @1:�@1	l@0�[@0m�@0,=@0�@/��@/�w@/K�@.�8@.��@.M�@-�Z@-�>@-��@-�C@-`B@,��@,��@,bN@,"h@+��@+�k@+e�@+=@*�M@*��@*B[@*�@)�9@)��@)e,@)%F@(�@(��@(bN@(D�@(<�@(:�@(<�@(-�@(�@'�@'�w@'�k@'s@'Mj@&�H@&��@&p;@&^5@&#:@%��@%��@%��@%F@%@@%�@$��@$��@$�o@$q@$4n@#��@#�P@#;d@"�,@"l�@":*@"�@!�^@!��@!zx@!O�@!@@ ��@ ��@ �@ ��@ ��@ �@ ~(@ >B@�a@�q@��@qv@33@��@h
@�@�@rG@V@ی@��@|�@C-@1@� @��@l�@�@�s@{�@C�@�D@�@�@��@m]@Q�@�@�U@U2@��@��@qv@>�@o@��@҉@͟@��@��@��@n�@J�@��@�~@5�@�@�v@��@w�@S�@"h@��@�Q@�a@��@g�@�@��@��@1�@e@�@�@_@��@��@X@4@0�@�@�@�p@��@�.@tT@Q�@6@  @��@��@��@��@dZ@��@��@ff@�@�@�X@��@zx@o @S&@A @7L@�@�/@�9@|�@S�@C-@�@��@e�@Y@
�@
�x@
ff@
8�@	��@	�^@	��@	�M@	p�@	Y�@	J�@	5�@	%F@	�@�K@��@q@Ft@  @ƨ@�[@j�@�@�s@��@��@d�@��@�d@��@o @/@ \@�@�|@�@�[@~(@e�@C-@%�@b@�W@��@�@e�@a@P�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AʝAʞAʝ�AʤAʦ�AʥAʤ�AʥFAʣ�Aʤ@AʥAʦAʧAʧ�Aʧ�AʨXAʩ_Aʩ�AʫAʬ�Aʮ�AʮIAʭCAʭ�Aʭ�AʮIAʯ�Aʰ!Aʱ�Aʯ�A�rA�($Aɚ�A�uAȲ-AȓAȆ�AȄ�A�p�A�`A�q�A�?�A��A�FA�8RA�� A��A�j�A��jA�8�A�Q�A���A��A�AA���A�$@A��iA�7�A��A���A�7LA���A�˒A�Z�A��cA�tA�EA�$tA�w2A��`A��fA��A��A��#A�~]A� �A��\A�Q�A��A|VAy��Av��Aq�Am�Ak�Aj
=Ag5�Af�uAe�Ad�AcU2A^�A]xA[�AY�AX��AV:�ASk�AR�AN�AJ�AF�HAD,=AAOvA@+A?�A;5?A9W?A8P�A7�)A84nA7oA5��A4l"A1��A/�.A/P�A..�A,�A+�fA*�A*�rA)�A(	A&��A&i�A&A#�mA#uA"�5A"�`A"�9A"p;A"4�A"  A!IRA!%FA �3A R�A�2A��AA.IA��A�Ag8A�jA��A��A7�A~(A��Ab�A�0A�]A�A�A�sAh
AVA�AL�A��ArGA!-A��A	AVA��A�AȴA�AaAu%A,�A�AJ�A��A�
Ae�A�A\�A�AZ�A	�A	OAXA�6Av�A��A=qAA�>A�A�?A��A��A*0A�
A�}AVmAGEA:*A!�A�	A��Ap�AzA �e@���@�u�@�.I@���@��'@��Z@���@�GE@�($@��@��=@�J@�`�@�=@�n�@�;d@�e�@�@��v@�I@�$�@���@��M@�M@�)_@�@�_p@�ی@�Q�@�H�@��@�/@��@�!�@��d@オ@���@�H@��3@ᧇ@�
=@��3@��@��,@�I�@��@ݮ@�o @�"�@��	@�>B@۫�@��)@�2�@���@��K@�8�@�$�@��@��8@ֹ$@�B[@�E9@԰!@�M�@Ӝ@�@O@�Y@Ү}@�*�@�Y�@�z�@Ͻ�@�7L@ι$@�2�@�s�@���@�PH@�~@�p�@ʯO@��@��d@ɡ�@�6z@Ƚ<@���@��"@��"@�ȴ@�oi@Ȅ�@Ȟ�@ȣ@�-�@�\)@���@ƅ�@�Z�@�%�@�k�@ô�@�hs@��@�D�@�  @�c@��B@�oi@��_@�YK@��@�
�@��
@�l�@��L@��@�L�@�Ov@�˒@���@�|�@�A @�҉@�$�@��h@��H@�c @�1@�w2@�Ft@��@��P@��k@��V@���@��4@���@�hs@��@��D@�'R@���@��@��@���@��@���@���@���@���@��@�rG@��v@�Q�@�C�@��[@��@�֡@��,@��	@��@�_�@�A�@�K^@���@���@�K^@��@�@O@��@��@���@�z@�h
@���@�+@��"@�oi@�$@�iD@�q�@�_@�-@��W@��+@��#@�~�@��@�h
@�D�@�H@�B[@�(�@�b@��N@�|@�֡@�s�@�A�@���@��p@�Z�@�!�@��Q@�&�@���@�6@���@�qv@�(@���@���@��@�w�@�6@��@���@��6@��C@�{J@�+@���@�^5@��@�o�@��@���@�GE@��@��H@�c@��@��P@���@�j@��@�خ@���@�zx@�o�@�j�@�?}@�C�@�-w@�@�ȴ@���@�>B@��T@�a�@��@��B@�w�@�C�@�@��@���@��'@��	@���@�s�@��@���@���@��S@�w2@��@��$@��o@�Ft@�-�@��@��@�u�@�9�@�S@��s@��@���@�kQ@�%�@��o@��k@�s�@�S&@��@���@�q@���@��@��@�Z�@�J�@�9�@���@��!@�tT@� �@��@��#@��*@�*0@�֡@�z@�i�@�[�@�Ft@���@�hs@��@��\@�}V@���@�~�@�H@��>@���@�hs@�v`@�o @�X�@�C�@��@�S@�ߤ@��z@��Y@�6�@��W@��a@���@���@�`B@�!-@�ȴ@�Z�@��@��@�@~GE@~@}�Z@}�z@}��@|��@|~@{�	@z�8@z�r@yԕ@x�K@w@O@vkQ@v?@u�T@t��@s��@s�@r�@qϫ@q@pPH@o��@n��@n0U@mhs@l�@l�@l*�@k�@k��@kj�@kS�@j�@j�r@jYK@j	@hѷ@hQ�@h	�@g��@g�a@g|�@gs@gv`@gx@g9�@f͟@f�@fd�@e�@e�n@e�@d��@dFt@c�@cy�@c_p@b�c@b͟@b�'@b�A@b	@a��@`�@`4n@_��@_�*@_S�@^�@^��@^J@]�9@]�@];@\��@\]d@[�w@[qv@[g�@[H�@Z�s@Z�L@Z�r@ZW�@Z
�@Y�z@Y��@YX@X�`@Xl"@X<�@W�@W�f@W!-@V�@V}V@U��@UL�@T�v@T	�@S��@S�0@S��@S)_@R�@R�+@R!�@Q�o@QG�@P��@P�@P�@P_@P2�@O��@O�;@O��@N�@Nxl@N8�@M�"@M�@L�@L`�@L1@K�;@KK�@J�L@J3�@I�j@Izx@I#�@H�@H��@HtT@H�@G��@G=@G�@F��@F��@F��@Fa|@F3�@F�@Eϫ@Ew2@E�@D�j@D�Y@DQ�@D7@Cg�@B�@B�@A��@A��@A[W@A \@@�/@@Ĝ@@Z@?�@@?�@>V@=�@=��@=%F@<(�@;��@;�P@;n/@;>�@;�@:�]@:s�@:3�@9��@9�^@9}�@9*0@8�[@8��@8`�@82�@7�;@76z@6��@6��@6�@6�@6z@6R�@6J@5w2@5*0@5#�@5V@4�f@4��@4m�@4K^@4�@3�w@3�f@3O@3�@3�@2�@2z@2Ov@2GE@1��@1��@1o @1:�@1	l@0�[@0m�@0,=@0�@/��@/�w@/K�@.�8@.��@.M�@-�Z@-�>@-��@-�C@-`B@,��@,��@,bN@,"h@+��@+�k@+e�@+=@*�M@*��@*B[@*�@)�9@)��@)e,@)%F@(�@(��@(bN@(D�@(<�@(:�@(<�@(-�@(�@'�@'�w@'�k@'s@'Mj@&�H@&��@&p;@&^5@&#:@%��@%��@%��@%F@%@@%�@$��@$��@$�o@$q@$4n@#��@#�P@#;d@"�,@"l�@":*@"�@!�^@!��@!zx@!O�@!@@ ��@ ��@ �@ ��@ ��@ �@ ~(@ >B@�a@�q@��@qv@33@��@h
@�@�@rG@V@ی@��@|�@C-@1@� @��@l�@�@�s@{�@C�@�D@�@�@��@m]@Q�@�@�U@U2@��@��@qv@>�@o@��@҉@͟@��@��@��@n�@J�@��@�~@5�@�@�v@��@w�@S�@"h@��@�Q@�a@��@g�@�@��@��@1�@e@�@�@_@��@��@X@4@0�@�@�@�p@��@�.@tT@Q�@6@  @��@��@��@��@dZ@��@��@ff@�@�@�X@��@zx@o @S&@A @7L@�@�/@�9@|�@S�@C-@�@��@e�@Y@
�@
�x@
ff@
8�@	��@	�^@	��@	�M@	p�@	Y�@	J�@	5�@	%F@	�@�K@��@q@Ft@  @ƨ@�[@j�@�@�s@��@��@d�@��@�d@��@o @/@ \@�@�|@�@�[@~(@e�@C-@%�@b@�W@��@�@e�@a@P�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	~(B	~B	~]B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	B	~�B	HB	B	~�B	~�B	�B	��B	�B	��B	�B
5�B
a�B
dtB
eFB
f�B
f�B
h>B
o�B
m�B
qB
�iB
�mB
��B
�vB
��B
�!B
�B
�B
�B
�AB+BB
�=B
��B
�HB
�\B
sB
_�B
^�B
\CB
_�B
^�B
X�B
VB
Q�B
Q�B
PHB
BAB
5%B
*0B
	B
�B	��B	�B	��B	�4B	��B	��B	��B	B	jB	b4B	X�B	M�B	IRB	EB	=�B	3�B	5B	�B		B	[B�PB�B�B��B�?B�$B��B�EB}�Bx�BwfBu�Bt9B{BHB�7B�&B��B��B�NB��B�B�B��B��B�EBɆB�dB�QB�B�B��B�FB�-B��B��B�dB	�B		lB	B	*KB	1B	0�B	0B	,�B	�B��B	�B	 �B	7B	KB	�B	W$B	u�B	RoB	SB	nB	k�B	jB	i*B	s3B	�aB	�oB	~�B	{B	v�B	p�B	h�B	a|B	^B	\CB	[�B	i�B	k�B	�MB	��B	��B	�\B	��B	��B	�,B	�B	�*B	��B	�
B	�eB	�B	��B	�*B	��B	�B	��B	�bB	�OB	��B	��B	��B	�FB	�FB	�+B	��B	�B	��B	��B	�tB	��B	�|B	�TB	�ZB	��B	��B	�ZB	�RB	�?B	��B	�_B	�HB	��B	��B	��B	��B	�,B	�SB	��B	��B	��B	�gB	��B	�B	�[B	��B	��B	�B	�qB	�/B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	��B	��B	�;B	��B	��B	��B	�MB	��B	�B	�?B	�2B	��B	�LB	��B	�B	��B	�^B	�qB	�.B	��B	��B	�3B	��B	��B	ƨB	��B	�tB	�_B	�1B	��B	�KB	ȚB	�=B	��B	��B	��B	�dB	�jB	ΊB	�B	�\B	ЗB	��B	бB	�B	�4B	�:B	�B	��B	�\B	ΊB	�B	�B	̳B	�@B	�B	�kB	�)B	�!B	��B	� B	� B	��B	�B	�B	��B	�B	�B	�B	��B	�B	�,B	�2B	�`B	�FB	�FB	�B	�QB	�B	�B	�iB	�B	�B	�B	�B	�yB	�B	��B	�B	�B	�B	��B	�fB	�fB	��B	��B	�zB	�B	�,B	��B	��B	��B	�8B	�_B	�B	�B	�qB	�B	�/B	�B	��B	�B	�B	��B	�B	�AB	�B	��B	�[B	�B	�aB	��B	�B	��B	��B	��B	��B	��B
�B
GB
�B
[B
�B
B
fB
1B
�B
�B
B
�B
�B
zB
zB
B
KB
	7B
	7B
	B
	�B
fB
	B
	�B

=B
0B
dB
�B
DB

XB
�B
B
�B
B
BB
�B
.B
�B
bB
�B
�B
NB
�B
 B
}B
�B
�B
~B
�B
^B
B
JB
�B
B
B
�B
�B
jB
VB
�B
vB
 B
hB
�B
�B
 B
oB
�B
�B
B
&B
FB
FB
�B
2B
B
mB
mB
YB
sB
YB
�B
�B
�B
]B
�B
dB
dB
5B
�B
�B
�B
�B
�B
 vB
 �B
 �B
 �B
!bB
"4B
"hB
"�B
"�B
#:B
#:B
#:B
"�B
#�B
#�B
$@B
$ZB
$ZB
$tB
$�B
$�B
%FB
%FB
%�B
%�B
%�B
&B
&2B
&2B
&�B
&�B
&�B
&�B
'mB
'�B
(�B
(�B
(�B
)DB
)B
)B
)yB
*B
*eB
*�B
*�B
+B
+QB
+�B
,�B
,�B
,�B
,�B
-B
-wB
-]B
-)B
,WB
,�B
.B
.cB
./B
-�B
-CB
.cB
/�B
0UB
0UB
0�B
1B
1[B
1vB
1�B
1�B
2�B
2�B
3B
33B
33B
3MB
3�B
4B
4nB
4�B
4�B
5?B
5tB
5?B
5tB
5tB
5?B
5�B
4�B
4�B
4�B
4�B
4�B
5?B
5?B
4B
4B
4�B
5�B
5ZB
4�B
5B
4�B
5�B
6B
6�B
7LB
8B
8�B
9XB
:*B
:B
:�B
:^B
:�B
:xB
9�B
:*B
:DB
;�B
:�B
;�B
<6B
<�B
=qB
="B
=VB
=<B
=qB
>(B
>�B
>�B
?�B
@�B
@iB
@�B
A�B
A�B
BuB
B�B
B�B
C-B
C-B
CGB
CaB
C�B
C�B
D�B
C�B
C�B
D3B
EB
ESB
E�B
FB
F?B
FtB
GB
GEB
GEB
G�B
G�B
G�B
G�B
H1B
H�B
H�B
HKB
H�B
H�B
I7B
I�B
J#B
JXB
J=B
JrB
J�B
K)B
K)B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
MPB
MjB
N<B
NpB
NpB
OB
O�B
O�B
PB
PHB
PbB
P}B
P}B
P�B
QB
Q�B
QhB
R:B
R�B
R�B
S[B
S�B
S�B
TB
TaB
T�B
T�B
U2B
UgB
UgB
U�B
U�B
VB
VmB
V�B
V�B
V�B
V�B
V�B
W
B
WYB
W�B
W�B
W�B
X+B
XB
X+B
XEB
XyB
X�B
Y�B
ZB
ZkB
ZkB
Z�B
Z�B
[WB
[#B
[=B
[�B
\B
]/B
]IB
]/B
]�B
^OB
^jB
^�B
^�B
^�B
^�B
^�B
_pB
_�B
_�B
_�B
`'B
`vB
`�B
a-B
abB
a�B
a�B
b�B
c B
cnB
c�B
c�B
d@B
c�B
dZB
e`B
e`B
d�B
e�B
d�B
e`B
ezB
ezB
e�B
fB
f2B
fLB
f�B
f�B
f�B
f�B
f�B
f�B
gRB
gB
gRB
gmB
g�B
g�B
h
B
h$B
hXB
h�B
h�B
h�B
i*B
iyB
i�B
jKB
jeB
j�B
j�B
kB
kB
kkB
lB
lqB
lWB
lB
lWB
l=B
lqB
l�B
l�B
m)B
mCB
mCB
m�B
m�B
m�B
m�B
nB
n/B
n/B
nIB
n/B
nIB
n}B
n}B
n�B
n�B
n�B
n�B
oiB
o�B
o�B
o�B
o�B
pB
o�B
p;B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
q'B
qAB
qvB
q�B
q�B
r|B
r�B
r�B
r�B
r�B
sMB
s3B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
wB
wLB
wfB
w�B
w�B
xB
x8B
x�B
x�B
x�B
y	B
y$B
yXB
yrB
yrB
y�B
y�B
zxB
z�B
z�B
{JB
{JB
{dB
{�B
{�B
{�B
{�B
{�B
|B
{�B
|B
|�B
|�B
}B
}VB
}qB
}�B
}�B
}�B
~(B
~]B
~wB
~wB
~�B
~�B
~�B
HB
�B
�B
�4B
�4B
�OB
�4B
�OB
�OB
�B
�B
��B
�;B
�;B
�oB
�oB
��B
��B
��B
��B
�[B
�[B
�AB
�[B
�uB
��B
�{B
�{B
��B
�B
�MB
�B
�3B
�3B
�MB
�gB
�gB
��B
��B
�B
�B
��B
��B
��B
��B
��B
�?B
��B
��B
�+B
�zB
��B
��B
��B
�KB
�KB
�fB
��B
��B
��B
��B
��B
��B
�7B
�RB
��B
��B
��B
��B
�XB
��B
�)B
�DB
�^B
�xB
�B
�0B
�B
��B
��B
�B
��B
�B
�B
�PB
��B
��B
��B
��B
�B
�"B
�<B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	~(B	~B	~]B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	B	~�B	HB	B	~�B	~�B	�B	��B	�B	��B	�B
5�B
a�B
dtB
eFB
f�B
f�B
h>B
o�B
m�B
qB
�iB
�mB
��B
�vB
��B
�!B
�B
�B
�B
�AB+BB
�=B
��B
�HB
�\B
sB
_�B
^�B
\CB
_�B
^�B
X�B
VB
Q�B
Q�B
PHB
BAB
5%B
*0B
	B
�B	��B	�B	��B	�4B	��B	��B	��B	B	jB	b4B	X�B	M�B	IRB	EB	=�B	3�B	5B	�B		B	[B�PB�B�B��B�?B�$B��B�EB}�Bx�BwfBu�Bt9B{BHB�7B�&B��B��B�NB��B�B�B��B��B�EBɆB�dB�QB�B�B��B�FB�-B��B��B�dB	�B		lB	B	*KB	1B	0�B	0B	,�B	�B��B	�B	 �B	7B	KB	�B	W$B	u�B	RoB	SB	nB	k�B	jB	i*B	s3B	�aB	�oB	~�B	{B	v�B	p�B	h�B	a|B	^B	\CB	[�B	i�B	k�B	�MB	��B	��B	�\B	��B	��B	�,B	�B	�*B	��B	�
B	�eB	�B	��B	�*B	��B	�B	��B	�bB	�OB	��B	��B	��B	�FB	�FB	�+B	��B	�B	��B	��B	�tB	��B	�|B	�TB	�ZB	��B	��B	�ZB	�RB	�?B	��B	�_B	�HB	��B	��B	��B	��B	�,B	�SB	��B	��B	��B	�gB	��B	�B	�[B	��B	��B	�B	�qB	�/B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	��B	��B	�;B	��B	��B	��B	�MB	��B	�B	�?B	�2B	��B	�LB	��B	�B	��B	�^B	�qB	�.B	��B	��B	�3B	��B	��B	ƨB	��B	�tB	�_B	�1B	��B	�KB	ȚB	�=B	��B	��B	��B	�dB	�jB	ΊB	�B	�\B	ЗB	��B	бB	�B	�4B	�:B	�B	��B	�\B	ΊB	�B	�B	̳B	�@B	�B	�kB	�)B	�!B	��B	� B	� B	��B	�B	�B	��B	�B	�B	�B	��B	�B	�,B	�2B	�`B	�FB	�FB	�B	�QB	�B	�B	�iB	�B	�B	�B	�B	�yB	�B	��B	�B	�B	�B	��B	�fB	�fB	��B	��B	�zB	�B	�,B	��B	��B	��B	�8B	�_B	�B	�B	�qB	�B	�/B	�B	��B	�B	�B	��B	�B	�AB	�B	��B	�[B	�B	�aB	��B	�B	��B	��B	��B	��B	��B
�B
GB
�B
[B
�B
B
fB
1B
�B
�B
B
�B
�B
zB
zB
B
KB
	7B
	7B
	B
	�B
fB
	B
	�B

=B
0B
dB
�B
DB

XB
�B
B
�B
B
BB
�B
.B
�B
bB
�B
�B
NB
�B
 B
}B
�B
�B
~B
�B
^B
B
JB
�B
B
B
�B
�B
jB
VB
�B
vB
 B
hB
�B
�B
 B
oB
�B
�B
B
&B
FB
FB
�B
2B
B
mB
mB
YB
sB
YB
�B
�B
�B
]B
�B
dB
dB
5B
�B
�B
�B
�B
�B
 vB
 �B
 �B
 �B
!bB
"4B
"hB
"�B
"�B
#:B
#:B
#:B
"�B
#�B
#�B
$@B
$ZB
$ZB
$tB
$�B
$�B
%FB
%FB
%�B
%�B
%�B
&B
&2B
&2B
&�B
&�B
&�B
&�B
'mB
'�B
(�B
(�B
(�B
)DB
)B
)B
)yB
*B
*eB
*�B
*�B
+B
+QB
+�B
,�B
,�B
,�B
,�B
-B
-wB
-]B
-)B
,WB
,�B
.B
.cB
./B
-�B
-CB
.cB
/�B
0UB
0UB
0�B
1B
1[B
1vB
1�B
1�B
2�B
2�B
3B
33B
33B
3MB
3�B
4B
4nB
4�B
4�B
5?B
5tB
5?B
5tB
5tB
5?B
5�B
4�B
4�B
4�B
4�B
4�B
5?B
5?B
4B
4B
4�B
5�B
5ZB
4�B
5B
4�B
5�B
6B
6�B
7LB
8B
8�B
9XB
:*B
:B
:�B
:^B
:�B
:xB
9�B
:*B
:DB
;�B
:�B
;�B
<6B
<�B
=qB
="B
=VB
=<B
=qB
>(B
>�B
>�B
?�B
@�B
@iB
@�B
A�B
A�B
BuB
B�B
B�B
C-B
C-B
CGB
CaB
C�B
C�B
D�B
C�B
C�B
D3B
EB
ESB
E�B
FB
F?B
FtB
GB
GEB
GEB
G�B
G�B
G�B
G�B
H1B
H�B
H�B
HKB
H�B
H�B
I7B
I�B
J#B
JXB
J=B
JrB
J�B
K)B
K)B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
MPB
MjB
N<B
NpB
NpB
OB
O�B
O�B
PB
PHB
PbB
P}B
P}B
P�B
QB
Q�B
QhB
R:B
R�B
R�B
S[B
S�B
S�B
TB
TaB
T�B
T�B
U2B
UgB
UgB
U�B
U�B
VB
VmB
V�B
V�B
V�B
V�B
V�B
W
B
WYB
W�B
W�B
W�B
X+B
XB
X+B
XEB
XyB
X�B
Y�B
ZB
ZkB
ZkB
Z�B
Z�B
[WB
[#B
[=B
[�B
\B
]/B
]IB
]/B
]�B
^OB
^jB
^�B
^�B
^�B
^�B
^�B
_pB
_�B
_�B
_�B
`'B
`vB
`�B
a-B
abB
a�B
a�B
b�B
c B
cnB
c�B
c�B
d@B
c�B
dZB
e`B
e`B
d�B
e�B
d�B
e`B
ezB
ezB
e�B
fB
f2B
fLB
f�B
f�B
f�B
f�B
f�B
f�B
gRB
gB
gRB
gmB
g�B
g�B
h
B
h$B
hXB
h�B
h�B
h�B
i*B
iyB
i�B
jKB
jeB
j�B
j�B
kB
kB
kkB
lB
lqB
lWB
lB
lWB
l=B
lqB
l�B
l�B
m)B
mCB
mCB
m�B
m�B
m�B
m�B
nB
n/B
n/B
nIB
n/B
nIB
n}B
n}B
n�B
n�B
n�B
n�B
oiB
o�B
o�B
o�B
o�B
pB
o�B
p;B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
q'B
qAB
qvB
q�B
q�B
r|B
r�B
r�B
r�B
r�B
sMB
s3B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
wB
wLB
wfB
w�B
w�B
xB
x8B
x�B
x�B
x�B
y	B
y$B
yXB
yrB
yrB
y�B
y�B
zxB
z�B
z�B
{JB
{JB
{dB
{�B
{�B
{�B
{�B
{�B
|B
{�B
|B
|�B
|�B
}B
}VB
}qB
}�B
}�B
}�B
~(B
~]B
~wB
~wB
~�B
~�B
~�B
HB
�B
�B
�4B
�4B
�OB
�4B
�OB
�OB
�B
�B
��B
�;B
�;B
�oB
�oB
��B
��B
��B
��B
�[B
�[B
�AB
�[B
�uB
��B
�{B
�{B
��B
�B
�MB
�B
�3B
�3B
�MB
�gB
�gB
��B
��B
�B
�B
��B
��B
��B
��B
��B
�?B
��B
��B
�+B
�zB
��B
��B
��B
�KB
�KB
�fB
��B
��B
��B
��B
��B
��B
�7B
�RB
��B
��B
��B
��B
�XB
��B
�)B
�DB
�^B
�xB
�B
�0B
�B
��B
��B
�B
��B
�B
�B
�PB
��B
��B
��B
��B
�B
�"B
�<B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230208005218  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230208005229  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230208005230  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230208005230                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230208005230  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230208005230  QCF$                G�O�G�O�G�O�            4000JA  ARUP                                                                        20230208010706                      G�O�G�O�G�O�                