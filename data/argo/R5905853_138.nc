CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-11-19T15:46:50Z creation;2022-11-19T15:46:55Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20221119154650  20221119160016  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��M-��1   @��M��@/!G�z��cPbM��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BI33BO33BW33B^ffBh  Bp  Bx  B�  B���B���B�  B�  B�  B�  B�  B�ffB�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  CL�C��C�fC"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBH�BN�BV�B^�Bg�RBo�RBw�RB�RB���B���B��)B��)B��)B��)B��)B�B�B��)B�B�B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\B��B��)B��)B��)B��)B��)B��)B��)C�zC�C�C�C	�C�C�C�C�C�C�C�C�C:�C��C�zC!�C#�C%�zC'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cd�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D&�D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DV�DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�:�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�ȴA�ȴA��A�уA��TA��,A���A�֡A��A��A��QA��A���A��A��jA���A�JA�/OA֌JA�ԕA��sA���A�ӏA�[WAЏ�Aω�A̭wA���A��YA�iA�@OA�_A���A�'�A���A��7A��qA�6�A��A���A�`vA�,�A���A�S�A��A��A��0A�hA��}A�(XA��?A�xA��PA���A�_�A��9A��A���A�2aA���A���A�ٴA�($A�W�A���A���A�9$A�z�A��A~�]A}jAy�AtߤArt�AoRTAk��AjU2Af�A\�AV�dAU� AT�?AN�$AK��AH�AF�tAD$tAA�A>OA<��A:GEA7�A5xlA2�A/=A,��A,u�A+��A+��A)V�A(P�A'��A%�A$�A"�XA �-AیA4A��A �A]�A�rA��AO�A�;A�A�PA�A�@A��AFtA��AT�AVmA�rAVmA�yA!�YA!�9A"JA"!�A"MA!��A �gA�A��Aw�AffA_A�Av`A��A*0AdZA;dA$�A��AqvAخA�jA��A�A�AS�AX�AJAߤA�AMjA!-A�A�A�VA�A��A�A�hAkQA�A�6AE9A��AE9A�HA�-An/ADgA�jAK�A�A��AI�AM�A��A<6AGEA
�A��A\�A
�.A
sA	��A��A �A�AA�>A~A�Aw�A7A"�A�TAIRA�Ah�A�A�.A�A�	A��A;�A�fAVmA�zA4A �A ͟A �_A o A >B@��@���@���@�)�@��.@�6z@�C�@��@��@�!-@��@�l"@���@��@���@�F@��X@�@�*0@�)�@�hs@�$@�?�@��@��@��@띲@��@��@�1'@��@�6@�@�@�'@曦@��@��@�k�@�.I@�h�@��@��3@��@�M@�2a@��]@��@�$@�a�@�j�@���@�6@���@�e@ݬq@�;d@ܹ�@ۺ^@�;�@َ"@ـ4@� \@��@�.�@��6@�O@�G�@��@��@��&@�a|@ӊ�@ЋD@Ϗ�@�Ɇ@�,=@��@�qv@���@̊r@�.�@��)@ˑh@�$t@ʟ�@Ɍ~@��@ȇ+@��@�m]@��@ƣ@�e@���@œ@���@�C�@�H�@÷�@Ø�@�J�@���@¡b@�@��@�F@�֡@��	@��@�~(@�Q@��@���@�L�@��@���@�(�@���@��@�\)@�@�w�@�	�@��n@�S@��@��@���@��4@���@�l�@���@�H�@���@��R@��D@�u%@�ff@��m@�k�@���@�}V@�[�@�b@��f@�,�@��@��'@�xl@��d@��C@�;d@�Ĝ@�n�@�B[@��@���@�U�@�$t@�@���@�"h@��@@��s@��T@���@�7L@��@�9X@��@��@��a@�{J@�+@�A�@��@��@��@�Mj@�"�@�%@�w�@��[@�\�@�@@���@�M@�:�@���@��-@���@�X�@�=@�+@���@�h
@�O@���@��@���@���@�X�@�ߤ@��'@��r@�YK@��A@�}�@�f�@�]�@�Mj@���@���@��@��u@�R�@��.@�<6@�Ɇ@���@�_@�8�@��@��}@�c�@���@�S�@�5?@�1'@��@���@��S@�`B@�;d@���@��@�}V@�A�@�4@���@��@��h@�kQ@��]@���@�|�@�Mj@�#�@��@�ߤ@���@�bN@��@��>@���@�]�@�#�@���@�Z�@�C�@�-�@���@�O@��K@��@�h
@�7�@�4@�خ@��@@�T�@�8@� \@�ی@��F@�e�@� �@���@��K@�c@�6z@�
=@��@���@���@�q�@�?�@��@��F@��4@��@��/@��L@�R�@���@��@��=@��$@���@��:@�c@�O@��@��}@���@�i�@�@�@�<�@�$�@�u@���@�rG@��@��@���@��j@��u@�1'@��@��t@�w2@�8�@��@��@��I@�M@�3�@�	�@���@���@�b�@��@� i@�ی@���@�S�@�	@�ݘ@�iD@�K�@�=�@�q@��E@�kQ@�?@�'R@�@Y@~��@~YK@}�@}o @}Q�@}8�@|��@|N�@|	�@{ƨ@{y�@{O@{9�@{,�@{&@z�L@y�@xѷ@x9X@wݘ@w�}@w�w@wy�@wE9@w i@v� @u�)@uDg@t��@s�r@s|�@s(@r��@r� @ri�@r{@q�@p��@pM@o˒@o��@o�$@oe�@n�r@m��@mB�@l��@ly>@l@k{J@k(@i�#@iX@i7L@hی@g�@g�f@gdZ@gC@f��@f+k@e�3@ehs@d��@d%�@c�@c>�@b��@b&�@a�@a�@`�$@_�@_W?@_�@^�c@^�B@^q�@^?@^J@]O�@\�@\h�@[�@Z��@Zc @Y��@Y5�@X��@XS�@W��@Wo�@V�M@V��@Vl�@V=q@U=�@T��@T]d@T,=@S�@S�f@S�@R�}@R?@Q2a@P�@PQ�@O��@O.I@N��@N�H@N}V@M�-@M4@Lѷ@L��@L7@K�;@K�	@KdZ@Kb�@K@J��@J�@Jz@JJ�@J�@Im]@I@@H��@H�e@HH@G��@Ga@G@F�s@F�F@Fn�@F^5@E�@E�S@E2a@D�)@D`�@C�@@Cs@C&@B��@Bu@A��@A�@@q@@S�@@:�@@ �@@  @?��@?1�@>��@>҉@>�@>4@=j@=0�@=#�@=	l@<Ɇ@<K^@;�V@;,�@;�@:��@:	@9�@9G�@8ѷ@8��@8u�@8�@7��@7�@6�@6kQ@6_�@6Z�@66�@6 �@5@5�@5�@4�z@4�4@4y>@4c�@41'@4@3��@3;d@3S@2kQ@2;�@2&�@1��@1S&@0�U@0�@0l"@0H@0 �@/�;@/�$@/(@.p;@.B[@..�@.{@-��@-�"@,��@,[�@+�+@+��@+.I@*��@*��@*�@*YK@*5?@*!�@*�@)�@)�@)��@)L�@)	l@(��@(_@(%�@(�@'��@'�f@'C@&�F@&i�@&YK@&B[@&4@%�@%��@%��@%zx@%&�@$��@$�e@$��@$�@$e�@#�]@#��@#��@#RT@#(@"�'@"��@"B[@"	@!ϫ@!hs@!-w@!%@ �p@ w�@ Ft@ 6@ (�@ M@�@ƨ@|�@33@�@��@�@�6@n�@�@�j@��@c�@X@IR@��@u�@(�@��@�@v`@a@F�@)_@�@ں@�'@d�@��@�9@�d@�H@�t@��@J�@�@�f@�@Q�@$@�@�6@��@��@W?@�@�}@� @GE@#:@��@�t@��@�@u�@V@�@�v@�.@|�@oi@:�@7@�@  @��@��@�a@�P@6z@��@��@��@V@3�@�z@�@�@�@��@��@I�@x@  @��@�a@��@_p@@�2@҉@�+@W�@+k@��@��@zx@<6@	l@�`@��@u�@Q�@M@��@˒@��@��@~�@O@�@
��@
}V@
)�@
�@
_@	�@	�3@	��@	��@	a�@	0�@	%@�@�@�e@�@~(@PH@~@�+@�&@�@o�@Mj@$t@�2@��@;�@)�@�>@�n@�S@�~@j@Vm@	l@��@�U@��@�@�Y@U2@C-@'R@��@ݘ@�[@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�ȴA�ȴA��A�уA��TA��,A���A�֡A��A��A��QA��A���A��A��jA���A�JA�/OA֌JA�ԕA��sA���A�ӏA�[WAЏ�Aω�A̭wA���A��YA�iA�@OA�_A���A�'�A���A��7A��qA�6�A��A���A�`vA�,�A���A�S�A��A��A��0A�hA��}A�(XA��?A�xA��PA���A�_�A��9A��A���A�2aA���A���A�ٴA�($A�W�A���A���A�9$A�z�A��A~�]A}jAy�AtߤArt�AoRTAk��AjU2Af�A\�AV�dAU� AT�?AN�$AK��AH�AF�tAD$tAA�A>OA<��A:GEA7�A5xlA2�A/=A,��A,u�A+��A+��A)V�A(P�A'��A%�A$�A"�XA �-AیA4A��A �A]�A�rA��AO�A�;A�A�PA�A�@A��AFtA��AT�AVmA�rAVmA�yA!�YA!�9A"JA"!�A"MA!��A �gA�A��Aw�AffA_A�Av`A��A*0AdZA;dA$�A��AqvAخA�jA��A�A�AS�AX�AJAߤA�AMjA!-A�A�A�VA�A��A�A�hAkQA�A�6AE9A��AE9A�HA�-An/ADgA�jAK�A�A��AI�AM�A��A<6AGEA
�A��A\�A
�.A
sA	��A��A �A�AA�>A~A�Aw�A7A"�A�TAIRA�Ah�A�A�.A�A�	A��A;�A�fAVmA�zA4A �A ͟A �_A o A >B@��@���@���@�)�@��.@�6z@�C�@��@��@�!-@��@�l"@���@��@���@�F@��X@�@�*0@�)�@�hs@�$@�?�@��@��@��@띲@��@��@�1'@��@�6@�@�@�'@曦@��@��@�k�@�.I@�h�@��@��3@��@�M@�2a@��]@��@�$@�a�@�j�@���@�6@���@�e@ݬq@�;d@ܹ�@ۺ^@�;�@َ"@ـ4@� \@��@�.�@��6@�O@�G�@��@��@��&@�a|@ӊ�@ЋD@Ϗ�@�Ɇ@�,=@��@�qv@���@̊r@�.�@��)@ˑh@�$t@ʟ�@Ɍ~@��@ȇ+@��@�m]@��@ƣ@�e@���@œ@���@�C�@�H�@÷�@Ø�@�J�@���@¡b@�@��@�F@�֡@��	@��@�~(@�Q@��@���@�L�@��@���@�(�@���@��@�\)@�@�w�@�	�@��n@�S@��@��@���@��4@���@�l�@���@�H�@���@��R@��D@�u%@�ff@��m@�k�@���@�}V@�[�@�b@��f@�,�@��@��'@�xl@��d@��C@�;d@�Ĝ@�n�@�B[@��@���@�U�@�$t@�@���@�"h@��@@��s@��T@���@�7L@��@�9X@��@��@��a@�{J@�+@�A�@��@��@��@�Mj@�"�@�%@�w�@��[@�\�@�@@���@�M@�:�@���@��-@���@�X�@�=@�+@���@�h
@�O@���@��@���@���@�X�@�ߤ@��'@��r@�YK@��A@�}�@�f�@�]�@�Mj@���@���@��@��u@�R�@��.@�<6@�Ɇ@���@�_@�8�@��@��}@�c�@���@�S�@�5?@�1'@��@���@��S@�`B@�;d@���@��@�}V@�A�@�4@���@��@��h@�kQ@��]@���@�|�@�Mj@�#�@��@�ߤ@���@�bN@��@��>@���@�]�@�#�@���@�Z�@�C�@�-�@���@�O@��K@��@�h
@�7�@�4@�خ@��@@�T�@�8@� \@�ی@��F@�e�@� �@���@��K@�c@�6z@�
=@��@���@���@�q�@�?�@��@��F@��4@��@��/@��L@�R�@���@��@��=@��$@���@��:@�c@�O@��@��}@���@�i�@�@�@�<�@�$�@�u@���@�rG@��@��@���@��j@��u@�1'@��@��t@�w2@�8�@��@��@��I@�M@�3�@�	�@���@���@�b�@��@� i@�ی@���@�S�@�	@�ݘ@�iD@�K�@�=�@�q@��E@�kQ@�?@�'R@�@Y@~��@~YK@}�@}o @}Q�@}8�@|��@|N�@|	�@{ƨ@{y�@{O@{9�@{,�@{&@z�L@y�@xѷ@x9X@wݘ@w�}@w�w@wy�@wE9@w i@v� @u�)@uDg@t��@s�r@s|�@s(@r��@r� @ri�@r{@q�@p��@pM@o˒@o��@o�$@oe�@n�r@m��@mB�@l��@ly>@l@k{J@k(@i�#@iX@i7L@hی@g�@g�f@gdZ@gC@f��@f+k@e�3@ehs@d��@d%�@c�@c>�@b��@b&�@a�@a�@`�$@_�@_W?@_�@^�c@^�B@^q�@^?@^J@]O�@\�@\h�@[�@Z��@Zc @Y��@Y5�@X��@XS�@W��@Wo�@V�M@V��@Vl�@V=q@U=�@T��@T]d@T,=@S�@S�f@S�@R�}@R?@Q2a@P�@PQ�@O��@O.I@N��@N�H@N}V@M�-@M4@Lѷ@L��@L7@K�;@K�	@KdZ@Kb�@K@J��@J�@Jz@JJ�@J�@Im]@I@@H��@H�e@HH@G��@Ga@G@F�s@F�F@Fn�@F^5@E�@E�S@E2a@D�)@D`�@C�@@Cs@C&@B��@Bu@A��@A�@@q@@S�@@:�@@ �@@  @?��@?1�@>��@>҉@>�@>4@=j@=0�@=#�@=	l@<Ɇ@<K^@;�V@;,�@;�@:��@:	@9�@9G�@8ѷ@8��@8u�@8�@7��@7�@6�@6kQ@6_�@6Z�@66�@6 �@5@5�@5�@4�z@4�4@4y>@4c�@41'@4@3��@3;d@3S@2kQ@2;�@2&�@1��@1S&@0�U@0�@0l"@0H@0 �@/�;@/�$@/(@.p;@.B[@..�@.{@-��@-�"@,��@,[�@+�+@+��@+.I@*��@*��@*�@*YK@*5?@*!�@*�@)�@)�@)��@)L�@)	l@(��@(_@(%�@(�@'��@'�f@'C@&�F@&i�@&YK@&B[@&4@%�@%��@%��@%zx@%&�@$��@$�e@$��@$�@$e�@#�]@#��@#��@#RT@#(@"�'@"��@"B[@"	@!ϫ@!hs@!-w@!%@ �p@ w�@ Ft@ 6@ (�@ M@�@ƨ@|�@33@�@��@�@�6@n�@�@�j@��@c�@X@IR@��@u�@(�@��@�@v`@a@F�@)_@�@ں@�'@d�@��@�9@�d@�H@�t@��@J�@�@�f@�@Q�@$@�@�6@��@��@W?@�@�}@� @GE@#:@��@�t@��@�@u�@V@�@�v@�.@|�@oi@:�@7@�@  @��@��@�a@�P@6z@��@��@��@V@3�@�z@�@�@�@��@��@I�@x@  @��@�a@��@_p@@�2@҉@�+@W�@+k@��@��@zx@<6@	l@�`@��@u�@Q�@M@��@˒@��@��@~�@O@�@
��@
}V@
)�@
�@
_@	�@	�3@	��@	��@	a�@	0�@	%@�@�@�e@�@~(@PH@~@�+@�&@�@o�@Mj@$t@�2@��@;�@)�@�>@�n@�S@�~@j@Vm@	l@��@�U@��@�@�Y@U2@C-@'R@��@ݘ@�[@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B�B��B�B�B�B��B�B�B�_B�yB�B�B��B�B��B��B�B�B^OB��B��B��B��B��B	K)B	W�B	D�B	̈́B	��B	� B	�B
.B
�B
YB
!�B
1�B
4�B
?cB
\]B
tnB
��B
ȀB
��B
�B
��B
��B
�)BB�B�NB�[B�YB{BnB<�B3B
�jB
��B
�!B
zDB
JrB
CB
iB
e�B
O�B
0UB
QB
uB
{B	��B	�B	��B	�[B	��B	��B	r�B	b�B	OBB	�B	3B�B�zB�
B�bBںB�oB��B��B��B��B��B��B�>B�,B��Bz*Bw�Bw�B}VB�[B|�By>Bw2Bu%BsBz�B~�B�(B�B��B�zB~�B~�B��B�B�]B��B͹B�=B�nB�qB�UB��B��B	
	B	3B	r�B	��B	�oB	�9B	��B	��B	��B	�B	��B	�JB	�B	�jB	�B	��B	�.B	ѝB	ӏB	خB	�1B	�eB	ڠB	�kB	ܒB	ڠB	�B	��B	�B	�NB	�;B	��B	ݲB	�;B	��B	�B	�mB	�sB	�_B	�qB	��B	��B	��B	�!B	�B	�B	�B	�wB	�B	�B	�8B	�yB	��B	� B	��B	��B	��B	�>B	��B	�hB	��B	�B	��B	��B	�mB	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	�MB	�B	��B	�`B	�zB	�B	�+B	��B	��B	��B	�nB	�+B	�zB	��B	��B	��B	�hB	�"B	��B	�:B	�'B	�OB	�/B	ܬB	ܒB	�)B	�)B	�dB	��B	ܬB	�B	��B	�xB	ܒB	ܒB	یB	�qB	�WB	ܒB	�~B	�xB	�B	�xB	�CB	ܬB	��B	�]B	�dB	޸B	ݘB	ݲB	�B	ޞB	�pB	��B	��B	�B	��B	�4B	�B	�B	�4B	��B	�2B	�0B	�B	�B	�)B	��B	��B	��B	�B	� B	�B	�B	�B	�OB	��B	�!B	�!B	�UB	�UB	�'B	��B	�OB	��B	��B	��B	�sB	��B	��B	�cB	�B	�B	�B	��B	��B	�3B	�MB	�B	�B	��B	�B	�MB	�GB	�MB	�9B	�nB	��B	��B	�+B	�ZB	�8B	��B	�B	�0B	��B	�B	��B	��B	�DB	�8B	�B	�GB	�ZB	��B	��B	�B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�(B	��B	�.B
 �B
AB
�B
B
AB
uB
�B	��B	��B	�wB
 �B
;B
MB
B
�B
?B
?B
?B
�B
�B
�B
B
�B
B
�B
KB
�B
	7B
	lB
	RB
�B
	RB
	�B
	�B

	B

#B
	�B
	B
	�B
KB
�B
�B
	RB
	RB
	B
	B
�B
	B
	B

�B

�B
JB
pB
�B
pB
pB
\B
�B
�B
"B
�B
�B
�B
6B
�B
6B
"B
�B
�B
�B
vB
�B
�B
�B
�B
vB
bB
vB
�B
�B
�B
�B
�B
VB
VB
<B
(B
\B
�B
.B
�B
�B
�B
 B
TB
�B
�B
�B
@B
FB
�B
9B
B
9B
SB
mB
�B

B
YB
�B
_B
yB
�B
�B
KB
7B
QB
�B
#B
�B
�B
B
]B
xB
�B
�B
IB
�B
�B
5B
B
�B
�B
VB
pB
;B
 \B
 �B
 �B
!-B
!�B
!�B
!�B
"B
"NB
"�B
"�B
"�B
"�B
#B
#nB
#�B
#�B
$B
$ZB
$�B
$�B
%B
$�B
%,B
%�B
%�B
&fB
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(sB
(sB
(XB
(sB
(sB
)*B
)*B
)*B
)yB
)�B
)yB
)�B
)�B
)�B
*0B
+B
+B
*�B
+6B
+QB
,B
,"B
,�B
-)B
-�B
-�B
-�B
.}B
/B
/B
/OB
/�B
/�B
0UB
0oB
1B
1[B
0�B
1�B
2B
2-B
2�B
2�B
3B
3hB
3�B
49B
49B
4B
4�B
4�B
5ZB
5?B
6+B
6�B
6�B
6�B
7B
7LB
72B
7LB
7LB
72B
7fB
72B
7B
7�B
8�B
9	B
9>B
9$B
9>B
9>B
9�B
9XB
9rB
9�B
:DB
:�B
:�B
;�B
;�B
;�B
<B
<B
<B
;�B
=VB
=<B
=�B
>(B
>B
>BB
>B
>�B
>�B
?�B
?}B
?�B
?�B
@4B
@B
A;B
A;B
A B
A�B
B'B
BuB
B�B
B�B
CGB
C�B
C�B
C�B
C�B
DMB
D�B
D�B
ESB
E�B
E�B
FYB
FtB
F�B
G+B
G_B
G�B
GzB
G�B
H1B
HKB
H�B
IB
IlB
I�B
JrB
J�B
KB
K�B
K�B
L0B
L~B
L�B
MPB
M�B
MjB
MjB
N�B
N�B
N�B
OB
OB
OvB
O�B
O�B
O�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
PB
P�B
P�B
Q4B
QNB
Q�B
R B
RoB
R�B
R�B
S@B
S[B
SuB
S�B
S[B
SuB
TB
TB
T,B
TFB
T{B
UMB
UMB
U�B
U�B
VB
U�B
VB
V�B
W$B
W�B
X_B
YB
Y1B
YB
YeB
ZB
ZQB
Z�B
[#B
[qB
[qB
[�B
[�B
\�B
]B
]dB
]�B
]�B
]�B
^5B
^�B
^�B
^�B
^�B
_B
_!B
_�B
`'B
`'B
`�B
`�B
a-B
a�B
a�B
a�B
bB
b�B
b�B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
ezB
ezB
e�B
f2B
fB
e�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
gB
g�B
h
B
h$B
h>B
hXB
h>B
h�B
i�B
i�B
i�B
j0B
j�B
j�B
j�B
kB
k6B
kkB
kkB
k�B
k�B
k�B
k�B
lB
k�B
lB
l�B
l�B
l�B
mB
mB
m�B
n/B
nIB
nIB
nIB
n�B
n�B
n�B
n�B
o B
o5B
o�B
o�B
o�B
o�B
o�B
p;B
p;B
p�B
p�B
p�B
p�B
qB
q[B
q�B
q�B
rB
r-B
raB
raB
r�B
r�B
sB
r�B
s3B
sB
sMB
s�B
s�B
s�B
tTB
t9B
tB
tnB
t�B
t�B
u?B
u?B
u%B
u?B
utB
u�B
vFB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
wLB
w�B
xB
x8B
xB
xB
xRB
xRB
x�B
x�B
x�B
y	B
yrB
yrB
y�B
y�B
y�B
y�B
zB
zxB
z�B
z�B
{B
{JB
{�B
{�B
{�B
{�B
|6B
|�B
|6B
|�B
|�B
}B
|�B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
~B
~BB
~wB
B
.B
cB
HB
�B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�'B
�uB
��B
��B
��B
�B
�GB
�aB
��B
��B
�B
�MB
�gB
��B
��B
�B
�9B
�SB
�mB
�mB
�SB
��B
��B
��B
�?B
��B
��B
�B
�B
�EB
�EB
�_B
�_B
��B
��B
��B
�1B
�KB
�1B
�B
��B
��B
��B
�B
�B
�7B
��B
��B
��B
�	B
�rB
��B
��B
�B
�DB
�^B
�^B
�xB
�xB
��B
�B
�0B
�0B
�JB
�~B
�~B
��B
��B
�B
�B
�PB
�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B�B��B�B�B�B��B�B�B�_B�yB�B�B��B�B��B��B�B�B^OB��B��B��B��B��B	K)B	W�B	D�B	̈́B	��B	� B	�B
.B
�B
YB
!�B
1�B
4�B
?cB
\]B
tnB
��B
ȀB
��B
�B
��B
��B
�)BB�B�NB�[B�YB{BnB<�B3B
�jB
��B
�!B
zDB
JrB
CB
iB
e�B
O�B
0UB
QB
uB
{B	��B	�B	��B	�[B	��B	��B	r�B	b�B	OBB	�B	3B�B�zB�
B�bBںB�oB��B��B��B��B��B��B�>B�,B��Bz*Bw�Bw�B}VB�[B|�By>Bw2Bu%BsBz�B~�B�(B�B��B�zB~�B~�B��B�B�]B��B͹B�=B�nB�qB�UB��B��B	
	B	3B	r�B	��B	�oB	�9B	��B	��B	��B	�B	��B	�JB	�B	�jB	�B	��B	�.B	ѝB	ӏB	خB	�1B	�eB	ڠB	�kB	ܒB	ڠB	�B	��B	�B	�NB	�;B	��B	ݲB	�;B	��B	�B	�mB	�sB	�_B	�qB	��B	��B	��B	�!B	�B	�B	�B	�wB	�B	�B	�8B	�yB	��B	� B	��B	��B	��B	�>B	��B	�hB	��B	�B	��B	��B	�mB	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	�MB	�B	��B	�`B	�zB	�B	�+B	��B	��B	��B	�nB	�+B	�zB	��B	��B	��B	�hB	�"B	��B	�:B	�'B	�OB	�/B	ܬB	ܒB	�)B	�)B	�dB	��B	ܬB	�B	��B	�xB	ܒB	ܒB	یB	�qB	�WB	ܒB	�~B	�xB	�B	�xB	�CB	ܬB	��B	�]B	�dB	޸B	ݘB	ݲB	�B	ޞB	�pB	��B	��B	�B	��B	�4B	�B	�B	�4B	��B	�2B	�0B	�B	�B	�)B	��B	��B	��B	�B	� B	�B	�B	�B	�OB	��B	�!B	�!B	�UB	�UB	�'B	��B	�OB	��B	��B	��B	�sB	��B	��B	�cB	�B	�B	�B	��B	��B	�3B	�MB	�B	�B	��B	�B	�MB	�GB	�MB	�9B	�nB	��B	��B	�+B	�ZB	�8B	��B	�B	�0B	��B	�B	��B	��B	�DB	�8B	�B	�GB	�ZB	��B	��B	�B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�(B	��B	�.B
 �B
AB
�B
B
AB
uB
�B	��B	��B	�wB
 �B
;B
MB
B
�B
?B
?B
?B
�B
�B
�B
B
�B
B
�B
KB
�B
	7B
	lB
	RB
�B
	RB
	�B
	�B

	B

#B
	�B
	B
	�B
KB
�B
�B
	RB
	RB
	B
	B
�B
	B
	B

�B

�B
JB
pB
�B
pB
pB
\B
�B
�B
"B
�B
�B
�B
6B
�B
6B
"B
�B
�B
�B
vB
�B
�B
�B
�B
vB
bB
vB
�B
�B
�B
�B
�B
VB
VB
<B
(B
\B
�B
.B
�B
�B
�B
 B
TB
�B
�B
�B
@B
FB
�B
9B
B
9B
SB
mB
�B

B
YB
�B
_B
yB
�B
�B
KB
7B
QB
�B
#B
�B
�B
B
]B
xB
�B
�B
IB
�B
�B
5B
B
�B
�B
VB
pB
;B
 \B
 �B
 �B
!-B
!�B
!�B
!�B
"B
"NB
"�B
"�B
"�B
"�B
#B
#nB
#�B
#�B
$B
$ZB
$�B
$�B
%B
$�B
%,B
%�B
%�B
&fB
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(sB
(sB
(XB
(sB
(sB
)*B
)*B
)*B
)yB
)�B
)yB
)�B
)�B
)�B
*0B
+B
+B
*�B
+6B
+QB
,B
,"B
,�B
-)B
-�B
-�B
-�B
.}B
/B
/B
/OB
/�B
/�B
0UB
0oB
1B
1[B
0�B
1�B
2B
2-B
2�B
2�B
3B
3hB
3�B
49B
49B
4B
4�B
4�B
5ZB
5?B
6+B
6�B
6�B
6�B
7B
7LB
72B
7LB
7LB
72B
7fB
72B
7B
7�B
8�B
9	B
9>B
9$B
9>B
9>B
9�B
9XB
9rB
9�B
:DB
:�B
:�B
;�B
;�B
;�B
<B
<B
<B
;�B
=VB
=<B
=�B
>(B
>B
>BB
>B
>�B
>�B
?�B
?}B
?�B
?�B
@4B
@B
A;B
A;B
A B
A�B
B'B
BuB
B�B
B�B
CGB
C�B
C�B
C�B
C�B
DMB
D�B
D�B
ESB
E�B
E�B
FYB
FtB
F�B
G+B
G_B
G�B
GzB
G�B
H1B
HKB
H�B
IB
IlB
I�B
JrB
J�B
KB
K�B
K�B
L0B
L~B
L�B
MPB
M�B
MjB
MjB
N�B
N�B
N�B
OB
OB
OvB
O�B
O�B
O�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
PB
P�B
P�B
Q4B
QNB
Q�B
R B
RoB
R�B
R�B
S@B
S[B
SuB
S�B
S[B
SuB
TB
TB
T,B
TFB
T{B
UMB
UMB
U�B
U�B
VB
U�B
VB
V�B
W$B
W�B
X_B
YB
Y1B
YB
YeB
ZB
ZQB
Z�B
[#B
[qB
[qB
[�B
[�B
\�B
]B
]dB
]�B
]�B
]�B
^5B
^�B
^�B
^�B
^�B
_B
_!B
_�B
`'B
`'B
`�B
`�B
a-B
a�B
a�B
a�B
bB
b�B
b�B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
ezB
ezB
e�B
f2B
fB
e�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
gB
g�B
h
B
h$B
h>B
hXB
h>B
h�B
i�B
i�B
i�B
j0B
j�B
j�B
j�B
kB
k6B
kkB
kkB
k�B
k�B
k�B
k�B
lB
k�B
lB
l�B
l�B
l�B
mB
mB
m�B
n/B
nIB
nIB
nIB
n�B
n�B
n�B
n�B
o B
o5B
o�B
o�B
o�B
o�B
o�B
p;B
p;B
p�B
p�B
p�B
p�B
qB
q[B
q�B
q�B
rB
r-B
raB
raB
r�B
r�B
sB
r�B
s3B
sB
sMB
s�B
s�B
s�B
tTB
t9B
tB
tnB
t�B
t�B
u?B
u?B
u%B
u?B
utB
u�B
vFB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
wLB
w�B
xB
x8B
xB
xB
xRB
xRB
x�B
x�B
x�B
y	B
yrB
yrB
y�B
y�B
y�B
y�B
zB
zxB
z�B
z�B
{B
{JB
{�B
{�B
{�B
{�B
|6B
|�B
|6B
|�B
|�B
}B
|�B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
~B
~BB
~wB
B
.B
cB
HB
�B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�'B
�uB
��B
��B
��B
�B
�GB
�aB
��B
��B
�B
�MB
�gB
��B
��B
�B
�9B
�SB
�mB
�mB
�SB
��B
��B
��B
�?B
��B
��B
�B
�B
�EB
�EB
�_B
�_B
��B
��B
��B
�1B
�KB
�1B
�B
��B
��B
��B
�B
�B
�7B
��B
��B
��B
�	B
�rB
��B
��B
�B
�DB
�^B
�^B
�xB
�xB
��B
�B
�0B
�0B
�JB
�~B
�~B
��B
��B
�B
�B
�PB
�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221119154521  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221119154650  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221119154654  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221119154655                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221119154657  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221119154657  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221119160016                      G�O�G�O�G�O�                