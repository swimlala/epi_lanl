CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-04-14T01:02:27Z creation;2023-04-14T01:02:28Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230414010227  20230414011028  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�#v�5y�1   @�#wk��@/(r� Ĝ�cӕ�$�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�33A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�ffB�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�ffBB�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C33C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF33CH  CJ  CK�fCN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz33C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DGfDG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@���@�A�HA>�HA]G�A~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBh�Bp�Bw�RB�RB��)B��)B��)B��)B��)B��)B��)B�u�B��)B��)B��)B��)B��)B�B�B��)B�\B¨�BǨ�B��)B��)B��)B��)B��)B��)B��)B��)B�B�B�u�B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C!GC�zC�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CF!GCG�CI�CK�zCM�CO�zCQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cz!GC{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DC�DC{�DC��DD{�DD��DE{�DE��DF{�DG�DG��DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D}�D}��D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AΌ�A΋A΅�A΀�A�~�A�~�A�x8A�u�A�t�A�uZA�t�A�u�A�v+A�wfA�o�A�p�A�p�A�qA�q�A�qvA�rA�qAA�q�A�r�A�tA�r�A�s�A�tTA�u%A�t�A�v`A�w2A�s�A�s�A�x�A�v`A�iDA�b�A��A˚7A�x8A��AɾwAɋDAɅ�A�z�A�C-A��sA�DA�b�A�8A�B�AÑ4A��]A���A��vA��A��DA���A�{�A�wfA��A�Z�A��|A�רA�aHA���A�^jA���A���A��A�u%A��MA���A�(XA��QA�f�A��JA��?A���A�|A��4A�2�A��A��\A��A�B�A��A��A��KA���A�xA�4A�A��A��"A��-A�GEA���A��tA� �A{JA{�zAt�Ao��AjjAj�Ai�=AgoAd-�Aa��A^J�A\$tAXk�AUV�AP�AL%AHrGAE�$AD[WAC!�A@�A>8A<w�A;�A9�A7�}A4#�A1�EA.�KA+��A)��A'!�A%+A$�A#A"��A"�AYKA�AAQAn�A \A�AȴAHA�pA��A:*AT�A�1AxA�oA�rA�A=�A��Av`A8�A
�MA
5?A	;�A�Aw2A!A��A��A�gA�mAf�A�A ��A %F@�|�@�O�@�M�@�n�@�!@�J@�Dg@��4@�;@���@�\)@��@�PH@�M�@��P@�;�@�A@縻@��@�"h@�:�@��@�e�@���@�l�@��@�j@�U2@�
�@���@�a@�(�@�<�@��@�Xy@�J�@���@�ݘ@��@�r�@��@�Z�@�t�@���@�N�@� �@��@�S@�33@��M@�c�@�^�@�-@��d@�Q�@�?}@�q@��B@ܹ$@��@�J#@��@�L0@�,�@�*0@�,�@���@�e,@��@�bN@�Y@�_p@�]�@�v�@�W�@�6@�}�@�q@��D@�|@�6z@�q@��m@�  @�a@�v`@�?}@�M@Ӕ�@��v@�e�@��@�_p@���@��3@�%@�~�@�a|@�R�@�C-@�*�@�@�k�@̒�@́o@�'R@˜@� \@���@��]@ȡb@�D�@���@�p�@���@��@��@š�@��P@�n�@�$�@�b@ô�@ �@�<�@��@��X@�*0@��@��@�}V@��@��[@���@��@��D@�0U@���@��o@�0U@�0U@�%�@��+@�}�@���@��@�u�@��@��E@���@�U2@��@��@�f�@�8�@���@���@�qv@��@�_�@�	@���@�~�@�Mj@�@��)@���@�I�@�@���@��H@��@�Ɇ@���@�%�@��A@��@���@���@��@�hs@�F@��@��@�Z�@�{@��@���@�4�@���@��'@���@�@�@�b@��V@��[@��@�,=@��M@�L�@��@���@�_�@�0U@���@��@��@��@�?}@��@���@���@�$@�ԕ@�RT@��2@���@��.@�l�@�H@��=@��@��@�0U@�-�@�&�@�@�U�@�%F@���@���@�L0@���@�4@��5@��$@��u@�[�@�:�@�@���@��@��n@���@�k�@�o@��|@��U@�h
@�u%@���@�u�@�8�@���@�J�@�O@�J#@���@�҉@���@���@�m�@�{@��h@�<6@��@��X@�xl@�0U@���@�~�@�^�@�A�@�(@��@�U2@�4@��}@�c�@�1�@��@��@�kQ@�A�@���@�IR@�Y@�\�@��@�Ft@�h�@�'R@��@��6@�c@�+@��@��o@���@�ی@��@��@��@��_@�?@�rG@�RT@�33@���@��z@��@�R�@�|@�ߤ@���@���@���@��@�~(@��@��@�Y@��U@�~(@�V@�H�@���@�rG@��@���@��D@���@���@�_@��I@���@�i�@�W�@�6�@�&�@��@���@�w2@�]�@�+@��@���@��v@�w�@��@�|�@��'@�j�@�A @��@���@��?@���@�*�@���@�Dg@�IR@�B�@�9�@�8�@�C�@�,�@���@���@�c @��@�ϫ@�s@�V@���@�A @�q@��s@���@��1@�,=@�K@{J@~�c@~z@~=q@}�7@}�@}	l@|M@{��@z�@zn�@y�.@yw2@y+�@y�@x��@xg8@w�+@wg�@vB[@u�@u��@t�	@tC-@s��@s�	@r�y@r��@r=q@q��@q[W@q[W@q�@p��@pH@o��@oF�@n҉@n��@n_@m�3@m?}@l��@lh�@lQ�@l!@k��@k�k@kC�@j�M@ja|@i��@i�@hXy@g��@giD@gS@f^5@e�@ew2@e�@d�4@c��@c�f@c/�@bn�@a�M@a5�@`��@`~(@_�@_�
@_��@_8@^��@^�@]�@]e,@]S&@\�@\H@[��@[A�@Z�@Zu%@ZR�@Z#:@Y�@Y5�@X�@X��@Xr�@X-�@W�W@W�@V�@V��@V1�@U@U`B@T��@T��@T]d@TM@R��@R�\@RB[@Q�#@Q2a@Q�@P��@PA�@O��@O��@O�*@Oo�@Nں@N�r@N-@M��@ML�@L�4@Lm�@L"h@K��@KdZ@J��@J�h@J��@J �@I��@I\�@I�@H�@H��@Hg8@G��@G(@F��@FB[@E�^@Ea�@EJ�@E�@D��@Dی@DĜ@Dw�@C�@C=@C(@B�@B�@B�c@B��@Bn�@A�o@A��@Ac@@֡@@�_@@��@@�.@@�u@@6@?خ@?&@>�2@>��@>v�@>@�@>4@=�o@=�z@=j@=2a@<�|@<��@<U2@;�A@;��@;33@:�X@:�+@:E�@9�@9��@9Vm@8��@8��@8�@8!@7��@7��@7��@7��@7��@7j�@7�@6�@6��@6#:@5��@5��@5m]@5G�@55�@5�@4�9@4_@3��@3��@3�0@3RT@2�2@2��@2R�@1��@1�@1��@15�@1q@1;@0ѷ@0�.@0Q�@0,=@/��@/��@/��@/b�@.҉@.O@-�T@-�^@-zx@-=�@-+�@,��@,��@,��@,N�@+��@+�@+>�@*�'@*�}@*��@*3�@)�@)w2@)4@)-w@)�@(�p@(�@(�o@(u�@(�@'�@@'F�@&�y@&�<@&�x@&p;@&kQ@&d�@&R�@&�@%��@%��@%��@%X@%	l@$�?@$��@$]d@$M@#�A@#��@"�M@"�X@"�@"^5@!��@!�@!@!�X@!��@!k�@![W@!-w@ �P@ ��@ S�@ "h@� @��@��@~�@@O@C@�M@��@��@{�@c @=q@6�@+k@�@�^@<6@�@�@�@֡@Q�@��@�@��@��@n/@_p@6z@��@� @{�@_�@W�@:*@��@�S@�@x�@m]@^�@L�@?}@/@�@�@�e@�D@M@��@�@�f@_p@�@�c@��@a|@;�@{@_@�@G�@%@�v@�$@y>@Q�@@��@��@=@@�2@�x@v�@=q@
�@ϫ@��@rG@7L@�@��@��@q@e�@D�@!@�@�@��@�@iD@=@�"@�@҉@��@&�@�D@�z@�@s�@=�@��@�v@��@�e@��@oi@Z@:�@��@��@��@�P@E9@
�,@
��@
�x@
ff@
0U@	�.@	�3@	��@	Y�@	+�@	@	;@�v@Ɇ@�p@��@��@�.@S�@4n@%�@�@��@|�@g�@]�@U�@C�@/�@Y@��@�H@�'@�L@u%@-@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AΌ�A΋A΅�A΀�A�~�A�~�A�x8A�u�A�t�A�uZA�t�A�u�A�v+A�wfA�o�A�p�A�p�A�qA�q�A�qvA�rA�qAA�q�A�r�A�tA�r�A�s�A�tTA�u%A�t�A�v`A�w2A�s�A�s�A�x�A�v`A�iDA�b�A��A˚7A�x8A��AɾwAɋDAɅ�A�z�A�C-A��sA�DA�b�A�8A�B�AÑ4A��]A���A��vA��A��DA���A�{�A�wfA��A�Z�A��|A�רA�aHA���A�^jA���A���A��A�u%A��MA���A�(XA��QA�f�A��JA��?A���A�|A��4A�2�A��A��\A��A�B�A��A��A��KA���A�xA�4A�A��A��"A��-A�GEA���A��tA� �A{JA{�zAt�Ao��AjjAj�Ai�=AgoAd-�Aa��A^J�A\$tAXk�AUV�AP�AL%AHrGAE�$AD[WAC!�A@�A>8A<w�A;�A9�A7�}A4#�A1�EA.�KA+��A)��A'!�A%+A$�A#A"��A"�AYKA�AAQAn�A \A�AȴAHA�pA��A:*AT�A�1AxA�oA�rA�A=�A��Av`A8�A
�MA
5?A	;�A�Aw2A!A��A��A�gA�mAf�A�A ��A %F@�|�@�O�@�M�@�n�@�!@�J@�Dg@��4@�;@���@�\)@��@�PH@�M�@��P@�;�@�A@縻@��@�"h@�:�@��@�e�@���@�l�@��@�j@�U2@�
�@���@�a@�(�@�<�@��@�Xy@�J�@���@�ݘ@��@�r�@��@�Z�@�t�@���@�N�@� �@��@�S@�33@��M@�c�@�^�@�-@��d@�Q�@�?}@�q@��B@ܹ$@��@�J#@��@�L0@�,�@�*0@�,�@���@�e,@��@�bN@�Y@�_p@�]�@�v�@�W�@�6@�}�@�q@��D@�|@�6z@�q@��m@�  @�a@�v`@�?}@�M@Ӕ�@��v@�e�@��@�_p@���@��3@�%@�~�@�a|@�R�@�C-@�*�@�@�k�@̒�@́o@�'R@˜@� \@���@��]@ȡb@�D�@���@�p�@���@��@��@š�@��P@�n�@�$�@�b@ô�@ �@�<�@��@��X@�*0@��@��@�}V@��@��[@���@��@��D@�0U@���@��o@�0U@�0U@�%�@��+@�}�@���@��@�u�@��@��E@���@�U2@��@��@�f�@�8�@���@���@�qv@��@�_�@�	@���@�~�@�Mj@�@��)@���@�I�@�@���@��H@��@�Ɇ@���@�%�@��A@��@���@���@��@�hs@�F@��@��@�Z�@�{@��@���@�4�@���@��'@���@�@�@�b@��V@��[@��@�,=@��M@�L�@��@���@�_�@�0U@���@��@��@��@�?}@��@���@���@�$@�ԕ@�RT@��2@���@��.@�l�@�H@��=@��@��@�0U@�-�@�&�@�@�U�@�%F@���@���@�L0@���@�4@��5@��$@��u@�[�@�:�@�@���@��@��n@���@�k�@�o@��|@��U@�h
@�u%@���@�u�@�8�@���@�J�@�O@�J#@���@�҉@���@���@�m�@�{@��h@�<6@��@��X@�xl@�0U@���@�~�@�^�@�A�@�(@��@�U2@�4@��}@�c�@�1�@��@��@�kQ@�A�@���@�IR@�Y@�\�@��@�Ft@�h�@�'R@��@��6@�c@�+@��@��o@���@�ی@��@��@��@��_@�?@�rG@�RT@�33@���@��z@��@�R�@�|@�ߤ@���@���@���@��@�~(@��@��@�Y@��U@�~(@�V@�H�@���@�rG@��@���@��D@���@���@�_@��I@���@�i�@�W�@�6�@�&�@��@���@�w2@�]�@�+@��@���@��v@�w�@��@�|�@��'@�j�@�A @��@���@��?@���@�*�@���@�Dg@�IR@�B�@�9�@�8�@�C�@�,�@���@���@�c @��@�ϫ@�s@�V@���@�A @�q@��s@���@��1@�,=@�K@{J@~�c@~z@~=q@}�7@}�@}	l@|M@{��@z�@zn�@y�.@yw2@y+�@y�@x��@xg8@w�+@wg�@vB[@u�@u��@t�	@tC-@s��@s�	@r�y@r��@r=q@q��@q[W@q[W@q�@p��@pH@o��@oF�@n҉@n��@n_@m�3@m?}@l��@lh�@lQ�@l!@k��@k�k@kC�@j�M@ja|@i��@i�@hXy@g��@giD@gS@f^5@e�@ew2@e�@d�4@c��@c�f@c/�@bn�@a�M@a5�@`��@`~(@_�@_�
@_��@_8@^��@^�@]�@]e,@]S&@\�@\H@[��@[A�@Z�@Zu%@ZR�@Z#:@Y�@Y5�@X�@X��@Xr�@X-�@W�W@W�@V�@V��@V1�@U@U`B@T��@T��@T]d@TM@R��@R�\@RB[@Q�#@Q2a@Q�@P��@PA�@O��@O��@O�*@Oo�@Nں@N�r@N-@M��@ML�@L�4@Lm�@L"h@K��@KdZ@J��@J�h@J��@J �@I��@I\�@I�@H�@H��@Hg8@G��@G(@F��@FB[@E�^@Ea�@EJ�@E�@D��@Dی@DĜ@Dw�@C�@C=@C(@B�@B�@B�c@B��@Bn�@A�o@A��@Ac@@֡@@�_@@��@@�.@@�u@@6@?خ@?&@>�2@>��@>v�@>@�@>4@=�o@=�z@=j@=2a@<�|@<��@<U2@;�A@;��@;33@:�X@:�+@:E�@9�@9��@9Vm@8��@8��@8�@8!@7��@7��@7��@7��@7��@7j�@7�@6�@6��@6#:@5��@5��@5m]@5G�@55�@5�@4�9@4_@3��@3��@3�0@3RT@2�2@2��@2R�@1��@1�@1��@15�@1q@1;@0ѷ@0�.@0Q�@0,=@/��@/��@/��@/b�@.҉@.O@-�T@-�^@-zx@-=�@-+�@,��@,��@,��@,N�@+��@+�@+>�@*�'@*�}@*��@*3�@)�@)w2@)4@)-w@)�@(�p@(�@(�o@(u�@(�@'�@@'F�@&�y@&�<@&�x@&p;@&kQ@&d�@&R�@&�@%��@%��@%��@%X@%	l@$�?@$��@$]d@$M@#�A@#��@"�M@"�X@"�@"^5@!��@!�@!@!�X@!��@!k�@![W@!-w@ �P@ ��@ S�@ "h@� @��@��@~�@@O@C@�M@��@��@{�@c @=q@6�@+k@�@�^@<6@�@�@�@֡@Q�@��@�@��@��@n/@_p@6z@��@� @{�@_�@W�@:*@��@�S@�@x�@m]@^�@L�@?}@/@�@�@�e@�D@M@��@�@�f@_p@�@�c@��@a|@;�@{@_@�@G�@%@�v@�$@y>@Q�@@��@��@=@@�2@�x@v�@=q@
�@ϫ@��@rG@7L@�@��@��@q@e�@D�@!@�@�@��@�@iD@=@�"@�@҉@��@&�@�D@�z@�@s�@=�@��@�v@��@�e@��@oi@Z@:�@��@��@��@�P@E9@
�,@
��@
�x@
ff@
0U@	�.@	�3@	��@	Y�@	+�@	@	;@�v@Ɇ@�p@��@��@�.@S�@4n@%�@�@��@|�@g�@]�@U�@C�@/�@Y@��@�H@�'@�L@u%@-@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
pB
�B
�B
;B
;B
!B
!B
B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
!B
VB
VB
�B
�B
�B
�B
pB
 BB
 \B
�B
 'B
"NB
# B
.IB
�BB
��B
�
B
��BVBPHBc B�UB��B��B�eB��BBB�B�B��B�mB�}B��B�Br-Bl�Bp�Bx�B.B�\B��BH�Ba�Bf�Bi�Be�BSuBB�B1'B,�B+QB!�B�BB�B�yB�NB�[B��B� B��B�Bl"BP�B;0B �B{B
�B
�>B
��B
e`B
P.B
A;B
8RB
5�B
-]B
xB	�zB	ޞB	��B	��B	��B	�nB	��B	�B	l�B	_pB	M6B	=B	(XB	,B	
�B	;B�B�>B��B��B��B�@B�~BרB��B��B��B��B�wB�qB��B��B��B�mB�4B��B�4B}BByXBt�Bt�ButB{dB~�B�GB��B��B��B�(B��B��B��B�B��B��B�tB�gB�tB��B��B��B�tB��B��B�B�zB�yB�yB��B�kB�OB�9B��BªB�-B�B��B�B�uB�VB˒B�XBԯBϑB�7B��B�fBɠB�7B�dB�NB�\B	�B	OB	&�B	5%B	8RB	7�B	:�B	?}B	B�B	[WB	`vB	b�B	dB	ezB	h�B	h�B	i�B	h�B	g�B	h�B	i*B	h>B	i�B	oOB	s�B	s�B	s�B	tB	v`B	vFB	w�B	x�B	y$B	y�B	z*B	y�B	{�B	|B	|B	~BB	�'B	�aB	�zB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	��B	�XB	��B	��B	��B	�BB	ªB	�EB	�#B	�dB	��B	�B	�(B	ΥB	��B	�pB	��B	�.B	�4B	�NB	�NB	�NB	�NB	�4B	�:B	�uB	��B	ԯB	��B	ԕB	�gB	՛B	�gB	��B	յB	�B	��B	�B	�_B	�B	��B	��B	�	B	��B	�qB	�IB	ݲB	��B	�B	�B	�'B	�vB	��B	�,B	�zB	��B	�B	��B	��B	�B	�FB	��B	�sB	�B	�B	�*B	�KB	�B	�=B	��B	�B	�CB	�wB	�B	��B	��B	��B	�B	�OB	�5B	��B	�B	�B	��B	�B	��B	�B	��B	�ZB	��B	��B	��B	�+B	�LB	��B	��B	��B	�lB	��B	�rB	�*B	�^B	�xB	�^B	�*B	��B	��B	�DB	�^B	�JB	�<B	��B	��B	��B	�qB	�"B	�BB	�cB	��B
 iB
'B
'B
[B
�B
aB
�B
3B
gB
SB
YB
�B
�B
�B
�B
1B
fB
	�B
	�B
	�B
	�B
	�B
	�B

XB
	�B

XB

XB

rB

XB
�B
dB
�B
dB
�B
�B
�B
PB
jB
B
�B
�B
�B
vB
bB
 B
�B
�B
�B
�B
�B
uB
�B
{B
B
�B
B
�B
�B
�B
?B
�B
�B
�B
+B
B
_B
+B
_B
�B
B
1B
�B
KB
B
�B
�B
B
�B
	B
WB
qB
)B
�B
)B
/B
�B
B
�B
xB
IB
�B
OB
!B
$B
$�B
$�B
$ZB
$@B
$B
$@B
#�B
'�B
*�B
,B
,=B
-]B
,�B
+�B
*�B
,=B
,qB
,�B
,�B
,qB
,�B
,B
)�B
)�B
*B
*�B
*�B
+B
,WB
.�B
1�B
1AB
1vB
0�B
0!B
/OB
-�B
,=B
+kB
,qB
-wB
-�B
/B
3�B
4�B
5tB
6FB
6B
6zB
8B
8RB
8�B
8�B
9$B
9$B
9>B
9$B
8�B
7B
8B
9rB
9�B
;0B
;JB
;0B
;B
;B
:xB
:DB
;B
;�B
;�B
;�B
;�B
<6B
=<B
=�B
=�B
>BB
>wB
>B
=B
:�B
8�B
9XB
9rB
9�B
;B
<6B
<B
<B
<jB
<�B
<�B
<jB
<jB
<�B
>B
>�B
>]B
>�B
?B
?�B
?�B
?�B
?�B
?�B
@ B
@4B
@ B
A;B
@�B
AB
AoB
B'B
BAB
BuB
C-B
CGB
C�B
DB
D�B
F%B
F�B
FYB
F?B
F%B
E�B
E�B
E�B
F?B
F?B
F�B
G_B
G�B
G�B
G�B
H1B
HB
HfB
HfB
H�B
IRB
I�B
JXB
J�B
J�B
J�B
K�B
K�B
K�B
LB
L~B
MB
M6B
MPB
N"B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
PB
PbB
P�B
Q4B
QNB
Q4B
Q�B
Q�B
RTB
R�B
R�B
S@B
SB
SB
S@B
S�B
S�B
T,B
TaB
T{B
T�B
T�B
UMB
U�B
U�B
U�B
VB
VmB
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
Y�B
Z�B
Z�B
[=B
[qB
[�B
\]B
\xB
\�B
]IB
]IB
]�B
]�B
]�B
^B
^5B
^�B
_!B
_pB
`B
`'B
`vB
`�B
`�B
`�B
`�B
abB
a�B
bB
bNB
b�B
b�B
b�B
cB
c B
c B
cB
c:B
cnB
dB
d&B
d&B
d&B
dB
d&B
d@B
d�B
d�B
d�B
ezB
ezB
ezB
eFB
e,B
e`B
e`B
f2B
fLB
ffB
f�B
f�B
f�B
f�B
f�B
gB
gB
gRB
gmB
g�B
g�B
g�B
hXB
h�B
h�B
h�B
iDB
i_B
i�B
jeB
jeB
j�B
kB
k6B
kkB
kkB
kkB
kQB
k�B
k�B
k�B
lB
l�B
l�B
l�B
l�B
l�B
l�B
mB
m)B
mCB
m�B
m�B
m�B
nB
n}B
n�B
oB
oOB
o5B
o�B
o�B
o�B
o�B
p!B
poB
p�B
p�B
qB
qAB
qAB
q[B
q�B
rGB
r�B
r�B
r�B
r�B
r�B
s3B
shB
s�B
s�B
s�B
s�B
s�B
tnB
tTB
tnB
t�B
t�B
uB
uZB
u?B
u?B
u�B
u�B
u�B
u�B
u�B
vFB
v�B
w2B
w2B
wfB
w�B
w�B
wfB
wfB
w�B
w�B
xB
xB
xRB
xlB
x�B
x�B
x�B
y	B
y	B
yXB
y�B
zB
z*B
z*B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{0B
{JB
{�B
|B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
}B
}<B
}VB
}VB
}qB
}�B
}qB
}qB
}qB
}�B
~BB
~]B
~BB
~BB
~(B
B
B
B
B
cB
cB
cB
}B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�;B
�;B
�oB
��B
��B
��B
��B
�'B
�AB
�uB
��B
��B
��B
��B
�B
�aB
�{B
��B
��B
��B
��B
�MB
�MB
��B
��B
��B
��B
�B
�B
�B
�SB
��B
��B
��B
��B
�%B
�tB
��B
��B
��B
��B
��B
�B
��B
�+B
�EB
�EB
�zB
��B
��B
��B
��B
�fB
��B
��B
��B
�B
�7B
��B
��B
��B
�	B
�#B
�=B
�=B
�XB
��B
��B
�)B
�B
�xB
��B
��B
��B
�JB
��B
��B
�B
�6B
��B
��B
��B
��B
��B
�"B
�VB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�BB
��B
��B
�.B
�B
�.B
�bB
��B
��B
��B
�B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
pB
�B
�B
;B
;B
!B
!B
B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
!B
VB
VB
�B
�B
�B
�B
pB
 BB
 \B
�B
 'B
"NB
# B
.IB
�BB
��B
�
B
��BVBPHBc B�UB��B��B�eB��BBB�B�B��B�mB�}B��B�Br-Bl�Bp�Bx�B.B�\B��BH�Ba�Bf�Bi�Be�BSuBB�B1'B,�B+QB!�B�BB�B�yB�NB�[B��B� B��B�Bl"BP�B;0B �B{B
�B
�>B
��B
e`B
P.B
A;B
8RB
5�B
-]B
xB	�zB	ޞB	��B	��B	��B	�nB	��B	�B	l�B	_pB	M6B	=B	(XB	,B	
�B	;B�B�>B��B��B��B�@B�~BרB��B��B��B��B�wB�qB��B��B��B�mB�4B��B�4B}BByXBt�Bt�ButB{dB~�B�GB��B��B��B�(B��B��B��B�B��B��B�tB�gB�tB��B��B��B�tB��B��B�B�zB�yB�yB��B�kB�OB�9B��BªB�-B�B��B�B�uB�VB˒B�XBԯBϑB�7B��B�fBɠB�7B�dB�NB�\B	�B	OB	&�B	5%B	8RB	7�B	:�B	?}B	B�B	[WB	`vB	b�B	dB	ezB	h�B	h�B	i�B	h�B	g�B	h�B	i*B	h>B	i�B	oOB	s�B	s�B	s�B	tB	v`B	vFB	w�B	x�B	y$B	y�B	z*B	y�B	{�B	|B	|B	~BB	�'B	�aB	�zB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	��B	�XB	��B	��B	��B	�BB	ªB	�EB	�#B	�dB	��B	�B	�(B	ΥB	��B	�pB	��B	�.B	�4B	�NB	�NB	�NB	�NB	�4B	�:B	�uB	��B	ԯB	��B	ԕB	�gB	՛B	�gB	��B	յB	�B	��B	�B	�_B	�B	��B	��B	�	B	��B	�qB	�IB	ݲB	��B	�B	�B	�'B	�vB	��B	�,B	�zB	��B	�B	��B	��B	�B	�FB	��B	�sB	�B	�B	�*B	�KB	�B	�=B	��B	�B	�CB	�wB	�B	��B	��B	��B	�B	�OB	�5B	��B	�B	�B	��B	�B	��B	�B	��B	�ZB	��B	��B	��B	�+B	�LB	��B	��B	��B	�lB	��B	�rB	�*B	�^B	�xB	�^B	�*B	��B	��B	�DB	�^B	�JB	�<B	��B	��B	��B	�qB	�"B	�BB	�cB	��B
 iB
'B
'B
[B
�B
aB
�B
3B
gB
SB
YB
�B
�B
�B
�B
1B
fB
	�B
	�B
	�B
	�B
	�B
	�B

XB
	�B

XB

XB

rB

XB
�B
dB
�B
dB
�B
�B
�B
PB
jB
B
�B
�B
�B
vB
bB
 B
�B
�B
�B
�B
�B
uB
�B
{B
B
�B
B
�B
�B
�B
?B
�B
�B
�B
+B
B
_B
+B
_B
�B
B
1B
�B
KB
B
�B
�B
B
�B
	B
WB
qB
)B
�B
)B
/B
�B
B
�B
xB
IB
�B
OB
!B
$B
$�B
$�B
$ZB
$@B
$B
$@B
#�B
'�B
*�B
,B
,=B
-]B
,�B
+�B
*�B
,=B
,qB
,�B
,�B
,qB
,�B
,B
)�B
)�B
*B
*�B
*�B
+B
,WB
.�B
1�B
1AB
1vB
0�B
0!B
/OB
-�B
,=B
+kB
,qB
-wB
-�B
/B
3�B
4�B
5tB
6FB
6B
6zB
8B
8RB
8�B
8�B
9$B
9$B
9>B
9$B
8�B
7B
8B
9rB
9�B
;0B
;JB
;0B
;B
;B
:xB
:DB
;B
;�B
;�B
;�B
;�B
<6B
=<B
=�B
=�B
>BB
>wB
>B
=B
:�B
8�B
9XB
9rB
9�B
;B
<6B
<B
<B
<jB
<�B
<�B
<jB
<jB
<�B
>B
>�B
>]B
>�B
?B
?�B
?�B
?�B
?�B
?�B
@ B
@4B
@ B
A;B
@�B
AB
AoB
B'B
BAB
BuB
C-B
CGB
C�B
DB
D�B
F%B
F�B
FYB
F?B
F%B
E�B
E�B
E�B
F?B
F?B
F�B
G_B
G�B
G�B
G�B
H1B
HB
HfB
HfB
H�B
IRB
I�B
JXB
J�B
J�B
J�B
K�B
K�B
K�B
LB
L~B
MB
M6B
MPB
N"B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
PB
PbB
P�B
Q4B
QNB
Q4B
Q�B
Q�B
RTB
R�B
R�B
S@B
SB
SB
S@B
S�B
S�B
T,B
TaB
T{B
T�B
T�B
UMB
U�B
U�B
U�B
VB
VmB
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
Y�B
Z�B
Z�B
[=B
[qB
[�B
\]B
\xB
\�B
]IB
]IB
]�B
]�B
]�B
^B
^5B
^�B
_!B
_pB
`B
`'B
`vB
`�B
`�B
`�B
`�B
abB
a�B
bB
bNB
b�B
b�B
b�B
cB
c B
c B
cB
c:B
cnB
dB
d&B
d&B
d&B
dB
d&B
d@B
d�B
d�B
d�B
ezB
ezB
ezB
eFB
e,B
e`B
e`B
f2B
fLB
ffB
f�B
f�B
f�B
f�B
f�B
gB
gB
gRB
gmB
g�B
g�B
g�B
hXB
h�B
h�B
h�B
iDB
i_B
i�B
jeB
jeB
j�B
kB
k6B
kkB
kkB
kkB
kQB
k�B
k�B
k�B
lB
l�B
l�B
l�B
l�B
l�B
l�B
mB
m)B
mCB
m�B
m�B
m�B
nB
n}B
n�B
oB
oOB
o5B
o�B
o�B
o�B
o�B
p!B
poB
p�B
p�B
qB
qAB
qAB
q[B
q�B
rGB
r�B
r�B
r�B
r�B
r�B
s3B
shB
s�B
s�B
s�B
s�B
s�B
tnB
tTB
tnB
t�B
t�B
uB
uZB
u?B
u?B
u�B
u�B
u�B
u�B
u�B
vFB
v�B
w2B
w2B
wfB
w�B
w�B
wfB
wfB
w�B
w�B
xB
xB
xRB
xlB
x�B
x�B
x�B
y	B
y	B
yXB
y�B
zB
z*B
z*B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{0B
{JB
{�B
|B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
}B
}<B
}VB
}VB
}qB
}�B
}qB
}qB
}qB
}�B
~BB
~]B
~BB
~BB
~(B
B
B
B
B
cB
cB
cB
}B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�;B
�;B
�oB
��B
��B
��B
��B
�'B
�AB
�uB
��B
��B
��B
��B
�B
�aB
�{B
��B
��B
��B
��B
�MB
�MB
��B
��B
��B
��B
�B
�B
�B
�SB
��B
��B
��B
��B
�%B
�tB
��B
��B
��B
��B
��B
�B
��B
�+B
�EB
�EB
�zB
��B
��B
��B
��B
�fB
��B
��B
��B
�B
�7B
��B
��B
��B
�	B
�#B
�=B
�=B
�XB
��B
��B
�)B
�B
�xB
��B
��B
��B
�JB
��B
��B
�B
�6B
��B
��B
��B
��B
��B
�"B
�VB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�BB
��B
��B
�.B
�B
�.B
�bB
��B
��B
��B
�B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230414010223  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230414010227  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230414010227  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230414010228                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230414010228  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230414010228  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230414011028                      G�O�G�O�G�O�                