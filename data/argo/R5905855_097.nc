CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:27:51Z creation;2022-06-04T19:27:52Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192751  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               aA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ٙ>��Y�1   @ٙ?�X�@,�=p��
�d,�1&�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B  BffB   B'��B/��B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB���B���B�  B���B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B���B���B�  B�  B�  B�  B�  B�  B�  C 33C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CY�fC\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQy�DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dރ3D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@!�@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�=qA�=qA�p�B�RB�RB�B�RB'Q�B/Q�B7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBx�B�u�B�u�B��)B���B��)B��)B��)B��)B��)B��)B�B�B���B��)B��)B��)B��)B��)B��)B��)B��)B�\B�B�B��)Bۨ�Bߨ�B��)B��)B��)B��)B��)B��)B��)C !GC�zC�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C&�C(�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CP�CQ�CS�CU�CW�CY�zC[�C]�C_�zCa�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�zC{�C}�C�C��
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
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP�DQuDQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�z�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�Dހ�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�;�A�7LA�8�A�4A�0�A�0UA�/�A�+�A�-wA�%A�&�A�MA���A���A��DA���A��A��A���A��QA��dAۭ�A�N<A���A�9�A٧�A�]dA���A���A�+Aؾ�A�GzA��dA�5�A�x�A��A�V9A̯�A��A�+�A�
�A�#:A��,A�2�A�@A���A�FtA�_;A��ZA��A�kQA��PA���A�	7A��UA��A��A���A���A� 'A���A��gA�_A��A�d�A�[WA���A� A� �A�(�A�zxA��A���A���A�e`A��|A���A��A�AA�IRA�K�A�G�A�;�A��(A���A�h�A�ٴA��A�4�A�8RA{��Ax��As��Ao�^Al��AjAe{JA_m]A\\�AV�zAQ�{AL~�AF��AC+AAL�A@"�A>s�A>a�A>A=8�A<�<A<=qA;33A9�A8]�A6��A6#:A6A4�	A2�A/�A/�hA/5?A.��A.��A.�A-/�A,�A,}VA,�A+��A+�}A+�A*�A)�qA(��A(PHA'�FA'��A':�A&��A&�pA%��A%VA$�>A$ںA$��A$J�A#��A#u�A#&A"qvA!`BA!&�A �YA��A=qA�A��A|�A?A��A��Ah
A�ZA�tA@�A�A��A�dA&�A��AffA$A�/AbNA�+A��A��Az�AC-A��A4A|AFtA�A�A�A��AXyA(A��A�AJ�A�]A�A�AJA�-A�LA�zA�APHA�AA}VA($A��A  Ae�AVA��A.�A�A��A0�A-A
�A	��A	F�A	+A�SA:*A�A�A�AbA��AM�A�A��A�A��Av�A\�AB�A��A�SAj�A ںA @�A +@��@��@��4@�a@���@�Ov@�:�@���@���@�q@�N�@�3�@�O@��@���@��h@�(�@�8�@�ԕ@���@��K@��@��@��	@�s@��@�Ta@�]�@���@��m@�n�@��@��@�(@���@�`B@���@�6@�(@��@�hs@��@�S�@��@�o@�S�@�o�@�(@��/@�kQ@��&@�@�j@��@���@��@�F@��@�~@ߏ�@��H@�{@���@�?}@��@�O@�&@��@ږ�@�.�@���@�/�@�y>@���@��@�S�@��A@�	�@�{J@���@�v�@Ӳ-@�o @�0�@ҁo@��T@�~�@Б @�7�@���@ϲ-@�~�@�>�@��@α�@�,=@��@͏�@�H�@�	l@̞@��@�c�@��@��K@�ȴ@ʅ�@��r@�&�@�z@�'R@��@�m]@�>�@��@�1�@ť@���@�z�@�ݘ@��@��@�Q�@��<@��@��@�q�@��K@�S&@�@��X@���@�*�@��@�{J@�/�@��I@��+@�^�@��@�M@���@��@��2@��.@��Z@���@���@�|�@�{J@�g�@�P�@�IR@�A @�(@��X@���@��@��@���@���@�H�@�%@���@���@�|@�/@��[@�PH@���@���@�j�@�X@�Q�@�H�@�@���@�خ@���@�Mj@��@�h�@��)@��{@�k�@�O�@��f@���@�GE@��@��.@��Q@��S@�Vm@��@���@�C-@�%�@��@��@�b�@�C�@�33@��@���@�D�@��@��Q@�L�@�	l@��@���@�,=@��@�&�@��\@�ff@�Z�@�Ft@�:*@�*�@���@��@�xl@�R�@�-@�	@��@��j@��@�RT@�u�@�-@���@�j�@�q@��@���@�~�@���@�M@�7@���@��j@��@�%@��`@��@���@��r@�x@�*0@�o@�ی@�_�@��>@���@���@���@��@���@���@�RT@��@��$@�h
@��@�� @���@�qv@�G�@�@��!@�bN@�b@��=@��@�Z�@��o@���@���@�X�@��@��K@��<@�q@�,=@��Z@��t@���@�8@��P@��v@�Z�@��@���@��@�ԕ@��N@���@���@�-w@��,@���@�|�@�B[@��]@��K@���@�w2@�P�@�V@��@�3�@���@���@�RT@��/@��m@���@���@�~@���@���@�6z@��v@���@���@�6�@�ԕ@��@���@�e,@��@���@�\�@�O@�	@�a@iD@@O@6z@~��@~?@~_@}@@|V�@|M@{�g@{��@{j�@zں@z($@y��@y��@yk�@y5�@y;@x�E@x�_@xc�@w�&@wY@v�r@u�@t�@t�@toi@tx@s��@siD@rߤ@q��@q+�@pɆ@p�D@pc�@p@o�@oa@o�@n��@nc @n�@m��@m�@m�C@mG�@l�`@lg8@k��@kZ�@kY@j�F@jkQ@ja|@j_�@jQ@j;�@j-@j
�@i�7@h�`@hw�@h<�@h�@g��@g��@gC�@f�@f�}@fd�@e@e^�@du�@d~@c��@c33@cS@b��@b:*@a�@`��@`y>@_��@_@O@_@^�@^0U@]�N@]�M@]G�@];@\��@\(�@[�A@[�w@[��@[t�@[P�@[�@Z�2@Zn�@Y�@Y��@Y�@Yk�@YDg@Y+�@X�@X(�@W�;@W�P@W>�@W�@V�@V��@V;�@U��@U��@U[W@U5�@U#�@T��@T��@T�@TN�@S��@S~�@R�@R6�@Q�)@Q�#@Q�@Q�3@Q��@Q�@Q�7@P��@O�K@O�@@N�H@Nxl@Na|@M�@MT�@M�@MV@L��@LS�@LFt@L	�@Kخ@Kl�@K6z@J�R@J0U@I�@I��@IDg@I%@H��@Hw�@H(�@G�;@G��@Gqv@GW?@G@F��@F;�@E�X@Ea�@E \@D��@D��@D�@C�@C�F@C��@C�@C>�@B�!@B:*@A�z@A+@@��@@bN@@M@?O@>ȴ@>��@>xl@=��@=�@=�@=�n@=w2@=X@=!�@<��@<�[@<��@<w�@<,=@;�F@;o�@;�@:��@:�@:�B@:#:@9@9��@9p�@9T�@9=�@9�@8ѷ@8��@8S�@7�A@7�}@7�F@7t�@7$t@6͟@6YK@5��@5c�@5@4��@4�@4�$@4U2@3�@3��@3dZ@3$t@2�y@2��@2i�@1��@1zx@17L@1@@0�	@0�@0Ĝ@0m�@/�W@/�F@/a@.͟@.l�@.O@-�.@-�@-�S@-Y�@-;@,��@,bN@,M@+�0@+dZ@++@*��@*�,@+@+S@*��@*3�@)��@)O�@)`B@)�@)ϫ@)}�@)0�@(�@(7�@'�]@'�@@'a@'�@&��@&kQ@&\�@&GE@&0U@&�@%��@%�^@%��@%u�@%F@$�@$�)@$��@$�9@$��@$�O@$%�@$  @#�W@#��@#��@#y�@#K�@#�@"��@"�H@"��@"~�@"GE@"e@!�>@!��@!��@!^�@!G�@ �@ �o@ [�@ >B@ 'R@� @�{@b�@Mj@=@ i@�X@C�@��@�t@w2@\�@?}@!�@�f@��@y>@Q�@PH@ �@�m@�*@>�@��@��@~�@0U@�@�@�@@�t@0�@Ɇ@�_@��@Q�@�@�@�Q@�@@X�@�@��@͟@�1@l�@\�@E�@$�@��@�M@c�@\�@Y�@IR@�@��@e�@G@�r@خ@b�@,�@S@�B@�x@6�@��@�=@o @IR@�@�	@�@�?@r�@7�@"h@�@�;@��@b�@4�@�@ i@� @n�@Z�@?@4@�@��@c�@(�@q@��@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�;�A�7LA�8�A�4A�0�A�0UA�/�A�+�A�-wA�%A�&�A�MA���A���A��DA���A��A��A���A��QA��dAۭ�A�N<A���A�9�A٧�A�]dA���A���A�+Aؾ�A�GzA��dA�5�A�x�A��A�V9A̯�A��A�+�A�
�A�#:A��,A�2�A�@A���A�FtA�_;A��ZA��A�kQA��PA���A�	7A��UA��A��A���A���A� 'A���A��gA�_A��A�d�A�[WA���A� A� �A�(�A�zxA��A���A���A�e`A��|A���A��A�AA�IRA�K�A�G�A�;�A��(A���A�h�A�ٴA��A�4�A�8RA{��Ax��As��Ao�^Al��AjAe{JA_m]A\\�AV�zAQ�{AL~�AF��AC+AAL�A@"�A>s�A>a�A>A=8�A<�<A<=qA;33A9�A8]�A6��A6#:A6A4�	A2�A/�A/�hA/5?A.��A.��A.�A-/�A,�A,}VA,�A+��A+�}A+�A*�A)�qA(��A(PHA'�FA'��A':�A&��A&�pA%��A%VA$�>A$ںA$��A$J�A#��A#u�A#&A"qvA!`BA!&�A �YA��A=qA�A��A|�A?A��A��Ah
A�ZA�tA@�A�A��A�dA&�A��AffA$A�/AbNA�+A��A��Az�AC-A��A4A|AFtA�A�A�A��AXyA(A��A�AJ�A�]A�A�AJA�-A�LA�zA�APHA�AA}VA($A��A  Ae�AVA��A.�A�A��A0�A-A
�A	��A	F�A	+A�SA:*A�A�A�AbA��AM�A�A��A�A��Av�A\�AB�A��A�SAj�A ںA @�A +@��@��@��4@�a@���@�Ov@�:�@���@���@�q@�N�@�3�@�O@��@���@��h@�(�@�8�@�ԕ@���@��K@��@��@��	@�s@��@�Ta@�]�@���@��m@�n�@��@��@�(@���@�`B@���@�6@�(@��@�hs@��@�S�@��@�o@�S�@�o�@�(@��/@�kQ@��&@�@�j@��@���@��@�F@��@�~@ߏ�@��H@�{@���@�?}@��@�O@�&@��@ږ�@�.�@���@�/�@�y>@���@��@�S�@��A@�	�@�{J@���@�v�@Ӳ-@�o @�0�@ҁo@��T@�~�@Б @�7�@���@ϲ-@�~�@�>�@��@α�@�,=@��@͏�@�H�@�	l@̞@��@�c�@��@��K@�ȴ@ʅ�@��r@�&�@�z@�'R@��@�m]@�>�@��@�1�@ť@���@�z�@�ݘ@��@��@�Q�@��<@��@��@�q�@��K@�S&@�@��X@���@�*�@��@�{J@�/�@��I@��+@�^�@��@�M@���@��@��2@��.@��Z@���@���@�|�@�{J@�g�@�P�@�IR@�A @�(@��X@���@��@��@���@���@�H�@�%@���@���@�|@�/@��[@�PH@���@���@�j�@�X@�Q�@�H�@�@���@�خ@���@�Mj@��@�h�@��)@��{@�k�@�O�@��f@���@�GE@��@��.@��Q@��S@�Vm@��@���@�C-@�%�@��@��@�b�@�C�@�33@��@���@�D�@��@��Q@�L�@�	l@��@���@�,=@��@�&�@��\@�ff@�Z�@�Ft@�:*@�*�@���@��@�xl@�R�@�-@�	@��@��j@��@�RT@�u�@�-@���@�j�@�q@��@���@�~�@���@�M@�7@���@��j@��@�%@��`@��@���@��r@�x@�*0@�o@�ی@�_�@��>@���@���@���@��@���@���@�RT@��@��$@�h
@��@�� @���@�qv@�G�@�@��!@�bN@�b@��=@��@�Z�@��o@���@���@�X�@��@��K@��<@�q@�,=@��Z@��t@���@�8@��P@��v@�Z�@��@���@��@�ԕ@��N@���@���@�-w@��,@���@�|�@�B[@��]@��K@���@�w2@�P�@�V@��@�3�@���@���@�RT@��/@��m@���@���@�~@���@���@�6z@��v@���@���@�6�@�ԕ@��@���@�e,@��@���@�\�@�O@�	@�a@iD@@O@6z@~��@~?@~_@}@@|V�@|M@{�g@{��@{j�@zں@z($@y��@y��@yk�@y5�@y;@x�E@x�_@xc�@w�&@wY@v�r@u�@t�@t�@toi@tx@s��@siD@rߤ@q��@q+�@pɆ@p�D@pc�@p@o�@oa@o�@n��@nc @n�@m��@m�@m�C@mG�@l�`@lg8@k��@kZ�@kY@j�F@jkQ@ja|@j_�@jQ@j;�@j-@j
�@i�7@h�`@hw�@h<�@h�@g��@g��@gC�@f�@f�}@fd�@e@e^�@du�@d~@c��@c33@cS@b��@b:*@a�@`��@`y>@_��@_@O@_@^�@^0U@]�N@]�M@]G�@];@\��@\(�@[�A@[�w@[��@[t�@[P�@[�@Z�2@Zn�@Y�@Y��@Y�@Yk�@YDg@Y+�@X�@X(�@W�;@W�P@W>�@W�@V�@V��@V;�@U��@U��@U[W@U5�@U#�@T��@T��@T�@TN�@S��@S~�@R�@R6�@Q�)@Q�#@Q�@Q�3@Q��@Q�@Q�7@P��@O�K@O�@@N�H@Nxl@Na|@M�@MT�@M�@MV@L��@LS�@LFt@L	�@Kخ@Kl�@K6z@J�R@J0U@I�@I��@IDg@I%@H��@Hw�@H(�@G�;@G��@Gqv@GW?@G@F��@F;�@E�X@Ea�@E \@D��@D��@D�@C�@C�F@C��@C�@C>�@B�!@B:*@A�z@A+@@��@@bN@@M@?O@>ȴ@>��@>xl@=��@=�@=�@=�n@=w2@=X@=!�@<��@<�[@<��@<w�@<,=@;�F@;o�@;�@:��@:�@:�B@:#:@9@9��@9p�@9T�@9=�@9�@8ѷ@8��@8S�@7�A@7�}@7�F@7t�@7$t@6͟@6YK@5��@5c�@5@4��@4�@4�$@4U2@3�@3��@3dZ@3$t@2�y@2��@2i�@1��@1zx@17L@1@@0�	@0�@0Ĝ@0m�@/�W@/�F@/a@.͟@.l�@.O@-�.@-�@-�S@-Y�@-;@,��@,bN@,M@+�0@+dZ@++@*��@*�,@+@+S@*��@*3�@)��@)O�@)`B@)�@)ϫ@)}�@)0�@(�@(7�@'�]@'�@@'a@'�@&��@&kQ@&\�@&GE@&0U@&�@%��@%�^@%��@%u�@%F@$�@$�)@$��@$�9@$��@$�O@$%�@$  @#�W@#��@#��@#y�@#K�@#�@"��@"�H@"��@"~�@"GE@"e@!�>@!��@!��@!^�@!G�@ �@ �o@ [�@ >B@ 'R@� @�{@b�@Mj@=@ i@�X@C�@��@�t@w2@\�@?}@!�@�f@��@y>@Q�@PH@ �@�m@�*@>�@��@��@~�@0U@�@�@�@@�t@0�@Ɇ@�_@��@Q�@�@�@�Q@�@@X�@�@��@͟@�1@l�@\�@E�@$�@��@�M@c�@\�@Y�@IR@�@��@e�@G@�r@خ@b�@,�@S@�B@�x@6�@��@�=@o @IR@�@�	@�@�?@r�@7�@"h@�@�;@��@b�@4�@�@ i@� @n�@Z�@?@4@�@��@c�@(�@q@��@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	��B	�OB	�iB	�OB	�iB	�4B	� B	.B	B	.B	cB	�B	�B	�OB	�iB	�UB	��B	��B
�B
tTB
�tB
�B
�rB
οB �B*0B%,B BB�B
��B
��B
�B
�aB
�EB
�*B
�/B
��B#BOvBiyB{�B�B�FB�\B�kB��B�/B�B�=B�MB��BňB�B�:B�1B��B�B��B��B��B��B�_B�B�8B�;B�1B�SB��B�^B��Bj�BM6B5�B�B�B
�cB
�)B
��B
��B
�4B
��B
{B
tnB
_�B
J	B
0�B
�B	�fB	רB	�qB	�B	�NB	}<B	[qB	MPB	1�B	�B	�B	�B	�B	�B	�B	�B	B	VB	 �B	 �B	 B	�B	!�B	$�B	'B	7LB	LdB	bNB	tB	~�B	�nB	��B	��B	�B	�bB	��B	��B	�lB	�(B
�B
�B
B
B
�B
QB
$�B
&LB
'RB
(XB
(
B
(XB
.}B
2|B
3�B
5?B
6zB
6zB
6�B
7�B
7fB
5�B
6`B
9	B
6�B
5B
?.B
B�B
B�B
D3B
F%B
G�B
I7B
J	B
KDB
K�B
MPB
M�B
NB
M�B
MjB
M�B
NpB
N<B
M�B
M�B
M�B
M�B
M�B
MjB
L�B
M�B
L�B
I�B
IB
HKB
G�B
F�B
@�B
?�B
@OB
B�B
D�B
E�B
F�B
F�B
HB
H�B
H�B
J�B
M�B
N�B
N�B
N�B
NpB
NpB
N"B
MPB
K�B
IB
F�B
DB
B�B
CGB
A;B
=B
9�B
8B
88B
7B
6�B
49B
2�B
1[B
0�B
/�B
,=B
)�B
(�B
&�B
&fB
#nB
�B
�B
CB
�B
WB
B
�B
�B
]B
�B
�B
/B
�B
�B
�B
�B
�B
�B
�B
	B
WB
�B
�B
qB
	B
KB
�B
�B
�B
B
bB
�B
�B
�B
WB
�B
xB
WB
eB
yB
�B
B
B
�B
�B
�B
�B
�B
�B
uB
�B
 B
NB
�B
�B
�B
vB
<B
�B
�B
6B
PB
�B
0B
B
�B
B
xB

�B
	�B
	lB
�B
�B
�B
�B
+B
�B
?B
�B
�B
�B
�B
�B
�B
tB
%B
�B
�B
	RB
�B
	B
	�B
	�B
	�B
	�B
�B
�B
�B
mB
�B
�B
SB
B
tB
�B
tB
�B
�B
mB
9B
9B
SB
SB
9B
�B
�B
B
�B
�B
mB
9B
mB
�B
SB
�B
3B
aB
AB
UB
oB
UB
�B
�B
 �B
 �B
�B
'B
[B
B
�B
uB
AB
�B
�B
�B
oB
oB
oB
B
[B
�B
B
GB
�B
-B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
_B
zB
�B
�B
�B
�B
�B
�B
�B
�B
	B
	�B
	�B

XB

�B
xB
)B
)B

�B
)B
�B
�B
^B
)B
)B
)B
�B
B
B
B
�B
jB
jB
�B
B
�B
�B
�B
�B
�B
�B
HB
hB
�B
�B
oB
�B
�B
FB
aB
B
,B
�B
�B
[B
uB
�B
gB
�B
B
SB
mB
�B
B
�B
�B
sB
sB
�B
�B
�B
+B
_B
1B
1B
�B
�B
�B
B
�B
�B
�B
�B
�B
=B
qB
WB
�B
)B
�B
�B
�B
�B
�B
�B
jB
VB
�B
�B
 BB
 �B
 �B
!B
!-B
!HB
!HB
!HB
!�B
"B
"NB
"�B
#:B
#�B
#�B
#�B
$B
$tB
$�B
$�B
%B
%FB
%FB
%�B
%�B
&2B
&fB
&2B
'B
'8B
'RB
'RB
'mB
'mB
'mB
'RB
'�B
'�B
'�B
($B
(�B
(sB
(sB
(
B
(>B
(
B
(�B
)*B
)DB
)�B
)�B
*eB
+QB
+B
*�B
+B
+�B
+�B
,�B
,�B
-wB
-CB
-]B
.B
.�B
.�B
.�B
.�B
/�B
0!B
0!B
0UB
0UB
0�B
0�B
1B
0�B
1�B
2|B
3hB
49B
4B
4B
49B
49B
4nB
5%B
5�B
5�B
5�B
5�B
5�B
6FB
6FB
6zB
6zB
6�B
7fB
7�B
8RB
9	B
9>B
9XB
9�B
9�B
9�B
:*B
:�B
;dB
;dB
;dB
;dB
;�B
;�B
<jB
<�B
<�B
<�B
=VB
=VB
=VB
=VB
=�B
=�B
=�B
=�B
>�B
?�B
@4B
@OB
@OB
@OB
@�B
@iB
@OB
@�B
A�B
BB
B�B
B�B
B�B
B�B
B�B
CB
C{B
C{B
C�B
DB
DgB
D�B
EB
E9B
D�B
D�B
D�B
ESB
F%B
E�B
E�B
E�B
E�B
F%B
F�B
F�B
F�B
F�B
G�B
HB
HfB
H�B
H�B
H�B
H�B
IB
IlB
I�B
I�B
I�B
JrB
J=B
J#B
J=B
JXB
J�B
J�B
KB
KB
K)B
K^B
KxB
K^B
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
L0B
L�B
MB
MPB
N"B
NVB
NVB
N<B
NVB
NVB
N<B
NB
N�B
O�B
O�B
P�B
P�B
P�B
P}B
PbB
Q B
QhB
Q�B
Q�B
RoB
RoB
RoB
R�B
SB
SuB
S�B
S�B
S�B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UMB
UMB
U�B
U�B
U�B
V9B
VmB
VmB
VmB
V�B
V�B
W
B
W
B
W?B
W�B
W�B
W�B
W�B
X�B
YB
YB
Y1B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZB
ZQB
Z�B
Z�B
[	B
[	B
[	B
Z�B
[�B
[�B
[�B
[�B
\)B
\B
\)B
\xB
\xB
\�B
\�B
\�B
\�B
]/B
]IB
]~B
]�B
^5B
^�B
^�B
_B
_B
_B
_�B
_�B
_�B
`'B
`\B
`vB
`�B
`vB
a-B
a-B
abB
a|B
a�B
a�B
a�B
a�B
bhB
bhB
b�B
c B
cTB
cnB
c�B
c�B
c�B
c�B
d&B
d&B
d�B
d�B
eB
e,B
e`B
e�B
fB
g�B
h�B
i*B
h�B
g�B
g�B
hXB
i_B
i�B
i�B
i�B
iDB
h�B
h�B
i�B
j0B
jB
kB
k6B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l=B
lqB
l�B
lqB
m�B
nB
m�B
nB
nB
nIB
ncB
ncB
n}B
n�B
o5B
o�B
o�B
o�B
o�B
o�B
o�B
pUB
qB
p�B
qB
q[B
qvB
q�B
q�B
q�B
rB
rB
r-B
r-B
q�B
r-B
q�B
q�B
q�B
rB
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t9B
tB
tTB
t�B
t�B
t�B
t�B
uB
u?B
u?B
utB
utB
utB
u�B
vB
vFB
vFB
v�B
v�B
wB
wfB
w�B
w�B
xB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
y$B
yrB
y�B
y�B
y�B
y�B
zB
z^B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|B
|B
|jB
|�B
|�B
|�B
}"B
}<B
}VB
}VB
}qB
}�B
~B
~B
~B
~]B
~�B
~�B
~�B
~�B
~�B
.B
B
.B
cB
�B
�B
�B
�B
�4B
��B
�B
��B
�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	��B	�OB	�iB	�OB	�iB	�4B	� B	.B	B	.B	cB	�B	�B	�OB	�iB	�UB	��B	��B
�B
tTB
�tB
�B
�rB
οB �B*0B%,B BB�B
��B
��B
�B
�aB
�EB
�*B
�/B
��B#BOvBiyB{�B�B�FB�\B�kB��B�/B�B�=B�MB��BňB�B�:B�1B��B�B��B��B��B��B�_B�B�8B�;B�1B�SB��B�^B��Bj�BM6B5�B�B�B
�cB
�)B
��B
��B
�4B
��B
{B
tnB
_�B
J	B
0�B
�B	�fB	רB	�qB	�B	�NB	}<B	[qB	MPB	1�B	�B	�B	�B	�B	�B	�B	�B	B	VB	 �B	 �B	 B	�B	!�B	$�B	'B	7LB	LdB	bNB	tB	~�B	�nB	��B	��B	�B	�bB	��B	��B	�lB	�(B
�B
�B
B
B
�B
QB
$�B
&LB
'RB
(XB
(
B
(XB
.}B
2|B
3�B
5?B
6zB
6zB
6�B
7�B
7fB
5�B
6`B
9	B
6�B
5B
?.B
B�B
B�B
D3B
F%B
G�B
I7B
J	B
KDB
K�B
MPB
M�B
NB
M�B
MjB
M�B
NpB
N<B
M�B
M�B
M�B
M�B
M�B
MjB
L�B
M�B
L�B
I�B
IB
HKB
G�B
F�B
@�B
?�B
@OB
B�B
D�B
E�B
F�B
F�B
HB
H�B
H�B
J�B
M�B
N�B
N�B
N�B
NpB
NpB
N"B
MPB
K�B
IB
F�B
DB
B�B
CGB
A;B
=B
9�B
8B
88B
7B
6�B
49B
2�B
1[B
0�B
/�B
,=B
)�B
(�B
&�B
&fB
#nB
�B
�B
CB
�B
WB
B
�B
�B
]B
�B
�B
/B
�B
�B
�B
�B
�B
�B
�B
	B
WB
�B
�B
qB
	B
KB
�B
�B
�B
B
bB
�B
�B
�B
WB
�B
xB
WB
eB
yB
�B
B
B
�B
�B
�B
�B
�B
�B
uB
�B
 B
NB
�B
�B
�B
vB
<B
�B
�B
6B
PB
�B
0B
B
�B
B
xB

�B
	�B
	lB
�B
�B
�B
�B
+B
�B
?B
�B
�B
�B
�B
�B
�B
tB
%B
�B
�B
	RB
�B
	B
	�B
	�B
	�B
	�B
�B
�B
�B
mB
�B
�B
SB
B
tB
�B
tB
�B
�B
mB
9B
9B
SB
SB
9B
�B
�B
B
�B
�B
mB
9B
mB
�B
SB
�B
3B
aB
AB
UB
oB
UB
�B
�B
 �B
 �B
�B
'B
[B
B
�B
uB
AB
�B
�B
�B
oB
oB
oB
B
[B
�B
B
GB
�B
-B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
_B
zB
�B
�B
�B
�B
�B
�B
�B
�B
	B
	�B
	�B

XB

�B
xB
)B
)B

�B
)B
�B
�B
^B
)B
)B
)B
�B
B
B
B
�B
jB
jB
�B
B
�B
�B
�B
�B
�B
�B
HB
hB
�B
�B
oB
�B
�B
FB
aB
B
,B
�B
�B
[B
uB
�B
gB
�B
B
SB
mB
�B
B
�B
�B
sB
sB
�B
�B
�B
+B
_B
1B
1B
�B
�B
�B
B
�B
�B
�B
�B
�B
=B
qB
WB
�B
)B
�B
�B
�B
�B
�B
�B
jB
VB
�B
�B
 BB
 �B
 �B
!B
!-B
!HB
!HB
!HB
!�B
"B
"NB
"�B
#:B
#�B
#�B
#�B
$B
$tB
$�B
$�B
%B
%FB
%FB
%�B
%�B
&2B
&fB
&2B
'B
'8B
'RB
'RB
'mB
'mB
'mB
'RB
'�B
'�B
'�B
($B
(�B
(sB
(sB
(
B
(>B
(
B
(�B
)*B
)DB
)�B
)�B
*eB
+QB
+B
*�B
+B
+�B
+�B
,�B
,�B
-wB
-CB
-]B
.B
.�B
.�B
.�B
.�B
/�B
0!B
0!B
0UB
0UB
0�B
0�B
1B
0�B
1�B
2|B
3hB
49B
4B
4B
49B
49B
4nB
5%B
5�B
5�B
5�B
5�B
5�B
6FB
6FB
6zB
6zB
6�B
7fB
7�B
8RB
9	B
9>B
9XB
9�B
9�B
9�B
:*B
:�B
;dB
;dB
;dB
;dB
;�B
;�B
<jB
<�B
<�B
<�B
=VB
=VB
=VB
=VB
=�B
=�B
=�B
=�B
>�B
?�B
@4B
@OB
@OB
@OB
@�B
@iB
@OB
@�B
A�B
BB
B�B
B�B
B�B
B�B
B�B
CB
C{B
C{B
C�B
DB
DgB
D�B
EB
E9B
D�B
D�B
D�B
ESB
F%B
E�B
E�B
E�B
E�B
F%B
F�B
F�B
F�B
F�B
G�B
HB
HfB
H�B
H�B
H�B
H�B
IB
IlB
I�B
I�B
I�B
JrB
J=B
J#B
J=B
JXB
J�B
J�B
KB
KB
K)B
K^B
KxB
K^B
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
L0B
L�B
MB
MPB
N"B
NVB
NVB
N<B
NVB
NVB
N<B
NB
N�B
O�B
O�B
P�B
P�B
P�B
P}B
PbB
Q B
QhB
Q�B
Q�B
RoB
RoB
RoB
R�B
SB
SuB
S�B
S�B
S�B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UMB
UMB
U�B
U�B
U�B
V9B
VmB
VmB
VmB
V�B
V�B
W
B
W
B
W?B
W�B
W�B
W�B
W�B
X�B
YB
YB
Y1B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZB
ZQB
Z�B
Z�B
[	B
[	B
[	B
Z�B
[�B
[�B
[�B
[�B
\)B
\B
\)B
\xB
\xB
\�B
\�B
\�B
\�B
]/B
]IB
]~B
]�B
^5B
^�B
^�B
_B
_B
_B
_�B
_�B
_�B
`'B
`\B
`vB
`�B
`vB
a-B
a-B
abB
a|B
a�B
a�B
a�B
a�B
bhB
bhB
b�B
c B
cTB
cnB
c�B
c�B
c�B
c�B
d&B
d&B
d�B
d�B
eB
e,B
e`B
e�B
fB
g�B
h�B
i*B
h�B
g�B
g�B
hXB
i_B
i�B
i�B
i�B
iDB
h�B
h�B
i�B
j0B
jB
kB
k6B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l=B
lqB
l�B
lqB
m�B
nB
m�B
nB
nB
nIB
ncB
ncB
n}B
n�B
o5B
o�B
o�B
o�B
o�B
o�B
o�B
pUB
qB
p�B
qB
q[B
qvB
q�B
q�B
q�B
rB
rB
r-B
r-B
q�B
r-B
q�B
q�B
q�B
rB
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t9B
tB
tTB
t�B
t�B
t�B
t�B
uB
u?B
u?B
utB
utB
utB
u�B
vB
vFB
vFB
v�B
v�B
wB
wfB
w�B
w�B
xB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
y$B
yrB
y�B
y�B
y�B
y�B
zB
z^B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|B
|B
|jB
|�B
|�B
|�B
}"B
}<B
}VB
}VB
}qB
}�B
~B
~B
~B
~]B
~�B
~�B
~�B
~�B
~�B
.B
B
.B
cB
�B
�B
�B
�B
�4B
��B
�B
��B
�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105248  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192751  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192752  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192752                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042800  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042800  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                