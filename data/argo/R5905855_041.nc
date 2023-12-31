CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:17:47Z creation;2022-06-04T19:17:48Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191747  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               )A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�yn]L;1   @�y���H@.��G�{�cr��n�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A���A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBy33B33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�33B���B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�33B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C033C2�C433C5��C7�fC:  C<  C>  C@  CA�fCD  CF  CH  CJ  CK�fCM�fCO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D D�� D�3D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�
>A���A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBp�Bx�B~�B��)B���B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�u�B�\Bè�B��)B˨�B��)B��)B��)Bۨ�B��)B��)B��)B��)B��)B�\B�\B��)B��)C�zC�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C0!GC2�C4!GC5��C7�zC9�C;�C=�C?�CA�zCC�CE�CG�CI�CK�zCM�zCO�zCQ�CS�CU�CW�CY�C[�C]�C_�Cb�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
C��C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D��D�D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�:�D�}�D���D���D�=�D�}�D½�D� �D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�z�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��A�͟A�ȴA�͟A���A��?A�רA��yA��A��A�ӏA�ɺA�ʌA�ȀA��EA�ΥA��9A�ŢA���A���A�ĜA��aAɹ�Aɸ�Aɻ�AɸAɳhA�c�A��Aǟ�A��AƘ�A�v+A�5?A��AŘ�A�V�Aņ�A�X�A���AĊ�A�ҽA�C�A��"A=A�u�A�y�A�ӏA��iA��A���A�7�A��A��WA��hA�)*A���A��iA�uA��A�QA�<jA�1A�V9A���A��$A���A�C�A��A��yA�@A�SA�R�A��KA���A��eA��RA���A�@�A��,A�n/A�	A��A��A���A�~]A���A�{�A��lA�A�A�(�AA Azx�AuK�ArAl�Ag[�Abn�A`{A]3�AY�;AU�QAR�MAO^�AN^�AL�AK&AG��AE�|A@��A;y�A7v�A5�nA3یA3�A6{JA5C-A5-�A4zA2��A0%A+s�A'l�A%��A%0UA$1A
�A�jA�AjA7Aj�A�A�OA��A�_A;dA�}A�zA;A�=AhsA&�A	lAPHA�A��A�`A7LAy>A�A!-Ao�A�A�A�A.IAk�A�HA��A�sA7LA�A�~A�A(A��AzA��AW�A͟A�A*�A=�A��A��A��A��A��AjA��AhsA�A
oiA	l�A	Q�A	1�A	CA	xAo A�fA�VAJ#AƨA7LA�`A��A=A��A��Ac�AjAW?A��Al"AB�A�]AR�A+A�A�A �;A ��A Q�A �@��@�p;@�*0@��H@��}@��r@�v`@���@�6@��^@�u�@���@�N�@���@�a�@��@�D�@��@�D�@��@�@@�D@�H@���@��@��@�S@�]d@���@킪@��s@��@��@ꄶ@�	@�@�+@�p;@�^@�@@�ں@� @�˒@�33@�@�q@�5?@�<6@�r@�0U@��@��@��P@�6�@߬q@�Dg@���@ި�@�]d@��A@��|@�1�@۞�@�+@�oi@��9@ٹ�@٥�@�t�@ؽ<@�_@�+@��@�m�@��@�(�@�e�@�B[@��@��@�^5@��g@�&@��'@Гu@�|�@�(�@��d@�H�@�S@���@Υz@�w�@�ƨ@�\)@�+@�ߤ@�l�@��@ˏ�@�Y�@�&@ʮ}@��@��9@ɖS@�Vm@�֡@��@�]�@�&@��@ƾ�@��+@��Q@�IR@Ľ<@�[�@��@�K�@¬@��@�$t@��y@��?@�  @���@�*�@���@���@�x@�u@���@�C-@��@�y>@�5?@���@�o�@��@�|�@���@��@��~@�j�@��@��@��@���@��@@�F@��"@�r�@���@��-@�zx@�>�@���@���@��@�?�@�($@��@��T@��@���@�|@�N<@�0�@���@��8@���@�ff@�)�@�ԕ@��q@��{@�B�@��@��R@�b@��	@�\)@�)_@��@�W�@�?�@�$�@�?�@��D@��m@���@���@��H@�j@�(@��D@��+@���@���@�H�@��@��$@�Q�@�u@��@��t@��n@���@�_p@��@��[@�S�@�	@��g@�o@�PH@��@�1@��@��t@��E@�oi@�S�@�/�@��@�V@���@�4n@��@��-@���@�qv@�9�@��@���@���@��'@��h@�o @�!-@��@��D@��@��W@��X@�hs@��@�h
@�p;@��@���@���@�O@�+�@��@��B@���@�L0@�� @�(@��m@�k�@�C�@��@�GE@�a�@��@���@��@��@�A�@�2a@�S@��'@�}V@�{@��K@�t�@��@���@���@�m�@��@�ݘ@��@���@���@��M@���@�s�@�?@�:*@�2�@�"h@�4@�	@��+@�@��h@�t�@�\�@���@���@��@��^@��@��P@�x�@�e�@�O@�0�@���@�p;@�1�@�@��@��{@�h
@��*@��X@���@�7L@�ی@��@�oi@�V�@�<�@��@�@��)@�O�@���@�p;@�Xy@�N�@�Ft@�A�@�@�u�@�L�@�*0@��@��K@���@��.@�v�@�9X@��@���@�ݘ@���@�s�@�9�@�/�@�(@���@��.@�9X@� �@��A@���@��@���@��h@�@��|@���@��u@�@�
@�{@@~�M@~�@~Ta@~	@}��@}��@}?}@|�@|  @{��@{dZ@{@O@z��@zZ�@z�@yc@y&�@x��@x�Y@x?�@x@w�Q@w�w@w�P@wo@v�'@v��@vYK@v	@u�j@u��@u�C@u�~@uY�@u%F@u#�@t��@t�@tPH@tFt@t-�@t1@s�@sy�@s�@r�,@r�x@r�@q��@q��@q��@q��@qzx@p�@o��@o�@n�h@n6�@m��@mQ�@m(�@l�	@lN�@k�q@j��@j��@j?@i�j@i#�@h��@h��@hy>@h	�@g�@@fں@f�1@e�j@d�K@d��@d�@d	�@c_p@b��@bE�@b8�@a�@`�@`�@`�@`�o@`7@_�@_y�@^ȴ@^1�@]��@]��@]��@]\�@\�f@\q@[��@[��@[a@[@Zȴ@Z6�@Y�#@Yc@X�j@X7@W��@Wb�@W>�@W
=@V�m@V{�@VGE@V$�@U��@U�z@Um]@T�?@T[�@T�@S��@SZ�@SS@R�B@R��@R6�@Re@Q�X@Q^�@Q�@P�@P��@P��@P�9@PFt@O�@Oo�@O�@N�'@Nh
@N:*@N �@M��@L��@K�r@K�K@K��@K>�@J��@J�r@I�@I�C@I��@I^�@I�@H�@H��@HM@G�m@G�V@Gg�@F��@FZ�@F�@E�>@E�@E�h@Ex�@EJ�@D�@D�_@Du�@DQ�@D�@Cݘ@C��@B�'@B�@A�)@A�@A��@A��@AF@A(�@@�9@@c�@?�+@?U�@>��@>��@>a|@=�d@=�X@=��@=^�@=-w@=�@=�@<��@;��@;�4@;W?@;1�@:�@:��@:{@9�#@9�C@9X@8��@8�$@8-�@8  @7�*@7s@7)_@6��@6e@5��@5��@5�N@5�-@5�7@50�@4�|@4�[@4��@4�e@4�_@4y>@4(�@3�;@3O@2�8@2��@2z@2-@2J@1��@1�t@1��@1�@0�?@0�e@0l"@0D�@01@/��@/�{@/Z�@/ i@.�!@-��@-�^@-��@-��@-�@,�@,e�@,7@+�:@+�@*��@*�\@*Ta@*($@)�T@)��@)L�@)�@(�P@(��@(�`@(��@(r�@'�w@'��@'RT@'/�@&�2@&�@&kQ@&8�@&O@%�@%s�@%@$�E@$tT@$D�@$�@$@#�r@#�@#s@#�@"\�@"@!�t@!|@!�@ ��@ �I@ !@ �@�r@ƨ@Z�@�m@h
@8�@e@��@ԕ@}�@?}@&�@	l@�@�j@��@H@�]@�@� @�a@�*@iD@�@�@�@��@�@$�@��@X@(�@��@֡@�$@�@�@��@oi@g8@M@'R@�@�$@P�@@��@h
@?@5?@($@ �@�@��@��@|@2a@�|@�E@�z@��@q@D�@-�@!@�@�f@a@A�@@�@��@z@s�@Ov@;�@ �@��@�M@S&@%F@�|@��@�@V�@G@�
@�w@�:@n/@A�@o@�"@�M@�2@�R@q�@W�@L0@@�@�@c@8�@+@�@֡@�U111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��A�͟A�ȴA�͟A���A��?A�רA��yA��A��A�ӏA�ɺA�ʌA�ȀA��EA�ΥA��9A�ŢA���A���A�ĜA��aAɹ�Aɸ�Aɻ�AɸAɳhA�c�A��Aǟ�A��AƘ�A�v+A�5?A��AŘ�A�V�Aņ�A�X�A���AĊ�A�ҽA�C�A��"A=A�u�A�y�A�ӏA��iA��A���A�7�A��A��WA��hA�)*A���A��iA�uA��A�QA�<jA�1A�V9A���A��$A���A�C�A��A��yA�@A�SA�R�A��KA���A��eA��RA���A�@�A��,A�n/A�	A��A��A���A�~]A���A�{�A��lA�A�A�(�AA Azx�AuK�ArAl�Ag[�Abn�A`{A]3�AY�;AU�QAR�MAO^�AN^�AL�AK&AG��AE�|A@��A;y�A7v�A5�nA3یA3�A6{JA5C-A5-�A4zA2��A0%A+s�A'l�A%��A%0UA$1A
�A�jA�AjA7Aj�A�A�OA��A�_A;dA�}A�zA;A�=AhsA&�A	lAPHA�A��A�`A7LAy>A�A!-Ao�A�A�A�A.IAk�A�HA��A�sA7LA�A�~A�A(A��AzA��AW�A͟A�A*�A=�A��A��A��A��A��AjA��AhsA�A
oiA	l�A	Q�A	1�A	CA	xAo A�fA�VAJ#AƨA7LA�`A��A=A��A��Ac�AjAW?A��Al"AB�A�]AR�A+A�A�A �;A ��A Q�A �@��@�p;@�*0@��H@��}@��r@�v`@���@�6@��^@�u�@���@�N�@���@�a�@��@�D�@��@�D�@��@�@@�D@�H@���@��@��@�S@�]d@���@킪@��s@��@��@ꄶ@�	@�@�+@�p;@�^@�@@�ں@� @�˒@�33@�@�q@�5?@�<6@�r@�0U@��@��@��P@�6�@߬q@�Dg@���@ި�@�]d@��A@��|@�1�@۞�@�+@�oi@��9@ٹ�@٥�@�t�@ؽ<@�_@�+@��@�m�@��@�(�@�e�@�B[@��@��@�^5@��g@�&@��'@Гu@�|�@�(�@��d@�H�@�S@���@Υz@�w�@�ƨ@�\)@�+@�ߤ@�l�@��@ˏ�@�Y�@�&@ʮ}@��@��9@ɖS@�Vm@�֡@��@�]�@�&@��@ƾ�@��+@��Q@�IR@Ľ<@�[�@��@�K�@¬@��@�$t@��y@��?@�  @���@�*�@���@���@�x@�u@���@�C-@��@�y>@�5?@���@�o�@��@�|�@���@��@��~@�j�@��@��@��@���@��@@�F@��"@�r�@���@��-@�zx@�>�@���@���@��@�?�@�($@��@��T@��@���@�|@�N<@�0�@���@��8@���@�ff@�)�@�ԕ@��q@��{@�B�@��@��R@�b@��	@�\)@�)_@��@�W�@�?�@�$�@�?�@��D@��m@���@���@��H@�j@�(@��D@��+@���@���@�H�@��@��$@�Q�@�u@��@��t@��n@���@�_p@��@��[@�S�@�	@��g@�o@�PH@��@�1@��@��t@��E@�oi@�S�@�/�@��@�V@���@�4n@��@��-@���@�qv@�9�@��@���@���@��'@��h@�o @�!-@��@��D@��@��W@��X@�hs@��@�h
@�p;@��@���@���@�O@�+�@��@��B@���@�L0@�� @�(@��m@�k�@�C�@��@�GE@�a�@��@���@��@��@�A�@�2a@�S@��'@�}V@�{@��K@�t�@��@���@���@�m�@��@�ݘ@��@���@���@��M@���@�s�@�?@�:*@�2�@�"h@�4@�	@��+@�@��h@�t�@�\�@���@���@��@��^@��@��P@�x�@�e�@�O@�0�@���@�p;@�1�@�@��@��{@�h
@��*@��X@���@�7L@�ی@��@�oi@�V�@�<�@��@�@��)@�O�@���@�p;@�Xy@�N�@�Ft@�A�@�@�u�@�L�@�*0@��@��K@���@��.@�v�@�9X@��@���@�ݘ@���@�s�@�9�@�/�@�(@���@��.@�9X@� �@��A@���@��@���@��h@�@��|@���@��u@�@�
@�{@@~�M@~�@~Ta@~	@}��@}��@}?}@|�@|  @{��@{dZ@{@O@z��@zZ�@z�@yc@y&�@x��@x�Y@x?�@x@w�Q@w�w@w�P@wo@v�'@v��@vYK@v	@u�j@u��@u�C@u�~@uY�@u%F@u#�@t��@t�@tPH@tFt@t-�@t1@s�@sy�@s�@r�,@r�x@r�@q��@q��@q��@q��@qzx@p�@o��@o�@n�h@n6�@m��@mQ�@m(�@l�	@lN�@k�q@j��@j��@j?@i�j@i#�@h��@h��@hy>@h	�@g�@@fں@f�1@e�j@d�K@d��@d�@d	�@c_p@b��@bE�@b8�@a�@`�@`�@`�@`�o@`7@_�@_y�@^ȴ@^1�@]��@]��@]��@]\�@\�f@\q@[��@[��@[a@[@Zȴ@Z6�@Y�#@Yc@X�j@X7@W��@Wb�@W>�@W
=@V�m@V{�@VGE@V$�@U��@U�z@Um]@T�?@T[�@T�@S��@SZ�@SS@R�B@R��@R6�@Re@Q�X@Q^�@Q�@P�@P��@P��@P�9@PFt@O�@Oo�@O�@N�'@Nh
@N:*@N �@M��@L��@K�r@K�K@K��@K>�@J��@J�r@I�@I�C@I��@I^�@I�@H�@H��@HM@G�m@G�V@Gg�@F��@FZ�@F�@E�>@E�@E�h@Ex�@EJ�@D�@D�_@Du�@DQ�@D�@Cݘ@C��@B�'@B�@A�)@A�@A��@A��@AF@A(�@@�9@@c�@?�+@?U�@>��@>��@>a|@=�d@=�X@=��@=^�@=-w@=�@=�@<��@;��@;�4@;W?@;1�@:�@:��@:{@9�#@9�C@9X@8��@8�$@8-�@8  @7�*@7s@7)_@6��@6e@5��@5��@5�N@5�-@5�7@50�@4�|@4�[@4��@4�e@4�_@4y>@4(�@3�;@3O@2�8@2��@2z@2-@2J@1��@1�t@1��@1�@0�?@0�e@0l"@0D�@01@/��@/�{@/Z�@/ i@.�!@-��@-�^@-��@-��@-�@,�@,e�@,7@+�:@+�@*��@*�\@*Ta@*($@)�T@)��@)L�@)�@(�P@(��@(�`@(��@(r�@'�w@'��@'RT@'/�@&�2@&�@&kQ@&8�@&O@%�@%s�@%@$�E@$tT@$D�@$�@$@#�r@#�@#s@#�@"\�@"@!�t@!|@!�@ ��@ �I@ !@ �@�r@ƨ@Z�@�m@h
@8�@e@��@ԕ@}�@?}@&�@	l@�@�j@��@H@�]@�@� @�a@�*@iD@�@�@�@��@�@$�@��@X@(�@��@֡@�$@�@�@��@oi@g8@M@'R@�@�$@P�@@��@h
@?@5?@($@ �@�@��@��@|@2a@�|@�E@�z@��@q@D�@-�@!@�@�f@a@A�@@�@��@z@s�@Ov@;�@ �@��@�M@S&@%F@�|@��@�@V�@G@�
@�w@�:@n/@A�@o@�"@�M@�2@�R@q�@W�@L0@@�@�@c@8�@+@�@֡@�U111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�B��B�zB��B�LB��B�fB�fB�fB�2B�2B��B��B��B��B��B��B��B�2B�fB�LB�2B�^B�0B�DB��B�wB�yB(
B~(B��B�XB�xB�?B	/�B	QhB	e�B	��B	��B	�QB	��B	�B	�*B	��B	ٴB
�B
7�B
�iB
�1B
�DB�BL~BB�Bl=B��B��B�eB�/B��B��B�^B�>B�@B��B�B��B��B�iB�B�B��B��B�B��B��B��B�tB��B�Bs�BO�B�B
��B
�TB
~�B
]B
D�B
9�B
2|B
�B	�B	��B	ϫB	�$B	�KB	�jB	�iB	ncB	m�B	�;B	��B	��B	��B	��B	��B	�%B	�B	qAB	SuB	9�B	VB�KB��B�B�B�B	-�B	1�B	9rB	7�B	-B	B	�B��B��B�!B֡B��B� B��BðB�B��B�zB�RBˬB��B��BյB�MB�kB�B�?B�$B	(B	!|B	.�B	DgB	R�B	e�B	r-B	��B	�B	�*B	B	ɺB	��B	��B	ϑB	��B	��B	�xB	��B	�[B	�.B	��B	�+B	��B	�qB	��B	��B	�B	�LB	��B	��B	��B	�@B	��B	��B	��B	��B	��B	�tB	�,B	�B	��B	��B	�8B	�XB	�WB	�KB	�sB	��B	��B	�
B	�>B	�>B	�eB	�KB	��B	��B	�B	�=B	��B	��B	�cB	�cB	�/B	�CB	�CB	��B	��B	��B	�OB	��B	��B	�!B	��B	�aB	�-B	�-B	��B	��B	�nB	��B	��B	��B	�B	��B	��B	��B	�HB	��B	��B	�PB	�<B	��B	�VB	�B	�<B	�<B	�"B	�<B	��B	�(B	�.B	�B	�AB	��B	�mB	�_B	ǮB	��B	��B	��B	�KB	�KB	�KB	�B	ɆB	�rB	�XB	ʦB	�~B	�B	�B	��B	�\B	��B	�}B	�B	�4B	�NB	�NB	�B	��B	�:B	��B	��B	�&B	�&B	�oB	ңB	ңB	҉B	��B	�hB	��B	�oB	�oB	҉B	ѷB	ңB	ӏB	ԯB	�aB	ԕB	ԯB	�B	��B	�YB	�B	�B	�7B	�#B	�#B	��B	�qB	�#B	�CB	ܒB	��B	�IB	�5B	�B	�VB	�pB	�;B	�;B	�;B	߾B	��B	�B	��B	�B	�B	�tB	�ZB	��B	�,B	�B	��B	��B	��B	�B	��B	�:B	��B	�VB	߾B	�pB	��B	�-B	��B	�B	�B	��B	�B	��B	�oB	�B	�?B	��B	�tB	��B	�tB	�ZB	��B	��B	�LB	�fB	��B	��B	�B	�RB	��B	��B	�>B	�XB	��B	�*B	�xB	�xB	��B	��B	�B	��B	��B	�B	�VB	�]B	��B	��B	�.B	�HB	�}B	�}B	��B
 �B
 �B
�B
[B
�B
�B
{B
aB
GB
-B
-B
�B
-B
�B
�B
-B
�B
%B
�B
�B
�B
�B
KB
�B
	�B
	�B
	�B

rB

�B
^B
DB
�B
JB
dB
�B
�B
~B
�B
�B
dB
B
�B
~B
6B
�B
�B
�B
�B
jB
�B
�B
�B
�B
B
�B
(B
BB
�B
�B
�B
�B
(B
\B
�B
HB
}B
}B
}B
�B
.B
B
�B
�B
:B
�B
{B
�B
�B
B
MB
2B
�B
�B
SB
mB
�B
�B
SB
�B
�B
pB
�B
�B
�B
�B
�B
�B
�B
B
�B
}B
�B
B
hB
:B
oB
�B
&B
uB
�B
,B
�B
2B
gB
�B
gB
�B
�B
�B
?B
?B
YB
�B
�B
�B
�B
�B
�B
B
�B
�B
qB
B
�B
~B
�B
OB
�B
�B
�B
!B
�B
 vB
 vB
 \B
 B
�B
�B
�B
!B
VB
 \B
 �B
!|B
!�B
!�B
!�B
"B
!�B
#nB
#�B
#TB
#B
#B
"�B
"�B
"�B
$B
$@B
$�B
$�B
%FB
%,B
%zB
%zB
%�B
&B
&2B
&2B
&LB
&�B
&�B
&�B
'�B
'�B
($B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)*B
)*B
)*B
)B
)B
(�B
)_B
)�B
)�B
*B
*eB
*eB
*�B
+B
+kB
+�B
,�B
,�B
,�B
,�B
-CB
-wB
-�B
-�B
.B
./B
.}B
.�B
.�B
/ B
/ B
/ B
/OB
/iB
/�B
/�B
/�B
0;B
0�B
0�B
0�B
1'B
1AB
1AB
2aB
3�B
3�B
4TB
5B
5B
5%B
5ZB
5ZB
5tB
5tB
5�B
6B
6+B
6B
5�B
5�B
6�B
7�B
8RB
8lB
8�B
9$B
9XB
9XB
9>B
9�B
:*B
:�B
:�B
;JB
;B
<�B
<�B
<�B
<�B
="B
=B
=�B
=VB
>BB
>�B
>�B
>�B
?.B
?�B
@�B
@�B
@�B
AUB
A�B
A�B
A�B
A�B
BB
BuB
BuB
CGB
C�B
C�B
DMB
DB
D3B
D�B
D�B
EB
E9B
ESB
E�B
E�B
FB
FYB
FtB
G+B
G�B
G�B
HB
HB
H1B
H�B
H�B
H�B
H�B
IB
IB
IB
I�B
I�B
J	B
J=B
J�B
J�B
J�B
KB
KDB
K)B
K�B
K�B
LB
LJB
L0B
L0B
LB
L~B
L~B
MB
M6B
MjB
M�B
M�B
M�B
M�B
NpB
N�B
N�B
N�B
N�B
N�B
O\B
PHB
PHB
P}B
P}B
P�B
P�B
P�B
QhB
QNB
Q�B
Q�B
Q�B
R:B
RoB
R�B
R�B
R�B
R�B
R�B
S&B
S[B
SuB
S[B
S�B
S�B
S�B
S�B
TFB
TFB
T,B
TaB
T�B
T�B
T�B
UB
UB
U�B
VB
V�B
V�B
V�B
W$B
W?B
WYB
WsB
W�B
W�B
WsB
W�B
XyB
X�B
X�B
X�B
YB
Y1B
Y�B
Y�B
Y�B
ZB
Z7B
ZQB
Z�B
Z�B
Z�B
[#B
[WB
[�B
\CB
\CB
\CB
\]B
\xB
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]B
]IB
]~B
^B
^5B
^OB
^�B
^�B
^�B
_!B
_VB
_;B
_�B
`B
`B
`'B
`BB
`vB
`�B
`�B
`�B
a-B
aB
a�B
a�B
a�B
a�B
b4B
b�B
b�B
b�B
cTB
c�B
c�B
c�B
d@B
d@B
dtB
d�B
d�B
eB
e,B
eFB
eB
e`B
e�B
fLB
ffB
ffB
f�B
f�B
gB
gRB
g�B
g�B
g�B
g�B
hXB
hsB
h�B
h�B
i*B
iB
iB
i*B
i_B
i�B
jKB
j�B
j�B
j�B
kkB
kkB
k�B
l"B
l"B
l"B
l=B
l�B
mB
m]B
mwB
m�B
m�B
m�B
n/B
nIB
ncB
n}B
n�B
n�B
n�B
oB
oOB
oOB
oiB
o�B
o�B
o�B
pB
p!B
p!B
pB
pB
p�B
q'B
qvB
q�B
q�B
q�B
q�B
q�B
rB
r-B
r-B
rGB
rGB
raB
r�B
r�B
s3B
sMB
s�B
tB
tB
t9B
t9B
tnB
tnB
tnB
t�B
t�B
uB
uZB
uZB
u�B
u�B
u�B
u�B
u�B
u�B
vB
v�B
v�B
v�B
v�B
v�B
wfB
wfB
wfB
w�B
w�B
w�B
xB
x8B
x�B
x�B
x�B
y$B
y$B
yXB
y�B
y�B
y�B
zB
z*B
z^B
z�B
z�B
z�B
z�B
z�B
{JB
{JB
{JB
{B
{�B
{B
|6B
|jB
|�B
|�B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�B��B�zB��B�LB��B�fB�fB�fB�2B�2B��B��B��B��B��B��B��B�2B�fB�LB�2B�^B�0B�DB��B�wB�yB(
B~(B��B�XB�xB�?B	/�B	QhB	e�B	��B	��B	�QB	��B	�B	�*B	��B	ٴB
�B
7�B
�iB
�1B
�DB�BL~BB�Bl=B��B��B�eB�/B��B��B�^B�>B�@B��B�B��B��B�iB�B�B��B��B�B��B��B��B�tB��B�Bs�BO�B�B
��B
�TB
~�B
]B
D�B
9�B
2|B
�B	�B	��B	ϫB	�$B	�KB	�jB	�iB	ncB	m�B	�;B	��B	��B	��B	��B	��B	�%B	�B	qAB	SuB	9�B	VB�KB��B�B�B�B	-�B	1�B	9rB	7�B	-B	B	�B��B��B�!B֡B��B� B��BðB�B��B�zB�RBˬB��B��BյB�MB�kB�B�?B�$B	(B	!|B	.�B	DgB	R�B	e�B	r-B	��B	�B	�*B	B	ɺB	��B	��B	ϑB	��B	��B	�xB	��B	�[B	�.B	��B	�+B	��B	�qB	��B	��B	�B	�LB	��B	��B	��B	�@B	��B	��B	��B	��B	��B	�tB	�,B	�B	��B	��B	�8B	�XB	�WB	�KB	�sB	��B	��B	�
B	�>B	�>B	�eB	�KB	��B	��B	�B	�=B	��B	��B	�cB	�cB	�/B	�CB	�CB	��B	��B	��B	�OB	��B	��B	�!B	��B	�aB	�-B	�-B	��B	��B	�nB	��B	��B	��B	�B	��B	��B	��B	�HB	��B	��B	�PB	�<B	��B	�VB	�B	�<B	�<B	�"B	�<B	��B	�(B	�.B	�B	�AB	��B	�mB	�_B	ǮB	��B	��B	��B	�KB	�KB	�KB	�B	ɆB	�rB	�XB	ʦB	�~B	�B	�B	��B	�\B	��B	�}B	�B	�4B	�NB	�NB	�B	��B	�:B	��B	��B	�&B	�&B	�oB	ңB	ңB	҉B	��B	�hB	��B	�oB	�oB	҉B	ѷB	ңB	ӏB	ԯB	�aB	ԕB	ԯB	�B	��B	�YB	�B	�B	�7B	�#B	�#B	��B	�qB	�#B	�CB	ܒB	��B	�IB	�5B	�B	�VB	�pB	�;B	�;B	�;B	߾B	��B	�B	��B	�B	�B	�tB	�ZB	��B	�,B	�B	��B	��B	��B	�B	��B	�:B	��B	�VB	߾B	�pB	��B	�-B	��B	�B	�B	��B	�B	��B	�oB	�B	�?B	��B	�tB	��B	�tB	�ZB	��B	��B	�LB	�fB	��B	��B	�B	�RB	��B	��B	�>B	�XB	��B	�*B	�xB	�xB	��B	��B	�B	��B	��B	�B	�VB	�]B	��B	��B	�.B	�HB	�}B	�}B	��B
 �B
 �B
�B
[B
�B
�B
{B
aB
GB
-B
-B
�B
-B
�B
�B
-B
�B
%B
�B
�B
�B
�B
KB
�B
	�B
	�B
	�B

rB

�B
^B
DB
�B
JB
dB
�B
�B
~B
�B
�B
dB
B
�B
~B
6B
�B
�B
�B
�B
jB
�B
�B
�B
�B
B
�B
(B
BB
�B
�B
�B
�B
(B
\B
�B
HB
}B
}B
}B
�B
.B
B
�B
�B
:B
�B
{B
�B
�B
B
MB
2B
�B
�B
SB
mB
�B
�B
SB
�B
�B
pB
�B
�B
�B
�B
�B
�B
�B
B
�B
}B
�B
B
hB
:B
oB
�B
&B
uB
�B
,B
�B
2B
gB
�B
gB
�B
�B
�B
?B
?B
YB
�B
�B
�B
�B
�B
�B
B
�B
�B
qB
B
�B
~B
�B
OB
�B
�B
�B
!B
�B
 vB
 vB
 \B
 B
�B
�B
�B
!B
VB
 \B
 �B
!|B
!�B
!�B
!�B
"B
!�B
#nB
#�B
#TB
#B
#B
"�B
"�B
"�B
$B
$@B
$�B
$�B
%FB
%,B
%zB
%zB
%�B
&B
&2B
&2B
&LB
&�B
&�B
&�B
'�B
'�B
($B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)*B
)*B
)*B
)B
)B
(�B
)_B
)�B
)�B
*B
*eB
*eB
*�B
+B
+kB
+�B
,�B
,�B
,�B
,�B
-CB
-wB
-�B
-�B
.B
./B
.}B
.�B
.�B
/ B
/ B
/ B
/OB
/iB
/�B
/�B
/�B
0;B
0�B
0�B
0�B
1'B
1AB
1AB
2aB
3�B
3�B
4TB
5B
5B
5%B
5ZB
5ZB
5tB
5tB
5�B
6B
6+B
6B
5�B
5�B
6�B
7�B
8RB
8lB
8�B
9$B
9XB
9XB
9>B
9�B
:*B
:�B
:�B
;JB
;B
<�B
<�B
<�B
<�B
="B
=B
=�B
=VB
>BB
>�B
>�B
>�B
?.B
?�B
@�B
@�B
@�B
AUB
A�B
A�B
A�B
A�B
BB
BuB
BuB
CGB
C�B
C�B
DMB
DB
D3B
D�B
D�B
EB
E9B
ESB
E�B
E�B
FB
FYB
FtB
G+B
G�B
G�B
HB
HB
H1B
H�B
H�B
H�B
H�B
IB
IB
IB
I�B
I�B
J	B
J=B
J�B
J�B
J�B
KB
KDB
K)B
K�B
K�B
LB
LJB
L0B
L0B
LB
L~B
L~B
MB
M6B
MjB
M�B
M�B
M�B
M�B
NpB
N�B
N�B
N�B
N�B
N�B
O\B
PHB
PHB
P}B
P}B
P�B
P�B
P�B
QhB
QNB
Q�B
Q�B
Q�B
R:B
RoB
R�B
R�B
R�B
R�B
R�B
S&B
S[B
SuB
S[B
S�B
S�B
S�B
S�B
TFB
TFB
T,B
TaB
T�B
T�B
T�B
UB
UB
U�B
VB
V�B
V�B
V�B
W$B
W?B
WYB
WsB
W�B
W�B
WsB
W�B
XyB
X�B
X�B
X�B
YB
Y1B
Y�B
Y�B
Y�B
ZB
Z7B
ZQB
Z�B
Z�B
Z�B
[#B
[WB
[�B
\CB
\CB
\CB
\]B
\xB
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]B
]IB
]~B
^B
^5B
^OB
^�B
^�B
^�B
_!B
_VB
_;B
_�B
`B
`B
`'B
`BB
`vB
`�B
`�B
`�B
a-B
aB
a�B
a�B
a�B
a�B
b4B
b�B
b�B
b�B
cTB
c�B
c�B
c�B
d@B
d@B
dtB
d�B
d�B
eB
e,B
eFB
eB
e`B
e�B
fLB
ffB
ffB
f�B
f�B
gB
gRB
g�B
g�B
g�B
g�B
hXB
hsB
h�B
h�B
i*B
iB
iB
i*B
i_B
i�B
jKB
j�B
j�B
j�B
kkB
kkB
k�B
l"B
l"B
l"B
l=B
l�B
mB
m]B
mwB
m�B
m�B
m�B
n/B
nIB
ncB
n}B
n�B
n�B
n�B
oB
oOB
oOB
oiB
o�B
o�B
o�B
pB
p!B
p!B
pB
pB
p�B
q'B
qvB
q�B
q�B
q�B
q�B
q�B
rB
r-B
r-B
rGB
rGB
raB
r�B
r�B
s3B
sMB
s�B
tB
tB
t9B
t9B
tnB
tnB
tnB
t�B
t�B
uB
uZB
uZB
u�B
u�B
u�B
u�B
u�B
u�B
vB
v�B
v�B
v�B
v�B
v�B
wfB
wfB
wfB
w�B
w�B
w�B
xB
x8B
x�B
x�B
x�B
y$B
y$B
yXB
y�B
y�B
y�B
zB
z*B
z^B
z�B
z�B
z�B
z�B
z�B
{JB
{JB
{JB
{B
{�B
{B
|6B
|jB
|�B
|�B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105235  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191747  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191748  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191748                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041755  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041755  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                