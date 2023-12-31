CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-11T00:35:36Z creation;2018-05-11T00:35:40Z conversion to V3.1;2019-12-19T07:38:38Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180511003536  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_239                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�a��ۀ1   @�a��[�@4�9XbN�dL@��4n1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр DѼ�D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @5�@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D��D��D{�D��D{�D��D	��D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�DѺ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�oA�{A�VA��`A��#A���A���A���A���A���A���A���A���A���A���A���AŬAŏ\A�v�A�dZA�VA�n�A�`BA�bNA�dZA�`BA�`BA�K�A���A�p�A���A´9A��A���A�;dA�JA�p�A���A��9A�\)A�1A��mA��^A�dZA�JA��^A�ffA�I�A��A��RA�A�A���A�~�A���A�I�A��mA���A���A���A�bNA�^5A��PA���A��PA��uA�t�A�{A��HA��A���A��A��mA��A�ĜA�7LA�XA� �A��HA�$�A�1A�5?A�A�A�9XA���A�+A�E�A���A��FA�XA��A�9XA�K�A���A�$�A��A�bA��TA�\)A�t�A���A�
=A���A�O�A�5?A��#A�p�A�z�A�S�A��/A��;A���A��A�G�A���A�~�A�-A�1AG�A~�HA~n�A}�AyS�Au;dAs/AqAo�TAm��Al�Al��Al1'AjE�Ah��Ah1Ae�
Ab��A`ZA_"�A[��AY|�AXȴAW/AT�`AS�-AQ`BAN�yAM�TAL�uAJ��AI�
AI�AGt�AF$�AE�ACK�AB�+AA?}A??}A>A�A=��A<�9A;�7A;�A:�uA8��A6JA4jA3t�A3"�A2�/A2z�A1XA/�mA.v�A-VA,��A,�RA,z�A*��A(�yA'�hA'�A&1A$-A#G�A"��A!�A Q�An�A�A  A��AA�;A�A��AA�AXAbA��A�7A��AbA�DAI�A�mAx�A�AjA�yAVA-A+A	�PA	�A��Az�A��A��AffA�A�TA+A ��@���@�ff@��j@��P@���@��+@��7@��y@�$�@�`B@���@��;@�p�@� �@�!@�V@��T@�V@��m@�|�@�+@�$�@�-@��`@�A�@��@�hs@�u@㕁@���@�1'@ߕ�@�o@�n�@���@�?}@ܼj@ܣ�@ܓu@ܣ�@ܬ@ۍP@ش9@׶F@�{@ԓu@Ӯ@��@�M�@��@Ͳ-@́@�`B@�b@ʧ�@�X@�%@�bN@�M�@���@��@�|�@�+@�ȴ@�n�@��h@�7L@��`@�Q�@���@�l�@�j@�K�@��!@��@�`B@�%@���@��@��@�33@���@��@���@��@�hs@�?}@�Ĝ@��
@�|�@���@��R@�E�@���@���@��9@��@�dZ@��@�n�@���@�(�@���@�ȴ@���@�G�@���@��`@��9@��F@���@�S�@���@���@�{@���@��^@���@�G�@�bN@�A�@�(�@��@��m@��F@�"�@�ȴ@���@�5?@�J@��@��@�?}@���@�z�@�z�@�j@�1'@��
@��@��P@�t�@�33@�
=@��!@�ff@�=q@�{@�J@���@�-@�5?@���@��T@��7@�Z@�9X@��F@�"�@���@��!@�~�@�5?@��@�J@��^@�hs@�X@��@���@�r�@�(�@�Z@��@�x�@�j@�z�@��@�1'@�1@���@��F@��P@�|�@�dZ@�ȴ@���@�V@�J@�?}@�I�@� �@�  @�  @���@�@��R@���@��!@���@�M�@���@�Ĝ@�9X@���@�\)@�K�@�@���@�^5@�@��#@��^@���@�x�@�`B@�/@��j@��@���@�A�@�(�@��w@���@�l�@�33@��H@��+@�~�@�^5@�M�@��@��@���@�{@��#@�&�@��`@��@�I�@��@��;@��w@��@�C�@�+@��@�
=@��y@��!@��\@�M�@�5?@�$�@���@���@�x�@�?}@���@��/@���@���@��u@��D@�j@�1'@��@�33@���@��R@���@��\@�M�@�{@���@���@�p�@��@���@���@��@��u@�r�@�I�@��;@�ƨ@�ƨ@��w@��w@��@���@��@�t�@�l�@�dZ@�"�@���@���@�^5@�V@�M�@�$�@�J@�J@��@���@���@��-@�x�@�?}@��@���@���@��@�bN@�9X@�@~��@~�R@~�R@~��@~��@~5?@~{@~@}�@}��@}��@}�@|��@|�@{"�@zn�@yG�@x��@xQ�@w�@w;d@v�R@vv�@vV@u�T@up�@t�@t�j@t��@t(�@s�
@s�@sC�@s@r�H@r�\@q�^@qx�@p��@pr�@p  @o�P@o
=@nv�@m��@l�@l�D@k�
@kdZ@jM�@i��@i��@ihs@i7L@i&�@i%@h�9@hA�@g��@g;d@f�@f�R@f�+@f{@e�-@ep�@eO�@e�@d��@d�j@d9X@d1@ct�@b��@b=q@a��@a�^@`��@`�@`A�@_�;@_�w@_\)@^�+@^$�@]��@]�h@]�@]�@]p�@]?}@\�@\(�@[�m@[�F@[t�@Zn�@Y�^@YG�@Xr�@X �@X  @W�w@W�@W|�@W\)@W�@V�y@W
=@W
=@W
=@V�@Vȴ@V��@V�+@V��@V�+@Vff@U�@Up�@T�@T�D@Tj@Tj@TZ@S��@SdZ@S@R�!@R^5@RJ@Q��@Qx�@QX@QG�@Q�@Pr�@P �@O�@O��@O�w@O�@O\)@N��@Nff@N$�@M��@M�@M�@L�/@L�j@L��@L�@K�m@Kƨ@Kƨ@Kƨ@K�F@KS�@Ko@J�@J�\@I�@I7L@H��@HbN@G��@GK�@G+@F�@FE�@E��@E�@D��@D�j@Dz�@DZ@D�@C�m@Cƨ@C@B��@B�\@BM�@A��@Ahs@A�@@�9@@�@@  @?�;@?�w@?�P@?\)@?�@>��@>�+@>ff@>$�@=�@=?}@<�@<Z@<1@;��@;t�@;S�@;"�@:��@:~�@:=q@9��@9x�@9&�@9%@8�`@8�9@8��@8r�@7��@7l�@7;d@7+@7�@7
=@6ȴ@6V@6$�@5��@5�h@5?}@4�@4��@4�D@4�@3ƨ@3�@3�@3dZ@3S�@333@2��@2^5@2�@2J@1��@1�#@1��@1��@1hs@1%@0�@0b@0  @0  @/�@/��@/\)@/K�@/
=@.ȴ@.�R@.v�@.{@-�-@-��@-�@,�j@,�D@,Z@,Z@,Z@,Z@,I�@+��@+�
@+��@+t�@+o@*�H@*��@*��@*~�@)�@)��@)X@)&�@)%@(��@(�`@(��@(Ĝ@(�9@(�9@(��@(Q�@( �@(  @'��@'��@'��@'|�@'+@'
=@&�y@&�y@&�y@&�y@&�@&ȴ@&��@&v�@&ff@&V@&$�@%��@%@%�-@%�@%`B@%V@$�@$I�@$(�@$1@#�
@#dZ@#o@#@"�H@"��@"��@"n�@"M�@"J@!�@!�#@!�^@!��@!�7@!hs@!X@!G�@!�@ ��@ �`@ �`@ ��@ �`@ �`@ ��@ �u@ A�@ 1'@  �@�@��@�w@��@ȴ@��@�+@$�@{@@�T@��@�-@�@O�@/@/@�@�@�/@��@�@��@I�@��@ƨ@��@t�@dZ@C�@"�@�@��@~�@n�@n�@n�@-@�#@��@��@��@�9@�9@r�@Q�@1'@b@�;@�@�P@l�@
=@�@�+@ff@V@�@��@V@�@�/@�j@�D@z�@z�@z�@Z@��@�@dZ@C�@C�@C�@o@M�@-@�@�@��@�^@�^@��@��@x�@hs@&�@��@�9@��@A�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�oA�{A�VA��`A��#A���A���A���A���A���A���A���A���A���A���A���AŬAŏ\A�v�A�dZA�VA�n�A�`BA�bNA�dZA�`BA�`BA�K�A���A�p�A���A´9A��A���A�;dA�JA�p�A���A��9A�\)A�1A��mA��^A�dZA�JA��^A�ffA�I�A��A��RA�A�A���A�~�A���A�I�A��mA���A���A���A�bNA�^5A��PA���A��PA��uA�t�A�{A��HA��A���A��A��mA��A�ĜA�7LA�XA� �A��HA�$�A�1A�5?A�A�A�9XA���A�+A�E�A���A��FA�XA��A�9XA�K�A���A�$�A��A�bA��TA�\)A�t�A���A�
=A���A�O�A�5?A��#A�p�A�z�A�S�A��/A��;A���A��A�G�A���A�~�A�-A�1AG�A~�HA~n�A}�AyS�Au;dAs/AqAo�TAm��Al�Al��Al1'AjE�Ah��Ah1Ae�
Ab��A`ZA_"�A[��AY|�AXȴAW/AT�`AS�-AQ`BAN�yAM�TAL�uAJ��AI�
AI�AGt�AF$�AE�ACK�AB�+AA?}A??}A>A�A=��A<�9A;�7A;�A:�uA8��A6JA4jA3t�A3"�A2�/A2z�A1XA/�mA.v�A-VA,��A,�RA,z�A*��A(�yA'�hA'�A&1A$-A#G�A"��A!�A Q�An�A�A  A��AA�;A�A��AA�AXAbA��A�7A��AbA�DAI�A�mAx�A�AjA�yAVA-A+A	�PA	�A��Az�A��A��AffA�A�TA+A ��@���@�ff@��j@��P@���@��+@��7@��y@�$�@�`B@���@��;@�p�@� �@�!@�V@��T@�V@��m@�|�@�+@�$�@�-@��`@�A�@��@�hs@�u@㕁@���@�1'@ߕ�@�o@�n�@���@�?}@ܼj@ܣ�@ܓu@ܣ�@ܬ@ۍP@ش9@׶F@�{@ԓu@Ӯ@��@�M�@��@Ͳ-@́@�`B@�b@ʧ�@�X@�%@�bN@�M�@���@��@�|�@�+@�ȴ@�n�@��h@�7L@��`@�Q�@���@�l�@�j@�K�@��!@��@�`B@�%@���@��@��@�33@���@��@���@��@�hs@�?}@�Ĝ@��
@�|�@���@��R@�E�@���@���@��9@��@�dZ@��@�n�@���@�(�@���@�ȴ@���@�G�@���@��`@��9@��F@���@�S�@���@���@�{@���@��^@���@�G�@�bN@�A�@�(�@��@��m@��F@�"�@�ȴ@���@�5?@�J@��@��@�?}@���@�z�@�z�@�j@�1'@��
@��@��P@�t�@�33@�
=@��!@�ff@�=q@�{@�J@���@�-@�5?@���@��T@��7@�Z@�9X@��F@�"�@���@��!@�~�@�5?@��@�J@��^@�hs@�X@��@���@�r�@�(�@�Z@��@�x�@�j@�z�@��@�1'@�1@���@��F@��P@�|�@�dZ@�ȴ@���@�V@�J@�?}@�I�@� �@�  @�  @���@�@��R@���@��!@���@�M�@���@�Ĝ@�9X@���@�\)@�K�@�@���@�^5@�@��#@��^@���@�x�@�`B@�/@��j@��@���@�A�@�(�@��w@���@�l�@�33@��H@��+@�~�@�^5@�M�@��@��@���@�{@��#@�&�@��`@��@�I�@��@��;@��w@��@�C�@�+@��@�
=@��y@��!@��\@�M�@�5?@�$�@���@���@�x�@�?}@���@��/@���@���@��u@��D@�j@�1'@��@�33@���@��R@���@��\@�M�@�{@���@���@�p�@��@���@���@��@��u@�r�@�I�@��;@�ƨ@�ƨ@��w@��w@��@���@��@�t�@�l�@�dZ@�"�@���@���@�^5@�V@�M�@�$�@�J@�J@��@���@���@��-@�x�@�?}@��@���@���@��@�bN@�9X@�@~��@~�R@~�R@~��@~��@~5?@~{@~@}�@}��@}��@}�@|��@|�@{"�@zn�@yG�@x��@xQ�@w�@w;d@v�R@vv�@vV@u�T@up�@t�@t�j@t��@t(�@s�
@s�@sC�@s@r�H@r�\@q�^@qx�@p��@pr�@p  @o�P@o
=@nv�@m��@l�@l�D@k�
@kdZ@jM�@i��@i��@ihs@i7L@i&�@i%@h�9@hA�@g��@g;d@f�@f�R@f�+@f{@e�-@ep�@eO�@e�@d��@d�j@d9X@d1@ct�@b��@b=q@a��@a�^@`��@`�@`A�@_�;@_�w@_\)@^�+@^$�@]��@]�h@]�@]�@]p�@]?}@\�@\(�@[�m@[�F@[t�@Zn�@Y�^@YG�@Xr�@X �@X  @W�w@W�@W|�@W\)@W�@V�y@W
=@W
=@W
=@V�@Vȴ@V��@V�+@V��@V�+@Vff@U�@Up�@T�@T�D@Tj@Tj@TZ@S��@SdZ@S@R�!@R^5@RJ@Q��@Qx�@QX@QG�@Q�@Pr�@P �@O�@O��@O�w@O�@O\)@N��@Nff@N$�@M��@M�@M�@L�/@L�j@L��@L�@K�m@Kƨ@Kƨ@Kƨ@K�F@KS�@Ko@J�@J�\@I�@I7L@H��@HbN@G��@GK�@G+@F�@FE�@E��@E�@D��@D�j@Dz�@DZ@D�@C�m@Cƨ@C@B��@B�\@BM�@A��@Ahs@A�@@�9@@�@@  @?�;@?�w@?�P@?\)@?�@>��@>�+@>ff@>$�@=�@=?}@<�@<Z@<1@;��@;t�@;S�@;"�@:��@:~�@:=q@9��@9x�@9&�@9%@8�`@8�9@8��@8r�@7��@7l�@7;d@7+@7�@7
=@6ȴ@6V@6$�@5��@5�h@5?}@4�@4��@4�D@4�@3ƨ@3�@3�@3dZ@3S�@333@2��@2^5@2�@2J@1��@1�#@1��@1��@1hs@1%@0�@0b@0  @0  @/�@/��@/\)@/K�@/
=@.ȴ@.�R@.v�@.{@-�-@-��@-�@,�j@,�D@,Z@,Z@,Z@,Z@,I�@+��@+�
@+��@+t�@+o@*�H@*��@*��@*~�@)�@)��@)X@)&�@)%@(��@(�`@(��@(Ĝ@(�9@(�9@(��@(Q�@( �@(  @'��@'��@'��@'|�@'+@'
=@&�y@&�y@&�y@&�y@&�@&ȴ@&��@&v�@&ff@&V@&$�@%��@%@%�-@%�@%`B@%V@$�@$I�@$(�@$1@#�
@#dZ@#o@#@"�H@"��@"��@"n�@"M�@"J@!�@!�#@!�^@!��@!�7@!hs@!X@!G�@!�@ ��@ �`@ �`@ ��@ �`@ �`@ ��@ �u@ A�@ 1'@  �@�@��@�w@��@ȴ@��@�+@$�@{@@�T@��@�-@�@O�@/@/@�@�@�/@��@�@��@I�@��@ƨ@��@t�@dZ@C�@"�@�@��@~�@n�@n�@n�@-@�#@��@��@��@�9@�9@r�@Q�@1'@b@�;@�@�P@l�@
=@�@�+@ff@V@�@��@V@�@�/@�j@�D@z�@z�@z�@Z@��@�@dZ@C�@C�@C�@o@M�@-@�@�@��@�^@�^@��@��@x�@hs@&�@��@�9@��@A�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�hB
��B
�B
�jB
��B
��B
��B
��BBDBoB+BhsB�=B�B�TB��B
=B�B�B�B-B>wBC�BH�BH�BF�B?}BbNBhsBo�Bm�Bk�BjBk�B^5BbNBbNBS�BT�B[#BQ�B]/BM�BF�B2-B/B+B"�B%�B$�B�B\B��B�yB�B��B��B�B�B�B�)BɺB��B�?B��B��B�JB�Bs�B^5Bq�Bk�BcTBZBN�BI�B33B�BoB
��B
�B
�BB
��B
�B
��B
��B
�wB
��B
}�B
hsB
hsB
XB
G�B
D�B
7LB
1'B
�B
�B
!�B
 �B
!�B
�B
PB	�B	�)B	�TB	�#B	��B	�wB	��B	�wB	�FB	��B	��B	�uB	|�B	n�B	cTB	`BB	Q�B	D�B	I�B	>wB	 �B	�B	{B	
=B	oB	
=B	B��B��B�B�mB�mB�#B�B��BȴB��B��BŢB�wB��B�dB�B��B��B��B�!B�B��B��B��B��B�bB��B��B�{B�+B}�B~�B�Bt�BdZBr�Bq�BdZB^5BP�BW
B^5B]/BdZBl�BffB_;B[#BT�BI�BN�BO�BYBR�BJ�B\)B]/BZBZBR�BI�BW
B[#BL�BC�BT�BVBP�BE�B5?B@�BI�BL�BS�BVBT�BS�BT�BZB]/B\)BW
BM�B\)B\)B\)BZBQ�BZB]/BdZBdZBcTBbNBiyBffBjBjBhsBgmBbNBgmBjBk�BffBl�Bs�Bv�Bu�Bv�Bu�Bw�B{�B{�B{�Bx�Bq�BjBx�Bz�Bz�B� Bz�B{�B�oB�uB�{B�oB�VB�VB�hB��B��B�oB��B��B��B�B�B�B�B�'B�3B�-B�?B�dBɺBȴB��B��B��B�B�B�
B�)B�5B�;B�;B�TB�fB�fB�`B�TB�NB�yB�B�B�B�B�B��B��B��B��B	  B	  B��B	VB	\B	uB	�B	�B	�B	�B	�B	+B	+B	,B	.B	/B	6FB	9XB	:^B	9XB	:^B	E�B	G�B	H�B	G�B	G�B	E�B	G�B	K�B	L�B	O�B	O�B	P�B	VB	XB	ZB	_;B	^5B	]/B	]/B	`BB	aHB	aHB	dZB	gmB	hsB	jB	m�B	n�B	p�B	p�B	v�B	w�B	v�B	y�B	y�B	v�B	|�B	�B	�B	�B	�+B	�+B	�1B	�=B	�=B	�7B	�JB	�\B	�bB	�hB	�{B	��B	��B	��B	��B	��B	�B	�!B	�B	�'B	�3B	�3B	�FB	�LB	�FB	�9B	�XB	�dB	�dB	�XB	�XB	B	ĜB	ƨB	ŢB	ĜB	ǮB	��B	��B	��B	��B	ȴB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�/B	�#B	�5B	�;B	�;B	�NB	�HB	�`B	�fB	�fB	�fB	�fB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
+B

=B
PB
JB
PB
VB
PB
PB
bB
oB
oB
oB
oB
oB
uB
{B
{B
uB
uB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
 �B
!�B
!�B
"�B
#�B
"�B
#�B
#�B
$�B
$�B
%�B
$�B
#�B
%�B
$�B
$�B
%�B
$�B
%�B
%�B
%�B
$�B
&�B
&�B
&�B
%�B
(�B
)�B
)�B
)�B
+B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
+B
,B
-B
-B
-B
-B
-B
-B
.B
-B
-B
-B
.B
.B
-B
.B
/B
/B
/B
/B
.B
0!B
0!B
1'B
2-B
2-B
2-B
1'B
0!B
/B
1'B
1'B
0!B
.B
.B
0!B
0!B
1'B
2-B
2-B
33B
33B
33B
2-B
33B
49B
49B
49B
33B
49B
5?B
5?B
6FB
6FB
5?B
49B
33B
33B
49B
6FB
6FB
5?B
49B
5?B
6FB
7LB
7LB
8RB
8RB
:^B
:^B
:^B
9XB
8RB
:^B
;dB
;dB
<jB
;dB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
>wB
?}B
>wB
>wB
=qB
>wB
?}B
>wB
=qB
=qB
>wB
>wB
>wB
?}B
A�B
@�B
@�B
A�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
C�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
G�B
I�B
I�B
H�B
G�B
I�B
I�B
H�B
J�B
J�B
K�B
L�B
K�B
K�B
K�B
L�B
L�B
K�B
L�B
N�B
N�B
M�B
N�B
M�B
L�B
M�B
O�B
P�B
P�B
P�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
W
B
VB
W
B
VB
VB
VB
XB
W
B
XB
ZB
ZB
[#B
[#B
[#B
ZB
ZB
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
\)B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
^5B
^5B
^5B
_;B
^5B
^5B
_;B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
_;B
_;B
_;B
^5B
`BB
_;B
_;B
_;B
^5B
^5B
_;B
aHB
aHB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
ffB
e`B
e`B
e`B
dZB
cTB
cTB
e`B
e`B
dZB
dZB
dZB
aHB
dZB
dZB
e`B
dZB
e`B
ffB
e`B
ffB
ffB
ffB
gmB
gmB
hsB
hsB
gmB
gmB
hsB
gmB
hsB
gmB
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
k�B
k�B
jB
iyB
iyB
jB
iyB
hsB
k�B
l�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
k�B
m�B
m�B
n�B
n�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
n�B
o�B
p�B
q�B
q�B
q�B
p�B
n�B
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
r�B
s�B
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�'B
�'B
�UB
�'B
�-B
�B
�-B
�3B
�B
�B
�B
�B
�B
�3B
�-B
��B
��B
��B
�/B
��B
��B
�B
��B
��B'BxBB,"BiDB��B��B�tB��BB1BBpB.�B?.BD3BIBIlBG�BBABb�BiBo�Bn/BlqBk�Bl�BaHBc�Bc�BV�BW�B]/BTB^5BPHBIB6+B1�B-CB$�B&�B%zB�BhB�*B��B�IB��B�B�cB�GB�wB�B�dBÖB��B�KB��B��B��Bw�Ba�Br|Bl�Bd�B\)BPbBK�B6�B#nBgBoB
�FB
��B
�B
��B
��B
�~B
�4B
�nB
�'B
k6B
i�B
Z�B
J=B
F�B
9	B
2�B
�B
�B
"hB
!�B
"�B
�B
�B	�lB	�B	�B	�B	�(B	��B	�UB	�.B	�LB	�B	�YB	��B	�B	rB	ffB	b�B	U�B	G+B	J�B	@�B	#�B	!�B	sB	B	�B	B	B�}B�<B��B�_B�B�dB�qB��B�)B�"BοB�+B� B�[B��B��B��B��B�6B��B��B��B�nB��B�eB� B��B�7B�gB��B��B��B��BvzBf�Bs�Br�BfB`BSuBYB_�B^�Be,Bl�BgB`BB\)BVmBK�BP�BQNBZBTaBL�B\�B]�BZ�BZ�BTFBKxBW�B[�BNpBE�BU�BV�BQ�BGEB7�BBuBKxBNpBT�BV�BVBUBVBZ�B]�B\�BXBOvB\�B\�B\�B[	BSuB[	B^5Bd�Bd�Bd&Bc:Bi�BgBj�BkBi*Bh>BcnBhXBkQBlqBg�Bm�Bt9BwLBvFBw2BvFBxRB|B|B|By$Br�Bl�By�B|6B|B�B|�B}qB��B��B��B��B�vB�vB�TB��B�YB��B��B�B�B�wB��B�wB��B��B��B��B��B��BɆBɆB�vB�oB҉B�SB�mBרBܒBބBߤB��B�B�B�B�B��B�B��B��B��B�B�!B�MB�DB�xB�xB�VB	 �B	 �B�.B	�B	.B	,B	�B	�B	B	B	jB	+B	+QB	,WB	.cB	/�B	6zB	9�B	:�B	9�B	:�B	E�B	G�B	H�B	G�B	G�B	FB	G�B	LB	MB	PB	PB	QNB	VSB	XyB	ZkB	_;B	^OB	]dB	]~B	`\B	abB	a|B	d�B	g�B	h�B	j�B	m�B	n�B	p�B	p�B	v�B	w�B	v�B	z*B	zDB	w�B	}"B	�oB	�oB	�SB	�EB	�_B	�fB	�XB	�rB	��B	��B	�vB	��B	��B	��B	��B	��B	�zB	��B	��B	�B	�!B	��B	�AB	�MB	�hB	�zB	�fB	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	��B	�B	��B	��B	��B	��B	�)B	�RB	�tB	�B	�B	�B	��B	�B	�<B	�HB	�B	�,B	�B	�?B	�1B	�CB	�~B	یB	�jB	�;B	ߤB	�hB	�B	�zB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�*B	�	B	�B	�0B	�6B	�B	�B	�B
 4B
 B	�HB	�<B	�JB	�VB
 OB
UB
GB
-B
[B
GB
GB
9B
SB
_B

rB
jB
~B
�B
pB
�B
�B
�B
�B
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
B
B
B
/B
�B
�B
�B
�B
 �B
!�B
!�B
!B
"B
!�B
"�B
#�B
#B
#�B
$B
$�B
$�B
%�B
%B
$&B
&B
%B
%,B
&B
%,B
&2B
&B
&2B
%,B
'8B
'8B
'8B
&fB
)*B
*B
*0B
*0B
+B
*B
*0B
*KB
*0B
+6B
+B
,"B
,"B
+QB
,=B
-CB
-CB
-CB
-CB
-)B
-]B
./B
-CB
-]B
-]B
./B
./B
-]B
.IB
/5B
/OB
/5B
/OB
.cB
0UB
0UB
1AB
2GB
2-B
2aB
1[B
0UB
/iB
1AB
1AB
0oB
.}B
.}B
0UB
0�B
1AB
2aB
2aB
33B
3hB
3hB
2GB
3hB
49B
49B
4TB
3hB
4TB
5ZB
5tB
6`B
6`B
5ZB
4�B
3hB
3hB
4TB
6`B
6`B
5tB
4�B
5�B
6zB
7fB
7�B
8lB
8�B
:xB
:xB
:xB
9rB
8�B
:xB
;�B
;�B
<�B
;B
:�B
:�B
:�B
;�B
;B
;B
;�B
<�B
<�B
<�B
<�B
=�B
>�B
?}B
>wB
>�B
=�B
>�B
?�B
>�B
=�B
=�B
>�B
>�B
>�B
?�B
A�B
@�B
@�B
A�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
C�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
G�B
I�B
I�B
H�B
G�B
I�B
I�B
IB
J�B
J�B
K�B
L�B
K�B
K�B
K�B
L�B
L�B
K�B
MB
N�B
OB
M�B
N�B
NB
MB
M�B
O�B
Q B
Q B
QB
O�B
PB
Q B
Q B
Q B
Q B
QB
RB
RB
R B
S&B
SB
TB
T,B
T,B
TB
S@B
T,B
TB
T�B
T�B
UB
U2B
UB
UB
U2B
U2B
VB
W$B
W
B
W$B
W?B
V9B
W?B
W$B
VB
W$B
V9B
V9B
VSB
X+B
W?B
XEB
ZQB
ZQB
[#B
[#B
[=B
ZQB
Z7B
[WB
[=B
[WB
[=B
[=B
\)B
\]B
\CB
[WB
\CB
\CB
]IB
^5B
^5B
^5B
^OB
^OB
_;B
_VB
^OB
^OB
^OB
_pB
^OB
^jB
_;B
^OB
^OB
^OB
_;B
_;B
_;B
_;B
_VB
_;B
^OB
^OB
_;B
_pB
_VB
^jB
`\B
_VB
_VB
_VB
^jB
^jB
_VB
aHB
abB
`vB
`�B
a|B
bNB
bhB
b�B
bhB
bhB
bhB
b�B
cnB
dZB
dtB
dZB
dtB
dtB
dtB
dtB
d�B
d�B
ezB
ffB
e`B
e`B
e`B
dtB
cnB
c�B
ezB
e`B
dtB
dtB
d�B
a�B
d�B
dtB
e�B
d�B
ezB
ffB
e�B
ffB
f�B
f�B
g�B
g�B
hsB
h�B
g�B
g�B
hsB
g�B
hsB
g�B
g�B
h�B
i�B
i�B
iyB
i�B
i�B
i�B
i�B
i�B
k�B
k�B
j�B
i�B
i�B
jB
i�B
h�B
k�B
l�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
k�B
m�B
m�B
n�B
n�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
n�B
o�B
p�B
q�B
q�B
q�B
p�B
n�B
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
r�B
s�B
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805150033422018051500334220180515003342201806221330212018062213302120180622133021201806042132462018060421324620180604213246  JA  ARFMdecpA19c                                                                20180511093517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180511003536  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180511003538  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180511003539  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180511003539  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180511003539  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180511003539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180511003539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180511003540  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180511003540                      G�O�G�O�G�O�                JA  ARUP                                                                        20180511005725                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180511153624  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180514153342  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180514153342  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604123246  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622043021  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                