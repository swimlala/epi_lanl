CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-17T00:35:23Z creation;2018-05-17T00:35:29Z conversion to V3.1;2019-12-19T07:42:11Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180517003523  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_240                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�c3���1   @�c5	{B�@:8�p:��dT%��1�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�C3DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D�|�D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D� D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @;�@{�@�@�A�HA=G�A^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C@�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
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
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&��D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�@�DՀ�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�z�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D��D��D���D�=�D�}�D序D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D뺏D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D��D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~�A�p�A�bNA�E�A�-A���A�n�A�O�A�C�A�/A�(�A�VA�z�A�~�A�E�A�?}A�&�A��A�VA�JA�JA���A���A���A��#A���A�+A�%A���A���A��
A�=qA��A���A�ƨA��A���A��A�M�A�VA���A��A�-A��A�  A�t�A�n�A���A�M�A��FA�M�A���A���A�p�A�bA�ffA�l�A�9XA��A��jA��\A�t�A�bNA��A�K�A�dZA�  A�;dA���A���A��A���A�n�A�oA��A���A��7A�S�A��HA�oA��A�XA�=qA��uA�bA��mA�A�VA�A~�RA~bA}O�Ax�uAv�AtQ�Ar��Ar$�Ao�wAk�PAj�AiK�AgAehsAcXAa�#Aa�A`��A`�A^�\A^Q�A]K�A\z�A[��AZȴAZ�AYl�AY&�AYAXz�AX�AW�
AW��AW�PAW?}AV��AU"�AS�AR1'AP{ANVAM"�AK�^AK\)AK+AJ��AI��AI�AH��AHJAG&�AF�jAF^5AE�AD�AD~�AC��AB��AA�A?�TA?�A>$�A<v�A;��A;�A:�+A9�A933A8�A8Q�A8$�A7�;A7O�A6��A5�A5��A5`BA5�A41'A3��A3�A37LA2�!A2�A1oA/hsA.�A-33A+�A*�`A*��A)�A(��A'�wA'��A'��A't�A'
=A%��A$�HA$v�A#�FA"5?A!�A ��A =qA\)A��A33A��A��A�\A �AA/A1'A��A�A�jAn�AQ�A5?A+A��A�jA��A�/AQ�A�yA�
A��A;dA{A1'A
�/A
�+A	|�A�HA �A
=A�RA�wA��AO�A��A-A/A9XA�FAK�A �HA  �@��F@��!@�%@��@�
=@�n�@�$�@��T@���@�Z@��
@�;d@�~�@�Ĝ@��y@�O�@�bN@�;d@�$�@���@�@�K�@�+@�bN@�$�@�7L@�@�dZ@�b@ݲ-@ܣ�@��@ۮ@��@��#@�V@׾w@�5?@�X@�/@��H@��@�?}@�33@Η�@��@�b@˅@�ȴ@���@�9X@�+@�hs@�b@��m@��
@�ƨ@î@�dZ@��@�&�@�Ĝ@���@�ƨ@�5?@�1@�K�@���@�ȴ@�5?@�/@��j@�I�@���@���@�v�@��@�%@�9X@��m@��
@�ƨ@��@�C�@��\@��h@�1@�@��7@�(�@�b@�  @���@�\)@�@��P@���@��@�7L@�r�@��@���@��@�t�@�+@���@�M�@���@�&�@��/@��@�z�@�1'@���@���@��\@��7@���@�A�@� �@��
@�S�@��y@�^5@��h@��@��/@��@�A�@���@�ȴ@���@���@�`B@�G�@�/@���@��j@��D@���@��R@�5?@��@��-@�O�@��`@�r�@��@���@�t�@�o@�^5@��@���@�G�@�j@���@�t�@��@��H@��+@�M�@�5?@�J@��^@���@�hs@�/@��`@�r�@�I�@�1'@���@���@�l�@�"�@���@�ȴ@�=q@��h@�O�@�/@��/@��`@���@���@���@��D@�@~��@~ȴ@~��@~�+@~ff@~V@~E�@~@}��@}�-@}�-@}�-@}�-@}�h@}?}@}V@|�D@|j@{�m@{ƨ@{�F@{dZ@{o@z��@z��@z^5@z-@zJ@y��@y�^@y�^@yX@x�9@xr�@x �@w�@w�;@w�;@w�w@wl�@w\)@wK�@w
=@vff@v$�@u�T@u��@u��@u��@u�@u�h@u�@up�@u?}@t�@t�@tj@tz�@sdZ@q�@q�^@q�^@q�^@q��@q�7@q��@qX@qx�@qX@q7L@pĜ@pbN@o��@nȴ@nV@m��@lz�@l�@k��@k��@k�@kS�@j�!@i�@iG�@h��@h  @g
=@f�@f�R@f�R@fV@e�-@e��@e��@e�@e`B@e?}@c�@b�H@b��@b�!@b��@b�\@bM�@b�@a�^@ahs@`�`@`Q�@_�w@_�@^�@^ȴ@^ȴ@^�+@^ff@^$�@]�@]�-@]�@]`B@]/@]�@\�@\Z@\9X@\�@[�
@[�@Z�!@Z-@Y��@Y��@Yx�@X��@Xr�@XQ�@W�@Wl�@W\)@W+@V��@VV@UO�@T�@T�j@T�@T�@T��@T�D@T�D@Tz�@TI�@T�@S�m@So@R�\@R-@Q�@Q�@Q��@Q��@Qx�@QX@Q�@P�9@P �@O��@O|�@N��@N{@M@M/@L��@K�F@KdZ@J�@J�!@J�\@I�#@IX@I7L@H��@Hr�@H �@G�@Gl�@G+@F�@F$�@E�T@E��@E�@D�/@D�@D��@Dz�@DI�@D1@C��@C��@C�F@CS�@C33@C33@C"�@C@B�@B��@B��@B��@B��@Bn�@B-@A��@Ahs@AX@AX@A&�@A�@@�`@@��@@�@@A�@@  @?�w@?K�@?�@>��@>ȴ@>ȴ@>�+@>V@>@=�-@=`B@=O�@=?}@=�@<�/@<�@<z�@<I�@;��@;ƨ@;t�@:��@:��@:�\@:~�@:=q@9��@9��@9�7@9X@9&�@8�9@8Q�@8A�@81'@81'@7��@7\)@6��@6�+@5�@5��@5��@5�@5?}@5V@4�@4�D@4(�@3�m@3ƨ@3��@333@2�@2��@2��@2~�@2^5@2-@2-@2�@2J@1G�@1%@0�9@0r�@0Q�@0b@/�@/�P@/;d@.��@.v�@.{@-O�@-V@-V@,��@,�@,��@,�j@,�D@,1@+�
@+ƨ@+��@+t�@+S�@+C�@+"�@*�@*�!@*�\@*^5@*-@)�7@(�`@(�u@(r�@(r�@(1'@( �@'�@'�P@&�y@&ȴ@&��@&�+@&ff@&$�@%�@%�T@%�-@%p�@%`B@%O�@%O�@%?}@%?}@%�@$�@$�/@$��@$��@$z�@$z�@$j@#ƨ@#�@#"�@"�!@"M�@!��@!��@!hs@!7L@!&�@!&�@!�@ ��@ �9@ A�@�;@��@\)@�@
=@�y@�+@$�@��@�@O�@?}@V@�j@(�@t�@S�@33@o@�H@��@n�@M�@��@�#@G�@%@Ĝ@�@�@r�@r�@A�@  @�;@�@�P@l�@�@�y@�@ȴ@��@��@�+@v�@V@$�@��@�-@`B@V@�@�/@�@I�@1@1@�
@ƨ@��@C�@�@�H@��@-@�@�#@�^@��@��@�7@X@G�@7L@�@%@%@��@Ĝ@�9@�u@�u@�@Q�@  @��@l�@�@ȴ@�R@��@�+@v�@ff@V@V@V@E�@5?@$�@$�@{@@�@�@��@��@��@`B@V@��@��@�D@j@�@��@t�@t�@dZ@33@33@o@
��@
��@
M�@
�@	�@	��@	�^@	��@	x�@	X@	7L@	%@�`@Ĝ@�9@�@1'@�;@��@��@��@l�@\)@K�@��@�R@��@v�@ff@V@ff@$�@@�T@�-@�@?}@V@��@��@�j@�@�j@�@��@��@��@��@�D@j@I�@I�@I�@(�@�m@�
@�
@��@S�@"�@o@o@�@�!@~�@n�@n�@n�@~�@~�@n�@n�@n�@^5@M�@�@�@X@G�@G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~�A�p�A�bNA�E�A�-A���A�n�A�O�A�C�A�/A�(�A�VA�z�A�~�A�E�A�?}A�&�A��A�VA�JA�JA���A���A���A��#A���A�+A�%A���A���A��
A�=qA��A���A�ƨA��A���A��A�M�A�VA���A��A�-A��A�  A�t�A�n�A���A�M�A��FA�M�A���A���A�p�A�bA�ffA�l�A�9XA��A��jA��\A�t�A�bNA��A�K�A�dZA�  A�;dA���A���A��A���A�n�A�oA��A���A��7A�S�A��HA�oA��A�XA�=qA��uA�bA��mA�A�VA�A~�RG�O�G�O�Ax�uAv�AtQ�Ar��Ar$�Ao�wAk�PAj�AiK�AgAehsAcXAa�#Aa�A`��A`�A^�\A^Q�A]K�A\z�A[��AZȴAZ�AYl�AY&�AYAXz�AX�AW�
AW��AW�PAW?}AV��AU"�AS�AR1'AP{ANVAM"�AK�^AK\)AK+AJ��AI��AI�AH��AHJAG&�AF�jAF^5AE�AD�AD~�AC��AB��AA�A?�TA?�A>$�A<v�A;��A;�A:�+A9�A933A8�A8Q�A8$�A7�;A7O�A6��A5�A5��A5`BA5�A41'A3��A3�A37LA2�!A2�A1oA/hsA.�A-33A+�A*�`A*��A)�A(��A'�wA'��A'��A't�A'
=A%��A$�HA$v�A#�FA"5?A!�A ��A =qA\)A��A33A��A��A�\A �AA/A1'A��A�A�jAn�AQ�A5?A+A��A�jA��A�/AQ�A�yA�
A��A;dA{A1'A
�/A
�+A	|�A�HA �A
=A�RA�wA��AO�A��A-A/A9XA�FAK�A �HA  �@��F@��!@�%@��@�
=@�n�@�$�@��T@���@�Z@��
@�;d@�~�@�Ĝ@��y@�O�@�bN@�;d@�$�@���@�@�K�@�+@�bN@�$�@�7L@�@�dZ@�b@ݲ-@ܣ�@��@ۮ@��@��#@�V@׾w@�5?@�X@�/@��H@��@�?}@�33@Η�@��@�b@˅@�ȴ@���@�9X@�+@�hs@�b@��m@��
@�ƨ@î@�dZ@��@�&�@�Ĝ@���@�ƨ@�5?@�1@�K�@���@�ȴ@�5?@�/@��j@�I�@���@���@�v�@��@�%@�9X@��m@��
@�ƨ@��@�C�@��\@��h@�1@�@��7@�(�@�b@�  @���@�\)@�@��P@���@��@�7L@�r�@��@���@��@�t�@�+@���@�M�@���@�&�@��/@��@�z�@�1'@���@���@��\@��7@���@�A�@� �@��
@�S�@��y@�^5@��h@��@��/@��@�A�@���@�ȴ@���@���@�`B@�G�@�/@���@��j@��D@���@��R@�5?@��@��-@�O�@��`@�r�@��@���@�t�@�o@�^5@��@���@�G�@�j@���@�t�@��@��H@��+@�M�@�5?@�J@��^@���@�hs@�/@��`@�r�@�I�@�1'@���@���@�l�@�"�@���@�ȴ@�=q@��h@�O�@�/@��/@��`@���@���@���@��D@�@~��@~ȴ@~��@~�+@~ff@~V@~E�@~@}��@}�-@}�-@}�-@}�-@}�h@}?}@}V@|�D@|j@{�m@{ƨ@{�F@{dZ@{o@z��@z��@z^5@z-@zJ@y��@y�^@y�^@yX@x�9@xr�@x �@w�@w�;@w�;@w�w@wl�@w\)@wK�@w
=@vff@v$�@u�T@u��@u��@u��@u�@u�h@u�@up�@u?}@t�@t�@tj@tz�@sdZ@q�@q�^@q�^@q�^@q��@q�7@q��@qX@qx�@qX@q7L@pĜ@pbN@o��@nȴ@nV@m��@lz�@l�@k��@k��@k�@kS�@j�!@i�@iG�@h��@h  @g
=@f�@f�R@f�R@fV@e�-@e��@e��@e�@e`B@e?}@c�@b�H@b��@b�!@b��@b�\@bM�@b�@a�^@ahs@`�`@`Q�@_�w@_�@^�@^ȴ@^ȴ@^�+@^ff@^$�@]�@]�-@]�@]`B@]/@]�@\�@\Z@\9X@\�@[�
@[�@Z�!@Z-@Y��@Y��@Yx�@X��@Xr�@XQ�@W�@Wl�@W\)@W+@V��@VV@UO�@T�@T�j@T�@T�@T��@T�D@T�D@Tz�@TI�@T�@S�m@So@R�\@R-@Q�@Q�@Q��@Q��@Qx�@QX@Q�@P�9@P �@O��@O|�@N��@N{@M@M/@L��@K�F@KdZ@J�@J�!@J�\@I�#@IX@I7L@H��@Hr�@H �@G�@Gl�@G+@F�@F$�@E�T@E��@E�@D�/@D�@D��@Dz�@DI�@D1@C��@C��@C�F@CS�@C33@C33@C"�@C@B�@B��@B��@B��@B��@Bn�@B-@A��@Ahs@AX@AX@A&�@A�@@�`@@��@@�@@A�@@  @?�w@?K�@?�@>��@>ȴ@>ȴ@>�+@>V@>@=�-@=`B@=O�@=?}@=�@<�/@<�@<z�@<I�@;��@;ƨ@;t�@:��@:��@:�\@:~�@:=q@9��@9��@9�7@9X@9&�@8�9@8Q�@8A�@81'@81'@7��@7\)@6��@6�+@5�@5��@5��@5�@5?}@5V@4�@4�D@4(�@3�m@3ƨ@3��@333@2�@2��@2��@2~�@2^5@2-@2-@2�@2J@1G�@1%@0�9@0r�@0Q�@0b@/�@/�P@/;d@.��@.v�@.{@-O�@-V@-V@,��@,�@,��@,�j@,�D@,1@+�
@+ƨ@+��@+t�@+S�@+C�@+"�@*�@*�!@*�\@*^5@*-@)�7@(�`@(�u@(r�@(r�@(1'@( �@'�@'�P@&�y@&ȴ@&��@&�+@&ff@&$�@%�@%�T@%�-@%p�@%`B@%O�@%O�@%?}@%?}@%�@$�@$�/@$��@$��@$z�@$z�@$j@#ƨ@#�@#"�@"�!@"M�@!��@!��@!hs@!7L@!&�@!&�@!�@ ��@ �9@ A�@�;@��@\)@�@
=@�y@�+@$�@��@�@O�@?}@V@�j@(�@t�@S�@33@o@�H@��@n�@M�@��@�#@G�@%@Ĝ@�@�@r�@r�@A�@  @�;@�@�P@l�@�@�y@�@ȴ@��@��@�+@v�@V@$�@��@�-@`B@V@�@�/@�@I�@1@1@�
@ƨ@��@C�@�@�H@��@-@�@�#@�^@��@��@�7@X@G�@7L@�@%@%@��@Ĝ@�9@�u@�u@�@Q�@  @��@l�@�@ȴ@�R@��@�+@v�@ff@V@V@V@E�@5?@$�@$�@{@@�@�@��@��@��@`B@V@��@��@�D@j@�@��@t�@t�@dZ@33@33@o@
��@
��@
M�@
�@	�@	��@	�^@	��@	x�@	X@	7L@	%@�`@Ĝ@�9@�@1'@�;@��@��@��@l�@\)@K�@��@�R@��@v�@ff@V@ff@$�@@�T@�-@�@?}@V@��@��@�j@�@�j@�@��@��@��@��@�D@j@I�@I�@I�@(�@�m@�
@�
@��@S�@"�@o@o@�@�!@~�@n�@n�@n�@~�@~�@n�@n�@n�@^5@M�@�@�@X@G�@G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B�B��B��B��B��B�B�B�#BȴB��B�B�B�B�B�B�B�B�
B�B��BȴBÖBɺBɺBƨB��B�3B�9B�jB�dB�LB�-B�B��B�hBy�BE�B��B�B�B��B��B��B��B�B�B�B�
B��B��B�FB��B�PB�bB�1B�hB�bB�=Bz�BbNB1'BD�B5?B+B�B�B�BhB	7B
��B
�B
�B
�yB
�)B
ĜB
��B
�hB
�B
�\B
�7B
�bB
�7B
y�B
m�B
r�B
cTB
O�B
�B
�B
hB
VB

=B	�B	�wB	��B	��B	�qB	��B	��B	��B	�B	��B	��B	�bB	��B	�oB	�DB	�=B	�B	�1B	�%B	�7B	�=B	�B	�B	�B	� B	}�B	t�B	gmB	R�B	J�B	:^B	1'B	(�B	1'B	-B	8RB	:^B	5?B	,B	+B	&�B	�B	�B	�B	�B	{B	VB	\B	1B��B�B�fB�B�B�B�;B�ZB�;B�B�B��B�#B�B��B��B��BÖB��B��BǮB�qB�wBB�qB�9B�B��B�\B�VB�uB�%B��B��B�bB�B�7B��B��B�bB�+Bu�B|�B}�Bx�BhsBs�Br�Bn�BgmB`BBjBp�Bv�Bq�BjBiyBbNB]/B^5BaHBcTBaHBaHB\)BI�B?}BA�BK�BA�BF�B=qB9XBI�BB�B.B#�B'�BA�B33B9XB5?B1'B<jB5?BB�B=qB6FB5?B-B,B5?B5?B5?B,B5?B0!B,B1'B49B7LB8RB8RB2-B2-B33B/B,B �B�B!�B(�B'�B$�B%�B#�B(�B!�B�BuB �B"�B�B	7BuB!�B'�B&�B#�B�B�B�B�B!�B"�B{B�B�BuB �B�B �B$�B!�B�B�B�B�B!�B2-B33B2-B/B,B!�B+B0!B/B(�B!�B!�B5?B:^B;dB7LB5?B;dB;dB:^B8RB>wB<jB:^B<jBD�BG�BG�BF�BB�B>wB;dB8RB=qB>wBD�BVBT�BR�BO�BH�BD�BXB^5B]/B\)B\)B`BBdZBjBw�Bw�Bz�By�B}�B�B�B�%B�B�B�B{�B�B�=B�hB��B�{B�{B��B��B��B��B��B��B��B��B��B��B�!B�9B�FB�LB�FB�FB�LB�9B�3BBɺB��B��B��B��B��B��B��B�
B�B�5B�TB�TB�HB�sB��B��B��B��B��B��B��B��B	B	B	B	B	B	DB	PB	DB	\B	hB	hB	�B	�B	{B	�B	"�B	%�B	%�B	,B	1'B	0!B	.B	-B	-B	/B	5?B	7LB	8RB	8RB	9XB	9XB	9XB	;dB	>wB	?}B	@�B	@�B	A�B	@�B	B�B	B�B	F�B	F�B	I�B	K�B	K�B	M�B	O�B	Q�B	S�B	VB	W
B	XB	YB	ZB	YB	YB	_;B	aHB	cTB	ffB	ffB	ffB	hsB	k�B	l�B	l�B	l�B	q�B	s�B	t�B	v�B	v�B	w�B	y�B	y�B	y�B	y�B	z�B	|�B	~�B	�B	~�B	~�B	�DB	�PB	�PB	�VB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�'B	�B	�B	�!B	�-B	�'B	�3B	�dB	�jB	�qB	�qB	�wB	ĜB	ŢB	ĜB	ǮB	ǮB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�5B	�5B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�BB	�HB	�BB	�;B	�NB	�TB	�NB	�HB	�BB	�ZB	�mB	�yB	�sB	�fB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
+B
B
+B

=B
	7B
	7B
DB
JB
DB
JB
JB
JB
\B
\B
\B
hB
oB
uB
uB
uB
uB
{B
{B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
$�B
"�B
#�B
#�B
#�B
$�B
$�B
#�B
$�B
'�B
'�B
'�B
%�B
%�B
&�B
'�B
'�B
+B
,B
+B
+B
,B
,B
+B
,B
-B
.B
.B
-B
.B
0!B
/B
0!B
0!B
0!B
1'B
1'B
0!B
-B
0!B
1'B
1'B
2-B
2-B
1'B
2-B
1'B
1'B
0!B
2-B
1'B
5?B
7LB
6FB
6FB
6FB
6FB
5?B
49B
6FB
7LB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
6FB
49B
5?B
8RB
:^B
;dB
:^B
:^B
;dB
:^B
9XB
=qB
>wB
=qB
>wB
=qB
>wB
>wB
>wB
>wB
@�B
@�B
A�B
A�B
A�B
@�B
A�B
A�B
B�B
A�B
A�B
B�B
A�B
?}B
A�B
A�B
A�B
A�B
C�B
C�B
D�B
E�B
F�B
F�B
E�B
D�B
D�B
C�B
D�B
E�B
F�B
F�B
G�B
G�B
F�B
F�B
F�B
H�B
H�B
I�B
H�B
G�B
G�B
H�B
L�B
M�B
M�B
M�B
L�B
L�B
M�B
L�B
M�B
L�B
N�B
O�B
O�B
Q�B
Q�B
Q�B
P�B
P�B
P�B
Q�B
R�B
Q�B
Q�B
R�B
S�B
T�B
S�B
T�B
S�B
S�B
S�B
R�B
R�B
S�B
S�B
S�B
VB
VB
VB
T�B
VB
XB
W
B
W
B
W
B
VB
W
B
XB
W
B
W
B
XB
ZB
ZB
[#B
[#B
ZB
ZB
[#B
[#B
[#B
\)B
\)B
[#B
\)B
\)B
\)B
\)B
[#B
ZB
ZB
[#B
ZB
[#B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
^5B
_;B
`BB
aHB
`BB
_;B
_;B
bNB
cTB
cTB
bNB
cTB
bNB
bNB
cTB
bNB
dZB
dZB
e`B
e`B
e`B
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
dZB
e`B
gmB
gmB
gmB
gmB
gmB
hsB
ffB
gmB
hsB
iyB
iyB
jB
l�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
n�B
o�B
o�B
n�B
n�B
n�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
p�B
s�B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�BB�.B�<B�dB��B�%B��B�B�B��B�BܒB��B�gB�7B�KB�KB�QB�B�B�EB�$B�9B�NB�lBāB��B��B��B�'B��B�B��B�B��B�aB�WB�bB�oB{�BJXB�BBB��B�BB��B  B�TB�BںBخB��B��B�B�eB�.B��B��B��B��B��B|Bd�B5BE�B7B,�B�B�BQBoB
=B
�PB
�OB
�GB
�KB
ݲB
��B
�B
��B
��B
��B
�rB
��B
��B
{B
n�B
s�B
e�G�O�G�O�B
5B
B
B
�B	��B	�aB	�}B	�NB	��B	��B	�&B	�dB	��B	��B	��B	�:B	�)B	��B	�dB	�DB	��B	�B	��B	��B	��B	��B	��B	�[B	�OB	~BB	utB	h�B	T�B	L�B	<�B	3�B	+6B	2�B	.�B	8�B	:�B	5�B	-CB	+�B	'�B	 �B	�B	VB	dB	gB	\B	.B		7B��B�ZB�B��B�B�)B�\B�B�'B�	B�$B��B�qB�B՛B͹B͟BĜB�<B�DB�KB��B�.B��B��B�%B�B�VB��B�B��B��B�SB�+B��B��B�XB��B��B��B�Bw�B}�B~�BzBjKBt�Bs�Bo�Bh�Ba�BkkBq'Bv�BrGBk6Bj0BcnB^jB_Ba�Bc�Ba�Ba�B\�BKxBAUBCBL�BC-BG�B?HB:�BJ#BCaB0B&B)�BBB4�B:DB6�B2�B<�B6zBCB>BB7fB6B.}B-CB5�B6B5�B-B5�B1B-CB1�B4�B7�B8�B8�B2�B2�B3�B/�B,�B"4B 'B"�B)�B(�B%�B&�B$�B)�B"�BB�B!�B#�B�B�B�B"�B(sB'mB$�B �B �B�B�B"hB#TB�B~BpB�B!�B�B!|B%`B"�B�B�B�B�B"�B2aB3hB2aB/iB,�B"�B+�B0oB/�B)�B# B#TB5�B:�B;�B7�B6B;�B;�B:�B9$B>�B<�B;B=BD�BG�BG�BF�BCB?HB<PB9�B>BB?�BE�BVBU2BS@BP�BJ	BFtBX�B^�B]�B\�B]/BaHBeFBkQBxBxlB{JBz^B~]B�GB�mB�YB�mB�mB�gB|�B��B��B��B��B��B��B�B�
B�KB�5B�B�:B�&B�TB��B�mB�UB�nB�zB�fB�zB��B��B��B�B��B��B�B�)B�0B�BB�4B�aB�MB�sBخB޸B�B�B��B��B��B�B�B�$B�B�.B�.B�cB	AB	[B	GB	�B	�B	xB	�B	�B	�B	�B	�B	�B	�B	�B	B	#B	&B	&2B	,"B	1'B	0;B	.cB	-wB	-wB	/iB	5tB	7�B	8lB	8lB	9�B	9rB	9rB	;�B	>�B	?�B	@�B	@�B	A�B	@�B	B�B	B�B	F�B	F�B	I�B	K�B	K�B	NB	O�B	RB	TB	V9B	W$B	XEB	YKB	Z7B	YKB	YB	_VB	abB	c�B	ffB	f�B	f�B	h�B	k�B	l�B	l�B	l�B	q�B	s�B	t�B	v�B	v�B	xB	y�B	y�B	zB	y�B	{B	}B	.B	�'B	cB	}B	�^B	�jB	�PB	�pB	��B	��B	��B	��B	�B	��B	� B	�B	�@B	�:B	�B	� B	�4B	�B	�]B	�GB	�GB	�[B	�iB	��B	��B	�aB	��B	��B	�dB	��B	��B	��B	��B	ĜB	żB	��B	��B	��B	�'B	��B	��B	��B	��B	� B	� B	�&B	� B	�FB	�,B	�MB	�SB	�_B	�=B	�OB	�OB	�dB	�OB	�OB	�jB	�VB	�vB	�\B	�\B	�bB	�vB	�pB	�hB	�nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	��B	�*B	�	B	��B	�0B	�*B	�"B	�B	�]B	�.B	�cB
3B
mB
?B
_B
mB
zB

rB
	RB
	�B
xB
~B
xB
~B
~B
�B
�B
vB
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 B
�B
#�B
#�B
$�B
"�B
$B
#�B
#�B
%B
%B
$&B
%B
(
B
(
B
(
B
&2B
&B
'8B
($B
($B
+B
,"B
+B
+B
,=B
,=B
+6B
,"B
-CB
./B
./B
-CB
./B
0;B
/5B
0UB
0UB
0;B
1AB
1AB
0;B
-]B
0;B
1AB
1AB
2GB
2GB
1[B
2GB
1[B
1AB
0UB
2aB
1�B
5ZB
7LB
6`B
6`B
6`B
6`B
5ZB
4nB
6`B
7LB
6zB
6zB
7�B
7�B
7fB
7fB
7fB
7fB
7fB
6`B
4�B
5�B
8lB
:xB
;B
:�B
:xB
;�B
:�B
9�B
=qB
>�B
=�B
>�B
=�B
>�B
>�B
>�B
>�B
@�B
@�B
A�B
A�B
A�B
@�B
A�B
A�B
B�B
A�B
A�B
B�B
A�B
?�B
A�B
A�B
A�B
A�B
C�B
C�B
D�B
E�B
F�B
F�B
E�B
D�B
D�B
C�B
D�B
E�B
F�B
F�B
G�B
G�B
F�B
F�B
F�B
H�B
H�B
I�B
H�B
G�B
G�B
H�B
L�B
M�B
NB
NB
L�B
L�B
M�B
MB
M�B
MB
N�B
PB
O�B
RB
Q�B
Q�B
Q B
Q B
QB
R B
SB
R B
RB
SB
S�B
T�B
TB
T�B
TB
TB
T,B
SB
SB
TB
T,B
TB
VB
VB
V9B
UMB
VB
XB
W$B
W$B
W?B
V9B
W$B
X+B
W$B
WYB
X+B
Z7B
Z7B
[#B
[=B
ZQB
Z7B
[=B
[=B
[=B
\)B
\CB
[WB
\)B
\)B
\CB
\CB
[WB
Z7B
Z7B
[WB
ZkB
[WB
^OB
^5B
^OB
^OB
_;B
_VB
_;B
_;B
_VB
_VB
_;B
_VB
_;B
_;B
_VB
_;B
_;B
_VB
_;B
^OB
^OB
^OB
_VB
`vB
abB
`\B
_pB
_�B
bhB
cTB
cTB
bhB
cTB
b�B
bhB
c�B
b�B
dtB
dtB
ezB
e`B
ezB
dtB
ezB
e�B
ezB
ezB
ezB
ezB
ezB
dtB
e�B
g�B
g�B
gmB
g�B
g�B
hsB
f�B
g�B
h�B
i�B
i�B
jB
l�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
n�B
o�B
o�B
n�B
n�B
n�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
p�B
s�B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805210038462018052100384620180521003846201806221242012018062212420120180622124201201806042120322018060421203220180604212032  JA  ARFMdecpA19c                                                                20180517093517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180517003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180517003526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180517003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180517003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180517003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180517003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180517003528  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180517003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180517003528  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180517003529  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180517003529                      G�O�G�O�G�O�                JA  ARUP                                                                        20180517005721                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180517153448  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180520153846  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180520153846  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604122032  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034201  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                