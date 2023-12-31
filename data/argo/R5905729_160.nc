CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-09-13T09:01:15Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220913090115  20220913090115  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @��SVH�1   @��S��lr@+      �dm�$�/1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D��D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�9XA�7LA�;dA�;dA�?}A�=qA�;dA�5?A�/A�1A�oA�M�A�K�A�A�A�1'A�1'A�33A�/A�/A�-A�$�A��A��A�VAُ\A�~�Aי�Aպ^AӓuA�r�A�-A�ZA�7LA�bAʶFA��A�x�A�5?A�r�A���A��A��+A�A�A��A��+A�~�A��A�v�A�(�A�E�A�  A�C�A�ffA��A���A�r�A���A�(�A�p�A���A��FA�ȴA��yA��`A���A��`A��`A�VA���A���A���A�$�A�hsA��hA�Q�A��A��A�r�A��-A��A��+A��A~^5AyVAt�Ao�Ah �Ac&�A]|�AW"�AT��AO�
AMp�AL��AKXAIdZAE33ACXAA�A=��A;;dA7�A4�`A3A2VA1�TA0��A/�A/�A/+A.��A.�/A.ĜA.I�A,��A+t�A*�A)��A(�`A(�A(ffA'�FA'dZA&�A&��A&�HA&  A%%A#�
A"�HA"=qA!��A!�;A!C�A z�AK�AM�A�AhsA;dA��A9XA��A��A+A�HA�DAQ�A{A?}Av�AVAJA�A�#A�-Ax�AS�A�A��A��AA�AJA�#A�FA�A�AȴA��A��A�+A1'A��A�PA;dA��A��Az�A=qA�A�FA"�A��A�Az�A�AA��A|�AK�A+A"�AoA�AĜA�+A-A��AhsA��A��Ar�A=qA��AS�A
=A
��A
�\A
I�A
1A	��A	l�A��A=qA$�AAƨA�A�A�/A�DAA��A�PA��A�Az�A^5AE�A-A �A�;AhsA�A��A��AbNA�AAp�A7LA �yA ��A r�A 9X@��
@��@�l�@�K�@��@�^5@��7@���@�bN@��m@�dZ@�@�~�@�v�@�ff@�5?@�$�@��T@�`B@��@���@�  @���@�|�@��y@�-@��-@�x�@�hs@�O�@�%@�Z@�@��H@�V@�@�-@�@���@�b@��
@@�33@�E�@�@��@��@�ƨ@�l�@�33@��@�\@�5?@���@陚@��@�u@�(�@�S�@�R@�E�@���@�7@�7L@䛦@�A�@㝲@�
=@�=q@��@��@�1'@�;d@ޟ�@�{@�`B@�G�@��@�(�@�\)@�@��H@ڰ!@�~�@�-@��@�?}@��/@؋D@�1'@��
@�|�@���@�-@���@Չ7@�?}@��/@ԋD@���@��@��@҇+@�{@Ѳ-@���@�A�@υ@�C�@�o@���@·+@�`B@�Ĝ@̓u@��@�+@��@ʰ!@�5?@ɺ^@�hs@��@��/@�z�@��@�"�@Ƈ+@���@�G�@��`@�bN@���@�"�@\@��@��h@�p�@�&�@���@�Z@��@�M�@��T@��7@�p�@�X@�%@��@��9@�Q�@��F@�S�@��@��H@�v�@���@��@�z�@��w@��y@�M�@��@�?}@���@�Ĝ@��@�(�@�\)@��@���@�^5@��T@�p�@���@��u@�Z@���@�@�~�@�E�@��T@��^@���@���@�O�@��@���@��D@�b@��P@��H@��!@��+@�M�@�{@��@��-@��7@�X@�%@��u@��@��@�l�@��@��@���@���@��\@��@���@�O�@��@��@�(�@���@��w@�|�@�;d@�ȴ@�n�@��T@��@�`B@�V@���@��@�Z@�Q�@�ƨ@�
=@��!@�=q@��@�{@��@�hs@�V@���@��u@��@���@��@�l�@�33@��!@�E�@��@��h@��j@��w@�+@�@�ȴ@��R@���@���@�~�@�E�@�J@���@�x�@�O�@�&�@��9@�Z@�(�@��;@��@���@��P@�K�@�o@��@���@�ff@��@���@�?}@�Ĝ@��u@�z�@�Z@� �@��
@���@��@��R@�~�@�$�@���@��@�r�@� �@�1@��;@�t�@�+@��@��y@���@��\@�~�@�n�@�n�@�n�@�-@���@��@��@���@��^@���@��h@�?}@��@��D@�I�@�b@�1@�  @�  @��
@�C�@��H@���@���@��+@�n�@�-@���@��^@�G�@��@��/@��@�A�@�1@��
@��w@��@��@�dZ@��H@��R@���@���@�n�@��@�@�p�@�7L@���@�I�@~��@~{@}O�@|�j@|�@{�
@{dZ@z��@z�@yhs@x�u@w�;@w;d@vV@t��@tI�@s��@s"�@so@r��@q�^@pQ�@o|�@o+@n�@n$�@m�h@m`B@m�@l��@l9X@k�
@k��@k"�@j��@jM�@i�#@i�^@ihs@i&�@hĜ@hr�@g�@g�@g|�@f��@e�T@e�-@e�h@e`B@d�@d��@d�D@d(�@c�m@c��@cS�@c@b��@b~�@b-@a�@`�u@`bN@`bN@`Q�@_�@_�w@_�@_��@_l�@^��@^��@]��@]`B@]O�@]V@\(�@[��@[dZ@[C�@[33@[o@Z�@Z��@Z�\@Z-@Y�@Y�#@Y��@YG�@Y&�@X��@XA�@Xb@X  @W�;@W�@W|�@W+@V�y@V�R@V�+@U�T@U/@T�@TZ@T1@S��@S�F@S@R��@Rn�@R-@Q�7@Q%@PQ�@O�@O\)@O;d@O+@O+@O�@O
=@N��@Nȴ@N��@NE�@N5?@NE�@NE�@N5?@M�-@M`B@MO�@M�@L��@Lj@K�m@KS�@K@J�@Ix�@Ix�@IX@I%@H�@HA�@G��@G
=@F5?@E�@D�D@D(�@Cƨ@B�H@B��@B��@B~�@B^5@B�@A�@A��@A��@A�7@A�@@�9@@��@@Q�@?��@?+@>�R@>�+@>�+@>�+@>V@>{@=��@=�-@=`B@=O�@=�@=/@=/@=V@<��@<��@<(�@<1@<1@;�
@;S�@:��@:^5@9��@9��@9�7@9hs@9X@8��@8bN@81'@8b@7�@7��@7�P@6��@6ff@6E�@6$�@6{@5�T@5�-@5�@5`B@4��@4j@4(�@4(�@4�@3�m@3�F@3��@3S�@2�H@2��@2J@1�7@1hs@1X@1X@1hs@1G�@1�@0�`@0Ĝ@0�9@0�9@0�u@0Q�@/��@/|�@/�@.��@.v�@.{@-��@-��@-�@-`B@-?}@,�@,j@+��@+�@+dZ@+"�@*��@*-@)��@)x�@)%@(�u@(�@(Q�@(  @'��@'l�@'K�@'
=@&�@&v�@&$�@%�T@%��@%��@%��@%@%@%�-@%��@%p�@%O�@%�@$�D@#�m@#�F@#��@#S�@#C�@#o@"�!@"�\@"M�@"-@"�@!��@!��@!�#@!�^@!��@!�7@!hs@ ��@ �`@ ��@ Ĝ@ �u@ A�@  �@  �@��@�@�P@;d@��@�@�R@�+@v�@E�@5?@5?@5?@$�@@�h@O�@/@�@��@�j@j@Z@Z@I�@I�@9X@�@�F@S�@C�@C�@33@�H@��@�!@n�@J@J@��@��@��@��@��@��@�^@��@�7@x�@hs@%@�9@b@��@�P@l�@+@
=@��@�@v�@V@$�@@{@@�-@p�@p�@`B@/@V@��@�@�j@�D@9X@��@�
@�
@ƨ@��@t�@S�@33@"�@�H@��@�!@�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�9XA�7LA�;dA�;dA�?}A�=qA�;dA�5?A�/A�1A�oA�M�A�K�A�A�A�1'A�1'A�33A�/A�/A�-A�$�A��A��A�VAُ\A�~�Aי�Aպ^AӓuA�r�A�-A�ZA�7LA�bAʶFA��A�x�A�5?A�r�A���A��A��+A�A�A��A��+A�~�A��A�v�A�(�A�E�A�  A�C�A�ffA��A���A�r�A���A�(�A�p�A���A��FA�ȴA��yA��`A���A��`A��`A�VA���A���A���A�$�A�hsA��hA�Q�A��A��A�r�A��-A��A��+A��A~^5AyVAt�Ao�Ah �Ac&�A]|�AW"�AT��AO�
AMp�AL��AKXAIdZAE33ACXAA�A=��A;;dA7�A4�`A3A2VA1�TA0��A/�A/�A/+A.��A.�/A.ĜA.I�A,��A+t�A*�A)��A(�`A(�A(ffA'�FA'dZA&�A&��A&�HA&  A%%A#�
A"�HA"=qA!��A!�;A!C�A z�AK�AM�A�AhsA;dA��A9XA��A��A+A�HA�DAQ�A{A?}Av�AVAJA�A�#A�-Ax�AS�A�A��A��AA�AJA�#A�FA�A�AȴA��A��A�+A1'A��A�PA;dA��A��Az�A=qA�A�FA"�A��A�Az�A�AA��A|�AK�A+A"�AoA�AĜA�+A-A��AhsA��A��Ar�A=qA��AS�A
=A
��A
�\A
I�A
1A	��A	l�A��A=qA$�AAƨA�A�A�/A�DAA��A�PA��A�Az�A^5AE�A-A �A�;AhsA�A��A��AbNA�AAp�A7LA �yA ��A r�A 9X@��
@��@�l�@�K�@��@�^5@��7@���@�bN@��m@�dZ@�@�~�@�v�@�ff@�5?@�$�@��T@�`B@��@���@�  @���@�|�@��y@�-@��-@�x�@�hs@�O�@�%@�Z@�@��H@�V@�@�-@�@���@�b@��
@@�33@�E�@�@��@��@�ƨ@�l�@�33@��@�\@�5?@���@陚@��@�u@�(�@�S�@�R@�E�@���@�7@�7L@䛦@�A�@㝲@�
=@�=q@��@��@�1'@�;d@ޟ�@�{@�`B@�G�@��@�(�@�\)@�@��H@ڰ!@�~�@�-@��@�?}@��/@؋D@�1'@��
@�|�@���@�-@���@Չ7@�?}@��/@ԋD@���@��@��@҇+@�{@Ѳ-@���@�A�@υ@�C�@�o@���@·+@�`B@�Ĝ@̓u@��@�+@��@ʰ!@�5?@ɺ^@�hs@��@��/@�z�@��@�"�@Ƈ+@���@�G�@��`@�bN@���@�"�@\@��@��h@�p�@�&�@���@�Z@��@�M�@��T@��7@�p�@�X@�%@��@��9@�Q�@��F@�S�@��@��H@�v�@���@��@�z�@��w@��y@�M�@��@�?}@���@�Ĝ@��@�(�@�\)@��@���@�^5@��T@�p�@���@��u@�Z@���@�@�~�@�E�@��T@��^@���@���@�O�@��@���@��D@�b@��P@��H@��!@��+@�M�@�{@��@��-@��7@�X@�%@��u@��@��@�l�@��@��@���@���@��\@��@���@�O�@��@��@�(�@���@��w@�|�@�;d@�ȴ@�n�@��T@��@�`B@�V@���@��@�Z@�Q�@�ƨ@�
=@��!@�=q@��@�{@��@�hs@�V@���@��u@��@���@��@�l�@�33@��!@�E�@��@��h@��j@��w@�+@�@�ȴ@��R@���@���@�~�@�E�@�J@���@�x�@�O�@�&�@��9@�Z@�(�@��;@��@���@��P@�K�@�o@��@���@�ff@��@���@�?}@�Ĝ@��u@�z�@�Z@� �@��
@���@��@��R@�~�@�$�@���@��@�r�@� �@�1@��;@�t�@�+@��@��y@���@��\@�~�@�n�@�n�@�n�@�-@���@��@��@���@��^@���@��h@�?}@��@��D@�I�@�b@�1@�  @�  @��
@�C�@��H@���@���@��+@�n�@�-@���@��^@�G�@��@��/@��@�A�@�1@��
@��w@��@��@�dZ@��H@��R@���@���@�n�@��@�@�p�@�7L@���@�I�@~��@~{@}O�@|�j@|�@{�
@{dZ@z��@z�@yhs@x�u@w�;@w;d@vV@t��@tI�@s��@s"�@so@r��@q�^@pQ�@o|�@o+@n�@n$�@m�h@m`B@m�@l��@l9X@k�
@k��@k"�@j��@jM�@i�#@i�^@ihs@i&�@hĜ@hr�@g�@g�@g|�@f��@e�T@e�-@e�h@e`B@d�@d��@d�D@d(�@c�m@c��@cS�@c@b��@b~�@b-@a�@`�u@`bN@`bN@`Q�@_�@_�w@_�@_��@_l�@^��@^��@]��@]`B@]O�@]V@\(�@[��@[dZ@[C�@[33@[o@Z�@Z��@Z�\@Z-@Y�@Y�#@Y��@YG�@Y&�@X��@XA�@Xb@X  @W�;@W�@W|�@W+@V�y@V�R@V�+@U�T@U/@T�@TZ@T1@S��@S�F@S@R��@Rn�@R-@Q�7@Q%@PQ�@O�@O\)@O;d@O+@O+@O�@O
=@N��@Nȴ@N��@NE�@N5?@NE�@NE�@N5?@M�-@M`B@MO�@M�@L��@Lj@K�m@KS�@K@J�@Ix�@Ix�@IX@I%@H�@HA�@G��@G
=@F5?@E�@D�D@D(�@Cƨ@B�H@B��@B��@B~�@B^5@B�@A�@A��@A��@A�7@A�@@�9@@��@@Q�@?��@?+@>�R@>�+@>�+@>�+@>V@>{@=��@=�-@=`B@=O�@=�@=/@=/@=V@<��@<��@<(�@<1@<1@;�
@;S�@:��@:^5@9��@9��@9�7@9hs@9X@8��@8bN@81'@8b@7�@7��@7�P@6��@6ff@6E�@6$�@6{@5�T@5�-@5�@5`B@4��@4j@4(�@4(�@4�@3�m@3�F@3��@3S�@2�H@2��@2J@1�7@1hs@1X@1X@1hs@1G�@1�@0�`@0Ĝ@0�9@0�9@0�u@0Q�@/��@/|�@/�@.��@.v�@.{@-��@-��@-�@-`B@-?}@,�@,j@+��@+�@+dZ@+"�@*��@*-@)��@)x�@)%@(�u@(�@(Q�@(  @'��@'l�@'K�@'
=@&�@&v�@&$�@%�T@%��@%��@%��@%@%@%�-@%��@%p�@%O�@%�@$�D@#�m@#�F@#��@#S�@#C�@#o@"�!@"�\@"M�@"-@"�@!��@!��@!�#@!�^@!��@!�7@!hs@ ��@ �`@ ��@ Ĝ@ �u@ A�@  �@  �@��@�@�P@;d@��@�@�R@�+@v�@E�@5?@5?@5?@$�@@�h@O�@/@�@��@�j@j@Z@Z@I�@I�@9X@�@�F@S�@C�@C�@33@�H@��@�!@n�@J@J@��@��@��@��@��@��@�^@��@�7@x�@hs@%@�9@b@��@�P@l�@+@
=@��@�@v�@V@$�@@{@@�-@p�@p�@`B@/@V@��@�@�j@�D@9X@��@�
@�
@ƨ@��@t�@S�@33@"�@�H@��@�!@�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ǮB
ĜB
ĜB
�sB	7BDBJBJBPBVBVB\BVBVBPB
=B  B
�B
�mB
��B
�?B
�XB
�5B
��B!�B7LBJ�Be`B^5B��B�-B�ZB�B&�B%�B49B33B �B#�B�B��B��B��B�;BB�BB��B�)B�HB�`B��B��BB�!B��B�B�Bt�BYBG�BhB
�BB
��B
��B
�}B
��B
�B
r�B
ffB
_;B
)�B	��B
1B	�fB	�'B	��B	o�B	L�B	-B	�B	B	�B	VB	�B	2-B	'�B	�B	bB	33B	)�B	�B	�B	�B	'�B	/B	L�B	ZB	W
B	hsB	x�B	�B	�7B	�PB	�VB	�JB	�=B	�VB	��B	�B	�9B	��B	��B	��B	�;B	�`B	��B
hB
�B
�B
"�B
'�B
,B
0!B
1'B
,B
'�B
'�B
+B
2-B
7LB
7LB
6FB
5?B
9XB
8RB
49B
8RB
:^B
>wB
?}B
<jB
F�B
R�B
R�B
VB
XB
VB
VB
W
B
VB
VB
W
B
VB
XB
XB
YB
XB
VB
W
B
ZB
[#B
ZB
W
B
VB
XB
W
B
YB
YB
XB
W
B
W
B
S�B
P�B
S�B
VB
T�B
S�B
T�B
W
B
XB
XB
XB
YB
YB
W
B
VB
T�B
S�B
S�B
P�B
R�B
T�B
W
B
VB
S�B
S�B
T�B
VB
W
B
T�B
T�B
S�B
P�B
N�B
N�B
S�B
S�B
Q�B
P�B
N�B
O�B
N�B
L�B
N�B
M�B
J�B
L�B
N�B
O�B
O�B
N�B
M�B
K�B
G�B
I�B
I�B
J�B
H�B
F�B
F�B
D�B
E�B
D�B
D�B
C�B
C�B
C�B
F�B
G�B
F�B
D�B
A�B
?}B
A�B
A�B
A�B
A�B
A�B
A�B
D�B
C�B
B�B
B�B
@�B
>wB
>wB
>wB
=qB
>wB
>wB
;dB
:^B
;dB
>wB
@�B
?}B
=qB
:^B
:^B
9XB
:^B
:^B
:^B
9XB
7LB
5?B
9XB
9XB
6FB
33B
33B
6FB
5?B
33B
6FB
7LB
6FB
6FB
5?B
5?B
49B
2-B
1'B
2-B
.B
0!B
0!B
0!B
1'B
2-B
0!B
0!B
.B
+B
'�B
&�B
+B
)�B
'�B
+B
+B
)�B
.B
,B
(�B
(�B
,B
-B
-B
,B
+B
)�B
'�B
(�B
(�B
(�B
'�B
&�B
#�B
$�B
&�B
&�B
%�B
#�B
#�B
!�B
�B
$�B
#�B
 �B
!�B
�B
�B
�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
!�B
!�B
�B
 �B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
!�B
�B
#�B
#�B
"�B
 �B
�B
 �B
"�B
!�B
 �B
�B
�B
"�B
!�B
�B
�B
!�B
$�B
$�B
&�B
'�B
&�B
$�B
$�B
#�B
#�B
"�B
!�B
!�B
&�B
'�B
&�B
&�B
'�B
'�B
&�B
&�B
%�B
#�B
$�B
%�B
(�B
(�B
)�B
+B
+B
(�B
%�B
%�B
'�B
'�B
&�B
(�B
+B
+B
+B
)�B
(�B
(�B
'�B
)�B
-B
+B
+B
/B
-B
-B
)�B
%�B
-B
-B
0!B
1'B
/B
,B
.B
/B
/B
.B
2-B
1'B
0!B
0!B
.B
.B
/B
.B
,B
-B
0!B
6FB
5?B
7LB
7LB
6FB
6FB
5?B
49B
33B
6FB
6FB
5?B
49B
5?B
7LB
7LB
8RB
9XB
9XB
7LB
7LB
7LB
7LB
6FB
6FB
6FB
7LB
7LB
:^B
:^B
:^B
9XB
8RB
8RB
7LB
8RB
9XB
9XB
7LB
7LB
9XB
<jB
=qB
=qB
<jB
=qB
@�B
?}B
?}B
A�B
A�B
B�B
B�B
B�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
@�B
@�B
A�B
C�B
C�B
E�B
E�B
D�B
B�B
@�B
B�B
D�B
D�B
D�B
D�B
C�B
B�B
E�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
I�B
H�B
G�B
I�B
K�B
J�B
I�B
H�B
H�B
I�B
J�B
H�B
G�B
G�B
J�B
L�B
L�B
L�B
N�B
M�B
L�B
L�B
K�B
K�B
L�B
M�B
M�B
M�B
O�B
O�B
O�B
Q�B
P�B
N�B
O�B
P�B
S�B
R�B
R�B
R�B
T�B
S�B
S�B
T�B
VB
VB
VB
VB
VB
W
B
XB
XB
XB
XB
XB
XB
YB
YB
W
B
XB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
\)B
^5B
_;B
`BB
`BB
_;B
`BB
`BB
`BB
_;B
^5B
_;B
^5B
_;B
aHB
`BB
^5B
`BB
aHB
bNB
cTB
bNB
bNB
bNB
bNB
bNB
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
cTB
cTB
cTB
dZB
dZB
dZB
cTB
cTB
dZB
e`B
ffB
ffB
ffB
dZB
ffB
ffB
ffB
e`B
e`B
e`B
ffB
hsB
iyB
iyB
jB
iyB
iyB
iyB
iyB
iyB
hsB
jB
jB
jB
iyB
hsB
hsB
iyB
iyB
hsB
gmB
gmB
gmB
hsB
gmB
hsB
k�B
jB
jB
iyB
jB
iyB
iyB
iyB
hsB
hsB
hsB
gmB
ffB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
iyB
jB
k�B
jB
iyB
iyB
jB
l�B
m�B
m�B
l�B
l�B
l�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
o�B
n�B
m�B
o�B
o�B
p�B
q�B
q�B
q�B
o�B
p�B
q�B
r�B
r�B
q�B
r�B
p�B
p�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
u�B
w�B
v�B
v�B
v�B
w�B
v�B
v�B
w�B
v�B
v�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
z�B
y�B
x�B
z�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
z�B
z�B
z�B
z�B
{�B
z�B
z�B
y�B
z�B
|�B
{�B
{�B
}�B
}�B
|�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
� B
�B
�B
� B
� B
� B
� B
� B
~�B
~�B
}�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�1B
�+B
�+B
�+B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�7B
�7B
�7B
�7B
�7B
�1B
�1B
�1B
�=B
�=B
�7B
�7B
�7B
�=B
�7B
�7B
�=B
�=B
�=B
�JB
�PB
�PB
�PB
�PB
�PB
�JB
�PB
�JB
�DB
�DB
�=B
�=B
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�bB
�bB
�\B
�\B
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ǮB
ĜB
ĜB
�sB	7BDBJBJBPBVBVB\BVBVBPB
=B  B
�B
�mB
��B
�?B
�XB
�5B
��B!�B7LBJ�Be`B^5B��B�-B�ZB�B&�B%�B49B33B �B#�B�B��B��B��B�;BB�BB��B�)B�HB�`B��B��BB�!B��B�B�Bt�BYBG�BhB
�BB
��B
��B
�}B
��B
�B
r�B
ffB
_;B
)�B	��B
1B	�fB	�'B	��B	o�B	L�B	-B	�B	B	�B	VB	�B	2-B	'�B	�B	bB	33B	)�B	�B	�B	�B	'�B	/B	L�B	ZB	W
B	hsB	x�B	�B	�7B	�PB	�VB	�JB	�=B	�VB	��B	�B	�9B	��B	��B	��B	�;B	�`B	��B
hB
�B
�B
"�B
'�B
,B
0!B
1'B
,B
'�B
'�B
+B
2-B
7LB
7LB
6FB
5?B
9XB
8RB
49B
8RB
:^B
>wB
?}B
<jB
F�B
R�B
R�B
VB
XB
VB
VB
W
B
VB
VB
W
B
VB
XB
XB
YB
XB
VB
W
B
ZB
[#B
ZB
W
B
VB
XB
W
B
YB
YB
XB
W
B
W
B
S�B
P�B
S�B
VB
T�B
S�B
T�B
W
B
XB
XB
XB
YB
YB
W
B
VB
T�B
S�B
S�B
P�B
R�B
T�B
W
B
VB
S�B
S�B
T�B
VB
W
B
T�B
T�B
S�B
P�B
N�B
N�B
S�B
S�B
Q�B
P�B
N�B
O�B
N�B
L�B
N�B
M�B
J�B
L�B
N�B
O�B
O�B
N�B
M�B
K�B
G�B
I�B
I�B
J�B
H�B
F�B
F�B
D�B
E�B
D�B
D�B
C�B
C�B
C�B
F�B
G�B
F�B
D�B
A�B
?}B
A�B
A�B
A�B
A�B
A�B
A�B
D�B
C�B
B�B
B�B
@�B
>wB
>wB
>wB
=qB
>wB
>wB
;dB
:^B
;dB
>wB
@�B
?}B
=qB
:^B
:^B
9XB
:^B
:^B
:^B
9XB
7LB
5?B
9XB
9XB
6FB
33B
33B
6FB
5?B
33B
6FB
7LB
6FB
6FB
5?B
5?B
49B
2-B
1'B
2-B
.B
0!B
0!B
0!B
1'B
2-B
0!B
0!B
.B
+B
'�B
&�B
+B
)�B
'�B
+B
+B
)�B
.B
,B
(�B
(�B
,B
-B
-B
,B
+B
)�B
'�B
(�B
(�B
(�B
'�B
&�B
#�B
$�B
&�B
&�B
%�B
#�B
#�B
!�B
�B
$�B
#�B
 �B
!�B
�B
�B
�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
!�B
!�B
�B
 �B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
!�B
�B
#�B
#�B
"�B
 �B
�B
 �B
"�B
!�B
 �B
�B
�B
"�B
!�B
�B
�B
!�B
$�B
$�B
&�B
'�B
&�B
$�B
$�B
#�B
#�B
"�B
!�B
!�B
&�B
'�B
&�B
&�B
'�B
'�B
&�B
&�B
%�B
#�B
$�B
%�B
(�B
(�B
)�B
+B
+B
(�B
%�B
%�B
'�B
'�B
&�B
(�B
+B
+B
+B
)�B
(�B
(�B
'�B
)�B
-B
+B
+B
/B
-B
-B
)�B
%�B
-B
-B
0!B
1'B
/B
,B
.B
/B
/B
.B
2-B
1'B
0!B
0!B
.B
.B
/B
.B
,B
-B
0!B
6FB
5?B
7LB
7LB
6FB
6FB
5?B
49B
33B
6FB
6FB
5?B
49B
5?B
7LB
7LB
8RB
9XB
9XB
7LB
7LB
7LB
7LB
6FB
6FB
6FB
7LB
7LB
:^B
:^B
:^B
9XB
8RB
8RB
7LB
8RB
9XB
9XB
7LB
7LB
9XB
<jB
=qB
=qB
<jB
=qB
@�B
?}B
?}B
A�B
A�B
B�B
B�B
B�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
@�B
@�B
A�B
C�B
C�B
E�B
E�B
D�B
B�B
@�B
B�B
D�B
D�B
D�B
D�B
C�B
B�B
E�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
I�B
H�B
G�B
I�B
K�B
J�B
I�B
H�B
H�B
I�B
J�B
H�B
G�B
G�B
J�B
L�B
L�B
L�B
N�B
M�B
L�B
L�B
K�B
K�B
L�B
M�B
M�B
M�B
O�B
O�B
O�B
Q�B
P�B
N�B
O�B
P�B
S�B
R�B
R�B
R�B
T�B
S�B
S�B
T�B
VB
VB
VB
VB
VB
W
B
XB
XB
XB
XB
XB
XB
YB
YB
W
B
XB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
\)B
^5B
_;B
`BB
`BB
_;B
`BB
`BB
`BB
_;B
^5B
_;B
^5B
_;B
aHB
`BB
^5B
`BB
aHB
bNB
cTB
bNB
bNB
bNB
bNB
bNB
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
cTB
cTB
cTB
dZB
dZB
dZB
cTB
cTB
dZB
e`B
ffB
ffB
ffB
dZB
ffB
ffB
ffB
e`B
e`B
e`B
ffB
hsB
iyB
iyB
jB
iyB
iyB
iyB
iyB
iyB
hsB
jB
jB
jB
iyB
hsB
hsB
iyB
iyB
hsB
gmB
gmB
gmB
hsB
gmB
hsB
k�B
jB
jB
iyB
jB
iyB
iyB
iyB
hsB
hsB
hsB
gmB
ffB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
iyB
jB
k�B
jB
iyB
iyB
jB
l�B
m�B
m�B
l�B
l�B
l�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
o�B
n�B
m�B
o�B
o�B
p�B
q�B
q�B
q�B
o�B
p�B
q�B
r�B
r�B
q�B
r�B
p�B
p�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
u�B
w�B
v�B
v�B
v�B
w�B
v�B
v�B
w�B
v�B
v�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
z�B
y�B
x�B
z�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
z�B
z�B
z�B
z�B
{�B
z�B
z�B
y�B
z�B
|�B
{�B
{�B
}�B
}�B
|�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
� B
�B
�B
� B
� B
� B
� B
� B
~�B
~�B
}�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�1B
�+B
�+B
�+B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�7B
�7B
�7B
�7B
�7B
�1B
�1B
�1B
�=B
�=B
�7B
�7B
�7B
�=B
�7B
�7B
�=B
�=B
�=B
�JB
�PB
�PB
�PB
�PB
�PB
�JB
�PB
�JB
�DB
�DB
�=B
�=B
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�bB
�bB
�\B
�\B
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220913090115                              AO  ARCAADJP                                                                    20220913090115    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220913090115  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20220913090115  QCF$                G�O�G�O�G�O�4000            