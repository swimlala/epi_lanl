CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-05-31T09:01:12Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220531090112  20220531090112  4903175 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7231                            2B  A   NAVIS_A                         0968                            170425                          863 @�����1   @��y\�"@2�
=p���d@ ě��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   F   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр DѼ�D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�zC-�C/�C1�C3�C5�C7�C:�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�DѺ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D��D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aѧ�Aѡ�Aѥ�Aљ�Aџ�A�|�AжFAЕ�AЍPAЅA�|�A�l�A�\)A�VA�I�A��A�x�A�^5A��A�A��A��A��A��A��yA���A̧�Ȧ+A�dZA�G�A�5?A��A�$�A�oA���A��A��AˑhA�n�AʮA�K�A�^5A���A�x�A���A�33AƧ�A�$�A�l�A��;A�XA�JA���A�1'A��;A�|�A��
A�K�A�S�A���A��hA�&�A��A��A�
=A��A�G�A���A�G�A���A��9A�dZA��FA�G�A��A��\A��/A��;A�O�A��A�E�A��FA�33A�JA�C�A�9XA���A��A��RA��A�Q�A���A��A�-A�K�A��^A��A���A�I�A�JA���A���A�`BA��/A��A�`BA�ƨA�-A��A�
=A�bA��uA� �A�p�A���A��#A�ĜA��uA�XA��A�ffA���A��A�oA�A}�PAy�hAvE�Aq��AmO�Ai�#AghsAb^5A_�#A^n�A\1AZ�+AX1AV��AUG�AS�;AQ��API�AN-AI\)AGAE�^AD�jAD�AB�+A@v�A?�;A=�TA;�A8A�A7G�A6��A6�A4�/A2�A/�TA-/A+x�A*E�A'��A&v�A%ƨA$��A"��A#
=A#`BA"�\A�
AVA��A�A��A1A\)A�A��AjA$�A�
A�A�uAQ�AA��A|�A\)A�A�`AbNA �A�^A��Ar�A5?A{A�#A;dAC�A��A�hA
$�A	��A�\A��A�
A�A�jA=qA�A�
A n�@��+@��h@���@�V@�^5@�
=@�?}@�9X@�b@��;@@�P@�R@���@�7@�9X@�h@�&�@�j@��@���@��/@�A�@�C�@�\@�$�@�-@�X@�b@�
=@ݺ^@�A�@��@���@��@���@�V@պ^@Ԭ@�"�@ѡ�@Л�@�j@�Q�@�1'@�b@ϝ�@Ο�@͙�@���@�~�@ɺ^@��@�Z@ǶF@�S�@���@ũ�@�Z@��@�dZ@�V@�x�@���@���@�5?@�G�@�Ĝ@���@�t�@�K�@�C�@�;d@�"�@���@��h@��@�j@�I�@��;@��@�v�@��@��@��@��/@�bN@�1@��H@��+@�v�@�V@��@���@��h@���@�I�@��F@��@���@��+@�n�@�V@�5?@���@�@���@��@��9@�9X@�b@��@���@�
=@��+@�=q@�-@��@���@���@�p�@�?}@�7L@�/@�V@��9@�9X@��w@��@�K�@�;d@�;d@�;d@�;d@��@��@�~�@��@�@�{@��#@��@���@� �@�l�@�33@��y@��R@���@��@��-@�hs@��@��/@�Q�@�b@��
@��@�dZ@�K�@�33@���@���@���@��+@�v�@�V@�{@��h@�`B@�7L@���@���@�bN@�Q�@��@�  @���@��P@�o@��H@���@�~�@�{@�X@�&�@��@��@�V@���@�I�@��w@�t�@�l�@�dZ@�S�@��R@���@��!@���@�M�@��#@���@�@��-@��h@��@�Ĝ@��u@�Z@�(�@�1@��F@��@�
=@���@�^5@�@�{@���@�@�x�@�x�@�x�@�O�@�7L@�V@���@��D@�ƨ@���@�|�@�;d@�o@�@��H@��R@��+@�ff@�5?@��-@�?}@��@��`@��j@��j@��@��@���@�r�@�Q�@�9X@�|�@�33@���@��\@�~�@�ff@�5?@��@��7@��@�x�@�p�@�?}@��@��D@��@���@���@��@��@�ȴ@��+@�^5@�M�@�-@�{@��#@��7@��@��@�j@�(�@���@��;@���@��w@��@��F@��@��@��@���@���@�33@�o@��@��H@�^5@�5?@��@��@��@�@��7@�O�@�7L@��@���@���@�A�@�@�w@�P@;d@~ȴ@~ff@}��@|�/@|1@{33@z��@z��@z�\@z^5@y�#@yx�@y7L@y�@x��@x�9@x�u@w��@w�P@w+@w
=@v�R@vE�@v@u��@t�@t�D@tz�@t(�@s�@s"�@r�H@r^5@q�#@q�7@q&�@p �@n��@n��@m�T@m��@l��@l�D@lz�@lI�@k��@kt�@j�H@jn�@i�^@i�@h��@h  @g�w@gK�@g�@f�y@f��@f$�@e@ep�@e�@d��@d�/@d�@d�D@dI�@d1@c�
@cƨ@cS�@c@b�!@b^5@b-@bJ@a�#@a��@a�@_��@_|�@_\)@_;d@_
=@^��@^V@^5?@]�T@]?}@\�@\(�@[�
@[ƨ@[��@[33@Z�@Z��@Z-@Y�@Y�7@X��@XQ�@XA�@X1'@Xb@W\)@Vȴ@VV@V@U�@Up�@UO�@U/@T�@S��@R��@R�@Q��@Q�@P�u@P1'@O�;@O�@Ol�@N�y@NV@N@M@M?}@L�j@L��@L�D@L�D@L�D@LZ@K��@KdZ@K"�@J��@J=q@J-@J-@J�@I�#@IX@HQ�@G�w@G�@F�y@F��@FE�@E�@E��@E��@EO�@E�@D9X@Cƨ@C��@C��@C�@C�@Ct�@CS�@B�!@A�#@AX@A%@@��@@Q�@@  @?��@?�@?
=@>ȴ@>��@>ff@>5?@>@=�T@=p�@=�@<�D@<�@;��@;��@;�@;t�@;C�@:�H@:M�@9�@9��@9G�@8��@8�9@8�9@8��@8�@8r�@8bN@81'@7�;@7|�@7K�@7K�@7;d@7
=@6ȴ@6ff@6{@5@5�@5�@4�j@4Z@3�F@3��@3��@3dZ@333@3o@2��@2�\@2M�@2�@1�#@1x�@0��@/��@/�@/��@/��@/|�@/l�@/l�@/K�@/
=@.�@.V@-�@-�h@-O�@-V@,�j@,z�@,Z@+��@+�
@+ƨ@+��@+t�@+S�@+33@*�@*~�@*=q@)��@)��@)X@)&�@)%@(��@(�`@(�9@(A�@(b@'�w@'\)@'K�@';d@'+@'�@&�y@&��@&v�@&ff@&V@&$�@%��@%p�@$��@$�D@$z�@$j@$Z@$Z@$I�@$�@#��@"�H@"��@"�!@"~�@"=q@!��@!G�@ ��@ �@ A�@ b@�@|�@
=@ȴ@��@V@�T@��@/@��@�@��@S�@33@@�H@~�@^5@M�@-@�#@�^@�7@x�@&�@�`@�`@��@Ĝ@��@bN@b@�@�P@|�@l�@K�@ȴ@��@��@v�@v�@5?@�-@�@`B@?}@/@/@/@/@/@�@��@�@9X@1@��@�m@ƨ@��@t�@@~�@�@�@�#@��@��@�7@X@&�@%@%@%@��@��@Q�@  @�@�;@��@�@��@��@�P@|�@l�@K�@;d@�@��@ȴ@��@��@��@��@v�@V@V@5?@$�@�-@p�@O�@/@�j@9X@ƨ@C�@
��@
�!@
�\@
n�@
n�@
^5@
M�@
=q@
=q@
-@
�@	�#@	��@	��@	x�@	&�@��@Ĝ@�9@��@��@��@�u@r�@A�@ �@b@b@��@|�@;d@�@�@��@v�@5?@�@@�@p�@`B@O�@O�@?}@/@�@V@��@��@V@��@��@��@�@�@�/@��@��@�/114411441444444414414441444111444444444111111441111114411144411144111114441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aѧ�Aѡ�Aѥ�Aљ�Aџ�A�|�AжFAЕ�AЍPAЅA�|�A�l�A�\)A�VA�I�A��A�x�A�^5A��A�A��A��A��A��A��yA���A̧�Ȧ+A�dZA�G�A�5?A��A�$�A�oA���A��A��AˑhA�n�AʮA�K�A�^5A���A�x�A���A�33AƧ�A�$�A�l�A��;A�XA�JA���A�1'A��;A�|�A��
A�K�A�S�A���A��hA�&�A��A��A�
=A��A�G�A���A�G�A���A��9A�dZA��FA�G�A��A��\A��/A��;A�O�A��A�E�A��FA�33A�JA�C�A�9XA���A��A��RA��A�Q�A���A��A�-A�K�A��^A��A���A�I�A�JA���A���A�`BA��/A��A�`BA�ƨA�-A��A�
=A�bA��uA� �A�p�A���A��#A�ĜA��uA�XA��A�ffA���A��A�oA�A}�PAy�hAvE�Aq��AmO�Ai�#AghsAb^5A_�#A^n�A\1AZ�+AX1AV��AUG�AS�;AQ��API�AN-AI\)AGAE�^AD�jAD�AB�+A@v�A?�;A=�TA;�A8A�A7G�A6��A6�A4�/A2�A/�TA-/A+x�A*E�A'��A&v�A%ƨA$��A"��A#
=A#`BA"�\A�
AVA��A�A��A1A\)A�A��AjA$�A�
A�A�uAQ�AA��A|�A\)A�A�`AbNA �A�^A��Ar�A5?A{A�#A;dAC�A��A�hA
$�A	��A�\A��A�
A�A�jA=qA�A�
A n�@��+@��h@���@�V@�^5@�
=@�?}@�9X@�b@��;@@�P@�R@���@�7@�9X@�h@�&�@�j@��@���@��/@�A�@�C�@�\@�$�@�-@�X@�b@�
=@ݺ^@�A�@��@���@��@���@�V@պ^@Ԭ@�"�@ѡ�@Л�@�j@�Q�@�1'@�b@ϝ�@Ο�@͙�@���@�~�@ɺ^@��@�Z@ǶF@�S�@���@ũ�@�Z@��@�dZ@�V@�x�@���@���@�5?@�G�@�Ĝ@���@�t�@�K�@�C�@�;d@�"�@���@��h@��@�j@�I�@��;@��@�v�@��@��@��@��/@�bN@�1@��H@��+@�v�@�V@��@���@��h@���@�I�@��F@��@���@��+@�n�@�V@�5?@���@�@���@��@��9@�9X@�b@��@���@�
=@��+@�=q@�-@��@���@���@�p�@�?}@�7L@�/@�V@��9@�9X@��w@��@�K�@�;d@�;d@�;d@�;d@��@��@�~�@��@�@�{@��#@��@���@� �@�l�@�33@��y@��R@���@��@��-@�hs@��@��/@�Q�@�b@��
@��@�dZ@�K�@�33@���@���@���@��+@�v�@�V@�{@��h@�`B@�7L@���@���@�bN@�Q�@��@�  @���@��P@�o@��H@���@�~�@�{@�X@�&�@��@��@�V@���@�I�@��w@�t�@�l�@�dZ@�S�@��R@���@��!@���@�M�@��#@���@�@��-@��h@��@�Ĝ@��u@�Z@�(�@�1@��F@��@�
=@���@�^5@�@�{@���@�@�x�@�x�@�x�@�O�@�7L@�V@���@��D@�ƨ@���@�|�@�;d@�o@�@��H@��R@��+@�ff@�5?@��-@�?}@��@��`@��j@��j@��@��@���@�r�@�Q�@�9X@�|�@�33@���@��\@�~�@�ff@�5?@��@��7@��@�x�@�p�@�?}@��@��D@��@���@���@��@��@�ȴ@��+@�^5@�M�@�-@�{@��#@��7@��@��@�j@�(�@���@��;@���@��w@��@��F@��@��@��@���@���@�33@�o@��@��H@�^5@�5?@��@��@��@�@��7@�O�@�7L@��@���@���@�A�@�@�w@�P@;d@~ȴ@~ff@}��@|�/@|1@{33@z��@z��@z�\@z^5@y�#@yx�@y7L@y�@x��@x�9@x�u@w��@w�P@w+@w
=@v�R@vE�@v@u��@t�@t�D@tz�@t(�@s�@s"�@r�H@r^5@q�#@q�7@q&�@p �@n��@n��@m�T@m��@l��@l�D@lz�@lI�@k��@kt�@j�H@jn�@i�^@i�@h��@h  @g�w@gK�@g�@f�y@f��@f$�@e@ep�@e�@d��@d�/@d�@d�D@dI�@d1@c�
@cƨ@cS�@c@b�!@b^5@b-@bJ@a�#@a��@a�@_��@_|�@_\)@_;d@_
=@^��@^V@^5?@]�T@]?}@\�@\(�@[�
@[ƨ@[��@[33@Z�@Z��@Z-@Y�@Y�7@X��@XQ�@XA�@X1'@Xb@W\)@Vȴ@VV@V@U�@Up�@UO�@U/@T�@S��@R��@R�@Q��@Q�@P�u@P1'@O�;@O�@Ol�@N�y@NV@N@M@M?}@L�j@L��@L�D@L�D@L�D@LZ@K��@KdZ@K"�@J��@J=q@J-@J-@J�@I�#@IX@HQ�@G�w@G�@F�y@F��@FE�@E�@E��@E��@EO�@E�@D9X@Cƨ@C��@C��@C�@C�@Ct�@CS�@B�!@A�#@AX@A%@@��@@Q�@@  @?��@?�@?
=@>ȴ@>��@>ff@>5?@>@=�T@=p�@=�@<�D@<�@;��@;��@;�@;t�@;C�@:�H@:M�@9�@9��@9G�@8��@8�9@8�9@8��@8�@8r�@8bN@81'@7�;@7|�@7K�@7K�@7;d@7
=@6ȴ@6ff@6{@5@5�@5�@4�j@4Z@3�F@3��@3��@3dZ@333@3o@2��@2�\@2M�@2�@1�#@1x�@0��@/��@/�@/��@/��@/|�@/l�@/l�@/K�@/
=@.�@.V@-�@-�h@-O�@-V@,�j@,z�@,Z@+��@+�
@+ƨ@+��@+t�@+S�@+33@*�@*~�@*=q@)��@)��@)X@)&�@)%@(��@(�`@(�9@(A�@(b@'�w@'\)@'K�@';d@'+@'�@&�y@&��@&v�@&ff@&V@&$�@%��@%p�@$��@$�D@$z�@$j@$Z@$Z@$I�@$�@#��@"�H@"��@"�!@"~�@"=q@!��@!G�@ ��@ �@ A�@ b@�@|�@
=@ȴ@��@V@�T@��@/@��@�@��@S�@33@@�H@~�@^5@M�@-@�#@�^@�7@x�@&�@�`@�`@��@Ĝ@��@bN@b@�@�P@|�@l�@K�@ȴ@��@��@v�@v�@5?@�-@�@`B@?}@/@/@/@/@/@�@��@�@9X@1@��@�m@ƨ@��@t�@@~�@�@�@�#@��@��@�7@X@&�@%@%@%@��@��@Q�@  @�@�;@��@�@��@��@�P@|�@l�@K�@;d@�@��@ȴ@��@��@��@��@v�@V@V@5?@$�@�-@p�@O�@/@�j@9X@ƨ@C�@
��@
�!@
�\@
n�@
n�@
^5@
M�@
=q@
=q@
-@
�@	�#@	��@	��@	x�@	&�@��@Ĝ@�9@��@��@��@�u@r�@A�@ �@b@b@��@|�@;d@�@�@��@v�@5?@�@@�@p�@`B@O�@O�@?}@/@�@V@��@��@V@��@��@��@�@�@�/@��@��@�/114411441444444414414441444111444444444111111441111114411144411144111114441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A��DA��9A��wA��TA�A��A�C�A�VA��!A��\A�33A�/A�&�A��DA��uA��-A��-A�^5A��+A���A���A���A� �A�1'A��+A�A�(�A�XA�7LA��#A��PA�ȴA�`BA��/A��hA��^A���A�G�A�%A�\)A���A�"�A���A���A�?}A�dZA�A�z�A�  A���A���A��uA��\A��A�+A�1'A��A��A�1'A�9XA��A��;A�l�A�VA��#A���A���A��A�5?A�K�A�(�A���A��\A�
=A��A���A���A���A���A�ƨA���A��^A�x�A�VA�1'A�+A���A��A��A��A�oA��yA�ƨA���A��-A��FA���A�l�A�S�A�G�A�bNA�C�A��A�  A�ȴA�ƨA�ĜA��wA���A���A��wA���A���A���A�x�A�;dA���A��HA��;A�ĜA���A�t�A�7LA�=qA�A���A��
A���A��FA��A�~�A���A��!A��\A��hA�z�A�|�A�l�A�S�A�1'A� �A��A���A�A���A���A���A��;A���A���A���A��hA�~�A���A��uA�t�A�G�A�{A���A��A���A���A��TA�A�%A���A���A� �A��A��A�ȴA���A�ĜA���A���A��
A��/A��HA��#A��;A��/A���A���A��A��#A��
A��
A��
A���A���A���A�ĜA�ƨA���A��^A���A���A��RA���A��PA�p�A�t�A�jA�v�A�~�A�n�A�ffA�v�A�z�A�z�A�n�A�XA�S�A�M�A�S�A�XA�G�A�7LA�7LA�7LA�O�A�^5A�hsA�dZA�`BA�^5A�S�A�Q�A�K�A�=qA�7LA�M�A�E�A�=qA�I�A�I�A�K�A�G�A�K�A�K�A�G�A�A�A�5?A�7LA�5?A�5?A�9XA�9XA�9XA�33A�?}A�9XA�1'A�/A�7LA�C�A�M�A�M�A�I�A�C�A�;dA�1'A�/A�-A�7LA�C�A�G�A�I�A�K�A�M�A�I�A�I�A�Q�A�^5A�ZA�VA�^5A�`BA�\)A�\)A�hsA�n�A�n�A�x�A�|�A�|�A�z�A�v�A�p�A�l�A�r�A��A�~�A�z�A�x�A�|�A��A��A��A��+A��A��+A��A��\A��uA��hA��\A��hA��PA��7A��7A��\A��\A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A���A���A���A���A��A��A���A���A��A��A��A��9A��FA��FA��FA��-A��9A��FA��RA��^A��RA��jA��wA���A���A�A�A���A�A�ƨA�ĜA�ĜA�A���A���A�ĜA�ĜA�ƨA�ĜA�ƨA�ȴA�ȴA�ȴA�ȴA�A�ĜA�ȴA�ȴA�ĜA�ĜA�ĜA���A���A���A���A�ȴA�ƨA�ȴA���A���A���A���A���A���A��
A���A���A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��
A��
A��
A��#A��A��A��A��
A���A���A���A��A��#A��#A��/A��;A��/A��/A��/A��/A��#A��
A��#A��/A��/A��/A��;A��;A��/A��#A��#A��A��
A���A��A��#A��#A��;A��/A��/A��#A��#A��;A��;A��/A��#A��A��A��A��;A��;A��/A��#A��/A��;A��HA��HA��;A��;A��/A��#A��A��#A��/A��/A��;A��HA��HA��HA��HA��TA��HA��HA��HA��;A��/A��A��/A��/A��#A��
A��/A��/A��/A��/A��#A��A��A��#A��A��#A��A��
A��A��#A��#A��A��
A��
A���A���A���A��
A��A��#A��A��A��
A��
A��
A��A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA�ȴA�ƨA�ȴA�ȴA�ƨA�ƨA�ĜA���A��wA�ȴA�ƨA�ƨA�ĜA�ĜA�ĜA�ĜA�A���A���A�A�ĜA�ĜA�A���A�A���A��wA���A��wA��jA���A�A���A��wA��^A��^A��jA��jA��jA��wA��jA��^A��RA��-A��FA��RA��RA��RA��RA��^A��^A��^A��RA��RA��RA��RA��RA��RA��RA��^A��jA��^A��^A��RA��9A��RA��FA��FA��FA��RA��RA��FA��-A��!A��A��!A��!A��-A��-A��-A��-A��-A��-A��!A��A��A��!A��-A��-A��-A��!A��A��A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA���A���A���A���A���A���A���A���A���A���A��uA��uA���A���A���A��uA���A���A��uA���A���A���A��uA��uA��uA��hA��\A��hA��hA��\A��hA��hA��hA��hA��\A��\A��PA��PA��PA��PA��\A��PA��PA��PA��DA��DA��DA��DA��DA��7A��+A��+A��A��7A��7A��7A��+A��+A��A��A��A��A��A��A��A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�|�A�z�A�z�A�|�A�z�A�z�A�x�A�x�A�v�A�v�A�v�A�x�A�x�A�x�A�x�A�v�A�v�A�v�A�v�A�t�A�t�A�t�A�t�A�t�A�r�A�t�A�t�A�r�A�p�A�p�A�n�A�l�A�l�A�n�A�n�A�l�A�l�A�jA�l�A�jA�jA�jA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�ffA�dZA�dZA�bNA�`BA�^5A�`BA�`BA�`BA�^5A�\)A�ZA�XA�XA�ZA�\)A�\)A�ZA�ZA�XA�XA�XA�XA�XA�VA�S�A�Q�A�O�A�Q�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�M�A�M�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�G�A�G�A�G�A�E�A�C�A�A�A�A�A�A�A�?}A�=qA�;dA�;dA�;dA�=qA�?}A�?}A�?}A�?}A�=qA�=qA�=qA�;dA�;dA�9XA�9XA�9XA�9XA�7LA�5?A�5?A�5?A�5?A�5?A�5?A�33A�33A�1'A�/A�/A�/A�-A�+A�(�A�(�A�(�A�(�A�&�A�&�A�&�A�$�A�$�A�&�A�&�A�&�A�$�A�$�A�$�A�$�A�"�A�"�A�"�A�"�A� �A� �A� �A��A��A��A��A��A��A��A��444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  A�I�A��DA��9A��wA��TA�A��A�C�A�VA��!A��\A�33A�/A�&�A��DA��uA��-A��-A�^5A��+A���A���A���A� �A�1'A��+A�A�(�A�XA�7LA��#A��PA�ȴA�`BA��/A��hA��^A���A�G�A�%A�\)A���A�"�A���A���A�?}A�dZA�A�z�A�  A���A���A��uA��\A��A�+A�1'A��A��A�1'A�9XA��A��;A�l�A�VA��#A���A���A��A�5?A�K�A�(�A���A��\A�
=A��A���A���A���A���A�ƨA���A��^A�x�A�VA�1'A�+A���A��A��A��A�oA��yA�ƨA���A��-A��FA���A�l�A�S�A�G�A�bNA�C�A��A�  A�ȴA�ƨA�ĜA��wA���A���A��wA���A���A���A�x�A�;dA���A��HA��;A�ĜA���A�t�A�7LA�=qA�A���A��
A���A��FA��A�~�A���A��!A��\A��hA�z�A�|�A�l�A�S�A�1'A� �A��A���A�A���A���A���A��;A���A���A���A��hA�~�A���A��uA�t�A�G�A�{A���A��A���A���A��TA�A�%A���A���A� �A��A��A�ȴA���A�ĜA���A���A��
A��/A��HA��#A��;A��/A���A���A��A��#A��
A��
A��
A���A���A���A�ĜA�ƨA���A��^A���A���A��RA���A��PA�p�A�t�A�jA�v�A�~�A�n�A�ffA�v�A�z�A�z�A�n�A�XA�S�A�M�A�S�A�XA�G�A�7LA�7LA�7LA�O�A�^5A�hsA�dZA�`BA�^5A�S�A�Q�A�K�A�=qA�7LA�M�A�E�A�=qA�I�A�I�A�K�A�G�A�K�A�K�A�G�A�A�A�5?A�7LA�5?A�5?A�9XA�9XA�9XA�33A�?}A�9XA�1'A�/A�7LA�C�A�M�A�M�A�I�A�C�A�;dA�1'A�/A�-A�7LA�C�A�G�A�I�A�K�A�M�A�I�A�I�A�Q�A�^5A�ZA�VA�^5A�`BA�\)A�\)A�hsA�n�A�n�A�x�A�|�A�|�A�z�A�v�A�p�A�l�A�r�A��A�~�A�z�A�x�A�|�A��A��A��A��+A��A��+A��A��\A��uA��hA��\A��hA��PA��7A��7A��\A��\A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A���A���A���A���A��A��A���A���A��A��A��A��9A��FA��FA��FA��-A��9A��FA��RA��^A��RA��jA��wA���A���A�A�A���A�A�ƨA�ĜA�ĜA�A���A���A�ĜA�ĜA�ƨA�ĜA�ƨA�ȴA�ȴA�ȴA�ȴA�A�ĜA�ȴA�ȴA�ĜA�ĜA�ĜA���A���A���A���A�ȴA�ƨA�ȴA���A���A���A���A���A���A��
A���A���A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��
A��
A��
A��#A��A��A��A��
A���A���A���A��A��#A��#A��/A��;A��/A��/A��/A��/A��#A��
A��#A��/A��/A��/A��;A��;A��/A��#A��#A��A��
A���A��A��#A��#A��;A��/A��/A��#A��#A��;A��;A��/A��#A��A��A��A��;A��;A��/A��#A��/A��;A��HA��HA��;A��;A��/A��#A��A��#A��/A��/A��;A��HA��HA��HA��HA��TA��HA��HA��HA��;A��/A��A��/A��/A��#A��
A��/A��/A��/A��/A��#A��A��A��#A��A��#A��A��
A��A��#A��#A��A��
A��
A���A���A���A��
A��A��#A��A��A��
A��
A��
A��A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA�ȴA�ƨA�ȴA�ȴA�ƨA�ƨA�ĜA���A��wA�ȴA�ƨA�ƨA�ĜA�ĜA�ĜA�ĜA�A���A���A�A�ĜA�ĜA�A���A�A���A��wA���A��wA��jA���A�A���A��wA��^A��^A��jA��jA��jA��wA��jA��^A��RA��-A��FA��RA��RA��RA��RA��^A��^A��^A��RA��RA��RA��RA��RA��RA��RA��^A��jA��^A��^A��RA��9A��RA��FA��FA��FA��RA��RA��FA��-A��!A��A��!A��!A��-A��-A��-A��-A��-A��-A��!A��A��A��!A��-A��-A��-A��!A��A��A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA���A���A���A���A���A���A���A���A���A���A��uA��uA���A���A���A��uA���A���A��uA���A���A���A��uA��uA��uA��hA��\A��hA��hA��\A��hA��hA��hA��hA��\A��\A��PA��PA��PA��PA��\A��PA��PA��PA��DA��DA��DA��DA��DA��7A��+A��+A��A��7A��7A��7A��+A��+A��A��A��A��A��A��A��A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�|�A�z�A�z�A�|�A�z�A�z�A�x�A�x�A�v�A�v�A�v�A�x�A�x�A�x�A�x�A�v�A�v�A�v�A�v�A�t�A�t�A�t�A�t�A�t�A�r�A�t�A�t�A�r�A�p�A�p�A�n�A�l�A�l�A�n�A�n�A�l�A�l�A�jA�l�A�jA�jA�jA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�ffA�dZA�dZA�bNA�`BA�^5A�`BA�`BA�`BA�^5A�\)A�ZA�XA�XA�ZA�\)A�\)A�ZA�ZA�XA�XA�XA�XA�XA�VA�S�A�Q�A�O�A�Q�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�M�A�M�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�G�A�G�A�G�A�E�A�C�A�A�A�A�A�A�A�?}A�=qA�;dA�;dA�;dA�=qA�?}A�?}A�?}A�?}A�=qA�=qA�=qA�;dA�;dA�9XA�9XA�9XA�9XA�7LA�5?A�5?A�5?A�5?A�5?A�5?A�33A�33A�1'A�/A�/A�/A�-A�+A�(�A�(�A�(�A�(�A�&�A�&�A�&�A�$�A�$�A�&�A�&�A�&�A�$�A�$�A�$�A�$�A�"�A�"�A�"�A�"�A� �A� �A� �A��A��A��A��A��A��A��A��444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220531090112                              AO  ARCAADJP                                                                    20220531090112    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220531090112  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220531090112  QCF$                G�O�G�O�G�O�C000            