CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:38:44Z creation;2022-06-04T17:38:44Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604173844  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               YA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ك��[�1   @ك�n,��@0�+�cZvȴ9X1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�ffC�fC�fC  C  C
  C  C  C  C33C�fC  C  C�fC  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@�CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� DyfDy�fDz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�C3Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @.�R@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�=qA�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�u�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B���B�B�C�zC�zC�C�C	�C�C�C�C!GC�zC�C�C�zC�C�C�C!�C#�C%�zC'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C>�C@�CA�zCC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Cl�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
C��=C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dy�Dy��Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�@�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AϺ*Aϸ�AϵtAϴ9Aϵ�AϷAϷLAϷAϸAϳhAϴAϳ�AϲaAϵ?A϶FAϸ�Aϸ�Aϲ�AϤ@Aϳ�A϶�A϶zAϴ9Aϫ6AϦLAϩ_AϦ�AϠ�AϜCAϠ\Aϕ�Aς�A�x�A�ncA�1�A��AΤ�A�e`A�#�A͠�A�EA�!�A�
=A�O�A�A�I�A���A��:A�*eA���A�o5A��WA�b�A��A�[�A��#A�W
A�0!A� �A���A��=A�qAA�R�A�bNA�A�A�ɆA�>BA��A��A���A��rA��UA��LA�A�#�A�`�A�$�A��0A�+A�a|A���A�רA�W�A��A��]A��dA��A�<�A��A��5A�0UA�y�A��@A�[�A��aA��XA��EA�d�A|z�Ay@�At�AqoAnA Alh
AkeAg��AcOvA_@AZѷAZ�4AZJAW�XAUxAS�6AR%AM�7AIoAG��AG2aAE�AE(�ADkQAB,=A@��A?�QA??A=%�A;��A;4�A:Y�A:HA:�A9kQA8:�A7�]A7]�A7SA5��A3��A2��A0�8A0[WA/�TA/�eA/��A.w2A,{�A,J�A,Z�A,-wA*�"A)��A*�A*u�A(��A&�YA%˒A%2�A$\�A"k�A!t�A f�A��A��A��A�oAc�A��AoAp�A?}A��AhsA �A�Ac�A^�A%FA��A�AȴA�zAe,A�A�A�.A֡A��AQ�A
=A�tA��A��AffA��A�eA	A��A��A�Ao AںAѷAl�A��A�{A�gA�A�Az�A��A3�A��A iA�AA@A��A
��A
�A	�<Av`A�`A�>A_�A�?A�1A�2A2aA��A��Ax�AS�A1�A��AX�A-wA-A8A `�@��"@�Ta@�tT@�E�@�`B@�O@�u@��@��"@��@�@O@��@���@��[@�Vm@�I�@�h�@���@�\)@�D�@��@�U�@��@���@�P�@�c�@�@�S&@��@�O@�_@�S�@��@�r�@���@�E9@�c @�*�@�خ@�@�a|@�]d@�^5@�H@��
@�J#@�{�@᯸@��@��@�,�@�L�@��@�8�@ޚ@ޫ6@ޢ4@�J@ݮ@��@�$@�"�@��"@ڜx@��D@�u�@��"@���@��z@ֆY@�$�@�?@�g�@ԛ�@�-�@���@�j@�bN@��@Ь�@�G@�}�@�w2@�]�@�*0@�Mj@�`�@�O@͇�@Ͱ�@�s�@�K�@���@̦L@˅@��@�7@�!-@� i@�҉@ȹ$@���@Ⱦ�@Ț@Ȏ�@�j@�@�\)@�H�@ńM@�Vm@�	l@�ff@���@�5�@¯O@�@�tT@�<�@��N@�j@�rG@�:�@��@�Ta@�F�@��}@�V@�O@�c @��@���@���@�A @�p;@��@���@�Z@��@��>@�o @�!�@���@�~(@�S�@��@���@���@�~(@�M�@�  @�X@�w�@�ϫ@��@���@���@�O@��#@�j@��@��L@�Z@�*�@���@�;@�C�@��@�e�@�Ft@�}�@��@��<@��@�m�@�GE@���@��q@�iD@�֡@���@�Xy@�O@���@�S@���@�w�@��	@��y@��@��@��^@��V@���@�[W@��'@�Z@�@�u@��)@���@���@���@�$�@���@��@�A�@�+@�%F@��K@��o@��=@��f@�dZ@�#�@�q@�6�@�W�@�($@��9@�4�@��y@�V@���@��@�i�@�B[@�b@���@�*0@���@���@���@�7@�/�@�[�@��j@�4�@�W�@���@�P�@�!-@�@@��y@���@�&@���@��@���@��t@��@���@�l�@���@���@���@���@�I�@��@���@���@��C@�#�@��X@��@��@�kQ@��@���@�O�@�6z@��@���@�ȴ@��'@�K^@��Z@���@��)@��Q@��q@�J#@�@��@���@��}@���@�m�@�I�@�
�@��@��4@�&@��`@��z@�s�@�S�@�#:@��g@��@���@���@��@���@�=�@��	@��5@��]@��.@�xl@�M�@���@��:@��@�ߤ@���@�L0@��@��0@�S�@�;@��,@���@�1�@�	@�u@��K@���@�[W@�&@���@���@���@��B@�� @�)�@���@���@�s@�S&@�+@��y@���@��I@�GE@�
�@��a@���@�e�@�K�@��@��K@��@��@�;�@���@���@��@�X�@�"�@�@��@��U@��I@�|�@�!@��@@O@~�F@}��@}w2@}/@|��@|I�@|(�@{�@{��@z�H@z�F@z�@y�X@xx@w'�@v�@v�<@vff@v@u�d@uhs@u	l@t	�@s��@s@O@r�,@r�R@rM�@r!�@q�@q��@q4@q@q�@p�j@p_@p�@o>�@n�y@nq�@n;�@n�@m��@m��@m�"@m�"@m2a@l�$@lH@k��@j;�@iq@hZ@hI�@hK^@hA�@h%�@g�
@g8@f�b@fv�@fz@fH�@e�n@d�@c�K@c;d@bxl@bTa@b��@b�A@b@�@b
�@a��@a��@aL�@a�@a@@`�@_�@^�@^@�@^_@]%F@\M@[{J@[;d@['�@[�@Zz@Y�@Y��@Y�"@Y�@X�z@X7�@W��@W��@Wa@WE9@WC@V��@V��@V.�@U��@VTa@VQ@U��@U�@T�@T�`@T�@Tw�@S�@S��@SE9@SS@R�<@R�r@R	@Qԕ@Q�9@Q�@Q�'@Q:�@P��@P�5@P��@P��@P_@PN�@P*�@P	�@O��@O�&@O�@O�w@O�F@O��@O�{@O�@O�@N�@Nn�@N1�@M�Z@MDg@L�@L��@L�@K�{@K=@J�,@JYK@I�j@I�S@H�v@HɆ@H�@H%�@H	�@G�@G��@G��@GZ�@G+@F��@F�@Eϫ@EY�@D�[@C��@C�@B�F@BO@B�@A��@A�@Ak�@A�@@�p@@�@?�A@?�Q@?��@?U�@?�@>�'@>�@=�@=L�@=F@=?}@=	l@<�4@<l"@<�@;$t@:�2@:ں@:��@:�F@:@9��@9=�@8ѷ@8�@8�Y@8,=@7��@7{J@7]�@7$t@6��@6��@6Ov@5�o@5&�@4Ɇ@4�z@4�Y@4`�@3�6@3�@2�@2� @2{@1��@1��@1�h@1o @1X@1L�@1 \@0��@0h�@0	�@0  @/�@/��@/S�@/C@.�8@.�X@.�r@.Q@.@-e,@-!�@-�@,��@,?�@,�@,7@,x@+�&@+��@+��@+\)@+�@*�'@*M�@)�7@(��@(��@(I�@(H@(C-@(?�@(1'@(�@'�@'�&@'�
@'�}@'�@'��@'�q@'��@'�	@'Z�@'C�@&�@&v�@&($@%�D@%�@%��@%�C@%��@%`B@%*0@$ی@$,=@#��@#��@#~�@#g�@#K�@#6z@#�@"ں@"�<@"a|@"8�@"1�@!�@!��@!��@!L�@!	l@ �9@ ��@ I�@ݘ@�k@|�@n/@b�@Mj@�@�2@�}@��@^5@�@�@��@^�@4@!�@�@֡@bN@خ@� @�w@�P@o@�2@�X@v�@#:@J@�Z@��@��@k�@@�@�@�@��@�+@�@ƨ@�*@C�@�@ں@ȴ@�r@a|@�@w2@J�@:�@-w@@@�@(�@�@��@S�@��@�H@ȴ@?@�#@�@�"@e,@^�@IR@4@��@`�@6@M@�W@��@�@@g�@_p@K�@�c@��@_�@!�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AϺ*Aϸ�AϵtAϴ9Aϵ�AϷAϷLAϷAϸAϳhAϴAϳ�AϲaAϵ?A϶FAϸ�Aϸ�Aϲ�AϤ@Aϳ�A϶�A϶zAϴ9Aϫ6AϦLAϩ_AϦ�AϠ�AϜCAϠ\Aϕ�Aς�A�x�A�ncA�1�A��AΤ�A�e`A�#�A͠�A�EA�!�A�
=A�O�A�A�I�A���A��:A�*eA���A�o5A��WA�b�A��A�[�A��#A�W
A�0!A� �A���A��=A�qAA�R�A�bNA�A�A�ɆA�>BA��A��A���A��rA��UA��LA�A�#�A�`�A�$�A��0A�+A�a|A���A�רA�W�A��A��]A��dA��A�<�A��A��5A�0UA�y�A��@A�[�A��aA��XA��EA�d�A|z�Ay@�At�AqoAnA Alh
AkeAg��AcOvA_@AZѷAZ�4AZJAW�XAUxAS�6AR%AM�7AIoAG��AG2aAE�AE(�ADkQAB,=A@��A?�QA??A=%�A;��A;4�A:Y�A:HA:�A9kQA8:�A7�]A7]�A7SA5��A3��A2��A0�8A0[WA/�TA/�eA/��A.w2A,{�A,J�A,Z�A,-wA*�"A)��A*�A*u�A(��A&�YA%˒A%2�A$\�A"k�A!t�A f�A��A��A��A�oAc�A��AoAp�A?}A��AhsA �A�Ac�A^�A%FA��A�AȴA�zAe,A�A�A�.A֡A��AQ�A
=A�tA��A��AffA��A�eA	A��A��A�Ao AںAѷAl�A��A�{A�gA�A�Az�A��A3�A��A iA�AA@A��A
��A
�A	�<Av`A�`A�>A_�A�?A�1A�2A2aA��A��Ax�AS�A1�A��AX�A-wA-A8A `�@��"@�Ta@�tT@�E�@�`B@�O@�u@��@��"@��@�@O@��@���@��[@�Vm@�I�@�h�@���@�\)@�D�@��@�U�@��@���@�P�@�c�@�@�S&@��@�O@�_@�S�@��@�r�@���@�E9@�c @�*�@�خ@�@�a|@�]d@�^5@�H@��
@�J#@�{�@᯸@��@��@�,�@�L�@��@�8�@ޚ@ޫ6@ޢ4@�J@ݮ@��@�$@�"�@��"@ڜx@��D@�u�@��"@���@��z@ֆY@�$�@�?@�g�@ԛ�@�-�@���@�j@�bN@��@Ь�@�G@�}�@�w2@�]�@�*0@�Mj@�`�@�O@͇�@Ͱ�@�s�@�K�@���@̦L@˅@��@�7@�!-@� i@�҉@ȹ$@���@Ⱦ�@Ț@Ȏ�@�j@�@�\)@�H�@ńM@�Vm@�	l@�ff@���@�5�@¯O@�@�tT@�<�@��N@�j@�rG@�:�@��@�Ta@�F�@��}@�V@�O@�c @��@���@���@�A @�p;@��@���@�Z@��@��>@�o @�!�@���@�~(@�S�@��@���@���@�~(@�M�@�  @�X@�w�@�ϫ@��@���@���@�O@��#@�j@��@��L@�Z@�*�@���@�;@�C�@��@�e�@�Ft@�}�@��@��<@��@�m�@�GE@���@��q@�iD@�֡@���@�Xy@�O@���@�S@���@�w�@��	@��y@��@��@��^@��V@���@�[W@��'@�Z@�@�u@��)@���@���@���@�$�@���@��@�A�@�+@�%F@��K@��o@��=@��f@�dZ@�#�@�q@�6�@�W�@�($@��9@�4�@��y@�V@���@��@�i�@�B[@�b@���@�*0@���@���@���@�7@�/�@�[�@��j@�4�@�W�@���@�P�@�!-@�@@��y@���@�&@���@��@���@��t@��@���@�l�@���@���@���@���@�I�@��@���@���@��C@�#�@��X@��@��@�kQ@��@���@�O�@�6z@��@���@�ȴ@��'@�K^@��Z@���@��)@��Q@��q@�J#@�@��@���@��}@���@�m�@�I�@�
�@��@��4@�&@��`@��z@�s�@�S�@�#:@��g@��@���@���@��@���@�=�@��	@��5@��]@��.@�xl@�M�@���@��:@��@�ߤ@���@�L0@��@��0@�S�@�;@��,@���@�1�@�	@�u@��K@���@�[W@�&@���@���@���@��B@�� @�)�@���@���@�s@�S&@�+@��y@���@��I@�GE@�
�@��a@���@�e�@�K�@��@��K@��@��@�;�@���@���@��@�X�@�"�@�@��@��U@��I@�|�@�!@��@@O@~�F@}��@}w2@}/@|��@|I�@|(�@{�@{��@z�H@z�F@z�@y�X@xx@w'�@v�@v�<@vff@v@u�d@uhs@u	l@t	�@s��@s@O@r�,@r�R@rM�@r!�@q�@q��@q4@q@q�@p�j@p_@p�@o>�@n�y@nq�@n;�@n�@m��@m��@m�"@m�"@m2a@l�$@lH@k��@j;�@iq@hZ@hI�@hK^@hA�@h%�@g�
@g8@f�b@fv�@fz@fH�@e�n@d�@c�K@c;d@bxl@bTa@b��@b�A@b@�@b
�@a��@a��@aL�@a�@a@@`�@_�@^�@^@�@^_@]%F@\M@[{J@[;d@['�@[�@Zz@Y�@Y��@Y�"@Y�@X�z@X7�@W��@W��@Wa@WE9@WC@V��@V��@V.�@U��@VTa@VQ@U��@U�@T�@T�`@T�@Tw�@S�@S��@SE9@SS@R�<@R�r@R	@Qԕ@Q�9@Q�@Q�'@Q:�@P��@P�5@P��@P��@P_@PN�@P*�@P	�@O��@O�&@O�@O�w@O�F@O��@O�{@O�@O�@N�@Nn�@N1�@M�Z@MDg@L�@L��@L�@K�{@K=@J�,@JYK@I�j@I�S@H�v@HɆ@H�@H%�@H	�@G�@G��@G��@GZ�@G+@F��@F�@Eϫ@EY�@D�[@C��@C�@B�F@BO@B�@A��@A�@Ak�@A�@@�p@@�@?�A@?�Q@?��@?U�@?�@>�'@>�@=�@=L�@=F@=?}@=	l@<�4@<l"@<�@;$t@:�2@:ں@:��@:�F@:@9��@9=�@8ѷ@8�@8�Y@8,=@7��@7{J@7]�@7$t@6��@6��@6Ov@5�o@5&�@4Ɇ@4�z@4�Y@4`�@3�6@3�@2�@2� @2{@1��@1��@1�h@1o @1X@1L�@1 \@0��@0h�@0	�@0  @/�@/��@/S�@/C@.�8@.�X@.�r@.Q@.@-e,@-!�@-�@,��@,?�@,�@,7@,x@+�&@+��@+��@+\)@+�@*�'@*M�@)�7@(��@(��@(I�@(H@(C-@(?�@(1'@(�@'�@'�&@'�
@'�}@'�@'��@'�q@'��@'�	@'Z�@'C�@&�@&v�@&($@%�D@%�@%��@%�C@%��@%`B@%*0@$ی@$,=@#��@#��@#~�@#g�@#K�@#6z@#�@"ں@"�<@"a|@"8�@"1�@!�@!��@!��@!L�@!	l@ �9@ ��@ I�@ݘ@�k@|�@n/@b�@Mj@�@�2@�}@��@^5@�@�@��@^�@4@!�@�@֡@bN@خ@� @�w@�P@o@�2@�X@v�@#:@J@�Z@��@��@k�@@�@�@�@��@�+@�@ƨ@�*@C�@�@ں@ȴ@�r@a|@�@w2@J�@:�@-w@@@�@(�@�@��@S�@��@�H@ȴ@?@�#@�@�"@e,@^�@IR@4@��@`�@6@M@�W@��@�@@g�@_p@K�@�c@��@_�@!�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�rB
�rB
�rB
ʌB
��B
��B
��B
ʌB
ʦB
�#B
�#B
�	B
�	B
�#B
�=B
�XB
�=B
��B
�fB
ɆB
�	B
�	B
ɠB
��B
��B
�1B
��B
�zB
��B
�EB
�?B
ĶB
ðB
ªB
�(B
�>B
�B
�5B
��B
�*B
��B
��B
��B
��B
��B
�WB
�HB
��B
�$B
�	B
�+B
�
B
�|B
��BBjB6zB?�BDBI�Bx�B{B|jB��B��B�B�3B��B�B��B-B�B�wB�lB�;B�!B�B�B��B�B}"BlqB_!BMPB9XB+6B$�B�BBuB
� B
��B
�gB
�nB
�B
c B
,�B
�B	�B	ΊB	�nB	��B	�FB	��B	{�B	k�B	W$B	B�B	-]B	-]B	-wB	(sB	"hB	�B	B	B	$B	�B	B	�B	xB	OB	%�B	,qB	3�B	49B	1AB	5�B	8�B	9$B	J�B	a�B	wfB	�^B	��B	�MB	��B	�KB	��B	��B	�DB	ɠB	רB	�kB	��B	��B	޸B	�B	�mB	�B	�4B	�XB
�B
VB
_B	�XB	��B	��B	�nB	�B	�B	��B	��B	��B	�:B	ԯB	��B	�'B	�5B	�B	�B	�B	�B	�[B	�vB	��B	�MB	�B	�CB	�B	�WB	��B	��B	�+B	�B	�lB	�B	�qB
B
aB
�B
�B	��B	��B	�B	�B	��B	��B
'B	�<B	��B	�B	�vB	�B	�!B	��B	�B	��B	�B	��B
�B
fB
�B
�B	��B	��B	�2B	�B	�5B	�WB	�B	�B	� B	�oB	��B	��B	��B	�MB	ԕB	�@B	�&B	�B	յB	��B	��B	ևB	�mB	��B	��B	�\B	�	B	�B	�B	��B	˒B	�[B	׍B	�?B	��B	�.B	��B	��B	�[B	��B	��B	��B	ܒB	�8B	�B	�`B	��B	�B
  B
B
 4B	�0B	��B	�B	��B	��B	�B	�tB	�NB	�B	��B	��B	�sB	רB	�yB	��B	چB	�]B	�dB	�jB	��B	�]B	��B	�qB	�B	�B	��B	��B	�OB	�KB	��B	��B	�B	� B	�5B	�)B	��B	�B	��B	��B	�B	�B	�B	�B	�*B	�B	�B	�;B	�B	�CB	�=B	�B	��B	��B	�B	�
B	�B	�"B	�)B	�B	�OB	�wB	��B	�qB	�B	��B	�IB	�}B	�5B	�B	��B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�wB	�B	��B	�kB	��B	�QB	�B	�B	��B	�yB	�B	�kB	�B	�=B	��B	�B	��B	��B	��B	�B	��B	�B	��B	�B	��B	�`B	�FB	��B	�?B	��B	��B	��B	�[B	�B	�B	�TB	�B	�9B	��B	�B	�MB	�MB	�MB	�9B	�B	�B	��B	�B	�cB	�B	��B	�B	� B	��B	��B	�B	�wB	�B	�B	�tB	�LB	��B	�lB	��B	��B	�MB	�B	��B	��B	�B	�9B	�B	��B	��B	��B	��B	��B	�^B	�rB	�$B	��B	��B	��B	�*B	��B	��B	��B	�(B	�BB	�B	��B	�"B	�VB	�BB	�wB	�BB	�wB	�wB	�BB	�.B	��B	��B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
-B
�B
�B
�B
�B
�B
+B
EB
�B
YB
�B
�B
�B
�B

�B
�B
�B

�B
�B
�B
�B
�B
�B
zB
�B
dB
�B

�B

�B
B
6B
�B
�B
�B
�B
B
1B
�B

B
�B
�B
B
KB
�B
7B
WB
�B
)B
�B
xB
B
�B
�B
]B
�B
B
B
�B
;B
�B
B
�B
;B
!B
�B
B
�B
 'B
�B
 B
 �B
 �B
!HB
"B
"4B
!�B
!�B
"�B
"hB
#B
"�B
"�B
"�B
"hB
#B
"hB
"�B
#B
#�B
#�B
#nB
$�B
$�B
%`B
$�B
%,B
&LB
&�B
%`B
&LB
%�B
&B
&2B
'�B
(
B
'�B
(
B
'�B
(�B
)B
(�B
(XB
(sB
(sB
)*B
)�B
*�B
*B
+B
+6B
+kB
+�B
+6B
+6B
,qB
,�B
,�B
,�B
-CB
-wB
-�B
-�B
-�B
-wB
-�B
-�B
.�B
.�B
/ B
/OB
/ B
/iB
/iB
0;B
0UB
1AB
1vB
1AB
1�B
1�B
1�B
2GB
2-B
2aB
2�B
2�B
2B
2�B
2�B
2�B
2�B
2GB
2GB
2�B
3MB
3hB
4B
49B
3hB
2�B
1�B
1[B
1'B
1[B
1B
0�B
0�B
1B
1B
1�B
1�B
1�B
1�B
2�B
3�B
2�B
33B
3�B
4TB
5tB
7LB
8�B
8�B
8�B
8�B
8�B
9>B
8RB
6`B
6B
7�B
8RB
8lB
8�B
9�B
9>B
8�B
8�B
8�B
:�B
;B
;�B
:DB
8�B
9$B
8RB
72B
8�B
9�B
9�B
:�B
;�B
=�B
>(B
=<B
=�B
>]B
="B
<�B
="B
>�B
?�B
?.B
?.B
@�B
@�B
@�B
A�B
BAB
A�B
B'B
B�B
B�B
B�B
BAB
B'B
BAB
B'B
B[B
BuB
C�B
D�B
EB
GEB
G+B
F�B
E�B
E�B
E�B
EB
E�B
EB
D�B
D�B
E9B
E�B
D�B
FB
G�B
GEB
G�B
HKB
IB
J#B
J�B
J�B
KDB
K^B
K^B
K^B
K^B
K�B
K�B
LB
L0B
MjB
M�B
NB
M�B
N<B
NpB
NVB
N�B
N�B
O�B
PB
PHB
P�B
Q4B
Q�B
R:B
Q�B
Q4B
QhB
RB
Q�B
RB
R�B
S&B
R�B
S�B
S[B
SuB
S�B
TB
TaB
T�B
T�B
T�B
VmB
V�B
V�B
V�B
W$B
V�B
W?B
W�B
W�B
W
B
XEB
XB
W�B
X+B
X_B
X�B
X�B
X�B
X�B
YeB
X�B
X�B
X�B
YB
YKB
YeB
Y�B
ZB
ZB
ZB
ZB
Z�B
Z7B
Z�B
Z7B
Z�B
Z�B
[#B
Z�B
Z�B
[	B
[	B
[�B
[�B
[�B
\B
]B
]/B
\�B
]/B
]IB
^B
^�B
_!B
^�B
_pB
_�B
_�B
_�B
_�B
_�B
_�B
`B
`�B
`�B
a�B
abB
a�B
bB
bB
bhB
bNB
b�B
b�B
bhB
cB
cB
cTB
b�B
cnB
c�B
d@B
cnB
c�B
c�B
dZB
c�B
dB
dZB
d�B
d�B
e�B
f�B
f�B
gB
gB
gB
f�B
gB
gB
g�B
gRB
gmB
gRB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h$B
h�B
h�B
iB
h�B
i*B
iyB
iyB
i_B
iyB
i�B
j�B
j�B
j�B
j�B
j�B
kB
j�B
kB
kQB
kQB
k�B
k�B
k�B
kkB
k�B
kQB
k�B
k�B
k�B
l=B
lqB
l�B
mwB
m�B
m�B
m�B
m�B
m�B
n/B
nB
n}B
n�B
o�B
o�B
o�B
pUB
o�B
p�B
pUB
p�B
p�B
q�B
q�B
q�B
q�B
r�B
sB
r�B
s�B
shB
shB
tB
tB
t9B
tnB
t�B
tnB
t�B
t�B
uB
t�B
tnB
t�B
t�B
u�B
u�B
v�B
vzB
v�B
v�B
wB
wLB
w�B
wfB
w�B
x8B
xlB
xRB
w�B
xB
xB
xRB
x8B
xB
y	B
y	B
x�B
y>B
y$B
y$B
yrB
y�B
zDB
z*B
z^B
z�B
z�B
z�B
z�B
{0B
z�B
{0B
{�B
{�B
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�rB
�rB
�rB
ʌB
��B
��B
��B
ʌB
ʦB
�#B
�#B
�	B
�	B
�#B
�=B
�XB
�=B
��B
�fB
ɆB
�	B
�	B
ɠB
��B
��B
�1B
��B
�zB
��B
�EB
�?B
ĶB
ðB
ªB
�(B
�>B
�B
�5B
��B
�*B
��B
��B
��B
��B
��B
�WB
�HB
��B
�$B
�	B
�+B
�
B
�|B
��BBjB6zB?�BDBI�Bx�B{B|jB��B��B�B�3B��B�B��B-B�B�wB�lB�;B�!B�B�B��B�B}"BlqB_!BMPB9XB+6B$�B�BBuB
� B
��B
�gB
�nB
�B
c B
,�B
�B	�B	ΊB	�nB	��B	�FB	��B	{�B	k�B	W$B	B�B	-]B	-]B	-wB	(sB	"hB	�B	B	B	$B	�B	B	�B	xB	OB	%�B	,qB	3�B	49B	1AB	5�B	8�B	9$B	J�B	a�B	wfB	�^B	��B	�MB	��B	�KB	��B	��B	�DB	ɠB	רB	�kB	��B	��B	޸B	�B	�mB	�B	�4B	�XB
�B
VB
_B	�XB	��B	��B	�nB	�B	�B	��B	��B	��B	�:B	ԯB	��B	�'B	�5B	�B	�B	�B	�B	�[B	�vB	��B	�MB	�B	�CB	�B	�WB	��B	��B	�+B	�B	�lB	�B	�qB
B
aB
�B
�B	��B	��B	�B	�B	��B	��B
'B	�<B	��B	�B	�vB	�B	�!B	��B	�B	��B	�B	��B
�B
fB
�B
�B	��B	��B	�2B	�B	�5B	�WB	�B	�B	� B	�oB	��B	��B	��B	�MB	ԕB	�@B	�&B	�B	յB	��B	��B	ևB	�mB	��B	��B	�\B	�	B	�B	�B	��B	˒B	�[B	׍B	�?B	��B	�.B	��B	��B	�[B	��B	��B	��B	ܒB	�8B	�B	�`B	��B	�B
  B
B
 4B	�0B	��B	�B	��B	��B	�B	�tB	�NB	�B	��B	��B	�sB	רB	�yB	��B	چB	�]B	�dB	�jB	��B	�]B	��B	�qB	�B	�B	��B	��B	�OB	�KB	��B	��B	�B	� B	�5B	�)B	��B	�B	��B	��B	�B	�B	�B	�B	�*B	�B	�B	�;B	�B	�CB	�=B	�B	��B	��B	�B	�
B	�B	�"B	�)B	�B	�OB	�wB	��B	�qB	�B	��B	�IB	�}B	�5B	�B	��B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�wB	�B	��B	�kB	��B	�QB	�B	�B	��B	�yB	�B	�kB	�B	�=B	��B	�B	��B	��B	��B	�B	��B	�B	��B	�B	��B	�`B	�FB	��B	�?B	��B	��B	��B	�[B	�B	�B	�TB	�B	�9B	��B	�B	�MB	�MB	�MB	�9B	�B	�B	��B	�B	�cB	�B	��B	�B	� B	��B	��B	�B	�wB	�B	�B	�tB	�LB	��B	�lB	��B	��B	�MB	�B	��B	��B	�B	�9B	�B	��B	��B	��B	��B	��B	�^B	�rB	�$B	��B	��B	��B	�*B	��B	��B	��B	�(B	�BB	�B	��B	�"B	�VB	�BB	�wB	�BB	�wB	�wB	�BB	�.B	��B	��B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
-B
�B
�B
�B
�B
�B
+B
EB
�B
YB
�B
�B
�B
�B

�B
�B
�B

�B
�B
�B
�B
�B
�B
zB
�B
dB
�B

�B

�B
B
6B
�B
�B
�B
�B
B
1B
�B

B
�B
�B
B
KB
�B
7B
WB
�B
)B
�B
xB
B
�B
�B
]B
�B
B
B
�B
;B
�B
B
�B
;B
!B
�B
B
�B
 'B
�B
 B
 �B
 �B
!HB
"B
"4B
!�B
!�B
"�B
"hB
#B
"�B
"�B
"�B
"hB
#B
"hB
"�B
#B
#�B
#�B
#nB
$�B
$�B
%`B
$�B
%,B
&LB
&�B
%`B
&LB
%�B
&B
&2B
'�B
(
B
'�B
(
B
'�B
(�B
)B
(�B
(XB
(sB
(sB
)*B
)�B
*�B
*B
+B
+6B
+kB
+�B
+6B
+6B
,qB
,�B
,�B
,�B
-CB
-wB
-�B
-�B
-�B
-wB
-�B
-�B
.�B
.�B
/ B
/OB
/ B
/iB
/iB
0;B
0UB
1AB
1vB
1AB
1�B
1�B
1�B
2GB
2-B
2aB
2�B
2�B
2B
2�B
2�B
2�B
2�B
2GB
2GB
2�B
3MB
3hB
4B
49B
3hB
2�B
1�B
1[B
1'B
1[B
1B
0�B
0�B
1B
1B
1�B
1�B
1�B
1�B
2�B
3�B
2�B
33B
3�B
4TB
5tB
7LB
8�B
8�B
8�B
8�B
8�B
9>B
8RB
6`B
6B
7�B
8RB
8lB
8�B
9�B
9>B
8�B
8�B
8�B
:�B
;B
;�B
:DB
8�B
9$B
8RB
72B
8�B
9�B
9�B
:�B
;�B
=�B
>(B
=<B
=�B
>]B
="B
<�B
="B
>�B
?�B
?.B
?.B
@�B
@�B
@�B
A�B
BAB
A�B
B'B
B�B
B�B
B�B
BAB
B'B
BAB
B'B
B[B
BuB
C�B
D�B
EB
GEB
G+B
F�B
E�B
E�B
E�B
EB
E�B
EB
D�B
D�B
E9B
E�B
D�B
FB
G�B
GEB
G�B
HKB
IB
J#B
J�B
J�B
KDB
K^B
K^B
K^B
K^B
K�B
K�B
LB
L0B
MjB
M�B
NB
M�B
N<B
NpB
NVB
N�B
N�B
O�B
PB
PHB
P�B
Q4B
Q�B
R:B
Q�B
Q4B
QhB
RB
Q�B
RB
R�B
S&B
R�B
S�B
S[B
SuB
S�B
TB
TaB
T�B
T�B
T�B
VmB
V�B
V�B
V�B
W$B
V�B
W?B
W�B
W�B
W
B
XEB
XB
W�B
X+B
X_B
X�B
X�B
X�B
X�B
YeB
X�B
X�B
X�B
YB
YKB
YeB
Y�B
ZB
ZB
ZB
ZB
Z�B
Z7B
Z�B
Z7B
Z�B
Z�B
[#B
Z�B
Z�B
[	B
[	B
[�B
[�B
[�B
\B
]B
]/B
\�B
]/B
]IB
^B
^�B
_!B
^�B
_pB
_�B
_�B
_�B
_�B
_�B
_�B
`B
`�B
`�B
a�B
abB
a�B
bB
bB
bhB
bNB
b�B
b�B
bhB
cB
cB
cTB
b�B
cnB
c�B
d@B
cnB
c�B
c�B
dZB
c�B
dB
dZB
d�B
d�B
e�B
f�B
f�B
gB
gB
gB
f�B
gB
gB
g�B
gRB
gmB
gRB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h$B
h�B
h�B
iB
h�B
i*B
iyB
iyB
i_B
iyB
i�B
j�B
j�B
j�B
j�B
j�B
kB
j�B
kB
kQB
kQB
k�B
k�B
k�B
kkB
k�B
kQB
k�B
k�B
k�B
l=B
lqB
l�B
mwB
m�B
m�B
m�B
m�B
m�B
n/B
nB
n}B
n�B
o�B
o�B
o�B
pUB
o�B
p�B
pUB
p�B
p�B
q�B
q�B
q�B
q�B
r�B
sB
r�B
s�B
shB
shB
tB
tB
t9B
tnB
t�B
tnB
t�B
t�B
uB
t�B
tnB
t�B
t�B
u�B
u�B
v�B
vzB
v�B
v�B
wB
wLB
w�B
wfB
w�B
x8B
xlB
xRB
w�B
xB
xB
xRB
x8B
xB
y	B
y	B
x�B
y>B
y$B
y$B
yrB
y�B
zDB
z*B
z^B
z�B
z�B
z�B
z�B
{0B
z�B
{0B
{�B
{�B
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104920  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173844  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173844  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173844                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023851  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023851  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                